(** Statement-block cleanup passes. *)

open Stylesheet
open Common

let list_filter_preserve = List.filter_preserve

let media_feature_is name (f : Media.feature) =
  match f with
  | Media.Plain (n, _) | Media.Boolean n -> Media.equal_name n name
  | Media.Range (n, _, _) | Media.Range_rev (_, _, n) -> Media.equal_name n name
  | Media.Interval (_, _, n, _, _) -> Media.equal_name n name
  | Media.General_enclosed _ -> false

let rec condition_has_feature name (c : Media.condition) =
  match c with
  | Media.Feature f -> media_feature_is name f
  | Media.Not c -> condition_has_feature name c
  | Media.And (a, b) | Media.Or (a, b) ->
      condition_has_feature name a || condition_has_feature name b

(* A query mentions feature [name] either directly or as the trailing condition
   of a media type ([not all and (min-width: ...)]). *)
let rec query_has_feature name (q : Media.t) =
  match q with
  | Media.Cond c -> condition_has_feature name c
  | Media.Type { trailing = Some c; _ } -> condition_has_feature name c
  | Media.Type _ -> false
  | Media.List qs -> List.exists (query_has_feature name) qs

let has_nested_preference_media block =
  List.exists
    (function
      | Media (cond, _) ->
          query_has_feature Media.Prefers_contrast cond
          || query_has_feature Media.Prefers_reduced_motion cond
          || query_has_feature Media.Prefers_color_scheme cond
      | _ -> false)
    block

(* CSS Cascade 6.4: consecutive named [@layer] blocks with the same name are
   spec-equivalent to a single block. Merge them when no rule with a conflicting
   condition appears between. Anonymous layers stay distinct because each
   [@layer { ... }] without a name creates a new layer. *)
let rec needs_layer_merge = function
  | Layer (Some prev_name, _) :: Layer (Some name, _) :: _
    when equal_layer_name prev_name name ->
      true
  | _ :: rest -> needs_layer_merge rest
  | [] -> false

let merge_layer_blocks ~optimize_merged_block stmts =
  let rec merge acc prev = function
    | [] -> (
        match prev with
        | Some (Some name, block) ->
            List.rev (Layer (Some name, optimize_merged_block block) :: acc)
        | Some (None, block) -> List.rev (Layer (None, block) :: acc)
        | None -> List.rev acc)
    | Layer (Some name, block) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) when equal_layer_name prev_name name
          ->
            merge acc (Some (Some name, prev_block @ block)) rest
        | Some (Some prev_name, prev_block) ->
            merge
              (Layer (Some prev_name, optimize_merged_block prev_block) :: acc)
              (Some (Some name, block))
              rest
        | Some (None, prev_block) ->
            merge
              (Layer (None, prev_block) :: acc)
              (Some (Some name, block))
              rest
        | None -> merge acc (Some (Some name, block)) rest)
    | (Layer (None, _) as anon) :: rest -> (
        match prev with
        | Some (Some prev_name, prev_block) ->
            merge
              (Layer (Some prev_name, optimize_merged_block prev_block) :: acc)
              None (anon :: rest)
        | Some (None, prev_block) ->
            merge (Layer (None, prev_block) :: acc) None (anon :: rest)
        | None -> merge (anon :: acc) None rest)
    | stmt :: rest -> (
        match prev with
        | Some (Some name, block) ->
            merge
              (stmt :: Layer (Some name, optimize_merged_block block) :: acc)
              None rest
        | Some (None, block) ->
            merge (stmt :: Layer (None, block) :: acc) None rest
        | None -> merge (stmt :: acc) None rest)
  in
  merge [] None stmts

let merge_consecutive_layers ?(optimize_merged_block = Fun.id)
    (stmts : statement list) : statement list =
  if needs_layer_merge stmts then
    merge_layer_blocks ~optimize_merged_block stmts
  else stmts

let rec needs_media_merge = function
  | Media (prev_cond, prev_block) :: Media (cond, block) :: _
    when Media.equal prev_cond cond
         && not
              (has_nested_preference_media prev_block
              || has_nested_preference_media block) ->
      true
  | _ :: rest -> needs_media_merge rest
  | [] -> false

let merge_media_blocks ~optimize_merged_block stmts =
  let rec merge acc prev_media = function
    | [] -> (
        match prev_media with
        | Some (cond, block) ->
            List.rev (Media (cond, optimize_merged_block block) :: acc)
        | None -> List.rev acc)
    | Media (cond, block) :: rest -> (
        match prev_media with
        | Some (prev_cond, prev_block)
          when Media.equal prev_cond cond
               && not
                    (has_nested_preference_media prev_block
                    || has_nested_preference_media block) ->
            merge acc (Some (cond, prev_block @ block)) rest
        | Some (prev_cond, prev_block) ->
            merge
              (Media (prev_cond, optimize_merged_block prev_block) :: acc)
              (Some (cond, block))
              rest
        | None -> merge acc (Some (cond, block)) rest)
    | stmt :: rest -> (
        match prev_media with
        | Some (cond, block) ->
            merge
              (stmt :: Media (cond, optimize_merged_block block) :: acc)
              None rest
        | None -> merge (stmt :: acc) None rest)
  in
  merge [] None stmts

let merge_consecutive_media ?(optimize_merged_block = Fun.id)
    (stmts : statement list) : statement list =
  if needs_media_merge stmts then
    merge_media_blocks ~optimize_merged_block stmts
  else stmts

(* Leaf rules reachable from a statement, in source order, descending into
   nested blocks. A nested declarations run (CSS Nesting 1 sec. 3.4) sets
   properties on the rule it sits in, so it reads as one more rule with that
   selector; missed, it is a conflict the hoisting analysis below never sees.
   Descent goes through [statement_children], so a statement that grows a block
   is reached without listing it here. *)
let rec leaf_rules ?parent stmt =
  match stmt with
  | Rule r -> r :: List.concat_map (fun s -> leaf_rules ~parent:r s) r.nested
  | Declarations decls -> (
      match parent with
      | Some (p : rule) -> [ { p with declarations = decls; nested = [] } ]
      | None -> [])
  | stmt ->
      List.concat_map (fun s -> leaf_rules ?parent s) (statement_children stmt)

(* A declarations run outside any style rule: [leaf_rules] reports nothing for
   it, so an analysis that must see every declaration refuses rather than read
   that silence as "no conflict". *)
let rec holds_unattributed_run stmt =
  match stmt with
  | Declarations _ -> true
  | Media (_, b)
  | Supports (_, b)
  | Layer (_, b)
  | Container (_, _, b)
  | Starting_style b
  | Origin (_, b)
  | Moz_document (_, b)
  | Scope (_, _, b)
  | When (_, b)
  | Else (_, b) ->
      List.exists holds_unattributed_run b
  | _ -> false

(* Two declarations an element computes differently once they swap order: they
   write a common longhand slot, and they are not the same declaration. Reading
   the slots off the shorthand footprint rather than the property name is what
   ties [background] to the [background-color] it also writes. A pair that
   differs in importance is decided by importance, not by order. *)
let declarations_conflict a b =
  Declaration.is_important a = Declaration.is_important b
  && (not (Declaration.same_minified a b))
  && Shorthand.declarations_overlap a b

(* Two rules whose declarations would reorder unsafely: overlapping selectors
   and a conflicting declaration pair. *)
let rules_conflict (r1 : rule) (r2 : rule) =
  Selector_summary.may_overlap
    (Selector_summary.of_selector r1.selector)
    (Selector_summary.of_selector r2.selector)
  && List.exists
       (fun a -> List.exists (declarations_conflict a) r2.declarations)
       r1.declarations

(* Only cross statements whose cascade is pure source order: plain rules and
   conditional groups. Layers, origins, scopes, imports, etc. establish
   ordering/context that hoisting past would change. *)
let crossable_by_distant_merge = function
  | Rule _ | Declarations _ | Media _ | Supports _ | Container _ -> true
  | _ -> false

(* CSS Conditional Rules 5 sec. 2: a conditional group rule applies its contents
   when its condition holds, so two blocks under one prelude carry the same
   content as one block over their concatenation. A distant merge adds a move to
   that equivalence: the later block's declarations come to sit before the
   statements written between the two, and order of appearance decides the
   cascade only between a pair of declarations that does not commute. So the
   merge stands when nothing the hoisted block writes conflicts with anything
   the statements it crosses write, and those statements are all ones the
   cascade reads in source order.

   [prelude] reads the identity two blocks of one family have to share, with the
   body under it, and answers [None] for every other statement; [equal_prelude]
   decides when two identities are one; [build] puts a merged body back under
   one; [hoistable] is a family's own refusal to move a body at all.

   [owner] is the style rule whose body [stmts] is, when it is one. A
   declarations run in that body sets properties on [owner] (CSS Nesting 1 sec.
   3.4), so the analysis below only sees it once it can name the rule the run
   belongs to. *)
let has_distant_partner ~prelude ~equal_prelude stmts =
  let keys = List.filter_map (fun s -> Option.map fst (prelude s)) stmts in
  let rec dup = function
    | [] -> false
    | k :: rest -> List.exists (equal_prelude k) rest || dup rest
  in
  dup keys

(* Split [out] at the first block whose prelude is [key], into what comes before
   it, its own body, and what sits between it and the caller. *)
let split_at_prelude ~prelude ~equal_prelude key out :
    (statement list * statement list * statement list) option =
  let rec go before :
      statement list ->
      (statement list * statement list * statement list) option = function
    | [] -> None
    | stmt :: after -> (
        match prelude stmt with
        | Some (k, body) when equal_prelude k key ->
            Some (List.rev before, body, after)
        | Some _ | None -> go (stmt :: before) after)
  in
  go [] out

let try_distant_merge ~prelude ~equal_prelude ~build ~leaves ~unattributed out
    key block : statement list option =
  let hoisted = leaves block in
  match split_at_prelude ~prelude ~equal_prelude key out with
  | None -> None
  | Some (before, target, after)
    when List.for_all crossable_by_distant_merge after ->
      let crossed = leaves after in
      if
        unattributed block || unattributed after
        || List.exists (fun h -> List.exists (rules_conflict h) crossed) hoisted
      then None
      else Some (before @ [ build key (target @ block) ] @ after)
  | Some _ -> None

let merge_distant ~prelude ~equal_prelude ~build ~hoistable ?owner
    ~optimize_merged_block stmts =
  if not (has_distant_partner ~prelude ~equal_prelude stmts) then stmts
  else
    let leaves stmts = List.concat_map (leaf_rules ?parent:owner) stmts in
    let unattributed stmts =
      Option.is_none owner && List.exists holds_unattributed_run stmts
    in
    (* The accumulator is carried reversed, as the consecutive-block passes
       nearby carry theirs: appending to the end copies it once per statement,
       which costs a square in the statement count. Only a block of the family
       needs it in order, to find the partner it merges into, and [rev_map] puts
       the result back in source order. *)
    let step rout stmt =
      match prelude stmt with
      | Some (key, block) when hoistable block -> (
          match
            try_distant_merge ~prelude ~equal_prelude ~build ~leaves
              ~unattributed (List.rev rout) key block
          with
          | Some out' -> List.rev out'
          | None -> stmt :: rout)
      | Some _ | None -> stmt :: rout
    in
    List.fold_left step [] stmts
    |> List.rev_map (fun stmt ->
        match prelude stmt with
        | Some (key, body) -> build key (optimize_merged_block body)
        | None -> stmt)

let merge_distant_media ?owner ?(optimize_merged_block = Fun.id) stmts =
  merge_distant
    ~prelude:(function Media (cond, block) -> Some (cond, block) | _ -> None)
    ~equal_prelude:Media.equal
    ~build:(fun cond block -> Media (cond, block))
    ~hoistable:(fun block -> not (has_nested_preference_media block))
    ?owner ~optimize_merged_block stmts

(* CSS Conditional Rules 5: adjacent same-condition [@supports] / [@container]
   blocks may be merged because the cascade evaluates them identically. Mirror
   the [@media] approach. *)
let merge_consecutive_supports ?(optimize_merged_block = Fun.id)
    (stmts : statement list) : statement list =
  let rec needs_merge = function
    | Supports (prev_cond, _) :: Supports (cond, _) :: _
      when Supports.equal prev_cond cond ->
        true
    | _ :: rest -> needs_merge rest
    | [] -> false
  in
  if not (needs_merge stmts) then stmts
  else
    (* [acc] holds output in REVERSE order; reverse once at the end. *)
    let rec merge acc prev = function
      | [] -> (
          match prev with
          | Some (cond, block) ->
              List.rev (Supports (cond, optimize_merged_block block) :: acc)
          | None -> List.rev acc)
      | Supports (cond, block) :: rest -> (
          match prev with
          | Some (prev_cond, prev_block) when Supports.equal prev_cond cond ->
              merge acc (Some (cond, prev_block @ block)) rest
          | Some (prev_cond, prev_block) ->
              merge
                (Supports (prev_cond, optimize_merged_block prev_block) :: acc)
                (Some (cond, block))
                rest
          | None -> merge acc (Some (cond, block)) rest)
      | stmt :: rest -> (
          match prev with
          | Some (cond, block) ->
              merge
                (stmt :: Supports (cond, optimize_merged_block block) :: acc)
                None rest
          | None -> merge (stmt :: acc) None rest)
    in
    merge [] None stmts

(* The whole [@container] merge gate: a name and a condition, read once to
   decide the pass has work and once per adjacent pair. A [<container-name>] is
   a [<custom-ident>] (Conditional Rules 5 sec. 5.4), so the name the parser
   unescaped is its identity; [Container.equal] reads the condition's normalised
   structure rather than its serialised text, which an unknown container feature
   can repeat without meaning it. *)
let same_container (prev_name, prev_cond) (name, cond) =
  Option.equal String.equal prev_name name
  && Option.equal Container.equal prev_cond cond

let merge_consecutive_containers ?(optimize_merged_block = Fun.id)
    (stmts : statement list) : statement list =
  let rec needs_merge = function
    | Container (prev_name, prev_cond, _) :: Container (name, cond, _) :: _
      when same_container (prev_name, prev_cond) (name, cond) ->
        true
    | _ :: rest -> needs_merge rest
    | [] -> false
  in
  if not (needs_merge stmts) then stmts
  else
    (* [acc] holds output in REVERSE order; reverse once at the end. *)
    let rec merge acc prev = function
      | [] -> (
          match prev with
          | Some (name, cond, block) ->
              List.rev
                (Container (name, cond, optimize_merged_block block) :: acc)
          | None -> List.rev acc)
      | Container (name, cond, block) :: rest -> (
          match prev with
          | Some (prev_name, prev_cond, prev_block)
            when same_container (prev_name, prev_cond) (name, cond) ->
              merge acc (Some (name, cond, prev_block @ block)) rest
          | Some (prev_name, prev_cond, prev_block) ->
              merge
                (Container
                   (prev_name, prev_cond, optimize_merged_block prev_block)
                :: acc)
                (Some (name, cond, block))
                rest
          | None -> merge acc (Some (name, cond, block)) rest)
      | stmt :: rest -> (
          match prev with
          | Some (name, cond, block) ->
              merge
                (stmt
                :: Container (name, cond, optimize_merged_block block)
                :: acc)
                None rest
          | None -> merge (stmt :: acc) None rest)
    in
    merge [] None stmts

(* Reaching the partner a statement is written between is the move
   [merge_distant] argues for, and a [@container] keys it on the whole prelude.
   Conditional Rules 5 sec. 5.4 resolves the query against the nearest ancestor
   the [<container-name>] selects, so an unnamed [@container (width>10px)] and
   [@container main (width>10px)] ask about different elements however equal
   their conditions read, and two names select one ancestor only when they are
   the one ident. *)
let merge_distant_containers ?owner ?(optimize_merged_block = Fun.id) stmts =
  merge_distant
    ~prelude:(function
      | Container (name, cond, block) -> Some ((name, cond), block) | _ -> None)
    ~equal_prelude:same_container
    ~build:(fun (name, cond) block -> Container (name, cond, block))
    ~hoistable:(fun _ -> true)
    ?owner ~optimize_merged_block stmts

(* CSS Transitions 2 sec. 3.3: [@starting-style] takes no prelude, so two
   adjacent blocks hold the same starting styles, in the same order, as one
   block over their concatenation. Adjacency is the whole gate; the conditional
   passes above compare a condition this rule does not have. *)
let merge_consecutive_starting_style ?(optimize_merged_block = Fun.id)
    (stmts : statement list) : statement list =
  let rec needs_merge = function
    | Starting_style _ :: Starting_style _ :: _ -> true
    | _ :: rest -> needs_merge rest
    | [] -> false
  in
  if not (needs_merge stmts) then stmts
  else
    (* [acc] holds output in REVERSE order; reverse once at the end. *)
    let rec merge acc prev = function
      | [] -> (
          match prev with
          | Some block ->
              List.rev (Starting_style (optimize_merged_block block) :: acc)
          | None -> List.rev acc)
      | Starting_style block :: rest -> (
          match prev with
          | Some prev_block -> merge acc (Some (prev_block @ block)) rest
          | None -> merge acc (Some block) rest)
      | stmt :: rest -> (
          match prev with
          | Some block ->
              merge
                (stmt :: Starting_style (optimize_merged_block block) :: acc)
                None rest
          | None -> merge (stmt :: acc) None rest)
    in
    merge [] None stmts

(* Whether a layer block holds nothing the cascade can act on. A rule that
   writes no declarations of its own still carries whatever it nests, so reading
   only the declarations called a layer empty and deleted the CSS under it. *)
let is_layer_empty (block : statement list) : bool =
  List.for_all
    (function Rule { declarations = []; nested = []; _ } -> true | _ -> false)
    block

(* Collect consecutive empty named layers and merge them into a Layer_decl *)
let rec collect_empty_layer_names names remaining =
  match remaining with
  | Layer (Some layer_name, layer_block) :: rest when is_layer_empty layer_block
    ->
      collect_empty_layer_names (layer_name :: names) rest
  | Layer_decl existing_names :: rest ->
      (* Merge with existing layer declaration *)
      (List.rev names @ existing_names, rest)
  | _ -> (List.rev names, remaining)

(* Merge consecutive Layer_decl statements *)
let merge_layer_declarations (stmts : statement list) : statement list =
  (* CSS Cascade 5 sec. 6.4.4.2: [@layer A, B;] declares layers in source order;
     re-declaring a layer name is a no-op for cascade order. Merge consecutive
     [@layer ...;] statements and dedupe repeated names so [@layer u; @layer u]
     emits one [@layer u]. *)
  let dedup_preserving_order names =
    let seen = Hashtbl.create (List.length names) in
    list_filter_preserve
      (fun name ->
        if Hashtbl.mem seen name then false
        else begin
          Hashtbl.add seen name ();
          true
        end)
      names
  in
  let rec merge acc = function
    | [] -> List.rev acc
    | Layer_decl names1 :: Layer_decl names2 :: rest ->
        merge acc (Layer_decl (dedup_preserving_order (names1 @ names2)) :: rest)
    | (Layer_decl names as stmt) :: rest ->
        let names' = dedup_preserving_order names in
        let stmt = if names' == names then stmt else Layer_decl names' in
        merge (stmt :: acc) rest
    | stmt :: rest -> merge (stmt :: acc) rest
  in
  merge [] stmts

let add_new_layer_names seen names =
  let seen, added_rev =
    List.fold_left
      (fun (seen, added_rev) name ->
        if List.exists (equal_layer_name name) seen then (seen, added_rev)
        else (name :: seen, name :: added_rev))
      (seen, []) names
  in
  (seen, List.rev added_rev)

let layer_names_of_statement = function
  | Layer_decl names -> names
  | Layer (Some name, _) -> [ name ]
  | _ -> []

let following_layer_introduction_order seen stmts =
  let rec loop seen introduced_rev added_by_layer_decl = function
    | [] | Import _ :: _ -> (List.rev introduced_rev, added_by_layer_decl)
    | stmt :: rest ->
        let names = layer_names_of_statement stmt in
        let seen, added = add_new_layer_names seen names in
        let added_by_layer_decl =
          added_by_layer_decl
          || match stmt with Layer_decl _ -> added <> [] | _ -> false
        in
        loop seen
          (List.rev_append added introduced_rev)
          added_by_layer_decl rest
  in
  loop seen [] false stmts

let list_has_prefix prefix list =
  let rec loop = function
    | [], _ -> true
    | _ :: _, [] -> false
    | x :: xs, y :: ys -> equal_layer_name x y && loop (xs, ys)
  in
  loop (prefix, list)

let layer_decl_forward_redundant seen names rest =
  let _, introduced_by_decl = add_new_layer_names seen names in
  let introduced_by_rest, added_by_layer_decl =
    following_layer_introduction_order seen rest
  in
  added_by_layer_decl && list_has_prefix introduced_by_decl introduced_by_rest

let layer_decl_backward_redundant seen names =
  List.for_all (fun name -> List.exists (equal_layer_name name) seen) names

(* Top-level CSS Cascade 6.4 cleanup. A layer statement is removable when it
   only repeats layer order already introduced in the current import-separated
   segment, or when the immediately following top-level layer blocks/statements
   introduce the same new names in the same order. Nested conditional layer
   declarations are deliberately left alone because their participation depends
   on the condition at evaluation time. *)
let drop_redundant_layer_decls stmts =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (Import _ as stmt) :: rest -> loop [] (stmt :: acc) rest
    | (Layer_decl names as stmt) :: rest ->
        if
          layer_decl_backward_redundant seen names
          || layer_decl_forward_redundant seen names rest
        then loop seen acc rest
        else
          let seen, _ = add_new_layer_names seen names in
          loop seen (stmt :: acc) rest
    | stmt :: rest ->
        let seen, _ =
          add_new_layer_names seen (layer_names_of_statement stmt)
        in
        loop seen (stmt :: acc) rest
  in
  loop [] [] stmts

(* CSS Cascade 6.1: an empty rule (no declarations, no nested rules) contributes
   nothing, so drop it under [~optimize:true]. A conditional group rule applies
   its contents when its condition holds (CSS Conditional 3 sec. 3), so an empty
   one applies nothing whatever the condition and goes the same way, as does an
   empty [@scope], [@starting-style] or [@page] box. An empty named [@layer]
   survives as a [Layer_decl] since the name still orders the layer (CSS Cascade
   L6 6.4), and an empty [Origin] survives because it gates nothing and records
   where a block came from. *)
let is_empty_statement ~chained = function
  | Rule { declarations = []; nested = []; _ } -> true
  | Rule { selector; _ } -> Selector.matches_nothing selector
  | Media (_, []) -> true
  | Supports (_, []) -> true
  | Container (_, _, []) -> true
  | Moz_document (_, []) -> true
  | Scope (_, _, []) -> true
  | Starting_style [] -> true
  | Page (_, []) -> true
  | Page_with_margins (_, [], []) -> true
  (* css-conditional-5 sec. 3 binds an [@else] to the [@when] or [@else] before
     it, so an empty branch is only inert when nothing chains onto it: dropping
     an antecedent leaves a bare [@else] that no parser accepts, and the branch
     that followed stops applying. *)
  | (When (_, []) | Else (_, [])) when not chained -> true
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Property _ | Layer_decl _ | Layer _ | Media _ | Container _ | Supports _
  | Moz_document _ | When _ | Else _ | Starting_style _ | Supports_condition _
  | Origin _ | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
  | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
  | Font_palette_values _ | Font_feature_values _ | View_transition _
  | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      false

(* CSS Paged Media 3 sec. 5 generates a page-margin box only where its [content]
   computes away from [none], so a box holding no descriptor generates nothing.
   Stripping those first lets [is_empty_statement] drop the page they leave with
   neither descriptor nor box. *)
let drop_empty_margin_boxes stmt =
  match stmt with
  | Page_with_margins (selector, descriptors, margins) ->
      let kept =
        list_filter_preserve
          (fun (m : page_margin_rule) -> m.descriptors <> [])
          margins
      in
      if kept == margins then stmt
      else Page_with_margins (selector, descriptors, kept)
  | stmt -> stmt

(* The tail is filtered first, so a branch is judged against the [@else] that
   survives rather than the one written: an empty [@when] whose only [@else] is
   itself empty goes with it in one pass. *)
let rec drop_empty_rules stmts =
  match stmts with
  | [] -> []
  | stmt :: rest ->
      let rest' = drop_empty_rules rest in
      let stmt' = drop_empty_margin_boxes stmt in
      let chained =
        match rest' with Else _ :: _ -> true | [] | _ :: _ -> false
      in
      if is_empty_statement ~chained stmt' then rest'
      else if rest' == rest && stmt' == stmt then stmts
      else stmt' :: rest'

(* CSS Cascade 5 sec. 2: a [@layer <name>;] declaration form is prelude-friendly
   and may interleave with [@charset] / [@import] / [@namespace], so a
   [Layer_decl] before [@import] / [@namespace] must not flip [seen_body] -
   otherwise the following [@import] gets dropped as misplaced. *)
let drop_misplaced_imports stmts =
  let seen_body = ref false in
  let seen_import_or_namespace = ref false in
  list_filter_preserve
    (fun stmt ->
      match stmt with
      | (Charset _ | Import _ | Namespace _) when not !seen_body ->
          (match stmt with
          | Import _ | Namespace _ -> seen_import_or_namespace := true
          | _ -> ());
          true
      | Import _ -> false
      | Charset _ | Namespace _ -> true
      | Layer_decl _ when not !seen_import_or_namespace -> true
      | _ ->
          seen_body := true;
          true)
    stmts

(* CSS Cascade 5 sec 6.4.2: same-name [@layer] blocks in the same enclosing
   context accumulate into one layer. Merge them at every level (top-level and
   nested inside @media / @supports / @container) but only when the first
   occurrence is non-empty - leading empty blocks fold into Layer_decl
   downstream. *)
let merge_named_layers_by_name (stmts : statement list) : statement list =
  let is_empty_block = function [] -> true | _ -> false in
  let content : (layer_name, statement list) Hashtbl.t = Hashtbl.create 8 in
  let first_nonempty : (layer_name, unit) Hashtbl.t = Hashtbl.create 8 in
  let has_merge = ref false in
  List.iter
    (fun stmt ->
      match stmt with
      | Layer (Some name, block) when not (is_empty_block block) ->
          let prev =
            Hashtbl.find_opt content name |> Option.value ~default:[]
          in
          if prev <> [] then has_merge := true;
          Hashtbl.replace content name (prev @ block);
          if not (Hashtbl.mem first_nonempty name) then
            Hashtbl.add first_nonempty name ()
      | _ -> ())
    stmts;
  if not !has_merge then stmts
  else
    let emitted = Hashtbl.create 8 in
    List.filter_map
      (fun stmt ->
        match stmt with
        | Layer (Some name, block) when not (is_empty_block block) ->
            if Hashtbl.mem emitted name then None
            else begin
              Hashtbl.add emitted name ();
              Some (Layer (Some name, Hashtbl.find content name))
            end
        | _ -> Some stmt)
      stmts
