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

(* Join each run of adjacent blocks that [prelude] reads as one prelude, by
   [same], into one block over their bodies in order, under the last block's
   prelude, and hand the joined body to [optimize_merged_block]. A block with no
   neighbour to join keeps its node, and a pass that joins nothing returns
   [stmts]. *)
let merge_adjacent ~prelude ~same ~build ~optimize_merged_block stmts =
  let rec needs_merge = function
    | a :: (b :: _ as rest) -> (
        match (prelude a, prelude b) with
        | Option.Some (k, _), Option.Some (k', _) when same k k' -> true
        | _ -> needs_merge rest)
    | [] | [ _ ] -> false
  in
  (* A run is its first statement, the prelude so far and its bodies, last
     first. *)
  let flush acc = function
    | Option.None -> acc
    | Option.Some (first, _, [ _ ]) -> first :: acc
    | Option.Some (_, key, bodies) ->
        build key (optimize_merged_block (List.concat (List.rev bodies))) :: acc
  in
  let rec merge acc run = function
    | [] -> List.rev (flush acc run)
    | stmt :: rest -> (
        match (prelude stmt, run) with
        | Option.Some (key, body), Option.Some (first, key', bodies)
          when same key' key ->
            merge acc (Option.Some (first, key, body :: bodies)) rest
        | Option.Some (key, body), _ ->
            merge (flush acc run) (Option.Some (stmt, key, [ body ])) rest
        | Option.None, _ -> merge (stmt :: flush acc run) Option.None rest)
  in
  if needs_merge stmts then merge [] Option.None stmts else stmts

(* CSS Cascade 6.4: consecutive named [@layer] blocks with the same name are
   spec-equivalent to a single block. Anonymous layers stay distinct because
   each [@layer { ... }] without a name creates a new layer. *)
let merge_consecutive_layers ?(optimize_merged_block = Fun.id) stmts =
  merge_adjacent
    ~prelude:(function
      | Layer (Some name, block) -> Option.Some (name, block) | _ -> Option.None)
    ~same:equal_layer_name
    ~build:(fun name block -> Layer (Some name, block))
    ~optimize_merged_block stmts

(* Media Queries 4 sec. 2: two adjacent [@media] blocks with one condition are
   evaluated identically, so concatenating their bodies in order is the same
   cascade. Nothing in the bodies bears on that, preference queries included:
   what a nested query gates is gated the same either way, and no statement
   moves past another. [merge_distant_media] is the pass that hoists across an
   intervening statement, and it keeps its own gate. *)
let merge_consecutive_media ?(optimize_merged_block = Fun.id) stmts =
  merge_adjacent
    ~prelude:(function
      | Media (cond, block) -> Option.Some (cond, block) | _ -> Option.None)
    ~same:Media.equal
    ~build:(fun cond block -> Media (cond, block))
    ~optimize_merged_block stmts

(* Cross statements whose cascade is pure source order, plain rules and
   conditional groups, and statements that take no part in the order of style
   rules: a comment, and a [@keyframes], [@font-face] or [@property] definition,
   which a block of style rules alone may move past ({!carries_style_only}). A
   scope body is crossed too: CSS Cascade 6 sec. 3.1 decides a scoped
   declaration against an unscoped one by specificity or scope proximity, never
   by order of appearance. Layers, origins, imports, etc. establish
   ordering/context that hoisting past would change. *)
let crossable_by_distant_merge = function
  | Rule _ | Declarations _ | Media _ | Supports _ | Container _ -> true
  | Bang_comment _ | Property _ | Keyframes _ | Webkit_keyframes _
  | Moz_keyframes _ | Font_face _ | Scope _ ->
      true
  | Charset _ | Import _ | Namespace _ | Layer_decl _ | Layer _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      false

(* A hoisted block moves its statements above the ones it crosses. Style rules
   and the groups of them move by what they write, which the hoist checks; a
   definition moves past another of its name, and the last [@keyframes] of a
   name is the one that runs, so a block holding anything else stays. *)
let rec carries_style_only = function
  | Rule r -> List.for_all carries_style_only r.nested
  | Declarations _ | Bang_comment _ -> true
  | Media (_, block)
  | Supports (_, block)
  | Container (_, _, block)
  | Starting_style block ->
      List.for_all carries_style_only block
  | Charset _ | Import _ | Namespace _ | Property _ | Layer_decl _ | Layer _
  | Moz_document _ | When _ | Else _ | Supports_condition _ | Origin _ | Scope _
  | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      false

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

   [owner] is the style rule whose body [stmts] is, when it is one, under the
   selector the cascade weighs it by. A declarations run in that body sets
   properties on [owner] (CSS Nesting 1 sec. 3.4), so the analysis below only
   sees it once it can name the rule the run belongs to, and a rule nested in
   the body weighs [&] as that selector.

   A block merges into the first kept block of its prelude, its partner, and so
   crosses every statement kept after that. A pass keeps statements by position
   and reads each one's leaves once a hoist first needs them, from [unread] on:
   [index] holds the leaves of every kept statement but a partner, a partner
   holds its own, which grow as blocks merge into it, and [blocked] is the last
   position a block may not cross, a statement the cascade does not read in
   source order or one whose leaves refuse.

   A block that merged nothing keeps its node; a partner that took blocks in is
   rebuilt over [optimize_merged_block] of its joined body. *)
type 'k partner = {
  at : int;
  first : statement;  (** the block as written *)
  mutable key : 'k;
  mutable chunks : statement list list;  (** the body, last chunk first *)
  leaves : Rule_graph.facts_index;
}

type 'k kept =
  | Plain of statement
  | Block of statement * 'k * statement list
  | Partner of 'k partner

type 'k pass = {
  build : 'k -> statement list -> statement;
  parent : Selector.t option;
  kept : 'k kept Dynarray.t;
  index : Rule_graph.facts_index;
  mutable partners : 'k partner list;
  mutable blocked : int;
  mutable unread : int;
}

let body p = List.concat (List.rev p.chunks)

let facts pass stmts =
  Option.map
    (List.map (fun r -> Rule_graph.facts r))
    (Nest.leaves ?parent:pass.parent stmts)

(* The statements of a crossed [stmt] an unscoped hoisted declaration can meet
   in order of appearance: a scope body's, at the top of a sheet, are none (CSS
   Cascade 6 sec. 3.1). Under a parent the hoisted rules may be scoped
   themselves, so the body is read, which refuses. *)
let rec unscoped_parts pass = function
  | Scope _ when Option.is_none pass.parent -> []
  | Media (_, block) | Supports (_, block) | Container (_, _, block) ->
      List.concat_map (unscoped_parts pass) block
  | stmt -> [ stmt ]

let crossed_facts pass stmt = facts pass (unscoped_parts pass stmt)

let read_kept pass =
  for at = pass.unread to Dynarray.length pass.kept - 1 do
    let stmt, into =
      match Dynarray.get pass.kept at with
      | Plain stmt -> (stmt, pass.index)
      | Block (stmt, _, _) -> (stmt, pass.index)
      | Partner p -> (pass.build p.key (body p), p.leaves)
    in
    match crossed_facts pass stmt with
    | Option.Some rules when crossable_by_distant_merge stmt ->
        List.iter (Rule_graph.index_facts into ~at) rules
    | Option.Some _ | Option.None -> pass.blocked <- at
  done;
  pass.unread <- Dynarray.length pass.kept

let crosses pass ~after h =
  Rule_graph.conflicts_after pass.index ~after h
  || List.exists
       (fun q -> Rule_graph.conflicts_after q.leaves ~after h)
       pass.partners

(* Merge [block], under [key], into its partner [p] when nothing kept after [p]
   stops it. *)
let merge pass p key block =
  read_kept pass;
  pass.blocked <= p.at
  &&
  match facts pass block with
  | Option.None -> false
  | Option.Some hoisted ->
      (not (List.exists (crosses pass ~after:p.at) hoisted))
      && begin
        List.iter (Rule_graph.index_facts p.leaves ~at:p.at) hoisted;
        p.key <- key;
        p.chunks <- block :: p.chunks;
        true
      end

(* Keep a block of the family, or merge it into its partner; [true] when it has
   one. *)
let keep_block pass ~equal_prelude ~hoistable stmt key block =
  match List.find_opt (fun p -> equal_prelude p.key key) pass.partners with
  | Option.None ->
      let p =
        {
          at = Dynarray.length pass.kept;
          first = stmt;
          key;
          chunks = [ block ];
          leaves = Rule_graph.facts_index ();
        }
      in
      pass.partners <- p :: pass.partners;
      Dynarray.add_last pass.kept (Partner p);
      false
  | Option.Some p ->
      if
        not
          (hoistable block
          && List.for_all carries_style_only block
          && merge pass p key block)
      then Dynarray.add_last pass.kept (Block (stmt, key, block));
      true

let merge_distant ~prelude ~equal_prelude ~build ~hoistable ?owner
    ~optimize_merged_block stmts =
  let pass =
    {
      build;
      parent = Option.map (fun (r : rule) -> r.selector) owner;
      kept = Dynarray.create ();
      index = Rule_graph.facts_index ();
      partners = [];
      blocked = -1;
      unread = 0;
    }
  in
  let partnered =
    List.fold_left
      (fun partnered stmt ->
        match prelude stmt with
        | Option.None ->
            Dynarray.add_last pass.kept (Plain stmt);
            partnered
        | Option.Some (key, block) ->
            keep_block pass ~equal_prelude ~hoistable stmt key block
            || partnered)
      false stmts
  in
  if not partnered then stmts
  else
    Dynarray.to_list pass.kept
    |> List.map (function
      | Plain stmt | Block (stmt, _, _) -> stmt
      | Partner { first; chunks = [ _ ]; _ } -> first
      | Partner p -> build p.key (optimize_merged_block (body p)))
    |> List.preserve stmts

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
let merge_consecutive_supports ?(optimize_merged_block = Fun.id) stmts =
  merge_adjacent
    ~prelude:(function
      | Supports (cond, block) -> Option.Some (cond, block) | _ -> Option.None)
    ~same:Supports.equal
    ~build:(fun cond block -> Supports (cond, block))
    ~optimize_merged_block stmts

(* The whole [@container] merge gate: a name and a condition, read once to
   decide the pass has work and once per adjacent pair. A [<container-name>] is
   a [<custom-ident>] (Conditional Rules 5 sec. 5.4), so the name the parser
   unescaped is its identity; [Container.equal] reads the condition's normalised
   structure rather than its serialised text, which an unknown container feature
   can repeat without meaning it. *)
let same_container (prev_name, prev_cond) (name, cond) =
  Option.equal String.equal prev_name name
  && Option.equal Container.equal prev_cond cond

let merge_consecutive_containers ?(optimize_merged_block = Fun.id) stmts =
  merge_adjacent
    ~prelude:(function
      | Container (name, cond, block) -> Option.Some ((name, cond), block)
      | _ -> Option.None)
    ~same:same_container
    ~build:(fun (name, cond) block -> Container (name, cond, block))
    ~optimize_merged_block stmts

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
let merge_consecutive_starting_style ?(optimize_merged_block = Fun.id) stmts =
  merge_adjacent
    ~prelude:(function
      | Starting_style block -> Option.Some ((), block) | _ -> Option.None)
    ~same:(fun () () -> true)
    ~build:(fun () block -> Starting_style block)
    ~optimize_merged_block stmts

(* Whether a layer block holds nothing the cascade can act on. A rule that
   writes no declarations of its own still carries whatever it nests, so reading
   only the declarations called a layer empty and deleted the CSS under it. *)
let is_layer_empty (block : statement list) : bool =
  List.for_all
    (function
      | Rule { declarations = []; nested = []; _ } -> true
      | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
      | Namespace _ | Property _ | Layer_decl _ | Layer _ | Media _
      | Container _ | Supports _ | Moz_document _ | Starting_style _ | When _
      | Else _ | Supports_condition _ | Origin _ | Scope _ | Keyframes _
      | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
      | Page _ | Page_with_margins _ | Font_palette_values _
      | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
      | Unknown_at_rule _ ->
          false)
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
  List.preserve stmts (merge [] stmts)

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
  | Layer (None, _)
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Media _ | Container _ | Supports _
  | Moz_document _ | Starting_style _ | When _ | Else _ | Supports_condition _
  | Origin _ | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
  | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
  | Font_palette_values _ | Font_feature_values _ | View_transition _
  | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      []

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
  List.preserve stmts (loop [] [] stmts)

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

(* A misplaced [@charset], [@import] or [@namespace] is invalid and a browser
   ignores it, so it goes. *)
let drop_misplaced_prelude stmts =
  let prelude = ref Stylesheet_read.prelude_start in
  list_filter_preserve
    (fun stmt ->
      let misplaced = Stylesheet_read.misplaced !prelude stmt in
      prelude := Stylesheet_read.prelude_next !prelude stmt;
      Option.is_none misplaced)
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
