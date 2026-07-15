(** Statement-block cleanup passes. *)

open Stylesheet
open Common

let list_filter_preserve = List.filter_preserve

let media_feature_is name (f : Media.feature) =
  match f with
  | Media.Plain (n, _) | Media.Boolean n -> n = name
  | Media.Range (n, _, _) | Media.Range_rev (_, _, n) -> n = name
  | Media.Interval (_, _, n, _, _) -> n = name

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
    when String.equal prev_name name ->
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
        | Some (Some prev_name, prev_block) when String.equal prev_name name ->
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

let merge_consecutive_layers ~optimize_merged_block (stmts : statement list) :
    statement list =
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

let merge_consecutive_media ~optimize_merged_block (stmts : statement list) :
    statement list =
  if needs_media_merge stmts then
    merge_media_blocks ~optimize_merged_block stmts
  else stmts

(* Leaf rules reachable from a statement, descending into nested blocks. *)
let rec leaf_rules stmt =
  match stmt with
  | Rule r -> r :: List.concat_map leaf_rules r.nested
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
      List.concat_map leaf_rules b
  | _ -> []

let decl_value_key d =
  ( Declaration.property_name d,
    Declaration.string_of_value ~minify:true d,
    Declaration.is_important d )

(* Two rules whose declarations would reorder unsafely: overlapping selectors
   and a shared property set to a different value. Equal values reorder
   freely. *)
let rules_conflict (r1 : rule) (r2 : rule) =
  Selector_summary.may_overlap
    (Selector_summary.of_selector r1.selector)
    (Selector_summary.of_selector r2.selector)
  && List.exists
       (fun a ->
         let pa = Declaration.property_name a in
         List.exists
           (fun b ->
             Declaration.property_name b = pa
             && decl_value_key a <> decl_value_key b)
           r2.declarations)
       r1.declarations

let needs_distant_media_merge stmts =
  let conds =
    List.filter_map (function Media (c, _) -> Some c | _ -> None) stmts
  in
  let rec dup = function
    | [] -> false
    | c :: rest -> List.exists (Media.equal c) rest || dup rest
  in
  dup conds

(* CSS Conditional 3: same-condition [@media] blocks are spec-equivalent to one
   block. Merge a later block into the first occurrence when hoisting it past
   the intervening statements cannot reorder a conflicting rule. *)
let merge_distant_media ~optimize_merged_block stmts =
  if not (needs_distant_media_merge stmts) then stmts
  else
    let try_merge out cond block : statement list option =
      let hoisted = List.concat_map leaf_rules block in
      let rec split before :
          statement list ->
          (statement list * statement list * statement list) option = function
        | [] -> None
        | Media (c, b) :: after when Media.equal c cond ->
            Some (List.rev before, b, after)
        | x :: rest -> split (x :: before) rest
      in
      (* Only cross statements whose cascade is pure source order: plain rules
         and conditional groups. Layers, origins, scopes, imports, etc.
         establish ordering/context that hoisting past would change. *)
      let crossable = function
        | Rule _ | Declarations _ | Media _ | Supports _ | Container _ -> true
        | _ -> false
      in
      match split [] out with
      | None -> None
      | Some (before, target, after) when List.for_all crossable after ->
          let crossed = List.concat_map leaf_rules after in
          if
            List.exists
              (fun h -> List.exists (rules_conflict h) crossed)
              hoisted
          then None
          else Some (before @ [ Media (cond, target @ block) ] @ after)
      | Some _ -> None
    in
    List.fold_left
      (fun out stmt ->
        match stmt with
        | Media (cond, block) when not (has_nested_preference_media block) -> (
            match try_merge out cond block with
            | Some out' -> out'
            | None -> out @ [ stmt ])
        | _ -> out @ [ stmt ])
      [] stmts
    |> List.map (function
      | Media (c, b) -> Media (c, optimize_merged_block b)
      | s -> s)

(* CSS Conditional Rules 5: adjacent same-condition [@supports] / [@container]
   blocks may be merged because the cascade evaluates them identically. Mirror
   the [@media] approach. *)
let merge_consecutive_supports ~optimize_merged_block (stmts : statement list) :
    statement list =
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

let merge_consecutive_containers ~optimize_merged_block (stmts : statement list)
    : statement list =
  let compare_condition a b =
    match (a, b) with
    | Some a, Some b -> Container.compare a b
    | Some _, None -> 1
    | None, Some _ -> -1
    | None, None -> 0
  in
  let rec needs_merge = function
    | Container (prev_name, prev_cond, _) :: Container (name, cond, _) :: _
      when prev_name = name && compare_condition prev_cond cond = 0 ->
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
            when prev_name = name && compare_condition prev_cond cond = 0 ->
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

(* Check if a layer block contains only empty rules or no statements *)
let is_layer_empty (block : statement list) : bool =
  List.for_all
    (function Rule { declarations = []; _ } -> true | _ -> false)
    block
  || block = []

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
  (* CSS Cascade 5 sec. 6.6.2: [@layer A, B;] declares layers in source order;
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
        if List.exists (String.equal name) seen then (seen, added_rev)
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
    | x :: xs, y :: ys -> String.equal x y && loop (xs, ys)
  in
  loop (prefix, list)

let layer_decl_forward_redundant seen names rest =
  let _, introduced_by_decl = add_new_layer_names seen names in
  let introduced_by_rest, added_by_layer_decl =
    following_layer_introduction_order seen rest
  in
  added_by_layer_decl && list_has_prefix introduced_by_decl introduced_by_rest

let layer_decl_backward_redundant seen names =
  List.for_all (fun name -> List.exists (String.equal name) seen) names

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

(* Main statement processing function with layer optimization *)
(* CSS Cascade 6.1: an empty rule (no declarations, no nested rules) contributes
   nothing, so drop it under [~optimize:true]; an empty [@media]/[@supports]/
   [@container]/[@scope]/[@starting-style] body is likewise removed. An empty
   named [@layer] survives as a [Layer_decl] since the name still orders the
   layer (CSS Cascade L6 6.4). *)
let drop_empty_rules stmts =
  list_filter_preserve
    (function
      | Rule { declarations = []; nested = []; _ } -> false
      | Rule { selector; _ } when Selector.matches_nothing selector -> false
      | Media (_, []) -> false
      | Supports (_, []) -> false
      | Container (_, _, []) -> false
      | Scope (_, _, []) -> false
      | Starting_style [] -> false
      | Page (_, []) -> false
      | Page_with_margins (_, [], []) -> false
      | _ -> true)
    stmts

(* CSS Cascade 5 §6.6.3: a [@layer <name>;] declaration form is prelude-friendly
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
  let content : (string, statement list) Hashtbl.t = Hashtbl.create 8 in
  let first_nonempty : (string, unit) Hashtbl.t = Hashtbl.create 8 in
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
