(** CSS comparison utilities for testing using the proper CSS parser *)

open Cascade

(* ===== Constants ===== *)

let header_comment_start = 3 (* Position after "/*" *)

(* ===== Type Definitions ===== *)

(* Use types from Tree_diff module *)
module D = Tree_diff

let is_empty = D.is_empty

(* Statistics about CSS differences *)
type stats = {
  expected : string;
  actual : string;
  expected_chars : int;
  actual_chars : int;
  added_rules : int;
  removed_rules : int;
  modified_rules : int;
  reordered_rules : int;
  rearranged_rules : int;
  regrouped_rules : int;
  container_changes : int;
  layer_order_swaps : int;
}

(* ===== Helper Functions ===== *)

(** Extract path component for an at-rule statement. Returns Some (path_segment,
    inner_statements) if the statement is an at-rule, None otherwise. *)
let supports_path_and_inner stmt =
  match Css.as_supports stmt with
  | Some (cond, inner) ->
      Some ("@supports " ^ Css.Supports.to_string cond, inner)
  | None -> None

let media_path_and_inner stmt =
  match Css.as_media stmt with
  | Some (cond, inner) -> Some ("@media " ^ Css.Media.to_string cond, inner)
  | None -> None

let layer_path_and_inner stmt =
  match Css.as_layer stmt with
  | Some (name_opt, inner) ->
      let name =
        Option.fold ~none:"" ~some:Css.Stylesheet.string_of_layer_name name_opt
      in
      Some ("@layer " ^ name, inner)
  | None -> None

let container_path_and_inner stmt =
  match Css.as_container stmt with
  | Some (name_opt, cond, inner) ->
      let prefix = match name_opt with Some n -> n ^ " " | None -> "" in
      let cond_str =
        match cond with Some c -> Css.Container.to_string c | None -> ""
      in
      Some ("@container " ^ prefix ^ cond_str, inner)
  | None -> None

let first_some thunks stmt =
  let rec try_each = function
    | [] -> None
    | f :: rest -> (
        match f stmt with Some _ as r -> r | None -> try_each rest)
  in
  try_each thunks

let at_rule_path_and_inner stmt =
  first_some
    [
      supports_path_and_inner;
      media_path_and_inner;
      layer_path_and_inner;
      container_path_and_inner;
    ]
    stmt

let strip_tool_header css =
  (* Strip a leading /*!...*/ header comment with simpler flow to reduce
     nesting *)
  let stripped =
    if not (String.starts_with ~prefix:"/*!" css) then css
    else
      let len = String.length css in
      (* Find the end of the opening header comment "*/" starting at index 3 *)
      let rec find_comment_end i =
        if i + 1 >= len then None
        else if css.[i] = '*' && css.[i + 1] = '/' then Some (i + 2)
        else find_comment_end (i + 1)
      in
      match find_comment_end header_comment_start with
      | None -> css
      | Some j ->
          let start_pos = if j < len && css.[j] = '\n' then j + 1 else j in
          if start_pos >= len then ""
          else String.sub css start_pos (len - start_pos)
  in
  (* Trim trailing whitespace for consistent comparison *)
  String.trim stripped

(* The canonical projection sorts a run of [@property] rules by name (see
   [Css.canonicalize_rule_order]): CSS Properties and Values API 1 sec. 2 makes
   registrations for different names order-independent. A caller that wants to
   assert its own emission order should inspect the AST, or use mode [`Tree]. *)

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)

let tree_diff ~(expected : Css.t) ~(actual : Css.t) : Tree_diff.t =
  D.diff ~expected ~actual

(* Canonical mode reparses generated CSS, so these indexes identify neither
   source AST. Keep the move, but not coordinates or an index-derived partner
   that a reader cannot locate in either input. *)
let hide_rule_reorder_position : D.rule_diff -> D.rule_diff = function
  | D.Reordered r ->
      D.Reordered
        { r with expected_pos = -1; actual_pos = -1; swapped_with = None }
  | ( D.Added _ | D.Removed _ | D.Content_changed _ | D.Selector_changed _
    | D.Rearranged _ | D.Regrouped _ ) as change ->
      change

let rec hide_container_reorder_positions : D.container_diff -> D.container_diff
    = function
  | D.Modified change ->
      D.Modified
        {
          change with
          rule_changes = List.map hide_rule_reorder_position change.rule_changes;
          container_changes =
            List.map hide_container_reorder_positions change.container_changes;
        }
  | D.Reordered change ->
      D.Reordered { change with expected_pos = -1; actual_pos = -1 }
  | (D.Added _ | D.Removed _ | D.Block_structure_changed _) as change -> change

let hide_canonical_reorder_positions (diff : D.t) =
  {
    D.rules = List.map hide_rule_reorder_position diff.rules;
    containers = List.map hide_container_reorder_positions diff.containers;
    layer_order = diff.layer_order;
  }

(* The canonical tree supplies normalized content changes; the source tree
   supplies authored ordering changes. Partition the two structured diffs, then
   merge their matching container paths back into one report. *)
let rule_is_reordered : D.rule_diff -> bool = function
  | D.Reordered _ -> true
  | D.Added _ | D.Removed _ | D.Content_changed _ | D.Selector_changed _
  | D.Rearranged _ | D.Regrouped _ ->
      false

let select_rule_reorders keep =
  List.filter (fun change -> Bool.equal keep (rule_is_reordered change))

let rec select_container_reorders keep :
    D.container_diff -> D.container_diff option = function
  | D.Reordered _ as change -> if keep then Some change else None
  | D.Modified change ->
      let rule_changes = select_rule_reorders keep change.rule_changes in
      let container_changes =
        List.filter_map
          (select_container_reorders keep)
          change.container_changes
      in
      if rule_changes = [] && container_changes = [] then None
      else Some (D.Modified { change with rule_changes; container_changes })
  | (D.Added _ | D.Removed _ | D.Block_structure_changed _) as change ->
      if keep then None else Some change

let select_reorders keep (diff : D.t) =
  {
    D.rules = select_rule_reorders keep diff.rules;
    containers =
      List.filter_map (select_container_reorders keep) diff.containers;
    layer_order = (if keep then None else diff.layer_order);
  }

let without_reorders = select_reorders false
let only_reorders = select_reorders true

let same_container (a : D.container_info) (b : D.container_info) =
  a.container_type = b.container_type && String.equal a.condition b.condition

(* A conditional block can contain both a normalized content change and a source
   reorder. Join those entries instead of printing the block twice. The tag
   claims a target after one merge, so repeated equal conditions pair by
   occurrence rather than all folding into the first block. *)
let rec merge_container_change source = function
  | [] -> [ (true, source) ]
  | (false, D.Modified target) :: rest -> (
      match source with
      | D.Modified additions when same_container target.info additions.info ->
          ( true,
            D.Modified
              {
                target with
                rule_changes = target.rule_changes @ additions.rule_changes;
                container_changes =
                  merge_container_changes target.container_changes
                    additions.container_changes;
              } )
          :: rest
      | _ -> (false, D.Modified target) :: merge_container_change source rest)
  | target :: rest -> target :: merge_container_change source rest

and merge_container_changes canonical source =
  let tagged = List.map (fun change -> (false, change)) canonical in
  List.fold_left
    (fun changes addition -> merge_container_change addition changes)
    tagged source
  |> List.map snd

let merge_source_reorders (canonical : D.t) (source : D.t) =
  {
    D.rules = canonical.rules @ source.rules;
    containers = merge_container_changes canonical.containers source.containers;
    layer_order = canonical.layer_order;
  }

(* A container entry is named by its printed prelude, and the projection keys an
   [@media] or [@container] prelude to the Level 4 form a source sheet need not
   write, so the two trees can name one block two ways. Key the source side the
   same before comparing them: this respells a prelude and moves nothing. *)
let key_prelude stmt =
  match Css.as_media stmt with
  | Some (query, inner) ->
      Css.media ~condition:(Css.Media.lower_for_minify query) inner
  | None -> (
      match Css.as_container stmt with
      | Some (name, Some condition, inner) ->
          Css.container ?name
            ~condition:(Css.Container.lower_for_minify condition)
            inner
      | Some (_, None, _) | None -> stmt)

let rec key_query_preludes stmts =
  List.map
    (fun stmt ->
      Css.Stylesheet.map_statement_children key_query_preludes
        (key_prelude stmt))
    stmts

type order_gate = {
  reordered_here : bool;
  blocks : (D.container_info * order_gate) list;
}
(* Whether the projection still disagrees about the order at one level, and the
   same answer for every block below it. The projection sorts each run whose
   order no element can observe, so a level it leaves in two orders holds a move
   an engine can see, and a level it agrees on holds none. Which statement is
   named as the one that moved stays the inputs' account: a swap has two
   participants and the two trees need not pick the same one. *)

let container_is_reordered : D.container_diff -> bool = function
  | D.Reordered _ -> true
  | D.Modified _ | D.Added _ | D.Removed _ | D.Block_structure_changed _ ->
      false

let rec gate_of_level rule_changes container_changes =
  {
    reordered_here =
      List.exists rule_is_reordered rule_changes
      || List.exists container_is_reordered container_changes;
    blocks = List.filter_map gate_of_container container_changes;
  }

and gate_of_container :
    D.container_diff -> (D.container_info * order_gate) option = function
  | D.Modified change ->
      Some
        (change.info, gate_of_level change.rule_changes change.container_changes)
  (* Blocks merged or split: the walk below cannot pair them up, so this level
     answers that it cannot tell rather than that nothing moved. *)
  | D.Block_structure_changed change ->
      Some
        ( {
            D.container_type = change.container_type;
            condition = change.condition;
            rules = [];
          },
          { reordered_here = true; blocks = [] } )
  | D.Reordered _ | D.Added _ | D.Removed _ -> None

let gate_of_canonical (diff : D.t) = gate_of_level diff.rules diff.containers

(* A block the canonical tree does not name is one the projection made the same
   on both sides, so nothing inside it moved. The exception is a nested rule,
   which the projection flattens away: its statements answer to the level that
   receives them. *)
let gate_for gate (info : D.container_info) =
  match info.container_type with
  | `Nesting -> Some gate
  | `Media | `Layer | `Supports | `Container | `Property | `At_rule -> (
      match List.filter (fun (i, _) -> same_container i info) gate.blocks with
      | [] -> None
      | matches ->
          Some
            {
              reordered_here =
                List.exists (fun (_, g) -> g.reordered_here) matches;
              blocks = List.concat_map (fun (_, g) -> g.blocks) matches;
            })

let gated_rule_changes gate changes =
  if gate.reordered_here then changes
  else List.filter (fun change -> not (rule_is_reordered change)) changes

let rec gated_container gate : D.container_diff -> D.container_diff option =
  function
  | D.Reordered _ as change -> if gate.reordered_here then Some change else None
  | D.Modified change -> (
      match gate_for gate change.info with
      | None -> None
      | Some gate ->
          let rule_changes = gated_rule_changes gate change.rule_changes in
          let container_changes =
            List.filter_map (gated_container gate) change.container_changes
          in
          if rule_changes = [] && container_changes = [] then None
          else Some (D.Modified { change with rule_changes; container_changes })
      )
  | (D.Added _ | D.Removed _ | D.Block_structure_changed _) as change ->
      Some change

let gate_source_reorders gate (diff : D.t) =
  {
    D.rules = gated_rule_changes gate diff.rules;
    containers = List.filter_map (gated_container gate) diff.containers;
    layer_order = diff.layer_order;
  }

(* Collect all rules with their path-qualified selector keys *)
let rec collect_keyed_rules acc path stmts =
  List.fold_left
    (fun acc stmt ->
      match Css.as_rule stmt with
      | Some (sel, decls, _) ->
          let key = String.concat " " (path @ [ Css.Selector.to_string sel ]) in
          (key, decls) :: acc
      | None -> (
          match at_rule_path_and_inner stmt with
          | Some (segment, inner) ->
              collect_keyed_rules acc (path @ [ segment ]) inner
          | None -> acc))
    acc stmts

let sig_of_decls decls =
  decls
  |> List.map (fun d -> (Css.declaration_name d, Css.declaration_value d))
  |> List.sort (fun (a1, b1) (a2, b2) ->
      let c = String.compare a1 a2 in
      if c <> 0 then c else String.compare b1 b2)

let reported_declaration d =
  let value = Css.declaration_value ~minify:false d in
  let value =
    if Css.declaration_is_important d then value ^ " !important" else value
  in
  (Css.declaration_name d, value)

let group_into_table rules = Group.by Fun.id rules

(* The keys the expected side names, in the order it names them. Walking [tbl1]
   with [Hashtbl.iter] instead would order the report by the stdlib hash and the
   table's capacity, neither of which is a fact about the sheet. *)
let keys_in_source_order rules = Group.keys Fun.id rules

(* Compare two declaration lists with the same key and emit diffs *)
let diff_same_key_pair key d1 d2 =
  let sig1 = sig_of_decls d1 in
  let sig2 = sig_of_decls d2 in
  if
    sig1 = sig2
    && (not (List.equal Declaration.equal_declaration d1 d2))
    && D.reorder_is_significant d1 d2
  then
    Some
      (D.Reordered
         {
           selector = key;
           expected_pos = -1;
           actual_pos = -1;
           swapped_with = None;
           old_declarations = Some d1;
           new_declarations = Some d2;
         }
        : D.rule_diff)
  else if sig1 <> sig2 then
    Some
      (D.Content_changed
         {
           selector = key;
           old_declarations = d1;
           new_declarations = d2;
           property_changes = [];
           added_properties =
             List.filter_map
               (fun d ->
                 let p = Css.declaration_name d in
                 if List.mem_assoc p sig1 then None
                 else Some (reported_declaration d))
               d2;
           removed_properties =
             List.filter_map
               (fun d ->
                 let p = Css.declaration_name d in
                 if List.mem_assoc p sig2 then None
                 else Some (reported_declaration d))
               d1;
         })
  else None

let diff_count_mismatch key ds1 ds2 =
  let n1 = List.length ds1 in
  let n2 = List.length ds2 in
  if n2 > n1 then
    (D.Added
       { selector = key ^ " (duplicate)"; declarations = List.nth ds2 (n2 - 1) }
      : D.rule_diff)
  else
    (D.Removed
       { selector = key ^ " (missing)"; declarations = List.nth ds1 (n1 - 1) }
      : D.rule_diff)

(* Detect declaration-reordering-only differences throughout a stylesheet *)
let collect_pairwise_diffs ~diffs key ds1 ds2 =
  List.iter2
    (fun d1 d2 ->
      match diff_same_key_pair key d1 d2 with
      | Some d -> diffs := d :: !diffs
      | None -> ())
    ds1 ds2

let collect_key_diffs ~tbl2 ~diffs key ds1 =
  match Hashtbl.find_opt tbl2 key with
  | Some ds2 when List.length ds1 = List.length ds2 ->
      collect_pairwise_diffs ~diffs key ds1 ds2
  | Some ds2 -> diffs := diff_count_mismatch key ds1 ds2 :: !diffs
  | None -> ()

let build_reorder_diff expected_css actual_css =
  let rules1 =
    collect_keyed_rules [] [] (Css.statements expected_css) |> List.rev
  in
  let rules2 =
    collect_keyed_rules [] [] (Css.statements actual_css) |> List.rev
  in
  let tbl1 = group_into_table rules1 in
  let tbl2 = group_into_table rules2 in
  let diffs = ref [] in
  List.iter
    (fun key ->
      Option.iter
        (collect_key_diffs ~tbl2 ~diffs key)
        (Hashtbl.find_opt tbl1 key))
    (keys_in_source_order rules1);
  if !diffs = [] then None
  else Some D.{ rules = List.rev !diffs; containers = []; layer_order = None }

(* [css] is value text neither side has parsed yet, so the declaration context
   [property] names is given as text too. CSS Syntax 3 (ED) sec. 4.3.7 lets an
   escape carry a [;] or a [}] into a custom property's name: written raw here
   such a name ends the declaration or closes the rule, and both values go with
   it, so the name is spelled the way the printer spells it. *)
let css_for_semantic_comparison ?property css =
  match property with
  | None -> css
  | Some property ->
      String.concat "" [ ":root{"; Parser.escape_ident property; ":"; css; "}" ]

let canonical_of_stylesheet ~lossless ~prune_unused_custom_props stylesheet =
  try
    let optimize stylesheet =
      (* Regrouping - factoring a shared declaration into a selector list,
         synthesising nesting from adjacent rules - depends on how the input
         happened to order its rules, so it is not confluent: the same sheet
         written either way would canonicalise differently. The projection skips
         it.

         [~enforce_spec:true] holds off the rewrites the optimizer justifies
         with what maintained browsers support, because they delete content:
         unwrapping a baseline-true [@supports] leaves the declaration written
         before the guard dead, dropping a vendor-prefixed declaration leaves
         the engine that needs the prefix nothing, and clearing an [@import
         supports()] guard decides the sheet always loads. An engine without the
         feature reads exactly what each of those deletes, so two sheets that
         disagree there paint differently and the projection has to keep them
         apart. The respellings gated with them - [min-X] into the range form,
         the Level 3 [not all and (X)] - delete nothing, and
         {!Css.canonicalize_rule_order} applies those on the comparison side
         instead. *)
      Css.optimize ~lossless ~regroup:false ~enforce_spec:true
        ~prune_unused_custom_props stylesheet
    in
    (* A declaration run written after a nested statement first needs the
       nesting-aware commute check: flattening turns the statement boundary into
       a top-level rule boundary. Both passes disable regrouping, so this
       normalization cannot synthesize the nesting the projection removes. *)
    Some
      (stylesheet |> optimize |> Css.flatten_nesting |> optimize
     |> Css.canonicalize_rule_order
      |> Css.to_string ~minify:true ~lossless)
  with Invalid_argument _ -> None

(* Parse both sides before canonicalising either. A caller that retries at a
   different strictness needs only to know that one side failed to parse, and
   canonicalising the other first is the whole pipeline's work thrown away. *)
let canonical_both ~strict ~lossless ~prune_unused_custom_props expected actual
    =
  match (Css.of_string ~strict expected, Css.of_string ~strict actual) with
  | Ok { stylesheet = expected; _ }, Ok { stylesheet = actual; _ } -> (
      let canonical =
        canonical_of_stylesheet ~lossless ~prune_unused_custom_props
      in
      match (canonical expected, canonical actual) with
      | Some expected, Some actual -> Some (expected, actual)
      | _ -> None)
  | _ -> None

let canonical_pair ~strict ~lossless expected actual =
  canonical_both ~strict ~lossless ~prune_unused_custom_props:false expected
    actual
  |> Option.map (fun (expected, actual) -> String.equal expected actual)

let canonical_diff_inputs ~strict ~lossless ?(prune_unused_custom_props = false)
    expected actual =
  canonical_both ~strict ~lossless ~prune_unused_custom_props expected actual

let canonical_diff_inputs_with_fallback ~lossless
    ?(prune_unused_custom_props = false) expected actual =
  match
    canonical_diff_inputs ~strict:true ~lossless ~prune_unused_custom_props
      expected actual
  with
  | Some _ as result -> result
  | None ->
      canonical_diff_inputs ~strict:false ~lossless ~prune_unused_custom_props
        expected actual

(* Internal: full-stylesheet equality under the canonical minified form. *)
let semantic_equal ?property ?(lossless = false) expected actual =
  let expected = strip_tool_header expected in
  let actual = strip_tool_header actual in
  if expected = actual then true
  else
    let expected_css = css_for_semantic_comparison ?property expected in
    let actual_css = css_for_semantic_comparison ?property actual in
    match canonical_pair ~strict:true ~lossless expected_css actual_css with
    | Some equal -> equal
    | None -> (
        match
          canonical_pair ~strict:false ~lossless expected_css actual_css
        with
        | Some equal -> equal
        | None -> false)

let equivalent_value ?lossless ~property a b =
  semantic_equal ?lossless ~property a b

(* Parse two CSS strings and return their diff or parse errors *)
type result =
  | Tree_diff of Tree_diff.t (* CSS AST differences found *)
  | String_diff of String_diff.t (* No structural diff but strings differ *)
  | No_diff (* No difference under the selected mode *)
  | Both_errors of Error.t * Error.t
  | Expected_error of Error.t
  | Actual_error of Error.t

type t = {
  result : result;
  expected_warnings : Error.t list;
  actual_warnings : Error.t list;
}

type mode = [ `Auto | `Tree | `String | `Canonical ]

let fallback_to_string_diff ~expected ~actual =
  (* Use original (header-stripped) strings for string diff *)
  match String_diff.diff ~expected actual with
  | Some sdiff -> String_diff sdiff
  | None ->
      failwith "BUG: different strings but String_diff found no difference"

let diff_after_empty_structural ~expected ~actual ~expected_norm ~actual_norm =
  (* Structural diff is empty but strings differ - attempt to classify
     declaration ordering-only differences throughout the stylesheet
     (recursively inside containers) as structural Reordered changes. If none
     detected, fall back to string diff. *)
  match build_reorder_diff expected_norm actual_norm with
  | Some d -> Tree_diff d
  | None -> fallback_to_string_diff ~expected ~actual

let diff_two_parsed ~expected ~actual ~expected_ast ~actual_ast =
  (* Order between [@property] registrations for different names carries no
     meaning; the canonical projection sorts them. *)
  let expected_norm = expected_ast in
  let actual_norm = actual_ast in
  let structural_diff = tree_diff ~expected:expected_norm ~actual:actual_norm in
  if not (is_empty structural_diff) then Tree_diff structural_diff
  else diff_after_empty_structural ~expected ~actual ~expected_norm ~actual_norm

let diff_auto ~expected ~actual ~expected_parse ~actual_parse =
  (* First check if original strings are identical *)
  if expected = actual then No_diff
  else
    match (expected_parse, actual_parse) with
    | ( Ok { Css.stylesheet = expected_ast; _ },
        Ok { Css.stylesheet = actual_ast; _ } ) ->
        diff_two_parsed ~expected ~actual ~expected_ast ~actual_ast
    | Error e1, Error e2 -> Both_errors (e1, e2)
    | Ok _, Error e -> Actual_error e
    | Error e, Ok _ -> Expected_error e

(* The two canonical forms already differ, so this only picks how to say it: the
   tree diff when its walk reaches the divergence, the bytes themselves when it
   does not. A tree diff that comes back empty over two differing canonical
   forms is a blind spot in the walk, and the string diff of the forms is what
   names it. *)
let diff_canonical_parsed ~expected ~actual ~expected_parse ~actual_parse
    ~expected_canon ~actual_canon =
  match (Css.of_string expected_canon, Css.of_string actual_canon) with
  | Ok { stylesheet = expected_ast; _ }, Ok { stylesheet = actual_ast; _ } ->
      let canonical_tree =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      let canonical_diff = without_reorders canonical_tree in
      (* A move has to be one the inputs hold and one the cascade can see. The
         source tree answers the first, the projection the second, and a move
         missing from either is not reported: neither the churn the projection's
         own ordering creates around a content change, nor a run the projection
         is free to sort, which reads the same however either sheet writes
         it. *)
      let gate = gate_of_canonical canonical_tree in
      let source_reorders =
        match (expected_parse, actual_parse) with
        | ( Ok { Css.stylesheet = expected_source; _ },
            Ok { Css.stylesheet = actual_source; _ } ) ->
            tree_diff
              ~expected:(key_query_preludes expected_source)
              ~actual:(key_query_preludes actual_source)
            |> only_reorders |> gate_source_reorders gate
        | Error _, _ | _, Error _ ->
            { D.rules = []; containers = []; layer_order = None }
      in
      let structural_diff =
        merge_source_reorders canonical_diff source_reorders
        |> hide_canonical_reorder_positions
      in
      if is_empty structural_diff then
        fallback_to_string_diff ~expected:expected_canon ~actual:actual_canon
      else Tree_diff structural_diff
  | _ -> diff_auto ~expected ~actual ~expected_parse ~actual_parse

let diff_canonical ~lossless ~prune_unused_custom_props ~expected ~actual
    ~expected_parse ~actual_parse =
  if expected = actual then No_diff
  else
    match
      canonical_diff_inputs_with_fallback ~lossless ~prune_unused_custom_props
        expected actual
    with
    | None -> diff_auto ~expected ~actual ~expected_parse ~actual_parse
    | Some (expected_canon, actual_canon) ->
        (* The canonical form is what this mode compares: equivalent inputs
           reach one form, so two forms that differ are two different
           stylesheets or one projection missing a normalisation key. Either is
           a difference here, and either is a finding - the key is added to the
           projection, the blind spot fixed in the walk. *)
        if String.equal expected_canon actual_canon then No_diff
        else
          diff_canonical_parsed ~expected ~actual ~expected_parse ~actual_parse
            ~expected_canon ~actual_canon

let diff_string ~expected ~actual =
  if expected = actual then No_diff
  else
    match String_diff.diff ~expected actual with
    | Some sdiff -> String_diff sdiff
    | None -> No_diff

let diff_tree ~expected_parse ~actual_parse =
  match (expected_parse, actual_parse) with
  | ( Ok { Css.stylesheet = expected_ast; _ },
      Ok { Css.stylesheet = actual_ast; _ } ) ->
      let structural_diff =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      if is_empty structural_diff then No_diff else Tree_diff structural_diff
  | Error e1, Error e2 -> Both_errors (e1, e2)
  | Ok _, Error e -> Actual_error e
  | Error e, Ok _ -> Expected_error e

let parse_warnings = function
  | Ok { Css.warnings; _ } -> warnings
  | Error _ -> []

let diff ?(mode = `Auto) ?(lossless = false)
    ?(prune_unused_custom_props = false) expected actual =
  let expected = strip_tool_header expected in
  let actual = strip_tool_header actual in
  if expected = actual then
    { result = No_diff; expected_warnings = []; actual_warnings = [] }
  else
    match mode with
    | `String ->
        {
          result = diff_string ~expected ~actual;
          expected_warnings = [];
          actual_warnings = [];
        }
    | (`Auto | `Tree | `Canonical) as mode ->
        let expected_parse = Css.of_string expected in
        let actual_parse = Css.of_string actual in
        let result =
          match mode with
          | `Auto -> diff_auto ~expected ~actual ~expected_parse ~actual_parse
          | `Canonical ->
              diff_canonical ~lossless ~prune_unused_custom_props ~expected
                ~actual ~expected_parse ~actual_parse
          | `Tree -> diff_tree ~expected_parse ~actual_parse
        in
        {
          result;
          expected_warnings = parse_warnings expected_parse;
          actual_warnings = parse_warnings actual_parse;
        }

let equal ?mode ?lossless ?prune_unused_custom_props a b =
  match (diff ?mode ?lossless ?prune_unused_custom_props a b).result with
  | No_diff -> true
  | _ -> false

let as_tree_diff t =
  match t.result with
  | Tree_diff d -> Some d
  | String_diff _ | No_diff | Both_errors _ | Expected_error _ | Actual_error _
    ->
      None

(* Compute statistics from diff results *)
let compute_stats ~expected_str ~actual_str diff_result =
  let expected_chars = String.length expected_str in
  let actual_chars = String.length actual_str in

  match diff_result.result with
  | Tree_diff d ->
      let count_rule_type pred = List.filter pred d.rules |> List.length in
      {
        expected = expected_str;
        actual = actual_str;
        expected_chars;
        actual_chars;
        added_rules =
          count_rule_type (function D.Added _ -> true | _ -> false);
        removed_rules =
          count_rule_type (function D.Removed _ -> true | _ -> false);
        modified_rules =
          count_rule_type (function
            | D.Content_changed _ | D.Selector_changed _ -> true
            | _ -> false);
        reordered_rules =
          count_rule_type (function D.Reordered _ -> true | _ -> false);
        rearranged_rules =
          count_rule_type (function D.Rearranged _ -> true | _ -> false);
        regrouped_rules =
          count_rule_type (function D.Regrouped _ -> true | _ -> false);
        container_changes = List.length d.containers;
        layer_order_swaps =
          (match d.layer_order with
          | None -> 0
          | Some { swapped; _ } -> List.length swapped);
      }
  | _ ->
      (* For non-tree diffs, just return character stats *)
      {
        expected = expected_str;
        actual = actual_str;
        expected_chars;
        actual_chars;
        added_rules = 0;
        removed_rules = 0;
        modified_rules = 0;
        reordered_rules = 0;
        rearranged_rules = 0;
        regrouped_rules = 0;
        container_changes = 0;
        layer_order_swaps = 0;
      }

(* Alias for compute_stats *)
let stats = compute_stats
let add_strings b ls = List.iter (Buffer.add_string b) ls

(* Every warning line starts fresh: what precedes it in the report ends where it
   ends, a snippet's caret row included. *)
let start_line buf =
  if Buffer.length buf > 0 && Buffer.nth buf (Buffer.length buf - 1) <> '\n'
  then Buffer.add_char buf '\n'

(* Two warnings are the same complaint when they fail the same way at the same
   place in the grammar. Where in the byte stream each side ran into it is no
   part of that. *)
(* The rendered kind is built once per warning rather than once per comparison:
   [partition_warnings] compares every pair. *)
type keyed = { warning : Error.t; kind : string }

let keyed (w : Error.t) =
  { warning = w; kind = Pp.to_string Error.pp_kind w.kind }

let same_warning a b =
  Sort.equal a.warning.Error.sort b.warning.Error.sort
  && List.equal String.equal a.warning.Error.path b.warning.Error.path
  && String.equal a.kind b.kind

let rec remove_first p = function
  | [] -> None
  | x :: xs when p x -> Some xs
  | x :: xs -> Option.map (fun rest -> x :: rest) (remove_first p xs)

(* Shared warnings keep the expected side's copy, so the snippet under one is
   the expected side's text. *)
let partition_warnings expected actual =
  let rec go exp_only act_only shared = function
    | [] ->
        let warnings = List.map (fun k -> k.warning) in
        ( warnings (List.rev shared),
          warnings (List.rev exp_only),
          warnings act_only )
    | w :: ws -> (
        match remove_first (same_warning w) act_only with
        | Some act_only -> go exp_only act_only (w :: shared) ws
        | None -> go (w :: exp_only) act_only shared ws)
  in
  go [] (List.map keyed actual) [] (List.map keyed expected)

let pp_warning buf label w =
  start_line buf;
  add_strings buf [ label; " parse warning: "; Error.to_string w; "\n" ]

let pp_overflow buf label hidden =
  start_line buf;
  add_strings buf
    [
      label;
      ": ";
      string_of_int hidden;
      (if hidden = 1 then " more parse warning\n" else " more parse warnings\n");
    ]

let pp_result ?(expected = "Expected") ?(actual = "Actual") ?(color = false)
    ?depth ?entries buf = function
  | Tree_diff d ->
      (* Show structural differences *)
      D.pp ~expected ~actual ~color ?depth ?entries buf d
  | String_diff sdiff ->
      String_diff.pp ~expected_label:expected ~actual_label:actual buf sdiff
  | No_diff -> ()
  | Both_errors (e1, e2) ->
      let err1 = Error.to_string e1 in
      let err2 = Error.to_string e2 in
      if String.equal err1 err2 then
        add_strings buf [ "Both CSS have same parse error: "; err1 ]
      else
        add_strings buf
          [
            "Parse errors:\n  ";
            expected;
            ": ";
            err1;
            "\n  ";
            actual;
            ": ";
            err2;
          ]
  | Expected_error e ->
      add_strings buf [ expected; " CSS parse error: "; Error.to_string e ]
  | Actual_error e ->
      add_strings buf [ actual; " CSS parse error: "; Error.to_string e ]

(* Render parse warnings so a declaration the parser dropped never reads as a
   phantom structural difference on the side that parsed. A warning both sides
   raise is one fact about the input rather than a finding about the diff, so it
   prints once, under a label naming both files, for one slot of the budget.
   One-sided warnings take that budget first: they are what qualifies the
   difference below them. Past [max] the rest are counted rather than printed: a
   stylesheet that trips the same unsupported syntax hundreds of times would
   otherwise bury the diff it is meant to qualify. *)
type warning_side = Expected | Actual | Both

let warnings t =
  let shared, expected_only, actual_only =
    partition_warnings t.expected_warnings t.actual_warnings
  in
  let tag side = List.map (fun w -> (side, w)) in
  tag Expected expected_only @ tag Actual actual_only @ tag Both shared

let pp_warnings ?(expected = "Expected") ?(actual = "Actual") ?max buf t =
  let shared, expected_only, actual_only =
    partition_warnings t.expected_warnings t.actual_warnings
  in
  let remaining = ref (Option.value max ~default:Stdlib.max_int) in
  (* One pass: the count of what was left out is part of the report, so the
     whole list is walked whether or not the budget runs out early. *)
  let render label ws =
    let hidden = ref 0 in
    List.iter
      (fun w ->
        if !remaining > 0 then begin
          decr remaining;
          pp_warning buf label w
        end
        else incr hidden)
      ws;
    if !hidden > 0 then pp_overflow buf label !hidden
  in
  render expected expected_only;
  render actual actual_only;
  render (String.concat "" [ expected; " and "; actual ]) shared

let has_warnings t = t.expected_warnings <> [] || t.actual_warnings <> []

let pp_diff ?(expected = "Expected") ?(actual = "Actual") ?(color = false)
    ?depth ?entries buf t =
  pp_result ~expected ~actual ~color ?depth ?entries buf t.result

let pp ?(expected = "Expected") ?(actual = "Actual") ?(color = false) ?depth
    ?entries buf t =
  (* Warnings come first: a dropped declaration qualifies every line below it,
     and trailing them puts that caveat past the end of a long report. *)
  pp_warnings ~expected ~actual buf t;
  if has_warnings t then Buffer.add_char buf '\n';
  pp_diff ~expected ~actual ~color ?depth ?entries buf t

let add_pct buf char_diff_pct =
  let rounded = Float.round (char_diff_pct *. 10.0) /. 10.0 in
  let s = string_of_float rounded in
  if String.contains s '.' then
    match String.split_on_char '.' s with
    | [ i; d ] ->
        let frac = if String.length d >= 1 then String.sub d 0 1 else d ^ "0" in
        add_strings buf [ i; "."; frac ]
    | _ -> Buffer.add_string buf s
  else add_strings buf [ s; ".0" ]

let add_change buf count action singular =
  let noun = if count = 1 then singular else singular ^ "s" in
  add_strings buf [ string_of_int count; " "; action; " "; noun ]

let emit_changes buf stats =
  let entries =
    [
      (stats.added_rules, "added", "rule");
      (stats.removed_rules, "removed", "rule");
      (stats.modified_rules, "modified", "rule");
      (stats.reordered_rules, "reordered", "rule");
      (stats.rearranged_rules, "rearranged", "rule");
      (stats.regrouped_rules, "regrouped", "rule");
    ]
    |> List.filter (fun (n, _, _) -> n > 0)
  in
  let container = stats.container_changes in
  let layers = stats.layer_order_swaps in
  (* Every counter above comes from a tree diff, so they all read zero on the
     results that never reached one: a comparison that fell through to the
     string diff, a side whose content the parser discarded, a parse error. The
     line says what it knows - nothing was classified - and leaves the verdict
     to the report under it. *)
  if entries = [] && container = 0 && layers = 0 then
    Buffer.add_string buf
      "Changes: none classified structurally (see report below)\n"
  else (
    Buffer.add_string buf "Changes: ";
    List.iteri
      (fun i (n, action, singular) ->
        if i > 0 then Buffer.add_string buf ", ";
        add_change buf n action singular)
      entries;
    if container > 0 then (
      if entries <> [] then Buffer.add_string buf ", ";
      add_change buf container "changed" "container");
    if layers > 0 then (
      if entries <> [] || container > 0 then Buffer.add_string buf ", ";
      add_change buf layers "swapped" "layer pair");
    Buffer.add_char buf '\n')

let pp_stats buf stats =
  let char_diff = abs (stats.actual_chars - stats.expected_chars) in
  (* Same order as the [---] / [+++] headers below: expected, then actual. *)
  add_strings buf
    [
      "CSS: ";
      string_of_int stats.expected_chars;
      " chars vs ";
      string_of_int stats.actual_chars;
    ];
  if stats.expected_chars > 0 then (
    let char_diff_pct =
      float_of_int char_diff *. 100.0 /. float_of_int stats.expected_chars
    in
    Buffer.add_string buf " chars (";
    add_pct buf char_diff_pct;
    Buffer.add_string buf "% diff)\n")
  else Buffer.add_string buf " chars\n";
  emit_changes buf stats
