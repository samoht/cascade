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
  container_changes : int;
}

(* ===== Helper Functions ===== *)

(** Extract path component for an at-rule statement. Returns Some (path_segment,
    inner_statements) if the statement is an at-rule, None otherwise. *)
let at_rule_path_and_inner stmt =
  match Css.as_supports stmt with
  | Some (cond, inner) ->
      Some ("@supports " ^ Css.Supports.to_string cond, inner)
  | None -> (
      match Css.as_media stmt with
      | Some (cond, inner) -> Some ("@media " ^ Css.Media.to_string cond, inner)
      | None -> (
          match Css.as_layer stmt with
          | Some (name_opt, inner) ->
              let name = match name_opt with Some n -> n | None -> "" in
              Some ("@layer " ^ name, inner)
          | None -> (
              match Css.as_container stmt with
              | Some (name_opt, cond, inner) ->
                  let prefix =
                    match name_opt with Some n -> n ^ " " | None -> ""
                  in
                  Some
                    ( "@container " ^ prefix ^ Css.Container.to_string cond,
                      inner )
              | None -> None)))

let strip_header css =
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

(* Note: We do NOT normalize @property order because their order matters for
   testing property_order values in our implementation. The order of @property
   rules in @layer properties reflects the intended cascade behavior. *)

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)

let tree_diff ~(expected : Css.t) ~(actual : Css.t) : Tree_diff.t =
  D.diff ~expected ~actual

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

let group_into_table rules =
  let tbl = Hashtbl.create 128 in
  List.iter
    (fun (k, d) ->
      let lst = match Hashtbl.find_opt tbl k with Some l -> l | None -> [] in
      Hashtbl.replace tbl k (lst @ [ d ]))
    rules;
  tbl

(* Compare two declaration lists with the same key and emit diffs *)
let diff_same_key_pair key d1 d2 =
  let sig1 = sig_of_decls d1 in
  let sig2 = sig_of_decls d2 in
  if sig1 = sig2 && d1 <> d2 then
    Some
      (D.Rule_reordered
         {
           selector = key;
           expected_pos = -1;
           actual_pos = -1;
           swapped_with = None;
           old_declarations = Some d1;
           new_declarations = Some d2;
         })
  else if sig1 <> sig2 then
    Some
      (D.Rule_content_changed
         {
           selector = key;
           old_declarations = d1;
           new_declarations = d2;
           property_changes = [];
           added_properties =
             List.filter_map
               (fun (p, _) -> if List.mem_assoc p sig1 then None else Some p)
               sig2;
           removed_properties =
             List.filter_map
               (fun (p, _) -> if List.mem_assoc p sig2 then None else Some p)
               sig1;
         })
  else None

let diff_count_mismatch key ds1 ds2 =
  let n1 = List.length ds1 in
  let n2 = List.length ds2 in
  if n2 > n1 then
    D.Rule_added
      { selector = key ^ " (duplicate)"; declarations = List.nth ds2 (n2 - 1) }
  else
    D.Rule_removed
      { selector = key ^ " (missing)"; declarations = List.nth ds1 (n1 - 1) }

(* Detect declaration-reordering-only differences throughout a stylesheet *)
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
  Hashtbl.iter
    (fun key ds1 ->
      match Hashtbl.find_opt tbl2 key with
      | Some ds2 when List.length ds1 = List.length ds2 ->
          List.iter2
            (fun d1 d2 ->
              match diff_same_key_pair key d1 d2 with
              | Some d -> diffs := d :: !diffs
              | None -> ())
            ds1 ds2
      | Some ds2 -> diffs := diff_count_mismatch key ds1 ds2 :: !diffs
      | None -> ())
    tbl1;
  if !diffs = [] then None
  else Some D.{ rules = List.rev !diffs; containers = [] }

(* Compare two CSS strings for structural and formatting equality *)
let compare css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  (* If strings are identical, they're equal *)
  if css1 = css2 then true
  else
    match (Css.of_string css1, Css.of_string css2) with
    | Ok expected, Ok actual ->
        let d = tree_diff ~expected ~actual in
        (* Only consider them equal if both structural diff is empty AND strings
           are identical *)
        is_empty d && css1 = css2
    | _ -> false (* Parse errors with different strings are not equal *)

(* Parse two CSS strings and return their diff or parse errors *)
type t =
  | Tree_diff of Tree_diff.t (* CSS AST differences found *)
  | String_diff of String_diff.t (* No structural diff but strings differ *)
  | No_diff (* Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error

let diff ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  (* First check if original strings are identical *)
  if expected = actual then No_diff
  else
    match (Css.of_string expected, Css.of_string actual) with
    | Ok expected_ast, Ok actual_ast -> (
        (* Do NOT normalize @property order - their order matters for tests *)
        let expected_norm = expected_ast in
        let actual_norm = actual_ast in
        let structural_diff =
          tree_diff ~expected:expected_norm ~actual:actual_norm
        in
        if not (is_empty structural_diff) then Tree_diff structural_diff
        else
          (* Structural diff is empty but strings differ - attempt to classify
             declaration ordering-only differences throughout the stylesheet
             (recursively inside containers) as structural Rule_reordered
             changes. If none detected, fall back to string diff. *)
          match build_reorder_diff expected_norm actual_norm with
          | Some d -> Tree_diff d
          | None -> (
              (* Use original (header-stripped) strings for string diff *)
              match String_diff.diff ~expected actual with
              | Some sdiff -> String_diff sdiff
              | None ->
                  failwith
                    "BUG: different strings but String_diff found no difference"
              ))
    | Error e1, Error e2 -> Both_errors (e1, e2)
    | Ok _, Error e -> Actual_error e
    | Error e, Ok _ -> Expected_error e

let diff_with_mode ~mode ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  match mode with
  | `Auto -> diff ~expected ~actual
  | `String -> (
      if
        (* Force string diff mode *)
        expected = actual
      then No_diff
      else
        match String_diff.diff ~expected actual with
        | Some sdiff -> String_diff sdiff
        | None -> No_diff)
  | `Tree -> (
      (* Force tree diff mode *)
      match (Css.of_string expected, Css.of_string actual) with
      | Ok expected_ast, Ok actual_ast ->
          let structural_diff =
            tree_diff ~expected:expected_ast ~actual:actual_ast
          in
          if expected = actual then No_diff else Tree_diff structural_diff
      | Error e1, Error e2 -> Both_errors (e1, e2)
      | Ok _, Error e -> Actual_error e
      | Error e, Ok _ -> Expected_error e)

let as_tree_diff = function
  | Tree_diff d -> Some d
  | String_diff _ | No_diff | Both_errors _ | Expected_error _ | Actual_error _
    ->
      None

(* Compute statistics from diff results *)
let compute_stats ~expected_str ~actual_str diff_result =
  let expected_chars = String.length expected_str in
  let actual_chars = String.length actual_str in

  match diff_result with
  | Tree_diff d ->
      let count_rule_type pred = List.filter pred d.rules |> List.length in
      {
        expected = expected_str;
        actual = actual_str;
        expected_chars;
        actual_chars;
        added_rules =
          count_rule_type (function D.Rule_added _ -> true | _ -> false);
        removed_rules =
          count_rule_type (function D.Rule_removed _ -> true | _ -> false);
        modified_rules =
          count_rule_type (function
            | D.Rule_content_changed _ | D.Rule_selector_changed _ -> true
            | _ -> false);
        reordered_rules =
          count_rule_type (function D.Rule_reordered _ -> true | _ -> false);
        container_changes = List.length d.containers;
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
        container_changes = 0;
      }

(* Alias for compute_stats *)
let stats = compute_stats

(* Format the result of diff with optional labels *)
let pp ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Tree_diff d ->
      (* Show structural differences *)
      D.pp ~expected ~actual fmt d
  | String_diff sdiff -> String_diff.pp fmt sdiff
  | No_diff ->
      (* No output for identical files *)
      ()
  | Both_errors (e1, e2) ->
      let err1 = Css.pp_parse_error e1 in
      let err2 = Css.pp_parse_error e2 in
      if String.equal err1 err2 then
        Fmt.pf fmt "Both CSS have same parse error: %s" err1
      else
        Fmt.pf fmt "Parse errors:\n  %s: %s\n  %s: %s" expected err1 actual err2
  | Expected_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" expected (Css.pp_parse_error e)
  | Actual_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" actual (Css.pp_parse_error e)

let pp_stats fmt stats =
  let char_diff = abs (stats.actual_chars - stats.expected_chars) in
  let char_diff_pct =
    if stats.expected_chars > 0 then
      float_of_int char_diff *. 100.0 /. float_of_int stats.expected_chars
    else 0.0
  in

  Fmt.pf fmt "@[<v>CSS: %d chars vs %d chars (%.1f%% diff)@," stats.actual_chars
    stats.expected_chars char_diff_pct;

  (* Helper to add change description if count > 0 *)
  let add_change count action singular changes =
    if count > 0 then
      Fmt.str "%d %s %s" count action
        (if count = 1 then singular else singular ^ "s")
      :: changes
    else changes
  in

  (* Build list of non-zero changes *)
  let changes =
    []
    |> add_change stats.added_rules "added" "rule"
    |> add_change stats.removed_rules "removed" "rule"
    |> add_change stats.modified_rules "modified" "rule"
    |> add_change stats.reordered_rules "reordered" "rule"
    |> List.rev
  in

  (* Add container changes *)
  let container_changes =
    if stats.container_changes > 0 then
      [ Fmt.str "%d containers" stats.container_changes ]
    else []
  in
  let all_changes = changes @ container_changes in

  if all_changes <> [] then
    Fmt.pf fmt "Changes: %s@," (String.concat ", " all_changes)
  else Fmt.pf fmt "No structural differences@,";

  Fmt.pf fmt "@]"
