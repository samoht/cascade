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
      let name = match name_opt with Some n -> n | None -> "" in
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
  if sig1 = sig2 && d1 <> d2 && D.reorder_is_significant d1 d2 then
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
  Hashtbl.iter (collect_key_diffs ~tbl2 ~diffs) tbl1;
  if !diffs = [] then None
  else Some D.{ rules = List.rev !diffs; containers = [] }

let css_for_semantic_comparison ?property css =
  match property with
  | None -> css
  | Some property -> ":root{" ^ property ^ ":" ^ css ^ "}"

let canonical_of_stylesheet ~lossless ~prune_unused_custom_props stylesheet =
  try
    Some
      (stylesheet
      (* Regrouping - factoring a shared declaration into a selector list,
         synthesising nesting from adjacent rules - depends on how the input
         happened to order its rules, so it is not confluent: the same sheet
         written either way would canonicalise differently. The projection skips
         it. *)
      |> Css.optimize ~lossless ~regroup:false ~prune_unused_custom_props
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
  | No_diff of { canonical_byte_diff : (string * string) option }
      (** Structurally equivalent. [canonical_byte_diff = None] means the two
          inputs were bytewise equal (after header strip / canonical minify).
          [Some (expected, actual)] means the structural comparator (tree-diff
          in [`Canonical] mode) found no difference but the canonical minified
          forms still differed - i.e. a canonical-pass gap to chip away at in
          future. The two strings let maintainers inspect what cascade hasn't
          normalised yet. *)
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
  if expected = actual then No_diff { canonical_byte_diff = None }
  else
    match (expected_parse, actual_parse) with
    | ( Ok { Css.stylesheet = expected_ast; _ },
        Ok { Css.stylesheet = actual_ast; _ } ) ->
        diff_two_parsed ~expected ~actual ~expected_ast ~actual_ast
    | Error e1, Error e2 -> Both_errors (e1, e2)
    | Ok _, Error e -> Actual_error e
    | Error e, Ok _ -> Expected_error e

let diff_canonical_parsed ~expected ~actual ~expected_parse ~actual_parse
    ~expected_canon ~actual_canon =
  match (Css.of_string expected_canon, Css.of_string actual_canon) with
  | Ok { stylesheet = expected_ast; _ }, Ok { stylesheet = actual_ast; _ } ->
      let structural_diff =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      if is_empty structural_diff then
        No_diff { canonical_byte_diff = Some (expected_canon, actual_canon) }
      else Tree_diff structural_diff
  | _ -> diff_auto ~expected ~actual ~expected_parse ~actual_parse

let diff_canonical ~lossless ~prune_unused_custom_props ~expected ~actual
    ~expected_parse ~actual_parse =
  if expected = actual then No_diff { canonical_byte_diff = None }
  else
    match
      canonical_diff_inputs_with_fallback ~lossless ~prune_unused_custom_props
        expected actual
    with
    | None -> diff_auto ~expected ~actual ~expected_parse ~actual_parse
    | Some (expected_canon, actual_canon) ->
        if String.equal expected_canon actual_canon then
          No_diff { canonical_byte_diff = None }
        else
          (* Tree-diff is the authoritative semantic comparator in Canonical
             mode; canonical-minified byte equality is a fast-path sufficient
             condition, not a necessary one. When the structural diff is empty,
             the inputs ARE equivalent - cascade's canonical pass just hasn't
             (yet) collapsed those particular textual variants (tool headers,
             [@layer a, b;] vs split-form, empty layer-order pins, whitespace
             inside [url()], ...). Expose the canonical byte forms on [No_diff]
             so maintainers can spot which canonical-pass gap to chip away at
             next, without ever overriding the structural answer. *)
          diff_canonical_parsed ~expected ~actual ~expected_parse ~actual_parse
            ~expected_canon ~actual_canon

let diff_string ~expected ~actual =
  if expected = actual then No_diff { canonical_byte_diff = None }
  else
    match String_diff.diff ~expected actual with
    | Some sdiff -> String_diff sdiff
    | None -> No_diff { canonical_byte_diff = None }

let diff_tree ~expected_parse ~actual_parse =
  match (expected_parse, actual_parse) with
  | ( Ok { Css.stylesheet = expected_ast; _ },
      Ok { Css.stylesheet = actual_ast; _ } ) ->
      let structural_diff =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      if is_empty structural_diff then No_diff { canonical_byte_diff = None }
      else Tree_diff structural_diff
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
    {
      result = No_diff { canonical_byte_diff = None };
      expected_warnings = [];
      actual_warnings = [];
    }
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
  | No_diff _ -> true
  | _ -> false

let as_tree_diff t =
  match t.result with
  | Tree_diff d -> Some d
  | String_diff _ | No_diff _ | Both_errors _ | Expected_error _
  | Actual_error _ ->
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
      }

(* Alias for compute_stats *)
let stats = compute_stats
let add_strings b ls = List.iter (Buffer.add_string b) ls

(* Render each side's parse warnings so a declaration the parser dropped never
   reads as a phantom structural difference on the side that parsed. Past [max]
   they are counted rather than printed: a stylesheet that trips the same
   unsupported syntax hundreds of times would otherwise bury the diff it is
   meant to qualify. *)
let pp_parse_warnings ?(max = Stdlib.max_int) buf label warnings =
  let shown, hidden =
    let n = List.length warnings in
    if n <= max then (warnings, 0)
    else (List.filteri (fun i _ -> i < max) warnings, n - max)
  in
  List.iter
    (fun w ->
      if Buffer.length buf > 0 && Buffer.nth buf (Buffer.length buf - 1) <> '\n'
      then Buffer.add_char buf '\n';
      add_strings buf [ label; " parse warning: "; Error.to_string w; "\n" ])
    shown;
  if hidden > 0 then
    add_strings buf
      [
        label;
        ": ";
        string_of_int hidden;
        (if hidden = 1 then " more parse warning\n"
         else " more parse warnings\n");
      ]

let pp_result ?(expected = "Expected") ?(actual = "Actual") ?(color = false)
    ?depth buf = function
  | Tree_diff d ->
      (* Show structural differences *)
      D.pp ~expected ~actual ~color ?depth buf d
  | String_diff sdiff -> String_diff.pp buf sdiff
  | No_diff _ ->
      (* No output for structurally equivalent files (whether or not the
         canonical-minified bytes also matched). *)
      ()
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

let pp_warnings ?(expected = "Expected") ?(actual = "Actual") ?max buf t =
  pp_parse_warnings ?max buf expected t.expected_warnings;
  pp_parse_warnings ?max buf actual t.actual_warnings

let has_warnings t = t.expected_warnings <> [] || t.actual_warnings <> []

let pp_diff ?(expected = "Expected") ?(actual = "Actual") ?(color = false)
    ?depth buf t =
  pp_result ~expected ~actual ~color ?depth buf t.result

let pp ?(expected = "Expected") ?(actual = "Actual") ?(color = false) ?depth buf
    t =
  (* Warnings come first: a dropped declaration qualifies every line below it,
     and trailing them puts that caveat past the end of a long report. *)
  pp_warnings ~expected ~actual buf t;
  if has_warnings t then Buffer.add_char buf '\n';
  pp_diff ~expected ~actual ~color ?depth buf t

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
  if entries = [] && container = 0 then
    Buffer.add_string buf "No structural differences\n"
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
    Buffer.add_char buf '\n')

let pp_stats buf stats =
  let char_diff = abs (stats.actual_chars - stats.expected_chars) in
  let char_diff_pct =
    if stats.expected_chars > 0 then
      float_of_int char_diff *. 100.0 /. float_of_int stats.expected_chars
    else 0.0
  in
  (* Same order as the [---] / [+++] headers below: expected, then actual. *)
  add_strings buf
    [
      "CSS: ";
      string_of_int stats.expected_chars;
      " chars vs ";
      string_of_int stats.actual_chars;
      " chars (";
    ];
  add_pct buf char_diff_pct;
  Buffer.add_string buf "% diff)\n";
  emit_changes buf stats
