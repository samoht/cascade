(** Tests for Cascade_diff.Tree_diff module *)

open Cascade

let parse css = Css.of_string_exn ~strict:false css

(* ===== Identical stylesheets ===== *)

let diff_identical () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_identical_multiple_rules () =
  let css = parse ".a { color: red } .b { margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical multi-rule is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_empty_stylesheets () =
  let css = parse "" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "empty stylesheets is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Rule additions ===== *)

let diff_rule_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } .b { margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "addition is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> []);
  (* Check that at least one Added exists *)
  let has_added =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with Cascade_diff.Tree_diff.Added _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Added" true has_added

(* ===== Rule removals ===== *)

let diff_rule_removed () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "removal is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let has_removed =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with Cascade_diff.Tree_diff.Removed _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Removed" true has_removed

(* ===== Property value changes ===== *)

let diff_property_changed () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "property change is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> [])

let diff_rule_added_property () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red; margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "added property is not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Rule reordering ===== *)

let diff_rule_reordered () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".b { margin: 0 } .a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "reorder is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let has_reordered =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with
        | Cascade_diff.Tree_diff.Reordered _ -> true
        | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Reordered" true has_reordered

let diff_neutral_decl_reorder_is_empty () =
  (* Disjoint declarations commute, so reordering them is no difference. *)
  let expected = parse ".a { color: red; background: blue }" in
  let actual = parse ".a { background: blue; color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "neutral declaration reorder is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_overlapping_decl_reorder_flagged () =
  (* A shorthand and its longhand overlap, so their order decides the
     cascade. *)
  let expected = parse ".a { margin: 1px; margin-top: 2px }" in
  let actual = parse ".a { margin-top: 2px; margin: 1px }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "overlapping declaration reorder is not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Container (media) changes ===== *)

let diff_media_added () =
  let expected = parse ".a { color: red }" in
  let actual =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media addition is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has container diffs" true (d.containers <> []);
  Alcotest.(check bool)
    "has media added" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `Media d)

let diff_media_removed () =
  let expected =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let actual = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media removal is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has media removed" true
    (Cascade_diff.Tree_diff.has_container_removed_of_type `Media d)

let diff_layer_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } @layer base { .b { margin: 0 } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "layer addition not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has layer added" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `Layer d)

(* ===== CSS nesting ===== *)

let diff_nesting_modified () =
  let expected =
    parse
      ".card { padding: 1rem; & .title { font-size: 1.5rem; color: #111 } & \
       .body { color: #333 } }"
  in
  let actual =
    parse
      ".card { padding: 1.5rem; & .title { font-size: 1.25rem; color: #000; \
       font-weight: 600 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nesting diff not empty" false
    (d.rules = [] && d.containers = []);
  (* Should detect nested rule changes *)
  let has_nesting =
    Cascade_diff.Tree_diff.count_containers_by_type `Nesting d > 0
  in
  Alcotest.(check bool) "has nesting container diff" true has_nesting

let diff_nesting_identical () =
  let css = parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical nesting is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_child_added () =
  let expected = parse ".card { padding: 1rem }" in
  let actual =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nested child added not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_child_removed () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let actual = parse ".card { padding: 1rem }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nested child removed not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_deep () =
  let expected = parse ".a { & .b { & .c { color: red } } }" in
  let actual = parse ".a { & .b { & .c { color: blue } } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "deep nesting diff not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_parent_props_only () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1rem } }"
  in
  let actual = parse ".card { padding: 2rem; & .title { font-size: 1rem } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "parent-only change not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  (* Nested rules are identical, so no nesting container diff *)
  let nesting_count =
    Cascade_diff.Tree_diff.count_containers_by_type `Nesting d
  in
  Alcotest.(check int) "no nesting container diff" 0 nesting_count

(* ===== Query functions ===== *)

let single_rule_diff_one_change () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "single_rule_diff returns Some" true
    (Option.is_some (Cascade_diff.Tree_diff.single_rule_diff d))

let single_rule_diff_no_change () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "single_rule_diff returns None" true
    (Option.is_none (Cascade_diff.Tree_diff.single_rule_diff d))

let single_rule_diff_multiple_changes () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: blue } .b { margin: 10px }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  (* Multiple rule changes means single_rule_diff should return None *)
  let result = Cascade_diff.Tree_diff.single_rule_diff d in
  (* Only None when there are exactly != 1 rule changes *)
  if List.length d.rules = 1 then
    Alcotest.(check bool) "single with 1 change" true (Option.is_some result)
  else
    Alcotest.(check bool)
      "none with multiple changes" true (Option.is_none result)

let count_containers_media () =
  let expected = parse ".a { color: red }" in
  let actual =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let count = Cascade_diff.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check bool) "at least one media container" true (count >= 1)

let count_containers_zero () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  let count = Cascade_diff.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check int) "zero media containers" 0 count

(* ===== Pretty printing ===== *)

let pp_does_not_crash () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  let output = Buffer.contents buf in
  Alcotest.(check bool) "pp produces output" true (String.length output > 0)

let pp_rule_diff_simple_ok () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  match d.rules with
  | [] -> Alcotest.fail "expected rule diffs"
  | rule :: _ ->
      let buf = Buffer.create 256 in
      Cascade_diff.Tree_diff.pp_rule_diff_simple buf rule;
      let output = Buffer.contents buf in
      Alcotest.(check bool)
        "pp_rule_diff_simple produces output" true
        (String.length output > 0)

(* ===== Selector grouping reconciliation ===== *)

let string_contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  go 0

let render d =
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  Buffer.contents buf

let diff_selector_group_split_reported () =
  (* Splitting a group with identical declarations is reported as a structural
     regroup (not add/remove noise, not silently identical). *)
  let expected = parse ".a, .b { color: red }" in
  let actual = parse ".a { color: red } .b { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "split is reported, not identical" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "describes a split" true
    (string_contains ~needle:"split" s);
  Alcotest.(check bool)
    "names both selectors" true
    (string_contains ~needle:".a" s && string_contains ~needle:".b" s)

let diff_selector_group_merge_reported () =
  let expected = parse ".a { color: red } .b { color: red }" in
  let actual = parse ".a, .b { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "merge is reported, not identical" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "describes a merge" true
    (string_contains ~needle:"merged" s)

let diff_selector_group_partial_change () =
  (* Splitting the group and changing one selector: only the changed selector
     surfaces, the unchanged one is reconciled away (not add/remove noise). *)
  let expected = parse ".a, .b { color: red }" in
  let actual = parse ".a { color: red } .b { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "partial regroup is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "reports the changed .b" true
    (string_contains ~needle:".b" s);
  Alcotest.(check bool)
    "does not report the unchanged .a" false
    (string_contains ~needle:".a" s)

(* ===== Suite ===== *)

let suite =
  ( "tree_diff",
    [
      Alcotest.test_case "selector group split reported" `Quick
        diff_selector_group_split_reported;
      Alcotest.test_case "selector group merge reported" `Quick
        diff_selector_group_merge_reported;
      Alcotest.test_case "selector group partial change" `Quick
        diff_selector_group_partial_change;
      Alcotest.test_case "identical" `Quick diff_identical;
      Alcotest.test_case "identical multiple rules" `Quick
        diff_identical_multiple_rules;
      Alcotest.test_case "empty stylesheets" `Quick diff_empty_stylesheets;
      Alcotest.test_case "rule added" `Quick diff_rule_added;
      Alcotest.test_case "rule removed" `Quick diff_rule_removed;
      Alcotest.test_case "property changed" `Quick diff_property_changed;
      Alcotest.test_case "property added to rule" `Quick
        diff_rule_added_property;
      Alcotest.test_case "rule reordered" `Quick diff_rule_reordered;
      Alcotest.test_case "neutral declaration reorder is empty" `Quick
        diff_neutral_decl_reorder_is_empty;
      Alcotest.test_case "overlapping declaration reorder flagged" `Quick
        diff_overlapping_decl_reorder_flagged;
      Alcotest.test_case "media added" `Quick diff_media_added;
      Alcotest.test_case "media removed" `Quick diff_media_removed;
      Alcotest.test_case "layer added" `Quick diff_layer_added;
      Alcotest.test_case "single_rule_diff one change" `Quick
        single_rule_diff_one_change;
      Alcotest.test_case "single_rule_diff no change" `Quick
        single_rule_diff_no_change;
      Alcotest.test_case "single_rule_diff multiple changes" `Quick
        single_rule_diff_multiple_changes;
      Alcotest.test_case "count containers media" `Quick count_containers_media;
      Alcotest.test_case "count containers zero" `Quick count_containers_zero;
      Alcotest.test_case "nesting modified" `Quick diff_nesting_modified;
      Alcotest.test_case "nesting identical" `Quick diff_nesting_identical;
      Alcotest.test_case "nesting child added" `Quick diff_nesting_child_added;
      Alcotest.test_case "nesting child removed" `Quick
        diff_nesting_child_removed;
      Alcotest.test_case "nesting deep" `Quick diff_nesting_deep;
      Alcotest.test_case "nesting only parent props changed" `Quick
        diff_nesting_parent_props_only;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "pp_rule_diff_simple does not crash" `Quick
        pp_rule_diff_simple_ok;
    ] )
