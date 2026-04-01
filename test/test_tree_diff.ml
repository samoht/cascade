(** Tests for Css_tools.Tree_diff module *)

open Cascade

let parse css =
  match Css.of_string css with Ok s -> s | Error _ -> failwith "parse"

(* ===== Identical stylesheets ===== *)

let diff_identical () =
  let css = parse ".a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical is empty" true
    (Css_tools.Tree_diff.is_empty d)

let diff_identical_multiple_rules () =
  let css = parse ".a { color: red } .b { margin: 0 }" in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical multi-rule is empty" true
    (Css_tools.Tree_diff.is_empty d)

let diff_empty_stylesheets () =
  let css = parse "" in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "empty stylesheets is empty" true
    (Css_tools.Tree_diff.is_empty d)

(* ===== Rule additions ===== *)

let diff_rule_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } .b { margin: 0 }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "addition is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> []);
  (* Check that at least one Rule_added exists *)
  let has_added =
    List.exists
      (function Css_tools.Tree_diff.Rule_added _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Rule_added" true has_added

(* ===== Rule removals ===== *)

let diff_rule_removed () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "removal is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  let has_removed =
    List.exists
      (function Css_tools.Tree_diff.Rule_removed _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Rule_removed" true has_removed

(* ===== Property value changes ===== *)

let diff_property_changed () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "property change is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> [])

let diff_property_added_to_rule () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red; margin: 0 }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "added property is not empty" false
    (Css_tools.Tree_diff.is_empty d)

(* ===== Rule reordering ===== *)

let diff_rule_reordered () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".b { margin: 0 } .a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "reorder is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  let has_reordered =
    List.exists
      (function Css_tools.Tree_diff.Rule_reordered _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Rule_reordered" true has_reordered

(* ===== Container (media) changes ===== *)

let diff_media_added () =
  let expected = parse ".a { color: red }" in
  let actual =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media addition is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  Alcotest.(check bool) "has container diffs" true (d.containers <> []);
  Alcotest.(check bool)
    "has media added" true
    (Css_tools.Tree_diff.has_container_added_of_type `Media d)

let diff_media_removed () =
  let expected =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let actual = parse ".a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media removal is not empty" false
    (Css_tools.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has media removed" true
    (Css_tools.Tree_diff.has_container_removed_of_type `Media d)

let diff_layer_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } @layer base { .b { margin: 0 } }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "layer addition not empty" false
    (Css_tools.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has layer added" true
    (Css_tools.Tree_diff.has_container_added_of_type `Layer d)

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
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool) "nesting diff not empty" false (d.rules = [] && d.containers = []);
  (* Should detect nested rule changes *)
  let has_nesting =
    Css_tools.Tree_diff.count_containers_by_type `Nesting d > 0
  in
  Alcotest.(check bool) "has nesting container diff" true has_nesting

let diff_nesting_identical () =
  let css =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool) "identical nesting is empty" true
    (Css_tools.Tree_diff.is_empty d)

let diff_nesting_child_added () =
  let expected = parse ".card { padding: 1rem }" in
  let actual =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool) "nested child added not empty" false
    (Css_tools.Tree_diff.is_empty d)

let diff_nesting_child_removed () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let actual = parse ".card { padding: 1rem }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool) "nested child removed not empty" false
    (Css_tools.Tree_diff.is_empty d)

let diff_nesting_deep () =
  let expected =
    parse ".a { & .b { & .c { color: red } } }"
  in
  let actual =
    parse ".a { & .b { & .c { color: blue } } }"
  in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool) "deep nesting diff not empty" false
    (Css_tools.Tree_diff.is_empty d)

let diff_nesting_only_parent_props_changed () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1rem } }"
  in
  let actual =
    parse ".card { padding: 2rem; & .title { font-size: 1rem } }"
  in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool) "parent-only change not empty" false
    (Css_tools.Tree_diff.is_empty d);
  (* Nested rules are identical, so no nesting container diff *)
  let nesting_count =
    Css_tools.Tree_diff.count_containers_by_type `Nesting d
  in
  Alcotest.(check int) "no nesting container diff" 0 nesting_count

(* ===== Query functions ===== *)

let single_rule_diff_one_change () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "single_rule_diff returns Some" true
    (Option.is_some (Css_tools.Tree_diff.single_rule_diff d))

let single_rule_diff_no_change () =
  let css = parse ".a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "single_rule_diff returns None" true
    (Option.is_none (Css_tools.Tree_diff.single_rule_diff d))

let single_rule_diff_multiple_changes () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: blue } .b { margin: 10px }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  (* Multiple rule changes means single_rule_diff should return None *)
  let result = Css_tools.Tree_diff.single_rule_diff d in
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
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  let count = Css_tools.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check bool) "at least one media container" true (count >= 1)

let count_containers_zero () =
  let css = parse ".a { color: red }" in
  let d = Css_tools.Tree_diff.diff ~expected:css ~actual:css in
  let count = Css_tools.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check int) "zero media containers" 0 count

(* ===== Pretty printing ===== *)

let pp_does_not_crash () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  let buf = Buffer.create 256 in
  Css_tools.Tree_diff.pp buf d;
  let output = Buffer.contents buf in
  Alcotest.(check bool) "pp produces output" true (String.length output > 0)

let pp_rule_diff_simple_ok () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Css_tools.Tree_diff.diff ~expected ~actual in
  match d.rules with
  | [] -> Alcotest.fail "expected rule diffs"
  | rule :: _ ->
      let buf = Buffer.create 256 in
      Css_tools.Tree_diff.pp_rule_diff_simple buf rule;
      let output = Buffer.contents buf in
      Alcotest.(check bool)
        "pp_rule_diff_simple produces output" true
        (String.length output > 0)

(* ===== Suite ===== *)

let suite =
  ( "tree_diff",
    [
      Alcotest.test_case "identical" `Quick diff_identical;
      Alcotest.test_case "identical multiple rules" `Quick
        diff_identical_multiple_rules;
      Alcotest.test_case "empty stylesheets" `Quick diff_empty_stylesheets;
      Alcotest.test_case "rule added" `Quick diff_rule_added;
      Alcotest.test_case "rule removed" `Quick diff_rule_removed;
      Alcotest.test_case "property changed" `Quick diff_property_changed;
      Alcotest.test_case "property added to rule" `Quick
        diff_property_added_to_rule;
      Alcotest.test_case "rule reordered" `Quick diff_rule_reordered;
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
        diff_nesting_only_parent_props_changed;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "pp_rule_diff_simple does not crash" `Quick
        pp_rule_diff_simple_ok;
    ] )
