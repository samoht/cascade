open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)

let rule_strings rules =
  List.map (fun r -> Pp.to_string ~minify:true Stylesheet.pp_rule r) rules

let same_decl a b = a == b || a = b

let test_pseudo_and_vendor_detection () =
  Alcotest.(check (option string))
    "pseudo at selector tail" (Some "::before")
    (Option.map Selector.to_string
       (Merge.pseudo (Selector.of_string ".a .b::before")));
  Alcotest.(check bool)
    "vendor pseudo detected" true
    (Merge.vendor (Selector.of_string ".a::-webkit-scrollbar"));
  Alcotest.(check bool)
    "regular selector is not vendor" false
    (Merge.vendor (Selector.of_string ".a::before"))

let test_adjacent_merges_same_selector_only () =
  let merged =
    Merge.adjacent (rules ".a{color:red}.a{width:1px}.b{color:red}")
  in
  Alcotest.(check (list string))
    "adjacent same selectors merge"
    [ ".a{color:red;width:1px}"; ".b{color:red}" ]
    (rule_strings merged)

let test_identical_combines_safe_non_adjacent_rules () =
  let merged =
    Merge.identical ~same:same_decl
      (rules ".a{color:red}.x{width:1px}.b{color:red}")
  in
  Alcotest.(check (list string))
    "non-perturbing middle rule is delayed"
    [ ".a,.b{color:red}"; ".x{width:1px}" ]
    (rule_strings merged)

let test_identical_stops_at_perturbing_rule () =
  let merged =
    Merge.identical ~same:same_decl
      (rules ".a{color:red}.x{color:blue}.b{color:red}")
  in
  Alcotest.(check (list string))
    "same property write blocks delayed combine"
    [ ".a{color:red}"; ".x{color:blue}"; ".b{color:red}" ]
    (rule_strings merged)

let test_declarations_equal_fast_and_structural_paths () =
  let r = rule ".a{color:red;width:1px}" in
  Alcotest.(check bool)
    "physical list fast path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r.declarations);
  let r2 = rule ".b{color:red;width:1px}" in
  Alcotest.(check bool)
    "structural path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r2.declarations);
  let r3 = rule ".c{color:red}" in
  Alcotest.(check bool)
    "different length" false
    (Merge.declarations_equal ~same:same_decl r.declarations r3.declarations)

let suite =
  ( "merge",
    [
      Alcotest.test_case "pseudo and vendor detection" `Quick
        test_pseudo_and_vendor_detection;
      Alcotest.test_case "adjacent merges same selector only" `Quick
        test_adjacent_merges_same_selector_only;
      Alcotest.test_case "identical combines safe non-adjacent rules" `Quick
        test_identical_combines_safe_non_adjacent_rules;
      Alcotest.test_case "identical stops at perturbing rule" `Quick
        test_identical_stops_at_perturbing_rule;
      Alcotest.test_case "declarations_equal fast and structural paths" `Quick
        test_declarations_equal_fast_and_structural_paths;
    ] )
