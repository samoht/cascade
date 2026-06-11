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

let sel_string sel = Selector.to_string ~minify:true sel

let test_contains_and_substitute () =
  let parent = Selector.class_ "card" in
  let nested = Selector.of_string "&:hover" in
  Alcotest.(check bool) "contains nesting" true (Nest.contains nested);
  Alcotest.(check bool)
    "plain selector" false
    (Nest.contains (Selector.class_ "plain"));
  Alcotest.(check string)
    "substitute parent" ".card:hover"
    (sel_string (Nest.substitute ~parent nested))

let test_combine_relative_nested_and_descendant () =
  let parent = Selector.class_ "card" in
  Alcotest.(check string)
    "descendant for plain child" ".card .title"
    (sel_string (Nest.combine parent (Selector.class_ "title")));
  Alcotest.(check string)
    "nesting replacement" ".card:hover"
    (sel_string (Nest.combine parent (Selector.of_string "&:hover")));
  Alcotest.(check string)
    "relative child" ".card>.title"
    (sel_string
       (Nest.combine parent
          (Selector.Relative (Selector.Child, Selector.class_ "title"))))

let test_merge_lone_wrapper () =
  let merged = Nest.merge_lone (rule ".card{.title{color:red}}") in
  Alcotest.(check string)
    "pure wrapper merged" ".card .title{color:red}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule merged);
  let with_decl = Nest.merge_lone (rule ".card{color:red;.title{width:1px}}") in
  Alcotest.(check string)
    "wrapper with declarations preserved" ".card{color:red;.title{width:1px}}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule with_decl)

let test_rules_synthesizes_isolated_chain () =
  let nested = Nest.rules (rules ".card{color:red}.card .title{width:1px}") in
  Alcotest.(check (list string))
    "flat chain nests when shorter"
    [ ".card{color:red;.title{width:1px}}" ]
    (rule_strings nested)

let test_rules_preserves_competing_outside_selector () =
  let original =
    rules ".card{color:red}.card .title{width:1px}.title{font-weight:bold}"
  in
  Alcotest.(check (list string))
    "outside competitor prevents nesting" (rule_strings original)
    (rule_strings (Nest.rules original))

let suite =
  ( "nest",
    [
      Alcotest.test_case "contains and substitute" `Quick
        test_contains_and_substitute;
      Alcotest.test_case "combine relative nested and descendant" `Quick
        test_combine_relative_nested_and_descendant;
      Alcotest.test_case "merge_lone wrapper" `Quick test_merge_lone_wrapper;
      Alcotest.test_case "rules synthesizes isolated chain" `Quick
        test_rules_synthesizes_isolated_chain;
      Alcotest.test_case "rules preserves competing outside selector" `Quick
        test_rules_preserves_competing_outside_selector;
    ] )
