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

let sel_string sel = Selector.to_string ~minify:true sel

let property_name (Edge.Packed property) =
  Pp.to_string ~minify:true Properties.pp_property property

let test_selectors_splits_selector_lists () =
  let selectors =
    Edge.selectors (Selector.list [ Selector.class_ "a"; Selector.class_ "b" ])
    |> List.map sel_string
  in
  Alcotest.(check (list string)) "selector branches" [ ".a"; ".b" ] selectors;
  Alcotest.(check (list string))
    "singleton selector" [ ".a" ]
    (List.map sel_string (Edge.selectors (Selector.class_ "a")))

let test_of_rule_cross_product_and_importance () =
  let edges = Edge.of_rule (rule ".a,.b{color:red;width:1px!important}") in
  Alcotest.(check int)
    "two selectors times two declarations" 4 (List.length edges);
  let names = List.map (fun edge -> property_name edge.Edge.property) edges in
  Alcotest.(check (list string))
    "property order repeats per selector"
    [ "color"; "width"; "color"; "width" ]
    names;
  Alcotest.(check (list bool))
    "importance preserved"
    [ false; true; false; true ]
    (List.map (fun edge -> edge.Edge.important) edges)

let test_of_rule_ignores_non_declarations () =
  let edges = Edge.of_rule (rule ".a{color:red;@unknown foo;}") in
  Alcotest.(check int) "only declaration edge" 1 (List.length edges);
  match edges with
  | [ edge ] ->
      Alcotest.(check string)
        "color edge" "color"
        (property_name edge.Edge.property)
  | _ -> Alcotest.fail "expected one edge"

let suite =
  ( "edge",
    [
      Alcotest.test_case "selectors splits selector lists" `Quick
        test_selectors_splits_selector_lists;
      Alcotest.test_case "of_rule cross product and importance" `Quick
        test_of_rule_cross_product_and_importance;
      Alcotest.test_case "of_rule ignores non-declarations" `Quick
        test_of_rule_ignores_non_declarations;
    ] )
