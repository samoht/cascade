open Cascade

let parse_rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let test_build_indexes () =
  let rules =
    parse_rules
      ".a{color:red;width:1px}.b{color:red;height:1px}.c{color:red;padding:1px}"
  in
  let g = Css_graph.build rules in
  Alcotest.(check int) "three row vertices" 3 (Css_graph.n_rules g);
  Alcotest.(check int) "four distinct columns" 4 (Css_graph.n_decls g);
  Alcotest.(check int)
    "every (rule, decl) pair is an edge" 6 (Css_graph.n_edges g)

let test_greedy_cover_picks_shared_color () =
  (* Three rules sharing [color:red] only; greedy must propose factoring it
     out. *)
  let rules =
    parse_rules
      ".a{color:red;width:1px}.b{color:red;height:1px}.c{color:red;padding:1px}"
  in
  let g = Css_graph.build rules in
  match Css_graph.greedy_cover g with
  | [ f ] ->
      Alcotest.(check (array int))
        "all three rows" [| 0; 1; 2 |] f.Css_graph.rules;
      Alcotest.(check int) "exactly one column" 1 (Array.length f.decls);
      Alcotest.(check bool) "saving is positive" true (f.saving > 0)
  | other -> Alcotest.failf "expected one biclique, got %d" (List.length other)

let test_greedy_cover_no_share () =
  let rules = parse_rules ".a{width:1px}.b{height:1px}.c{padding:1px}" in
  let g = Css_graph.build rules in
  Alcotest.(check (list pass))
    "no biclique when rules share nothing" [] (Css_graph.greedy_cover g)

let test_greedy_cover_two_groups () =
  (* Two disjoint biclique opportunities; greedy should report both. *)
  let rules =
    parse_rules
      ".a{display:block;color:red;width:1px}.b{display:block;color:red;height:1px}.c{display:block;color:red;padding:1px}.d{display:block;background-color:blue;width:2px}.e{display:block;background-color:blue;height:2px}.f{display:block;background-color:blue;padding:2px}"
  in
  let g = Css_graph.build rules in
  let cover = Css_graph.greedy_cover g in
  Alcotest.(check int) "two bicliques" 2 (List.length cover);
  List.iter
    (fun f ->
      Alcotest.(check int) "3-rule biclique" 3 (Array.length f.Css_graph.rules);
      Alcotest.(check bool) "positive saving" true (f.saving > 0))
    cover

let suite =
  ( "css_graph",
    [
      Alcotest.test_case "build indexes rules and columns" `Quick
        test_build_indexes;
      Alcotest.test_case "greedy picks the single shared declaration" `Quick
        test_greedy_cover_picks_shared_color;
      Alcotest.test_case "greedy reports no biclique when nothing shared" `Quick
        test_greedy_cover_no_share;
      Alcotest.test_case "greedy reports both disjoint bicliques" `Quick
        test_greedy_cover_two_groups;
    ] )
