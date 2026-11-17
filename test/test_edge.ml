open Cascade

let sel_string sel = Selector.to_string ~minify:true sel

let test_selectors_splits_selector_lists () =
  let selectors =
    Edge.selectors (Selector.list [ Selector.class_ "a"; Selector.class_ "b" ])
    |> List.map sel_string
  in
  Alcotest.(check (list string)) "selector branches" [ ".a"; ".b" ] selectors;
  Alcotest.(check (list string))
    "singleton selector" [ ".a" ]
    (List.map sel_string (Edge.selectors (Selector.class_ "a")))

let suite =
  ( "edge",
    [
      Alcotest.test_case "selectors splits selector lists" `Quick
        test_selectors_splits_selector_lists;
    ] )
