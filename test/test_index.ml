open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
      |> Array.of_list
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let same a b =
  a == b
  || Declaration.hash a = Declaration.hash b
     && Declaration.equal_declaration a b

let decl css = Declaration.of_string css

let test_rows_are_sorted_unique () =
  let t =
    Index.v ~same
      (rules ".a{color:red;color:red}.b{width:1px}.c{color:red}.d{color:red}")
  in
  Alcotest.(check (array int))
    "one row per matching rule" [| 0; 2; 3 |]
    (Index.rows t (decl "color:red"))

let test_eligible_filters_rows () =
  let rs = rules ".a{color:red}.b{color:red}.c{color:red}" in
  let t =
    Index.v ~same
      ~keep:(fun rule ->
        not (String.equal (Css.Selector.to_string rule.selector) ".b"))
      rs
  in
  Alcotest.(check (array int))
    "filtered row is absent" [| 0; 2 |]
    (Index.rows t (decl "color:red"))

let suite =
  ( "index",
    [
      Alcotest.test_case "rows are sorted and unique" `Quick
        test_rows_are_sorted_unique;
      Alcotest.test_case "eligibility filters rows" `Quick
        test_eligible_filters_rows;
    ] )
