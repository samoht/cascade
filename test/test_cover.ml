open Cascade

let decl css = Declaration.of_string css

let single_selector css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> (
      match
        List.find_map
          (function Stylesheet.Rule r -> Some r | _ -> None)
          stylesheet
      with
      | Some r -> r.selector
      | None -> Alcotest.fail "no rule")
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let sel css = Fmt.kstr single_selector "%s{x:1}" css

let test_empty_table_covers_nothing () =
  let t = Cover.v () in
  let s = sel ".a" in
  Alcotest.(check bool)
    "empty table reports no coverage" false
    (Cover.covered t s (decl "color:red"))

let test_add_then_covered () =
  let t = Cover.v () in
  let s = sel ".a" in
  Cover.add t s (decl "color:red");
  Alcotest.(check bool)
    "same property reported covered" true
    (Cover.covered t s (decl "color:blue"));
  Alcotest.(check bool)
    "different property not covered" false
    (Cover.covered t s (decl "width:1px"))

let test_per_selector_isolation () =
  let t = Cover.v () in
  let a = sel ".a" in
  let b = sel ".b" in
  Cover.add t a (decl "color:red");
  Alcotest.(check bool)
    "coverage is selector-scoped" false
    (Cover.covered t b (decl "color:blue"))

let test_importance_partitioning () =
  let t = Cover.v () in
  let s = sel ".a" in
  Cover.add t s (decl "color:red");
  Alcotest.(check bool)
    "normal does not cover important" false
    (Cover.covered t s (decl "color:blue!important"));
  Cover.add t s (decl "color:red!important");
  Alcotest.(check bool)
    "important covers important" true
    (Cover.covered t s (decl "color:blue!important"))

let suite =
  ( "cover",
    [
      Alcotest.test_case "empty table covers nothing" `Quick
        test_empty_table_covers_nothing;
      Alcotest.test_case "covered after add" `Quick test_add_then_covered;
      Alcotest.test_case "selector isolation" `Quick test_per_selector_isolation;
      Alcotest.test_case "importance partitioning" `Quick
        test_importance_partitioning;
    ] )
