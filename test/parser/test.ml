(* css_parser test runner *)

let () =
  let open Alcotest in
  run "CSS Parser"
    (Test_reader.suite
    @ [
        ("css_parser", Test_css_parser.tests);
        ("declaration", Test_declaration.tests);
        ("selector", Test_selector.tests);
        ("rule", Test_rule.tests);
      ])
