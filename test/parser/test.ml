(* css_parser test runner *)

let () =
  let open Alcotest in
  run "CSS Parser"
    (Test_reader.suite
    @ [
        ("values", Test_values.tests);
        ("property", Test_property.tests);
        ("custom_property", Test_custom_property.tests);
        ("selector", Test_selector.tests);
        ("declaration", Test_declaration.tests);
        ("rule", Test_rule.tests);
        ("css_parser", Test_css_parser.tests);
      ])
