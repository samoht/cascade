(* css_parser test runner *)

let () =
  let open Alcotest in
  run "CSS Parser"
    [
      ("custom_property", Test_custom_property.tests);
      ("declaration", Test_declaration.tests);
      ("rule", Test_rule.tests);
      ("css_parser", Test_css_parser.tests);
    ]
