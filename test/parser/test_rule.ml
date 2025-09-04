open Alcotest
open Css_parser

let test_one_style_rule () =
  let r = Reader.of_string ".class { color: red; }" in
  match Rule.one r with
  | Some _ -> check bool "parsed style rule" true true
  | None -> fail "Expected style rule"

let test_one_at_rule () =
  let r = Reader.of_string "@media screen { .class { color: blue; } }" in
  match Rule.one r with
  | Some _ -> check bool "parsed at-rule" true true
  | None -> fail "Expected at-rule"

let test_rules () =
  let css = ".a { color: red; } .b { margin: 10px; }" in
  let r = Reader.of_string css in
  let rules = Rule.rules r in
  check int "rule count" 2 (List.length rules)

let test_stylesheet () =
  let css =
    ".class { padding: 5px; } @media print { .hidden { display: none; } }"
  in
  let r = Reader.of_string css in
  let _ = Rule.stylesheet r in
  check bool "parsed stylesheet" true true

let test_empty () =
  let r = Reader.of_string "" in
  let rules = Rule.rules r in
  check int "empty" 0 (List.length rules)

let tests =
  [
    test_case "one_style_rule" `Quick test_one_style_rule;
    test_case "one_at_rule" `Quick test_one_at_rule;
    test_case "rules" `Quick test_rules;
    test_case "stylesheet" `Quick test_stylesheet;
    test_case "empty" `Quick test_empty;
  ]
