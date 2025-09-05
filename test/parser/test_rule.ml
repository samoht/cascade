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

let test_media_rule_parsing () =
  let css =
    "@media (min-width: 768px) { .a { color: red } .b { margin: 0 } }"
  in
  let r = Reader.of_string css in
  match Rule.stylesheet r with
  | sheet ->
      let medias = Css.stylesheet_media_queries sheet in
      check int "one media" 1 (List.length medias);
      let m = List.hd medias in
      check int "nested rules" 2 (Css.media_rules m |> List.length)

let test_layer_and_supports_parsing () =
  let css =
    "@layer components { .btn { padding: 4px } @supports (display:grid) { .g { \
     display: grid } } }"
  in
  let r = Reader.of_string css in
  let sheet = Rule.stylesheet r in
  let layers = Css.stylesheet_layers sheet in
  check int "one layer" 1 (List.length layers);
  let name = Css.layer_name (List.hd layers) in
  check string "layer name" "components" name

let test_unknown_at_rule_skipped () =
  let css = "@unknown stuff; .x { color: red }" in
  let r = Reader.of_string css in
  let sheet = Rule.stylesheet r in
  check int "rule preserved after unknown" 1
    (Css.stylesheet_rules sheet |> List.length)

let tests =
  [
    test_case "one_style_rule" `Quick test_one_style_rule;
    test_case "one_at_rule" `Quick test_one_at_rule;
    test_case "rules" `Quick test_rules;
    test_case "stylesheet" `Quick test_stylesheet;
    test_case "empty" `Quick test_empty;
    test_case "media_rule_parsing" `Quick test_media_rule_parsing;
    test_case "layer_and_supports_parsing" `Quick
      test_layer_and_supports_parsing;
    test_case "unknown_at_rule_skipped" `Quick test_unknown_at_rule_skipped;
  ]
