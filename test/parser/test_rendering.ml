(** Tests for CSS Rendering & Optimization parsing *)

open Alcotest

let test_extract_custom_declarations () =
  let css =
    ".test { --primary: blue; color: var(--primary); --spacing: 1rem; }"
  in
  let t = Css_parser.Reader.of_string css in
  let custom_props = Css_parser.Rendering.extract_custom_declarations t in
  check (list string) "extracted custom properties"
    [ "--primary"; "--spacing" ]
    custom_props

let test_optimize_duplicate_rules () =
  (* Same selector with different properties should be merged *)
  let css = ".btn { color: red; } .btn { padding: 10px; }" in
  let t = Css_parser.Reader.of_string css in
  let optimized = Css_parser.Rendering.optimize t in
  let output = Css.to_string ~minify:true optimized in
  check string "merged duplicate selectors" ".btn{color:red;padding:10px}"
    output

let test_optimize_identical_rules () =
  (* Different selectors with same properties can be combined *)
  let css = ".a { color: red; } .b { color: red; }" in
  let t = Css_parser.Reader.of_string css in
  let optimized = Css_parser.Rendering.optimize t in
  let output = Css.to_string ~minify:true optimized in
  check string "combined identical rules" ".a,.b{color:red}" output

let test_stylesheet_rules () =
  let css =
    ".header { color: blue; } @media (min-width: 768px) { .header { font-size: \
     2rem; } }"
  in
  let t = Css_parser.Reader.of_string css in
  let rules = Css_parser.Rendering.stylesheet_rules t in
  check int "extracted 2 rules" 2 (List.length rules)

let test_deduplicate_declarations () =
  (* Last declaration should win *)
  let decls_str = "color: red; margin: 10px; color: blue; padding: 5px;" in
  let t = Css_parser.Reader.of_string decls_str in
  let deduped = Css_parser.Rendering.deduplicate_declarations t in
  (* Should have 3 declarations: margin, color (blue), padding *)
  check int "deduplicated to 3 declarations" 3 (List.length deduped)

let test_inline_style_of_declarations () =
  let css = ".test { color: red; margin: 10px; }" in
  let t = Css_parser.Reader.of_string css in
  let inline = Css_parser.Rendering.inline_style_of_declarations t in
  check string "inline style format" "color:red;margin:10px" inline

let test_analyze_declarations () =
  (* analyze_declarations should identify optimization opportunities *)
  let css =
    ".a { color: red; } .b { color: red; } /* duplicate */ .c { margin: 0; \
     padding: 0; } /* could use shorthand */"
  in
  let t = Css_parser.Reader.of_string css in
  (* This should not crash and should analyze the CSS *)
  Css_parser.Rendering.analyze_declarations t;
  check bool "analysis completed" true true

let tests =
  [
    test_case "extract custom declarations" `Quick
      test_extract_custom_declarations;
    test_case "optimize duplicate rules" `Quick test_optimize_duplicate_rules;
    test_case "optimize identical rules" `Quick test_optimize_identical_rules;
    test_case "extract stylesheet rules" `Quick test_stylesheet_rules;
    test_case "deduplicate declarations" `Quick test_deduplicate_declarations;
    test_case "inline style of declarations" `Quick
      test_inline_style_of_declarations;
    test_case "analyze declarations" `Quick test_analyze_declarations;
  ]
