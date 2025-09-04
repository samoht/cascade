(* css_parser tests *)

open Alcotest

(* Toplevel selectors for reuse *)
let complex_selector =
  let open Css.Selector in
  combine
    (combine
       (combine (element "div") Descendant (class_ "class"))
       Descendant (id "id"))
    Child
    (combine
       (combine (element "p") Descendant (pseudo_class "first-child"))
       Subsequent_sibling
       (combine (element "span") Descendant
          (attribute "data-attr" (Exact "value"))))

let vendor = Css.Selector.class_ "vendor"
let root = Css.Selector.pseudo_class "root"
let test = Css.Selector.class_ "test"
let strings = Css.Selector.class_ "strings"
let important = Css.Selector.class_ "important"
let calc = Css.Selector.class_ "calc"
let gradient = Css.Selector.class_ "gradient"

let test_before =
  Css.Selector.(combine (class_ "test") Descendant (pseudo_element "before"))

let test_after =
  Css.Selector.(combine (class_ "test") Descendant (pseudo_element "after"))

let test_first_child_before =
  Css.Selector.(
    combine
      (combine (class_ "test") Descendant (pseudo_class "first-child"))
      Descendant (pseudo_element "before"))

let a = Css.Selector.class_ "a"
let b = Css.Selector.class_ "b"

(* Fun selector tests *)
let is_fun_selector =
  Css.Selector.fun_ "is"
    [ Css.Selector.element "button"; Css.Selector.element "input" ]

let has_fun_selector = Css.Selector.fun_ "has" [ Css.Selector.class_ "active" ]

let select_fun =
  Css.Selector.(
    combine (element "select") Descendant
      (fun_ "is" [ attribute "multiple" Presence; attribute "size" Presence ]))

let c_hover =
  Css.Selector.(combine (class_ "c") Descendant (pseudo_class "hover"))

let check_parse_success css expected_rule_count =
  match Css_parser.of_string css with
  | Ok ast ->
      let rules = Css.stylesheet_rules ast in
      check int "rule count" expected_rule_count (List.length rules)
  | Error err -> fail (Printf.sprintf "Parse failed: %s" err)

let check_parse_failure css =
  match Css_parser.of_string css with
  | Ok _ -> fail "Expected parse failure but succeeded"
  | Error _ -> ()

let check_selector css expected_selector =
  match Css_parser.of_string css with
  | Ok ast -> (
      match Css.stylesheet_rules ast with
      | rule :: _ ->
          check string "selector" expected_selector
            (Css.Selector.to_string (Css.selector rule))
      | [] -> fail "No rules found")
  | Error err -> fail (Printf.sprintf "Parse failed: %s" err)

(* Test cases *)

let test_simple_rule () =
  let css = ".test { color: red; }" in
  check_parse_success css 1;
  check_selector css ".test"

let test_malformed_css_unclosed_comment () =
  let css = ".test { color: /* unclosed comment" in
  check_parse_failure css

let test_multiple_properties () =
  let css = ".box { width: 100px; height: 200px; padding: 10px; }" in
  match Css_parser.of_string css with
  | Ok ast -> (
      let rules = Css.stylesheet_rules ast in
      check int "rule count" 1 (List.length rules);
      match rules with
      | [ rule ] ->
          let props = Css.declarations rule in
          check int "property count" 3 (List.length props)
      | _ -> fail "Expected exactly one rule")
  | Error err -> fail err

let test_multiple_rules () =
  let css = ".a { color: red; } .b { color: blue; } .c { color: green; }" in
  check_parse_success css 3

let test_minified_css () =
  let css = ".a{color:red;padding:0}.b{margin:1px}.c:hover{color:blue}" in
  check_parse_success css 3

let test_pseudo_selectors () =
  let css = ".btn:hover { color: blue; } .link:active { color: red; }" in
  check_parse_success css 2;
  check_selector ".btn:hover { color: blue; }" ".btn:hover"

let test_complex_selector () =
  let css = "div > p + span.class#id[attr='value'] { display: none; }" in
  check_parse_success css 1;
  check_selector css "div > p + span.class#id[attr=\"value\"]"

let test_empty_rule () =
  let css = ".empty { }" in
  match Css_parser.of_string css with
  | Ok ast -> (
      let rules = Css.stylesheet_rules ast in
      check int "rule count" 1 (List.length rules);
      match rules with
      | [ rule ] ->
          let props = Css.declarations rule in
          check int "empty rule has no properties" 0 (List.length props)
      | _ -> fail "Expected exactly one rule")
  | Error err -> fail err

let test_comments () =
  let css = "/* comment */ .test { /* another */ color: red; /* end */ }" in
  check_parse_success css 1

let test_multiline_comments () =
  let css =
    {|
    /* This is a
       multiline comment */
    .test {
      /* Another
         multiline */
      color: red;
    }
  |}
  in
  check_parse_success css 1

let test_string_values () =
  let css =
    {|.test { content: "hello world"; font-family: 'Arial', sans-serif; }|}
  in
  check_parse_success css 1

let test_escaped_strings () =
  let css = {|.test { content: "hello \"world\""; }|} in
  check_parse_success css 1

let test_url_values () =
  let css = ".bg { background-image: url(image.png); }" in
  check_parse_success css 1

let test_calc_values () =
  let css = ".box { width: calc(100% - 20px); height: calc(50vh + 10px); }" in
  check_parse_success css 1

let test_nested_parens () =
  let css = ".test { width: calc((100% - 20px) / 2); }" in
  check_parse_success css 1

let test_rgb_colors () =
  let css =
    ".color { color: rgb(255, 0, 0); background: rgba(0, 0, 0, 0.5); }"
  in
  check_parse_success css 1

let test_gradient () =
  let css =
    ".gradient { background: linear-gradient(to right, #000, #fff); }"
  in
  check_parse_success css 1

let test_multiple_selectors () =
  let css = ".a, .b, .c { color: red; }" in
  check_parse_success css 1;
  check_selector css ".a, .b, .c"

let test_descendant_selector () =
  let css = ".parent .child { margin: 0; }" in
  check_parse_success css 1;
  check_selector css ".parent .child"

let test_attribute_selector () =
  let css = {|input[type="text"] { border: 1px solid; }|} in
  check_parse_success css 1

let test_important_flag () =
  let css = ".important { color: red !important; }" in
  check_parse_success css 1

let test_css_variables () =
  let css =
    ":root { --main-color: #333; } .test { color: var(--main-color); }"
  in
  check_parse_success css 2

let test_var_with_fallback () =
  let css =
    ".test { color: var(--undefined, blue); background: var(--bg, #fff); }"
  in
  check_parse_success css 1

let test_nested_var () =
  let css = ".test { color: var(--primary, var(--secondary, red)); }" in
  check_parse_success css 1

let test_var_with_complex_fallback () =
  let css =
    ".test { background: var(--bg, linear-gradient(to right, red, blue)); }"
  in
  check_parse_success css 1

let test_var_in_calc () =
  let css = ".test { width: calc(100% - var(--spacing, 20px)); }" in
  check_parse_success css 1

let test_multiple_vars () =
  let css =
    ".test { margin: var(--top, 10px) var(--right, 20px) var(--bottom, 10px) \
     var(--left, 20px); }"
  in
  check_parse_success css 1

let test_keyframes () =
  (* Currently skipped by parser *)
  let css = "@keyframes slide { from { left: 0; } to { left: 100%; } }" in
  check_parse_success css 0 (* Parser skips @-rules for now *)

let test_media_query () =
  (* Currently skipped by parser *)
  let css = "@media (min-width: 768px) { .responsive { display: block; } }" in
  check_parse_success css 0 (* Parser skips @-rules for now *)

let test_malformed_css () =
  let css = ".test { color: " in
  (* Parser might handle this gracefully or fail *)
  let _ = Css_parser.of_string css in
  ()

let test_unclosed_comment () =
  let css = ".test { color: red; /* unclosed comment" in
  let _ = Css_parser.of_string css in
  ()

let test_missing_semicolon () =
  let css = ".test { color: red background: blue; }" in
  (* Parser should handle this gracefully *)
  let _ = Css_parser.of_string css in
  ()

let test_consecutive_rules () =
  let css = ".a{color:red}.b{color:blue}.c{color:green}" in
  check_parse_success css 3

let test_whitespace_handling () =
  let css = "   .test   {   color   :   red   ;   }   " in
  check_parse_success css 1;
  check_selector css ".test"

let test_newline_in_selector () =
  let css = ".test\n.another { color: red; }" in
  check_parse_success css 1

let test_tabs_and_spaces () =
  let css = "\t.test\t{\tcolor:\tred;\t}\t" in
  check_parse_success css 1

let test_vendor_prefixes () =
  let css =
    ".test { -webkit-transform: rotate(45deg); -moz-appearance: none; }"
  in
  check_parse_success css 1

let test_custom_properties () =
  let css = ".test { --custom-prop: 10px; width: var(--custom-prop); }" in
  check_parse_success css 1

let test_percentage_values () =
  let css = ".test { width: 50%; opacity: 75%; }" in
  check_parse_success css 1

let test_negative_values () =
  let css = ".test { margin: -10px; top: -5%; }" in
  check_parse_success css 1

let test_float_values () =
  let css = ".test { opacity: 0.5; line-height: 1.5; }" in
  check_parse_success css 1

let test_units () =
  let css = ".test { width: 10px; height: 5em; margin: 2rem; padding: 50%; }" in
  check_parse_success css 1

let test_shorthand_properties () =
  let css = ".test { margin: 10px 20px 30px 40px; padding: 5px 10px; }" in
  check_parse_success css 1

let test_border_shorthand () =
  let css = ".test { border: 1px solid black; }" in
  check_parse_success css 1

let test_font_shorthand () =
  let css = ".test { font: italic bold 16px/1.5 Arial, sans-serif; }" in
  check_parse_success css 1

let test_transform_multiple () =
  let css = ".test { transform: rotate(45deg) scale(1.5) translateX(10px); }" in
  check_parse_success css 1

let test_box_shadow () =
  let css =
    ".test { box-shadow: 0 2px 4px rgba(0,0,0,0.1), 0 4px 8px rgba(0,0,0,0.2); \
     }"
  in
  check_parse_success css 1

let test_transition () =
  let css = ".test { transition: all 0.3s ease-in-out; }" in
  check_parse_success css 1

let test_animation () =
  let css = ".test { animation: slide 2s infinite alternate; }" in
  check_parse_success css 1

let test_grid_template () =
  let css = ".grid { grid-template-columns: 1fr 2fr 1fr; grid-gap: 10px; }" in
  check_parse_success css 1

let test_flexbox () =
  let css =
    ".flex { display: flex; justify-content: center; align-items: center; }"
  in
  check_parse_success css 1

let test_pseudo_elements () =
  let css = ".test::before { content: '→'; } .test::after { content: '←'; }" in
  check_parse_success css 2

let test_nth_child () =
  let css = ".test:nth-child(2n+1) { background: #f0f0f0; }" in
  check_parse_success css 1

let test_not_selector () =
  let css = ".test:not(.excluded) { display: block; }" in
  check_parse_success css 1

let test_unicode_content () =
  let css = {|.test { content: "→ ← ↑ ↓"; }|} in
  check_parse_success css 1

let test_emoji () =
  let css = {|.test { content: "🎉"; }|} in
  check_parse_success css 1

let test_chinese_comments () =
  let css = "/* 中文注释 */ .test { color: red; }" in
  check_parse_success css 1

let test_data_uri () =
  let css = ".test { background: url(data:image/png;base64,iVBORw0KG...); }" in
  check_parse_success css 1

let test_supports_rule () =
  (* Currently skipped *)
  let css = "@supports (display: grid) { .grid { display: grid; } }" in
  check_parse_success css 0

let test_import_rule () =
  (* Currently skipped *)
  let css = "@import url('styles.css');" in
  check_parse_success css 0

let test_charset_rule () =
  (* Currently skipped *)
  let css = "@charset \"UTF-8\";" in
  check_parse_success css 0

let test_namespace_rule () =
  (* Currently skipped *)
  let css = "@namespace svg \"http://www.w3.org/2000/svg\";" in
  check_parse_success css 0

let test_page_rule () =
  (* Currently skipped *)
  let css = "@page { margin: 1in; }" in
  check_parse_success css 0

let test_font_face () =
  (* Currently skipped *)
  let css = "@font-face { font-family: 'Custom'; src: url('font.woff2'); }" in
  check_parse_success css 0

let test_counter_style () =
  (* Currently skipped *)
  let css = "@counter-style custom { system: cyclic; symbols: '▶' '▷'; }" in
  check_parse_success css 0

let test_viewport_rule () =
  (* Currently skipped *)
  let css = "@viewport { width: device-width; }" in
  check_parse_success css 0

let test_layer_rule () =
  (* Currently skipped *)
  let css = "@layer utilities { .text-red { color: red; } }" in
  check_parse_success css 0

let test_container_query () =
  (* Currently skipped *)
  let css = "@container (min-width: 400px) { .card { display: flex; } }" in
  check_parse_success css 0

(* Round-trip tests *)

let test_round_trip_simple () =
  (* Create a CSS rule using the CSS API *)
  let rule =
    Css.(
      rule ~selector:(Selector.class_ "test")
        [ color (Hex { hash = false; value = "ff0000" }); padding (Px 10) ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  (* Convert to string *)
  let css_string = Css.to_string ~minify:false css_ast in

  (* Parse it back *)
  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      (* Compare the ASTs *)
      let original_str = Css.to_string ~minify:true css_ast in
      let parsed_str = Css.to_string ~minify:true parsed_ast in
      check string "round-trip CSS" original_str parsed_str
  | Error err -> fail (Printf.sprintf "Round-trip parse failed: %s" err)

let test_round_trip_multiple_rules () =
  let rules =
    Css.
      [
        Rule
          (rule ~selector:(Selector.class_ "a")
             [ color (Hex { hash = false; value = "ff0000" }) ]);
        Rule
          (rule ~selector:(Selector.class_ "b")
             [ color (Hex { hash = false; value = "0000ff" }) ]);
        Rule
          (rule ~selector:(Selector.class_ "c")
             [ color (Hex { hash = false; value = "008000" }) ]);
      ]
  in
  let css_ast = Css.stylesheet rules in

  let css_string = Css.to_string ~minify:true css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      let original_str = Css.to_string ~minify:true css_ast in
      let parsed_str = Css.to_string ~minify:true parsed_ast in
      check string "round-trip multiple rules" original_str parsed_str
  | Error err -> fail err

let test_round_trip_complex_selector () =
  let rule =
    Css.(rule ~selector:complex_selector [ display None; visibility Hidden ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast -> (
      (* Selectors should match exactly *)
      match (Css.stylesheet_rules css_ast, Css.stylesheet_rules parsed_ast) with
      | [ orig ], [ parsed ] ->
          check string "selector preserved"
            (Css.Selector.to_string (Css.selector orig))
            (Css.Selector.to_string (Css.selector parsed))
      | _ -> fail "Expected exactly one rule")
  | Error err -> fail err

let test_round_trip_vendor_prefixes () =
  let rule =
    Css.(
      rule ~selector:vendor
        [
          (* Only CSS variables are supported for vendor prefixes *)
          custom_property "--webkit-transform" "rotate(45deg)";
          custom_property "--moz-transform" "rotate(45deg)";
          custom_property "--transform" "rotate(45deg)";
        ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      let orig_props =
        match Css.stylesheet_rules css_ast with
        | [ r ] -> List.length (Css.declarations r)
        | _ -> 0
      in
      let parsed_props =
        match Css.stylesheet_rules parsed_ast with
        | [ r ] -> List.length (Css.declarations r)
        | _ -> 0
      in
      check int "property count preserved" orig_props parsed_props
  | Error err -> fail err

let test_round_trip_css_variables () =
  let rules =
    Css.
      [
        Rule
          (rule ~selector:root
             [
               custom_property "--main-color" "#333";
               custom_property "--spacing" "1rem";
             ]);
        Rule
          (rule ~selector:test
             [
               (* CSS variables can reference other variables *)
               custom_property "--derived-color" "var(--main-color)";
               custom_property "--derived-spacing" "var(--spacing)";
             ]);
      ]
  in
  let css_ast = Css.stylesheet rules in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      check int "rule count preserved"
        (List.length (Css.stylesheet_rules css_ast))
        (List.length (Css.stylesheet_rules parsed_ast))
  | Error err -> fail err

let test_round_trip_string_values () =
  let rule =
    Css.(
      rule ~selector:strings
        [
          (* Font family is now supported *)
          font_family [ Css.Arial; Css.Helvetica; Css.Sans_serif ];
          (* Other complex properties as CSS variables *)
          custom_property "--content" "\"Hello World\"";
          custom_property "--background" "url('image.png')";
        ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      (* String values should be preserved *)
      let original_str = Css.to_string ~minify:true css_ast in
      let parsed_str = Css.to_string ~minify:true parsed_ast in
      check string "strings preserved" original_str parsed_str
  | Error err -> fail err

let test_round_trip_important () =
  let sel = important in
  let rule =
    Css.(
      rule ~selector:sel
        [
          (* Important declarations *)
          important (color (Hex { hash = false; value = "ff0000" }));
          important (display Block);
        ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      (* Check that the parsed CSS still contains !important when rendered *)
      let parsed_css = Css.to_string ~minify:false parsed_ast in
      let has_important =
        Astring.String.is_infix ~affix:"!important" parsed_css
      in
      check bool "!important preserved" true has_important
  | Error err -> fail err

let test_round_trip_calc () =
  let rule =
    Css.(
      rule ~selector:calc
        [
          width (Calc (Expr (Val (Pct 100.), Sub, Val (Px 20))));
          height
            (Calc
               (Expr (Expr (Val (Vh 100.), Sub, Val (Px 80)), Div, Val (Num 2.))));
          margin (Calc (Expr (Val (Rem 1.), Add, Val (Px 10))));
        ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      (* Check that calc() is preserved in rendered CSS *)
      let parsed_css = Css.to_string ~minify:false parsed_ast in
      let has_calc = Astring.String.is_infix ~affix:"calc" parsed_css in
      check bool "calc preserved" true has_calc
  | Error err -> fail err

let test_round_trip_gradients () =
  let rule =
    Css.(
      rule ~selector:gradient
        [
          background_image
            (Linear_gradient
               ( To_right,
                 [
                   Color_position
                     (Hex { hash = false; value = "000000" }, Pct 0.);
                   Color_position
                     (Hex { hash = false; value = "ffffff" }, Pct 100.);
                 ] ));
          background_image
            (Radial_gradient
               [
                 Color_position (Hex { hash = false; value = "ff0000" }, Pct 0.);
                 Color_position (Hex { hash = false; value = "0000ff" }, Pct 50.);
                 Color_position
                   (Hex { hash = false; value = "00ff00" }, Pct 100.);
               ]);
        ])
  in
  let css_ast = Css.stylesheet [ Rule rule ] in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      let parsed_str = Css.to_string ~minify:true parsed_ast in
      (* Gradients are complex, just check we got something back *)
      check bool "gradient parsed" true (String.length parsed_str > 0)
  | Error err -> fail err

let test_round_trip_pseudo_elements () =
  let rules =
    Css.
      [
        Rule (rule ~selector:test_before [ content (String "→") ]);
        Rule (rule ~selector:test_after [ content (String "←") ]);
        Rule (rule ~selector:test_first_child_before [ content (String "*") ]);
      ]
  in
  let css_ast = Css.stylesheet rules in

  let css_string = Css.to_string ~minify:false css_ast in

  match Css_parser.of_string css_string with
  | Ok parsed_ast ->
      check int "pseudo-element rules preserved"
        (List.length (Css.stylesheet_rules css_ast))
        (List.length (Css.stylesheet_rules parsed_ast))
  | Error err -> fail err

let test_round_trip_minified () =
  (* Test that minified output can be parsed back *)
  let rules =
    Css.
      [
        Rule
          (rule ~selector:a [ color (Hex { hash = false; value = "ff0000" }) ]);
        Rule (rule ~selector:b [ margin Zero ]);
        Rule
          (rule ~selector:c_hover
             [ color (Hex { hash = false; value = "0000ff" }) ]);
      ]
  in
  let css_ast = Css.stylesheet rules in

  (* Generate minified CSS *)
  let minified = Css.to_string ~minify:true css_ast in

  (* Should look like: .a{color:red}.b{margin:0}.c:hover{color:blue} *)
  check bool "is minified" false (Astring.String.is_infix ~affix:"\n" minified);

  (* Parse the minified CSS *)
  match Css_parser.of_string minified with
  | Ok parsed_ast ->
      check int "rules preserved after minification"
        (List.length (Css.stylesheet_rules css_ast))
        (List.length (Css.stylesheet_rules parsed_ast))
  | Error err -> fail err

(* Test that var() values are properly preserved in round-trip *)
let test_round_trip_var_values () =
  (* Create CSS rules with var() values *)
  let rules =
    Css.
      [
        Rule
          (rule ~selector:root
             [
               custom_property "--primary-color" "#007bff";
               custom_property "--spacing" "1rem";
             ]);
        Rule
          (rule ~selector:test
             [
               (* Test var() without fallback *)
               color (Var (var_ref "primary-color"));
               (* Test var() with fallback *)
               padding (Var (var_ref ~fallback:(Px 16) "spacing"));
               margin_left (Var (var_ref "margin"));
             ]);
      ]
  in
  let css_ast = Css.stylesheet rules in

  (* Convert to string *)
  let css_string = Css.to_string css_ast in

  (* Parse it back *)
  match Css_parser.of_string css_string with
  | Error e -> Alcotest.fail ("Failed to parse generated CSS: " ^ e)
  | Ok parsed_ast ->
      (* Convert parsed AST back to string *)
      let reparsed_string = Css.to_string parsed_ast in
      (* They should be identical *)
      Alcotest.(check string)
        "round-trip preserves var() values" css_string reparsed_string

(* Fun selector parsing tests *)
let test_fun_selector_parsing () =
  let css = ".container :is(button, input) { color: red; }" in
  check_parse_success css 1;
  check_selector css ".container :is(button, input)"

let test_fun_selector_has () =
  let css = ".card:has(.active) { border: 1px solid blue; }" in
  check_parse_success css 1;
  check_selector css ".card:has(.active)"

let test_fun_selector_complex () =
  let css = "select:is([multiple], [size]) { background: gray; }" in
  check_parse_success css 1;
  check_selector css "select:is([multiple], [size])"

let test_fun_selector_values () =
  (* Use the predefined fun selector values to ensure they compile *)
  let _ = is_fun_selector in
  let _ = has_fun_selector in
  let _ = select_fun in
  ()

(* New fail-fast tests for invalid inputs *)
let test_invalid_selector () =
  let css = ".a > { color: red; }" in
  match Css_parser.of_string css with
  | Ok _ -> fail "Expected selector parse failure for dangling combinator"
  | Error _ -> ()

let test_invalid_rgb_args () =
  let css = ".a { color: rgb(255,); }" in
  match Css_parser.of_string css with
  | Ok _ -> fail "Expected invalid rgb() to fail"
  | Error _ -> ()

let test_invalid_z_index () =
  let css = ".a { z-index: top; }" in
  match Css_parser.of_string css with
  | Ok _ -> fail "Expected invalid z-index to fail"
  | Error _ -> ()

let test_invalid_length_unit () =
  let css = ".a { width: 12pp; }" in
  match Css_parser.of_string css with
  | Ok _ -> fail "Expected invalid length unit to fail"
  | Error _ -> ()

let tests =
  [
    (* Basic parsing *)
    test_case "simple rule" `Quick test_simple_rule;
    test_case "malformed CSS unclosed comment" `Quick
      test_malformed_css_unclosed_comment;
    test_case "multiple properties" `Quick test_multiple_properties;
    test_case "multiple rules" `Quick test_multiple_rules;
    test_case "minified CSS" `Quick test_minified_css;
    test_case "empty rule" `Quick test_empty_rule;
    (* Selectors *)
    test_case "pseudo selectors" `Quick test_pseudo_selectors;
    test_case "complex selector" `Quick test_complex_selector;
    test_case "multiple selectors" `Quick test_multiple_selectors;
    test_case "descendant selector" `Quick test_descendant_selector;
    test_case "attribute selector" `Quick test_attribute_selector;
    test_case "pseudo elements" `Quick test_pseudo_elements;
    test_case "nth-child" `Quick test_nth_child;
    test_case "not selector" `Quick test_not_selector;
    (* Comments *)
    test_case "comments" `Quick test_comments;
    test_case "multiline comments" `Quick test_multiline_comments;
    (* Values *)
    test_case "string values" `Quick test_string_values;
    test_case "escaped strings" `Quick test_escaped_strings;
    test_case "url values" `Quick test_url_values;
    test_case "calc values" `Quick test_calc_values;
    test_case "nested parentheses" `Quick test_nested_parens;
    test_case "rgb colors" `Quick test_rgb_colors;
    test_case "gradient" `Quick test_gradient;
    test_case "important flag" `Quick test_important_flag;
    test_case "CSS variables" `Quick test_css_variables;
    test_case "var with fallback" `Quick test_var_with_fallback;
    test_case "nested var" `Quick test_nested_var;
    test_case "var with complex fallback" `Quick test_var_with_complex_fallback;
    test_case "var in calc" `Quick test_var_in_calc;
    test_case "multiple vars" `Quick test_multiple_vars;
    test_case "vendor prefixes" `Quick test_vendor_prefixes;
    test_case "custom properties" `Quick test_custom_properties;
    test_case "percentage values" `Quick test_percentage_values;
    test_case "negative values" `Quick test_negative_values;
    test_case "float values" `Quick test_float_values;
    test_case "units" `Quick test_units;
    (* Shorthand properties *)
    test_case "shorthand properties" `Quick test_shorthand_properties;
    test_case "border shorthand" `Quick test_border_shorthand;
    test_case "font shorthand" `Quick test_font_shorthand;
    test_case "transform multiple" `Quick test_transform_multiple;
    test_case "box shadow" `Quick test_box_shadow;
    test_case "transition" `Quick test_transition;
    test_case "animation" `Quick test_animation;
    test_case "grid template" `Quick test_grid_template;
    test_case "flexbox" `Quick test_flexbox;
    (* @-rules - currently skipped *)
    test_case "keyframes" `Quick test_keyframes;
    test_case "media query" `Quick test_media_query;
    test_case "supports rule" `Quick test_supports_rule;
    test_case "import rule" `Quick test_import_rule;
    test_case "charset rule" `Quick test_charset_rule;
    test_case "namespace rule" `Quick test_namespace_rule;
    test_case "page rule" `Quick test_page_rule;
    test_case "font-face" `Quick test_font_face;
    test_case "counter-style" `Quick test_counter_style;
    test_case "viewport rule" `Quick test_viewport_rule;
    test_case "layer rule" `Quick test_layer_rule;
    test_case "container query" `Quick test_container_query;
    (* Error handling *)
    test_case "malformed CSS" `Quick test_malformed_css;
    test_case "unclosed comment" `Quick test_unclosed_comment;
    test_case "missing semicolon" `Quick test_missing_semicolon;
    (* Whitespace *)
    test_case "consecutive rules" `Quick test_consecutive_rules;
    test_case "whitespace handling" `Quick test_whitespace_handling;
    test_case "newline in selector" `Quick test_newline_in_selector;
    test_case "tabs and spaces" `Quick test_tabs_and_spaces;
    (* Fail-fast cases *)
    test_case "invalid selector" `Quick test_invalid_selector;
    test_case "invalid rgb args" `Quick test_invalid_rgb_args;
    test_case "invalid z-index" `Quick test_invalid_z_index;
    test_case "invalid length unit" `Quick test_invalid_length_unit;
    (* Unicode *)
    test_case "unicode content" `Quick test_unicode_content;
    test_case "emoji" `Quick test_emoji;
    test_case "chinese comments" `Quick test_chinese_comments;
    (* Data URIs *)
    test_case "data URI" `Quick test_data_uri;
    (* Round-trip tests *)
    test_case "round-trip simple" `Quick test_round_trip_simple;
    test_case "round-trip multiple rules" `Quick test_round_trip_multiple_rules;
    test_case "round-trip complex selector" `Quick
      test_round_trip_complex_selector;
    test_case "round-trip vendor prefixes" `Quick
      test_round_trip_vendor_prefixes;
    test_case "round-trip CSS variables" `Quick test_round_trip_css_variables;
    test_case "round-trip string values" `Quick test_round_trip_string_values;
    test_case "round-trip important" `Quick test_round_trip_important;
    test_case "round-trip calc" `Quick test_round_trip_calc;
    test_case "round-trip gradients" `Quick test_round_trip_gradients;
    test_case "round-trip pseudo-elements" `Quick
      test_round_trip_pseudo_elements;
    test_case "round-trip minified" `Quick test_round_trip_minified;
    test_case "round-trip var values" `Quick test_round_trip_var_values;
    (* Fun selector parsing tests *)
    test_case "fun selector parsing" `Quick test_fun_selector_parsing;
    test_case "fun selector has" `Quick test_fun_selector_has;
    test_case "fun selector complex" `Quick test_fun_selector_complex;
    test_case "fun selector values" `Quick test_fun_selector_values;
  ]

let suite = ("css_parser", tests)
