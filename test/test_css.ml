(** High-level CSS integration tests

    Tests the integration between different CSS modules and end-to-end CSS
    generation using only the public Css module interface.

    Detailed module functionality is tested in dedicated files:
    - test_values.ml - CSS value types
    - test_properties.ml - CSS properties
    - test_selector.ml - CSS selectors
    - test_declaration.ml - CSS declarations
    - test_stylesheet.ml - Stylesheet construction
    - test_optimize.ml - CSS optimization
    - test_variables.ml - CSS variables
    - test_pp.ml - Pretty printing *)

open Css

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

(* Test end-to-end CSS generation *)
let generation () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ color (Hex { hash = true; value = "ff0000" }); padding [ Px 10. ] ];
        rule ~selector:card
          [
            margin [ Px 5. ];
            background_color (Hex { hash = false; value = "ffffff" });
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#ff0000;padding:10px}.card{margin:5px;background-color:#ffffff}\n"
    css

(* Test optimization flag works *)
let optimization_flag () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            color (Hex { hash = true; value = "ff0000" });
            color (Hex { hash = true; value = "0000ff" });
            (* duplicate - should be optimized *)
          ];
      ]
  in

  let css_optimized = Css.to_string ~minify:true ~optimize:true stylesheet in
  Alcotest.(check string)
    "optimized exact" ".btn{color:#0000ff}\n" css_optimized

(* Test layers work end-to-end *)
let layers_integration () =
  let utility_rule = rule ~selector:btn [ padding [ Px 10. ] ] in
  let stylesheet =
    Css.of_statements [ layer ~name:"utilities" [ utility_rule ] ]
  in

  let css = Css.to_string ~minify:true stylesheet in

  (* Should contain @layer *)
  Alcotest.(check bool)
    "contains @layer" true
    (Astring.String.is_infix ~affix:"@layer" css);
  Alcotest.(check bool)
    "contains layer name" true
    (Astring.String.is_infix ~affix:"utilities" css)

(* Test media queries work end-to-end *)
let media_integration () =
  let mobile_rule = rule ~selector:btn [ font_size (Rem 0.875) ] in
  let stylesheet =
    Css.of_statements [ media ~condition:"(max-width: 640px)" [ mobile_rule ] ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "media exact" "@media (max-width: 640px){.btn{font-size:.875rem}}\n" css

(* Test minify flag *)
let minify_flag () =
  let stylesheet =
    v [ rule ~selector:btn [ color (Hex { hash = true; value = "ff0000" }) ] ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#ff0000}\n" css_minified

(* Test important declarations *)
let important_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            important (color (Hex { hash = true; value = "ff0000" }));
            padding [ Px 10. ];
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#ff0000!important;padding:10px}\n" css

(* Test custom properties integration *)
let custom_properties_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ custom_property "--primary-color" "blue"; color (Named Blue) ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "custom properties exact" ".btn{--primary-color:blue;color:blue}\n" css

(* CSS Roundtrip Test: Parse generated CSS and compare roundtrip *)
let roundtrip () =
  let original_css =
    match Examples.read "empty_tailwind.css" with
    | Some css -> css
    | None -> Alcotest.fail "Could not read empty_tailwind.css from examples"
  in

  (* Parse the CSS with context on failure *)
  let parsed_stylesheet =
    match Css.of_string original_css with
    | Ok stylesheet -> stylesheet
    | Error err ->
        (* Format the structured error *)
        let formatted_error = Css.pp_parse_error err in
        Alcotest.fail ("Failed to parse CSS: " ^ formatted_error)
  in

  (* Render it back to string with same settings (minified) *)
  let roundtrip_css = Css.to_string ~minify:true parsed_stylesheet in

  (* Compare - they should be identical *)
  if original_css <> roundtrip_css then
    (* Show where the difference occurs *)
    match Tw_tools.Diff_format.first_diff_pos original_css roundtrip_css with
    | Some pos ->
        Fmt.epr "CSS roundtrip differs at position %d@." pos;
        Alcotest.fail "CSS roundtrip should be identical"
    | None -> Alcotest.fail "CSS roundtrip should be identical"
  else
    Alcotest.(check string)
      "CSS roundtrip should be identical" original_css roundtrip_css

(* Test AST introspection helpers *)
let test_layer_block () =
  let stylesheet =
    v
      [
        layer ~name:"theme" [ rule ~selector:btn [ color (hex "#ff0000") ] ];
        layer ~name:"utilities" [ rule ~selector:card [ padding [ Px 10. ] ] ];
        rule ~selector:(Selector.class_ "base") [ margin [ Px 5. ] ];
      ]
  in

  (* Test extracting theme layer *)
  let theme_stmts = layer_block "theme" stylesheet in
  Alcotest.(check bool) "theme layer found" true (Option.is_some theme_stmts);

  let theme_rules = theme_stmts |> Option.get |> rules_from_statements in
  Alcotest.(check int) "theme has one rule" 1 (List.length theme_rules);

  (* Test extracting non-existent layer *)
  let missing = layer_block "missing" stylesheet in
  Alcotest.(check bool) "missing layer not found" true (Option.is_none missing)

let test_rules_from_statements () =
  let stmts =
    [
      rule ~selector:btn [ color (hex "#ff0000") ];
      media ~condition:"(min-width: 768px)"
        [ rule ~selector:card [ padding [ Px 5. ] ] ];
      rule ~selector:card [ margin [ Px 10. ] ];
    ]
  in

  let rules = rules_from_statements stmts in
  Alcotest.(check int) "extracts 2 rules from statements" 2 (List.length rules);

  let selectors = List.map (fun (sel, _) -> Selector.to_string sel) rules in
  Alcotest.(check bool) "contains btn selector" true (List.mem ".btn" selectors);
  Alcotest.(check bool)
    "contains card selector" true
    (List.mem ".card" selectors)

let test_custom_prop_names () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let decls = [ color_def; margin_def; padding [ Px 10. ] ] in
  let prop_names = custom_prop_names decls in

  Alcotest.(check int) "finds 2 custom properties" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names)

let test_custom_props_from_rules () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let rules =
    [
      (btn, [ color_def; padding [ Px 10. ] ]);
      (card, [ margin_def; background_color (hex "#ffffff") ]);
    ]
  in

  let prop_names = custom_props_from_rules rules in

  Alcotest.(check int)
    "finds 2 custom properties total" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names);

  (* Test order preservation *)
  Alcotest.(check string)
    "first property is primary-color" "--primary-color" (List.hd prop_names)

let suite =
  [
    ( "css",
      [
        (* Integration tests using public Css interface only *)
        Alcotest.test_case "CSS generation end-to-end" `Quick generation;
        Alcotest.test_case "optimization flag works" `Quick optimization_flag;
        Alcotest.test_case "layers integration" `Quick layers_integration;
        Alcotest.test_case "media queries integration" `Quick media_integration;
        Alcotest.test_case "minify flag" `Quick minify_flag;
        Alcotest.test_case "important declarations" `Quick important_integration;
        Alcotest.test_case "custom properties" `Quick
          custom_properties_integration;
        Alcotest.test_case "CSS roundtrip parsing" `Quick roundtrip;
        (* AST introspection helpers *)
        Alcotest.test_case "layer_block extraction" `Quick test_layer_block;
        Alcotest.test_case "rules_from_statements" `Quick
          test_rules_from_statements;
        Alcotest.test_case "custom_prop_names" `Quick test_custom_prop_names;
        Alcotest.test_case "custom_props_from_rules" `Quick
          test_custom_props_from_rules;
      ] );
  ]
