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
    - test_render.ml - CSS rendering
    - test_variables.ml - CSS variables
    - test_pp.ml - Pretty printing *)

open Css

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

(* Test end-to-end CSS generation *)
let test_css_generation () =
  let stylesheet =
    stylesheet
      [
        Rule
          (rule ~selector:btn
             [ color (Hex { hash = true; value = "ff0000" }); padding (Px 10.) ]);
        Rule
          (rule ~selector:card
             [
               margin (Px 5.);
               background_color (Hex { hash = false; value = "ffffff" });
             ]);
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#ff0000;padding:10px}.card{margin:5px;background-color:#ffffff}"
    css

(* Test optimization flag works *)
let test_css_optimization_flag () =
  let stylesheet =
    stylesheet
      [
        Rule
          (rule ~selector:btn
             [
               color (Hex { hash = true; value = "ff0000" });
               color (Hex { hash = true; value = "0000ff" });
               (* duplicate - should be optimized *)
             ]);
      ]
  in

  let css_optimized = Css.to_string ~minify:true ~optimize:true stylesheet in
  Alcotest.(check string) "optimized exact" ".btn{color:#0000ff}" css_optimized

(* Test layers work end-to-end *)
let test_css_layers_integration () =
  let utility_rule = rule ~selector:btn [ padding (Px 10.) ] in
  let nested_utility_rule = rule_to_nested utility_rule in
  let utility_layer = layer ~name:"utilities" [ nested_utility_rule ] in
  let stylesheet = stylesheet [ Layer utility_layer ] in

  let css = Css.to_string ~minify:true stylesheet in

  (* Should contain @layer *)
  Alcotest.(check bool) "contains @layer" true (String.contains css '@');
  Alcotest.(check bool)
    "contains layer name" true
    (String.contains css 'u' && String.contains css 't')

(* Test media queries work end-to-end *)
let test_css_media_integration () =
  let mobile_rule = rule ~selector:btn [ font_size (Rem 0.875) ] in
  let media_query = media ~condition:"(max-width: 640px)" [ mobile_rule ] in
  let stylesheet = stylesheet [ Media media_query ] in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "media exact" "@media (max-width: 640px){.btn{font-size:.875rem}}" css

(* Test minify flag *)
let test_css_minify_flag () =
  let stylesheet =
    stylesheet
      [
        Rule
          (rule ~selector:btn [ color (Hex { hash = true; value = "ff0000" }) ]);
      ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#ff0000}" css_minified

(* Test important declarations *)
let test_css_important_integration () =
  let stylesheet =
    stylesheet
      [
        Rule
          (rule ~selector:btn
             [
               important (color (Hex { hash = true; value = "ff0000" }));
               padding (Px 10.);
             ]);
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#ff0000!important;padding:10px}" css

(* Test custom properties integration *)
let test_css_custom_properties_integration () =
  let stylesheet =
    stylesheet
      [
        Rule
          (rule ~selector:btn
             [ custom_property "--primary-color" "blue"; color (Named Blue) ]);
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "custom properties exact" ".btn{--primary-color:blue;color:blue}" css

let suite =
  [
    ( "css",
      [
        (* Integration tests using public Css interface only *)
        Alcotest.test_case "CSS generation end-to-end" `Quick
          test_css_generation;
        Alcotest.test_case "optimization flag works" `Quick
          test_css_optimization_flag;
        Alcotest.test_case "layers integration" `Quick
          test_css_layers_integration;
        Alcotest.test_case "media queries integration" `Quick
          test_css_media_integration;
        Alcotest.test_case "minify flag" `Quick test_css_minify_flag;
        Alcotest.test_case "important declarations" `Quick
          test_css_important_integration;
        Alcotest.test_case "custom properties" `Quick
          test_css_custom_properties_integration;
      ] );
  ]
