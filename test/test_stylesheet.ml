module Stylesheet = Css.Stylesheet
module Selector = Css.Selector
open Css.Declaration

let test_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let selector = Stylesheet.selector rule in
  (* Just check we can get selector back *)
  Alcotest.(check bool) "selector exists" true (selector = Selector.class_ "red");
  Alcotest.(check int)
    "rule declarations count" 1
    (List.length (Stylesheet.declarations rule))

let test_media_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let media =
    Stylesheet.media ~condition:"screen and (min-width: 768px)" [ rule ]
  in
  Alcotest.(check string)
    "media condition" "screen and (min-width: 768px)"
    (Stylesheet.media_condition media);
  Alcotest.(check int)
    "media rules count" 1
    (List.length (Stylesheet.media_rules media))

let test_container_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let container =
    Stylesheet.container ~name:"sidebar" ~condition:"(min-width: 400px)"
      [ rule ]
  in
  Alcotest.(check (option string))
    "container name" (Some "sidebar") container.container_name;
  Alcotest.(check string)
    "container condition" "(min-width: 400px)" container.container_condition;
  Alcotest.(check int)
    "container rules count" 1
    (List.length container.container_rules)

let test_supports_rule_creation () =
  let decl = display Grid in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "grid") [ decl ] in
  let supports = Stylesheet.supports ~condition:"(display: grid)" [ rule ] in
  Alcotest.(check string)
    "supports condition" "(display: grid)" supports.supports_condition;
  match supports.supports_content with
  | Support_rules rules ->
      Alcotest.(check int) "supports rules count" 1 (List.length rules)
  | Support_nested _ -> Alcotest.fail "Expected Support_rules"

let test_supports_nested_creation () =
  let decl = display Grid in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "grid") [ decl ] in
  let nested_supports =
    Stylesheet.supports ~condition:"(color: red)" [ rule ]
  in
  let supports =
    Stylesheet.supports_nested ~condition:"(display: grid)" [ rule ]
      [ nested_supports ]
  in
  Alcotest.(check string)
    "supports condition" "(display: grid)" supports.supports_condition;
  match supports.supports_content with
  | Support_nested (rules, nested) ->
      Alcotest.(check int) "supports rules count" 1 (List.length rules);
      Alcotest.(check int) "nested supports count" 1 (List.length nested)
  | Support_rules _ -> Alcotest.fail "Expected Support_nested"

let test_property_rule_creation () =
  let prop =
    Stylesheet.property ~syntax:"<color>" ~initial_value:"red" ~inherits:true
      "--my-color"
  in
  Alcotest.(check string)
    "property name" "--my-color"
    (Stylesheet.property_rule_name prop);
  Alcotest.(check (option string))
    "property initial" (Some "red")
    (Stylesheet.property_rule_initial prop);
  Alcotest.(check string) "property syntax" "<color>" prop.syntax;
  Alcotest.(check bool) "property inherits" true prop.inherits

let test_layer_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let nested_rule = Stylesheet.rule_to_nested rule in
  let layer = Stylesheet.layer ~name:"utilities" [ nested_rule ] in
  Alcotest.(check string) "layer name" "utilities" (Stylesheet.layer_name layer);
  Alcotest.(check int)
    "layer rules count" 1
    (List.length (Stylesheet.layer_rules layer))

let test_empty_stylesheet () =
  let empty = Stylesheet.empty in
  Alcotest.(check int)
    "empty layers" 0
    (List.length (Stylesheet.stylesheet_layers empty));
  Alcotest.(check int)
    "empty rules" 0
    (List.length (Stylesheet.stylesheet_rules empty));
  Alcotest.(check int)
    "empty media" 0
    (List.length (Stylesheet.stylesheet_media_queries empty));
  Alcotest.(check int)
    "empty container" 0
    (List.length (Stylesheet.stylesheet_container_queries empty))

let test_stylesheet_construction () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let media = Stylesheet.media ~condition:"screen" [ rule ] in
  let prop = Stylesheet.property ~syntax:"<color>" "--my-color" in

  let sheet =
    Stylesheet.stylesheet
      [ Stylesheet.Rule rule; Stylesheet.Media media; Stylesheet.Property prop ]
  in

  Alcotest.(check int)
    "sheet rules count" 1
    (List.length (Stylesheet.stylesheet_rules sheet));
  Alcotest.(check int)
    "sheet media count" 1
    (List.length (Stylesheet.stylesheet_media_queries sheet));
  Alcotest.(check int)
    "sheet properties count" 1
    (List.length sheet.at_properties)

let test_stylesheet_items_conversion () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl ] in
  let media = Stylesheet.media ~condition:"screen" [ rule ] in

  let sheet =
    Stylesheet.stylesheet [ Stylesheet.Rule rule; Stylesheet.Media media ]
  in

  let items = Stylesheet.stylesheet_items sheet in
  Alcotest.(check int) "items count" 2 (List.length items);

  (* Check we can round-trip *)
  let reconstructed = Stylesheet.stylesheet items in
  Alcotest.(check int)
    "reconstructed rules" 1
    (List.length (Stylesheet.stylesheet_rules reconstructed));
  Alcotest.(check int)
    "reconstructed media" 1
    (List.length (Stylesheet.stylesheet_media_queries reconstructed))

let test_concat_stylesheets () =
  let decl1 = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule1 = Stylesheet.rule ~selector:(Selector.class_ "red") [ decl1 ] in
  let sheet1 = Stylesheet.stylesheet [ Stylesheet.Rule rule1 ] in

  let decl2 = color (Hex { hash = true; value = "0000ff" }) in
  let rule2 = Stylesheet.rule ~selector:(Selector.class_ "blue") [ decl2 ] in
  let sheet2 = Stylesheet.stylesheet [ Stylesheet.Rule rule2 ] in

  let combined = Stylesheet.concat [ sheet1; sheet2 ] in
  Alcotest.(check int)
    "combined rules count" 2
    (List.length (Stylesheet.stylesheet_rules combined))

let test_default_decl_of_property_rule () =
  let prop_with_initial =
    Stylesheet.property ~syntax:"<color>" ~initial_value:"red" "--my-color"
  in
  let decl_with_initial =
    Stylesheet.default_decl_of_property_rule prop_with_initial
  in

  let prop_no_initial = Stylesheet.property ~syntax:"<color>" "--other-color" in
  let decl_no_initial =
    Stylesheet.default_decl_of_property_rule prop_no_initial
  in

  (* Both should create custom_property declarations - just verify they don't
     crash *)
  match (decl_with_initial, decl_no_initial) with
  | Custom_declaration _, Custom_declaration _ -> ()
  | _ -> Alcotest.fail "Expected custom declarations"

let stylesheet_tests =
  [
    ("rule creation", `Quick, test_rule_creation);
    ("media rule creation", `Quick, test_media_rule_creation);
    ("container rule creation", `Quick, test_container_rule_creation);
    ("supports rule creation", `Quick, test_supports_rule_creation);
    ("supports nested creation", `Quick, test_supports_nested_creation);
    ("property rule creation", `Quick, test_property_rule_creation);
    ("layer rule creation", `Quick, test_layer_rule_creation);
    ("empty stylesheet", `Quick, test_empty_stylesheet);
    ("stylesheet construction", `Quick, test_stylesheet_construction);
    ("stylesheet items conversion", `Quick, test_stylesheet_items_conversion);
    ("concat stylesheets", `Quick, test_concat_stylesheets);
    ("default decl of property rule", `Quick, test_default_decl_of_property_rule);
  ]

let suite = [ ("stylesheet", stylesheet_tests) ]
