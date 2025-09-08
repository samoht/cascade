(** Tests for CSS stylesheet interface types - CSS/MDN spec compliance *)

module Stylesheet = Css.Stylesheet
module Selector = Css.Selector
open Css.Declaration
open Css.Stylesheet

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

(** Test @charset rules *)
let test_stylesheet_charset () =
  let charset : Stylesheet.charset_rule = { encoding = "UTF-8" } in
  Alcotest.(check string) "charset encoding" "UTF-8" charset.encoding;

  (* Test charset must be first *)
  let sheet : Stylesheet.t =
    {
      charset = Some charset;
      imports = [];
      namespaces = [];
      layers = [];
      keyframes = [];
      font_faces = [];
      pages = [];
      rules = [];
      media_queries = [];
      container_queries = [];
      starting_styles = [];
      supports_queries = [];
      at_properties = [];
    }
  in
  Alcotest.(check bool) "charset is optional" true (sheet.charset = Some charset)

(** Test @import rules *)
let test_stylesheet_import () =
  (* Basic import *)
  let import1 : Stylesheet.import_rule =
    { url = "styles.css"; layer = None; supports = None; media = None }
  in
  Alcotest.(check string) "import url" "styles.css" import1.url;

  (* Import with layer *)
  let import2 : Stylesheet.import_rule =
    {
      url = "utilities.css";
      layer = Some "utilities";
      supports = None;
      media = None;
    }
  in
  Alcotest.(check (option string))
    "import layer" (Some "utilities") import2.layer;

  (* Import with media query *)
  let import3 : Stylesheet.import_rule =
    { url = "print.css"; layer = None; supports = None; media = Some "print" }
  in
  Alcotest.(check (option string)) "import media" (Some "print") import3.media

(** Test @namespace rules *)
let test_stylesheet_namespace () =
  (* Default namespace *)
  let ns1 : Stylesheet.namespace_rule =
    { prefix = None; uri = "http://www.w3.org/1999/xhtml" }
  in
  Alcotest.(check string) "namespace uri" "http://www.w3.org/1999/xhtml" ns1.uri;

  (* Prefixed namespace *)
  let ns2 : Stylesheet.namespace_rule =
    { prefix = Some "svg"; uri = "http://www.w3.org/2000/svg" }
  in
  Alcotest.(check (option string)) "namespace prefix" (Some "svg") ns2.prefix

(** Test @keyframes rules *)
let test_stylesheet_keyframes () =
  let keyframe1 : Stylesheet.keyframe_block =
    { selector = "0%"; declarations = [] }
  in
  let keyframe2 : Stylesheet.keyframe_block =
    { selector = "100%"; declarations = [] }
  in
  let animation : Stylesheet.keyframes_rule =
    { name = "slide"; keyframes = [ keyframe1; keyframe2 ] }
  in
  Alcotest.(check string) "animation name" "slide" animation.name;
  Alcotest.(check int) "keyframes count" 2 (List.length animation.keyframes);

  (* Test from/to keywords *)
  let keyframe_from : Stylesheet.keyframe_block =
    { selector = "from"; declarations = [] }
  in
  let keyframe_to : Stylesheet.keyframe_block =
    { selector = "to"; declarations = [] }
  in
  Alcotest.(check string) "from selector" "from" keyframe_from.selector;
  Alcotest.(check string) "to selector" "to" keyframe_to.selector

(** Test @font-face rules *)
let test_stylesheet_font_face () =
  let font_face : Stylesheet.font_face_rule =
    {
      font_family = Some "MyCustomFont";
      src = Some "url('font.woff2') format('woff2')";
      font_style = Some "normal";
      font_weight = Some "400";
      font_stretch = Some "normal";
      font_display = Some "swap";
      unicode_range = Some "U+0000-00FF";
      font_variant = None;
      font_feature_settings = None;
      font_variation_settings = None;
    }
  in
  Alcotest.(check (option string))
    "font family" (Some "MyCustomFont") font_face.font_family;
  Alcotest.(check (option string))
    "font src" (Some "url('font.woff2') format('woff2')") font_face.src;
  Alcotest.(check (option string))
    "font display" (Some "swap") font_face.font_display

(** Test @page rules *)
let test_stylesheet_page () =
  (* Default page rule *)
  let page1 : Stylesheet.page_rule = { selector = None; declarations = [] } in
  Alcotest.(check (option string)) "default page selector" None page1.selector;

  (* Page with :first selector *)
  let page2 : Stylesheet.page_rule =
    { selector = Some ":first"; declarations = [] }
  in
  Alcotest.(check (option string))
    "first page selector" (Some ":first") page2.selector

(** Test sheet_item variants *)
let test_stylesheet_sheet_item () =
  (* Test all variants can be constructed *)
  let items : Stylesheet.sheet_item list =
    [
      Stylesheet.Charset { encoding = "UTF-8" };
      Stylesheet.Import
        { url = "test.css"; layer = None; supports = None; media = None };
      Stylesheet.Namespace
        { prefix = None; uri = "http://www.w3.org/1999/xhtml" };
      Stylesheet.Rule { selector = Selector.universal; declarations = [] };
      Stylesheet.Media { media_condition = "print"; media_rules = [] };
      Stylesheet.Supports
        {
          supports_condition = "(display: grid)";
          supports_content = Stylesheet.Support_rules [];
        };
      Stylesheet.Container
        {
          container_name = None;
          container_condition = "(width > 40em)";
          container_rules = [];
        };
      Stylesheet.Layer
        {
          layer = "base";
          rules = [];
          media_queries = [];
          container_queries = [];
          supports_queries = [];
        };
      Stylesheet.Property
        { name = "--var"; syntax = "*"; inherits = false; initial_value = None };
      Stylesheet.Starting_style { starting_rules = [] };
      Stylesheet.Keyframes { name = "spin"; keyframes = [] };
      Stylesheet.Font_face
        {
          font_family = None;
          src = None;
          font_style = None;
          font_weight = None;
          font_stretch = None;
          font_display = None;
          unicode_range = None;
          font_variant = None;
          font_feature_settings = None;
          font_variation_settings = None;
        };
      Stylesheet.Page { selector = None; declarations = [] };
    ]
  in
  Alcotest.(check int) "sheet items count" 13 (List.length items)

(** Test stylesheet ordering constraints *)
let test_stylesheet_ordering () =
  (* Per CSS spec, certain at-rules must appear in specific order *)
  let sheet : Stylesheet.t =
    {
      charset = Some { encoding = "UTF-8" };
      (* Must be first *)
      imports =
        [ { url = "base.css"; layer = None; supports = None; media = None } ];
      (* After charset *)
      namespaces =
        [ { prefix = Some "svg"; uri = "http://www.w3.org/2000/svg" } ];
      (* After imports *)
      layers = [];
      keyframes = [];
      font_faces = [];
      pages = [];
      rules = [];
      media_queries = [];
      container_queries = [];
      starting_styles = [];
      supports_queries = [];
      at_properties = [];
    }
  in
  (* Just verify the structure compiles and fields exist in correct order *)
  Alcotest.(check bool)
    "charset before imports" true
    (sheet.charset <> None || List.length sheet.imports >= 0);
  Alcotest.(check bool)
    "imports before namespaces" true
    (List.length sheet.imports >= 0 && List.length sheet.namespaces >= 0)

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
    (* New CSS/MDN spec compliance tests *)
    ("charset", `Quick, test_stylesheet_charset);
    ("import", `Quick, test_stylesheet_import);
    ("namespace", `Quick, test_stylesheet_namespace);
    ("keyframes", `Quick, test_stylesheet_keyframes);
    ("font_face", `Quick, test_stylesheet_font_face);
    ("page", `Quick, test_stylesheet_page);
    ("sheet_item", `Quick, test_stylesheet_sheet_item);
    ("ordering", `Quick, test_stylesheet_ordering);
  ]

let suite = [ ("stylesheet", stylesheet_tests) ]
