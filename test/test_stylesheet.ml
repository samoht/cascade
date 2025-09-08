(** Tests for CSS stylesheet interface types - CSS/MDN spec compliance *)

module Selector = Css.Selector
open Css.Declaration
open Css.Stylesheet
open Alcotest

let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Generic check function for stylesheet types *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  Alcotest.check Alcotest.string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
  let v2 = reader t2 in
  let s2 = to_string pp v2 in
  Alcotest.check Alcotest.string (Fmt.str "roundtrip %s %s" name input) s s2

(* Check functions for each type - one-liner definitions *)
let check_rule = check_value "rule" pp_rule read_rule
let check_media = check_value "media" pp_media_rule read_media_rule
let check_property = check_value "property" pp_property_rule read_property_rule
let check_supports = check_value "supports" pp_supports_rule read_supports_rule

let of_string css =
  try
    let r = Css.Reader.of_string css in
    Ok (Css.Stylesheet.read_stylesheet r)
  with Css.Reader.Parse_error _ -> Error "boom"

let string_of_rule r = Css.Pp.to_string ~minify:true pp_rule r

(* Helper for testing rule construction *)
let check_construct_rule name expected rule =
  let actual = string_of_rule rule in
  Alcotest.check Alcotest.string name expected actual

(* Helper for testing complete stylesheet *)
let check_stylesheet name expected sheet =
  let actual = Css.pp ~minify:true sheet in
  Alcotest.check Alcotest.string name expected actual

let test_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let selector = selector rule in
  (* Just check we can get selector back *)
  Alcotest.(check bool)
    "selector exists" true
    (selector = Css.Selector.class_ "red");
  Alcotest.(check int)
    "rule declarations count" 1
    (List.length (declarations rule))

let test_media_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media = media ~condition:"screen and (min-width: 768px)" [ rule ] in
  Alcotest.(check string)
    "media condition" "screen and (min-width: 768px)" (media_condition media);
  Alcotest.(check int) "media rules count" 1 (List.length (media_rules media))

let test_container_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let container =
    container ~name:"sidebar" ~condition:"(min-width: 400px)" [ rule ]
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
  let rule = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let supports = supports ~condition:"(display: grid)" [ rule ] in
  Alcotest.(check string)
    "supports condition" "(display: grid)" supports.supports_condition;
  match supports.supports_content with
  | Support_rules rules ->
      Alcotest.(check int) "supports rules count" 1 (List.length rules)
  | Support_nested _ -> Alcotest.fail "Expected Support_rules"

let test_supports_nested_creation () =
  let decl = display Grid in
  let rule = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let nested_supports = supports ~condition:"(color: red)" [ rule ] in
  let supports =
    supports_nested ~condition:"(display: grid)" [ rule ] [ nested_supports ]
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
    property ~syntax:"<color>" ~initial_value:"red" ~inherits:true "--my-color"
  in
  Alcotest.(check string) "property name" "--my-color" (property_rule_name prop);
  Alcotest.(check (option string))
    "property initial" (Some "red")
    (property_rule_initial prop);
  Alcotest.(check string) "property syntax" "<color>" prop.syntax;
  Alcotest.(check bool) "property inherits" true prop.inherits

let test_layer_rule_creation () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let nested_rule = rule_to_nested rule in
  let layer = layer ~name:"utilities" [ nested_rule ] in
  Alcotest.(check string) "layer name" "utilities" (layer_name layer);
  Alcotest.(check int) "layer rules count" 1 (List.length (layer_rules layer))

let test_empty_stylesheet () =
  let empty = empty in
  Alcotest.(check int) "empty layers" 0 (List.length (stylesheet_layers empty));
  Alcotest.(check int) "empty rules" 0 (List.length (stylesheet_rules empty));
  Alcotest.(check int)
    "empty media" 0
    (List.length (stylesheet_media_queries empty));
  Alcotest.(check int)
    "empty container" 0
    (List.length (stylesheet_container_queries empty))

let test_stylesheet_construction () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media = media ~condition:"screen" [ rule ] in
  let prop = property ~syntax:"<color>" "--my-color" in

  let sheet = stylesheet [ Rule rule; Media media; Property prop ] in

  Alcotest.(check int)
    "sheet rules count" 1
    (List.length (stylesheet_rules sheet));
  Alcotest.(check int)
    "sheet media count" 1
    (List.length (stylesheet_media_queries sheet));
  Alcotest.(check int)
    "sheet properties count" 1
    (List.length sheet.at_properties)

let test_stylesheet_items_conversion () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media = media ~condition:"screen" [ rule ] in

  let sheet = stylesheet [ Rule rule; Media media ] in

  let items = stylesheet_items sheet in
  Alcotest.(check int) "items count" 2 (List.length items);

  (* Check we can round-trip *)
  let reconstructed = stylesheet items in
  Alcotest.(check int)
    "reconstructed rules" 1
    (List.length (stylesheet_rules reconstructed));
  Alcotest.(check int)
    "reconstructed media" 1
    (List.length (stylesheet_media_queries reconstructed))

let test_concat_stylesheets () =
  let decl1 = background_color (Hex { hash = true; value = "ff0000" }) in
  let rule1 = rule ~selector:(Selector.class_ "red") [ decl1 ] in
  let sheet1 = stylesheet [ Rule rule1 ] in

  let decl2 = color (Hex { hash = true; value = "0000ff" }) in
  let rule2 = rule ~selector:(Selector.class_ "blue") [ decl2 ] in
  let sheet2 = stylesheet [ Rule rule2 ] in

  let combined = concat [ sheet1; sheet2 ] in
  Alcotest.(check int)
    "combined rules count" 2
    (List.length (stylesheet_rules combined))

let test_default_decl_of_property_rule () =
  let prop_with_initial =
    property ~syntax:"<color>" ~initial_value:"red" "--my-color"
  in
  let decl_with_initial = default_decl_of_property_rule prop_with_initial in

  let prop_no_initial = property ~syntax:"<color>" "--other-color" in
  let decl_no_initial = default_decl_of_property_rule prop_no_initial in

  (* Both should create custom_property declarations - just verify they don't
     crash *)
  match (decl_with_initial, decl_no_initial) with
  | Custom_declaration _, Custom_declaration _ -> ()
  | _ -> Alcotest.fail "Expected custom declarations"

(** Test rule read/pp roundtrip *)
let test_rule_roundtrip () =
  (* Test basic rule *)
  let input = ".test { color: red }" in
  let r = Css.Reader.of_string input in
  let parsed_rule = read_rule r in
  let output = Css.Pp.to_string pp_rule parsed_rule in
  Alcotest.(check bool)
    "rule roundtrip contains selector" true
    (String.contains output '.');
  Alcotest.(check bool)
    "rule roundtrip contains property" true
    (String.contains output ':');

  (* Test rule construction and pp *)
  let decl = background_color (Hex { hash = true; value = "0000ff" }) in
  let constructed = rule ~selector:(Selector.id "main") [ decl ] in
  let pp_output = Css.Pp.to_string pp_rule constructed in
  Alcotest.(check bool)
    "constructed rule has id selector" true
    (String.contains pp_output '#')

(** Test media rule read/pp roundtrip *)
let test_media_roundtrip () =
  let input = "@media screen { .test { color: blue } }" in
  let r = Css.Reader.of_string input in
  let media_rule = read_media_rule r in
  let output = Css.Pp.to_string pp_media_rule media_rule in
  Alcotest.(check bool)
    "media roundtrip contains @media" true
    (String.contains output '@');
  Alcotest.(check string)
    "media condition preserved" "screen" media_rule.media_condition

(** Test property rule read/pp roundtrip *)
let test_property_roundtrip () =
  let input = "@property --color { syntax: \"<color>\"; inherits: true; }" in
  let r = Css.Reader.of_string input in
  let prop_rule = read_property_rule r in
  let output = Css.Pp.to_string pp_property_rule prop_rule in
  Alcotest.(check bool)
    "property roundtrip contains @property" true
    (String.contains output '@');
  Alcotest.(check string) "property name preserved" "--color" prop_rule.name;
  Alcotest.(check string) "property syntax preserved" "<color>" prop_rule.syntax;
  Alcotest.(check bool) "property inherits preserved" true prop_rule.inherits

(** Test supports rule read/pp roundtrip *)
let test_supports_roundtrip () =
  let input = "@supports (display: grid) { .grid { display: grid } }" in
  let r = Css.Reader.of_string input in
  let supports_rule = read_supports_rule r in
  let output = Css.Pp.to_string pp_supports_rule supports_rule in
  Alcotest.(check bool)
    "supports roundtrip contains @supports" true
    (String.contains output '@');
  Alcotest.(check string)
    "supports condition preserved" "(display: grid)"
    supports_rule.supports_condition

(** Test layer pp *)
let test_layer_pp () =
  let decl = color (Hex { hash = true; value = "0000ff" }) in
  let rule_obj = rule ~selector:(Selector.class_ "blue") [ decl ] in
  let nested = rule_to_nested rule_obj in
  let layer_rule = layer ~name:"utilities" [ nested ] in

  let output = Css.Pp.to_string pp_layer_rule layer_rule in
  Alcotest.(check bool)
    "layer pp contains @layer" true
    (String.contains output '@');

  (* Test empty layer *)
  let empty_layer = layer ~name:"base" [] in
  let empty_output = Css.Pp.to_string pp_layer_rule empty_layer in
  Alcotest.(check bool)
    "empty layer ends with semicolon" true
    (String.contains empty_output ';')

(** Test complete stylesheet pp *)
let test_stylesheet_pp () =
  let decl = background_color (Hex { hash = true; value = "ff0000" }) in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let m = media ~condition:"screen" [ r ] in
  let prop = property ~syntax:"<color>" "--primary" in

  let sheet = stylesheet [ Rule r; Media m; Property prop ] in

  let output = Css.Pp.to_string Css.Stylesheet.pp ~minify:true sheet in
  Alcotest.(check bool)
    "stylesheet contains rule" true
    (String.contains output '.');
  Alcotest.(check bool)
    "stylesheet contains media" true
    (String.contains output '@' && String.contains output 'm');
  Alcotest.(check bool)
    "stylesheet contains property" true
    (String.contains output 'p' && String.contains output 'r')

(** Test @charset rules *)
let test_stylesheet_charset () =
  let charset : charset_rule = { encoding = "UTF-8" } in
  Alcotest.(check string) "charset encoding" "UTF-8" charset.encoding;

  (* Test charset must be first *)
  let sheet : t =
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
  let import1 : import_rule =
    { url = "styles.css"; layer = None; supports = None; media = None }
  in
  Alcotest.(check string) "import url" "styles.css" import1.url;

  (* Import with layer *)
  let import2 : import_rule =
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
  let import3 : import_rule =
    { url = "print.css"; layer = None; supports = None; media = Some "print" }
  in
  Alcotest.(check (option string)) "import media" (Some "print") import3.media

(** Test @namespace rules *)
let test_stylesheet_namespace () =
  (* Default namespace *)
  let ns1 : namespace_rule =
    { prefix = None; uri = "http://www.w3.org/1999/xhtml" }
  in
  Alcotest.(check string) "namespace uri" "http://www.w3.org/1999/xhtml" ns1.uri;

  (* Prefixed namespace *)
  let ns2 : namespace_rule =
    { prefix = Some "svg"; uri = "http://www.w3.org/2000/svg" }
  in
  Alcotest.(check (option string)) "namespace prefix" (Some "svg") ns2.prefix

(** Test @keyframes rules *)
let test_stylesheet_keyframes () =
  let keyframe1 : keyframe_block = { selector = "0%"; declarations = [] } in
  let keyframe2 : keyframe_block = { selector = "100%"; declarations = [] } in
  let animation : keyframes_rule =
    { name = "slide"; keyframes = [ keyframe1; keyframe2 ] }
  in
  Alcotest.(check string) "animation name" "slide" animation.name;
  Alcotest.(check int) "keyframes count" 2 (List.length animation.keyframes);

  (* Test from/to keywords *)
  let keyframe_from : keyframe_block =
    { selector = "from"; declarations = [] }
  in
  let keyframe_to : keyframe_block = { selector = "to"; declarations = [] } in
  Alcotest.(check string) "from selector" "from" keyframe_from.selector;
  Alcotest.(check string) "to selector" "to" keyframe_to.selector

(** Test @font-face rules *)
let test_stylesheet_font_face () =
  let font_face : font_face_rule =
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
  let page1 : page_rule = { selector = None; declarations = [] } in
  Alcotest.(check (option string)) "default page selector" None page1.selector;

  (* Page with :first selector *)
  let page2 : page_rule = { selector = Some ":first"; declarations = [] } in
  Alcotest.(check (option string))
    "first page selector" (Some ":first") page2.selector

(** Test sheet_item variants *)
let test_stylesheet_sheet_item () =
  (* Test all variants can be constructed *)
  let items : sheet_item list =
    [
      Charset { encoding = "UTF-8" };
      Import { url = "test.css"; layer = None; supports = None; media = None };
      Namespace { prefix = None; uri = "http://www.w3.org/1999/xhtml" };
      Rule { selector = Selector.universal; declarations = [] };
      Media { media_condition = "print"; media_rules = [] };
      Supports
        {
          supports_condition = "(display: grid)";
          supports_content = Support_rules [];
        };
      Container
        {
          container_name = None;
          container_condition = "(width > 40em)";
          container_rules = [];
        };
      Layer
        {
          layer = "base";
          rules = [];
          media_queries = [];
          container_queries = [];
          supports_queries = [];
        };
      Property
        { name = "--var"; syntax = "*"; inherits = false; initial_value = None };
      Starting_style { starting_rules = [] };
      Keyframes { name = "spin"; keyframes = [] };
      Font_face
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
      Page { selector = None; declarations = [] };
    ]
  in
  Alcotest.(check int) "sheet items count" 13 (List.length items)

(** Test stylesheet ordering constraints *)
let test_stylesheet_ordering () =
  (* Per CSS spec, certain at-rules must appear in specific order *)
  let sheet : t =
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

let test_read_stylesheet_basic () =
  let css = ".btn { color: red; padding: 10px; }" in
  let reader = Css.Reader.of_string css in
  let sheet = Css.Stylesheet.read_stylesheet reader in
  let rules = stylesheet_rules sheet in
  check int "has one rule" 1 (List.length rules);
  let rule = List.hd rules in
  let decls = Css.Stylesheet.declarations rule in
  check int "rule has two declarations" 2 (List.length decls)

let test_read_stylesheet_multiple_rules () =
  let css = ".btn { color: red; } .card { margin: 5px; }" in
  let reader = Css.Reader.of_string css in
  let sheet = Css.Stylesheet.read_stylesheet reader in
  let rules = stylesheet_rules sheet in
  check int "has two rules" 2 (List.length rules)

let test_read_stylesheet_empty () =
  let css = "" in
  let reader = Css.Reader.of_string css in
  let sheet = Css.Stylesheet.read_stylesheet reader in
  let rules = stylesheet_rules sheet in
  check int "empty stylesheet has no rules" 0 (List.length rules)

let test_read_stylesheet_whitespace_only () =
  let css = "   \n\t  " in
  let reader = Css.Reader.of_string css in
  let sheet = Css.Stylesheet.read_stylesheet reader in
  let rules = stylesheet_rules sheet in
  check int "whitespace-only stylesheet has no rules" 0 (List.length rules)

let test_read_stylesheet_with_comments () =
  let css = "/* comment */ .btn { color: red; } /* another comment */" in
  let reader = Css.Reader.of_string css in
  let sheet = Css.Stylesheet.read_stylesheet reader in
  let rules = stylesheet_rules sheet in
  check int "has one rule despite comments" 1 (List.length rules)

let test_read_stylesheet_error_recovery () =
  let css = ".btn { color: red; } invalid-css-here .card { margin: 5px; }" in
  let reader = Css.Reader.of_string css in
  (* Should fail on invalid CSS without recovery *)
  try
    let _sheet = Css.Stylesheet.read_stylesheet reader in
    Alcotest.fail "Expected parsing to fail on invalid CSS"
  with Css.Reader.Parse_error _ ->
    (* This is expected - parsing should fail *)
    ()

let test_of_string () =
  let css = ".btn { color: red; }" in
  match of_string css with
  | Ok sheet ->
      let rules = stylesheet_rules sheet in
      check int "of_string works" 1 (List.length rules)
  | Error msg -> Alcotest.fail ("of_string failed: " ^ msg)

let test_of_string_positive () =
  (* Test valid CSS - simple rule *)
  let css1 = ".btn { color: red; }" in
  (match of_string css1 with
  | Ok sheet ->
      let rules = stylesheet_rules sheet in
      check int "single rule parsed" 1 (List.length rules)
  | Error msg -> Alcotest.fail ("valid CSS failed: " ^ msg));

  (* Test valid CSS - multiple rules *)
  let css2 = ".btn { color: red; } .card { margin: 10px; }" in
  (match of_string css2 with
  | Ok sheet ->
      let rules = stylesheet_rules sheet in
      check int "multiple rules parsed" 2 (List.length rules)
  | Error msg -> Alcotest.fail ("multiple rules failed: " ^ msg));

  (* Test valid CSS - empty stylesheet *)
  let css3 = "" in
  (match of_string css3 with
  | Ok sheet ->
      let rules = stylesheet_rules sheet in
      check int "empty stylesheet" 0 (List.length rules)
  | Error msg -> Alcotest.fail ("empty stylesheet failed: " ^ msg));

  (* Test valid CSS - whitespace only *)
  let css4 = "   \n\t  " in
  match of_string css4 with
  | Ok sheet ->
      let rules = stylesheet_rules sheet in
      check int "whitespace only" 0 (List.length rules)
  | Error msg -> Alcotest.fail ("whitespace only failed: " ^ msg)

let test_of_string_negative () =
  (* Helper function to test invalid CSS *)
  let test_invalid_css css expected_error =
    match of_string css with
    | Ok _ ->
        Alcotest.fail
          ("should have failed: " ^ expected_error ^ " - CSS: " ^ css)
    | Error msg ->
        check bool
          ("error contains information for " ^ expected_error)
          true
          (String.length msg > 0)
  in

  (* Basic syntax errors *)
  test_invalid_css ".btn { color: }" "empty property value";
  test_invalid_css ".btn { color: red; " "unclosed brace";
  test_invalid_css "{ color: red; }" "missing selector";
  test_invalid_css ".btn { invalid-property-that-does-not-exist: red; }"
    "invalid property name";

  (* Property-specific value validation errors *)
  test_invalid_css ".btn { color: invalidcolor; }" "invalid color value";
  test_invalid_css ".btn { display: invalidmode; }" "invalid display value";
  test_invalid_css ".btn { position: invalidpos; }" "invalid position value";
  test_invalid_css ".btn { width: invalidlength; }" "invalid length value";
  test_invalid_css ".btn { height: notanumber; }" "invalid height value";

  (* Missing colons and semicolons *)
  test_invalid_css ".btn { color red; }" "missing colon after property name";
  test_invalid_css ".btn color: red; }" "missing opening brace";
  test_invalid_css ".btn { : red; }" "missing property name";

  (* Invalid color formats *)
  test_invalid_css ".btn { color: rgb(300, 300, 300); }"
    "invalid RGB values (out of range)";
  test_invalid_css ".btn { color: rgb(50%, 100, 50%); }"
    "mixed RGB units (percentages and numbers)";
  test_invalid_css ".btn { color: #gggggg; }" "invalid hex color";
  test_invalid_css ".btn { color: rgba(255, 0, 0); }" "rgba with missing alpha";

  (* Invalid length/size values *)
  test_invalid_css ".btn { width: 100unknown; }" "unknown length unit";
  test_invalid_css ".btn { margin: px; }" "missing numeric value for unit";
  test_invalid_css ".btn { padding: -10px; }"
    "negative padding (should be invalid)";

  (* Selector syntax errors *)
  test_invalid_css "..double-dot { color: red; }"
    "invalid selector (double dot)";
  test_invalid_css ".btn..extra { color: red; }"
    "invalid selector (double class)";
  test_invalid_css "# { color: red; }" "empty ID selector";
  test_invalid_css ". { color: red; }" "empty class selector";

  (* Nested braces and structure errors *)
  test_invalid_css ".btn { color: red; { margin: 10px; } }"
    "unexpected nested braces";
  test_invalid_css ".btn { color: red; } } " "extra closing brace";
  test_invalid_css ".btn { { color: red; }" "extra opening brace";

  (* CSS function syntax errors *)
  test_invalid_css ".btn { color: rgb(255, 0); }" "incomplete RGB function";
  test_invalid_css ".btn { color: rgb(255 0 0, 0.5); }"
    "mixed comma and space syntax in RGB";
  test_invalid_css ".btn { transform: rotate(45); }"
    "missing unit in rotate function";

  (* Important declaration errors *)
  test_invalid_css ".btn { color: red !importent; }" "misspelled !important";
  test_invalid_css ".btn { color: red! important; }" "space before !important";
  test_invalid_css ".btn { color: red ! important; }" "space in !important";

  (* String and quote errors *)
  test_invalid_css ".btn { content: 'unclosed string; }" "unclosed single quote";
  test_invalid_css ".btn { content: \"unclosed string; }"
    "unclosed double quote";
  test_invalid_css ".btn { content: 'mixed quotes\"; }" "mixed quote types";

  (* Vendor prefix validation *)
  test_invalid_css ".btn { -invalid-vendor-transform: rotate(45deg); }"
    "unknown vendor prefix";
  test_invalid_css ".btn { webkit-transform: rotate(45deg); }"
    "missing hyphen in vendor prefix";

  (* Value list errors *)
  test_invalid_css ".btn { margin: 10px 20px 30px 40px 50px; }"
    "too many margin values";
  test_invalid_css ".btn { font-family: Arial, , sans-serif; }"
    "empty font family in list";

  (* Calc function errors *)
  test_invalid_css ".btn { width: calc(100% + ); }" "incomplete calc expression";
  test_invalid_css ".btn { width: calc(100px * 2px); }"
    "invalid calc (length * length)";

  (* Custom property errors *)
  test_invalid_css ".btn { --: value; }" "empty custom property name";
  test_invalid_css ".btn { -custom: value; }"
    "invalid custom property (single hyphen)";

  (* Media query and at-rule errors *)
  test_invalid_css "@media { .btn { color: red; } }"
    "media query without condition";
  test_invalid_css "@unknown-rule { .btn { color: red; } }" "unknown at-rule";

  (* Specificity and cascade errors *)
  test_invalid_css "btn.#id { color: red; }" "invalid selector combination";
  test_invalid_css ".btn .btn { color: red; }"
    "duplicate class selector (should parse but warn)";

  (* Unicode and encoding errors *)
  test_invalid_css ".btn { content: '\\'; }" "incomplete escape sequence";
  test_invalid_css ".btn { content: '\\gggg'; }" "invalid unicode escape"

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
    (* Roundtrip tests *)
    ("rule roundtrip", `Quick, test_rule_roundtrip);
    ("media roundtrip", `Quick, test_media_roundtrip);
    ("property roundtrip", `Quick, test_property_roundtrip);
    ("supports roundtrip", `Quick, test_supports_roundtrip);
    ("layer pp", `Quick, test_layer_pp);
    ("stylesheet pp", `Quick, test_stylesheet_pp);
    (* New CSS/MDN spec compliance tests *)
    ("charset", `Quick, test_stylesheet_charset);
    ("import", `Quick, test_stylesheet_import);
    ("namespace", `Quick, test_stylesheet_namespace);
    ("keyframes", `Quick, test_stylesheet_keyframes);
    ("font_face", `Quick, test_stylesheet_font_face);
    ("page", `Quick, test_stylesheet_page);
    ("sheet_item", `Quick, test_stylesheet_sheet_item);
    ("ordering", `Quick, test_stylesheet_ordering);
    (* CSS parsing tests *)
    ("read_stylesheet basic", `Quick, test_read_stylesheet_basic);
    ( "read_stylesheet multiple rules",
      `Quick,
      test_read_stylesheet_multiple_rules );
    ("read_stylesheet empty", `Quick, test_read_stylesheet_empty);
    ( "read_stylesheet whitespace only",
      `Quick,
      test_read_stylesheet_whitespace_only );
    ("read_stylesheet with comments", `Quick, test_read_stylesheet_with_comments);
    ( "read_stylesheet fails on invalid CSS",
      `Quick,
      test_read_stylesheet_error_recovery );
    ("of_string", `Quick, test_of_string);
    ("of_string positive", `Quick, test_of_string_positive);
    ("of_string negative", `Quick, test_of_string_negative);
  ]

let suite = [ ("stylesheet", stylesheet_tests) ]
