(** Tests for CSS stylesheet interface types - CSS/MDN spec compliance *)

open Cascade
module Selector = Css.Selector
open Css.Stylesheet
open Css_test_helpers

let check_rule = check_value_cursor "rule" read_rule pp_rule

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d ->
      Format.pp_print_string fmt (Css.Declaration.string_of_declaration d))
    ( = )

let check_import_rule =
  check_value_cursor "import_rule" read_import_rule pp_import_rule

let check_declaration =
  check_value_cursor "declaration" Css.Declaration.read_declaration
    (Css.Pp.option Css.Declaration.pp_declaration)

let check_config = check_value_cursor "config" read_config pp_config

let check_stylesheet =
  check_value_cursor "stylesheet" read_stylesheet pp_stylesheet

(* Legacy alias *)
let check = check_stylesheet

(* Not a roundtrip test *)
let test_rule () =
  (* Basic rules *)
  check_rule ".btn{color:red}";
  check_rule "h1{font-size:2rem}";
  check_rule "#main{display:flex}";
  check_rule "div.container{margin:auto}";

  (* Multiple declarations *)
  check_rule ".card{padding:1rem;border:1px solid #ccc}";
  check_rule "body{margin:0;font-family:Arial,sans-serif}";

  (* Multiple selectors *)
  check_rule ".a,.b{display:block}";

  (* Universal selector *)
  check_rule ~expected:"*{box-sizing:border-box}" "* { box-sizing: border-box }";

  (* Test invalid rule syntax *)
  neg_cursor read_stylesheet "{color:red}";
  (* Missing selector *)
  neg_cursor read_stylesheet ".btn";
  (* Missing declarations. CSS Syntax §5.3.7 auto-closes [.btn{] so it is
     spec-valid and not asserted here. *)
  neg_cursor read_stylesheet ".btn{color}";
  (* Missing value *)
  neg_cursor read_rule "" (* Empty rule *)

let test_stylesheet () =
  (* Test basic stylesheet parsing *)
  check_stylesheet ".btn{color:red}";
  check_stylesheet "body{margin:0}.btn{color:blue}";

  (* Test stylesheet with at-rules *)
  check_stylesheet "@media screen{.btn{color:green}}";
  check_stylesheet "@layer base{body{margin:0}}";

  (* Test empty stylesheet *)
  check_stylesheet "";

  (* Test stylesheet with comments - comments are stripped in minified output *)
  check_stylesheet ~expected:".btn{color:red}" "/*comment*/.btn{color:red}";

  check_stylesheet ~expected:"@media (min-width:768px){.a{display:block}}"
    "@media (min-width: 768px) { .a { display: block } }";
  check_stylesheet
    ~expected:"@media screen and (max-width:640px){.btn{font-size:.875rem}}"
    "@media screen and (max-width: 640px){.btn{font-size:.875rem}}";
  check_stylesheet ~expected:"@media screen{.test{color:blue}}"
    "@media screen { .test { color: blue } }";
  check_stylesheet ~expected:"@supports (display:grid){.grid{display:grid}}"
    "@supports (display: grid) { .grid { display: grid } }";
  check_stylesheet
    "@supports (display:grid){.grid{display:grid}@supports \
     (color:red){.x{color:red}}}";
  check_stylesheet ~expected:"@supports (display:grid){.grid{display:grid}}"
    "@supports (display: grid) { .grid { display: grid } }";
  check_stylesheet
    ~expected:
      "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: \
     red }";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}100%{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet
    ~expected:"@font-face {font-family:MyFont;src:url(font.woff2)}"
    "@font-face { font-family: MyFont; src: url(font.woff2); }";
  check_stylesheet ~expected:"@page:first{margin:1in}"
    "@page :first { margin: 1in }";
  check_stylesheet ~expected:".test{color:red}" ".test { color: red }";

  (* Test invalid stylesheet syntax *)
  neg_cursor read_stylesheet "@media { }";
  (* Media without condition *)
  neg_cursor read_stylesheet "@charset 'UTF-8'" (* Wrong charset quotes *)

let of_string css =
  try
    let r = Css.Cursor.of_string css in
    Ok (read_stylesheet r)
    (* Internal API *)
  with Css.Cursor.Parse_error _ -> Error "boom"

let string_of_stylesheet s = Css.Stylesheet.pp ~minify:true ~newline:false s

(* Helper for testing rule construction *)
let check_construct_rule name expected rule =
  check_construct name (Css.Pp.to_string ~minify:true pp_rule) expected rule

(* Helper for testing complete stylesheet *)
let check_stylesheet_helper name expected sheet =
  check_construct name string_of_stylesheet expected sheet

(* Not a roundtrip test *)
let test_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let selector = selector rule in
  (* Just check we can get selector back *)
  Alcotest.(check bool)
    "selector exists" true
    (selector = Css.Selector.class_ "red");
  Alcotest.(check int)
    "rule declarations count" 1
    (List.length (declarations rule))

(* Not a roundtrip test *)
let test_media_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media
      ~condition:(Css.Media.of_string "screen and (min-width: 768px)")
      [ Rule r ]
  in
  let sheet = Css.Stylesheet.v [ media_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_container_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let container_stmt =
    container ~name:"sidebar"
      ~condition:(Css.Container.of_string "(min-width: 400px)")
      [ Rule r ]
  in
  let sheet = Css.Stylesheet.v [ container_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_supports_rule_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let supports_stmt =
    supports
      ~condition:(Css.Supports.property "display" "grid")
      [ Css.Stylesheet.Rule r ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_supports_nested_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let nested_supports =
    supports
      ~condition:(Css.Supports.property "color" "red")
      [ Css.Stylesheet.Rule r ]
  in
  let supports_stmt =
    supports
      ~condition:(Css.Supports.property "display" "grid")
      [ Rule r; nested_supports ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_property_rule_creation () =
  let prop : Css.Values.color property_rule =
    {
      name = "--my-color";
      syntax = Css.Variables.Color;
      initial_value = Some (Css.Values.Hex { hash = true; value = "ff0000" });
      inherits = true;
    }
  in
  Alcotest.(check string) "property name" "--my-color" prop.name;
  (* Property has typed syntax field *)
  Alcotest.(check bool) "property inherits" true prop.inherits

(* Not a roundtrip test *)
let test_layer_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let layer_stmt = layer ~name:"utilities" [ Css.Stylesheet.Rule rule ] in
  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  Alcotest.(check string)
    "layer rule creation" "@layer utilities{.red{background-color:#ff0000}}"
    output

(* Not a roundtrip test *)
let test_construct_rule_helper () =
  (* Test rule construction and string representation *)
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule1 = rule ~selector:(Selector.class_ "red") [ decl ] in
  check_construct_rule "simple rule" ".red{background-color:#ff0000}" rule1;

  let decls =
    [
      Css.Declaration.color (Css.Values.Hex { hash = true; value = "000000" });
      Css.Declaration.margin [ Css.Values.Px 10. ];
    ]
  in
  let rule2 = rule ~selector:(Selector.id "test") decls in
  check_construct_rule "multiple declarations"
    "#test{color:#000000;margin:10px}" rule2

(* Not a roundtrip test *)
let helper () =
  (* Test complete stylesheet construction and string representation *)
  let decl = Css.Declaration.display Css.Properties.Block in
  let rule = rule ~selector:(Selector.element "div") [ decl ] in
  let sheet = Css.Stylesheet.v [ Css.Stylesheet.Rule rule ] in
  check_stylesheet_helper "simple stylesheet" "div{display:block}" sheet;

  let media_stmt =
    media ~condition:(Css.Media.of_string "print") [ Css.Stylesheet.Rule rule ]
  in
  let sheet2 = Css.Stylesheet.v [ media_stmt ] in
  check_stylesheet_helper "media stylesheet" "@media print{div{display:block}}"
    sheet2

(* Not a roundtrip test *)
let test_empty_stylesheet () =
  let empty = empty_stylesheet in
  Alcotest.(check int) "empty layers" 0 (List.length (layers empty));
  Alcotest.(check int) "empty rules" 0 (List.length (rules empty));
  Alcotest.(check int) "empty media" 0 (List.length (media_queries empty));
  Alcotest.(check int)
    "empty container" 0
    (List.length (container_queries empty))

(* Not a roundtrip test *)
let construction () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:(Css.Media.of_string "screen") [ Css.Stylesheet.Rule rule ]
  in
  let prop = property ~syntax:Css.Variables.Color "--my-color" in

  let sheet = Css.Stylesheet.v [ Css.Stylesheet.Rule rule; media_stmt; prop ] in

  Alcotest.(check int) "sheet rules count" 1 (List.length (rules sheet));
  Alcotest.(check int) "sheet media count" 1 (List.length (media_queries sheet));
  let props_count =
    List.fold_left
      (fun acc -> function Property _ -> acc + 1 | _ -> acc)
      0 sheet
  in
  Alcotest.(check int) "sheet properties count" 1 props_count

let items_conversion () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:(Css.Media.of_string "screen") [ Css.Stylesheet.Rule rule ]
  in

  let sheet = Css.Stylesheet.v [ Css.Stylesheet.Rule rule; media_stmt ] in

  let items = sheet in
  Alcotest.(check int) "items count" 2 (List.length items);

  (* Check we can round-trip *)
  let reconstructed = Css.Stylesheet.v items in
  Alcotest.(check int)
    "reconstructed rules" 1
    (List.length (Css.Stylesheet.rules reconstructed));
  Alcotest.(check int)
    "reconstructed media" 1
    (List.length (media_queries reconstructed))

(* Not a roundtrip test *)
let test_concat_stylesheets () =
  let decl1 =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule1 = rule ~selector:(Selector.class_ "red") [ decl1 ] in
  let _sheet1 = Css.Stylesheet.v [ Rule rule1 ] in

  let decl2 =
    Css.Declaration.color (Css.Values.Hex { hash = true; value = "0000ff" })
  in
  let rule2 = rule ~selector:(Selector.class_ "blue") [ decl2 ] in
  let _sheet2 = Css.Stylesheet.v [ Rule rule2 ] in

  let combined = Css.Stylesheet.v [ Rule rule1; Rule rule2 ] in
  Alcotest.(check int)
    "combined rules count" 2
    (List.length (Css.Stylesheet.rules combined))

(* Not a roundtrip test *)
let test_default_property_rule () =
  (* Test that property rules can be created with and without initial values *)
  let prop_with_initial =
    property ~syntax:Css.Variables.Color
      ~initial_value:(Css.Values.hex "#ff0000") "--my-color"
  in
  (* Universal syntax can omit initial-value *)
  let prop_no_initial =
    property ~syntax:Css.Variables.Universal "--other-var"
  in

  (* Test these generate valid statements *)
  let sheet = Css.Stylesheet.v [ prop_with_initial; prop_no_initial ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in

  check_stylesheet output

(* Not a roundtrip test *)
let test_property_composite_syntax () =
  (* Typed composite syntax: <length> | <percentage> *)
  let syn = Css.Variables.Or (Css.Variables.Length, Css.Variables.Percentage) in
  let prop =
    property ~syntax:syn ~initial_value:(Either.Left (Css.Values.Px 0.))
      "--size"
  in
  let sheet = Css.Stylesheet.v [ prop ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

(** Test [@property] descriptor permutations and minified canonical order *)

(* Not a roundtrip test *)
let test_property_permutations () =
  (* Test that @property descriptors can appear in any order but print
     canonically According to CSS spec, the canonical order should be: 1. syntax
     (required) 2. inherits (required) 3. initial-value (optional) *)

  (* Different permutations of the same property should all produce the same
     output *)
  let canonical =
    "@property --x{syntax:\"<length>\";inherits:true;initial-value:0px}"
  in

  (* Permutation 1: syntax, inherits, initial-value (canonical order) *)
  check_stylesheet ~expected:canonical
    "@property --x { syntax: \"<length>\"; inherits: true; initial-value: 0px }";

  (* Permutation 2: inherits, syntax, initial-value *)
  check_stylesheet ~expected:canonical
    "@property --x { inherits: true; syntax: \"<length>\"; initial-value: 0px }";

  (* Permutation 3: initial-value, inherits, syntax *)
  check_stylesheet ~expected:canonical
    "@property --x { initial-value: 0px; inherits: true; syntax: \"<length>\" }";

  (* Permutation 4: syntax, initial-value, inherits *)
  check_stylesheet ~expected:canonical
    "@property --x { syntax: \"<length>\"; initial-value: 0px; inherits: true }";

  (* Permutation 5: inherits, initial-value, syntax *)
  check_stylesheet ~expected:canonical
    "@property --x { inherits: true; initial-value: 0px; syntax: \"<length>\" }";

  (* Permutation 6: initial-value, syntax, inherits *)
  check_stylesheet ~expected:canonical
    "@property --x { initial-value: 0px; syntax: \"<length>\"; inherits: true }";

  (* Test with only required descriptors in different orders *)
  let minimal = "@property --y{syntax:\"*\";inherits:false}" in

  check_stylesheet ~expected:minimal
    "@property --y { syntax: \"*\"; inherits: false }";
  check_stylesheet ~expected:minimal
    "@property --y { inherits: false; syntax: \"*\" }"

(** Negative helper for [@property] parsing errors *)
let expect_property_error name input =
  let r = Css.Cursor.of_string input in
  try
    let _ = read_stylesheet r in
    Alcotest.failf "%s: expected parse error" name
  with Css.Cursor.Parse_error _ -> ()

(* Not a roundtrip test *)
let test_property_missing_descriptors () =
  expect_property_error "missing syntax" "@property --x { inherits: true }";
  expect_property_error "missing inherits"
    "@property --x { syntax: \"<color>\" }";
  (* initial-value is required for non-universal syntax *)
  expect_property_error "missing initial-value for <length>"
    "@property --x { syntax: \"<length>\"; inherits: true }";
  (* But initial-value is optional for universal syntax "*" *)
  check_stylesheet ~expected:"@property --x{syntax:\"*\";inherits:false}"
    "@property --x { syntax: \"*\"; inherits: false }"

(* Not a roundtrip test *)
let test_property_invalid_inherits () =
  expect_property_error "invalid inherits value"
    "@property --x { syntax: \"*\"; inherits: maybe }"

(* Not a roundtrip test *)
let test_property_unknown_descriptor () =
  expect_property_error "unknown descriptor"
    "@property --x { syntax: \"*\"; inherits: true; unknown: 1 }"

(* Not a roundtrip test *)
let test_property_duplicate_descriptors () =
  (* Test duplicate descriptors *)
  check_stylesheet ~expected:"@property --dup{syntax:\"*\";inherits:false}"
    "@property --dup { inherits: true; syntax: \"*\"; inherits: false; }";
  check_stylesheet
    ~expected:
      "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"
    "@property --color { syntax: \"<color>\"; initial-value: blue; inherits: \
     true; initial-value: red }"

(* Not a roundtrip test *)
let test_property_comments_whitespace () =
  (* Test property with comments *)
  check_stylesheet ~expected:"@property --gap{syntax:\"*\";inherits:true}"
    "@property --gap { /* allow any */ syntax: \"*\"; /* ws */  inherits: true \
     }"

(* ignore-test *)
let test_property_spec_syntax_vectors () =
  check_stylesheet
    ~expected:
      "@property --length-percentage{syntax:\"<length> | \
       <percentage>\";inherits:false;initial-value:0%}"
    "@property --length-percentage { syntax: \"<length> | <percentage>\"; \
     inherits: false; initial-value: 0%; }";
  check_stylesheet
    ~expected:
      "@property \
       --color-list{syntax:\"<color>#\";inherits:true;initial-value:red}"
    "@property --color-list { syntax: \"<color>#\"; inherits: true; \
     initial-value: red; }";
  check_stylesheet
    ~expected:
      "@property \
       --transform-like{syntax:\"<transform-list>\";inherits:false;initial-value:none}"
    "@property --transform-like { syntax: \"<transform-list>\"; inherits: \
     false; initial-value: none; }";
  check_stylesheet
    ~expected:
      "@property \
       --custom-ident{syntax:\"<custom-ident>\";inherits:false;initial-value:foo}"
    "@property --custom-ident { syntax: \"<custom-ident>\"; inherits: false; \
     initial-value: foo; }";
  expect_property_error "property name must be custom property"
    "@property color { syntax: \"<color>\"; inherits: false; initial-value: \
     red }";
  expect_property_error "syntax descriptor must be string"
    "@property --x { syntax: <color>; inherits: false; initial-value: red }";
  expect_property_error "empty syntax string"
    "@property --x { syntax: \"\"; inherits: false; initial-value: red }";
  expect_property_error "invalid initial for syntax"
    "@property --x { syntax: \"<length>\"; inherits: false; initial-value: red \
     }"

(* Not a roundtrip test *)
let test_layer_pp () =
  let decl =
    Css.Declaration.color (Css.Values.Hex { hash = true; value = "0000ff" })
  in
  let rule_obj = rule ~selector:(Selector.class_ "blue") [ decl ] in
  let layer_stmt = layer ~name:"utilities" [ Rule rule_obj ] in

  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  Alcotest.(check string)
    "layer pp" "@layer utilities{.blue{color:#0000ff}}" output;

  (* Test empty layer - per CSS spec, empty @layer statements end with
     semicolon *)
  let empty_layer = layer ~name:"base" [] in
  let empty_sheet = Css.Stylesheet.v [ empty_layer ] in
  let empty_output =
    Css.Stylesheet.pp ~minify:true ~newline:false empty_sheet
  in
  Alcotest.(check string) "empty layer" "@layer base;" empty_output

(** Test complete stylesheet pp *)
let pp_case () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt = media ~condition:(Css.Media.of_string "screen") [ Rule r ] in
  let prop =
    property ~syntax:Css.Variables.Color
      ~initial_value:(Css.Values.Named Css.Values.Blue) "--primary"
  in

  let sheet = Css.Stylesheet.v [ Rule r; media_stmt; prop ] in

  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  Alcotest.(check string)
    "stylesheet pp"
    ".red{background-color:#ff0000}@media \
     screen{.red{background-color:#ff0000}}@property \
     --primary{syntax:\"<color>\";inherits:false;initial-value:blue}"
    output

(** Test [@charset] rules *)
let charset_case () =
  (* Test charset roundtrip *)
  check_stylesheet "@charset \"UTF-8\";"

(** Test [@import] rules *)
let import_case () =
  (* Test various import forms *)
  check_stylesheet "@import 'styles.css';";
  check_stylesheet "@import url(utilities.css) layer(utilities);";
  check_stylesheet "@import 'print.css' print;";
  check_import_rule
    ~expected:"@import \"theme.css\" supports(selector(:has(img))) screen;"
    "@import url(theme.css) supports(selector(:has(img))) screen;";
  check_import_rule
    ~expected:
      "@import \"tokens.css\" layer(theme.tokens) supports(--theme:dark) \
       (prefers-color-scheme:dark);"
    "@import url(tokens.css) layer(theme.tokens) supports(--theme: dark) \
     (prefers-color-scheme: dark);";
  check_import_rule
    ~expected:"@import \"wide.css\" supports(width:stretch) (width >= 40em);"
    "@import url(wide.css) supports((width: stretch)) (width >= 40em);";
  neg_cursor read_import_rule "@import url(theme.css) screen layer(theme);";
  neg_cursor read_import_rule
    "@import url(theme.css) supports(selector()) screen;";
  neg_cursor read_import_rule "@import url(theme.css) layer(theme,) screen;"

(** Test [@namespace] rules *)
let namespace_case () =
  (* Test namespace roundtrips *)
  check_stylesheet "@namespace url(http://www.w3.org/1999/xhtml);";
  check_stylesheet ~expected:"@namespace svg url(http://www.w3.org/2000/svg);"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  check_stylesheet
    ~expected:"@namespace math \"http://www.w3.org/1998/Math/MathML\";"
    "@namespace math \"http://www.w3.org/1998/Math/MathML\";";
  neg_cursor read_stylesheet "@namespace { url(http://example.test); }";
  neg_cursor read_stylesheet "@namespace svg;"

(** Test [@keyframes] rules *)
let keyframes_case () =
  (* Test keyframes roundtrip *)
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}100%{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes fade{from{opacity:0}to{opacity:1}}"
    "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"

(* ignore-test *)
let test_keyframes_spec_edge_vectors () =
  check_stylesheet
    ~expected:"@keyframes pulse{0%,50%,100%{opacity:1}25%,75%{opacity:.5}}"
    "@keyframes pulse { 0%, 50%, 100% { opacity: 1 } 25%, 75% { opacity: .5 } }";
  check_stylesheet
    ~expected:
      "@keyframes \
       slide{100%{transform:translateX(10px)}0%{transform:translateX(0)}}"
    "@keyframes slide { 100% { transform: translateX(10px) } 0% { transform: \
     translateX(0) } }";
  check_stylesheet
    ~expected:"@-webkit-keyframes fade{from{opacity:0}to{opacity:1}}"
    "@-webkit-keyframes fade { from { opacity: 0 } to { opacity: 1 } }";
  neg_cursor read_stylesheet "@keyframes missing-block";
  neg_cursor read_stylesheet "@keyframes bad { -1% { opacity: 0 } }";
  neg_cursor read_stylesheet "@keyframes bad { 101% { opacity: 1 } }";
  neg_cursor read_stylesheet "@keyframes bad { 50px { opacity: 1 } }";
  neg_cursor read_stylesheet "@keyframes bad { from, { opacity: 1 } }"

(** Test [@font-face] rules *)
let font_face_case () =
  (* Test font-face roundtrip *)
  check_stylesheet
    ~expected:
      "@font-face \
       {font-family:MyCustomFont;src:url('font.woff2');font-display:swap}"
    "@font-face { font-family: MyCustomFont; src: url('font.woff2'); \
     font-display: swap; }"

let spec_fontface_descriptors () =
  check_stylesheet
    ~expected:
      "@font-face {font-family:Brand;src:local(\"Brand\"),url(\"brand.woff2\") \
       format(\"woff2\") tech(variations);font-weight:400 \
       700;font-style:normal italic;font-stretch:75% \
       125%;font-display:optional;unicode-range:U+25-FF}"
    "@font-face { font-family: Brand; src: local(\"Brand\"), \
     url(\"brand.woff2\") format(\"woff2\") tech(variations); font-weight: 400 \
     700; font-style: normal italic; font-stretch: 75% 125%; font-display: \
     optional; unicode-range: U+0025-00FF; }";
  check_stylesheet
    ~expected:
      "@font-face \
       {font-family:MetricAdjusted;src:url(metric.woff2);size-adjust:92%;ascent-override:90%;descent-override:25%;line-gap-override:normal}"
    "@font-face { font-family: MetricAdjusted; src: url(metric.woff2); \
     size-adjust: 92%; ascent-override: 90%; descent-override: 25%; \
     line-gap-override: normal; }";
  check_stylesheet
    ~expected:
      "@font-face \
       {font-family:FeatureFont;src:url(feature.woff2);font-feature-settings:\"kern\" \
       1;font-variation-settings:\"wght\" 650}"
    "@font-face { font-family: FeatureFont; src: url(feature.woff2); \
     font-feature-settings: \"kern\" 1; font-variation-settings: \"wght\" 650; \
     }";
  neg_cursor read_stylesheet "@font-face { src: url(font.woff2); }";
  neg_cursor read_stylesheet "@font-face { font-family: Brand; }";
  neg_cursor read_stylesheet
    "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
     maybe; }"

(** Test [@page] rules *)
let page_case () =
  (* Test page roundtrip *)
  check_stylesheet ~expected:"@page{margin:1in}" "@page { margin: 1in; }";
  check_stylesheet ~expected:"@page:first{margin:2in}"
    "@page :first { margin: 2in; }";
  check_stylesheet ~expected:"@page:left{margin-left:4cm;margin-right:3cm}"
    "@page :left { margin-left: 4cm; margin-right: 3cm; }";
  check_stylesheet ~expected:"@page:right{size:A4;margin:1cm}"
    "@page :right { size: A4; margin: 1cm; }";
  check_stylesheet ~expected:"@page chapter:first{margin-top:6cm}"
    "@page chapter:first { margin-top: 6cm; }";
  check_stylesheet
    ~expected:
      "@page{@top-left{content:\"title\"}@bottom-center{content:counter(page)}}"
    "@page { @top-left { content: \"title\" } @bottom-center { content: \
     counter(page) } }";
  neg_cursor read_stylesheet "@page : { margin: 1cm }";
  neg_cursor read_stylesheet "@page :unknown { margin: 1cm }";
  neg_cursor read_stylesheet "@page { color: red }"

let page_margin_edges () =
  check_stylesheet
    ~expected:
      "@page \
       invoice:first{size:A4;margin:1cm;@top-left{content:\"Invoice\"}@bottom-right{content:counter(page)}}"
    "@page invoice:first { size: A4; margin: 1cm; @top-left { content: \
     \"Invoice\" } @bottom-right { content: counter(page) } }";
  check_stylesheet
    ~expected:
      "@page:left{margin-left:3cm;@left-middle{content:string(chapter)}}"
    "@page :left { margin-left: 3cm; @left-middle { content: string(chapter) } \
     }";
  check_stylesheet
    ~expected:"@page{bleeds:6pt;marks:crop cross;@top-center{content:none}}"
    "@page { bleeds: 6pt; marks: crop cross; @top-center { content: none } }";
  neg_cursor read_stylesheet "@page invoice:blank:first { margin: 1cm }";
  neg_cursor read_stylesheet "@page { @unknown { content: none } }";
  neg_cursor read_stylesheet "@page { @top-left; }";
  neg_cursor read_stylesheet "@page { @top-left { color: red } }"

let property_rule_edges () =
  check_stylesheet
    ~expected:
      "@property \
       --shadow-color{syntax:\"<color>\";inherits:true;initial-value:transparent}"
    "@property --shadow-color { syntax: \"<color>\"; inherits: true; \
     initial-value: transparent; }";
  check_stylesheet
    ~expected:
      "@property \
       --track-list{syntax:\"<length>#\";inherits:false;initial-value:1px}"
    "@property --track-list { syntax: \"<length>#\"; inherits: false; \
     initial-value: 1px; }";
  check_stylesheet
    ~expected:"@property --any-tokens{syntax:\"*\";inherits:true}"
    "@property --any-tokens { syntax: \"*\"; inherits: true; }";
  neg_cursor read_stylesheet
    "@property --bad { syntax: \"<length>+\"; inherits: false; initial-value: \
     red }";
  neg_cursor read_stylesheet
    "@property --bad { syntax: \"<color>\"; inherits: yes; initial-value: red }";
  neg_cursor read_stylesheet
    "@property --bad { syntax: \"<length> |\"; inherits: false; initial-value: \
     1px }";
  neg_cursor read_stylesheet
    "@property color { syntax: \"*\"; inherits: false }"

let spec_font_face_descriptor_matrix () =
  (* Descriptor syntax is a spec oracle; these are not snapshots of the current
     printer. *)
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@font-face {font-family:\"Brand Sans\";src:local(\"Brand \
         Sans\"),url(brand.woff2) format(\"woff2\");font-display:fallback}",
        "@font-face { font-family: \"Brand Sans\"; src: local(\"Brand Sans\"), \
         url(brand.woff2) format(\"woff2\"); font-display: fallback; }" );
      ( "@font-face \
         {font-family:RangeFont;src:url(range.woff2);font-weight:100 \
         900;font-style:oblique 10deg 20deg;font-stretch:50% 200%}",
        "@font-face { font-family: RangeFont; src: url(range.woff2); \
         font-weight: 100 900; font-style: oblique 10deg 20deg; font-stretch: \
         50% 200%; }" );
      ( "@font-face \
         {font-family:Metrics;src:url(metrics.woff2);size-adjust:100%;ascent-override:normal;descent-override:20%;line-gap-override:0%}",
        "@font-face { font-family: Metrics; src: url(metrics.woff2); \
         size-adjust: 100%; ascent-override: normal; descent-override: 20%; \
         line-gap-override: 0%; }" );
      ( "@font-face \
         {font-family:TallMetrics;src:url(tall.woff2);ascent-override:120%;descent-override:125%;line-gap-override:0%}",
        "@font-face { font-family: TallMetrics; src: url(tall.woff2); \
         ascent-override: 120%; descent-override: 125%; line-gap-override: 0%; \
         }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@font-face { font-family: Brand; src: url(brand.woff2); font-weight: \
       900 100 }";
      "@font-face { font-family: Brand; src: url(brand.woff2); \
       ascent-override: -1%; }";
      "@font-face { font-family: Brand; src: url(brand.woff2); size-adjust: \
       normal; }";
    ]

let spec_keyframes_selector_matrix () =
  check_stylesheet
    ~expected:
      "@keyframes move{from{translate:none}50%{translate:10px \
       20px}to{translate:20px 0}}"
    "@keyframes move { from { translate: none } 50% { translate: 10px 20px } \
     to { translate: 20px 0 } }";
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@keyframes bad { 50%, { opacity: 1 } }";
      "@keyframes bad { from, 120% { opacity: 1 } }";
    ]

let spec_page_margin_descriptor_matrix () =
  check_stylesheet
    ~expected:
      "@page chapter:right{size:letter \
       landscape;margin:1in;@right-top{content:counter(page)}@bottom-center{content:\"Chapter\"}}"
    "@page chapter:right { size: letter landscape; margin: 1in; @right-top { \
     content: counter(page) } @bottom-center { content: \"Chapter\" } }";
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@page :first:left { margin: 1cm }";
      "@page { @top-center { display: block } }";
    ]

let spec_property_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@property \
         --angle-list{syntax:\"<angle>#\";inherits:false;initial-value:0deg}",
        "@property --angle-list { syntax: \"<angle>#\"; inherits: false; \
         initial-value: 0deg }" );
      ( "@property --ident-or-color{syntax:\"<custom-ident> | \
         <color>\";inherits:true;initial-value:currentColor}",
        "@property --ident-or-color { syntax: \"<custom-ident> | <color>\"; \
         inherits: true; initial-value: currentColor }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@property --bad { syntax: \"<angle>#\"; inherits: false; initial-value: \
       red }";
      "@property --bad { syntax: \"<length> || <color>\"; inherits: false; \
       initial-value: 1px }";
    ]

(** Test sheet_item variants *)
let sheet_item_case () =
  (* Test that we can parse stylesheets with various statement types *)
  let test_statements =
    [
      ("@charset \"UTF-8\";", "@charset \"UTF-8\";");
      ("@import 'test.css';", "@import 'test.css';");
      (".class { color: red; }", ".class{color:red}");
      ( "@media print { .class { color: black; } }",
        "@media print{.class{color:black}}" );
      ( "@layer base { .btn { padding: 10px; } }",
        "@layer base{.btn{padding:10px}}" );
      ( "@property --var { syntax: \"*\"; inherits: false; }",
        "@property --var{syntax:\"*\";inherits:false}" );
    ]
  in
  List.iter
    (fun (input, expected) -> check_stylesheet ~expected input)
    test_statements

(** Test stylesheet ordering constraints *)
let ordering () =
  (* Per CSS spec, certain at-rules must appear in specific order *)
  (* Test that we can parse stylesheets with at-rules in the correct order *)
  let input =
    "@charset \"UTF-8\";\n@import 'base.css';\n.btn { color: red; }"
  in
  check_stylesheet
    ~expected:"@charset \"UTF-8\";@import 'base.css';.btn{color:red}" input

(* Not a roundtrip test *)
let test_read_stylesheet_basic () =
  let css = ".btn { color: red; padding: 10px; }" in
  let reader = Css.Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule" 1 (List.length rules);
  let rule = List.hd rules in
  let decls = declarations rule in
  Alcotest.(check int) "rule has two declarations" 2 (List.length decls)

(* Not a roundtrip test *)
let test_read_stylesheet_multiple_rules () =
  let css = ".btn { color: red; } .card { margin: 5px; }" in
  let reader = Css.Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has two rules" 2 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_empty () =
  let css = "" in
  let reader = Css.Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "empty stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_whitespace_only () =
  let css = "   \n\t  " in
  let reader = Css.Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int)
    "whitespace-only stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_with_comments () =
  let css = "/* comment */ .btn { color: red; } /* another comment */" in
  let reader = Css.Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule despite comments" 1 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_error_recovery () =
  (* According to CSS spec, element selectors can be any valid identifier.
     "invalid-css-here .card" is valid CSS (element selector + descendant
     combinator + class). We need actual invalid CSS syntax to test error
     handling. *)
  let css = ".btn { color: red; } { margin: 5px; }" in
  (* Missing selector before { *)
  let reader = Css.Cursor.of_string css in
  (* Should fail on invalid CSS without recovery *)
  try
    let _sheet = read_stylesheet reader in
    Alcotest.fail "Expected parsing to fail on invalid CSS"
  with Css.Cursor.Parse_error _ ->
    (* This is expected - parsing should fail *)
    ()

(* Not a roundtrip test *)
let test_of_string () =
  let css = ".btn { color: red; }" in
  match of_string css with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "of_string works" 1 (List.length rules)
  | Error msg -> Alcotest.fail ("of_string failed: " ^ msg)

(* Not a roundtrip test *)
let test_of_string_positive () =
  (* Test valid CSS - simple rule *)
  let css1 = ".btn { color: red; }" in
  (match of_string css1 with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "single rule parsed" 1 (List.length rules)
  | Error msg -> Alcotest.fail ("valid CSS failed: " ^ msg));

  (* Test valid CSS - multiple rules *)
  let css2 = ".btn { color: red; } .card { margin: 10px; }" in
  (match of_string css2 with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "multiple rules parsed" 2 (List.length rules)
  | Error msg -> Alcotest.fail ("multiple rules failed: " ^ msg));

  (* Test valid CSS - empty stylesheet *)
  let css3 = "" in
  (match of_string css3 with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "empty stylesheet" 0 (List.length rules)
  | Error msg -> Alcotest.fail ("empty stylesheet failed: " ^ msg));

  (* Test valid CSS - whitespace only *)
  let css4 = "   \n\t  " in
  (match of_string css4 with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "whitespace only" 0 (List.length rules)
  | Error msg -> Alcotest.fail ("whitespace only failed: " ^ msg));

  check_stylesheet ~expected:".btn{color:rgb(255 255 255)}"
    ".btn { color: rgb(300, 300, 300); }";

  check_stylesheet ~expected:".btn{color:rgb(255 0 0)}"
    ".btn { color: rgba(255, 0, 0); }";

  check_stylesheet ~expected:".btn{color:rgb(50% 100 50%)}"
    ".btn { color: rgb(50%, 100, 50%); }";

  check_stylesheet ~expected:".btn{--:value}" ".btn { --: value; }"

(* Not a roundtrip test *)
let test_of_string_negative () =
  (* Helper function to test invalid CSS *)
  let test_invalid_css css expected_error =
    match of_string css with
    | Ok _ ->
        Alcotest.fail
          ("should have failed: " ^ expected_error ^ " - CSS: " ^ css)
    | Error msg ->
        Alcotest.(check bool)
          ("error contains information for " ^ expected_error)
          true
          (String.length msg > 0)
  in

  (* Basic syntax errors *)
  test_invalid_css ".btn { color: }" "empty property value";
  (* Unclosed brace is recovered per CSS Syntax §5.3.7 — blocks auto-close at
     EOF. Verified as a positive case below in test_invalid_syntax. *)
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
  test_invalid_css ".btn { color: #gggggg; }" "invalid hex color";

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

  (* String and quote errors — per CSS Syntax §4.3.5, an unterminated string at
     EOF is a parse-error warning but still yields a [<string-token>] whose
     contents run to EOF. The outer block then auto-closes per §5.3.7. So these
     are recovered (positive) cases, not hard errors. *)

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
  test_invalid_css ".btn { -custom: value; }"
    "invalid custom property (single hyphen)";

  (* Media query and at-rule errors *)
  test_invalid_css "@media { .btn { color: red; } }"
    "media query without condition";

  (* Specificity and cascade errors *)
  test_invalid_css "btn.#id { color: red; }" "invalid selector combination";

  (* Unicode and encoding errors — per §4.3.5 an unterminated string at EOF
     still produces a [<string-token>]; the [\\'] escape consumes the next [\'],
     extending the string past the semicolon. This is recovered per §5.3.7, not
     a hard error. *)

  (* According to CSS spec section 4.3.7, \g is a valid escape that produces 'g'
     So '\gggg' is valid CSS and should parse successfully. *)
  match of_string ".btn { content: '\\gggg'; }" with
  | Ok sheet ->
      let rules = Css.Stylesheet.rules sheet in
      Alcotest.(check int)
        "\\gggg escape sequence should parse (valid per CSS spec)" 1
        (List.length rules)
  | Error _ ->
      Alcotest.fail
        "\\gggg should be valid per CSS spec (\\g escapes to 'g', followed by \
         'ggg')"

let stylesheet_tests =
  [
    (* Core type tests *)
    ("rule", `Quick, test_rule);
    ("stylesheet", `Quick, test_stylesheet);
    ("rule creation", `Quick, test_rule_creation);
    ("media rule creation", `Quick, test_media_rule_creation);
    ("container rule creation", `Quick, test_container_rule_creation);
    ("supports rule creation", `Quick, test_supports_rule_creation);
    ("supports nested creation", `Quick, test_supports_nested_creation);
    ("property rule creation", `Quick, test_property_rule_creation);
    ("layer rule creation", `Quick, test_layer_rule_creation);
    ("construct rule helper", `Quick, test_construct_rule_helper);
    ("stylesheet helper", `Quick, helper);
    ("empty stylesheet", `Quick, test_empty_stylesheet);
    ("stylesheet construction", `Quick, construction);
    ("stylesheet items conversion", `Quick, items_conversion);
    ("concat stylesheets", `Quick, test_concat_stylesheets);
    ("default decl of property rule", `Quick, test_default_property_rule);
    ("property composite syntax", `Quick, test_property_composite_syntax);
    (* Additional property tests *)
    ("property permutations", `Quick, test_property_permutations);
    ("property missing descriptors", `Quick, test_property_missing_descriptors);
    ("property invalid inherits", `Quick, test_property_invalid_inherits);
    ("property unknown descriptor", `Quick, test_property_unknown_descriptor);
    ( "property duplicate descriptors",
      `Quick,
      test_property_duplicate_descriptors );
    ("property comments/whitespace", `Quick, test_property_comments_whitespace);
    ("property spec syntax vectors", `Quick, test_property_spec_syntax_vectors);
    ("layer pp", `Quick, test_layer_pp);
    ("stylesheet pp", `Quick, pp_case);
    (* New CSS/MDN spec compliance tests *)
    ("charset", `Quick, charset_case);
    ("import", `Quick, import_case);
    ("namespace", `Quick, namespace_case);
    ("keyframes", `Quick, keyframes_case);
    ("keyframes spec edge vectors", `Quick, test_keyframes_spec_edge_vectors);
    ("font_face", `Quick, font_face_case);
    ("font-face spec descriptor vectors", `Quick, spec_fontface_descriptors);
    ( "spec font-face descriptor matrix",
      `Quick,
      spec_font_face_descriptor_matrix );
    ("spec keyframes selector matrix", `Quick, spec_keyframes_selector_matrix);
    ("page", `Quick, page_case);
    ("page margin edges", `Quick, page_margin_edges);
    ( "spec page margin descriptor matrix",
      `Quick,
      spec_page_margin_descriptor_matrix );
    ("property rule edges", `Quick, property_rule_edges);
    ("spec property descriptor matrix", `Quick, spec_property_descriptor_matrix);
    ("sheet_item", `Quick, sheet_item_case);
    ("ordering", `Quick, ordering);
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

(* Tests for newly added check functions *)
(* Not a roundtrip test *)
let test_check () =
  (* Test basic stylesheet parsing using check function *)
  check ~expected:".test{color:red}" ".test { color: red }";
  check ~expected:"@media screen{.test{color:blue}}"
    "@media screen { .test { color: blue } }"

let test_import_rule () =
  check_import_rule ~expected:"@import \"test.css\";" "@import 'test.css';";
  check_import_rule ~expected:"@import \"styles.css\" screen;"
    "@import url('styles.css') screen;";

  (* Test invalid import rules *)
  neg_cursor read_import_rule "@import";
  (* Missing URL *)
  neg_cursor read_import_rule "@import test.css";
  (* Missing quotes *)
  neg_cursor read_import_rule "import 'test.css'";
  (* Missing @ *)
  (* Unclosed quote at EOF — per CSS Syntax §4.3.5 the lexer still returns a
     string-token (the ill-formedness is a parse-error warning, not a
     token-level failure), so [\@import 'test.css] parses as a valid import. *)
  check_import_rule ~expected:"@import \"test.css\";" "@import 'test.css"

let test_config () =
  (* Test config parsing - configs are rendering configuration objects, not CSS
     at-rules *)
  check_config
    ~expected:
      "{ minify = true; mode = Variables; optimize = false; newline = true }"
    "{ minify = true; mode = Variables; optimize = false; newline = true }";
  check_config
    ~expected:
      "{ minify = false; mode = Variables; optimize = true; newline = false }"
    "{ minify = false; mode = Variables; optimize = true; newline = false }";
  neg_cursor read_config "@import"
(* Incomplete import *)

(* Not a roundtrip test *)
let test_advanced_selectors () =
  check_stylesheet ~expected:".btn:hover{color:blue}"
    ".btn:hover { color: blue; }";
  check_stylesheet ~expected:".btn:before{content:\"icon\"}"
    ".btn::before { content: 'icon'; }";
  (* Attribute values that are valid identifiers get normalized to unquoted
     form *)
  check_stylesheet ~expected:".btn[data-type=primary]{background:blue}"
    ".btn[data-type='primary'] { background: blue; }";
  check_stylesheet ~expected:".parent>.child{margin:0}"
    ".parent > .child { margin: 0; }";
  check_stylesheet ~expected:".sibling+.next{padding:10px}"
    ".sibling + .next { padding: 10px; }";
  check_stylesheet ~expected:".element~.general-sibling{color:red}"
    ".element ~ .general-sibling { color: red; }"

(* Not a roundtrip test *)
let test_advanced_properties () =
  check_stylesheet ~expected:".box{transform:rotate(45deg) scale(1.2)}"
    ".box { transform: rotate(45deg) scale(1.2); }";
  check_stylesheet ~expected:".grid{display:grid;grid-template-columns:1fr 2fr}"
    ".grid { display: grid; grid-template-columns: 1fr 2fr; }";
  check_stylesheet ~expected:".flex{display:flex;justify-content:space-between}"
    ".flex { display: flex; justify-content: space-between; }";
  check_stylesheet ~expected:".shadow{box-shadow:0 4px 8px rgb(0 0 0/.2)}"
    ".shadow { box-shadow: 0 4px 8px rgba(0,0,0,0.2); }";
  check_stylesheet
    ~expected:".gradient{background:linear-gradient(to right,red,blue)}"
    ".gradient { background: linear-gradient(to right, red, blue); }"

(* Not a roundtrip test *)
let test_complex_values () =
  check_stylesheet ~expected:".calc{width:calc(100% - 20px)}"
    ".calc { width: calc(100% - 20px); }";
  check_stylesheet ~expected:".multi{margin:10px 20px 30px 40px}"
    ".multi { margin: 10px 20px 30px 40px; }";
  check_stylesheet ~expected:".var{color:var(--primary-color,blue)}"
    ".var { color: var(--primary-color, blue); }";
  check_stylesheet ~expected:".clamp{font-size:clamp(1rem,2vw,2rem)}"
    ".clamp { font-size: clamp(1rem, 2vw, 2rem); }";
  check_stylesheet ~expected:".minmax{width:minmax(200px,1fr)}"
    ".minmax { width: minmax(200px, 1fr); }"

(* Not a roundtrip test *)
let test_nested_rules () =
  check_stylesheet
    ~expected:
      "@media (min-width:768px){@supports (display:grid){.grid{display:grid}}}"
    "@media (min-width: 768px) { @supports (display: grid) { .grid { display: \
     grid; } } }";
  check_stylesheet
    ~expected:"@layer base{@media print{.print-only{display:block}}}"
    "@layer base { @media print { .print-only { display: block; } } }";
  check_stylesheet
    ~expected:
      "@container (width > 400px){@media \
       (orientation:landscape){.landscape{color:green}}}"
    "@container (width > 400px) { @media (orientation: landscape) { .landscape \
     { color: green; } } }"

(** Negative tests for invalid CSS *)
let expect_parse_error input =
  let r = Css.Cursor.of_string input in
  try
    let _ = read_stylesheet r in
    Alcotest.failf "Expected parse error for: %s" input
  with Css.Cursor.Parse_error _ -> ()

(* Not a roundtrip test *)
let spec_s7_block_examples () =
  (* CSS Syntax Level 3 section 7.1: these productions are parsed as generic
     block contents, then validated by the rule grammar that owns the block. *)
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  check_stylesheet ~expected:"p>a{color:blue;text-decoration:underline}"
    "p > a { color: blue; text-decoration: underline; }";
  check_stylesheet
    ~expected:"@font-face {font-family:MyFont;src:url(font.woff2)}"
    "@font-face { font-family: MyFont; src: url(font.woff2); }";
  check_stylesheet ~expected:"@page:left{margin-left:4cm;margin-right:3cm}"
    "@page :left { margin-left: 4cm; margin-right: 3cm; }";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}100%{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet ~expected:".card{color:red;& .title{color:blue}}"
    ".card { color: red; & .title { color: blue; } }";
  expect_parse_error "@media print { color: red; body { font-size: 10pt } }";
  expect_parse_error "@keyframes slide { color: red; 50% { opacity: 1 } }";
  expect_parse_error "@font-face { .x { color: red } }"

(* Not a roundtrip test *)
let spec_s8_rule_shapes () =
  (* CSS Syntax Level 3 sections 8.1 and 8.2: top-level qualified rules are
     style rules, and at-rules are either statement or block rules depending on
     whether they end with a semicolon or a {} block. *)
  check_stylesheet ~expected:"p>a{color:blue}" "p > a { color: blue }";
  check_stylesheet ~expected:"@import \"theme.css\";" "@import \"theme.css\";";
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  check_stylesheet ~expected:"@layer reset,base;" "@layer reset, base;";
  expect_parse_error "p > a";
  expect_parse_error "@import \"theme.css\" .a { color: red }";
  expect_parse_error "@media print"

let spec_namespace_serialization () =
  (* CSS Namespaces: a prefixed namespace rule serializes as [@namespace
     <prefix> <namespace-url>;]. The whitespace between prefix and URL is
     required syntax, not an implementation formatting choice. *)
  check_stylesheet ~expected:"@namespace svg url(http://www.w3.org/2000/svg);"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  check_stylesheet
    ~expected:"@namespace math \"http://www.w3.org/1998/Math/MathML\";"
    "@namespace math \"http://www.w3.org/1998/Math/MathML\";";
  check_stylesheet ~expected:"@namespace url(http://www.w3.org/1999/xhtml);"
    "@namespace url(http://www.w3.org/1999/xhtml);"

(* Not a roundtrip test *)
let spec_s8_charset_not_rule () =
  (* CSS Syntax Level 3 section 8.3: @charset is an encoding declaration shape,
     not a CSS at-rule after stylesheet parsing. *)
  expect_parse_error "@charset \"utf-8\";";
  expect_parse_error "@charset \"utf-8\"; .a { color: red }"

(* Not a roundtrip test *)
let test_invalid_selectors () =
  expect_parse_error "..double-class { color: red; }";
  expect_parse_error "# { color: red; }";
  expect_parse_error ". { color: red; }";
  expect_parse_error "[invalid-attr { color: red; }";
  expect_parse_error ".class:invalid-pseudo { color: red; }"

(* Not a roundtrip test *)
let test_invalid_properties () =
  expect_parse_error ".btn { color: invalid-color; }";
  expect_parse_error ".btn { display: invalid-display; }";
  expect_parse_error ".btn { width: 100invalid; }";
  expect_parse_error ".btn { margin: px; }"

(* Not a roundtrip test *)
let test_invalid_syntax () =
  (* CSS Syntax §5.3.7: unclosed blocks auto-close at EOF and the inner
     declaration is preserved. Verify the AST, don't just accept "didn't
     crash". *)
  check_stylesheet ~expected:".btn{color:red}" ".btn { color: red ";
  expect_parse_error ".btn color: red; }";
  expect_parse_error "{ color: red; }";
  expect_parse_error ".btn { : red; }";
  expect_parse_error ".btn { color red; }"

(* Not a roundtrip test *)
let test_invalid_at_rules () =
  expect_parse_error "@media { .btn { color: red; } }";
  expect_parse_error "@property { syntax: 'color'; inherits: true; }";
  expect_parse_error "@property --var { invalid-descriptor: value; }";
  expect_parse_error "@keyframes { 0% { opacity: 0; } }"

(* Not a roundtrip test *)
let css_syntax_recovery () =
  let check_recovery name css expected min_warnings =
    let { Css.stylesheet; warnings } = Css.parse css in
    Alcotest.(check string)
      (name ^ " stylesheet") expected
      (Css.to_string ~minify:true stylesheet |> String.trim);
    Alcotest.(check bool)
      (name ^ " warning count") true
      (List.length warnings >= min_warnings)
  in
  check_recovery "unknown declaration"
    ".btn { unknown-property: value; color: red; }" ".btn{color:red}" 1;
  check_recovery "invalid declaration"
    ".btn { color: invalid-color; color: red; }" ".btn{color:red}" 1;
  check_recovery "invalid selector list"
    ".ok { color: green } .bad,:future-pseudo { color: red }" ".ok{color:green}"
    1;
  check_recovery "unknown at-rule"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    ".ok{color:blue}" 1

let css_syntax_recovery_structural () =
  let declaration_counts stylesheet =
    Css.rule_statements stylesheet
    |> List.map (fun statement ->
        match Css.as_rule statement with
        | Some (_, declarations, _) -> List.length declarations
        | None -> Alcotest.fail "expected recovered qualified rule")
  in
  let check_counts name css expected_counts min_warnings =
    let { Css.stylesheet; warnings } = Css.parse css in
    Alcotest.(check (list int))
      (name ^ " declaration counts")
      expected_counts
      (declaration_counts stylesheet);
    Alcotest.(check bool)
      (name ^ " warning count") true
      (List.length warnings >= min_warnings)
  in
  (* CSS Syntax 5.4.4: invalid declarations are discarded; qualified rules
     survive even if every declaration in the rule was invalid. *)
  check_counts "bad declarations leave empty rules"
    ".a { color: rgb(300); } .b { color: red; } .c { width: calc(1px + ); }"
    [ 0; 1; 0 ] 2;
  check_counts "bad declaration does not discard later declaration"
    ".a { color: rgb(300); background-color: red; }" [ 1 ] 1;
  check_counts "bad selector list drops rule only"
    ".ok { color: green } .bad,:future-pseudo { color: red } .next { color: \
     blue }"
    [ 1; 1 ] 1;
  check_counts "unknown at-rule block skipped"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }" [ 1 ] 1;
  check_counts "unclosed block auto-closed"
    ".a { color: red; .b { color: blue }" [ 1 ] 0

(* Not a roundtrip test *)
let test_invalid_functions () =
  expect_parse_error ".btn { color: rgb(300); }";
  expect_parse_error ".btn { transform: rotate(); }";
  expect_parse_error ".btn { width: calc(100% +); }";
  expect_parse_error ".btn { background: url(; }"

(* Not a roundtrip test *)
let test_layer_roundtrip () =
  let test_css ~expected input =
    let r = Css.Cursor.of_string input in
    try
      let stylesheet = Css.Stylesheet.read r in
      let roundtrip =
        String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
      in
      Alcotest.(check string)
        ("layer roundtrip for " ^ input)
        expected roundtrip
    with Css.Cursor.Parse_error err ->
      Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Css.Error.to_string err)
  in
  (* Layer statement form should roundtrip as-is *)
  test_css ~expected:"@layer components,utilities;"
    "@layer components,utilities;";
  (* Empty layer blocks should be minified to statement form *)
  test_css ~expected:"@layer components;@layer utilities;"
    "@layer components{}@layer utilities{}"

(* Not a roundtrip test *)
let c64_layer_name_syntax () =
  (* CSS Cascade section 6.4.2: a layer name is a dot-separated list of idents
     with no whitespace around dots; CSS-wide keywords are reserved. *)
  check_stylesheet ~expected:"@layer framework.theme{blockquote{display:block}}"
    "@layer framework.theme { blockquote { display: block } }";
  check_stylesheet ~expected:"@layer framework.base,framework.theme;"
    "@layer framework.base, framework.theme;";
  check_stylesheet
    ~expected:
      "@layer reset.type{strong{font-weight:bold}}@layer \
       reset{[hidden]{display:none}}"
    "@layer reset.type { strong { font-weight: bold } } @layer reset { \
     [hidden] { display: none } }";
  neg_cursor read_stylesheet
    "@layer framework . theme { blockquote { display: block } }";
  neg_cursor read_stylesheet "@layer initial { blockquote { display: block } }";
  neg_cursor read_stylesheet
    "@layer framework.revert-layer { blockquote { display: block } }"

(* Not a roundtrip test *)
let c64_layer_nesting_examples () =
  (* CSS Cascade sections 6.4.2 and 6.4.3: dotted layer names are shorthand for
     nested layer segments; nested names do not escape their parent layer. *)
  check_stylesheet
    ~expected:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:rebeccapurple}}"
    "@layer base { p { max-width: 70ch } } @layer framework { @layer base { p \
     { margin-block: 0.75em } } @layer theme { p { color: #222 } } } @layer \
     framework.theme { blockquote { color: rebeccapurple } }";
  check_stylesheet
    ~expected:
      "@layer reset.type{strong{font-weight:bold}}@layer \
       framework{.title{font-weight:100}@layer \
       theme{h1,h2{color:maroon}}}@layer reset{[hidden]{display:none}}"
    "@layer reset.type { strong { font-weight: bold } } @layer framework { \
     .title { font-weight: 100 } @layer theme { h1, h2 { color: maroon } } } \
     @layer reset { [hidden] { display: none } }"

(* Not a roundtrip test *)
let c64_layer_statement_edges () =
  (* CSS Cascade section 6.4.4.2: statement @layer accepts one or more layer
     names, can appear before imports, and declares names in source order. *)
  check_stylesheet
    ~expected:
      "@layer default,theme,components;@import url(theme.css) \
       layer(theme);@layer default{audio[controls]{display:block}}"
    "@layer default, theme, components; @import url(theme.css) layer(theme); \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet
    ~expected:
      "@layer default;@import url(theme.css) layer(theme);@layer \
       components;@layer default{audio[controls]{display:block}}"
    "@layer default; @import url(theme.css) layer(theme); @layer components; \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet ~expected:"@layer framework.base,framework.theme;"
    "@layer framework.base, framework.theme;";
  neg_cursor read_stylesheet "@layer;";
  neg_cursor read_stylesheet "@layer , theme;";
  neg_cursor read_stylesheet "@layer default, { audio { display: block } }";
  neg_cursor read_stylesheet
    "@layer default, theme { audio { display: block } }"

(* Not a roundtrip test *)
let c64_anonymous_layer_edges () =
  (* CSS Cascade section 6.4.2.1: anonymous layers are valid block @layer rules
     but cannot be referenced by name from outside the block. *)
  check_stylesheet
    ~expected:
      "@layer{.private{color:red}}@layer{.private{display:block}}@layer public;"
    "@layer { .private { color: red } } @layer { .private { display: block } } \
     @layer public;";
  check_stylesheet
    ~expected:
      "@layer{@layer foo{.inside{color:red}}@layer foo{.inside{display:block}}}"
    "@layer { @layer foo { .inside { color: red } } @layer foo { .inside { \
     display: block } } }";
  check_stylesheet
    ~expected:
      "@layer{@layer foo{.inside{color:red}}}@layer{@layer \
       foo{.inside{display:block}}}"
    "@layer { @layer foo { .inside { color: red } } } @layer { @layer foo { \
     .inside { display: block } } }"

(* Not a roundtrip test *)
let c64_import_layer_syntax () =
  (* CSS Cascade section 6.4.1: @import can assign an imported sheet to a named
     layer with layer(<layer-name>) or to an anonymous layer with
     layer/layer(). *)
  check_import_rule ~expected:"@import \"headings.css\" layer(default);"
    "@import url(headings.css) layer(default);";
  check_import_rule ~expected:"@import \"links.css\" layer(default) screen;"
    "@import url(links.css) layer(default) screen;";
  check_import_rule ~expected:"@import \"theme.css\" layer(framework.theme);"
    "@import url(theme.css) layer(framework.theme);";
  check_import_rule ~expected:"@import \"base-forms.css\" layer;"
    "@import url(base-forms.css) layer;";
  check_import_rule ~expected:"@import \"base-links.css\" layer;"
    "@import url(base-links.css) layer();";
  check_import_rule ~expected:"@import \"conditional.css\" layer print;"
    "@import url(conditional.css) layer() print;";
  neg_cursor read_import_rule "@import url(theme.css) layer(initial);";
  neg_cursor read_import_rule "@import url(theme.css) layer(framework . theme);";
  neg_cursor read_import_rule "@import url(theme.css) layer(framework,theme);"

(* Not a roundtrip test *)
let c2_import_conditions () =
  (* CSS Cascade sections 2 and 2.1: @import accepts url/string sources,
     optional layer or layer(<layer-name>), optional supports(), and optional
     media query lists. A declaration inside supports() is equivalent to the
     same declaration wrapped as a supports condition. *)
  check_import_rule ~expected:"@import \"mystyle.css\";"
    "@import url(mystyle.css);";
  check_import_rule ~expected:"@import \"mystyle.css\";"
    "@import \"mystyle.css\";";
  check_import_rule
    ~expected:"@import \"narrow.css\" supports(display:flex) handheld;"
    "@import url(narrow.css) supports(display: flex) handheld;";
  check_import_rule
    ~expected:"@import \"narrow.css\" supports(display:flex) handheld;"
    "@import url(narrow.css) supports((display: flex)) handheld;";
  check_import_rule
    ~expected:
      "@import \"layout.css\" layer(framework.component) \
       supports(display:grid) screen and (min-width:30em);"
    "@import url(layout.css) layer(framework.component) supports(display: \
     grid) screen and (min-width: 30em);";
  check_import_rule ~expected:"@import \"bluish.css\" projection,tv;"
    "@import url(bluish.css) projection, tv;";
  neg_cursor read_import_rule "@import url(theme.css) supports();";
  neg_cursor read_import_rule "@import url(theme.css) supports(display);";
  neg_cursor read_import_rule "@import layer(default) url(theme.css);"

(* Not a roundtrip test *)
let c64_import_namespace_order () =
  (* CSS Cascade sections 2 and 6.4.4.2: empty @layer statements may appear
     before @import, but @layer rules must not be interleaved with consecutive
     @import/@namespace rules. Block @layer rules cannot be interleaved with
     @import rules. *)
  check_stylesheet
    ~expected:
      "@charset \"UTF-8\";@layer reset,theme;@import url(theme.css) \
       layer(theme);@namespace url(http://www.w3.org/1999/xhtml);"
    "@charset \"UTF-8\"; @layer reset, theme; @import url(theme.css) \
     layer(theme); @namespace url(http://www.w3.org/1999/xhtml);";
  neg_cursor read_stylesheet
    "@import url(default.css) layer(default); @layer theme; @import \
     url(components.css) layer(components);";
  neg_cursor read_stylesheet
    "@import url(default.css) layer(default); @layer theme { .x { color: red } \
     } @import url(components.css) layer(components);";
  neg_cursor read_stylesheet
    "@import url(default.css) layer(default); @layer theme; @namespace \
     url(http://www.w3.org/1999/xhtml);"

(* Not a roundtrip test *)
let c64_invalid_layer_names () =
  (* CSS Cascade section 6.4.2 reserves CSS-wide keywords in every layer-name
     segment, and the <layer-name> grammar has no empty segments. *)
  List.iter
    (fun keyword ->
      neg_cursor read_stylesheet ("@layer " ^ keyword ^ " { .x { color: red } }");
      neg_cursor read_stylesheet
        ("@layer framework." ^ keyword ^ " { .x { color: red } }"))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  neg_cursor read_stylesheet "@layer framework..theme { .x { color: red } }";
  neg_cursor read_stylesheet "@layer .framework { .x { color: red } }";
  neg_cursor read_stylesheet "@layer framework. { .x { color: red } }";
  neg_cursor read_stylesheet "@layer framework.theme. { .x { color: red } }";
  neg_cursor read_stylesheet "@layer InHeRiT { .x { color: red } }"

(* Not a roundtrip test *)
let c8_layer_api () =
  (* CSS Cascade section 8: CSSOM exposes the declared layer name on imports and
     layer block rules, and the declared name list on layer statement rules.
     Nested block rule names are the at-rule's own name, not parent-prefixed. *)
  let import_named =
    {
      url = "theme.css";
      layer = Some "framework.theme";
      supports = None;
      media = None;
    }
  in
  let import_anonymous =
    { url = "private.css"; layer = Some ""; supports = None; media = None }
  in
  let import_plain =
    { url = "plain.css"; layer = None; supports = None; media = None }
  in
  Alcotest.(check (option string))
    "named import layerName" (Some "framework.theme")
    (Css.Stylesheet.import_layer_name import_named);
  Alcotest.(check (option string))
    "anonymous import layerName is empty string" (Some "")
    (Css.Stylesheet.import_layer_name import_anonymous);
  Alcotest.(check (option string))
    "unlayered import layerName is null" None
    (Css.Stylesheet.import_layer_name import_plain);
  Alcotest.(check (option string))
    "named layer block API name" (Some "framework.theme")
    (Css.Stylesheet.layer_block_name
       (Css.Stylesheet.Layer (Some "framework.theme", [])));
  Alcotest.(check (option string))
    "anonymous layer block API name is empty string" (Some "")
    (Css.Stylesheet.layer_block_name (Css.Stylesheet.Layer (None, [])));
  (match
     Css.Stylesheet.Layer
       (Some "outer", [ Css.Stylesheet.Layer (Some "foo.bar", []) ])
   with
  | Css.Stylesheet.Layer (_, [ inner ]) ->
      Alcotest.(check (option string))
        "inner layer block API name is not parent-prefixed" (Some "foo.bar")
        (Css.Stylesheet.layer_block_name inner)
  | _ -> Alcotest.fail "expected nested layer block");
  Alcotest.(check (option (list string)))
    "layer statement API nameList"
    (Some [ "reset"; "framework.theme"; "components" ])
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer_decl [ "reset"; "framework.theme"; "components" ]));
  Alcotest.(check (option (list string)))
    "non-statement layer has no nameList" None
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer (Some "reset", [])))

(* Not a roundtrip test *)
let c41_declared_values () =
  (* CSS Cascade section 4.1: each property declaration applied to an element
     contributes a declared value for that element/property. The library can
     expose those declaration-level values without resolving selector
     matching. *)
  let declarations =
    [
      Css.Declaration.color (Css.Values.hex "#ff0000");
      Css.Declaration.margin [ Css.Values.Px 1. ];
      Css.Declaration.important
        (Css.Declaration.color (Css.Values.hex "#0000ff"));
      Css.Declaration.custom_property "--accent" "currentColor";
    ]
  in
  let declared = Css.Stylesheet.declared_values declarations in
  let color_declared =
    Css.Stylesheet.declared_values ~property:"color" declarations
  in
  let declared_property (d : Css.Stylesheet.declared_value) = d.property in
  let declared_value (d : Css.Stylesheet.declared_value) = d.value in
  let declared_important (d : Css.Stylesheet.declared_value) = d.important in
  let declared_source_order (d : Css.Stylesheet.declared_value) =
    d.source_order
  in
  Alcotest.(check (list string))
    "declared values preserve every declaration property"
    [ "color"; "margin"; "color"; "--accent" ]
    (List.map declared_property declared);
  Alcotest.(check (list string))
    "declared values expose serialized values"
    [ "#ff0000"; "1px"; "#0000ff"; "currentColor" ]
    (List.map declared_value declared);
  Alcotest.(check (list int))
    "declared values preserve source order" [ 0; 1; 2; 3 ]
    (List.map declared_source_order declared);
  Alcotest.(check (list string))
    "declared value filtering selects one property" [ "#ff0000"; "#0000ff" ]
    (List.map declared_value color_declared);
  Alcotest.(check (list bool))
    "declared values preserve importance for cascade sorting" [ false; true ]
    (List.map declared_important color_declared)

(* Not a roundtrip test *)
let c42_cascaded_values () =
  (* CSS Cascade section 4.2: after cascade sorting there is at most one
     cascaded value per property. No matching declaration means no cascaded
     value. *)
  let candidate origin important source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important; source_order; value }
  in
  Alcotest.(check (option string))
    "empty candidate set has no cascaded value" None
    (Css.Stylesheet.cascaded_value []);
  Alcotest.(check (option string))
    "highest origin/importance candidate becomes the cascaded value"
    (Some "author-important")
    (Css.Stylesheet.cascaded_value
       [
         candidate User_agent false 0 "ua-normal";
         candidate User false 1 "user-normal";
         candidate Author false 2 "author-normal";
         candidate Author true 3 "author-important";
       ]);
  Alcotest.(check (option string))
    "source order breaks ties within one origin/importance bucket"
    (Some "later")
    (Css.Stylesheet.cascaded_value
       [
         candidate Author false 10 "earlier"; candidate Author false 11 "later";
       ])

let spec_cascade_origin_importance_order () =
  (* CSS Cascade origin/importance order, including generated animation and
     transition origins. Larger rank wins in the helper API. *)
  let rank origin important =
    Css.Stylesheet.origin_importance_rank ~important origin
  in
  let open Css.Stylesheet in
  Alcotest.(check (list int))
    "normal and important origin ranks"
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    [
      rank User_agent false;
      rank User false;
      rank Author_presentational_hint false;
      rank Author false;
      rank Animation false;
      rank Author true;
      rank User true;
      rank User_agent true;
      rank Transition false;
    ];
  let candidate origin important source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important; source_order; value }
  in
  Alcotest.(check (option string))
    "transition beats important user-agent" (Some "transition")
    (Css.Stylesheet.cascaded_value
       [
         candidate User_agent true 0 "ua-important";
         candidate Transition false 1 "transition";
       ]);
  Alcotest.(check (option string))
    "important user beats important author" (Some "user-important")
    (Css.Stylesheet.cascaded_value
       [
         candidate Author true 0 "author-important";
         candidate User true 1 "user-important";
       ])

let specified_source_name = function
  | Css.Stylesheet.Cascaded -> "cascaded"
  | Initial_default -> "initial-default"
  | Inherited_default -> "inherited-default"
  | Initial_keyword -> "initial-keyword"
  | Inherit_keyword -> "inherit-keyword"
  | Unset_initial -> "unset-initial"
  | Unset_inherited -> "unset-inherited"

let check_specified name expected_value expected_source actual =
  Alcotest.(check string)
    (name ^ " value") expected_value actual.specified_value;
  Alcotest.(check string)
    (name ^ " source") expected_source
    (specified_source_name actual.specified_value_source)

(* Not a roundtrip test *)
let c43_specified_values () =
  (* CSS Cascade section 4.3: defaulting guarantees a specified value exists for
     every property. CSS-wide keywords are handled before computed values. *)
  check_specified "normal cascaded value" "block" "cascaded"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"inline"
       ~inherited:None ~cascaded:(Some "block"));
  check_specified "missing non-inherited property" "auto" "initial-default"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"auto"
       ~inherited:None ~cascaded:None);
  check_specified "missing inherited property" "blue" "inherited-default"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"black"
       ~inherited:(Some "blue") ~cascaded:None);
  check_specified "initial keyword" "medium" "initial-keyword"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"medium"
       ~inherited:(Some "large") ~cascaded:(Some "initial"));
  check_specified "inherit keyword" "4.2px" "inherit-keyword"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "inherit keyword on root" "medium" "inherit-keyword"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"medium"
       ~inherited:None ~cascaded:(Some "inherit"));
  check_specified "unset on inherited property" "inside" "unset-inherited"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "unset"));
  check_specified "unset on non-inherited property" "auto" "unset-initial"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"auto"
       ~inherited:(Some "80px") ~cascaded:(Some "unset"))

(* Not a roundtrip test *)
let c448_value_stage_scope () =
  (* Later value stages need context supplied by a caller or platform; the
     parser itself only models the local ordering of stages. *)
  Alcotest.(check (list bool))
    "declared/cascaded/specified are library-local; later stages need context"
    [ false; false; false; true; true; true ]
    (List.map Css.Stylesheet.value_processing_requires_document_context
       [
         Css.Stylesheet.Declared_value;
         Cascaded_value;
         Specified_value;
         Computed_value;
         Used_value;
         Actual_value;
       ])

(* Not a roundtrip test *)
let c42_integrated_order () =
  (* CSS Cascade section 4.2 consumes the sorted cascade output. This helper
     covers the full ordering criteria available without DOM matching:
     origin/importance, layer, specificity, scope proximity, and source
     order. *)
  let candidate origin important layer specificity scope_hops source_order value
      : Css.Stylesheet.cascade_candidate =
    {
      candidate_origin = origin;
      candidate_important = important;
      candidate_layer = layer;
      candidate_specificity = specificity;
      candidate_scope_hops = scope_hops;
      candidate_source_order = source_order;
      candidate_value = value;
    }
  in
  let winner_value candidates =
    Css.Stylesheet.winning_cascade_candidate
      ~layer_order:[ "reset"; "theme"; "utilities" ]
      candidates
    |> Option.map (fun (c : Css.Stylesheet.cascade_candidate) ->
        c.candidate_value)
  in
  Alcotest.(check (option string))
    "important user origin beats important author origin"
    (Some "user-important")
    (winner_value
       [
         candidate Author true None 1 None 0 "author-important";
         candidate User true None 1 None 1 "user-important";
       ]);
  Alcotest.(check (option string))
    "normal unlayered beats explicit layers" (Some "unlayered")
    (winner_value
       [
         candidate Author false (Some "utilities") 1 None 2 "utilities";
         candidate Author false None 1 None 1 "unlayered";
       ]);
  Alcotest.(check (option string))
    "specificity beats later source order" (Some "id-selector")
    (winner_value
       [
         candidate Author false None 10 None 2 "later-class";
         candidate Author false None 100 None 1 "id-selector";
       ]);
  Alcotest.(check (option string))
    "closer scope beats farther scope" (Some "near-scope")
    (winner_value
       [
         candidate Author false None 10 (Some 4) 1 "far-scope";
         candidate Author false None 10 (Some 1) 0 "near-scope";
       ]);
  Alcotest.(check (option string))
    "source order breaks final ties" (Some "later")
    (winner_value
       [
         candidate Author false None 10 None 1 "earlier";
         candidate Author false None 10 None 2 "later";
       ])

(* Not a roundtrip test *)
let c43_revert_values () =
  (* CSS Cascade sections 4.3 and 7.3.4-7.3.5: [revert] and [revert-layer]
     resolve by rolling back the candidate set, then defaulting if no lower
     candidate remains. *)
  let origin_candidate origin source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important = false; source_order; value }
  in
  check_specified "revert rolls back to user origin" "blue" "cascaded"
    (Css.Stylesheet.specified_value_after_revert ~inherits:true ~initial:"black"
       ~inherited:(Some "purple")
       [
         origin_candidate User_agent 0 "black";
         origin_candidate User 1 "blue";
         origin_candidate Author 2 "revert";
       ]);
  check_specified "revert with no previous origin defaults" "black"
    "initial-default"
    (Css.Stylesheet.specified_value_after_revert ~inherits:false
       ~initial:"black" ~inherited:None
       [ origin_candidate User_agent 0 "revert" ]);
  let layer_candidate layer source_order value :
      Css.Stylesheet.cascade_layer_candidate =
    { layer; important = false; source_order; value }
  in
  check_specified "revert-layer rolls back to lower layer" "green" "cascaded"
    (Css.Stylesheet.specified_value_after_revert_layer ~inherits:false
       ~initial:"transparent" ~inherited:None ~layer_order:[ "base"; "theme" ]
       [
         layer_candidate (Some "base") 0 "green";
         layer_candidate (Some "theme") 1 "revert-layer";
       ]);
  check_specified "revert-layer with no lower layer defaults" "transparent"
    "initial-default"
    (Css.Stylesheet.specified_value_after_revert_layer ~inherits:false
       ~initial:"transparent" ~inherited:None ~layer_order:[ "base"; "theme" ]
       [ layer_candidate (Some "base") 0 "revert-layer" ])

(* Not a roundtrip test *)
let c47_examples () =
  (* CSS Cascade section 4.7 examples this library can model from CSS text. *)
  check_specified "border width inherit example" "4.2px" "inherit-keyword"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "width missing declaration example" "auto" "initial-default"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"auto"
       ~inherited:None ~cascaded:None);
  check_specified "list-style-position inherit example" "inside"
    "inherit-keyword"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "inherit"));
  check_specified "list-style-position initial example" "outside"
    "initial-keyword"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "initial"))

let dom_selector_boundary () =
  (* Selector matching is DOM context, but selector syntax is CSS-file
     surface. *)
  let selector_cases =
    [
      ".card";
      "article.card > h2:first-child";
      ":scope > .item";
      ".card:has(> img[alt])";
      "a:visited";
      "::part(label)";
      ":host(.active) .title";
    ]
  in
  List.iter
    (fun selector -> ignore (Css.Selector.of_string selector))
    selector_cases;
  neg_cursor read_stylesheet ".card:has(> ) { color: red }";
  neg_cursor read_stylesheet "::before::after { color: red }";
  neg_cursor read_stylesheet ":host-context() { color: red }"

let fetch_url_boundary () =
  (* @import and url(...) syntax is in scope; loading/resolution is not. *)
  let import_cases =
    [
      ( "@import url(base.css) layer(reset) supports(display: grid) screen;",
        "base.css",
        Some "reset" );
      ("@import \"print.css\" print;", "print.css", None);
      ("@import url(theme.css) layer();", "theme.css", Some "");
      ( "@import url(theme.css) layer(theme) supports(selector(:has(img))) \
         screen and (width >= 40em);",
        "theme.css",
        Some "theme" );
      ( "@import url(\"../fonts/brand.woff2\") layer(fonts);",
        "../fonts/brand.woff2",
        Some "fonts" );
    ]
  in
  List.iter
    (fun (input, url, layer) ->
      let r = Css.Cursor.of_string input in
      let rule = Css.Stylesheet.read_import_rule r in
      Alcotest.(check string) "import url" url rule.url;
      Alcotest.(check (option string)) "import layer" layer rule.layer)
    import_cases;
  check_declaration ~expected:"background-image:url(../img/logo.svg)"
    "background-image: url(../img/logo.svg);";
  check_declaration ~expected:"cursor:url(cursor.cur),auto"
    "cursor: url(cursor.cur), auto";
  check_declaration ~expected:"src:url(brand.woff2) format(woff2)"
    "src: url(brand.woff2) format(woff2)";
  check_import_rule ~expected:"@import \"theme.css\" supports(display:);"
    "@import url(theme.css) supports(display:);";
  neg_cursor read_import_rule "@import url(theme.css) layer(theme) layer(base);";
  neg_cursor read_import_rule
    "@import url(theme.css) screen supports(display: grid);"

let environment_query_boundary () =
  (* Query syntax is in scope; matching needs explicit environment context. *)
  check_stylesheet
    ~expected:
      "@media (width >= 40em){@supports (display:grid){@container card \
       style(--theme: dark){.card{display:grid}}}}"
    "@media (width >= 40em) { @supports (display: grid) { @container card \
     style(--theme: dark) { .card { display: grid } } } }";
  check_stylesheet ~expected:"@supports (display:){.x{color:red}}"
    "@supports (display:) { .x { color: red } }";
  neg_cursor read_stylesheet "@media (width >= ) { .x { color: red } }";
  neg_cursor read_stylesheet "@container card style() { .x { color: red } }";
  neg_cursor read_stylesheet "@container card (width >) { .x { color: red } }"

let value_resolution_boundary () =
  let open Css.Values in
  let inherited_font_size = Css.Declaration.of_string "font-size: 16px" in
  let inherited_color = Css.Declaration.of_string "color: canvastext" in
  let initial_width = Css.Declaration.of_string "width: auto" in
  let initial_display = Css.Declaration.of_string "display: inline" in
  let ctx =
    {
      Css.Context.empty with
      inherited_values = [ inherited_font_size; inherited_color ];
      initial_values = [ initial_width; initial_display ];
      base_url = Some "https://example.test/css/app.css";
      root_font_size = Some (Px 16.);
      parent_font_size = Some (Px 16.);
      current_color = Some (System Canvas_text);
      viewport_width = Some (Px 1024.);
      viewport_height = Some (Px 768.);
      container_width = Some (Px 640.);
      container_height = Some (Px 480.);
    }
  in
  Alcotest.(check (option decl_t))
    "inherited font size" (Some inherited_font_size)
    (Css.Context.inherited_value "font-size" ctx);
  Alcotest.(check (option decl_t))
    "initial width" (Some initial_width)
    (Css.Context.initial_value "width" ctx);
  check_specified "inherit fallback before computed stage" "16px"
    "inherit-keyword"
    (Css.Stylesheet.specified_value ~inherits:false ~initial:"medium"
       ~inherited:(Some "16px") ~cascaded:(Some "inherit"));
  check_specified "unset chooses inherited before computed stage" "canvastext"
    "unset-inherited"
    (Css.Stylesheet.specified_value ~inherits:true ~initial:"black"
       ~inherited:(Some "canvastext") ~cascaded:(Some "unset"))

let custom_property_boundary () =
  let check_specified_value name css expected =
    let decl = Css.Declaration.of_string css in
    Alcotest.(check string)
      name expected
      (Css.Declaration.string_of_value ~minify:false decl)
  in
  let check_minified_value name css expected =
    let decl = Css.Declaration.of_string css in
    Alcotest.(check string)
      name expected
      (Css.Declaration.string_of_value ~minify:true decl)
  in
  let gap = Css.Declaration.of_string "--gap: 1rem" in
  let ctx =
    {
      Css.Context.empty with
      custom_properties =
        [
          gap;
          Css.Declaration.of_string "--a: var(--b)";
          Css.Declaration.of_string "--b: var(--a, red)";
        ];
    }
  in
  Alcotest.(check (option decl_t))
    "custom property context" (Some gap)
    (Css.Context.custom_property "--gap" ctx);
  check_declaration ~expected:"--a:var(--b)" "--a: var(--b);";
  (* CSS Custom Properties requires specified custom-property values to
     serialize as authored. Assert the value serialization directly so this spec
     boundary does not depend on declaration-colon formatting. *)
  check_specified_value "custom property var fallback" "--b: var(--a, red);"
    "var(--a, red)";
  check_specified_value "var fallback in ordinary declaration"
    "color: var(--brand, red);" "var(--brand, red)";
  (* Minification may remove whitespace around the fallback comma while
     preserving the var() grammar. Keep this explicit and separate from the
     specified-value serialization checks above. *)
  check_minified_value "custom property var fallback minified"
    "--b: var(--a, red);" "var(--a,red)";
  check_minified_value "nested var fallback minified"
    "--nested: var(--a, var(--b, red));" "var(--a,var(--b,red))";
  check_declaration ~expected:"--:var(--x)" "--: var(--x);";
  check_declaration ~expected:"--tokens:{color:red}" "--tokens: { color: red };";
  check_declaration ~expected:"--empty:var(--missing,)"
    "--empty: var(--missing,);";
  check_specified_value "nested var fallback"
    "--nested: var(--a, var(--b, red));" "var(--a, var(--b, red))";
  neg_cursor read_stylesheet
    "@property --registered { syntax: \"<color>\"; inherits: false; \
     initial-value: 10px }"

let spec_current_at_rules () =
  check_stylesheet ~expected:"@media (dynamic-range:high){.photo{color:red}}"
    "@media (dynamic-range: high) { .photo { color: red } }";
  check_stylesheet
    ~expected:"@media (prefers-reduced-data:reduce){.hero{display:none}}"
    "@media (prefers-reduced-data: reduce) { .hero { display: none } }";
  check_stylesheet
    ~expected:"@supports selector(:has(img)){.card{display:block}}"
    "@supports selector(:has(img)) { .card { display: block } }";
  check_stylesheet
    ~expected:".card{color:red;@media (width >= 40em){&>img{display:block}}}"
    ".card { color: red; @media (width >= 40em) { & > img { display: block } } \
     }";
  check_stylesheet ~expected:"@scope(.card) to (.footer){.title{color:red}}"
    "@scope (.card) to (.footer) { .title { color: red } }";
  check_stylesheet
    ~expected:
      "@font-palette-values \
       --brand{font-family:Brand;base-palette:1;override-colors:0 red}"
    "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
     override-colors: 0 red; }";
  check_stylesheet
    ~expected:
      "@font-face {font-family:ColorFont;src:url(color.woff2) \
       tech(color-COLRv1);font-tech:color-COLRv1}"
    "@font-face { font-family: ColorFont; src: url(color.woff2) \
     tech(color-COLRv1); font-tech: color-COLRv1; }";
  check_stylesheet ~expected:"@view-transition{navigation:auto}"
    "@view-transition { navigation: auto; }";
  check_stylesheet
    ~expected:"@position-try --below{top:anchor(bottom);left:anchor(center)}"
    "@position-try --below { top: anchor(bottom); left: anchor(center); }";
  check_stylesheet
    ~expected:
      "@container card style(--variant: \
       featured){.card{view-transition-name:card}}"
    "@container card style(--variant: featured) { .card { \
     view-transition-name: card } }";
  check_stylesheet
    ~expected:"@container style(--variant: featured){.card{color:red}}"
    "@container style(--variant: featured) { .card { color: red } }";
  check_stylesheet
    ~expected:"@container scroll-state(stuck: top){.card{color:red}}"
    "@container scroll-state(stuck: top) { .card { color: red } }";
  neg_cursor read_stylesheet "@container style() { .card { color: red } }";
  neg_cursor read_stylesheet
    "@container scroll-state() { .card { color: red } }";
  check_stylesheet
    ~expected:
      "@container (30em <= inline-size < 60em){@supports \
       (display:grid){.grid{display:grid}}}"
    "@container (30em <= inline-size < 60em) { @supports (display: grid) { \
     .grid { display: grid } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog{opacity:0;translate:0 1rem}}"
    "@starting-style { .dialog { opacity: 0; translate: 0 1rem } }";
  check_stylesheet ~expected:"@page chapter:left{margin:2cm}"
    "@page chapter:left { margin: 2cm }";
  neg_cursor read_stylesheet "@media (width >) { .x { color: red } }";
  neg_cursor read_stylesheet "@supports selector() { .x { color: red } }";
  neg_cursor read_stylesheet "@scope (.card) .title { color: red }";
  neg_cursor read_stylesheet "@font-palette-values { base-palette: 1; }";
  neg_cursor read_stylesheet "@position-try default { top: 0; }";
  neg_cursor read_stylesheet "@container () { .x { color: red } }";
  neg_cursor read_stylesheet "@page : { margin: 1cm }"

let font_palette_values_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@font-palette-values \
         --brand{font-family:Brand;base-palette:1;override-colors:0 red,1 \
         color(display-p3 1 0 0)}",
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         override-colors: 0 red, 1 color(display-p3 1 0 0); }" );
      ( "@font-palette-values --dark{font-family:\"Color \
         Font\",Brand;base-palette:dark}",
        "@font-palette-values --dark { font-family: \"Color Font\", Brand; \
         base-palette: dark; }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@font-palette-values brand { font-family: Brand; base-palette: 1 }";
      "@font-palette-values --brand;";
      "@font-palette-values --brand { font-family: Brand; override-colors: -1 \
       red }";
    ]

let spec_view_transition_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@view-transition{navigation:auto}",
        "@view-transition { navigation: auto; }" );
      ( "@view-transition{navigation:none}",
        "@view-transition { navigation: none; }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@view-transition page { navigation: auto; }";
      "@view-transition;";
      "@view-transition { navigation: always; }";
    ]

let spec_position_try_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@position-try \
         --below{top:anchor(bottom);left:anchor(center);width:anchor-size(width)}",
        "@position-try --below { top: anchor(bottom); left: anchor(center); \
         width: anchor-size(width); }" );
      ( "@position-try \
         --inline-start{inset-inline-end:anchor(start);margin-inline:1rem}",
        "@position-try --inline-start { inset-inline-end: anchor(start); \
         margin-inline: 1rem; }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@position-try default { top: 0; }";
      "@position-try --fallback;";
      "@position-try --fallback { @media screen { .x { color: red } } }";
    ]

let spec_at_rule_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@property --accent{syntax:\"<color>\";inherits:true;initial-value:red}",
        "@property --accent { initial-value: red; inherits: true; syntax: \
         \"<color>\" }" );
      ( "@property --dup{syntax:\"*\";inherits:false}",
        "@property --dup { syntax: \"<length>\"; inherits: true; syntax: \
         \"*\"; inherits: false }" );
      ( "@font-face {font-weight:100 \
         900;font-display:swap;src:url(brand.woff2);font-family:Brand}",
        "@font-face { font-weight: 100 900; font-display: swap; src: \
         url(brand.woff2); font-family: Brand }" );
      ( "@page invoice:first{margin:1cm;size:A4;@top-left{content:\"Invoice\"}}",
        "@page invoice:first { margin: 1cm; size: A4; @top-left { content: \
         \"Invoice\" } }" );
      ( "@font-palette-values \
         --brand{font-family:Brand;base-palette:2;override-colors:0 red}",
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         base-palette: 2; override-colors: 0 red }" );
      ( "@view-transition{navigation:none}",
        "@view-transition { navigation: auto; navigation: none }" );
      ( "@position-try --below{top:anchor(bottom);left:anchor(center)}",
        "@position-try --below { left: anchor(center); top: anchor(bottom) }" );
      ( "@media screen and (width >= 40em){.card{display:grid}}",
        "@media screen and (width >= 40em) { .card { display: grid } }" );
      ( "@supports ((display:grid) and selector(:has(img))){.card{display:grid}}",
        "@supports ((display: grid) and selector(:has(img))) { .card { \
         display: grid } }" );
      ( "@container card style(--variant: featured){.card{color:red}}",
        "@container card style(--variant: featured) { .card { color: red } }" );
      ( "@scope(.card) to (.boundary){.title{color:red}}",
        "@scope (.card) to (.boundary) { .title { color: red } }" );
      ( "@starting-style{.dialog{opacity:0}}",
        "@starting-style { .dialog { opacity: 0 } }" );
    ];
  List.iter
    (neg_cursor read_stylesheet)
    [
      "@property --bad { syntax: \"<length>\"; inherits: false }";
      "@property --bad { syntax: \"<length>\"; inherits: false; initial-value: \
       red }";
      "@font-face { font-family: Brand; src: url(brand.woff2); @media screen { \
       .x { color: red } } }";
      "@font-palette-values --brand { font-family: Brand; @media screen { .x { \
       color: red } } }";
      "@view-transition { @media screen { .x { color: red } } }";
      "@position-try --below { @supports (display: grid) { .x { color: red } } \
       }";
      "@page { @top-center { display: block } }";
      "@keyframes fade { @media screen { opacity: 1 } }";
      "@media screen;";
      "@media screen and or (width) { .x { color: red } }";
      "@supports (display: grid) and (gap: 1rem) or (color: red) { .x { color: \
       red } }";
      "@container card style() { .x { color: red } }";
      "@scope () { .x { color: red } }";
      "@scope (.x) to () { .x { color: red } }";
      "@starting-style;";
    ]

let spec_at_rule_inventory_matrix () =
  let module A = Cascade_spec_inventory.At_rule_grammar in
  List.iter
    (fun (row : A.row) -> check_stylesheet ~expected:row.expected row.input)
    A.positive;
  List.iter
    (fun (row : A.invalid_row) -> neg_cursor read_stylesheet row.input)
    A.negative;
  let positive_features = A.features A.positive in
  let negative_features =
    A.negative
    |> List.map (fun (row : A.invalid_row) -> row.feature)
    |> List.sort_uniq String.compare
  in
  Alcotest.(check (list string))
    "at-rule inventory positive/negative feature parity" positive_features
    negative_features;
  List.iter
    (fun feature ->
      let positive_branches =
        List.filter (fun (row : A.row) -> row.feature = feature) A.positive
      in
      let negative_branches =
        List.filter
          (fun (row : A.invalid_row) -> row.feature = feature)
          A.negative
      in
      if positive_branches = [] then
        Alcotest.failf "%s has no positive at-rule inventory rows" feature;
      if negative_branches = [] then
        Alcotest.failf "%s has no negative at-rule inventory rows" feature)
    positive_features

(* ignore-test *)
let test_spec_snapshot_tracking_vectors () =
  (* Snapshot tracking vectors span stable cross-module syntax from recent CSS
     snapshots. The matrix tracks exact snapshot membership; these tests make
     historical snapshot coverage visible in the normal stylesheet suite. *)
  check_stylesheet
    ~expected:
      "@layer reset,base,components;@layer \
       components{.card{display:grid;gap:1rem}}"
    "@layer reset, base, components; @layer components { .card { display: \
     grid; gap: 1rem } }";
  check_stylesheet
    ~expected:
      "@container card (inline-size > \
       30em){.card{grid-template-columns:subgrid}}"
    "@container card (inline-size > 30em) { .card { grid-template-columns: \
     subgrid } }";
  check_stylesheet
    ~expected:
      "@supports (color:oklch(50% .1 20)){.accent{color:oklch(50% .1 20)}}"
    "@supports (color: oklch(50% 0.1 20)) { .accent { color: oklch(50% 0.1 20) \
     } }";
  check_stylesheet
    ~expected:
      ".card{color:var(--fg);@media \
       (prefers-color-scheme:dark){&{color:white}}}"
    ".card { color: var(--fg); @media (prefers-color-scheme: dark) { & { \
     color: white } } }";
  neg_cursor read_stylesheet "@layer reset,,base;";
  neg_cursor read_stylesheet "@container card () { .card { color: red } }";
  neg_cursor read_stylesheet "@supports () { .accent { color: red } }"

(* ignore-test *)
let test_snapshot_membership_matrix () =
  let module S = Cascade_spec_inventory.Css_snapshot in
  let keys rows = List.map S.key rows |> List.sort String.compare in
  let snapshot_2026 =
    [
      "CSS Animations@1/2";
      "CSS Backgrounds and Borders@3";
      "CSS Box Alignment@3";
      "CSS Box Model@3";
      "CSS Cascading and Inheritance@5";
      "CSS Color@3";
      "CSS Color@4";
      "CSS Containment@3";
      "CSS Custom Properties@1";
      "CSS Display@3";
      "CSS Flexible Box Layout@1";
      "CSS Fonts@4";
      "CSS Grid Layout@1/2";
      "CSS Logical Properties@1";
      "CSS Overflow@3/4";
      "CSS Positioned Layout@3";
      "CSS Pseudo-Elements@4";
      "CSS Scroll Snap@1";
      "CSS Sizing@3";
      "CSS Syntax@3";
      "CSS Transforms@1/2";
      "CSS Transitions@1/2";
      "Conditional Rules@3";
      "Media Queries@4";
      "Selectors@4";
      "Values and Units@3";
    ]
  in
  let current_work =
    [
      "CSS Cascading and Inheritance@6";
      "CSS Fonts@5";
      "CSS Nesting@1";
      "CSS Properties and Values API@1";
      "Conditional Rules@4";
      "Media Queries@5";
      "Values and Units@4";
    ]
  in
  let experimental = [ "CSS Color@5"; "Values and Units@5" ] in
  Alcotest.(check (list string))
    "CSS Snapshot 2026 module membership" snapshot_2026
    (keys (S.by_baseline S.Snapshot_2026));
  Alcotest.(check (list string))
    "current-work module membership" current_work
    (keys (S.by_baseline S.Current_work));
  Alcotest.(check (list string))
    "experimental module membership" experimental
    (keys (S.by_baseline S.Experimental));
  List.iter
    (fun row ->
      if row.S.tests = [] then
        Alcotest.failf "%s has no deterministic test surface" (S.key row);
      if row.S.fuzzers = [] then
        Alcotest.failf "%s has no fuzz surface" (S.key row))
    S.rows

(** {2 CSS Nesting Round-trip Tests} *)

(** Helper: parse CSS, print minified, compare to expected *)
let test_nesting_roundtrip ~expected input =
  let r = Css.Cursor.of_string input in
  try
    let stylesheet = Css.Stylesheet.read r in
    let roundtrip =
      String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
    in
    Alcotest.(check string)
      ("nesting roundtrip for " ^ input)
      expected roundtrip
  with Css.Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Css.Error.to_string err)

(** Helper: parse CSS, print minified, parse again, print again -- verify
    idempotent *)
let test_nesting_idempotent input =
  let r = Css.Cursor.of_string input in
  try
    let sheet1 = Css.Stylesheet.read r in
    let printed1 = String.trim (Css.Stylesheet.to_string ~minify:true sheet1) in
    let r2 = Css.Cursor.of_string printed1 in
    let sheet2 = Css.Stylesheet.read r2 in
    let printed2 = String.trim (Css.Stylesheet.to_string ~minify:true sheet2) in
    Alcotest.(check string)
      ("nesting idempotent for " ^ input)
      printed1 printed2
  with Css.Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Css.Error.to_string err)

(* ignore-test *)
let test_nesting_basic () =
  (* Basic nesting with & descendant combinator *)
  test_nesting_roundtrip ~expected:".parent{color:red;& .child{color:blue}}"
    ".parent { color: red; & .child { color: blue; } }";
  test_nesting_idempotent ".parent { color: red; & .child { color: blue; } }"

(* ignore-test *)
let test_nesting_ampersand_hover () =
  (* Ampersand with pseudo-class *)
  test_nesting_roundtrip ~expected:".btn{color:red;&:hover{color:blue}}"
    ".btn { color: red; &:hover { color: blue; } }";
  test_nesting_idempotent ".btn { color: red; &:hover { color: blue; } }"

(* ignore-test *)
let test_nesting_multiple () =
  (* Multiple nested rules *)
  test_nesting_roundtrip
    ~expected:
      ".card{padding:1rem;& .title{font-size:1.5rem}& .body{font-size:1rem}}"
    ".card { padding: 1rem; & .title { font-size: 1.5rem; } & .body { \
     font-size: 1rem; } }";
  test_nesting_idempotent
    ".card { padding: 1rem; & .title { font-size: 1.5rem; } & .body { \
     font-size: 1rem; } }"

(* ignore-test *)
let test_nesting_media () =
  (* Nested @media query inside a rule *)
  test_nesting_roundtrip
    ~expected:".foo{color:red;@media (min-width:768px){color:blue;}}"
    ".foo { color: red; @media (min-width: 768px) { color: blue; } }";
  test_nesting_idempotent
    ".foo { color: red; @media (min-width: 768px) { color: blue; } }"

(* ignore-test *)
let test_nesting_deep () =
  (* Deeply nested rules *)
  test_nesting_roundtrip ~expected:".a{& .b{& .c{color:red}}}"
    ".a { & .b { & .c { color: red; } } }";
  test_nesting_idempotent ".a { & .b { & .c { color: red; } } }"

(* ignore-test *)
let test_nesting_with_declarations () =
  (* Nested rule after multiple declarations *)
  test_nesting_roundtrip
    ~expected:".parent{color:red;padding:1rem;&:focus{outline:none}}"
    ".parent { color: red; padding: 1rem; &:focus { outline: none; } }";
  test_nesting_idempotent
    ".parent { color: red; padding: 1rem; &:focus { outline: none; } }"

(* ignore-test *)
let test_nesting_check_stylesheet () =
  (* Also test via check_stylesheet for consistency *)
  check_stylesheet ~expected:".parent{color:red;& .child{color:blue}}"
    ".parent { color: red; & .child { color: blue; } }";
  check_stylesheet ~expected:".btn{color:red;&:hover{color:blue}}"
    ".btn { color: red; &:hover { color: blue; } }";
  check_stylesheet ~expected:".a{& .b{& .c{color:red}}}"
    ".a { & .b { & .c { color: red; } } }"

let spec_nesting_selector_edges () =
  check_stylesheet
    ~expected:
      ".card{color:red;&:is(:hover,:focus-visible){color:blue}&:has(>img){display:grid}}"
    ".card { color: red; &:is(:hover, :focus-visible) { color: blue } &:has(> \
     img) { display: grid } }";
  check_stylesheet
    ~expected:
      ".card{@supports selector(:has(img)){&:has(img){display:grid}}@container \
       (inline-size > 30em){&>.media{display:block}}}"
    ".card { @supports selector(:has(img)) { &:has(img) { display: grid } } \
     @container (inline-size > 30em) { & > .media { display: block } } }";
  check_stylesheet
    ~expected:
      "@scope(.card) to (.boundary){.title{color:red;&:hover{color:blue}}}"
    "@scope (.card) to (.boundary) { .title { color: red; &:hover { color: \
     blue } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog[open]{opacity:0;transform:scale(.95)}}"
    "@starting-style { .dialog[open] { opacity: 0; transform: scale(0.95) } }";
  neg_cursor read_stylesheet ".card { & { & { color: red } } }";
  neg_cursor read_stylesheet "@scope () { .x { color: red } }";
  neg_cursor read_stylesheet "@scope (.x) to () { .x { color: red } }";
  neg_cursor read_stylesheet "@starting-style;"

let additional_tests =
  [
    ("check function", `Quick, test_check);
    ("import_rule", `Quick, test_import_rule);
    ("config", `Quick, test_config);
    (* Positive tests *)
    ("advanced selectors", `Quick, test_advanced_selectors);
    ("advanced properties", `Quick, test_advanced_properties);
    ("complex values", `Quick, test_complex_values);
    ("nested rules", `Quick, test_nested_rules);
    ("spec section 7.1 block grammar examples", `Quick, spec_s7_block_examples);
    ("spec section 8.1-8.2 stylesheet rule shapes", `Quick, spec_s8_rule_shapes);
    ("spec namespace prefix serialization", `Quick, spec_namespace_serialization);
    ("spec section 8.3 charset is not a rule", `Quick, spec_s8_charset_not_rule);
    ( "spec CSS Syntax structural recovery",
      `Quick,
      css_syntax_recovery_structural );
    (* CSS nesting round-trip tests *)
    ("nesting basic", `Quick, test_nesting_basic);
    ("nesting ampersand hover", `Quick, test_nesting_ampersand_hover);
    ("nesting multiple nested rules", `Quick, test_nesting_multiple);
    ("nesting nested media query", `Quick, test_nesting_media);
    ("nesting deeply nested", `Quick, test_nesting_deep);
    ("nesting with declarations", `Quick, test_nesting_with_declarations);
    ("nesting check_stylesheet", `Quick, test_nesting_check_stylesheet);
    ( "spec nesting selector and conditional edges",
      `Quick,
      spec_nesting_selector_edges );
    (* Negative tests *)
    ("invalid selectors", `Quick, test_invalid_selectors);
    ("invalid properties", `Quick, test_invalid_properties);
    ("invalid syntax", `Quick, test_invalid_syntax);
    ("invalid at-rules", `Quick, test_invalid_at_rules);
    ("spec CSS Syntax recovery", `Quick, css_syntax_recovery);
    ("invalid functions", `Quick, test_invalid_functions);
    ("layer roundtrip", `Quick, test_layer_roundtrip);
    ("spec cascade 6.4 layer name syntax", `Quick, c64_layer_name_syntax);
    ( "spec cascade 6.4 layer nesting examples",
      `Quick,
      c64_layer_nesting_examples );
    ("spec cascade 6.4 layer statement edges", `Quick, c64_layer_statement_edges);
    ("spec cascade 6.4 anonymous layer edges", `Quick, c64_anonymous_layer_edges);
    ("spec cascade 6.4 import layer syntax", `Quick, c64_import_layer_syntax);
    ("spec cascade 2 import conditions", `Quick, c2_import_conditions);
    ( "spec cascade 6.4 import namespace ordering",
      `Quick,
      c64_import_namespace_order );
    ("spec cascade 6.4 invalid layer names", `Quick, c64_invalid_layer_names);
    ("spec cascade 8 layer api", `Quick, c8_layer_api);
    ("spec cascade 4.1 declared values", `Quick, c41_declared_values);
    ("spec cascade 4.2 cascaded values", `Quick, c42_cascaded_values);
    ( "spec cascade origin/importance order",
      `Quick,
      spec_cascade_origin_importance_order );
    ("spec cascade 4.2 integrated cascade order", `Quick, c42_integrated_order);
    ("spec cascade 4.3 specified values", `Quick, c43_specified_values);
    ("spec cascade 4.3 revert specified values", `Quick, c43_revert_values);
    ("spec cascade 4.7 examples", `Quick, c47_examples);
    ( "spec DOM selector matching boundary vectors",
      `Quick,
      dom_selector_boundary );
    ("spec fetch import and URL boundary vectors", `Quick, fetch_url_boundary);
    ( "spec environment query evaluation boundary vectors",
      `Quick,
      environment_query_boundary );
    ("spec value resolution boundary vectors", `Quick, value_resolution_boundary);
    ( "spec custom property computed-time boundary vectors",
      `Quick,
      custom_property_boundary );
    ("spec current-work at-rules", `Quick, spec_current_at_rules);
    ( "spec font-palette-values descriptor matrix",
      `Quick,
      font_palette_values_descriptor_matrix );
    ( "spec view-transition descriptor matrix",
      `Quick,
      spec_view_transition_descriptor_matrix );
    ( "spec position-try descriptor matrix",
      `Quick,
      spec_position_try_descriptor_matrix );
    ( "spec at-rule descriptor order duplicate matrix",
      `Quick,
      spec_at_rule_descriptor_matrix );
    ( "spec at-rule shared inventory matrix",
      `Quick,
      spec_at_rule_inventory_matrix );
    ( "spec snapshot tracking vectors",
      `Quick,
      test_spec_snapshot_tracking_vectors );
    ("spec snapshot membership matrix", `Quick, test_snapshot_membership_matrix);
    ( "spec cascade 4.4-4.8 value processing scope",
      `Quick,
      c448_value_stage_scope );
    ( "partial recovery: bad declaration does not poison sibling rule",
      `Quick,
      fun () ->
        (* Strict [read_stylesheet] would raise on the bad [rgb()]. The partial
           entry point drops just the bad declaration; both rules survive (the
           empty [.a\{\}] and the good [.b]). Per 5.4.4. *)
        let sheet, warnings =
          parse_stylesheet_partial ".a { color: rgb(300); } .b { color: red; }"
        in
        Alcotest.(check int)
          "both rules survive" 2
          (List.length (Css.Stylesheet.rules sheet));
        Alcotest.(check int) "one warning" 1 (List.length warnings) );
    ( "partial recovery: warnings carry snippets",
      `Quick,
      fun () ->
        let _sheet, warnings =
          parse_stylesheet_partial ".a { color: rgb(300); }"
        in
        match warnings with
        | [ e ] -> (
            match Css.Error.snippet e with
            | None -> Alcotest.fail "expected warning with snippet"
            | Some _ -> ())
        | _ -> Alcotest.fail "expected exactly one warning" );
    ( "partial recovery: Css.parse entry point",
      `Quick,
      fun () ->
        (* Public [Css.parse] always succeeds and surfaces non-fatal warnings;
           [Css.of_string] on the same input is still strict. *)
        let { Css.stylesheet; warnings } =
          Css.parse ".a { color: red; } .b { color: rgb(300); }"
        in
        Alcotest.(check int)
          "both rules survive" 2
          (List.length (Css.rule_statements stylesheet));
        Alcotest.(check int) "one warning" 1 (List.length warnings);
        (* filename threads through *)
        match warnings with
        | [ (_, fname) ] ->
            Alcotest.(check string) "default filename" "<string>" fname
        | _ -> Alcotest.fail "expected one warning" );
    ( "partial recovery: unclosed brace recovered per 5.3.7",
      `Quick,
      fun () ->
        let { Css.stylesheet; warnings = _ } = Css.parse ".btn { color: red;" in
        Alcotest.(check int)
          "one rule recovered" 1
          (List.length (Css.rule_statements stylesheet)) );
    ( "partial recovery: bad declaration drops, rule survives",
      `Quick,
      fun () ->
        (* CSS Syntax 5.4.4: an invalid declaration is discarded while the
           enclosing rule and its other declarations survive. *)
        let { Css.stylesheet; warnings } =
          Css.parse ".a { color: invalidcolor; color: red; }"
        in
        let rules = Css.rule_statements stylesheet in
        Alcotest.(check int) "rule kept" 1 (List.length rules);
        Alcotest.(check int)
          "one warning for the bad decl" 1 (List.length warnings) );
    ( "partial recovery: filename propagates to warnings",
      `Quick,
      fun () ->
        let { Css.warnings; _ } =
          Css.parse ~filename:"user.css" ".a { color: rgb(300); }"
        in
        match warnings with
        | [ (_, fname) ] -> Alcotest.(check string) "filename" "user.css" fname
        | _ -> Alcotest.fail "expected one warning" );
    ( "meta: `Full attaches snippets, `None skips them",
      `Quick,
      fun () ->
        let input = ".a { color: rgb(300); }" in
        let full = Css.parse ~meta:`Full input in
        let none = Css.parse ~meta:`None input in
        (match full.warnings with
        | [ (e, _) ] ->
            Alcotest.(check bool)
              "`Full: snippet present" true
              (Css.Error.snippet e <> None)
        | _ -> Alcotest.fail "expected one warning under `Full");
        match none.warnings with
        | [ (e, _) ] ->
            Alcotest.(check bool)
              "`None: snippet skipped" true
              (Css.Error.snippet e = None)
        | _ -> Alcotest.fail "expected one warning under `None" );
    ( "semicolon-terminated at-rule survives partial parse",
      `Quick,
      fun () ->
        (* [@layer base;] parses through section 5.4.2 to an at-rule with [block
           = None]. The replay cursor must still present a terminating [;] to
           [read_layer], otherwise the at-rule is silently dropped. *)
        let { Css.stylesheet; warnings } = Css.parse "@layer base;" in
        let stmts = Css.statements stylesheet in
        Alcotest.(check int) "one statement" 1 (List.length stmts);
        Alcotest.(check int) "no warnings" 0 (List.length warnings) );
    ( "unknown at-rule surfaces as Unknown_at_rule warning",
      `Quick,
      fun () ->
        (* Section 5.4.1: an at-rule with no handler is discarded with a typed
           warning; the surrounding stylesheet continues to parse. *)
        let { Css.stylesheet; warnings } =
          Css.parse "@unknown-rule { color: red; } .a { color: blue; }"
        in
        Alcotest.(check int)
          "good rule survives" 1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ (e, _) ] -> (
            match e.Css.Error.kind with
            | Css.Error.Unknown_at_rule name ->
                Alcotest.(check string) "at-rule name" "unknown-rule" name
            | _ ->
                Alcotest.failf "expected Unknown_at_rule, got %s"
                  (Css.Error.to_string e))
        | _ -> Alcotest.fail "expected one warning" );
    ( "malformed @supports surfaces as Bad_condition warning",
      `Quick,
      fun () ->
        (* A bad @supports condition used to raise via the untyped
           [Reader.Parse_error] shape, bypassing the partial-parse contract. It
           now becomes a typed [Bad_condition] warning while surrounding rules
           keep parsing. *)
        let { Css.stylesheet; warnings } =
          Css.parse
            "@supports not-a-function foo { .a { color: red } } .b { color: \
             blue }"
        in
        Alcotest.(check int)
          "sibling rule survives" 1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ (e, _) ] -> (
            match e.Css.Error.kind with
            | Css.Error.Bad_condition { at_rule; _ } ->
                Alcotest.(check string) "at-rule label" "@supports" at_rule
            | _ ->
                Alcotest.failf "expected Bad_condition, got %s"
                  (Css.Error.to_string e))
        | _ -> Alcotest.fail "expected one warning" );
  ]

let suite = ("stylesheet", stylesheet_tests @ additional_tests)
