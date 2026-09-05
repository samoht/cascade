(** Tests for CSS stylesheet interface types - CSS/MDN spec compliance *)

open Cascade
module Selector = Css.Selector
open Css.Stylesheet
open Css_test_helpers

(* Test helper: compose optimize + minified to_string the way [to_string
   ~minify:true] used to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

let statement_of_rule (r : rule) =
  Css.rule ~selector:r.selector ~nested:r.nested ?merge_key:r.merge_key
    r.declarations

let check_rule = check_value_cursor "rule" read_rule pp_rule

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Css.Declaration.to_string d))
    ( = )

let check_import_rule =
  check_value_cursor "import_rule" read_import_rule pp_import_rule

let check_layer_name =
  check_value_cursor "layer_name" read_layer_name pp_layer_name

let check_declaration =
  check_value_cursor "declaration" Css.Declaration.read_declaration
    (Css.Pp.option Css.Declaration.pp)

let check_stylesheet = check_value_cursor "stylesheet" read pp_stylesheet

(* Assert both serialization paths for one input. [minified] is pure pp: the
   held value in its shortest same-node spelling. [optimized] is pp+optimize:
   the canonical value after cross-node folds (colour names <-> hex, calc
   folding, keyword folds). pp never changes a node; optimize does. *)
let assert_minify_and_optimize input ~minified ~optimized =
  match Css.of_string ~strict:false input with
  | Error _ -> Alcotest.failf "parse failed: %s" input
  | Ok p ->
      Alcotest.(check string)
        (input ^ " [minify]") minified
        (Css.to_string ~minify:true p.stylesheet |> String.trim);
      Alcotest.(check string)
        (input ^ " [minify+optimize]")
        optimized
        (minify p.stylesheet |> String.trim)

(* Short alias for stylesheet serialization checks. *)
let check = check_stylesheet

(* Not a roundtrip test *)
let test_rule () =
  (* Basic rules *)
  check_rule ".btn{color:red}";
  check_rule "h1{font-size:2rem}";
  check_rule "#main{display:flex}";
  check_rule "div.container{margin:auto}";

  (* Multiple declarations *)
  check_rule ".card{padding:1rem;border:1px solid#ccc}";
  check_rule "body{margin:0;font-family:Arial,sans-serif}";

  (* Multiple selectors *)
  check_rule ".a,.b{display:block}";

  (* Universal selector *)
  check_rule ~expected:"*{box-sizing:border-box}" "* { box-sizing: border-box }";

  (* Test invalid rule syntax *)
  neg_cursor read "{color:red}";
  (* Missing selector *)
  neg_cursor read ".btn";
  (* Missing declarations. CSS Syntax sec. 2.2 auto-closes [.btn{] so it is
     spec-valid and not asserted here. *)
  neg_cursor read ".btn{color}";
  (* Missing value *)
  neg_cursor read_rule "" (* Empty rule *)

let test_stylesheet () =
  (* Test basic stylesheet parsing *)
  check_stylesheet ".btn{color:red}";
  (* Regression (cssQuake): [aspect-ratio:auto] before a separator must keep the
     declaration (the trailing token used to defeat the bare-auto path), and a
     [background] shorthand layer holds a single image, so commas separate
     layers rather than being swallowed as a [background-image] list. *)
  check_stylesheet ".x{aspect-ratio:auto;color:red}";
  check_stylesheet ".x{background:url(a.png),red}";
  (* pp holds the authored Named node; cross-folding blue -> #00f (hex at most
     as long as the name) is optimize. *)
  assert_minify_and_optimize "body{margin:0}.btn{color:blue}"
    ~minified:"body{margin:0}.btn{color:blue}"
    ~optimized:"body{margin:0}.btn{color:#00f}";

  (* Test stylesheet with at-rules *)
  check_stylesheet "@media screen{.btn{color:green}}";
  check_stylesheet "@layer base{body{margin:0}}";

  (* Test empty stylesheet *)
  check_stylesheet "";

  (* Test stylesheet with comments - comments are stripped in minified output *)
  check_stylesheet ~expected:".btn{color:red}" "/*comment*/.btn{color:red}";

  check_stylesheet ~expected:"@media(min-width:768px){.a{display:block}}"
    "@media (min-width: 768px) { .a { display: block } }";
  check_stylesheet
    ~expected:"@media screen and (max-width:640px){.btn{font-size:.875rem}}"
    "@media screen and (max-width: 640px){.btn{font-size:.875rem}}";
  assert_minify_and_optimize "@media screen { .test { color: blue } }"
    ~minified:"@media screen{.test{color:blue}}"
    ~optimized:"@media screen{.test{color:#00f}}";
  check_stylesheet ~expected:"@supports(display:grid){.grid{display:grid}}"
    "@supports (display: grid) { .grid { display: grid } }";
  check_stylesheet
    "@supports(display:grid){.grid{display:grid}@supports(color:red){.x{color:red}}}";
  check_stylesheet ~expected:"@supports(display:grid){.grid{display:grid}}"
    "@supports (display: grid) { .grid { display: grid } }";
  check_stylesheet
    ~expected:
      "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: \
     red }";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}to{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet
    ~expected:"@font-face{font-family:MyFont;src:url(font.woff2)}"
    "@font-face { font-family: MyFont; src: url(font.woff2); }";
  check_stylesheet ~expected:"@page:first{margin:1in}"
    "@page :first { margin: 1in }";
  check_stylesheet ~expected:".test{color:red}" ".test { color: red }";

  (* MQ5 sec. 2.1: an empty media query list is valid and evaluates like all. *)
  check_stylesheet ~expected:"@media{.test{color:red}}"
    "@media { .test { color: red } }";
  check_stylesheet ~expected:"@media{}" "@media { }";
  neg_cursor read "@charset 'UTF-8'" (* Wrong charset quotes *)

let string_of_stylesheet s = Css.Stylesheet.to_string ~minify:true s

(* Helper for testing rule construction *)
let check_construct_rule name expected rule =
  check_construct name (Css.Pp.to_string ~minify:true pp_rule) expected rule

(* Helper for testing complete stylesheet *)
let check_stylesheet_helper name expected sheet =
  check_construct name string_of_stylesheet expected sheet

(* Not a roundtrip test *)
let test_rule_creation () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let selector = selector rule in
  (* Just check we can get selector back *)
  Alcotest.(check bool)
    "selector exists" true
    (Css.Selector.equal selector (Css.Selector.class_ "red"));
  Alcotest.(check int)
    "rule declarations count" 1
    (List.length (declarations rule))

(* Not a roundtrip test *)
let test_media_rule_creation () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media
      ~condition:(Css.Media.of_string "screen and (min-width: 768px)")
      [ statement_of_rule r ]
  in
  let sheet = Css.Stylesheet.v [ media_stmt ] in
  let output = Css.Stylesheet.to_string ~minify:true sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_container_rule_creation () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let container_stmt =
    container ~name:"sidebar"
      ~condition:(Css.Container.of_string "(min-width: 400px)")
      [ statement_of_rule r ]
  in
  let sheet = Css.Stylesheet.v [ container_stmt ] in
  let output = Css.Stylesheet.to_string ~minify:true sheet in
  check_stylesheet output

(* ignore-test: error-recovery contract, not a per-statement constructor. *)
let test_nested_container_recovers () =
  (* A nested @container with an invalid query (empty style(), mixed operators)
     must surface through of_string as a recoverable parse warning, not an
     uncaught Failure escaping the documented (parse, Error.t) result. *)
  let recovers input =
    match Css.of_string input with
    | Ok { Css.warnings; _ } ->
        Alcotest.(check bool)
          ("nested container recovered: " ^ input)
          true (warnings <> [])
    | Error _ ->
        Alcotest.failf "expected recovery with a warning, got Error: %s" input
  in
  recovers {|.x { @container style() { .y { color: red } } }|};
  recovers
    {|.x { @container (width > 1px) and (height > 1px) or (width > 2px) { .y { color: red } } }|}

(* ignore-test: error-recovery contract, not a per-statement constructor. *)
let test_layer_rule_recovery () =
  (* CSS Syntax 3 sec. 5.4.1: an invalid rule inside an @layer / @media block is
     dropped on its own; its sibling rules must survive. [!] is a delim wherever
     it appears in a selector, so [.x!y] is invalid under any reading of the
     ident rules. *)
  let keeps_siblings input =
    match Css.of_string input with
    | Ok { Css.stylesheet; warnings; _ } ->
        let out = Css.to_string ~minify:true stylesheet in
        Alcotest.(check bool) ("warns: " ^ input) true (warnings <> []);
        Alcotest.(check bool)
          ("keeps .a and .b: " ^ out)
          true
          (Astring.String.is_infix ~affix:".a{" out
          && Astring.String.is_infix ~affix:".b{" out)
    | Error e ->
        Alcotest.failf "expected recovery: %s" (Cascade.Error.to_string e)
  in
  keeps_siblings "@layer u{.a{color:red}.x!y{color:lime}.b{color:blue}}";
  keeps_siblings "@media screen{.a{color:red}.x!y{color:lime}.b{color:blue}}";
  keeps_siblings
    "@supports (display:grid){.a{color:red}.x!y{color:lime}.b{color:blue}}"

(* Not a roundtrip test *)
let test_supports_rule_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let supports_stmt =
    supports
      ~condition:(Css.Supports.property "display" "grid")
      [ statement_of_rule r ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.to_string ~minify:true sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_supports_nested_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let nested_supports =
    supports
      ~condition:(Css.Supports.property "color" "red")
      [ statement_of_rule r ]
  in
  let supports_stmt =
    supports
      ~condition:(Css.Supports.property "display" "grid")
      [ statement_of_rule r; nested_supports ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.to_string ~minify:true sheet in
  check_stylesheet output

(* Not a roundtrip test *)
let test_property_rule_creation () =
  let prop : Css.Values.color property_rule =
    {
      name = "--my-color";
      syntax = Css.Variables.Color;
      initial_value = Some (Css.Values.hex "ff0000");
      inherits = true;
    }
  in
  Alcotest.(check string) "property name" "--my-color" prop.name;
  (* Property has typed syntax field *)
  Alcotest.(check bool) "property inherits" true prop.inherits

(* Not a roundtrip test *)
let test_layer_rule_creation () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let layer_stmt = layer ~name:[ "utilities" ] [ statement_of_rule rule ] in
  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  (* Both paths for the same node. minify (pp) does the shortest same-node
     spelling: Hex ff0000 -> #f00. minify+optimize cross-folds to the shortest
     node: Hex ff0000 -> red (Named, shorter). *)
  Alcotest.(check string)
    "layer rule creation (minify)"
    "@layer utilities{.red{background-color:#f00}}"
    (Css.Stylesheet.to_string ~minify:true sheet);
  Alcotest.(check string)
    "layer rule creation (minify+optimize)"
    "@layer utilities{.red{background-color:red}}" (minify sheet)

(* Not a roundtrip test *)
let test_construct_rule_helper () =
  (* Test rule construction and string representation *)
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule1 = rule ~selector:(Selector.class_ "red") [ decl ] in
  check_construct_rule "simple rule" ".red{background-color:#f00}" rule1;

  let decls =
    [
      Css.Declaration.color (Css.Values.hex "000000");
      Css.Declaration.margin [ Css.Values.Px 10. ];
    ]
  in
  let rule2 = rule ~selector:(Selector.id "test") decls in
  check_construct_rule "multiple declarations" "#test{color:#000;margin:10px}"
    rule2

(* Not a roundtrip test *)
let helper () =
  (* Test complete stylesheet construction and string representation *)
  let decl = Css.Declaration.display Css.Properties.Block in
  let rule = rule ~selector:(Selector.element "div") [ decl ] in
  let sheet = Css.Stylesheet.v [ statement_of_rule rule ] in
  check_stylesheet_helper "simple stylesheet" "div{display:block}" sheet;

  let media_stmt =
    media ~condition:(Css.Media.of_string "print") [ statement_of_rule rule ]
  in
  let sheet2 = Css.Stylesheet.v [ media_stmt ] in
  check_stylesheet_helper "media stylesheet" "@media print{div{display:block}}"
    sheet2

(* Not a roundtrip test *)
let test_empty_stylesheet () =
  let sheet = empty in
  Alcotest.(check int) "empty layers" 0 (List.length (layers sheet));
  Alcotest.(check int) "empty rules" 0 (List.length (rules sheet));
  Alcotest.(check int) "empty media" 0 (List.length (media_queries sheet));
  Alcotest.(check int)
    "empty container" 0
    (List.length (container_queries sheet))

(* Not a roundtrip test *)
let construction () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:(Css.Media.of_string "screen") [ statement_of_rule rule ]
  in
  let prop = property ~syntax:Css.Variables.Color "--my-color" in

  let sheet = Css.Stylesheet.v [ statement_of_rule rule; media_stmt; prop ] in

  Alcotest.(check int) "sheet rules count" 1 (List.length (rules sheet));
  Alcotest.(check int) "sheet media count" 1 (List.length (media_queries sheet));
  let props_count =
    List.fold_left
      (fun acc -> function Property _ -> acc + 1 | _ -> acc)
      0 sheet
  in
  Alcotest.(check int) "sheet properties count" 1 props_count

let items_conversion () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:(Css.Media.of_string "screen") [ statement_of_rule rule ]
  in

  let sheet = Css.Stylesheet.v [ statement_of_rule rule; media_stmt ] in

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
  let decl1 = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let rule1 = rule ~selector:(Selector.class_ "red") [ decl1 ] in
  let _sheet1 = Css.Stylesheet.v [ statement_of_rule rule1 ] in

  let decl2 = Css.Declaration.color (Css.Values.hex "0000ff") in
  let rule2 = rule ~selector:(Selector.class_ "blue") [ decl2 ] in
  let _sheet2 = Css.Stylesheet.v [ statement_of_rule rule2 ] in

  let combined =
    Css.Stylesheet.v [ statement_of_rule rule1; statement_of_rule rule2 ]
  in
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
  let output = Css.Stylesheet.to_string ~minify:true sheet in

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
  let output = Css.Stylesheet.to_string ~minify:true sheet in
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
  let r = Cursor.of_string input in
  try
    let _ = read r in
    Alcotest.failf "%s: expected parse error" name
  with Cursor.Parse_error _ -> ()

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
      "@property \
       --length-percentage{syntax:\"<length>|<percentage>\";inherits:false;initial-value:0%}"
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
  let decl = Css.Declaration.color (Css.Values.hex "0000ff") in
  let rule_obj = rule ~selector:(Selector.class_ "blue") [ decl ] in
  let layer_stmt = layer ~name:[ "utilities" ] [ statement_of_rule rule_obj ] in

  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.to_string ~minify:true sheet in
  Alcotest.(check string)
    "layer pp" "@layer utilities{.blue{color:#00f}}" output;

  (* Test empty layer - per CSS spec, empty @layer statements end with
     semicolon *)
  let empty_layer = layer ~name:[ "base" ] [] in
  let empty_sheet = Css.Stylesheet.v [ empty_layer ] in
  let empty_output = Css.Stylesheet.to_string ~minify:true empty_sheet in
  Alcotest.(check string) "empty layer" "@layer base;" empty_output

(** Test complete stylesheet pp *)
let pp_case () =
  let decl = Css.Declaration.background_color (Css.Values.hex "ff0000") in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:(Css.Media.of_string "screen") [ statement_of_rule r ]
  in
  let prop =
    property ~syntax:Css.Variables.Color
      ~initial_value:(Css.Values.Named Css.Values.Blue) "--primary"
  in

  let sheet = Css.Stylesheet.v [ statement_of_rule r; media_stmt; prop ] in

  let output = Css.Stylesheet.to_string ~minify:true sheet in
  Alcotest.(check string)
    "stylesheet pp"
    (* pp emits each node's shortest same-node spelling: Hex ff0000 -> #f00,
       Named Blue -> blue. Cross-node folds (#f00->red, blue->#00f) are
       optimize, not pp. *)
    ".red{background-color:#f00}@media \
     screen{.red{background-color:#f00}}@property \
     --primary{syntax:\"<color>\";inherits:false;initial-value:blue}"
    output

(** Test [@charset] rules *)
let charset_case () =
  (* CSS Syntax 3 section 8.3: parsed UTF-8 [@charset] is compatibility surface,
     not an emitted stylesheet at-rule. *)
  check_stylesheet ~expected:"" "@charset \"UTF-8\";"

(** Test [@import] rules *)
let import_case () =
  (* Test various import forms *)
  check_stylesheet ~expected:"@import\"styles.css\";" "@import 'styles.css';";
  check_stylesheet ~expected:"@import\"utilities.css\"layer(utilities);"
    "@import url(utilities.css) layer(utilities);";
  check_stylesheet ~expected:"@import\"print.css\"print;"
    "@import 'print.css' print;";
  check_import_rule
    ~expected:"@import\"theme.css\"supports(selector(:has(img)))screen;"
    "@import url(theme.css) supports(selector(:has(img))) screen;";
  check_import_rule
    ~expected:
      "@import\"tokens.css\"layer(theme.tokens)supports(--theme:dark) \
       (prefers-color-scheme:dark);"
    "@import url(tokens.css) layer(theme.tokens) supports(--theme: dark) \
     (prefers-color-scheme: dark);";
  check_import_rule
    ~expected:"@import\"wide.css\"supports(width:stretch)(width>=40em);"
    "@import url(wide.css) supports((width: stretch)) (width >= 40em);";
  neg_cursor read_import_rule "@import url(theme.css) screen layer(theme);";
  check_import_rule ~expected:"@import\"theme.css\"supports(selector())screen;"
    "@import url(theme.css) supports(selector()) screen;";
  neg_cursor read_import_rule "@import url(theme.css) layer(theme,) screen;"

(** Test [@namespace] rules *)
let namespace_case () =
  (* Test namespace roundtrips *)
  check_stylesheet ~expected:"@namespace \"http://www.w3.org/1999/xhtml\";"
    "@namespace url(http://www.w3.org/1999/xhtml);";
  check_stylesheet ~expected:"@namespace svg\"http://www.w3.org/2000/svg\";"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  check_stylesheet
    ~expected:"@namespace math\"http://www.w3.org/1998/Math/MathML\";"
    "@namespace math \"http://www.w3.org/1998/Math/MathML\";";
  neg_cursor read "@namespace { url(http://example.test); }";
  neg_cursor read "@namespace svg;"

(** Test [@keyframes] rules *)
let keyframes_case () =
  (* CSS Animations 1 section 3: [from] and [to] are spec-equivalent to [0%] and
     [100%]; under [~minify:true] the printer canonicalizes to the shorter
     spelling - [0%] beats [from] and [to] beats [100%], matching cssnano and
     Lightning CSS. *)
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}to{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes fade{0%{opacity:0}to{opacity:1}}"
    "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"

(* ignore-test *)
let test_keyframes_spec_edge_vectors () =
  check_stylesheet
    ~expected:"@keyframes pulse{0%,50%,to{opacity:1}25%,75%{opacity:.5}}"
    "@keyframes pulse { 0%, 50%, 100% { opacity: 1 } 25%, 75% { opacity: .5 } }";
  check_stylesheet
    ~expected:
      "@keyframes \
       slide{to{transform:translateX(10px)}0%{transform:translateX(0)}}"
    "@keyframes slide { 100% { transform: translateX(10px) } 0% { transform: \
     translateX(0) } }";
  check_stylesheet
    ~expected:"@-webkit-keyframes fade{0%{opacity:0}to{opacity:1}}"
    "@-webkit-keyframes fade { from { opacity: 0 } to { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { -1% { opacity: 0 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { 101% { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { 50px { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { from, { opacity: 1 } }";
  neg_cursor read "@keyframes missing-block"

(** Test [@font-face] rules *)
let font_face_case () =
  (* Test font-face roundtrip *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:MyCustomFont;src:url(font.woff2);font-display:swap}"
    "@font-face { font-family: MyCustomFont; src: url('font.woff2'); \
     font-display: swap; }"

let spec_fontface_descriptors () =
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:local(Brand),url(brand.woff2)format(woff2)tech(variations);font-weight:400 \
       700;font-style:normal italic;font-stretch:75% \
       125%;font-display:optional;unicode-range:U+25-FF}"
    "@font-face { font-family: Brand; src: local(\"Brand\"), \
     url(\"brand.woff2\") format(\"woff2\") tech(variations); font-weight: 400 \
     700; font-style: normal italic; font-stretch: 75% 125%; font-display: \
     optional; unicode-range: U+0025-00FF; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:MetricAdjusted;src:url(metric.woff2);size-adjust:92%;ascent-override:90%;descent-override:25%;line-gap-override:normal}"
    "@font-face { font-family: MetricAdjusted; src: url(metric.woff2); \
     size-adjust: 92%; ascent-override: 90%; descent-override: 25%; \
     line-gap-override: normal; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:FeatureFont;src:url(feature.woff2);font-feature-settings:\"kern\" \
       1;font-variation-settings:\"wght\" 650}"
    "@font-face { font-family: FeatureFont; src: url(feature.woff2); \
     font-feature-settings: \"kern\" 1; font-variation-settings: \"wght\" 650; \
     }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:VariantFont;src:url(variant.woff2);font-variant:common-ligatures \
       small-caps tabular-nums ruby}"
    "@font-face { font-family: VariantFont; src: url(variant.woff2); \
     font-variant: common-ligatures small-caps tabular-nums ruby; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:Sparse;src:local(Sparse),url(sparse.woff2)}"
    "@font-face { ; font-family: Sparse; ; src: local(\"Sparse\") \
     url(\"sparse.woff2\"); ; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:Emoji;src:url(emoji.woff2);unicode-range:U+1F600-1F64F,U+???}"
    "@font-face { font-family: Emoji; src: url(emoji.woff2); unicode-range: \
     U+1F600-1F64F, U+???; }";
  check_stylesheet ~expected:"" "@font-face { src: url(font.woff2); }";
  check_stylesheet ~expected:"" "@font-face { font-family: Brand; }";
  check_stylesheet ~expected:"" "@font-face { font-display: swap; }";
  (* An unknown descriptor (e.g. Fontsource's non-standard font-named-instance)
     is dropped; the rest of the @font-face is kept, like browsers. *)
  check_stylesheet
    ~expected:"@font-face{font-family:Foo;src:url(foo.woff2);font-style:normal}"
    "@font-face { font-family: Foo; src: url(foo.woff2); font-named-instance: \
     'Regular'; font-style: normal; }";
  (* An invalid value of a *known* descriptor drops just that descriptor and
     keeps the rest of the @font-face, like browsers (CSS Fonts 4 sec. 4.1). *)
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
     maybe; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-variant: \
     common-ligatures no-common-ligatures; }";
  (* sec. 4.4 gives the font-width descriptor the values of the property of the
     same name, so a keyword endpoint takes the property's percentage fold. A
     descriptor never reaches factoring, but the fold is still a node change and
     so waits for the optimizer. *)
  assert_minify_and_optimize
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     condensed; }"
    ~minified:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:condensed}"
    ~optimized:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:75%}";
  assert_minify_and_optimize
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     condensed expanded; }"
    ~minified:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:condensed \
       expanded}"
    ~optimized:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:75% 125%}";
  (* A descending font-stretch range is kept like the font-weight / oblique
     ranges below: CSS Fonts 4 sec. 4.4 swaps the endpoints for font matching
     and leaves the descriptor as it was written. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:200% 50%}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: 200% \
     50%; }";
  (* [oblique <angle> <angle>] is kept even when the first angle is larger than
     the second, and [local("")] is a valid empty family name; browsers accept
     both, so cascade keeps them rather than dropping the @font-face. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-style:oblique \
       20deg 10deg}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: \
     oblique 20deg 10deg; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:local(\"\")}"
    "@font-face { font-family: Brand; src: local(\"\"); }"

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
  neg_cursor read "@page : { margin: 1cm }";
  neg_cursor read "@page :unknown { margin: 1cm }";
  neg_cursor read "@page { color: notacolor }"

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
  check_stylesheet ~expected:"@page invoice:blank:first{margin:1cm}"
    "@page invoice:blank:first { margin: 1cm }";
  neg_cursor read "@page { @unknown { content: none } }";
  neg_cursor read "@page { @top-left; }";
  neg_cursor read "@page { @top-left { color: notacolor } }"

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
  (* The [+] multiplier repeats a component space-separated (CSS Values 4 sec.
     2.3), and that space is the only thing separating the two: CSS Syntax 3
     sec. 4.3.1 consumes [10px20px] as one dimension whose unit is [px20px]. The
     [#] multiplier carries a comma of its own, so it is the control. *)
  check_stylesheet
    ~expected:
      "@property \
       --edges{syntax:\"<length>+\";inherits:false;initial-value:10px 20px}"
    "@property --edges { syntax: \"<length>+\"; inherits: false; \
     initial-value: 10px 20px; }";
  check_stylesheet
    ~expected:
      "@property \
       --stops{syntax:\"<color>+\";inherits:false;initial-value:yellow blue}"
    "@property --stops { syntax: \"<color>+\"; inherits: false; initial-value: \
     yellow blue; }";
  check_stylesheet
    ~expected:
      "@property \
       --palette{syntax:\"<color>#\";inherits:false;initial-value:yellow,blue}"
    "@property --palette { syntax: \"<color>#\"; inherits: false; \
     initial-value: yellow, blue; }";
  neg_cursor read
    "@property --bad { syntax: \"<length>+\"; inherits: false; initial-value: \
     red }";
  neg_cursor read
    "@property --bad { syntax: \"<color>\"; inherits: yes; initial-value: red }";
  neg_cursor read
    "@property --bad { syntax: \"<length> |\"; inherits: false; initial-value: \
     1px }";
  neg_cursor read "@property color { syntax: \"*\"; inherits: false }"

let spec_font_face_descriptor_matrix () =
  (* Descriptor syntax is a spec oracle; these are not snapshots of the current
     printer. *)
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@font-face{font-family:Brand Sans;src:local(\"Brand \
         Sans\"),url(brand.woff2)format(woff2);font-display:fallback}",
        "@font-face { font-family: \"Brand Sans\"; src: local(\"Brand Sans\"), \
         url(brand.woff2) format(\"woff2\"); font-display: fallback; }" );
      ( "@font-face{font-family:RangeFont;src:url(range.woff2);font-weight:100 \
         900;font-style:oblique 10deg 20deg;font-stretch:50% 200%}",
        "@font-face { font-family: RangeFont; src: url(range.woff2); \
         font-weight: 100 900; font-style: oblique 10deg 20deg; font-stretch: \
         50% 200%; }" );
      ( "@font-face{font-family:Metrics;src:url(metrics.woff2);size-adjust:100%;ascent-override:normal;descent-override:20%;line-gap-override:0%}",
        "@font-face { font-family: Metrics; src: url(metrics.woff2); \
         size-adjust: 100%; ascent-override: normal; descent-override: 20%; \
         line-gap-override: 0%; }" );
      ( "@font-face{font-family:TallMetrics;src:url(tall.woff2);ascent-override:120%;descent-override:125%;line-gap-override:0%}",
        "@font-face { font-family: TallMetrics; src: url(tall.woff2); \
         ascent-override: 120%; descent-override: 125%; line-gap-override: 0%; \
         }" );
    ];
  (* A descending font-weight range is kept (browsers accept it, like the
     oblique range above). *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(brand.woff2);font-weight:900 100}"
    "@font-face { font-family: Brand; src: url(brand.woff2); font-weight: 900 \
     100 }";
  (* An invalid metric / size-adjust value drops just that descriptor. *)
  check_stylesheet
    ~expected:"@font-face{font-family:Brand;src:url(brand.woff2)}"
    "@font-face { font-family: Brand; src: url(brand.woff2); ascent-override: \
     -1%; }";
  check_stylesheet
    ~expected:"@font-face{font-family:Brand;src:url(brand.woff2)}"
    "@font-face { font-family: Brand; src: url(brand.woff2); size-adjust: \
     normal; }"

let spec_keyframes_selector_matrix () =
  (* CSS Animations 1 section 3: [from] / [to] / [0%] / [100%] are pairwise
     spec-equivalent. The printer canonicalizes to the shorter spelling - [0%]
     beats [from], [to] beats [100%]. *)
  check_stylesheet
    ~expected:
      "@keyframes move{0%{translate:none}50%{translate:10px \
       20px}to{translate:20px 0}}"
    "@keyframes move { from { translate: none } 50% { translate: 10px 20px } \
     to { translate: 20px 0 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { 50%, { opacity: 1 } }";
  check_stylesheet ~expected:"@keyframes bad{}"
    "@keyframes bad { from, 120% { opacity: 1 } }"

(* CSS Properties and Values API 1 sec. 2: an [@property] registration is
   document-global, so [var(--ring)] carries a [<color>] wherever it is
   referenced. In [box-shadow: 0 0 var(--ring)] the reference therefore fills
   the colour slot, not the blur slot, and a [@keyframes] frame is an ordinary
   declaration block (CSS Animations 1 sec. 3), so the same declaration
   normalises the same way inside one. *)
let spec_keyframes_shadow_color_var () =
  let sheet css =
    match Css.of_string ~strict:false css with
    | Ok { Css.stylesheet; _ } -> stylesheet
    | Error e -> Alcotest.failf "parse failed: %s" (Cascade.Error.to_string e)
  in
  let registration =
    "@property --ring{syntax:\"<color>\";inherits:false;initial-value:red}"
  in
  Alcotest.(check string)
    "a registered colour var fills the shadow colour slot inside @keyframes"
    (registration
   ^ ":root{--ring:red}@keyframes glow{to{box-shadow:0 0 0 var(--ring)}}")
    (minify
       (sheet
          (registration
         ^ ":root{--ring:rgb(255 0 0)}@keyframes glow{to{box-shadow:0 0 \
            var(--ring)}}")));
  Alcotest.(check string)
    "a colour var declared only inside @keyframes is still a colour var"
    (registration
   ^ "@keyframes glow{0%{--ring:red}}.a{box-shadow:0 0 0 var(--ring)}")
    (minify
       (sheet
          (registration
         ^ "@keyframes glow{from{--ring:rgb(255 0 0)}}.a{box-shadow:0 0 \
            var(--ring)}")))

let spec_page_margin_descriptor_matrix () =
  check_stylesheet
    ~expected:
      "@page chapter:right{size:letter \
       landscape;margin:1in;@right-top{content:counter(page)}@bottom-center{content:\"Chapter\"}}"
    "@page chapter:right { size: letter landscape; margin: 1in; @right-top { \
     content: counter(page) } @bottom-center { content: \"Chapter\" } }";
  List.iter (neg_cursor read) [ "@page { @top-center { display: 1px } }" ];
  check_stylesheet ~expected:"@page:first:left{margin:1cm}"
    "@page :first:left { margin: 1cm }";
  check_stylesheet ~expected:"@page:blank:first{margin:.5cm}"
    "@page :blank:first { margin: 0.5cm }"

(* CSS Paged Media 3 sec. 6: Appendix A is the normative list of CSS 2.1
   properties that apply in the page context and in the margin context, and
   "behavior for properties not included in CSS 2.1 is undefined" - undefined,
   not invalid, "to allow the gradual addition of appropriate CSS3 properties as
   they emerge". Page-margin boxes inherit from the page context and the page
   context inherits from the root element, so an inherited property set on the
   page carries into every margin box. Blink 146 reads every declaration below
   back out of [cssRules[0].cssText] unchanged, in the page body and in the
   margin box alike. *)
let spec_page_context_properties () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      (* Appendix A, page context. *)
      ("@page{color:red}", "@page { color: red }");
      ("@page{font-size:12pt}", "@page { font-size: 12pt }");
      ("@page{direction:rtl}", "@page { direction: rtl }");
      ("@page{text-align:center}", "@page { text-align: center }");
      ("@page{padding:1cm}", "@page { padding: 1cm }");
      ("@page{border:1px solid red}", "@page { border: 1px solid red }");
      ("@page{visibility:hidden}", "@page { visibility: hidden }");
      (* Sec. 6.1: page-based counters are defined in the page context. *)
      ("@page{counter-increment:page 1}", "@page { counter-increment: page 1 }");
      (* Sec. 7.1.2 and 7.3 descriptors of this same module. Blink 146 keeps
         [page-orientation] and drops [bleed], which it does not implement;
         [bleed] is the name the module defines. *)
      ( "@page{page-orientation:rotate-left}",
        "@page { page-orientation: rotate-left }" );
      ("@page{bleed:6pt}", "@page { bleed: 6pt }");
      (* Not in Appendix A, so undefined rather than invalid; Blink 146 keeps
         both. Cascade formats CSS, so an undefined descriptor is carried
         through rather than dropped. *)
      ("@page{orphans:3;widows:3}", "@page { orphans: 3; widows: 3 }");
      (* Blink 146 drops a custom property in a page context and no module
         defines one there, so admissibility is open; carrying it costs nothing,
         dropping it loses the author's text. *)
      ("@page{--custom:1}", "@page { --custom: 1 }");
      (* Appendix A, margin context: the page-context list plus [content]. *)
      ( "@page{@top-center{color:red;font-size:9pt;content:\"x\"}}",
        "@page { @top-center { color: red; font-size: 9pt; content: \"x\" } }"
      );
      ( "@page{@top-center{display:block}}",
        "@page { @top-center { display: block } }" );
      (* An inherited property on the page reaches the margin boxes. *)
      ( "@page{color:red;@top-center{content:\"x\"}}",
        "@page { color: red; @top-center { content: \"x\" } }" );
    ];
  (* What browsers still reject: a value the property's grammar does not admit,
     and an item that is not a declaration at all. Blink 146 drops each of these
     and keeps the rest of the block. *)
  List.iter (neg_cursor read)
    [
      "@page { margin: notalength }";
      "@page { color: notacolor }";
      "@page { width: 10 }";
      "@page { .a { b: c } }";
      "@page { @media print { margin: 1cm } }";
      "@page { @unknown { content: none } }";
      "@page { @top-center { display: 1px } }";
      "@page { @top-center { @media screen { .x { color: red } } } }";
    ]

(* CSS Cascade 5 sec. 6.2 puts an important author declaration above a normal
   one whichever was written first, and sec. 6.4 breaks a tie between two of the
   same importance by order of appearance. The page context cascades on those
   rules like any other author context, so a duplicate name in a page body or a
   margin box keeps the important declaration, and the last one when both carry
   the same importance. Blink 146 reads exactly that back out of
   [cssRules[0].cssText] in both contexts. Blink serializes the survivors with
   the normal declarations before the important ones, a consequence of how it
   builds its property set; cascade keeps the order the author wrote, which
   names the same declarations. *)
let spec_page_descriptor_importance () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@page{margin:1cm!important}",
        "@page { margin: 1cm !important; margin: 2cm }" );
      ( "@page{margin:2cm!important}",
        "@page { margin: 1cm; margin: 2cm !important }" );
      ("@page{margin:2cm}", "@page { margin: 1cm; margin: 2cm }");
      ( "@page{margin:2cm!important}",
        "@page { margin: 1cm !important; margin: 2cm !important }" );
      ( "@page{color:red!important}",
        "@page { color: red !important; color: blue; color: green }" );
      (* The important declaration keeps the place it was written in. *)
      ( "@page{margin:1cm!important;size:A4}",
        "@page { margin: 1cm !important; size: a4; margin: 2cm }" );
      (* Sec. 5: a margin box holds a declaration list of its own, and the same
         cascade decides it. *)
      ( "@page{@top-center{content:\"a\"!important}}",
        "@page { @top-center { content: \"a\" !important; content: \"b\" } }" );
      ( "@page{@top-center{content:\"b\"!important}}",
        "@page { @top-center { content: \"a\"; content: \"b\" !important } }" );
      ( "@page{@top-center{content:\"b\"}}",
        "@page { @top-center { content: \"a\"; content: \"b\" } }" );
      (* A longhand written after its shorthand is a different name, so neither
         replaces the other and both stay. Blink expands the pair to longhands
         and prints [margin: 2cm 1cm 1cm], the same four values. *)
      ( "@page{margin:1cm;margin-top:2cm}",
        "@page { margin: 1cm; margin-top: 2cm }" );
      ( "@page{margin-top:2cm!important;margin:1cm}",
        "@page { margin-top: 2cm !important; margin: 1cm }" );
      (* Blink drops a custom property in a page context, so it gives no reading
         here; the cascade rule that decides every other name decides this one
         too. *)
      ("@page{--x:1!important}", "@page { --x: 1 !important; --x: 2 }");
    ]

(* CSS Paged Media 3 sec. 5 gives a margin at-rule a [<declaration-list>], which
   CSS Syntax 3 sec. 5.4.4 consumes even when it holds nothing, so a margin box
   with an empty block is valid and Blink 146 reads one back unchanged - a box
   left empty by a stray [;] too, which sec. 5.4.3 discards with no declaration
   to validate. Sec. 5 generates the box only where its [content] computes away
   from [none], so an empty one paints nothing, and {!Css.to_string} elides what
   paints nothing whether or not the sheet was optimized: an empty box goes the
   way an empty style rule and an empty [@page] already go, and a page left with
   neither descriptor nor box goes with it. *)
let spec_page_margin_box_empty () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ("@page{@top-center{}}", "@page { @top-center { } }");
      ("@page{@top-center{}}", "@page { @top-center {} }");
      ("@page{@top-center{}}", "@page { @top-center { ; } }");
      ( "@page{margin:1cm;@top-center{}}",
        "@page { margin: 1cm; @top-center { } }" );
      ( "@page{@top-center{}@bottom-center{content:\"x\"}}",
        "@page { @top-center { } @bottom-center { content: \"x\" } }" );
    ];
  (* An empty block is not a missing one: sec. 5 still asks for a block. *)
  neg_cursor read "@page { @top-left; }";
  assert_minify_and_optimize "@page { @top-center { } }" ~minified:""
    ~optimized:"";
  assert_minify_and_optimize "@page { margin: 1cm; @top-center { } }"
    ~minified:"@page{margin:1cm}" ~optimized:"@page{margin:1cm}";
  assert_minify_and_optimize
    "@page { @top-center { } @bottom-center { content: \"x\" } }"
    ~minified:"@page{@bottom-center{content:\"x\"}}"
    ~optimized:"@page{@bottom-center{content:\"x\"}}"

let spec_property_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@property \
         --angle-list{syntax:\"<angle>#\";inherits:false;initial-value:0deg}",
        "@property --angle-list { syntax: \"<angle>#\"; inherits: false; \
         initial-value: 0deg }" );
      ( "@property \
         --angle-zero{syntax:\"<angle>\";inherits:false;initial-value:0deg}",
        "@property --angle-zero { syntax: \"<angle>\"; inherits: false; \
         initial-value: 0deg }" );
      ( "@property \
         --ident-or-color{syntax:\"<custom-ident>|<color>\";inherits:true;initial-value:currentColor}",
        "@property --ident-or-color { syntax: \"<custom-ident> | <color>\"; \
         inherits: true; initial-value: currentColor }" );
    ];
  List.iter (neg_cursor read)
    [
      "@property --bad { syntax: \"<angle>#\"; inherits: false; initial-value: \
       red }";
      "@property --bad { syntax: \"<angle>\"; inherits: false; initial-value: \
       0 }";
      "@property --bad { syntax: \"<angle>#\"; inherits: false; initial-value: \
       0 }";
      "@property --bad { syntax: \"<length> || <color>\"; inherits: false; \
       initial-value: 1px }";
    ]

(** Test sheet_item variants *)
let sheet_item_case () =
  (* Test that we can parse stylesheets with various statement types *)
  let test_statements =
    [
      ("@charset \"UTF-8\";", "");
      ("@import 'test.css';", "@import\"test.css\";");
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
  check_stylesheet ~expected:"@import\"base.css\";.btn{color:red}" input

(* Not a roundtrip test *)
let test_read_stylesheet_basic () =
  let css = ".btn { color: red; padding: 10px; }" in
  let reader = Cursor.of_string css in
  let sheet = read reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule" 1 (List.length rules);
  let rule = List.hd rules in
  let decls = declarations rule in
  Alcotest.(check int) "rule has two declarations" 2 (List.length decls)

(* Not a roundtrip test *)
let test_read_stylesheet_multiple_rules () =
  let css = ".btn { color: red; } .card { margin: 5px; }" in
  let reader = Cursor.of_string css in
  let sheet = read reader in
  let rules = rules sheet in
  Alcotest.(check int) "has two rules" 2 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_empty () =
  let css = "" in
  let reader = Cursor.of_string css in
  let sheet = read reader in
  let rules = rules sheet in
  Alcotest.(check int) "empty stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_whitespace_only () =
  let css = "   \n\t  " in
  let reader = Cursor.of_string css in
  let sheet = read reader in
  let rules = rules sheet in
  Alcotest.(check int)
    "whitespace-only stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_with_comments () =
  let css = "/* comment */ .btn { color: red; } /* another comment */" in
  let reader = Cursor.of_string css in
  let sheet = read reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule despite comments" 1 (List.length rules)

let string_of_strict_error e = Cascade.Error.to_string e

let strict_accept name css =
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      let strict_output = minify parsed.stylesheet in
      let { Css.stylesheet; warnings; _ } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected valid %s: %s" name
              (Cascade.Error.to_string err)
      in
      Alcotest.(check int)
        ("lenient parse is warning-free for valid " ^ name)
        0 (List.length warnings);
      Alcotest.(check string)
        ("strict/lenient serialization agree for " ^ name)
        strict_output (minify stylesheet)
  | Error err ->
      Alcotest.failf "strict parser rejected valid %s: %s" name
        (string_of_strict_error err)

let strict_reject name css =
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      Alcotest.failf "strict parser accepted invalid %s: %S -> %S" name css
        (minify parsed.stylesheet)
  | Error err ->
      Alcotest.(check bool)
        ("strict error carries detail for " ^ name)
        true
        (String.length (string_of_strict_error err) > 0);
      let { Css.stylesheet; warnings; _ } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected invalid %s: %s" name
              (Cascade.Error.to_string err)
      in
      ignore (minify stylesheet : string);
      Alcotest.(check bool)
        ("lenient parse warns for " ^ name)
        true
        (List.length warnings > 0)

let font_family_descriptor_grammar () =
  strict_accept "single named @font-face family"
    "@font-face { font-family: Brand; src: url(brand.woff2) }";
  strict_accept "quoted generic word as @font-face family name"
    "@font-face { font-family: \"serif\"; src: url(serif.woff2) }";
  strict_accept "named @font-palette-values family list"
    "@font-palette-values --brand { font-family: Brand, \"Color Font\" }";
  strict_accept "quoted generic word as palette family name"
    "@font-palette-values --serif { font-family: \"serif\" }";
  strict_reject "empty @font-face font-family"
    "@font-face { font-family:; src: url(brand.woff2) }";
  strict_reject "multiple @font-face families"
    "@font-face { font-family: Brand, Other; src: url(brand.woff2) }";
  strict_reject "generic @font-face family"
    "@font-face { font-family: serif; src: url(serif.woff2) }";
  strict_reject "generic word in an unquoted @font-face family"
    "@font-face { font-family: Brand serif; src: url(brand.woff2) }";
  strict_reject "CSS-wide @font-face family"
    "@font-face { font-family: inherit; src: url(brand.woff2) }";
  strict_reject "empty @font-palette-values font-family"
    "@font-palette-values --brand { font-family: }";
  strict_reject "generic @font-palette-values family"
    "@font-palette-values --serif { font-family: serif }";
  strict_reject "generic in an @font-palette-values family list"
    "@font-palette-values --brand { font-family: Brand, serif }";
  strict_reject "generic word in an unquoted palette family"
    "@font-palette-values --brand { font-family: Brand serif }";
  strict_reject "CSS-wide @font-palette-values family"
    "@font-palette-values --brand { font-family: inherit }"

let lenient_recover name css expected min_warnings =
  let { Css.stylesheet; warnings; _ } =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed
    | Error err ->
        Alcotest.failf "%s did not recover leniently: %s" name
          (Cascade.Error.to_string err)
  in
  Alcotest.(check string)
    (name ^ " recovered stylesheet")
    expected
    (minify stylesheet |> String.trim);
  Alcotest.(check bool)
    (name ^ " warning count") true
    (List.length warnings >= min_warnings)

let spec_strict_accepts_valid_stylesheets () =
  List.iter
    (fun (name, css) -> strict_accept name css)
    [
      ("empty stylesheet", "");
      ("whitespace-only stylesheet", "   \n\t  ");
      ( "top-level CDO and CDC ignored",
        "<!-- .x { color: red } --> .y { color: blue }" );
      ("charset first", "@charset \"UTF-8\"; .x { color: red }");
      ("simple qualified rule", ".btn { color: red; }");
      ("empty qualified rule", ".empty { }");
      ("multiple qualified rules", ".btn { color: red } .card { margin: 10px }");
      ( "comments between declarations",
        ".x { color: red /* keep */ ; margin: 0 }" );
      ("four-value margin shorthand", ".x { margin: 1px 2px 3px 4px }");
      ( "font shorthand with line-height and family list",
        ".x { font: italic small-caps 700 1rem/1.5 \"Brand\", sans-serif }" );
      ("unknown vendor-prefixed declaration", ".x { -webkit-line-clamp: 2 }");
      ( "unknown future declaration",
        ".x { view-transition-class: card primary }" );
      ( "custom property token stream",
        ".x { --token-list: [a, b] (c) { d: e } }" );
      ("custom property digit dashed-ident", ".x { font-size: var(--1A202C) }");
      ("valid escape in string", ".x { content: '\\gggg' }");
      ("out-of-range rgb channels clamp", ".x { color: rgb(300, 300, 300) }");
      ("mixed numeric rgb channels", ".x { color: rgb(50%, 100, 50%) }");
      ( "transparent currentColor color-mix",
        ".x { color: color-mix(in srgb, currentColor 25%, transparent) }" );
      ( "hsl color-mix hue interpolation",
        ".x { color: color-mix(in hsl longer hue, red, blue) }" );
      ( "relative color syntax",
        ".x { color: rgb(from var(--brand) r g b / .5) }" );
      ( "typed attr fallback with calc",
        ".x { width: attr(data-w px, calc(10px + 0px)) }" );
      ( "rectangular grid template areas",
        ".x { grid-template-areas: \"head head\" \"nav main\" }" );
      ("escaped selector identifiers", ".\\31 0\\% { color: red }");
      ( "selector list with where and has",
        ".card:where(:has(> img), .featured) { color: red }" );
      ( "forgiving selector list keeps valid branch",
        ".x:is(.a,:future) { color: red }" );
      ( "not selector list",
        ".x:not(.disabled, [aria-disabled=\"true\"]) { color: red }" );
      ( "nth-child of selector list",
        ".x:nth-child(2n+1 of .item, :not(.skip)) { color: red }" );
      ("nested style rule", ".card { color: red; &:hover { color: blue } }");
      ( "nested conditional style rule",
        ".card { @media (width >= 30em) { color: red } }" );
      ( "combined supports and media import",
        "@import url(theme.css) supports(display: grid) screen;" );
      ( "anonymous import layer with supports and media",
        "@import url(theme.css) layer() supports((display: grid)) screen and \
         (color);" );
      ( "layer statement before import",
        "@layer reset, theme; @import url(theme.css) layer(theme);" );
      ("anonymous layer block", "@layer { .anonymous { color: red } }");
      ( "nested layer block",
        "@layer framework { @layer theme { .title { color: red } } }" );
      ( "charset import namespace prelude order",
        "@charset \"UTF-8\"; @import url(base.css); @namespace svg \
         url(http://www.w3.org/2000/svg); svg|a { fill: currentColor }" );
      ( "namespace before qualified rule",
        "@namespace svg url(http://www.w3.org/2000/svg); svg|a { color: red }"
      );
      ( "namespaced universal selector",
        "@namespace svg url(http://www.w3.org/2000/svg); *|a { color: red }" );
      ( "media not with feature",
        "@media not screen and (color) { .x { color: red } }" );
      ( "media range interval",
        "@media (400px <= width <= 1000px) { .x { color: red } }" );
      ( "media query list",
        "@media only screen and (hover: hover), print and (color) { .x { \
         color: red } }" );
      ( "supports font-tech",
        "@supports font-tech(variations) { .x { font-variation-settings: \
         \"wght\" 700 } }" );
      ( "supports grouped boolean logic",
        "@supports ((display: grid) or (display: flex)) and (not (display: \
         subgrid)) { .x { display: grid } }" );
      ( "container named style query",
        "@container card style(--variant: featured) { .x { display: grid } }" );
      ( "container range query",
        "@container sidebar (inline-size > 30em) { .x { display: grid } }" );
      (* Paged Media 3 SS 3.1 permits multiple page pseudo-classes; compound
         vectors are covered by the page descriptor matrix. *)
      ("empty page margin box", "@page { @top-center { } }");
      ( "scope with end boundary",
        "@scope (.card) to (.footer) { .title { color: red } }" );
      ( "font-face wildcard unicode range",
        "@font-face { font-family: Icons; src: url(icons.woff2); \
         unicode-range: U+4?? }" );
      (* CSS Fonts 4 sec. 4.4 makes a descending descriptor range well defined
         rather than an error: the user agent swaps the two endpoints. Only
         [unicode-range] keeps an ordering rule. *)
      ( "font-face descending font-weight range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
         900 100 }" );
      ( "font-face descending oblique angle range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-style: \
         oblique 20deg 10deg }" );
      ( "font-face descending font-stretch range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
         200% 50% }" );
      ( "counter-style cyclic",
        "@counter-style thumbs { system: cyclic; symbols: \"*\" \"x\"; suffix: \
         \" \" }" );
    ]

let spec_strict_rejects_invalid_stylesheets () =
  List.iter
    (fun (name, css) -> strict_reject name css)
    [
      (* CSS Syntax and stylesheet grammar. *)
      ("top-level bare block", "{ color: red }");
      ("stray top-level right brace", ".x { color: red } }");
      ("unterminated block repaired at EOF", ".x { color: red");
      ( "malformed charset missing semicolon",
        "@charset \"UTF-8\" .x { color: red }" );
      ("charset must use string token", "@charset url(UTF-8);");
      ("late charset", ".x { color: red } @charset \"UTF-8\";");
      ( "late import after qualified rule",
        ".x { color: red } @import url(late.css);" );
      ( "namespace after qualified rule",
        ".x { color: red } @namespace svg url(http://www.w3.org/2000/svg);" );
      ( "namespace before import",
        "@namespace svg url(http://www.w3.org/2000/svg); @import url(late.css);"
      );
      ("import inside style rule", ".x { @import url(inner.css); color: red }");
      (* CSS Nesting 1 sec. 3.3: an at-rule whose body holds no style rule does
         not nest, so writing one inside a style rule is invalid. *)
      ( "font-face inside style rule",
        ".x { color: red; @font-face { font-family: F; src: url(f.woff2) } }" );
      ( "keyframes inside style rule",
        ".x { color: red; @keyframes k { to { opacity: 1 } } }" );
      ( "webkit keyframes inside style rule",
        ".x { color: red; @-webkit-keyframes k { to { opacity: 1 } } }" );
      ( "moz keyframes inside style rule",
        ".x { color: red; @-moz-keyframes k { to { opacity: 1 } } }" );
      ( "property inside style rule",
        ".x { color: red; @property --p { syntax: \"*\"; inherits: false } }" );
      ("page inside style rule", ".x { color: red; @page { margin: 1cm } }");
      ( "counter-style inside style rule",
        ".x { color: red; @counter-style c { system: cyclic; symbols: \"x\" } }"
      );
      ( "position-try inside style rule",
        ".x { color: red; @position-try --t { top: 1px } }" );
      ( "font-palette-values inside style rule",
        ".x { color: red; @font-palette-values --v { font-family: F; \
         base-palette: 0 } }" );
      ( "font-feature-values inside style rule",
        ".x { color: red; @font-feature-values F { @styleset { s: 1 } } }" );
      ( "viewport inside style rule",
        ".x { color: red; @viewport { width: 1px } }" );
      ( "ms-viewport inside style rule",
        ".x { color: red; @-ms-viewport { width: 1px } }" );
      ( "supports-condition inside style rule",
        ".x { color: red; @supports-condition (color: red) { color: green } }"
      );
      (* CSS Conditional 5 sec. 4: an @else needs a preceding @when here too. *)
      ( "orphan else inside style rule",
        ".x { color: red; @else { color: green } }" );
      ( "orphan else inside a nested group rule",
        ".x { @media screen { color: red; @else { color: green } } }" );
      ("import inside media rule", "@media screen { @import url(inner.css); }");
      ("import with block", "@import url(theme.css) { .x { color: red } }");
      ( "import layer after condition",
        "@import url(theme.css) screen layer(theme);" );
      ( "import duplicate layer condition",
        "@import url(theme.css) layer(theme) layer(base);" );
      ( "import supports after media condition",
        "@import url(theme.css) screen supports(display: grid);" );
      ("invalid layer list comma", "@layer reset,,base;");
      ("invalid css-wide layer name", "@layer initial { .x { color: red } }");
      ("qualified rule without block", ".x");
      ("declaration missing colon", ".x { color red }");
      ("declaration missing property", ".x { : red }");
      (* Selectors. *)
      ("empty class selector", ". { color: red }");
      ("empty id selector", "# { color: red }");
      ("invalid attribute operator", ".x[data-value ~~ test] { color: red }");
      ("empty is pseudo", ".x:is() { color: red }");
      ("empty has pseudo", ".x:has() { color: red }");
      ("invalid has relative selector", ".x:has(>) { color: red }");
      ("empty functional pseudo", ".x:not() { color: red }");
      ("invalid nth-child argument", ".x:nth-child(foo) { color: red }");
      ("invalid nth-child of selector list", ".x:nth-child(2 of) { color: red }");
      ("invalid nth-child An+B", ".x:nth-child(2n+ of .item) { color: red }");
      (* Cascade and declaration grammar. *)
      ("css-wide keyword mixed with ordinary value", ".x { color: inherit red }");
      ("all shorthand non-css-wide value", ".x { all: auto }");
      ( "duplicate important annotation",
        ".x { color: red !important !important }" );
      ("misspelled important annotation", ".x { color: red !importent }");
      ("single-hyphen custom property", ".x { -custom: value }");
      ("custom property empty name", ".x { --: value }");
      (* Values and property grammars. *)
      ("width missing unit", ".x { width: 10 }");
      ("unknown length unit", ".x { width: 10pp }");
      ("margin too many components", ".x { margin: 1px 2px 3px 4px 5px }");
      ("negative padding", ".x { padding: -1px }");
      ("negative line-height", ".x { line-height: -1 }");
      ("negative animation duration", ".x { animation-duration: -1s }");
      ("negative transition duration", ".x { transition-duration: -1s }");
      ("font-weight outside range", ".x { font-weight: 0 }");
      ("integer property rejects number", ".x { order: 1.5 }");
      ("font-family trailing comma", ".x { font-family: Brand, }");
      ("z-index rejects length", ".x { z-index: 1px }");
      ("invalid aspect-ratio", ".x { aspect-ratio: 16 / }");
      ("invalid calc operator", ".x { width: calc(1px + ) }");
      ( "invalid calc dimensional multiplication",
        ".x { width: calc(1px * 2px) }" );
      ("invalid min function empty", ".x { width: min() }");
      ("invalid clamp arity", ".x { width: clamp(1px, 2px) }");
      ("invalid short hex color", ".x { color: #12 }");
      ("invalid color function arity", ".x { color: rgb(255 0) }");
      ("mixed legacy and modern rgb syntax", ".x { color: rgb(255 0 0, .5) }");
      ("invalid hsl arity", ".x { color: hsl(0 100%) }");
      ("invalid lab channel", ".x { color: lab(50% 20) }");
      ( "invalid color-mix hue keyword",
        ".x { color: color-mix(in hsl specified hue, red, blue) }" );
      ("invalid transform angle", ".x { transform: rotate(45) }");
      ("invalid translate length", ".x { translate: 10 }");
      ("invalid scale arity", ".x { scale: 1 2 3 4 }");
      ("invalid filter function", ".x { filter: blur(red) }");
      ( "invalid grid area row shape",
        ".x { grid-template-areas: \"a a\" \"a b\" }" );
      ( "invalid grid area row width mismatch",
        ".x { grid-template-areas: \"a\" \"a b\" }" );
      (* At-rule descriptor grammars. *)
      ( "property missing syntax descriptor",
        "@property --x { inherits: false; initial-value: 0px }" );
      ( "property angle initial-value requires angle unit",
        "@property --x { syntax: \"<angle>\"; inherits: false; initial-value: \
         0 }" );
      ( "property angle-list initial-value requires angle unit",
        "@property --x { syntax: \"<angle>#\"; inherits: false; initial-value: \
         0 }" );
      ( "property invalid inherits descriptor",
        "@property --x { syntax: \"<length>\"; inherits: maybe; initial-value: \
         0px }" );
      ( "property initial-value rejects css-wide keyword",
        "@property --x { syntax: \"<length>\"; inherits: false; initial-value: \
         inherit }" );
      ( "font-face unicode-range out of range",
        "@font-face { font-family: Brand; src: url(font.woff2); unicode-range: \
         U+110000 }" );
      ( "font-face unicode-range descending range",
        "@font-face { font-family: Brand; src: url(font.woff2); unicode-range: \
         U+20-10 }" );
      ( "font-face invalid font-display list",
        "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
         block swap }" );
      ( "font-palette missing font-family",
        "@font-palette-values --brand { override-colors: 0 red }" );
      ( "counter-style missing system",
        "@counter-style thumbs { symbols: \"*\" }" );
      ( "counter-style cyclic missing symbols",
        "@counter-style thumbs { system: cyclic }" );
      ("page margin invalid declaration", "@page { @top-left { display: 1px } }");
      ("keyframes invalid selector", "@keyframes fade { 50px { opacity: 0 } }");
      ("keyframes forbidden name none", "@keyframes none { to { opacity: 1 } }");
      ( "keyframes forbidden css-wide name",
        "@keyframes initial { to { opacity: 1 } }" );
      (* Conditional query grammars. *)
      ( "media ungrouped mixed boolean operators",
        "@media (width) and (height) or (color) { .x { color: red } }" );
      ("media dangling not", "@media not { .x { color: red } }");
      ("media dangling and", "@media screen and { .x { color: red } }");
      ("container empty style query", "@container style() { .x { color: red } }");
      ( "container empty scroll-state query",
        "@container scroll-state() { .x { color: red } }" );
      ("supports dangling not", "@supports not { .x { color: red } }");
      ( "supports dangling operator",
        "@supports (display: grid) and { .x { color: red } }" );
      ( "supports mixed operators without grouping",
        "@supports (display: grid) and (color: red) or (width: 1px) { .x { \
         color: red } }" );
      ("scope invalid empty root", "@scope () { .x { color: red } }");
      ( "scope invalid empty end boundary",
        "@scope (.x) to () { .x { color: red } }" );
    ]

let spec_lenient_recovery_stylesheets () =
  lenient_recover "bad declaration then good declaration"
    ".a { color: invalid-color; color: red }" ".a{color:red}" 1;
  lenient_recover "bad declaration keeps sibling rule"
    ".a { color: rgb(300); } .b { color: red }" ".b{color:red}" 1;
  (* CSS Syntax 3 sec. 5.4.2 consumes the at-rule and its block whatever the
     name, so what recovers here is the rule after it, not the at-rule. *)
  lenient_recover "unknown at-rule keeps its neighbour"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    "@unknown-rule{.bad{color:red}}.ok{color:#00f}" 1;
  lenient_recover "bad selector list drops rule only"
    ".ok { color: green } .bad:not() { color: red } .next { color: blue }"
    ".ok{color:green}.next{color:#00f}" 1;
  lenient_recover "unclosed block auto-closes" ".btn { color: red;"
    ".btn{color:red}" 0;
  lenient_recover "descriptor at-rule in a style rule keeps its neighbours"
    ".a { color: red; @font-face { font-family: F; src: url(f.woff2) } \
     background: blue }"
    ".a{color:red;background:#00f}" 1;
  lenient_recover "orphan else in a style rule keeps its neighbours"
    ".a { color: red; @else { color: green } background: blue }"
    ".a{color:red;background:#00f}" 1

(* CSS Syntax 3 sec. 5.4.4 "consume a style block's contents" ends an invalid
   declaration at the next top-level [;], and a [{}] met on the way is one
   component value of the value being skipped, not a stopping point. A nested
   at-rule's body is <block-contents> just like a style rule's (CSS Nesting 1
   sec. 3.3), so the same recovery applies inside it: Blink 146 drops the one
   declaration and keeps every neighbour, the enclosing group rule and the rest
   of the sheet. *)
let spec_lenient_recovery_nested_at_rule_declarations () =
  let recovered = ".a{@media screen{color:red;background:#00f}}" in
  lenient_recover "bad declaration first in a nested at-rule"
    ".a { @media screen { width: 10; color: red; background: blue } }" recovered
    1;
  lenient_recover "bad declaration mid-run in a nested at-rule"
    ".a { @media screen { color: red; width: 10; background: blue } }" recovered
    1;
  lenient_recover "bad declaration last in a nested at-rule"
    ".a { @media screen { color: red; background: blue; width: 10 } }" recovered
    1;
  lenient_recover "sole declaration of a nested at-rule dropped"
    ".a { @media screen { width: 10 } }" "" 1;
  lenient_recover "bad declaration in a nested style rule"
    ".a { & b { color: red; width: 10; margin: 0 } }" ".a b{color:red;margin:0}"
    1;
  lenient_recover "bad declaration in a style rule under a nested at-rule"
    ".a { @media screen { & b { color: red; width: 10; margin: 0 } } }"
    ".a{@media screen{& b{color:red;margin:0}}}" 1;
  lenient_recover "bad declaration in a doubly nested at-rule"
    ".a { @media screen { @container (width > 0px) { color: red; width: 10; \
     background: blue } } }"
    ".a{@media screen{@container(width>0px){color:red;background:#00f}}}" 1;
  lenient_recover "nested at-rule keeps recovery in @layer"
    ".a { @layer x { color: red; width: 10; background: blue } }"
    ".a{@layer x{color:red;background:#00f}}" 1;
  lenient_recover "nested at-rule keeps recovery in @scope"
    ".a { @scope (.b) { color: red; width: 10; background: blue } }"
    ".a{@scope(.b){color:red;background:#00f}}" 1;
  lenient_recover "nested at-rule keeps recovery in @starting-style"
    ".a { @starting-style { color: red; width: 10; background: blue } }"
    ".a{@starting-style{color:red;background:#00f}}" 1;
  (* A [{}] in the dropped value is consumed with it; the run resumes at the [;]
     after it. An unclosed function has no such [;] - the tokenizer closes it at
     the end of the block, so it swallows the rest, as Blink does. *)
  lenient_recover "curly block inside a dropped value is skipped with it"
    ".a { @media screen { color: red; width: {1}; background: blue } }"
    recovered 1;
  lenient_recover "unclosed function swallows the rest of the block"
    ".a { @media screen { color: red; width: calc(1px; background: blue } }"
    ".a{@media screen{color:red}}" 1;
  lenient_recover "a run of stray tokens is dropped like a bad declaration"
    ".a { @media screen { color: red; !!!; background: blue } }" recovered 1

(* A dropped declaration leaves no gap: the run written around it is one run, as
   it is in Blink 146, the same way a dropped at-rule leaves one. *)
let spec_lenient_recovery_nested_declaration_run () =
  lenient_recover "bad declaration immediately before a nested rule"
    ".a { @media screen { width: 10; & b { color: red } } }"
    ".a{@media screen{& b{color:red}}}" 1;
  lenient_recover "bad declaration immediately after a nested rule"
    ".a { @media screen { & b { color: red } width: 10; background: blue } }"
    ".a{@media screen{& b{color:red}background:#00f}}" 1;
  lenient_recover "dropped declaration does not seal the run before a rule"
    ".a { @media screen { color: red; width: 10; background: blue; & b { \
     margin: 0 } } }"
    ".a{@media screen{color:red;background:#00f;& b{margin:0}}}" 1;
  lenient_recover "dropped declaration does not seal the run it opened"
    ".a { @media screen { width: 10; color: red; & b { margin: 0 } background: \
     blue } }"
    ".a{@media screen{color:red;& b{margin:0}background:#00f}}" 1

(* [css] reports exactly [expected] warnings leniently, and [~strict:true]
   rejects it exactly when the lenient parse warned. *)
let warns_exactly css expected =
  let warnings =
    match Css.of_string ~strict:false css with
    | Ok { Css.warnings; _ } -> warnings
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" css
          (Cascade.Error.to_string err)
  in
  Alcotest.(check int) ("warnings for " ^ css) expected (List.length warnings);
  match (Css.of_string ~strict:true css, expected) with
  | Ok _, 0 -> ()
  | Error _, 0 -> Alcotest.failf "strict parse rejected warning-free %S" css
  | Ok _, _ -> Alcotest.failf "strict parse accepted warned-about %S" css
  | Error _, _ -> ()

(* Each recovered declaration reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. *)
let spec_recovery_warns_once_per_declaration () =
  let check = warns_exactly in
  check ".a { @media screen { color: red; width: 10; background: blue } }" 1;
  check ".a { @media screen { width: 10; height: 20; color: red } }" 2;
  check
    ".a { @media screen { @container (width > 0px) { width: 10; color: red } } \
     }"
    1;
  check ".a { @media screen { color: red; background: blue } }" 0

(* CSS Paged Media 3 sec. 4.1: "If an error is encountered during the processing
   of a declaration block within a page or a margin context, the Rules for
   handling parsing errors apply; that is, valid declarations within the block
   are applied." One bad descriptor therefore costs that descriptor, not the
   [@page] holding it and not the sheet holding that. The discard follows CSS
   Syntax 3 sec. 5.4.4 for a declaration - up to the next top-level [;], a [{}]
   met on the way counting as one component value of the value being skipped -
   and sec. 5.4.2 for an at-rule, which ends at its block or its [;]. Blink 146
   keeps both neighbours in every case below. *)
let spec_lenient_recovery_page_descriptors () =
  let recovered = "@page{margin:1cm;margin-top:2cm}" in
  lenient_recover "bad descriptor first in @page"
    "@page { width: 10; margin: 1cm; margin-top: 2cm }" recovered 1;
  lenient_recover "bad descriptor mid-body in @page"
    "@page { margin: 1cm; width: 10; margin-top: 2cm }" recovered 1;
  lenient_recover "bad descriptor last in @page"
    "@page { margin: 1cm; margin-top: 2cm; width: 10 }" recovered 1;
  lenient_recover "sole descriptor of @page dropped" "@page { width: 10 }" "" 1;
  lenient_recover "curly block inside a dropped page value is skipped with it"
    "@page { margin: 1cm; width: {1}; margin-top: 2cm }" recovered 1;
  lenient_recover "two curly blocks in a dropped page value are one skip"
    "@page { margin: 1cm; width: {1}{2}; margin-top: 2cm }" recovered 1;
  lenient_recover "tokens after a curly block in a dropped page value"
    "@page { margin: 1cm; width: {1} 2px; margin-top: 2cm }" recovered 1;
  (* An unclosed function has no top-level [;] left to stop at - the tokenizer
     closes it at the end of the block - so it takes the rest of the body, as it
     does in Blink. *)
  lenient_recover "unclosed function swallows the rest of the page body"
    "@page { margin: 1cm; width: calc(1px; margin-top: 2cm }"
    "@page{margin:1cm}" 1;
  (* A selector-shaped item is not a declaration, so sec. 5.4.4 skips it as a
     bad one: with no [;] after its block it takes the body's tail with it, and
     with a [;] it costs only itself. Blink 146 splits the two the same way. *)
  lenient_recover "selector-shaped item without a semicolon takes the tail"
    "@page { margin: 1cm; .a { b: c } margin-top: 2cm }" "@page{margin:1cm}" 1;
  lenient_recover "selector-shaped item with a semicolon costs only itself"
    "@page { margin: 1cm; .a { b: c }; margin-top: 2cm }" recovered 1;
  (* An at-rule ends at its block, so discarding an invalid one leaves the
     descriptor written after it in the page. *)
  lenient_recover "unknown page margin rule is dropped alone"
    "@page { @bogus-box { content: \"x\" } margin: 1cm; margin-top: 2cm }"
    recovered 1;
  lenient_recover "at-rule that is no page margin rule is dropped alone"
    "@page { margin: 1cm; @media screen { color: red } margin-top: 2cm }"
    recovered 1;
  lenient_recover "page margin rule with no block ends at its semicolon"
    "@page { margin: 1cm; @top-center; margin-top: 2cm }" recovered 1

(* A page-margin box holds a declaration block of its own (CSS Paged Media 3
   sec. 5), so sec. 4.1 applies inside it too: the descriptors written around a
   bad one stay in the box, and the box stays in the [@page]. *)
let spec_lenient_recovery_page_margin_box () =
  let recovered = "@page{@top-center{content:\"x\";margin:0}}" in
  lenient_recover "bad descriptor in a page margin box"
    "@page { @top-center { content: \"x\"; width: 10; margin: 0 } }" recovered 1;
  lenient_recover "at-rule in a page margin box ends at its block"
    "@page { @top-center { @media screen { a: b } content: \"x\"; margin: 0 } }"
    recovered 1;
  lenient_recover "selector-shaped item in a margin box with a semicolon"
    "@page { @top-center { content: \"x\"; .a { b: c }; margin: 0 } }" recovered
    1;
  (* The box outlives its only item, as it does in Blink 146, and what is left
     is an empty box: nothing to paint, so it is elided on output like any other
     block that applies nothing, and the page it empties goes with it. *)
  lenient_recover "selector-shaped item alone in a page margin box"
    "@page { @top-center { .a { b: c } } }" "" 1

(* A dropped descriptor leaves no gap: the page keeps the descriptors written
   around it in source order, and its margin boxes keep theirs. *)
let spec_lenient_recovery_page_body_order () =
  lenient_recover "bad descriptor before a page margin box"
    "@page { margin: 1cm; width: 10; @top-center { content: \"x\" } \
     margin-top: 2cm }"
    "@page{margin:1cm;margin-top:2cm;@top-center{content:\"x\"}}" 1;
  lenient_recover "bad descriptor after a page margin box"
    "@page { margin: 1cm; @top-center { content: \"x\" } width: 10; \
     margin-top: 2cm }"
    "@page{margin:1cm;margin-top:2cm;@top-center{content:\"x\"}}" 1;
  lenient_recover "bad descriptor between two page margin boxes"
    "@page { @top-center { content: \"x\" } width: 10; @bottom-center { \
     content: \"y\" } margin: 1cm }"
    "@page{margin:1cm;@top-center{content:\"x\"}@bottom-center{content:\"y\"}}"
    1

(* Each dropped page descriptor reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. A stray [;] is discarded without a
   declaration to validate (CSS Syntax 3 sec. 5.4.3), so it costs nothing. *)
let spec_page_recovery_warns_once_per_descriptor () =
  warns_exactly "@page { margin: 1cm; width: 10; margin-top: 2cm }" 1;
  warns_exactly "@page { margin: 1cm; width: {1}{2}; margin-top: 2cm }" 1;
  warns_exactly "@page { width: 10; margin: 1cm; height: 20 }" 2;
  warns_exactly "@page { @top-center { content: \"x\"; width: 10; margin: 0 } }"
    1;
  warns_exactly "@page { margin: 1cm; margin-top: 2cm }" 0;
  warns_exactly "@page { margin: 1cm;; margin-top: 2cm }" 0;
  warns_exactly "@page { ; margin: 1cm }" 0;
  warns_exactly "@page { @top-center { content: \"x\" } margin: 1cm }" 0;
  (* A name no module defines in a page context is undefined there, not invalid,
     so it neither warns nor is dropped - the same treatment cascade gives an
     unknown property name in a style rule. *)
  warns_exactly "@page { margin: 1cm; zzz: 1; margin-top: 2cm }" 0;
  warns_exactly "@page { @top-center { content: \"x\"; zzz: 1; margin: 0 } }" 0

(* CSS Properties and Values API 1 sec. 2: [@property] holds the [syntax],
   [inherits] and [initial-value] descriptors, and "unknown descriptors are
   invalid and ignored". Dropping one costs that descriptor, not the
   registration and not the sheet holding it. The discard follows CSS Syntax 3
   sec. 5.4.4 for a declaration - up to the next top-level [;], a [{}] met on
   the way counting as one component value of the value being skipped - and sec.
   5.4.2 for an at-rule, which ends at its block or its [;]. Blink 146 keeps the
   registration in every case below. *)
let spec_lenient_recovery_property_descriptors () =
  let registered =
    "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}"
  in
  let body rest = "@property --x { syntax: \"<length>\"; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @property"
    "@property --x { zzz: 1; syntax: \"<length>\"; inherits: false; \
     initial-value: 0px }"
    registered 1;
  lenient_recover "unknown descriptor mid-body in @property"
    (body "zzz: 1; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "unknown descriptor last in @property"
    (body "inherits: false; initial-value: 0px; zzz: 1")
    registered 1;
  lenient_recover "curly block inside a dropped descriptor value"
    (body "zzz: {1}; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "two curly blocks in a dropped descriptor value are one skip"
    (body "zzz: {1}{2}; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "tokens after a curly block in a dropped descriptor value"
    (body "zzz: {1} 2px; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "a run of stray tokens is dropped like a bad descriptor"
    (body "!!!; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "selector-shaped item in @property is dropped alone"
    (body ".a { b: c }; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "at-rule in @property ends at its block"
    (body "@media screen { color: red } inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "at-rule with no block in @property ends at its semicolon"
    (body "@media screen; inherits: false; initial-value: 0px")
    registered 1;
  (* A descriptor's value is its whole declaration, so anything left over makes
     the declaration invalid rather than the leftover alone. Losing [inherits]
     that way leaves the registration incomplete, which drops it - as it does in
     Blink. *)
  lenient_recover "trailing tokens invalidate the descriptor they follow"
    (body "inherits: false bogus; initial-value: 0px")
    "" 1;
  lenient_recover "an important flag invalidates the descriptor it follows"
    (body "inherits: false !important; initial-value: 0px")
    "" 1;
  lenient_recover "a later descriptor still overrides the one dropped"
    (body "inherits: false !important; initial-value: 0px; inherits: true")
    "@property --x{syntax:\"<length>\";inherits:true;initial-value:0px}" 1

(* CSS Syntax 3 sec. 5.4.3 "consume a block's contents" discards a [;] that no
   declaration precedes rather than validating one, so a stray semicolon in an
   [@property] body costs nothing. Blink 146 reads all three of these. *)
let spec_property_skips_stray_semicolons () =
  let registered =
    "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}"
  in
  lenient_recover "leading semicolon in @property"
    "@property --x { ; syntax: \"<length>\"; inherits: false; initial-value: \
     0px }"
    registered 0;
  lenient_recover "doubled semicolon in @property"
    "@property --x { syntax: \"<length>\";; inherits: false; initial-value: \
     0px }"
    registered 0;
  lenient_recover "trailing semicolon in @property"
    "@property --x { syntax: \"<length>\"; inherits: false; initial-value: \
     0px; }"
    registered 0

(* Each dropped [@property] descriptor reports once, and [~strict:true] rejects
   exactly the inputs the lenient parse warned about. *)
let spec_property_recovery_warns_once_per_descriptor () =
  let body rest = "@property --x { syntax: \"<length>\"; " ^ rest ^ " }" in
  warns_exactly (body "zzz: 1; inherits: false; initial-value: 0px") 1;
  warns_exactly (body "zzz: {1}{2}; inherits: false; initial-value: 0px") 1;
  warns_exactly (body "zzz: 1; yyy: 2; inherits: false; initial-value: 0px") 2;
  warns_exactly (body "inherits: false; initial-value: 0px") 0;
  warns_exactly (body ";; inherits: false; initial-value: 0px") 0

(* A rule that fails to parse inside a grouping at-rule's block ends where CSS
   Syntax 3 says its kind ends: an at-rule at its block or its [;] (sec. 5.4.2),
   a qualified rule at its block (sec. 5.4.3). A [(] or a [[] met before that
   block is a component value of the prelude, so the rule being discarded runs
   on past it. Stopping there instead would leave the tail of the prelude to be
   read as a rule of its own, and Blink 146 keeps no such rule: for each input
   below it reads back only the rule named in the expectation. *)
let spec_lenient_recovery_block_statements () =
  lenient_recover "bad @supports prelude in @media ends at its block"
    "@media screen { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "bad @supports prelude in @layer ends at its block"
    "@layer base { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    "@layer base{b{color:#00f}}" 1;
  lenient_recover "bad @supports prelude in @supports ends at its block"
    "@supports (color: red) { @supports (display: grid) bogus { a { color: red \
     } } b { color: blue } }"
    "@supports(color:red){b{color:#00f}}" 1;
  lenient_recover "bad @media prelude in @media ends at its block"
    "@media screen { @media (min-width: 1px) and { a { color: red } } b { \
     color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "bad @container prelude in @media ends at its block"
    "@media screen { @container (min-width: 1px) !! { a { color: red } } b { \
     color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "at-rule with no block in @media ends at its semicolon"
    "@media screen { @supports (display: grid) bogus; b { color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "bad selector holding a [] in @media ends at its block"
    "@media screen { a[href=] { color: red } p { color: blue } }"
    "@media screen{p{color:#00f}}" 1;
  lenient_recover "bad selector holding a [] in @layer ends at its block"
    "@layer base { a[href=] { color: red } p { color: blue } }"
    "@layer base{p{color:#00f}}" 1;
  lenient_recover "bad selector opening on a () in @media ends at its block"
    "@media screen { (foo) bar { a { color: red } } b { color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "two bad statements in one block cost only themselves"
    "@media screen { @supports (display: grid) bogus { a { color: red } } \
     @container (min-width: 1px) !! { c { color: lime } } b { color: blue } }"
    "@media screen{b{color:#00f}}" 2

(* Each statement dropped from a grouping at-rule's block reports once: the tail
   of its prelude is part of what is discarded, not a second rule to be read and
   rejected on its own. *)
let spec_block_recovery_warns_once_per_statement () =
  warns_exactly
    "@media screen { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    1;
  warns_exactly
    "@media screen { @container (min-width: 1px) !! { a { color: red } } b { \
     color: blue } }"
    1;
  warns_exactly
    "@media screen { @supports (display: grid) bogus; b { color: blue } }" 1;
  warns_exactly "@media screen { a[href=] { color: red } p { color: blue } }" 1;
  warns_exactly "@layer base { a[href=] { color: red } p { color: blue } }" 1;
  warns_exactly
    "@media screen { @supports (display: grid) bogus { a { color: red } } \
     @container (min-width: 1px) !! { c { color: lime } } b { color: blue } }"
    2;
  warns_exactly
    "@media screen { @supports (display: grid) { a { color: red } } b { color: \
     blue } }"
    0

(* CSS Counter Styles 3 sec. 3 gives [@counter-style] a block of descriptor
   declarations, so CSS Syntax 3 sec. 5.4.3 keeps what that block already
   yielded when one item fails to parse: a descriptor cascade rejects costs that
   descriptor, not the [@counter-style] holding it and not the sheet holding
   that. The discard follows sec. 5.4.4 for a declaration - up to the next
   top-level [;], a [{}] met on the way counting as one component value of the
   value being skipped - and sec. 5.4.2 for an at-rule, which ends at its block
   or its [;]. Blink 146 keeps the rule in every case below. *)
let spec_lenient_recovery_counter_style_descriptors () =
  let recovered =
    "@counter-style c{system:cyclic;symbols:\"a\";suffix:\" \"}"
  in
  let shorter = "@counter-style c{system:cyclic;symbols:\"a\"}" in
  let body rest = "@counter-style c { system: cyclic; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @counter-style"
    "@counter-style c { zzz: 1; system: cyclic; symbols: \"a\"; suffix: \" \" }"
    recovered 1;
  lenient_recover "unknown descriptor mid-body in @counter-style"
    (body "zzz: 1; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover "unknown descriptor last in @counter-style"
    (body "symbols: \"a\"; suffix: \" \"; zzz: 1")
    recovered 1;
  lenient_recover "bad value for a known @counter-style descriptor"
    (body "symbols: \"a\"; suffix: !!!")
    shorter 1;
  lenient_recover "two curly blocks in a dropped counter-style value"
    (body "zzz: {1}{2}; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  (* A descriptor's value is the whole of its declaration, and no descriptor
     takes an [!important] flag, so the declaration it follows is invalid rather
     than the flag alone. *)
  lenient_recover "an important flag invalidates the counter-style descriptor"
    (body "symbols: \"a\"; suffix: \" \" !important")
    shorter 1;
  lenient_recover "an important flag on an opaque counter-style descriptor"
    (body "symbols: \"a\"; pad: 2 \"0\" !important; suffix: \" \"")
    recovered 1;
  lenient_recover "at-rule in @counter-style ends at its block"
    (body "@media screen { color: red } symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover
    "at-rule with no block in @counter-style ends at its semicolon"
    (body "@media screen; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover "selector-shaped item in @counter-style with a semicolon"
    (body ".a { b: c }; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  (* Sec. 5.4.4 skips a bad declaration to the next top-level [;], and a
     selector-shaped item has none of its own, so it takes the tail of the body
     with it. Blink 146 splits the two the same way. *)
  lenient_recover "selector-shaped item in @counter-style takes the tail"
    (body "symbols: \"a\"; .a { b: c } suffix: \" \"")
    shorter 1;
  lenient_recover "a dropped counter-style descriptor keeps the sheet whole"
    (".a { color: red } "
    ^ body "zzz: 1; symbols: \"a\"; suffix: \" \""
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS Fonts 4 sec. 12.1 gives [@font-palette-values] a block of descriptor
   declarations, so one descriptor cascade rejects costs that descriptor alone.
   A descriptor's value is the whole of its declaration: a trailing [!important]
   or a stray ident makes the declaration invalid rather than the leftover
   alone, as it does in Blink 146. *)
let spec_lenient_recovery_font_palette_descriptors () =
  let recovered = "@font-palette-values --p{font-family:X;base-palette:1}" in
  let body rest = "@font-palette-values --p { font-family: X; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @font-palette-values"
    "@font-palette-values --p { zzz: 1; font-family: X; base-palette: 1 }"
    recovered 1;
  lenient_recover "unknown descriptor mid-body in @font-palette-values"
    (body "zzz: 1; base-palette: 1")
    recovered 1;
  lenient_recover "unknown descriptor last in @font-palette-values"
    (body "base-palette: 1; zzz: 1")
    recovered 1;
  lenient_recover "bad value for a known @font-palette-values descriptor"
    (body "base-palette: -1; base-palette: 1")
    recovered 1;
  lenient_recover "two curly blocks in a dropped font-palette value"
    (body "zzz: {1}{2}; base-palette: 1")
    recovered 1;
  lenient_recover "trailing tokens invalidate the font-palette descriptor"
    (body "base-palette: 2 bogus; base-palette: 1")
    recovered 1;
  lenient_recover "an important flag invalidates the font-palette descriptor"
    (body "base-palette: 2 !important; base-palette: 1")
    recovered 1;
  lenient_recover "at-rule in @font-palette-values ends at its block"
    (body "@media screen { color: red } base-palette: 1")
    recovered 1;
  lenient_recover
    "at-rule with no block in @font-palette-values ends at its semicolon"
    (body "@media screen; base-palette: 1")
    recovered 1;
  lenient_recover
    "selector-shaped item in @font-palette-values with a semicolon"
    (body ".a { b: c }; base-palette: 1")
    recovered 1;
  lenient_recover "a dropped font-palette descriptor keeps the sheet whole"
    (".a { color: red } "
    ^ body "zzz: 1; base-palette: 1"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS View Transitions 2 sec. 2.3.1 gives [@view-transition] a block of
   descriptor declarations, so one descriptor cascade rejects costs that
   descriptor alone. Blink 146 keeps the rule in every case below. *)
let spec_lenient_recovery_view_transition_descriptors () =
  let recovered = "@view-transition{navigation:auto}" in
  let both = "@view-transition{navigation:auto;types:a}" in
  lenient_recover "unknown descriptor first in @view-transition"
    "@view-transition { zzz: 1; navigation: auto }" recovered 1;
  lenient_recover "unknown descriptor mid-body in @view-transition"
    "@view-transition { navigation: auto; zzz: 1; types: a }" both 1;
  lenient_recover "unknown descriptor last in @view-transition"
    "@view-transition { navigation: auto; zzz: 1 }" recovered 1;
  lenient_recover "bad value for a known @view-transition descriptor"
    "@view-transition { navigation: bogus; navigation: auto }" recovered 1;
  lenient_recover "two curly blocks in a dropped view-transition value"
    "@view-transition { zzz: {1}{2}; navigation: auto }" recovered 1;
  lenient_recover "trailing tokens invalidate the view-transition descriptor"
    "@view-transition { navigation: none bogus; navigation: auto }" recovered 1;
  lenient_recover "an important flag invalidates the view-transition descriptor"
    "@view-transition { navigation: none !important; navigation: auto }"
    recovered 1;
  lenient_recover "at-rule in @view-transition ends at its block"
    "@view-transition { @media screen { color: red } navigation: auto }"
    recovered 1;
  lenient_recover
    "at-rule with no block in @view-transition ends at its semicolon"
    "@view-transition { @media screen; navigation: auto }" recovered 1;
  lenient_recover "selector-shaped item in @view-transition with a semicolon"
    "@view-transition { .a { b: c }; navigation: auto }" recovered 1;
  lenient_recover "a dropped view-transition descriptor keeps the sheet whole"
    ".a { color: red } @view-transition { zzz: 1; navigation: auto } .z { \
     color: lime }"
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS Fonts 4 sec. 11.1 fills an [@font-feature-values] body with feature value
   blocks, so it is a rule list: CSS Syntax 3 sec. 5.4.2 ends the block cascade
   rejects at its own [{}] or [;], leaving the blocks written around it in the
   rule. A [;] with no rule before it is discarded with nothing to validate
   (sec. 5.4.3), so it costs nothing. *)
let spec_lenient_recovery_font_feature_values_blocks () =
  let recovered = "@font-feature-values Xf{@swash{s:1}@ornaments{o:2}}" in
  let body rest = "@font-feature-values Xf { @swash { s: 1 } " ^ rest ^ " }" in
  lenient_recover "unknown block first in @font-feature-values"
    "@font-feature-values Xf { @zzz { a: 1 } @swash { s: 1 } @ornaments { o: 2 \
     } }"
    recovered 1;
  lenient_recover "unknown block mid-body in @font-feature-values"
    (body "@zzz { a: 1 } @ornaments { o: 2 }")
    recovered 1;
  lenient_recover "unknown block last in @font-feature-values"
    (body "@ornaments { o: 2 } @zzz { a: 1 }")
    recovered 1;
  lenient_recover
    "at-rule with no block in @font-feature-values ends at its semicolon"
    (body "@zzz; @ornaments { o: 2 }")
    recovered 1;
  lenient_recover
    "selector-shaped item in @font-feature-values ends at its block"
    (body ".a { b: c } @ornaments { o: 2 }")
    recovered 1;
  lenient_recover
    "declaration in an @font-feature-values body ends at its semicolon"
    (body "color: red; @ornaments { o: 2 }")
    recovered 1;
  lenient_recover "stray semicolon in @font-feature-values costs nothing"
    (body "; @ornaments { o: 2 }")
    recovered 0;
  lenient_recover "leading semicolon in @font-feature-values costs nothing"
    "@font-feature-values Xf { ; @swash { s: 1 } @ornaments { o: 2 } }"
    recovered 0;
  lenient_recover "a dropped feature block keeps the sheet whole"
    (".a { color: red } "
    ^ body "@zzz { a: 1 } @ornaments { o: 2 }"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* Each dropped descriptor reports once, and [~strict:true] rejects exactly the
   inputs the lenient parse warned about. *)
let spec_descriptor_recovery_warns_once_per_descriptor () =
  warns_exactly "@counter-style c { system: cyclic; zzz: 1; symbols: \"a\" }" 1;
  warns_exactly
    "@counter-style c { system: cyclic; zzz: {1}{2}; symbols: \"a\" }" 1;
  warns_exactly
    "@counter-style c { system: cyclic; zzz: 1; yyy: 2; symbols: \"a\" }" 2;
  warns_exactly "@counter-style c { system: cyclic; symbols: \"a\" }" 0;
  warns_exactly "@counter-style c { ;; system: cyclic; symbols: \"a\" }" 0;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: 1; base-palette: 1 }" 1;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: {1}{2}; base-palette: 1 }"
    1;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: 1; yyy: 2; base-palette: \
     1 }"
    2;
  warns_exactly "@font-palette-values --p { font-family: X; base-palette: 1 }" 0;
  warns_exactly "@view-transition { zzz: 1; navigation: auto }" 1;
  warns_exactly "@view-transition { zzz: {1}{2}; navigation: auto }" 1;
  warns_exactly "@view-transition { zzz: 1; yyy: 2; navigation: auto }" 2;
  warns_exactly "@view-transition { navigation: auto }" 0;
  warns_exactly "@view-transition { ;; navigation: auto }" 0;
  warns_exactly "@font-feature-values Xf { @zzz { a: 1 } @swash { s: 1 } }" 1;
  warns_exactly
    "@font-feature-values Xf { @zzz { a: 1 } @yyy { b: 2 } @swash { s: 1 } }" 2;
  warns_exactly "@font-feature-values Xf { @swash { s: 1 } }" 0;
  warns_exactly "@font-feature-values Xf { ;; @swash { s: 1 } }" 0

(* CSS Fonts 4 sec. 9.2 names the one mandatory descriptor:
   "@font-palette-values rules require a font-family descriptor; if it is
   missing, the @font-palette-values rule is invalid and must be ignored
   entirely." base-palette is optional, and sec. 9.2.2 gives it a default: "If
   this descriptor is not present in the @font-palette-values, [...] it behaves
   as if 0 were specified." Both sections read the same in the Editor's Draft
   and in the TR Working Draft of 25 August 2026. *)
let spec_font_palette_values_requires_font_family () =
  warns_exactly
    "@font-palette-values --p { font-family: A; override-colors: 0 red }" 0;
  warns_exactly "@font-palette-values --p { font-family: A }" 0;
  warns_exactly
    "@font-palette-values --p { base-palette: 1; override-colors: 0 red }" 1;
  warns_exactly "@font-palette-values --p { override-colors: 0 red }" 1

(* CSS Animations 1 sec. 3 fills a [@keyframes] body with keyframe rules, so it
   is a list of rules: an at-rule has no place there, and CSS Syntax 3 sec.
   5.4.2 ends the one being discarded at its own [{}] block or at its [;],
   leaving the keyframes written around it in the animation. A [(] or a [[] met
   in the prelude on the way is a component value of that prelude, not an end.
   Blink 146 keeps both neighbours in every case below. *)
let spec_lenient_recovery_keyframes_at_rule () =
  let recovered = "@keyframes k{0%{color:red}50%{background:#0f0}}" in
  let body rest = "@keyframes k { " ^ rest ^ " }" in
  lenient_recover "at-rule first in @keyframes"
    (body
       "@media print { to { color: pink } } from { color: red } 50% { \
        background: lime }")
    recovered 1;
  lenient_recover "at-rule mid-body in @keyframes"
    (body
       "from { color: red } @media print { to { color: pink } } 50% { \
        background: lime }")
    recovered 1;
  lenient_recover "at-rule last in @keyframes"
    (body
       "from { color: red } 50% { background: lime } @media print { to { \
        color: pink } }")
    recovered 1;
  lenient_recover "at-rule with no block in @keyframes ends at its semicolon"
    (body "from { color: red } @zzz; 50% { background: lime }")
    recovered 1;
  lenient_recover "a paren in the prelude of a dropped @keyframes at-rule"
    (body
       "from { color: red } @media (min-width: 1px) { to { color: pink } } 50% \
        { background: lime }")
    recovered 1;
  lenient_recover "a bracket in the prelude of a dropped @keyframes at-rule"
    (body
       "from { color: red } @zzz [a] { to { color: pink } } 50% { background: \
        lime }")
    recovered 1;
  lenient_recover "two at-rules in @keyframes are dropped one at a time"
    (body
       "from { color: red } @media print { a: b } @supports (display: grid) { \
        b: c } 50% { background: lime }")
    recovered 2;
  (* The animation outlives its only item, as it does in Blink 146: what is left
     is a [@keyframes] that animates nothing. *)
  lenient_recover "at-rule alone in @keyframes"
    (body "@media print { to { color: pink } }")
    "@keyframes k{}" 1;
  lenient_recover "a dropped at-rule in @keyframes keeps the sheet whole"
    (".a { color: red } "
    ^ body
        "from { color: red } @media print { to { color: pink } } 50% { \
         background: lime }"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* Each dropped keyframe rule reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. *)
let spec_keyframes_recovery_warns_once_per_rule () =
  let body rest = "@keyframes k { " ^ rest ^ " }" in
  warns_exactly
    (body
       "from { color: red } @media print { to { color: pink } } 50% { \
        background: lime }")
    1;
  warns_exactly
    (body
       "from { color: red } @media print { a: b } @supports (display: grid) { \
        b: c } 50% { background: lime }")
    2;
  warns_exactly (body "from { color: red } @zzz; 50% { background: lime }") 1;
  warns_exactly (body "from { color: red } 50% { background: lime }") 0

let stylesheet_tests =
  [
    (* Core type tests *)
    ("rule", `Quick, test_rule);
    ("stylesheet", `Quick, test_stylesheet);
    ("rule creation", `Quick, test_rule_creation);
    ("media rule creation", `Quick, test_media_rule_creation);
    ("container rule creation", `Quick, test_container_rule_creation);
    ("nested container recovers", `Quick, test_nested_container_recovers);
    ("invalid rule in a block recovers", `Quick, test_layer_rule_recovery);
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
    ("font-family descriptor grammar", `Quick, font_family_descriptor_grammar);
    ("spec keyframes selector matrix", `Quick, spec_keyframes_selector_matrix);
    ("spec keyframes shadow colour var", `Quick, spec_keyframes_shadow_color_var);
    ("page", `Quick, page_case);
    ("page margin edges", `Quick, page_margin_edges);
    ( "spec page margin descriptor matrix",
      `Quick,
      spec_page_margin_descriptor_matrix );
    ("spec page context properties", `Quick, spec_page_context_properties);
    ("spec page descriptor importance", `Quick, spec_page_descriptor_importance);
    ("spec page margin box empty", `Quick, spec_page_margin_box_empty);
    ("property rule edges", `Quick, property_rule_edges);
    ("spec property descriptor matrix", `Quick, spec_property_descriptor_matrix);
    ("sheet_item", `Quick, sheet_item_case);
    ("ordering", `Quick, ordering);
    (* CSS parsing tests *)
    ("read basic", `Quick, test_read_stylesheet_basic);
    ("read multiple rules", `Quick, test_read_stylesheet_multiple_rules);
    ("read empty", `Quick, test_read_stylesheet_empty);
    ("read whitespace only", `Quick, test_read_stylesheet_whitespace_only);
    ("read with comments", `Quick, test_read_stylesheet_with_comments);
    ( "spec strict accepts valid stylesheets",
      `Quick,
      spec_strict_accepts_valid_stylesheets );
    ( "spec strict rejects invalid stylesheets",
      `Quick,
      spec_strict_rejects_invalid_stylesheets );
    ( "spec lenient recovery stylesheets",
      `Quick,
      spec_lenient_recovery_stylesheets );
    ( "spec lenient recovery in a nested at-rule",
      `Quick,
      spec_lenient_recovery_nested_at_rule_declarations );
    ( "spec lenient recovery keeps a nested declaration run whole",
      `Quick,
      spec_lenient_recovery_nested_declaration_run );
    ( "spec recovery warns once per dropped declaration",
      `Quick,
      spec_recovery_warns_once_per_declaration );
    ( "spec lenient recovery in a @page body",
      `Quick,
      spec_lenient_recovery_page_descriptors );
    ( "spec lenient recovery in a page margin box",
      `Quick,
      spec_lenient_recovery_page_margin_box );
    ( "spec lenient recovery keeps a @page body in order",
      `Quick,
      spec_lenient_recovery_page_body_order );
    ( "spec page recovery warns once per dropped descriptor",
      `Quick,
      spec_page_recovery_warns_once_per_descriptor );
    ( "spec lenient recovery in an @property body",
      `Quick,
      spec_lenient_recovery_property_descriptors );
    ( "spec @property skips stray semicolons",
      `Quick,
      spec_property_skips_stray_semicolons );
    ( "spec property recovery warns once per dropped descriptor",
      `Quick,
      spec_property_recovery_warns_once_per_descriptor );
    ( "spec lenient recovery in a grouping at-rule block",
      `Quick,
      spec_lenient_recovery_block_statements );
    ( "spec block recovery warns once per dropped statement",
      `Quick,
      spec_block_recovery_warns_once_per_statement );
    ( "spec lenient recovery in a @counter-style body",
      `Quick,
      spec_lenient_recovery_counter_style_descriptors );
    ( "spec lenient recovery in a @font-palette-values body",
      `Quick,
      spec_lenient_recovery_font_palette_descriptors );
    ( "spec lenient recovery in a @view-transition body",
      `Quick,
      spec_lenient_recovery_view_transition_descriptors );
    ( "spec lenient recovery in a @font-feature-values body",
      `Quick,
      spec_lenient_recovery_font_feature_values_blocks );
    ( "spec descriptor recovery warns once per dropped descriptor",
      `Quick,
      spec_descriptor_recovery_warns_once_per_descriptor );
    ( "spec font-palette-values requires font-family",
      `Quick,
      spec_font_palette_values_requires_font_family );
    ( "spec lenient recovery in a @keyframes body",
      `Quick,
      spec_lenient_recovery_keyframes_at_rule );
    ( "spec keyframes recovery warns once per dropped rule",
      `Quick,
      spec_keyframes_recovery_warns_once_per_rule );
  ]

(* Tests for newly added check functions *)
(* Not a roundtrip test *)
let test_check () =
  (* Test basic stylesheet parsing using check function *)
  check ~expected:".test{color:red}" ".test { color: red }";
  assert_minify_and_optimize "@media screen { .test { color: blue } }"
    ~minified:"@media screen{.test{color:blue}}"
    ~optimized:"@media screen{.test{color:#00f}}"

let test_import_rule () =
  check_import_rule ~expected:"@import\"test.css\";" "@import 'test.css';";
  check_import_rule ~expected:"@import\"styles.css\"screen;"
    "@import url('styles.css') screen;";

  (* Test invalid import rules *)
  neg_cursor read_import_rule "@import";
  (* Missing URL *)
  neg_cursor read_import_rule "@import test.css";
  (* Missing quotes *)
  neg_cursor read_import_rule "import 'test.css'";
  (* Missing @ *)
  (* Unclosed quote at EOF -- per CSS Syntax sec. 4.3.5 the lexer still returns a
     string-token (the ill-formedness is a parse-error warning, not a
     token-level failure), so [\@import 'test.css] parses as a valid import. *)
  check_import_rule ~expected:"@import\"test.css\";" "@import 'test.css"

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*]. The
   idents are the name; a [.] one of them carries is written back escaped (CSS
   Syntax 3 sec. 2.1) and only a bare [.] separates two. A CSS-wide keyword is
   reserved, in any ident of the name. *)
let test_layer_name () =
  check_layer_name ~roundtrip:true "a";
  check_layer_name ~roundtrip:true "a.b";
  check_layer_name ~roundtrip:true ~expected:"a\\.b" "a\\2e b";
  check_layer_name ~roundtrip:true ~expected:"a\\.b.c" "a\\2e b.c";
  check_layer_name ~roundtrip:true ~expected:"a.b\\.c" "a.b\\2e c";
  check_layer_name ~roundtrip:true ~expected:"a\\;b" "a\\3b b";
  neg_cursor read_layer_name "initial";
  neg_cursor read_layer_name "a.revert-layer";
  neg_cursor read_layer_name ".a";
  neg_cursor read_layer_name "a."

(* Not a roundtrip test *)
let test_advanced_selectors () =
  assert_minify_and_optimize ".btn:hover { color: blue; }"
    ~minified:".btn:hover{color:blue}" ~optimized:".btn:hover{color:#00f}";
  check_stylesheet ~expected:".btn:before{content:\"icon\"}"
    ".btn::before { content: 'icon'; }";
  (* Attribute values that are valid identifiers get normalized to unquoted
     form *)
  assert_minify_and_optimize ".btn[data-type='primary'] { background: blue; }"
    ~minified:".btn[data-type=primary]{background:blue}"
    ~optimized:".btn[data-type=primary]{background:#00f}";
  check_stylesheet ~expected:".parent>.child{margin:0}"
    ".parent > .child { margin: 0; }";
  check_stylesheet ~expected:".sibling+.next{padding:10px}"
    ".sibling + .next { padding: 10px; }";
  check_stylesheet ~expected:".element~.general-sibling{color:red}"
    ".element ~ .general-sibling { color: red; }"

(* Not a roundtrip test *)
let test_advanced_properties () =
  check_stylesheet ~expected:".box{transform:rotate(45deg)scale(1.2)}"
    ".box { transform: rotate(45deg) scale(1.2); }";
  check_stylesheet ~expected:".grid{display:grid;grid-template-columns:1fr 2fr}"
    ".grid { display: grid; grid-template-columns: 1fr 2fr; }";
  check_stylesheet ~expected:".flex{display:flex;justify-content:space-between}"
    ".flex { display: flex; justify-content: space-between; }";
  (* pp serializes the modern rgb() form (rgba unifies into rgb, commas ->
     spaces, alpha via slash); optimize cross-folds to the shorter 4-digit
     hex. *)
  assert_minify_and_optimize
    ".shadow { box-shadow: 0 4px 8px rgba(0,0,0,0.2); }"
    ~minified:".shadow{box-shadow:0 4px 8px rgb(0 0 0/.2)}"
    ~optimized:".shadow{box-shadow:0 4px 8px #0003}";
  (* "to right" is a <side-or-corner>, a distinct node from the <angle> 90deg
     (corners like "to top right" are not fixed angles), so pp holds the
     authored keyword; optimize folds the side to 90deg and Named blue to
     #00f. *)
  assert_minify_and_optimize
    ".gradient { background: linear-gradient(to right, red, blue); }"
    ~minified:".gradient{background:linear-gradient(to right,red,blue)}"
    ~optimized:".gradient{background:linear-gradient(90deg,red,#00f)}"

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
  check_stylesheet ~expected:".minmax{grid-template-columns:minmax(200px,1fr)}"
    ".minmax { grid-template-columns: minmax(200px, 1fr); }"

(* Not a roundtrip test *)
let test_nested_rules () =
  check_stylesheet
    ~expected:
      "@media(min-width:768px){@supports(display:grid){.grid{display:grid}}}"
    "@media (min-width: 768px) { @supports (display: grid) { .grid { display: \
     grid; } } }";
  check_stylesheet
    ~expected:"@layer base{@media print{.print-only{display:block}}}"
    "@layer base { @media print { .print-only { display: block; } } }";
  check_stylesheet
    ~expected:
      "@container(width>400px){@media(orientation:landscape){.landscape{color:green}}}"
    "@container (width > 400px) { @media (orientation: landscape) { .landscape \
     { color: green; } } }"

(** Negative tests for invalid CSS *)
let expect_parse_error input =
  let r = Cursor.of_string input in
  try
    let _ = read r in
    Alcotest.failf "Expected parse error for: %s" input
  with Cursor.Parse_error _ | Error.Parse_error _ -> ()

(* Not a roundtrip test *)
let spec_s7_block_examples () =
  (* CSS Syntax Level 3 section 7.1: these productions are parsed as generic
     block contents, then validated by the rule grammar that owns the block. *)
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  assert_minify_and_optimize
    "p > a { color: blue; text-decoration: underline; }"
    ~minified:"p>a{color:blue;text-decoration:underline}"
    ~optimized:"p>a{color:#00f;text-decoration:underline}";
  check_stylesheet
    ~expected:"@font-face{font-family:MyFont;src:url(font.woff2)}"
    "@font-face { font-family: MyFont; src: url(font.woff2); }";
  check_stylesheet ~expected:"@page:left{margin-left:4cm;margin-right:3cm}"
    "@page :left { margin-left: 4cm; margin-right: 3cm; }";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}to{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  assert_minify_and_optimize ".card { color: red; & .title { color: blue; } }"
    ~minified:".card{color:red;& .title{color:blue}}"
    ~optimized:".card{color:red;.title{color:#00f}}";
  expect_parse_error "@media print { color: red; body { font-size: 10pt } }";
  check_stylesheet ~expected:"@keyframes slide{50%{opacity:1}}"
    "@keyframes slide { color: red; 50% { opacity: 1 } }";
  expect_parse_error "@font-face { .x { color: red } }"

(* Not a roundtrip test *)
let spec_s8_rule_shapes () =
  (* CSS Syntax Level 3 sections 8.1 and 8.2: top-level qualified rules are
     style rules, and at-rules are either statement or block rules depending on
     whether they end with a semicolon or a {} block. *)
  assert_minify_and_optimize "p > a { color: blue }" ~minified:"p>a{color:blue}"
    ~optimized:"p>a{color:#00f}";
  check_stylesheet ~expected:"@import\"theme.css\";" "@import \"theme.css\";";
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  check_stylesheet ~expected:"@layer reset,base;" "@layer reset, base;";
  expect_parse_error "p > a";
  expect_parse_error "@import \"theme.css\" .a { color: red }";
  expect_parse_error "@media print"

let spec_namespace_serialization () =
  (* CSS Namespaces: minified prefixed namespace rules can omit whitespace
     between the prefix ident and URL token while preserving token boundaries.
     URL tokens use the shorter string spelling when possible. *)
  check_stylesheet ~expected:"@namespace svg\"http://www.w3.org/2000/svg\";"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  check_stylesheet
    ~expected:"@namespace math\"http://www.w3.org/1998/Math/MathML\";"
    "@namespace math \"http://www.w3.org/1998/Math/MathML\";";
  check_stylesheet ~expected:"@namespace \"http://www.w3.org/1999/xhtml\";"
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
  (* CSS Syntax sec. 2.2: unclosed blocks auto-close at EOF and the inner
     declaration is preserved. Verify the AST, don't just accept "didn't
     crash". *)
  check_stylesheet ~expected:".btn{color:red}" ".btn { color: red ";
  expect_parse_error ".btn color: red; }";
  expect_parse_error "{ color: red; }";
  expect_parse_error ".btn { : red; }";
  expect_parse_error ".btn { color red; }"

(* Not a roundtrip test *)
let test_invalid_at_rules () =
  check_stylesheet ~expected:"@media{.btn{color:red}}"
    "@media { .btn { color: red; } }";
  expect_parse_error "@property { syntax: 'color'; inherits: true; }";
  expect_parse_error "@property --var { invalid-descriptor: value; }";
  expect_parse_error "@keyframes { 0% { opacity: 0; } }"

(* Not a roundtrip test *)
let css_syntax_recovery () =
  let check_strict name css expected =
    match Css.of_string ~strict:true css with
    | Ok { Css.stylesheet; warnings = _; _ } ->
        Alcotest.(check string)
          (name ^ " stylesheet") expected
          (minify stylesheet |> String.trim)
    | Error err ->
        Alcotest.failf "%s should parse strictly: %s" name
          (Cascade.Error.to_string err)
  in
  let check_recovery name css expected min_warnings =
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false css with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s did not recover leniently: %s" name
            (Cascade.Error.to_string err)
    in
    Alcotest.(check string)
      (name ^ " stylesheet") expected
      (minify stylesheet |> String.trim);
    Alcotest.(check bool)
      (name ^ " warning count") true
      (List.length warnings >= min_warnings)
  in
  (* Syntactically valid unknown declarations are preserved (browsers do, and
     dropping changes the stylesheet for vendor properties / future spec
     additions / properties Cascade just doesn't model). Only malformed or
     known-but-invalid declarations are dropped (see [invalid declaration] case
     below). *)
  check_strict "unknown declaration"
    ".btn { unknown-property: value; color: red; }"
    ".btn{unknown-property:value;color:red}";
  check_recovery "invalid declaration"
    ".btn { color: invalid-color; color: red; }" ".btn{color:red}" 1;
  check_recovery "invalid selector list"
    ".ok { color: green } .bad:not() { color: red }" ".ok{color:green}" 1;
  check_recovery "unknown at-rule"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    "@unknown-rule{.bad{color:red}}.ok{color:#00f}" 1

let css_syntax_recovery_structural () =
  let declaration_counts stylesheet =
    Css.rule_statements stylesheet
    |> List.map (fun statement ->
        match Css.as_rule statement with
        | Some (_, declarations, _) -> List.length declarations
        | None -> Alcotest.fail "expected recovered qualified rule")
  in
  let check_counts name css expected_counts min_warnings =
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false css with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s did not recover leniently: %s" name
            (Cascade.Error.to_string err)
    in
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
    ".ok { color: green } .bad:not() { color: red } .next { color: blue }"
    [ 1; 1 ] 1;
  check_counts "unknown at-rule block skipped"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }" [ 1 ] 1;
  check_counts "unclosed block auto-closed"
    ".a { color: red; .b { color: blue }" [ 1 ] 0

(* CSS Syntax 3 sections 4.3.1 and 5.4.2: an unknown at-rule's at-keyword and
   its prelude are separate tokens, and the whitespace between them is the only
   thing that keeps them apart. Minified output that drops it turns [@foo bar]
   into the at-keyword [@foobar], so the printed text no longer re-parses to the
   value that produced it. Lightning CSS, esbuild, csso, clean-css, and cssnano
   all keep the space. An at-rule with no prelude has no boundary to preserve
   and stays unspaced. *)
let s3431_unknown_at_rule_prelude_separator () =
  let parse input = read (Cursor.of_string input) in
  let unknown input =
    match parse input with
    | [ Unknown_at_rule { name; prelude; block } ] -> (name, prelude, block)
    | _ -> Alcotest.failf "expected a single unknown at-rule for %S" input
  in
  let roundtrips input =
    let printed =
      String.trim (Css.Stylesheet.to_string ~minify:true (parse input))
    in
    let at_rule = Alcotest.(triple string string (option string)) in
    Alcotest.check at_rule (input ^ " roundtrip") (unknown input)
      (unknown printed);
    printed
  in
  Alcotest.(check string)
    "block form keeps the at-keyword boundary" "@foo bar{x:1}"
    (roundtrips "@foo bar{x:1}");
  Alcotest.(check string)
    "statement form keeps the at-keyword boundary" "@foo bar;"
    (roundtrips "@foo bar;");
  Alcotest.(check string)
    "multi-token prelude keeps the at-keyword boundary" "@foo bar baz{x:1}"
    (roundtrips "@foo bar baz{x:1}");
  (* Control: no prelude, so no separator to emit. *)
  Alcotest.(check string)
    "block form without a prelude stays unspaced" "@foo{x:1}"
    (roundtrips "@foo{x:1}");
  Alcotest.(check string)
    "statement form without a prelude stays unspaced" "@foo;"
    (roundtrips "@foo;")

(* CSS Syntax 3 sec. 4.3.1: a backslash is the start of an escape unless a
   newline follows it, so a raw body ending on an odd run of backslashes eats
   the [}] written straight after it and the at-rule never closes. Parsing can
   only produce such a body at EOF, where recovery closes the block again and
   hides the damage; the reachable case is a stylesheet that holds a statement
   after the at-rule, where the escape swallows that statement instead.

   A newline is the only separator that repairs it. Sec. 4.3.7 reads a space or
   a hex digit as part of the escape, so either one changes the last backslash
   from the delim token it was; a newline cannot be escaped, so the delim stays
   a delim and the closer stays a closer. *)
let s3431_unknown_at_rule_trailing_backslash () =
  let parse input = read (Cursor.of_string input) in
  let printed sheet =
    String.trim (Css.Stylesheet.to_string ~minify:true sheet)
  in
  let at_rule body =
    Unknown_at_rule { name = "o"; prelude = "x"; block = Some body }
  in
  let survives name body =
    let sheet = [ at_rule body; List.hd (parse ".b{color:red}") ] in
    Alcotest.(check int)
      (name ^ ": the statement after the at-rule survives a round-trip")
      2
      (List.length (parse (printed sheet)))
  in
  survives "one backslash" " a \\";
  survives "three backslashes" " a \\\\\\";
  (* Control: an even run is a complete escape, and the closer after it already
     closes the block. *)
  survives "two backslashes" " a \\\\";
  Alcotest.(check string)
    "a body ending on a delim backslash is closed after a newline"
    "@o x{ a \\\n}"
    (printed [ at_rule " a \\" ]);
  Alcotest.(check string)
    "an escaped backslash needs no separator" "@o x{ a \\\\}"
    (printed [ at_rule " a \\\\" ])

(* CSS Syntax 3 (editor's draft) sec. 5.5.2 "consume an at-rule": an at-rule is
   an at-keyword, then a prelude ending at the first top-level [;] or [{], then
   either that [;] or a block. Sec. 5.5.9 makes the block a balanced token
   sequence ending at its matching [}], and sec. 4.3.3 reads [@] as an
   at-keyword only when an ident follows it.

   [unknown_at_rule] hands those parts to a caller as text, so it answers for
   every boundary: a part carrying one of its own terminators prints a sheet
   that re-consumes to statements nobody wrote, and an empty name prints an [@]
   delim that re-consumes to nothing at all. *)
let s552_unknown_at_rule_constructor () =
  let printed stmt = String.trim (Css.to_string ~minify:true [ stmt ]) in
  let built what = function
    | Ok stmt -> stmt
    | Error e ->
        Alcotest.failf "%s: unexpected error %s" what
          (Cascade.Error.to_string e)
  in
  let rejected what = function
    | Error _ -> ()
    | Ok stmt ->
        Alcotest.failf "%s: expected an error, built %S" what (printed stmt)
  in
  let parts stmt =
    match Css.of_string_exn (printed stmt) with
    | [ Unknown_at_rule { name; prelude; block } ] -> (name, prelude, block)
    | other ->
        Alcotest.failf "expected one unknown at-rule, got %d statements"
          (List.length other)
  in
  let at_rule = Alcotest.(triple string string (option string)) in
  let block = Css.of_string_exn ".a{color:red}" in
  (* A block cascade does model reaches the body through its own printer, so a
     caller places one without assembling or re-reading a sheet. *)
  let statements =
    built "printed block body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:(Css.to_string ~minify:true block)
         ())
  in
  Alcotest.(check string)
    "a printed block sits inside the at-rule's block"
    "@utility tab-4{.a{color:red}}" (printed statements);
  Alcotest.check at_rule "a printed block re-consumes to the same at-rule"
    ("utility", "tab-4", Some ".a{color:red}")
    (parts statements);
  (* The body of an at-rule cascade does not model travels whole. *)
  let text =
    built "text body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:".a{color:red}" ())
  in
  Alcotest.check at_rule "a text body re-consumes to the same at-rule"
    ("utility", "tab-4", Some ".a{color:red}")
    (parts text);
  (* Sec. 5.5.2: with no block the at-rule ends at its [;]. *)
  let bodyless =
    built "no body" (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ())
  in
  Alcotest.(check string)
    "an at-rule with no body ends at its semicolon" "@utility tab-4;"
    (printed bodyless);
  Alcotest.check at_rule "an at-rule with no body re-consumes to the same one"
    ("utility", "tab-4", None) (parts bodyless);
  (* Every part that carries a terminator of its own is refused, not printed. *)
  rejected "a prelude holding its own block opener"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4{x:1}"
       ~block:"color:red" ());
  rejected "a prelude holding its own terminator"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4;.evil{color:red}" ());
  rejected "a body closing the block early"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
       ~block:"x} .evil{color:red" ());
  rejected "a body that never closes what it opens"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ~block:".a{color:red"
       ());
  rejected "an empty at-keyword"
    (Css.unknown_at_rule ~name:"" ~prelude:"tab-4" ());
  (* Sec. 4.3.2 "consume comments" runs an unclosed [/*] to EOF, so a part that
     opens one swallows the closer written after it and every statement that
     follows. A closed comment is only text and travels like the rest of the
     body. *)
  let commented =
    built "closed comment body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:"/* keep */color:red" ())
  in
  Alcotest.check at_rule "a closed comment travels inside the body"
    ("utility", "tab-4", Some "/* keep */color:red")
    (parts commented);
  rejected "a body opening a comment it never closes"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ~block:"/* color:red"
       ());
  rejected "a prelude opening a comment it never closes"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4 /*" ());
  (* An at-keyword cascade does have a grammar for reads back as that at-rule,
     so the statement would disagree with the sheet it prints to. Sec. 5.5.2
     dispatches on the at-keyword, and [media] is one this AST already models
     with a typed condition. *)
  rejected "an at-keyword cascade already models"
    (Css.unknown_at_rule ~name:"media" ~prelude:"screen" ~block:".a{color:red}"
       ())

(* The refusal is per at-rule. Assembling a sheet as text and re-parsing it
   gives the whole buffer one error path, so a single malformed body takes every
   other at-rule with it; building each statement on its own loses only the
   malformed one. *)
let s552_unknown_at_rule_error_is_local () =
  let declared =
    List.map
      (fun (prelude, body) ->
        Css.unknown_at_rule ~name:"utility" ~prelude ~block:body ())
      [
        ("tab-4", "color:red");
        ("bad", "x} .evil{color:blue}");
        ("tab-8", "color:lime");
      ]
  in
  let kept = List.filter_map Result.to_option declared in
  Alcotest.(check int) "only the malformed at-rule is lost" 2 (List.length kept);
  Alcotest.(check int)
    "the survivors print a sheet holding both of them" 2
    (List.length (Css.of_string_exn (Css.to_string ~minify:true kept)))

(* CSS Syntax 3 sec. 5.4.2: an unrecognised at-rule has no grammar to
   re-serialise its body from, so the body travels as the source text between
   its braces. That text is what sits between the at-rule's own braces. Taking
   it from the at-rule's child components instead stops at the last one the
   lexer produced, which is inside the closer whenever the body ends in a nested
   block, and the printed at-rule then never closes. Sec. 5.4.6: an unterminated
   nested construct swallows the closer the other way, leaving none in the
   source to exclude and one for the serializer to supply. *)
let s542_unknown_at_rule_block_body () =
  let printed input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  let roundtrips name input expected =
    Alcotest.(check string) name expected (printed input);
    Alcotest.(check string)
      (name ^ " is a fixed point")
      expected (printed expected)
  in
  roundtrips "a body ending in a nested block keeps both closers"
    "@foo test{div{color:red}}" "@foo test{div{color:red}}";
  roundtrips "nested blocks all the way down" "@foo{a{b{c:d}}}"
    "@foo{a{b{c:d}}}";
  roundtrips "an empty body stays empty" "@foo{}" "@foo{}";
  roundtrips "an unterminated function does not stack closers" "@foo{a:(b}"
    "@foo{a:(b)}";
  roundtrips "an unterminated nested block gets one closer" "@foo{a{b:c"
    "@foo{a{b:c}}"

(* CSS Syntax 3 (ED) sec. 5.5.2 "consume an at-rule" reads a prelude and a block
   whatever the at-keyword spells, and cascade keeps both as the source text it
   cannot re-serialise from a grammar. Sec. 4.3.5 returns the string token at
   end of input and 4.3.6 the url token, both calling it a parse error, 4.3.2
   ends the comment there and calls that one too, and sec. 5.5.9 and 5.5.10
   close a simple block and a function on the EOF token. Each defines what end
   of input leaves, so text that stopped mid-construct means the closed form.
   Written back open, that construct swallows the [;] or [}] the at-rule ends
   with, and the next reader sees one at-rule where cascade held an at-rule and
   the rule after it. *)
let s552_unknown_at_rule_eof_closers () =
  let printed input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  (* The at-rule has to end on its own: append a rule to what was printed and
     both must come back, and re-reading must report nothing left
     unterminated. *)
  let self_delimiting name printed =
    let input = String.concat "" [ printed; ".z{color:red}" ] in
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false input with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s: %S failed to reparse: %s" name input
            (Cascade.Error.to_string err)
    in
    Alcotest.(check int)
      (String.concat "" [ name; ": the rule after it survives" ])
      2
      (List.length (Css.statements stylesheet));
    let unterminated =
      List.filter
        (fun (e : Error.t) ->
          match e.Error.kind with Error.Unterminated _ -> true | _ -> false)
        warnings
    in
    Alcotest.(check int)
      (String.concat "" [ name; ": nothing left unterminated" ])
      0 (List.length unterminated)
  in
  let closes name input expected =
    Alcotest.(check string) name expected (printed input);
    Alcotest.(check string)
      (String.concat "" [ name; " is a fixed point" ])
      expected (printed expected);
    self_delimiting name expected
  in
  closes "a string closes at end of input" "@o x{ a \"i" "@o x{ a \"i\"}";
  closes "a single-quoted string closes too" "@o x{ a 'i" "@o x{ a 'i'}";
  closes "a url closes at end of input" "@o x{ a url(i" "@o x{ a url(i)}";
  closes "a bad url closes too" "@o x{ a url(i j" "@o x{ a url(i j)}";
  closes "a function closes at end of input" "@o x{ a f(i" "@o x{ a f(i)}";
  closes "a square block closes at end of input" "@o x{ a [i" "@o x{ a [i]}";
  closes "a paren block closes at end of input" "@o x{ a (i" "@o x{ a (i)}";
  closes "a curly block closes at end of input" "@o x{ a { b" "@o x{ a { b}}";
  closes "a comment ends at end of input" "@o x{ a /* b" "@o x{ a /* b*/}";
  (* The prelude runs to the [;] the at-rule ends with, and swallows it the same
     way. *)
  closes "a prelude string closes at end of input" "@o \"i" "@o \"i\";";
  closes "a prelude url closes at end of input" "@o url(i" "@o url(i);";
  (* Sec. 4.3.6 reads the whitespace before the missing [)] and keeps none of it
     in the url, so the closer takes its place rather than following it: a
     minified stylesheet carries no newline of its own. *)
  closes "whitespace before a missing closer is not carried" "@o url(i\n"
    "@o url(i);";
  closes "a prelude function closes at end of input" "@o f(i" "@o f(i);";
  (* Controls: nothing is open, so nothing is added. *)
  closes "a closed body is untouched" "@o x{ a b }" "@o x{ a b }";
  closes "a quote inside a comment is not an opener" "@o x{ a /* \" */ b"
    "@o x{ a /* \" */ b}";
  closes "an escaped quote is not an opener" "@o x{ a \"i\\\"j\""
    "@o x{ a \"i\\\"j\"}";
  closes "a closed url is not reopened" "@o x{url(a)}" "@o x{url(a)}";
  closes "a closed prelude is untouched" "@o bar" "@o bar;"

(* Gecko's [@document]/[@-moz-document] takes a comma-separated list of [<url> |
   url-prefix(<string>) | domain(<string>) | media-document(<string>) |
   regexp(<string>)]. Cascade modelled only [url-prefix()], so every other form
   failed the prelude read and the at-rule went down with every rule inside it.
   The five minifiers in [test/interop/lightning] all keep the at-rule verbatim.
   A [<string>] argument stays quoted; a [<url>] takes the shortest spelling. *)
let moz_document_prelude_forms () =
  let roundtrips input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  List.iter
    (fun (prelude, expected) ->
      let wrap p =
        String.concat "" [ "@-moz-document "; p; "{.b{color:red}}" ]
      in
      Alcotest.(check string)
        prelude (wrap expected)
        (roundtrips (wrap prelude)))
    [
      ("url-prefix(\"x\")", "url-prefix(\"x\")");
      ("url-prefix()", "url-prefix()");
      ("url(x)", "url(x)");
      ("url(\"x\")", "url(x)");
      ("domain(\"example.com\")", "domain(\"example.com\")");
      ("regexp(\"x\")", "regexp(\"x\")");
      ("media-document(\"video\")", "media-document(\"video\")");
      ("url-prefix(\"a\"),domain(\"b\")", "url-prefix(\"a\"),domain(\"b\")");
    ];
  (* A form outside the grammar still takes the at-rule down, which is what CSS
     Syntax 3 sec. 5.4.2 does with a prelude no grammar accepts. The recovering
     reader is the one that drops it; the raw one raises. *)
  match
    Css.of_string ~strict:false
      ".a{color:red}@-moz-document wibble(\"x\"){.b{color:red}}"
  with
  | Error e -> Alcotest.fail (Error.to_string e)
  | Ok { Css.stylesheet; _ } ->
      Alcotest.(check string)
        "an unknown prelude function drops the at-rule" ".a{color:red}"
        (String.trim (Css.Stylesheet.to_string ~minify:true stylesheet))

(* Not a roundtrip test *)
let test_invalid_functions () =
  expect_parse_error ".btn { color: rgb(300); }";
  expect_parse_error ".btn { transform: rotate(); }";
  expect_parse_error ".btn { width: calc(100% +); }";
  expect_parse_error ".btn { background: url(; }"

(* Not a roundtrip test *)
let test_layer_roundtrip () =
  let test_css ~expected input =
    let r = Cursor.of_string input in
    try
      let stylesheet = Css.Stylesheet.read r in
      let roundtrip =
        String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
      in
      Alcotest.(check string)
        ("layer roundtrip for " ^ input)
        expected roundtrip
    with Cursor.Parse_error err ->
      Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)
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
  neg_cursor read "@layer framework . theme { blockquote { display: block } }";
  neg_cursor read "@layer initial { blockquote { display: block } }";
  neg_cursor read
    "@layer framework.revert-layer { blockquote { display: block } }"

(* Not a roundtrip test *)
let c64_layer_nesting_examples () =
  (* CSS Cascade sections 6.4.2 and 6.4.3: dotted layer names are shorthand for
     nested layer segments; nested names do not escape their parent layer. *)
  let nested_layers =
    "@layer base { p { max-width: 70ch } } @layer framework { @layer base { p \
     { margin-block: 0.75em } } @layer theme { p { color: #222 } } } @layer \
     framework.theme { blockquote { color: rebeccapurple } }"
  in
  check_stylesheet
    ~expected:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:rebeccapurple}}"
    nested_layers;
  assert_minify_and_optimize nested_layers
    ~minified:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:rebeccapurple}}"
    ~optimized:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:#639}}";
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
      "@layer default,theme,components;@import\"theme.css\"layer(theme);@layer \
       default{audio[controls]{display:block}}"
    "@layer default, theme, components; @import url(theme.css) layer(theme); \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet
    ~expected:
      "@layer default;@import\"theme.css\"layer(theme);@layer \
       components;@layer default{audio[controls]{display:block}}"
    "@layer default; @import url(theme.css) layer(theme); @layer components; \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet ~expected:"@layer framework.base,framework.theme;"
    "@layer framework.base, framework.theme;";
  neg_cursor read "@layer;";
  neg_cursor read "@layer , theme;";
  neg_cursor read "@layer default, { audio { display: block } }";
  neg_cursor read "@layer default, theme { audio { display: block } }"

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
  check_import_rule ~expected:"@import\"headings.css\"layer(default);"
    "@import url(headings.css) layer(default);";
  check_import_rule ~expected:"@import\"links.css\"layer(default)screen;"
    "@import url(links.css) layer(default) screen;";
  check_import_rule ~expected:"@import\"theme.css\"layer(framework.theme);"
    "@import url(theme.css) layer(framework.theme);";
  check_import_rule ~expected:"@import\"base-forms.css\"layer;"
    "@import url(base-forms.css) layer;";
  check_import_rule ~expected:"@import\"base-links.css\"layer;"
    "@import url(base-links.css) layer();";
  check_import_rule ~expected:"@import\"conditional.css\"layer print;"
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
  check_import_rule ~expected:"@import\"mystyle.css\";"
    "@import url(mystyle.css);";
  check_import_rule ~expected:"@import\"mystyle.css\";"
    "@import \"mystyle.css\";";
  check_import_rule
    ~expected:"@import\"narrow.css\"supports(display:flex)handheld;"
    "@import url(narrow.css) supports(display: flex) handheld;";
  check_import_rule
    ~expected:"@import\"narrow.css\"supports(display:flex)handheld;"
    "@import url(narrow.css) supports((display: flex)) handheld;";
  check_import_rule
    ~expected:
      "@import\"layout.css\"layer(framework.component)supports(display:grid)screen \
       and (min-width:30em);"
    "@import url(layout.css) layer(framework.component) supports(display: \
     grid) screen and (min-width: 30em);";
  check_import_rule ~expected:"@import\"bluish.css\"projection,tv;"
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
      "@layer reset,theme;@import\"theme.css\"layer(theme);@namespace \
       \"http://www.w3.org/1999/xhtml\";"
    "@charset \"UTF-8\"; @layer reset, theme; @import url(theme.css) \
     layer(theme); @namespace url(http://www.w3.org/1999/xhtml);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme; @import \
     url(components.css) layer(components);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme { .x { color: red } \
     } @import url(components.css) layer(components);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme; @namespace \
     url(http://www.w3.org/1999/xhtml);"

(* Not a roundtrip test *)
let c64_invalid_layer_names () =
  (* CSS Cascade section 6.4.2 reserves CSS-wide keywords in every layer-name
     segment, and the <layer-name> grammar has no empty segments. *)
  List.iter
    (fun keyword ->
      neg_cursor read ("@layer " ^ keyword ^ " { .x { color: red } }");
      neg_cursor read ("@layer framework." ^ keyword ^ " { .x { color: red } }"))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  neg_cursor read "@layer framework..theme { .x { color: red } }";
  neg_cursor read "@layer .framework { .x { color: red } }";
  neg_cursor read "@layer framework. { .x { color: red } }";
  neg_cursor read "@layer framework.theme. { .x { color: red } }";
  neg_cursor read "@layer InHeRiT { .x { color: red } }"

(* Not a roundtrip test *)
let c8_layer_api () =
  (* CSS Cascade section 8: CSSOM exposes the declared layer name on imports and
     layer block rules, and the declared name list on layer statement rules. A
     name is its idents, so [framework.theme] is two of them; nested block rule
     names are the at-rule's own name, not parent-prefixed. *)
  let name = Alcotest.(option (list string)) in
  let import_named =
    {
      url = "theme.css";
      layer = Some [ "framework"; "theme" ];
      supports = None;
      media = None;
    }
  in
  let import_anonymous =
    { url = "private.css"; layer = Some []; supports = None; media = None }
  in
  let import_plain =
    { url = "plain.css"; layer = None; supports = None; media = None }
  in
  Alcotest.check name "named import layerName"
    (Some [ "framework"; "theme" ])
    (Css.Stylesheet.import_layer_name import_named);
  Alcotest.check name "anonymous import layerName is the empty name" (Some [])
    (Css.Stylesheet.import_layer_name import_anonymous);
  Alcotest.check name "unlayered import layerName is null" None
    (Css.Stylesheet.import_layer_name import_plain);
  Alcotest.check name "named layer block API name"
    (Some [ "framework"; "theme" ])
    (Css.Stylesheet.layer_block_name
       (Css.Stylesheet.Layer (Some [ "framework"; "theme" ], [])));
  Alcotest.check name "anonymous layer block API name is the empty name"
    (Some [])
    (Css.Stylesheet.layer_block_name (Css.Stylesheet.Layer (None, [])));
  (match
     Css.Stylesheet.Layer
       (Some [ "outer" ], [ Css.Stylesheet.Layer (Some [ "foo"; "bar" ], []) ])
   with
  | Css.Stylesheet.Layer (_, [ inner ]) ->
      Alcotest.check name "inner layer block API name is not parent-prefixed"
        (Some [ "foo"; "bar" ])
        (Css.Stylesheet.layer_block_name inner)
  | _ -> Alcotest.fail "expected nested layer block");
  Alcotest.(check (option (list (list string))))
    "layer statement API nameList"
    (Some [ [ "reset" ]; [ "framework"; "theme" ]; [ "components" ] ])
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer_decl
          [ [ "reset" ]; [ "framework"; "theme" ]; [ "components" ] ]));
  Alcotest.(check (option (list (list string))))
    "non-statement layer has no nameList" None
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer (Some [ "reset" ], [])))

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
    [ "#f00"; "1px"; "#00f"; "currentcolor" ]
    (List.map declared_value declared);
  Alcotest.(check (list int))
    "declared values preserve source order" [ 0; 1; 2; 3 ]
    (List.map declared_source_order declared);
  Alcotest.(check (list string))
    "declared value filtering selects one property" [ "#f00"; "#00f" ]
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

let check_specified name expected_value expected_source
    (actual : Css.Stylesheet.value) =
  Alcotest.(check string) (name ^ " value") expected_value actual.value;
  Alcotest.(check string)
    (name ^ " source") expected_source
    (specified_source_name actual.value_source)

(* Not a roundtrip test *)
let c43_specified_values () =
  (* CSS Cascade section 4.3: defaulting guarantees a specified value exists for
     every property. CSS-wide keywords are handled before computed values. *)
  check_specified "normal cascaded value" "block" "cascaded"
    (Css.Stylesheet.value ~inherits:false ~initial:"inline" ~inherited:None
       ~cascaded:(Some "block"));
  check_specified "missing non-inherited property" "auto" "initial-default"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto" ~inherited:None
       ~cascaded:None);
  check_specified "missing inherited property" "blue" "inherited-default"
    (Css.Stylesheet.value ~inherits:true ~initial:"black"
       ~inherited:(Some "blue") ~cascaded:None);
  check_specified "initial keyword" "medium" "initial-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"medium"
       ~inherited:(Some "large") ~cascaded:(Some "initial"));
  check_specified "inherit keyword" "4.2px" "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "inherit keyword on root" "medium" "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium" ~inherited:None
       ~cascaded:(Some "inherit"));
  check_specified "unset on inherited property" "inside" "unset-inherited"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "unset"));
  check_specified "unset on non-inherited property" "auto" "unset-initial"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto"
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
    { origin; important; layer; specificity; scope_hops; source_order; value }
  in
  let winner_value candidates =
    Css.Stylesheet.winning_cascade_candidate
      ~layer_order:[ "reset"; "theme"; "utilities" ]
      candidates
    |> Option.map (fun (c : Css.Stylesheet.cascade_candidate) -> c.value)
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
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "width missing declaration example" "auto" "initial-default"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto" ~inherited:None
       ~cascaded:None);
  check_specified "list-style-position inherit example" "inside"
    "inherit-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "inherit"));
  check_specified "list-style-position initial example" "outside"
    "initial-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "initial"))

let dom_selector_boundary () =
  (* Selector matching is DOM context, but selector syntax is CSS-file
     surface. *)
  let selector_cases =
    [
      (".card", ".card");
      ("article.card > h2:first-child", "article.card>h2:first-child");
      (":scope > .item", ":scope>.item");
      (".card:has(> img[alt])", ".card:has(>img[alt])");
      ("a:visited", "a:visited");
      ("::part(label)", "::part(label)");
      (":host(.active) .title", ":host(.active) .title");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        ("minify selector " ^ input)
        expected
        (Css.Selector.to_string ~minify:true (Css.Selector.of_string input)))
    selector_cases;
  neg_cursor read ".card:has(> ) { color: red }";
  neg_cursor read "::before::after { color: red }";
  neg_cursor read ":host-context() { color: red }"

let fetch_url_boundary () =
  (* @import and url(...) syntax is in scope; loading/resolution is not. *)
  let import_cases =
    [
      ( "@import url(base.css) layer(reset) supports(display: grid) screen;",
        "base.css",
        Some [ "reset" ] );
      ("@import \"print.css\" print;", "print.css", None);
      ("@import url(theme.css) layer();", "theme.css", Some []);
      ( "@import url(theme.css) layer(theme) supports(selector(:has(img))) \
         screen and (width >= 40em);",
        "theme.css",
        Some [ "theme" ] );
      ( "@import url(\"../fonts/brand.woff2\") layer(fonts);",
        "../fonts/brand.woff2",
        Some [ "fonts" ] );
    ]
  in
  List.iter
    (fun (input, url, layer) ->
      let r = Cursor.of_string input in
      let rule = Css.Stylesheet.read_import_rule r in
      Alcotest.(check string) "import url" url rule.url;
      Alcotest.(check (option (list string))) "import layer" layer rule.layer)
    import_cases;
  check_declaration ~expected:"background-image:url(../img/logo.svg)"
    "background-image: url(../img/logo.svg);";
  check_declaration ~expected:"cursor:url(cursor.cur),auto"
    "cursor: url(cursor.cur), auto";
  (* A [<url-token>] ends at its [)], so [format(] needs no separator after it
     and minify drops the space, as it does after [@import] just below. *)
  check_declaration ~expected:"src:url(brand.woff2)format(woff2)"
    "src: url(brand.woff2) format(woff2)";
  check_import_rule ~expected:"@import\"theme.css\"supports(display:);"
    "@import url(theme.css) supports(display:);";
  neg_cursor read_import_rule "@import url(theme.css) layer(theme) layer(base);";
  neg_cursor read_import_rule
    "@import url(theme.css) screen supports(display: grid);"

let environment_query_boundary () =
  (* Query syntax is in scope; matching needs explicit environment context. *)
  check_stylesheet
    ~expected:
      "@media(width>=40em){@supports(display:grid){@container card \
       style(--theme:dark){.card{display:grid}}}}"
    "@media (width >= 40em) { @supports (display: grid) { @container card \
     style(--theme: dark) { .card { display: grid } } } }";
  check_stylesheet ~expected:"@supports(display:){.x{color:red}}"
    "@supports (display:) { .x { color: red } }";
  check_stylesheet ~expected:"@media(width >= ){.x{color:red}}"
    "@media (width >= ) { .x { color: red } }";
  neg_cursor read "@container card style() { .x { color: red } }";
  check_stylesheet ~expected:"@container card (width >){.x{color:red}}"
    "@container card (width >) { .x { color: red } }"

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
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "16px") ~cascaded:(Some "inherit"));
  check_specified "unset chooses inherited before computed stage" "canvastext"
    "unset-inherited"
    (Css.Stylesheet.value ~inherits:true ~initial:"black"
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
  neg_cursor Css.Declaration.read_declaration "--: var(--x);";
  check_declaration ~expected:"--tokens:{color:red}" "--tokens: { color: red };";
  check_declaration ~expected:"--empty:var(--missing,)"
    "--empty: var(--missing,);";
  check_specified_value "nested var fallback"
    "--nested: var(--a, var(--b, red));" "var(--a, var(--b, red))";
  neg_cursor read
    "@property --registered { syntax: \"<color>\"; inherits: false; \
     initial-value: 10px }"

let spec_current_at_rules () =
  check_stylesheet ~expected:"@media(dynamic-range:high){.photo{color:red}}"
    "@media (dynamic-range: high) { .photo { color: red } }";
  check_stylesheet
    ~expected:"@media(prefers-reduced-data:reduce){.hero{display:none}}"
    "@media (prefers-reduced-data: reduce) { .hero { display: none } }";
  check_stylesheet
    ~expected:"@supports selector(:has(img)){.card{display:block}}"
    "@supports selector(:has(img)) { .card { display: block } }";
  check_stylesheet
    ~expected:".card{color:red;@media(width>=40em){&>img{display:block}}}"
    ".card { color: red; @media (width >= 40em) { & > img { display: block } } \
     }";
  check_stylesheet ~expected:"@scope(.card)to (.footer){.title{color:red}}"
    "@scope (.card) to (.footer) { .title { color: red } }";
  check_stylesheet ~expected:"@scope(.card){.title{color:red}}"
    "@scope (.card) { .title { color: red } }";
  (* The scope-end selector list is held in authored order by pp; only optimize
     sorts it into canonical order (and folds the color). *)
  assert_minify_and_optimize
    "@scope (:root) to (.stop, .end) { .title { color: blue } }"
    ~minified:"@scope(:root)to (.stop,.end){.title{color:blue}}"
    ~optimized:"@scope(:root)to (.end,.stop){.title{color:#00f}}";
  check_stylesheet
    ~expected:
      "@font-palette-values \
       --brand{font-family:Brand;base-palette:1;override-colors:0 red}"
    "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
     override-colors: 0 red; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:ColorFont;src:url(color.woff2)tech(color-COLRv1);font-tech:color-COLRv1}"
    "@font-face { font-family: ColorFont; src: url(color.woff2) \
     tech(color-COLRv1); font-tech: color-COLRv1; }";
  check_stylesheet ~expected:"@view-transition{navigation:auto}"
    "@view-transition { navigation: auto; }";
  check_stylesheet
    ~expected:"@position-try --below{top:anchor(bottom);left:anchor(center)}"
    "@position-try --below { top: anchor(bottom); left: anchor(center); }";
  check_stylesheet
    ~expected:
      "@container card \
       style(--variant:featured){.card{view-transition-name:card}}"
    "@container card style(--variant: featured) { .card { \
     view-transition-name: card } }";
  check_stylesheet
    ~expected:"@container style(--variant:featured){.card{color:red}}"
    "@container style(--variant: featured) { .card { color: red } }";
  check_stylesheet
    ~expected:"@container scroll-state(stuck:top){.card{color:red}}"
    "@container scroll-state(stuck: top) { .card { color: red } }";
  neg_cursor read "@container style() { .card { color: red } }";
  neg_cursor read "@container scroll-state() { .card { color: red } }";
  check_stylesheet
    ~expected:
      "@container(30em<=inline-size<60em){@supports(display:grid){.grid{display:grid}}}"
    "@container (30em <= inline-size < 60em) { @supports (display: grid) { \
     .grid { display: grid } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog{opacity:0;translate:0 1rem}}"
    "@starting-style { .dialog { opacity: 0; translate: 0 1rem } }";
  check_stylesheet ~expected:"@page chapter:left{margin:2cm}"
    "@page chapter:left { margin: 2cm }";
  check_stylesheet ~expected:"@media(width >){.x{color:red}}"
    "@media (width >) { .x { color: red } }";
  (* Mediaqueries 5 sec. 3.1 sends a condition no grammar claims to
     <general-enclosed>, which a parser keeps and never matches. Chrome keeps
     both of these in cssText for the same reason. *)
  check_stylesheet ~expected:"@media(width >= ){.x{color:red}}"
    "@media (width >= ) { .x { color: red } }";
  check_stylesheet ~expected:"@supports selector(){.x{color:red}}"
    "@supports selector() { .x { color: red } }";
  neg_cursor read "@scope (.card) .title { color: red }";
  neg_cursor read "@font-palette-values { base-palette: 1; }";
  neg_cursor read "@position-try default { top: 0; }";
  check_stylesheet ~expected:"@container(){.x{color:red}}"
    "@container () { .x { color: red } }";
  neg_cursor read "@page : { margin: 1cm }"

let font_palette_values_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@font-palette-values \
         --brand{font-family:Brand;base-palette:1;override-colors:0 red,1 \
         color(display-p3 1 0 0)}",
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         override-colors: 0 red, 1 color(display-p3 1 0 0); }" );
      ( "@font-palette-values --dark{font-family:Color \
         Font,Brand;base-palette:dark}",
        "@font-palette-values --dark { font-family: \"Color Font\", Brand; \
         base-palette: dark; }" );
    ];
  List.iter (neg_cursor read)
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
  List.iter (neg_cursor read)
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
  List.iter (neg_cursor read)
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
      ( "@font-face{font-weight:100 \
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
      ( "@media screen and (width>=40em){.card{display:grid}}",
        "@media screen and (width >= 40em) { .card { display: grid } }" );
      ( "@supports((display:grid)and selector(:has(img))){.card{display:grid}}",
        "@supports ((display: grid) and selector(:has(img))) { .card { \
         display: grid } }" );
      ( "@container card style(--variant:featured){.card{color:red}}",
        "@container card style(--variant: featured) { .card { color: red } }" );
      ( "@scope(.card)to (.boundary){.title{color:red}}",
        "@scope (.card) to (.boundary) { .title { color: red } }" );
      ( "@starting-style{.dialog{opacity:0}}",
        "@starting-style { .dialog { opacity: 0 } }" );
    ];
  List.iter (neg_cursor read)
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
      "@page { @top-center { @media screen { .x { color: red } } } }";
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
  List.iter (fun (row : A.invalid_row) -> neg_cursor read row.input) A.negative;
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
      "@container card (inline-size>30em){.card{grid-template-columns:subgrid}}"
    "@container card (inline-size > 30em) { .card { grid-template-columns: \
     subgrid } }";
  let oklch_support =
    "@supports (color: oklch(50% 0.1 20)) { .accent { color: oklch(50% 0.1 20) \
     } }"
  in
  (* The condition keeps [0.1] as written: it is the declaration the browser is
     asked to parse, not a value to re-spell. Its insignificant whitespace goes,
     as it does everywhere else in a prelude: [%] closes the percentage token,
     so [50%0.1] is the same two tokens. The guarded rule folds. *)
  check_stylesheet
    ~expected:
      "@supports(color:oklch(50%0.1 20)){.accent{color:oklch(50%.1 20)}}"
    oklch_support;
  assert_minify_and_optimize oklch_support
    ~minified:
      "@supports(color:oklch(50%0.1 20)){.accent{color:oklch(50%.1 20)}}"
    ~optimized:"@supports(color:oklch(50%0.1 20)){.accent{color:#944a4b}}";
  let nested_media =
    ".card { color: var(--fg); @media (prefers-color-scheme: dark) { & { \
     color: white } } }"
  in
  check_stylesheet
    ~expected:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:white}}}"
    nested_media;
  assert_minify_and_optimize nested_media
    ~minified:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:white}}}"
    ~optimized:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:#fff}}}";
  neg_cursor read "@layer reset,,base;";
  check_stylesheet ~expected:"@container card (){.card{color:red}}"
    "@container card () { .card { color: red } }";
  check_stylesheet ~expected:"@supports(){.accent{color:red}}"
    "@supports () { .accent { color: red } }"

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
  let r = Cursor.of_string input in
  try
    let stylesheet = Css.Stylesheet.read r in
    let roundtrip =
      String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
    in
    Alcotest.(check string)
      ("nesting roundtrip for " ^ input)
      expected roundtrip
  with Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)

(** Helper: parse CSS, print minified, parse again, print again -- verify
    idempotent *)
let test_nesting_idempotent input =
  let r = Cursor.of_string input in
  try
    let sheet1 = Css.Stylesheet.read r in
    let printed1 = String.trim (Css.Stylesheet.to_string ~minify:true sheet1) in
    let r2 = Cursor.of_string printed1 in
    let sheet2 = Css.Stylesheet.read r2 in
    let printed2 = String.trim (Css.Stylesheet.to_string ~minify:true sheet2) in
    Alcotest.(check string)
      ("nesting idempotent for " ^ input)
      printed1 printed2
  with Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)

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
    ~expected:".foo{color:red;@media(min-width:768px){color:blue}}"
    ".foo { color: red; @media (min-width: 768px) { color: blue; } }";
  test_nesting_idempotent ".foo{color:red;@media(min-width:768px){color:blue}}"

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
  (* optimize folds blue -> #00f and drops the redundant leading "& " (a bare
     nested selector already means descendant); pp holds both. *)
  assert_minify_and_optimize ".parent { color: red; & .child { color: blue; } }"
    ~minified:".parent{color:red;& .child{color:blue}}"
    ~optimized:".parent{color:red;.child{color:#00f}}";
  assert_minify_and_optimize ".btn { color: red; &:hover { color: blue; } }"
    ~minified:".btn{color:red;&:hover{color:blue}}"
    ~optimized:".btn{color:red;&:hover{color:#00f}}";
  check_stylesheet ~expected:".a{& .b{& .c{color:red}}}"
    ".a { & .b { & .c { color: red; } } }"

(* CSS Syntax 3 sec. 5.5.3 consumes a qualified rule whole, block and all,
   before deciding it is invalid, so sec. 5.5.5 resumes right after that block.
   Recovering to the next semicolon, which is what sec. 5.5.6 does for a bad
   declaration, would take every item written after the bad rule and the parent
   with them. *)
let nesting_invalid_rule_recovery () =
  (* Each expectation is what the same sheet gives with the bad rule deleted:
     dropping it costs the parent nothing and the items around it nothing. *)
  lenient_recover "invalid nested rule before a good one"
    ".a { .b <::::invalid::::> {} & .c { color: red } }" ".a .c{color:red}" 1;
  lenient_recover "invalid nested rule after a good one"
    ".a { & .c { color: red } .b <::::invalid::::> {} }" ".a .c{color:red}" 1;
  lenient_recover "invalid nested rule between declarations"
    ".a { color: red; .b <::::invalid::::> {} & .c { color: blue } }"
    ".a{color:red;.c{color:#00f}}" 1

(* A nested @layer holds nesting content: bare declarations belong to the parent
   selector, exactly as in @media/@supports. Blink and WebKit both read
   [.a{@layer n{color:red}}] as a layer block wrapping nested declarations. *)
let spec_nesting_layer_block () =
  test_nesting_roundtrip ~expected:".a{@layer n{color:red}}"
    ".a { @layer n { color: red; } }";
  test_nesting_roundtrip ~expected:".a{@layer{color:red}}"
    ".a { @layer { color: red; } }";
  test_nesting_roundtrip ~expected:".a{color:red;@layer n{color:blue}}"
    ".a { color: red; @layer n { color: blue; } }";
  test_nesting_roundtrip ~expected:".a{@layer n{& b{color:red}}}"
    ".a { @layer n { & b { color: red; } } }";
  test_nesting_idempotent ".a { @layer n { color: red; } }"

(* The statement form [@layer n;] is only a layer-order declaration, which no
   style rule can contain: both Blink and WebKit drop it. An empty nested layer
   therefore keeps its block form so the output re-reads. *)
let spec_nesting_empty_layer_keeps_block () =
  test_nesting_roundtrip ~expected:".a{@layer n{}}" ".a { @layer n { } }";
  test_nesting_idempotent ".a { @layer n { } }";
  test_nesting_roundtrip ~expected:"@layer n;" "@layer n { }"

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well", and its body is then read as nesting
   content. [@starting-style] is a grouping rule over style rules (CSS
   Transitions 2 sec. 3.3), and Blink 146 keeps the whole shape. *)
let spec_nesting_starting_style () =
  test_nesting_roundtrip ~expected:".a{@starting-style{color:red}}"
    ".a { @starting-style { color: red } }";
  test_nesting_roundtrip ~expected:".a{@starting-style{color:red}color:green}"
    ".a { @starting-style { color: red } color: green }";
  test_nesting_roundtrip ~expected:".a{@starting-style{& b{color:red}}}"
    ".a { @starting-style { & b { color: red } } }";
  test_nesting_idempotent ".a { @starting-style { color: red } color: green }"

(* CSS Conditional 5 sec. 3 and sec. 4: a prelude the [@when] / [@else]
   condition grammar rejects is a condition failure of that at-rule, and the
   caret belongs on the slice of the condition that failed. The reader holds
   those components, so it can point at them rather than at the block that
   follows; the same rule the [@container] and [@supports] preludes read by. *)
let conditional_prelude_errors () =
  let case (input, at_rule, offending) =
    let warnings =
      match Css.of_string ~strict:false input with
      | Error e -> [ e ]
      | Ok { Css.warnings; _ } -> warnings
    in
    match warnings with
    | [ ({ Error.kind = Error.Bad_condition { at_rule = named; _ }; _ } as e) ]
      ->
        Alcotest.(check string) (input ^ ": at-rule named") at_rule named;
        Alcotest.(check string)
          (input ^ ": caret on the offending slice")
          offending
          (String.sub input e.Error.loc.Loc.start_pos
             (e.Error.loc.Loc.end_pos - e.Error.loc.Loc.start_pos))
    | [ e ] ->
        Alcotest.failf "%s: expected a condition error, got %s" input
          (Error.to_string e)
    | warnings ->
        Alcotest.failf "%s: expected one warning, got %d" input
          (List.length warnings)
  in
  List.iter case
    [
      ("@when foo(x){.a{color:red}}", "@when", "foo(x)");
      ( "@when media(screen) and supports(top:0) or media(print){.a{c:red}}",
        "@when",
        "or" );
      ( "@when media(screen) or supports(top:0) and media(print){.a{c:red}}",
        "@when",
        "and" );
      ("@when .a{color:red}", "@when", ".");
      ( "@when media(screen) media(print){.a{color:red}}",
        "@when",
        "media(print)" );
      ( "@when media(screen{.a{color:red}}",
        "@when",
        "media(screen{.a{color:red}}" );
      ( "@when media(width>0px){.a{color:red}}@else foo(x){.b{color:red}}",
        "@else",
        "foo(x)" );
      ( "@when media(width>0px){.a{c:red}}@else media(print) x{.b{c:red}}",
        "@else",
        "x" );
    ]

(* The same section covers every conditional group rule, not just the three
   cascade already nested: [@-moz-document] carries style rules, and CSS
   Conditional 5 sec. 3 and sec. 4 make [@when] and [@else] conditional group
   rules over a rule list. *)
let spec_nesting_other_group_rules () =
  test_nesting_roundtrip ~expected:".a{@-moz-document url-prefix(){color:red}}"
    ".a { @-moz-document url-prefix() { color: red } }";
  test_nesting_roundtrip ~expected:".a{@when media(width>0px){color:red}}"
    ".a { @when media(width > 0px) { color: red } }";
  test_nesting_roundtrip
    ~expected:".a{@when media(width>0px){color:red}@else{color:green}}"
    ".a { @when media(width > 0px) { color: red } @else { color: green } }";
  test_nesting_idempotent ".a { @-moz-document url-prefix() { color: red } }"

(* A nested group rule's body is nesting content all the way down, so an at-rule
   inside one is itself a nested group rule. Blink 146 keeps [.a{@layer n{@media
   screen{color:red}}}] whole. *)
let spec_nesting_at_rule_inside_nested_group () =
  test_nesting_roundtrip ~expected:".a{@layer n{@media screen{color:red}}}"
    ".a { @layer n { @media screen { color: red } } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{@supports(foo:bar){color:red}}}"
    ".a { @media screen { @supports (foo: bar) { color: red } } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{@starting-style{color:red}}}"
    ".a { @media screen { @starting-style { color: red } } }";
  test_nesting_idempotent ".a { @layer n { @media screen { color: red } } }"

(* CSS Nesting 1 sec. 3.3 nests "any at-rule whose body contains style rules"; a
   descriptor rule, a keyframe list and a declaration-list rule contain none, so
   inside a style rule they are invalid. Blink 146 drops each one and keeps the
   declarations written around it, which is the recovery CSS Syntax 3 sec. 5.4.4
   describes: discard the invalid construct, resume at the next one. *)
let spec_nesting_rejects_non_group_at_rules () =
  let drops at_rule =
    test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
      (".a { color: red; " ^ at_rule ^ " background: blue }")
  in
  drops "@font-face { font-family: F; src: url(f.woff2) }";
  drops "@keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@-webkit-keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@-moz-keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@property --p { syntax: \"<color>\"; inherits: false }";
  drops "@page { margin: 1cm }";
  drops "@counter-style c { system: cyclic; symbols: \"x\" }";
  drops "@position-try --t { top: 1px }";
  drops "@font-palette-values --v { font-family: F; base-palette: 0 }";
  drops "@font-feature-values F { @styleset { s: 1 } }";
  drops "@viewport { width: 100px }";
  drops "@-ms-viewport { width: 100px }";
  drops "@supports-condition (color: red) { color: green }"

(* The same rejection inside every nesting context that a style rule opens: a
   nested group rule's body, a nested style rule, and a rule reached through a
   stylesheet-level group rule. *)
let spec_nesting_rejects_non_group_at_rules_deep () =
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @font-face { font-family: F; src: \
     url(f.woff2) } background: blue } }";
  test_nesting_roundtrip ~expected:".a{& b{color:red;background:blue}}"
    ".a { & b { color: red; @keyframes k { to { opacity: 1 } } background: \
     blue } }";
  test_nesting_roundtrip
    ~expected:"@media screen{.a{color:red;background:blue}}"
    "@media screen { .a { color: red; @page { margin: 1cm } background: blue } \
     }"

(* Dropping the invalid at-rule must not take a nested style rule written after
   it, and the surviving text has to read back unchanged. *)
let spec_nesting_rejection_keeps_the_rest () =
  test_nesting_roundtrip ~expected:".a{color:red;& span{color:lime}b:2}"
    ".a { color: red; @font-face { font-family: F; src: url(f.woff2) } & span \
     { color: lime } b: 2 }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @font-face; background: blue }";
  test_nesting_idempotent
    ".a { color: red; @page { margin: 1cm } background: blue }"

(* CSS View Transitions 2 gives [\@view-transition] a descriptor body, so CSS
   Nesting 1 sec. 3.3 does not nest it either - but Blink 146 keeps it inside a
   style rule, down to [&:hover], where every rule above is dropped. Dropping
   what a shipping engine still reads is the one lossy direction, so cascade
   keeps it. *)
let spec_nesting_keeps_view_transition () =
  test_nesting_roundtrip
    ~expected:".a{color:red;@view-transition{navigation:auto}background:blue}"
    ".a { color: red; @view-transition { navigation: auto } background: blue }"

(* CSS Conditional 5 sec. 4: [\@else] is only valid after a [\@when] or another
   [\@else], wherever it is written. *)
let spec_nesting_rejects_orphan_else () =
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @else { color: green } background: blue }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @else { color: green } background: blue \
     } }";
  test_nesting_roundtrip
    ~expected:".a{@when media(width>0px){color:red}@else{color:green}}"
    ".a { @when media(width > 0px) { color: red } @else { color: green } }"

(* A top-of-sheet rule is invalid in a style rule too, and Blink 146 drops it
   the same way. Discarding it ends at the at-rule: [\@import url(x){}] carries
   a block, and a skip to the next [;] runs past the end of the group rule
   holding it. *)
let spec_nesting_rejects_top_of_sheet_at_rules () =
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @charset \"utf-8\"; background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @import url(x.css); background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @import url(x.css) { color: pink } background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @namespace n url(http://e.com); background: blue }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @import url(x.css); background: blue } }"

(* CSS Nesting 1 sec. 3: a nested rule's prelude may start with an ident, so
   [h2:where(.b) { ... }] is a rule and not a [h2] declaration, however much its
   head looks like one. That holds in a nested at-rule's body as much as in a
   style rule's, and Blink 146 reads both as rules. *)
let spec_nesting_ident_prelude_in_nested_at_rule () =
  test_nesting_roundtrip ~expected:".a{@media screen{h2:where(.b){color:red}}}"
    ".a { @media screen { h2:where(.b) { color: red } } }";
  test_nesting_idempotent ".a { @media screen { h2:where(.b) { color: red } } }";
  test_nesting_roundtrip
    ~expected:
      ".a{@media screen{color:red;h2:where(.b){margin:0}background:blue}}"
    ".a { @media screen { color: red; h2:where(.b) { margin: 0 } background: \
     blue } }"

(* CSS Syntax 3 sec. 5.4.3 "consume a block's contents" drops a [;] that no
   declaration precedes rather than validating one, so a stray semicolon in a
   nested at-rule's body costs nothing. Blink 146 keeps both neighbours. *)
let spec_nesting_skips_stray_semicolons () =
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red;; background: blue } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { ; color: red; background: blue } }"

(* CSS Nesting 1 sec. 3.4: a run of declarations written after a nested rule is
   wrapped in a nested declarations rule, which keeps its place among the nested
   rules. The spec's own worked example names the hoisted spelling as NOT
   equivalent, and Blink and WebKit both compute the later declaration. *)
let spec_nesting_declaration_after_nested_rule () =
  test_nesting_roundtrip ~expected:".a{color:red;& b{color:blue}color:green}"
    ".a { color: red; & b { color: blue } color: green }";
  test_nesting_roundtrip
    ~expected:".a{@supports(color:red){color:blue}color:green}"
    ".a { @supports (color: red) { color: blue } color: green }";
  test_nesting_roundtrip ~expected:".a{c:1;& b{d:2}e:3;f:4;& g{h:5}i:6}"
    ".a { c: 1; & b { d: 2 } e: 3; f: 4; & g { h: 5 } i: 6 }";
  test_nesting_idempotent ".a { color: red; & b { color: blue } color: green }";
  test_nesting_idempotent
    ".a { @media (min-width: 1px) { padding: 2rem } color: green }"

let spec_nesting_selector_edges () =
  assert_minify_and_optimize
    ".card { color: red; &:is(:hover, :focus-visible) { color: blue } &:has(> \
     img) { display: grid } }"
    ~minified:
      ".card{color:red;&:is(:hover,:focus-visible){color:blue}&:has(>img){display:grid}}"
      (* nested rule order is preserved; selector spelling and declaration
         values are still canonicalized. *)
    ~optimized:
      ".card{color:red;&:is(:focus-visible,:hover){color:#00f}&:has(>img){display:grid}}";
  check_stylesheet
    ~expected:
      ".card{@supports \
       selector(:has(img)){&:has(img){display:grid}}@container(inline-size>30em){&>.media{display:block}}}"
    ".card { @supports selector(:has(img)) { &:has(img) { display: grid } } \
     @container (inline-size > 30em) { & > .media { display: block } } }";
  assert_minify_and_optimize
    "@scope (.card) to (.boundary) { .title { color: red; &:hover { color: \
     blue } } }"
    ~minified:
      "@scope(.card)to (.boundary){.title{color:red;&:hover{color:blue}}}"
    ~optimized:
      "@scope(.card)to (.boundary){.title{color:red;&:hover{color:#00f}}}";
  assert_minify_and_optimize
    ".card { @scope (&) to (.boundary) { & .title { color: blue } } }"
    ~minified:".card{@scope(&)to (.boundary){& .title{color:blue}}}"
    ~optimized:".card{@scope(&)to (.boundary){& .title{color:#00f}}}";
  check_stylesheet
    ~expected:
      ".card{color:red;@scope(.feature)to (.boundary){&>.title{display:grid}}}"
    ".card { color: red; @scope (.feature) to (.boundary) { & > .title { \
     display: grid } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog[open]{opacity:0;transform:scale(.95)}}"
    "@starting-style { .dialog[open] { opacity: 0; transform: scale(0.95) } }";
  neg_cursor read ".card { & { & { color: red } } }";
  neg_cursor read "@scope () { .x { color: red } }";
  neg_cursor read "@scope (.x) to () { .x { color: red } }";
  neg_cursor read "@starting-style;"

(* CSS Nesting Module Level 1, sections 1 and 2: a nested style rule is a
   qualified rule appearing inside another qualified rule's block. The grammar
   does not require flattening on serialization, so a parse and print of a
   nested input must keep the nested shape rather than collapsing it into
   sibling rules joined by descendant combinators. *)
let nesting_module_l1_preserves_structure () =
  let input =
    ".card { color: red; & .title { color: blue; } &:hover { color: green; } }"
  in
  let r = Cursor.of_string input in
  let sheet = Css.Stylesheet.read r in
  let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
  Alcotest.(check string)
    "parse + print keeps nested rules nested"
    ".card{color:red;& .title{color:blue}&:hover{color:green}}" printed;
  let optimized = Css.Optimize.stylesheet sheet in
  let opt_printed =
    String.trim (Css.Stylesheet.to_string ~minify:true optimized)
  in
  Alcotest.(check string)
    "optimize keeps nested rules nested (no flattening)"
    ".card{color:red;.title{color:#00f}&:hover{color:green}}" opt_printed

(* CSS Cascade Module Level 6, section 2 (Importing Style Sheets): @import
   identifies an external style sheet by URL. The cascade treats the import as a
   substitution point but does not fetch or inline the referenced sheet at the
   syntax layer; the URL string and conditional clauses must survive parse and
   print without resolution or rewriting. *)
let c6_2_import_preserved_verbatim () =
  let url = "https://example.test/path/to/theme.css?v=1" in
  let input =
    "@import url(\"" ^ url
    ^ "\") layer(framework.theme) supports(display:grid) screen and \
       (min-width:30em);"
  in
  let r = Cursor.of_string input in
  let sheet = Css.Stylesheet.read r in
  let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
  let contains_url s = Astring.String.is_infix ~affix:url s in
  Alcotest.(check bool)
    "import URL string survives parse and print verbatim" true
    (contains_url printed);
  Alcotest.(check bool)
    "layer name preserved on import" true
    (Astring.String.is_infix ~affix:"layer(framework.theme)" printed);
  Alcotest.(check bool)
    "supports() condition preserved on import" true
    (Astring.String.is_infix ~affix:"supports(display:grid)" printed);
  Alcotest.(check bool)
    "media query list preserved on import" true
    (Astring.String.is_infix ~affix:"screen and (min-width:30em)" printed);
  let optimized = Css.Optimize.stylesheet sheet in
  let opt_printed =
    String.trim (Css.Stylesheet.to_string ~minify:true optimized)
  in
  Alcotest.(check bool)
    "optimize does not resolve, inline, or rewrite the import URL" true
    (contains_url opt_printed);
  Alcotest.(check int)
    "no rule statements are synthesized from the import" 0
    (List.length (Css.Stylesheet.rules optimized))

(* CSS Syntax Level 3, section 4.3.2 (Consume comments): comments are consumed
   during tokenization and produce no tokens. The /*# sourceMappingURL=... */
   and /*# sourceURL=... */ pragmas are developer-tool conventions with no W3C
   CSS specification: to a conforming CSS parser they are ordinary comments at
   every position they may appear (top of file, between rules, inside a rule's
   declaration block, and inside an at-rule's block). They must not survive into
   the AST or the serialized output, and they must not influence the surrounding
   rule. *)
let s3432_sourcemap_comment () =
  let cases =
    [
      ( "pragma at top of file",
        "/*# sourceMappingURL=app.css.map */ .a { color: red }",
        ".a{color:red}" );
      ( "pragma between rules",
        ".a { color: red } /*# sourceMappingURL=app.css.map */ .b { color: \
         blue }",
        ".a{color:red}.b{color:blue}" );
      ( "pragma at end of file",
        ".a { color: red } /*# sourceMappingURL=app.css.map */",
        ".a{color:red}" );
      ( "pragma inside declaration block",
        ".a { color: red; /*# sourceMappingURL=app.css.map */ padding: 1px }",
        ".a{color:red;padding:1px}" );
      ( "pragma inside at-rule block",
        "@media screen { /*# sourceMappingURL=app.css.map */ .a { color: red } \
         }",
        "@media screen{.a{color:red}}" );
      ( "sourceURL pragma alongside sourceMappingURL",
        "/*# sourceMappingURL=app.css.map */ /*# sourceURL=app.css */ .a { \
         color: red }",
        ".a{color:red}" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let r = Cursor.of_string input in
      let sheet = Css.Stylesheet.read r in
      let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
      Alcotest.(check string) name expected printed)
    cases

(* CSS Cascade Module Level 5, section 6.4.4.2 (The Layer Statement Rule): the
   statement form [@layer foo, bar;] is defined as equivalent to declaring each
   named layer with an empty block in the same order. The two surface shapes
   must therefore parse to the same effective layer order and produce the same
   serialization once empty blocks are normalized. *)
let c6442_empty_blocks_equiv () =
  let parse css =
    Css.Stylesheet.read (Cursor.of_string css)
    |> Css.Optimize.stylesheet
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  let statement_form = parse "@layer reset, theme, components;" in
  let empty_block_form =
    parse "@layer reset {} @layer theme {} @layer components {}"
  in
  Alcotest.(check string)
    "statement form and empty-block form normalize to the same output"
    statement_form empty_block_form;
  let statement_then_block =
    parse "@layer reset, theme; @layer reset { .a { color: red } }"
  in
  let two_blocks =
    parse "@layer reset {} @layer theme {} @layer reset { .a { color: red } }"
  in
  Alcotest.(check string)
    "subsequent block in a previously-declared layer is order-equivalent"
    statement_then_block two_blocks

(* CSS Cascade Module Level 5, section 6.4.2 (Layer Naming and Nesting): a
   dotted layer name is shorthand for nested layer blocks. The spec says [@layer
   foo.bar { ... }] declares the same nested layer as [@layer foo { @layer bar {
   ... } }], so the rule placed inside either form must end up in the same
   effective cascade layer named [foo.bar]. *)
let c643_dotted_nested_layer () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let dotted = parse "@layer foo.bar { .x { color: red } }" in
  let nested = parse "@layer foo { @layer bar { .x { color: red } } }" in
  let inner_rule_text = function
    | Some stmts -> Css.to_string ~minify:true (Css.v stmts) |> String.trim
    | None -> "<no foo.bar layer>"
  in
  Alcotest.(check string)
    "the inner rule appears in the foo.bar layer regardless of input form"
    (inner_rule_text (Css.layer_block [ "foo"; "bar" ] dotted))
    (inner_rule_text (Css.layer_block [ "foo"; "bar" ] nested));
  Alcotest.(check (list (list string)))
    "both forms expose the same set of declared layers" (Css.layers dotted)
    (Css.layers nested)

(* CSS Syntax Level 3, section 4.3.2 (Consume comments): comments are stripped
   at tokenization. The /*# sourceMappingURL */ and /*# sourceURL */ pragmas are
   developer-tool conventions with no W3C status, so a conforming printer must
   never emit them on output regardless of what was in the input. The absence of
   the [/*#] sequence in the printed stylesheet is an implementation-independent
   guarantee that there is no source-map support. *)
let s3432_no_sourcemap_print () =
  let inputs =
    [
      ".a{color:red}";
      "/*# sourceMappingURL=foo.css.map */ .a { color: red }";
      "@media screen { /*# sourceMappingURL=foo.css.map */ .a { color: red } }";
      "@import url(\"a.css\"); /*# sourceURL=a.css */ .a { color: red }";
    ]
  in
  List.iter
    (fun input ->
      let r = Cursor.of_string input in
      let sheet = Css.Stylesheet.read r in
      let printed = Css.Stylesheet.to_string ~minify:true sheet in
      Alcotest.(check bool)
        ("printed stylesheet contains no /*# pragma for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"/*#" printed);
      Alcotest.(check bool)
        ("printed stylesheet contains no sourceMappingURL for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"sourceMappingURL" printed);
      Alcotest.(check bool)
        ("printed stylesheet contains no sourceURL for input: " ^ input)
        false
        (Astring.String.is_infix ~affix:"sourceURL" printed))
    inputs

(* CSS Values and Units Module Level 4, section 6 (Distance Units): "A 0 length
   is unitless and may be substituted for 0px or 0em (etc.) in any
   length-accepting context." Two property values that differ only in the
   spelling of a zero length are spec-equivalent, so the optimizer is free to
   pick any of them - but parsing two equivalent forms must produce the same
   computed declaration after a round-trip through the optimizer. *)
let v461_zero_length_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let zero_unitless = normalize ".a { margin: 0 }" in
  let zero_px = normalize ".a { margin: 0px }" in
  let zero_em = normalize ".a { margin: 0em }" in
  let zero_rem = normalize ".a { margin: 0rem }" in
  Alcotest.(check string)
    "0 and 0px are spec-equivalent for <length>" zero_unitless zero_px;
  Alcotest.(check string)
    "0 and 0em are spec-equivalent for <length>" zero_unitless zero_em;
  Alcotest.(check string)
    "0 and 0rem are spec-equivalent for <length>" zero_unitless zero_rem

(* CSS Color Module Level 4, section 5.2 (Hex Notation): a 6-digit hex color
   [#rrggbb] and the equivalent 3-digit shorthand [#rgb] (where each pair is the
   same character) denote the identical sRGB color. They are spec- equivalent
   forms; the optimizer may choose either, but a round trip must yield the same
   color regardless of input spelling. *)
let color4121_hex_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      ("#ffffff", "#fff");
      ("#000000", "#000");
      ("#112233", "#123");
      ("#aabbcc", "#abc");
    ]
  in
  List.iter
    (fun (long, short) ->
      let long_form = normalize (".a { color: " ^ long ^ " }") in
      let short_form = normalize (".a { color: " ^ short ^ " }") in
      Alcotest.(check string)
        ("hex " ^ long ^ " and " ^ short ^ " are spec-equivalent")
        long_form short_form)
    pairs

(* CSS Cascade and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): when all higher-priority criteria tie, declarations are ordered by
   source order, with later declarations winning. The optimizer is free to merge
   or rewrite rules, but it MUST NOT reorder declarations of the same property
   in a way that changes the cascaded result. A round trip through the optimizer
   must preserve the winning value when two declarations of the same property
   tie on origin, importance, layer and specificity. *)
let rec color_value_stop printed j =
  if j >= String.length printed then j
  else if printed.[j] = ';' || printed.[j] = '}' then j
  else color_value_stop printed (j + 1)

let rec last_color_value printed start_idx best =
  match Astring.String.find_sub ~start:start_idx ~sub:"color:" printed with
  | None -> best
  | Some i ->
      let after = i + String.length "color:" in
      let stop = color_value_stop printed after in
      let value = String.sub printed after (stop - after) in
      last_color_value printed (i + 1) (Some value)

let c61_keeps_winner () =
  let winning_color css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        let printed = minify parsed.stylesheet |> String.trim in
        (* Last occurrence of "color:" in the optimized output is the cascaded
           winner under section 6.1's "later wins" rule for tied candidates. *)
        last_color_value printed 0 None
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check (option string))
    "later declaration wins after optimization (same selector)" (Some "#00f")
    (winning_color ".a { color: red; color: blue }");
  Alcotest.(check (option string))
    "later rule wins after optimization (same selector consecutive)"
    (Some "#00f")
    (winning_color ".a { color: red } .a { color: blue }")

(* CSS Color Module Level 4, section 5.1 (The RGB functions): the named color
   [red], the hex notations [#f00] and [#ff0000], and the rgb function [rgb(255,
   0, 0)] all denote the same sRGB color. Under industry-standard minification
   (cssnano / Lightning CSS / clean-css) the printer canonicalizes to the
   shortest equivalent form, so all of these resolve to the same serialized
   output. *)
let color414_form_equiv () =
  (* These forms parse to distinct color nodes; collapsing them to the shortest
     spelling is an optimize transform, not a pp same-node choice. Canonicalize
     through optimize+minify. *)
  let canonical s =
    match
      Css.of_string ~strict:false (String.concat "" [ ".x{color:"; s; "}" ])
    with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  (* These hex forms all denote the same sRGB color (255, 0, 0) and optimize
     canonicalizes each to the named color [red]. The rgb() function forms are
     also spec-equivalent and could canonicalize the same way; that is an
     additional optimizer freedom not asserted here. *)
  let forms = [ "red"; "#f00"; "#ff0000" ] in
  match forms with
  | [] -> ()
  | first :: rest ->
      let first_canonical = canonical first in
      List.iter
        (fun form ->
          Alcotest.(check string)
            (form ^ " is spec-equivalent to " ^ first)
            first_canonical (canonical form))
        rest

(* CSS Color Module Level 4 defines named colors and hex colors as alternate
   notations for the same sRGB colors. For minification, the suite uses a single
   deterministic shortest-form policy: choose the shortest spelling, and when a
   named color and a short hex spelling are the same length, choose hex. This
   matches the pinned Lightning CSS trace for equal-length examples such as
   [yellow] -> [#ff0] and keeps parse -> minify -> parse -> minify idempotent
   for [blue] / [#00f] and [lime] / [#0f0]. *)
let color4_hex_tie_policy () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let cases =
    [
      ("red is shorter than #f00", ".x{color:red}", ".x { color: red }");
      ("blue ties #00f, hex wins", ".x{color:#00f}", ".x { color: blue }");
      ("#0000ff shortens to #00f", ".x{color:#00f}", ".x { color: #0000ff }");
      ("lime ties #0f0, hex wins", ".x{color:#0f0}", ".x { color: lime }");
      ( "hsl green canonicalizes through the same tie policy",
        ".x{color:#0f0}",
        ".x { color: hsl(120 100% 50%) }" );
      ("yellow ties #ff0, hex wins", ".x{color:#ff0}", ".x { color: yellow }");
      ("black uses shorter hex", ".x{color:#000}", ".x { color: black }");
      ("white uses shorter hex", ".x{color:#fff}", ".x { color: white }");
      ("gray is shorter than #808080", ".x{color:gray}", ".x { color: gray }");
    ]
  in
  List.iter
    (fun (name, expected, css) ->
      Alcotest.(check string) name expected (normalize css))
    cases

(* CSS Values and Units Module Level 4, section 6 only allows dropping units
   from zero lengths. Percentages keep their percent sign because [0%] is still
   a percentage token, not the bare number [0]. *)
let v465_zero_percentage_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let zero = normalize ".a { width: 0 }" in
  let zero_pct = normalize ".a { width: 0% }" in
  let zero_px = normalize ".a { width: 0px }" in
  Alcotest.(check string)
    "0 and 0px are spec-equivalent for zero length" zero zero_px;
  Alcotest.(check string)
    "0% preserves its percentage token" ".a{width:0%}" zero_pct;
  Alcotest.(check bool)
    "0 and 0% stay distinct for width" false
    (String.equal zero zero_pct)

(* CSSOM Level 1, section 6.6 (Serialize a CSS declaration): the serialized form
   puts ":" between property name and value with no surrounding spaces in
   minified mode, and "!important" follows the value with no extra whitespace.
   The property name is serialized as-is (already lowercased by the syntax layer
   per CSS Syntax sec. 8.1) regardless of input case. *)
let cssom662_decl_serialization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "uppercase property name lowercased on serialize" ".a{color:red}"
    (normalize ".a { COLOR: red }");
  Alcotest.(check string)
    "mixed-case property name lowercased on serialize"
    ".a{background-color:red}"
    (normalize ".a { Background-Color: red }");
  Alcotest.(check string)
    "important serialized without internal whitespace" ".a{color:red!important}"
    (normalize ".a { color: red !important }");
  Alcotest.(check string)
    "important whitespace after ! collapsed" ".a{color:red!important}"
    (normalize ".a { color: red !  IMPORTANT }")

(* CSS Color Module Level 4, section 6.3 (The "transparent" color keyword): the
   [transparent] keyword is defined as equivalent to [rgba(0, 0, 0, 0)]. The
   8-digit hex [#00000000] (or its 4-digit shorthand [#0000]) is also equivalent
   to that fully-transparent black per section 5.2. The optimizer may pick any
   spec-equivalent form; the test asserts the parsed colors denote the same
   value via the canonical printer. *)
let color4_6_4_transparent_equivalence () =
  (* These four forms parse to distinct color nodes, so collapsing them to one
     canonical spelling is an optimize transform, not a pp one: canonicalize
     through optimize, not bare pp. *)
  let canonical s =
    match Css.of_string ~strict:false (".x{color:" ^ s ^ "}") with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let forms =
    [ "transparent"; "rgba(0, 0, 0, 0)"; "rgb(0 0 0 / 0)"; "#0000" ]
  in
  match forms with
  | [] -> ()
  | first :: rest ->
      let first_canonical = canonical first in
      List.iter
        (fun form ->
          Alcotest.(check string)
            (form ^ " is spec-equivalent to " ^ first)
            first_canonical (canonical form))
        rest

(* CSS Color Module Level 4, section 6.1 (Named Colors): "CSS named colors are
   ASCII case-insensitive." [RED], [Red], and [red] all denote the same color
   and must serialize identically. *)
let color461_named_case () =
  let canonical s =
    match Css.parse_color s with
    | Some c -> Css.Pp.to_string ~minify:true Css.pp_color c
    | None -> Alcotest.failf "failed to parse color: %s" s
  in
  let cases =
    [
      ("red", "RED");
      ("red", "Red");
      ("blue", "BLUE");
      ("rebeccapurple", "RebeccaPurple");
    ]
  in
  List.iter
    (fun (lower, mixed) ->
      Alcotest.(check string)
        (mixed ^ " is case-equivalent to " ^ lower)
        (canonical lower) (canonical mixed))
    cases

(* CSS Values and Units Module Level 4, section 5.3 (Numbers and Numeric Data
   Types): [<number>] tokens accept several syntactic spellings of the same
   numeric value. [.5], [0.5] and [0.50] all denote 0.5; [1.0] and [1] denote
   the integer 1. The optimizer is free to pick the shortest spelling, but a
   round trip must yield the same numeric value regardless of input format. *)
let v481_number_format () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      (".5", "0.5"); (".5", "0.50"); ("1", "1.0"); ("1", "1.00"); (".25", "0.25");
    ]
  in
  List.iter
    (fun (a, b) ->
      let a_form = normalize (".x { opacity: " ^ a ^ " }") in
      let b_form = normalize (".x { opacity: " ^ b ^ " }") in
      Alcotest.(check string)
        ("opacity " ^ a ^ " and " ^ b ^ " are spec-equivalent")
        a_form b_form)
    pairs

(* CSS Selectors Module Level 4, section 5.2 (The Universal Selector): "If the
   universal selector is not the only component of a compound selector, the [*]
   may be omitted." So [*.foo] and [.foo] are spec-equivalent compound
   selectors, and [*#id] and [#id] are equivalent. *)
let s435_universal_redundant () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let pairs =
    [
      ("*.foo { color: red }", ".foo { color: red }");
      ("*#main { color: red }", "#main { color: red }");
      ("*[data-x] { color: red }", "[data-x] { color: red }");
      ("*:hover { color: red }", ":hover { color: red }");
    ]
  in
  List.iter
    (fun (with_star, without_star) ->
      Alcotest.(check string)
        (with_star ^ " is spec-equivalent to " ^ without_star)
        (normalize without_star) (normalize with_star))
    pairs;
  (* Dropping the redundant universal is a node change, so pp must HOLD it in
     both pretty and minify; only optimize drops it. A minify-only drop is a
     pp-purity bug - these assert the held minify form so it is detected. *)
  assert_minify_and_optimize "*.foo { color: red }" ~minified:"*.foo{color:red}"
    ~optimized:".foo{color:red}";
  assert_minify_and_optimize "*#main { color: red }"
    ~minified:"*#main{color:red}" ~optimized:"#main{color:red}";
  assert_minify_and_optimize "*[data-x] { color: red }"
    ~minified:"*[data-x]{color:red}" ~optimized:"[data-x]{color:red}";
  assert_minify_and_optimize "*:hover { color: red }"
    ~minified:"*:hover{color:red}" ~optimized:":hover{color:red}"

(* A comma-separated selector list is a union of its branches: reordering the
   branches and removing duplicate branches leaves both matching and the cascade
   unchanged, so each is a safe rewrite. But each is also a node change, so pp
   must HOLD the authored branch order (duplicates included) in BOTH pretty and
   minify; sorting into Cascade's canonical selector order and deduping branches
   are optimize transforms. Selector.pp doing the sort+dedup on every print is a
   pp-purity bug (and a per-print cost: a Pp.to_string key per branch on every
   to_string and every size) - these assert the held minify form so it is
   detected, alongside the canonical optimize form. *)
let selector_list_canonical_order () =
  assert_minify_and_optimize "div, .class, #id { color: red }"
    ~minified:"div,.class,#id{color:red}" ~optimized:"#id,.class,div{color:red}";
  assert_minify_and_optimize ".a, .a, .b { color: red }"
    ~minified:".a,.a,.b{color:red}" ~optimized:".a,.b{color:red}"

(* CSS Cascading and Inheritance / Scoping: the @scope prelude carries two
   selector lists - the scope-start [(<scope-start>)] and the scope-end [to
   (<scope-end>)], each a forgiving selector list whose branches are an
   unordered set. The selector-list policy applies to both: pp holds the
   authored branch order and duplicates in both pretty and minify ([minify]
   sides), and optimize sorts and de-duplicates both preludes ([minify+optimize]
   sides). *)
let scope_selector_list_canonical () =
  (* scope-start list: sorted by optimize, held by pp *)
  assert_minify_and_optimize "@scope (.b, .a) { .x { color: red } }"
    ~minified:"@scope(.b,.a){.x{color:red}}"
    ~optimized:"@scope(.a,.b){.x{color:red}}";
  (* scope-start list: duplicate branch dropped by optimize *)
  assert_minify_and_optimize "@scope (.a, .a) { .x { color: red } }"
    ~minified:"@scope(.a,.a){.x{color:red}}"
    ~optimized:"@scope(.a){.x{color:red}}";
  (* scope-end list: duplicate branch dropped by optimize *)
  assert_minify_and_optimize "@scope (:root) to (.b, .b) { .x { color: red } }"
    ~minified:"@scope(:root)to (.b,.b){.x{color:red}}"
    ~optimized:"@scope(:root)to (.b){.x{color:red}}";
  (* both scope-start and scope-end lists canonicalised at once *)
  assert_minify_and_optimize "@scope (.b, .a) to (.d, .c) { .x { color: red } }"
    ~minified:"@scope(.b,.a)to (.d,.c){.x{color:red}}"
    ~optimized:"@scope(.a,.b)to (.c,.d){.x{color:red}}"

(* CSS Color Module Level 4, section 4.3 (the <hue> syntax): the [<hue>]
   component accepts angles or numbers and is normalised modulo 360deg. So
   [hsl(360 100% 50%)] denotes the same color as [hsl(0 100% 50%)] and as the
   named color [red]; [hsl(720 ...)] is also red. Hue values outside [0, 360)
   must reduce, and the fully-saturated red hue must canonicalize to the named
   color under industry-standard minification. *)
let color4_3_hue_modulo_canonicalization () =
  (* Hue-modulo reduction and hsl -> named are optimize transforms, not pp
     same-node spellings; route each form through optimize+minify. *)
  let canonical s =
    match
      Css.of_string ~strict:false (String.concat "" [ ".x{color:"; s; "}" ])
    with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let red = canonical "red" in
  Alcotest.(check string)
    "hsl(0 100% 50%) is red" red
    (canonical "hsl(0 100% 50%)");
  Alcotest.(check string)
    "hsl(360 100% 50%) reduces modulo 360" red
    (canonical "hsl(360 100% 50%)");
  Alcotest.(check string)
    "hsl(720 100% 50%) reduces modulo 360" red
    (canonical "hsl(720 100% 50%)");
  Alcotest.(check string)
    "hsl(-360 100% 50%) reduces modulo 360" red
    (canonical "hsl(-360 100% 50%)")

(* CSS Color Module Level 4, section 4.2 (the <alpha-value> syntax): an alpha
   value may be expressed as a [<number>] in the closed interval [\[0, 1\]] or
   as a [<percentage>] in [\[0%, 100%\]]; the two forms are spec-equivalent.
   [rgb(255 0 0 / .5)] and [rgb(255 0 0 / 50%)] denote the identical color. *)
let color413_alpha_equiv () =
  let canonical s =
    match Css.of_string ~strict:false (".x{color:" ^ s ^ "}") with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse color: %s" s
  in
  let pairs =
    [
      ("rgb(255 0 0 / 0.5)", "rgb(255 0 0 / 50%)");
      ("rgb(0 0 0 / 0.25)", "rgb(0 0 0 / 25%)");
      ("hsl(0 100% 50% / 0.75)", "hsl(0 100% 50% / 75%)");
    ]
  in
  List.iter
    (fun (number_form, percent_form) ->
      Alcotest.(check string)
        (number_form ^ " is spec-equivalent to " ^ percent_form)
        (canonical number_form) (canonical percent_form))
    pairs

(* CSS Animations Module Level 1, section 3 (Keyframe Selectors): the keyframe
   selectors [from] and [to] are defined as equivalent to [0%] and [100%]
   respectively. A round trip through the parser and printer therefore places
   the rule in the same logical keyframe regardless of which spelling was used
   in the input. *)
let anim171_keyframe_equiv () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "@keyframes [from] is spec-equivalent to [0%]"
    (normalize "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }")
    (normalize "@keyframes fade { 0% { opacity: 0 } 100% { opacity: 1 } }");
  Alcotest.(check string)
    "@keyframes mixed [from]/[100%] and [0%]/[to] are spec-equivalent"
    (normalize "@keyframes fade { from { opacity: 0 } 100% { opacity: 1 } }")
    (normalize "@keyframes fade { 0% { opacity: 0 } to { opacity: 1 } }")

(* CSS Color Module Level 4, sections 1.4 and 12 (rgb() Functional Notation):
   integer channels outside [0, 255] are clamped to that range, and a
   fully-opaque rgb() with all-zero channels is the same color as [black]. Both
   Lightning CSS and cssnano canonicalize the clamped result to the shortest
   equivalent spelling. *)
let color4_12_rgb_clamp_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "rgb(300, 0, 0) clamps to red and canonicalizes" ".x{color:red}"
    (normalize ".x { color: rgb(300, 0, 0) }");
  Alcotest.(check string)
    "rgb(-10, 0, 0) clamps to black and canonicalizes to #000" ".x{color:#000}"
    (normalize ".x { color: rgb(-10, 0, 0) }")

(* CSS Color Module Level 4, section 4.2 (the <alpha-value> syntax): an alpha
   value of [1] (or [100%]) means fully opaque, which is by definition the same
   color as the form without an alpha channel. So [rgba(255, 0, 0, 1)] is
   spec-equivalent to [rgb(255, 0, 0)] and to [red]. Both Lightning CSS and
   cssnano collapse the redundant alpha. *)
let color413_opaque_alpha_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "rgba(255, 0, 0, 1) collapses to red" ".x{color:red}"
    (normalize ".x { color: rgba(255, 0, 0, 1) }");
  Alcotest.(check string)
    "hsla(0, 100%, 50%, 1) collapses to red" ".x{color:red}"
    (normalize ".x { color: hsla(0, 100%, 50%, 1) }");
  Alcotest.(check string)
    "rgba(0, 0, 0, 100%) collapses to #000" ".x{color:#000}"
    (normalize ".x { color: rgba(0, 0, 0, 100%) }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): [<number>] tokens
   with trailing zeroes after a decimal point ([1.000], [1.500]) are equivalent
   to the same value without the trailing zeroes ([1], [1.5]). Both Lightning
   CSS and cssnano normalise these. *)
let v481_trailing_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "1.000em loses trailing zeroes" ".x{width:1em}"
    (normalize ".x { width: 1.000em }");
  Alcotest.(check string)
    "1.500em loses trailing zero" ".x{width:1.5em}"
    (normalize ".x { width: 1.500em }");
  Alcotest.(check string)
    "0.500 loses both leading and trailing zero" ".x{opacity:.5}"
    (normalize ".x { opacity: 0.500 }")

(* CSS Fonts Module Level 4, section 2.2 (Common Weight Name Mapping): the
   keywords [normal] and [bold] for [font-weight] are defined as numeric values
   [400] and [700] respectively. Both Lightning CSS and cssnano canonicalize to
   the numeric form. *)
let fonts4512_weight_number () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font-weight: normal canonicalizes to 400"
    (normalize ".x { font-weight: 400 }")
    (normalize ".x { font-weight: normal }");
  Alcotest.(check string)
    "font-weight: bold canonicalizes to 700"
    (normalize ".x { font-weight: 700 }")
    (normalize ".x { font-weight: bold }")

(* CSS Box Model Module Level 4, sections 7.1 (margin) and 8.1 (padding): the
   margin / padding shorthand expands to four sides and collapses to shorter
   forms when sides repeat. [1px 1px 1px 1px] equals [1px]; [1px 2px 1px 2px]
   equals [1px 2px]. Both Lightning CSS and cssnano apply this collapse. *)
let box4_margin_shorthand_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "margin: 1px 1px 1px 1px collapses to 1px"
    (normalize ".x { margin: 1px }")
    (normalize ".x { margin: 1px 1px 1px 1px }");
  Alcotest.(check string)
    "margin: 0 0 0 0 collapses to 0"
    (normalize ".x { margin: 0 }")
    (normalize ".x { margin: 0 0 0 0 }");
  Alcotest.(check string)
    "padding: 1px 2px 1px 2px collapses to 1px 2px"
    (normalize ".x { padding: 1px 2px }")
    (normalize ".x { padding: 1px 2px 1px 2px }")

(* CSS Color Module Level 4, section 6.3 (The transparent keyword): all forms of
   fully-transparent black ([transparent], [rgba(0, 0, 0, 0)], [#00000000],
   [#0000]) denote the same color. The spec leaves the canonical serialized form
   to implementations; the printer canonicalizes to the shortest spec-equivalent
   spelling, matching Lightning CSS. *)
let color464_transparent_shortest () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let canonical = ".x{color:#0000}" in
  Alcotest.(check string)
    "#0000 is the shortest canonical for transparent" canonical
    (normalize ".x { color: transparent }");
  Alcotest.(check string)
    "rgba(0, 0, 0, 0) canonicalizes to #0000" canonical
    (normalize ".x { color: rgba(0, 0, 0, 0) }");
  Alcotest.(check string)
    "#00000000 canonicalizes to #0000" canonical
    (normalize ".x { color: #00000000 }")

(* CSS Values and Units Module Level 4, section 6 lets zero lengths drop their
   unit. It does not turn zero percentages into numbers. *)
let v465_zero_length_shortest () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let canonical = ".x{width:0}" in
  Alcotest.(check string)
    "0px canonicalizes to 0" canonical
    (normalize ".x { width: 0px }");
  Alcotest.(check string)
    "0em canonicalizes to 0" canonical
    (normalize ".x { width: 0em }");
  Alcotest.(check string)
    "0% stays a percentage" ".x{width:0%}"
    (normalize ".x { width: 0% }");
  Alcotest.(check string)
    "0vh canonicalizes to 0" canonical
    (normalize ".x { width: 0vh }")

(* CSS Backgrounds and Borders Module Level 3, section 4.1 (Border Radius):
   [border-radius] takes one to four [<length-percentage>] values. When all four
   sides are equal, the shorthand collapses to a single value. The spec does not
   mandate the collapsed form; the printer takes the freedom and picks the
   single-value shorthand, matching Lightning CSS. *)
let bg35_radius_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "border-radius: 0 0 0 0 collapses to 0" ".x{border-radius:0}"
    (normalize ".x { border-radius: 0 0 0 0 }");
  Alcotest.(check string)
    "border-radius: 1px 1px 1px 1px collapses to 1px" ".x{border-radius:1px}"
    (normalize ".x { border-radius: 1px 1px 1px 1px }")

(* CSS Values 4 sec. 10.9 determines a math function's type before simplifying
   it. A unitless zero is a [<number>], so adding it to a dimension or using the
   numeric result where a dimension is required makes the declaration invalid.
   Variables remain deferred until substitution, and zero terms with compatible
   dimension types remain valid. *)
let v410_calc_add_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let rejects label css =
    Alcotest.(check bool)
      label true
      (match Css.of_string ~strict:true css with
      | Error _ -> true
      | Ok _ -> false)
  in
  let accepts label css =
    Alcotest.(check bool)
      label true
      (match Css.of_string ~strict:true css with
      | Ok _ -> true
      | Error _ -> false)
  in
  rejects "number plus length is rejected" ".x { width: calc(0 + 1px) }";
  rejects "length plus number is rejected" ".x { width: calc(1px + 0) }";
  rejects "a numeric calc cannot be a width" ".x { width: calc(0) }";
  rejects "number plus angle is rejected" ".x { rotate: calc(45deg + 0) }";
  rejects "number plus time is rejected"
    ".x { transition-duration: calc(1s + 0) }";
  rejects "number plus percentage is rejected" ".x { opacity: calc(.5 + 0%) }";
  rejects "line-height number plus percentage is rejected"
    ".x { line-height: calc(1 + 0%) }";
  accepts "number plus number remains valid" ".x { opacity: calc(.5 + 0) }";
  accepts "a variable keeps type checking deferred"
    ".x { width: calc(var(--size) + 0) }";
  Alcotest.(check string)
    "a cross-unit zero stays in a length sum" ".x{width:calc(1em + 0px)}"
    (normalize ".x { width: calc(1em + 0px) }");
  Alcotest.(check string)
    "a zero percentage stays in a length sum" ".x{width:calc(1px + 0%)}"
    (normalize ".x { width: calc(1px + 0%) }");
  Alcotest.(check string)
    "calc(1px + 0px) simplifies to 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px + 0px) }")

(* CSS Backgrounds and Borders Module Level 3, section 2.6
   (background-position): the two-value form [<x> <y>] collapses to the
   single-value form when [x = y]. The single-value form is the shortest
   spec-equivalent spelling that Lightning CSS picks. *)
let bg336_bgpos_collapse () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "background-position: 50% 50% collapses to 50%"
    ".x{background-position:50%}"
    (normalize ".x { background-position: 50% 50% }");
  Alcotest.(check string)
    "background-position: 0 0 preserves both axes" ".x{background-position:0 0}"
    (normalize ".x { background-position: 0 0 }")

(* {2 Non-minified fidelity}

   Under the default [~minify:false] mode the printer must be faithful to the
   input: spec-allowed canonicalizations like hex shortening, named-color
   conversion, [from]/[to] -> [0%]/[100%] mapping, universal-selector stripping,
   alpha number/percentage canonicalization, and zero-unit drop are all
   minify-only optimizations. A pretty-printed stylesheet should preserve the
   source spelling so the output is suitable for human reading and diffing
   without losing information. *)

(* Helper: round-trip [css] through parse and pretty-print (default
   [~minify:false]) and assert the body of each rule appears verbatim. The
   helper compares fragments rather than full output to be tolerant of the
   pretty-printer's whitespace formatting. *)
let pretty_preserves css fragments =
  match Css.of_string ~strict:false css with
  | Error _ -> Alcotest.failf "failed to parse: %s" css
  | Ok parsed ->
      let printed = Css.to_string parsed.stylesheet in
      List.iter
        (fun fragment ->
          let label =
            String.concat ""
              [
                "non-minified output preserves [";
                fragment;
                "] from input [";
                css;
                "]";
              ]
          in
          Alcotest.(check bool)
            label true
            (Astring.String.is_infix ~affix:fragment printed))
        fragments

(* CSS Color 4 section 5.2 + cascade convention: authored hex colours decode to
   sRGB bytes but keep their source spelling for pretty-printing, while
   minify+optimize emits the shorter equivalent spelling. *)
(* The pretty printer formats every braced at-rule body with the same line
   discipline as a style rule body: one declaration per line at one indent
   level deeper, a trailing semicolon on the last declaration, and the closing
   brace back at the parent's indentation. Sibling blocks inside @keyframes
   are separated by a blank line, like statements in any other block. The
   oracle is Tailwind's authored output format (e.g. the @keyframes bounce
   block in tailwindcss theme.css). *)
let pretty_at_rule_block_bodies () =
  let pretty css =
    match Css.of_string ~strict:false css with
    | Error _ -> Alcotest.failf "failed to parse: %s" css
    | Ok parsed -> Css.to_string parsed.stylesheet |> String.trim
  in
  Alcotest.(check string)
    "@property body formats like a rule body"
    "@property --x {\n\
    \  syntax: \"<length>\";\n\
    \  inherits: true;\n\
    \  initial-value: 0px;\n\
     }"
    (pretty
       "@property --x { syntax: \"<length>\"; inherits: true; initial-value: \
        0px }");
  Alcotest.(check string)
    "@font-face body formats like a rule body"
    "@font-face {\n  font-family: MyFont;\n  src: url(font.woff2);\n}"
    (pretty "@font-face { font-family: MyFont; src: url(font.woff2) }");
  Alcotest.(check string)
    "@keyframes frames format like sibling blocks"
    "@keyframes spin {\n\
    \  from {\n\
    \    transform: rotate(0deg);\n\
    \  }\n\n\
    \  to {\n\
    \    transform: rotate(360deg);\n\
    \  }\n\
     }"
    (pretty
       "@keyframes spin { from { transform: rotate(0deg) } to { transform: \
        rotate(360deg) } }")

let fidelity_hex_form_preserved () =
  pretty_preserves ".x { color: #ff0000 }" [ "#ff0000" ];
  pretty_preserves ".x { color: #f00 }" [ "#f00" ];
  pretty_preserves ".x { color: #FF0000 }" [ "#FF0000" ];
  pretty_preserves ".x { color: #ABCDEF }" [ "#ABCDEF" ];
  assert_minify_and_optimize ".x { color: #ff0000 }" ~minified:".x{color:#f00}"
    ~optimized:".x{color:red}";
  (* Optimisation is the node-changing pass, and CSS Color 4 sec. 5.2 makes the
     digit case carry no meaning, so the authored spelling does not survive it:
     pretty output taken after [Css.optimize] carries the canonical colour, the
     same way [#ff0000] already arrives there as [red]. *)
  Alcotest.(check string)
    "optimize drops the authored hex spelling" ".x {\n  color: #abcdef;\n}"
    (Css.of_string_exn ".x { color: #ABCDEF }"
    |> Css.optimize |> Css.to_string |> String.trim)

(* CSS Color 4 section 5.1 + cascade convention: under non-minified output, the
   named-color and rgb() forms are preserved as written - no cross-form
   canonicalization. *)
let fidelity_color_form_preserved () =
  (* Pretty preserves the colour value and the named/hex spelling, but a colour
     function decodes to one node (no legacy flag), so its syntax canonicalizes
     to modern in pretty too - commas -> spaces, rgba/hsla -> rgb/hsl - exactly
     as a hex colour case-folds (#FF0000 -> #ffffff). *)
  pretty_preserves ".x { color: red }" [ "red" ];
  pretty_preserves ".x { color: rgb(255, 0, 0) }" [ "rgb(255 0 0)" ];
  pretty_preserves ".x { color: hsl(0, 100%, 50%) }" [ "hsl(0 100% 50%)" ];
  pretty_preserves ".x { color: transparent }" [ "transparent" ];
  pretty_preserves ".x { color: rgba(0, 0, 0, 0) }" [ "rgb(0 0 0 / 0)" ];
  pretty_preserves ".x { color: #0000 }" [ "#0000" ]

(* CSS Animations 1 section 3 + cascade convention: [from] / [to] are
   canonicalized to [0%] / [100%] only under minify; the pretty printer keeps
   the source keyword. *)
let fidelity_keyframe_selector_preserved () =
  pretty_preserves "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"
    [ "from"; "to" ];
  pretty_preserves "@keyframes fade { 0% { opacity: 0 } 100% { opacity: 1 } }"
    [ "0%"; "100%" ]

(* CSS Selectors 4 section 5.2 + cascade convention: stripping [*] in a
   non-solitary compound is a minify-only optimization; the pretty printer keeps
   the universal selector as written. *)
let fidelity_universal_in_compound_preserved () =
  pretty_preserves "*.foo { color: red }" [ "*.foo" ];
  pretty_preserves "*#main { color: red }" [ "*#main" ];
  pretty_preserves "*[data-x] { color: red }" [ "*[data-x]" ]

(* CSS Color 4 section 4.2 + cascade convention: alpha unit form is preserved in
   pretty output, while numeric spelling follows the shared shortest decimal
   convention. *)
let fidelity_alpha_form_preserved () =
  pretty_preserves ".x { color: rgba(255, 0, 0, 0.5) }" [ ".5" ];
  pretty_preserves ".x { color: rgb(255 0 0 / 50%) }" [ "50%" ];
  pretty_preserves ".x { color: hsl(180 50% 25% / 30%) }" [ "30%" ]

let fidelity_percentage_precision_preserved () =
  (* [w-1/3] and friends author repeating-fraction percentages; the printer
     keeps every digit (Tailwind does) rather than rounding to six significant
     figures, in pretty and minified output alike. *)
  pretty_preserves ".x { width: 33.333333% }" [ "33.333333%" ];
  Alcotest.(check bool)
    "minify keeps the full percentage" true
    (match Css.of_string ~strict:false ".x { width: 66.666667% }" with
    | Ok parsed ->
        Astring.String.is_infix ~affix:"66.666667%"
          (Css.to_string ~minify:true parsed.stylesheet)
    | Error _ -> false)

(* CSS Values 4 section 6 + cascade convention: dropping the unit on a zero
   length is a minify-only optimization; the pretty printer keeps the source
   spelling. *)
let fidelity_zero_length_preserved () =
  pretty_preserves ".x { width: 0px }" [ "0px" ];
  pretty_preserves ".x { width: 0em }" [ "0em" ];
  pretty_preserves ".x { width: 0% }" [ "0%" ];
  pretty_preserves ".x { margin: 0px 0px 0px 0px }" [ "0px 0px 0px 0px" ]

(* CSS Animations 1 section 3 + cascade convention: shorthand collapses
   ([margin: 1px 1px 1px 1px] -> [margin: 1px], [border-radius: 0 0 0 0] -> [0])
   are minify-only optimizations; the pretty printer keeps the source
   spelling. *)
let fidelity_shorthand_form_preserved () =
  pretty_preserves ".x { margin: 1px 1px 1px 1px }" [ "1px 1px 1px 1px" ];
  pretty_preserves ".x { padding: 1px 2px 1px 2px }" [ "1px 2px 1px 2px" ];
  pretty_preserves ".x { border-radius: 0 0 0 0 }" [ "0 0 0 0" ];
  pretty_preserves ".x { background-position: 50% 50% }" [ "50% 50%" ]

(* CSS Fonts 4 section 2.2 + cascade convention: mapping the [normal] / [bold]
   keywords to the numeric weights [400] / [700] is a minify-only optimization;
   the pretty printer keeps the keyword. *)
let fidelity_font_weight_keyword_preserved () =
  pretty_preserves ".x { font-weight: normal }" [ "normal" ];
  pretty_preserves ".x { font-weight: bold }" [ "bold" ]

(* CSS Selectors Level 4, section 13.3.1 (The :nth-child() Pseudo-class): the
   [<an+b>] microsyntax has multiple spec-equivalent spellings - [2n+1] is the
   same set as [odd]; [2n+0] equals [2n] (matches even); a constant b like
   [(0n+5)] equals [(5)]; and [(1)] is equivalent to the [:first-child]
   pseudo-class. Both Lightning CSS and cssnano canonicalize these to the
   shortest spelling. *)
let s4_14_nth_child_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":nth-child(2n+1) canonicalizes to :nth-child(odd)"
    (normalize ".x :nth-child(odd) { color: red }")
    (normalize ".x :nth-child(2n+1) { color: red }");
  Alcotest.(check string)
    ":nth-child(even) canonicalizes to :nth-child(2n)"
    (normalize ".x :nth-child(2n) { color: red }")
    (normalize ".x :nth-child(even) { color: red }");
  Alcotest.(check string)
    ":nth-child(1) canonicalizes to :first-child"
    (normalize ".x :first-child { color: red }")
    (normalize ".x :nth-child(1) { color: red }")

(* CSS Selectors Level 4, section 6.1 (Attribute selectors): the value in
   [\[attr=value\]] may be an identifier or a string. When the value matches the
   [<ident-token>] grammar, the quotes are redundant and may be dropped; both
   single and double quotes are equivalent. Both minifiers strip redundant
   quotes. *)
let s462_attr_quote_canonical () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "[type=\"text\"] drops redundant quotes to [type=text]"
    (normalize ".x [type=text] { color: red }")
    (normalize ".x [type=\"text\"] { color: red }");
  Alcotest.(check string)
    "[data-x='hello'] drops redundant quotes to [data-x=hello]"
    (normalize ".x [data-x=hello] { color: red }")
    (normalize ".x [data-x='hello'] { color: red }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): scientific
   notation [<number>] tokens like [1e3] and [1.5e2] are spec-equivalent to
   their decimal expansion. Both minifiers expand these to the decimal form when
   shorter. *)
let v481_scientific_notation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "1e3px expands to 1000px" ".x{width:1000px}"
    (normalize ".x { width: 1e3px }");
  Alcotest.(check string)
    "1.5e2px expands to 150px" ".x{width:150px}"
    (normalize ".x { width: 1.5e2px }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): negative zero is
   the same value as zero; [-0px] is the same length as [0]. Both minifiers
   normalise. *)
let v481_negative_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "-0px canonicalizes to 0" ".x{width:0}"
    (normalize ".x { width: -0px }");
  Alcotest.(check string)
    "-0 canonicalizes to 0" ".x{width:0}"
    (normalize ".x { width: -0 }")

(* CSS Values and Units Module Level 4, section 4.5 (URLs): the [<url>] type
   accepts both quoted strings and unquoted token sequences when the URL does
   not contain whitespace, parentheses, or non-printable characters. Both
   minifiers drop the quotes when not needed. *)
let v4_7_url_quote_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "url('image.png') drops quotes" ".x{background:url(image.png)}"
    (normalize ".x { background: url('image.png') }");
  Alcotest.(check string)
    "url(\"image.png\") drops quotes" ".x{background:url(image.png)}"
    (normalize ".x { background: url(\"image.png\") }")

(* CSS Values and Units Module Level 4, section 10 (Mathematical Expressions):
   when all operands of [calc()] are dimensions in the same unit (or numbers),
   the expression simplifies to a single value. Nested [calc()] collapses to a
   single [calc()] (and to a bare value when constant). *)
let v410_calc_nested_constant () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(calc(1px + 2px)) simplifies to 3px" ".x{width:3px}"
    (normalize ".x { width: calc(calc(1px + 2px)) }");
  Alcotest.(check string)
    "calc(1px + 2px) simplifies to 3px" ".x{width:3px}"
    (normalize ".x { width: calc(1px + 2px) }")

(* CSS Cascading and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): consecutive rules with the same condition (same [@media], same
   [@layer]) may be merged into one block, since the cascade evaluates them
   identically. The merge is spec-allowed when no rule with conflicting
   conditions appears between them. *)
let c61_same_condition_merge () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "consecutive @media screen blocks merge into one"
    (normalize "@media screen { .a { color: red } .b { color: blue } }")
    (normalize
       "@media screen { .a { color: red } } @media screen { .b { color: blue } \
        }");
  Alcotest.(check string)
    "consecutive @layer base blocks merge into one"
    (normalize "@layer base { .a { color: red } .b { color: blue } }")
    (normalize
       "@layer base { .a { color: red } } @layer base { .b { color: blue } }")

(* CSS Cascade 6.1 (Cascade Sorting Order): [.a] and the intervening [.b] tie on
   specificity (0,1,0), but the merge folds the later [.a] up into the earlier
   slot and canonicalises the merged body. [.b] writes [color], which neither
   [.a] property conflicts with, so the move is unobservable and the two [.a]
   rules combine across it. A conflicting, same-specificity intervening rule
   would block it. *)
let c61_merge_across_nonconflicting () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize ".a { padding: 10px } .b { color: red } .a { margin: 5px }"
  in
  Alcotest.(check string)
    "non-conflicting intervening rule does not block the merge"
    ".a{margin:5px;padding:10px}.b{color:red}" output

(* {2 More fidelity tests for the new edges}

   Each canonicalization above is a minify-only optimization; the pretty printer
   must keep the source spelling. *)

let fidelity_nth_child_form_preserved () =
  pretty_preserves ".x :nth-child(2n+1) { color: red }" [ "2n+1" ];
  pretty_preserves ".x :nth-child(even) { color: red }" [ "even" ];
  pretty_preserves ".x :nth-child(1) { color: red }" [ ":nth-child(1)" ]

let fidelity_attribute_quotes_preserved () =
  pretty_preserves ".x [type=\"text\"] { color: red }" [ "[type=\"text\"]" ];
  pretty_preserves ".x [data-x='hello'] { color: red }" [ "[data-x='hello']" ]

let fidelity_scientific_notation_preserved () =
  pretty_preserves ".x { width: 1e3px }" [ "1e3px" ];
  pretty_preserves ".x { width: 1.5e2px }" [ "1.5e2px" ]

let fidelity_url_quotes_preserved () =
  pretty_preserves ".x { background: url('image.png') }" [ "url('image.png')" ];
  pretty_preserves ".x { background: url(\"image.png\") }"
    [ "url(\"image.png\")" ]

let fidelity_calc_form_preserved () =
  pretty_preserves ".x { width: calc(1px + 2px) }" [ "calc(1px + 2px)" ];
  pretty_preserves ".x { width: calc(calc(1px + 2px)) }"
    [ "calc(calc(1px + 2px))" ]

(* CSS Cascading and Inheritance Module Level 5, section 6.1 (Cascade Sorting
   Order): when two declarations of the same property tie on every higher-
   priority criterion, only the later wins. The earlier declaration is dead and
   may be removed. Both Lightning CSS and cssnano drop dead duplicates for
   shorthand properties; for differing-value duplicates of the same longhand the
   spec leaves the choice to the implementation. *)
let c6_1_dead_shorthand_removed () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "shadowed margin shorthand removed" ".x{margin:5px}"
    (normalize ".x { margin: 10px; margin: 5px }");
  Alcotest.(check string)
    "exact duplicate property collapsed" ".x{color:red}"
    (normalize ".x { color: red; color: red }")

(* CSS Cascading and Inheritance Module Level 5, section 6.1: an empty
   declaration block contributes no declared values to the cascade. The rule may
   be removed entirely as a no-op. Both minifiers drop empty rules. *)
let c6_1_empty_rule_removed () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "empty rule before populated rule is dropped" ".y{color:red}"
    (normalize ".x { } .y { color: red }");
  Alcotest.(check string)
    "empty rule after populated rule is dropped" ".y{color:red}"
    (normalize ".y { color: red } .x { }")

(* CSS Cascading and Inheritance Module Level 5, section 7.3 (CSS-Wide
   Keywords): the keywords [initial], [inherit], [unset], [revert], and
   [revert-layer] are valid for every property. Implementations must preserve
   them through serialization since they have observable cascade semantics that
   no shorter spelling captures. *)
let c67_css_wide_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let cases = [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] in
  List.iter
    (fun kw ->
      let css = String.concat "" [ ".x { color: "; kw; " }" ] in
      let expected = String.concat "" [ ".x{color:"; kw; "}" ] in
      Alcotest.(check string)
        (String.concat "" [ kw; " preserved through minify" ])
        expected (normalize css))
    cases

(* Cascade L5 SS 7.3: CSS-wide keywords are whole-property values, invalid
   inside lists like [font-family: Arial, inherit]. Per the dual-mode contract
   ([cross_mode_pinning]) lenient parsing drops the declaration and warns;
   nothing remains to preserve in either pretty or minified output. *)
let c67_bad_css_wide_list () =
  let minified css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet
        |> Css.optimize ~flatten_nesting:true
        |> Css.to_string ~minify:true
    | Error e -> Alcotest.fail (Cascade.Error.to_string e)
  in
  Alcotest.(check string)
    "invalid CSS-wide list item drops under minify" ".y{color:red}"
    (minified ".x { font-family: Arial, inherit }.y { color: red }")

(* CSS Cascading and Inheritance Module Level 5, section 3.2 (The all
   Shorthand): the [all] property is a shorthand that sets every CSS-wide
   keyword for all properties. It only accepts CSS-wide keywords as values and
   must round-trip through the printer unchanged. *)
let c632_all_shorthand_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "all: unset preserved" ".x{all:unset}"
    (normalize ".x { all: unset }");
  Alcotest.(check string)
    "all: revert preserved" ".x{all:revert}"
    (normalize ".x { all: revert }");
  Alcotest.(check string)
    "all: initial preserved" ".x{all:initial}"
    (normalize ".x { all: initial }")

(* CSS Cascading and Inheritance Module Level 5, section 6.4 (Cascade Layers):
   named layers preserve their declared order. Two non-adjacent [@layer base]
   blocks separated by an [@layer theme] block still contribute to the same
   layer, so they may be serialized in one [base] block at the first [base]
   occurrence. *)
let c64_named_layers_order () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize
      "@layer base { .a { color: red } } @layer theme { .a { color: blue } } \
       @layer base { .a { padding: 10px } }"
  in
  Alcotest.(check bool)
    "@layer base keeps color declaration" true
    (Astring.String.is_infix ~affix:"color:red" output);
  Alcotest.(check bool)
    "@layer theme block remains" true
    (Astring.String.is_infix ~affix:"color:#00f" output);
  Alcotest.(check bool)
    "@layer base keeps padding declaration" true
    (Astring.String.is_infix ~affix:"padding:10px" output);
  Alcotest.(check bool)
    "same-name @layer base blocks are serialized together before theme" true
    (let find sub = Astring.String.find_sub ~sub output in
     match (find "color:red", find "color:#00f", find "padding:10px") with
     | Some r, Some t, Some p -> r < p && p < t
     | _ -> false)

(* CSS Cascade L5 section 2.1 (Conditional @import) and CSS Custom Properties L1
   section 2 (var()): a [var()] reference with a fallback must be preserved
   end-to-end. The optimizer cannot resolve [var(--undef, red)] to [red] without
   a context that says [--undef] is undefined - that's a cascade-time fact, not
   a syntax-time one. *)
let css_var_fallback_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() with fallback preserved" ".x{color:var(--undef,red)}"
    (normalize ".x { color: var(--undef, red) }");
  Alcotest.(check string)
    "nested var() preserved" ".x{color:var(--a,var(--b,red))}"
    (normalize ".x { color: var(--a, var(--b, red)) }")

(* CSS Cascade L5 section 6.4.2.1 (anonymous @layer): two anonymous layers are
   distinct - they must NOT merge, because the spec states that each [@layer {
   ... }] without a name creates a new, independent layer. *)
let c644_anonymous_layers_distinct () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let output =
    normalize "@layer { .a { color: red } } @layer { .b { color: blue } }"
  in
  let layer_count =
    let rec count from acc =
      match Astring.String.find_sub ~start:from ~sub:"@layer" output with
      | None -> acc
      | Some i -> count (i + 1) (acc + 1)
    in
    count 0 0
  in
  Alcotest.(check int)
    "two anonymous @layer blocks remain as two distinct blocks" 2 layer_count

(* {2 Fidelity tests for dead-code and layer edges} *)

let fidelity_dead_property_preserved () =
  pretty_preserves ".x { color: red; color: blue }"
    [ "color: red"; "color: blue" ];
  pretty_preserves ".x { margin: 10px; margin: 5px }"
    [ "margin: 10px"; "margin: 5px" ]

let fidelity_empty_rule_preserved () =
  (* Empty rules carry no declarations and are dropped even in pretty mode,
     matching Lightning CSS / cssnano. *)
  pretty_preserves ".x { } .y { color: red }" [ ".y" ]

let fidelity_incomplete_font_face_preserved () =
  pretty_preserves "@font-face { src: url(font.woff2); }"
    [ "@font-face"; "src: url(font.woff2)" ];
  pretty_preserves "@font-face { font-family: Brand; }"
    [ "@font-face"; "font-family: Brand" ]

let fidelity_css_wide_keywords_preserved () =
  pretty_preserves ".x { color: revert }" [ "revert" ];
  pretty_preserves ".x { color: revert-layer }" [ "revert-layer" ];
  pretty_preserves ".x { color: unset }" [ "unset" ];
  pretty_preserves ".x { all: initial }" [ "all"; "initial" ]

(* CSS Backgrounds and Borders Module Level 3, section 2.6
   (background-position): the keyword pairs [top left] / [left top] denote the
   same position as [0% 0%], and [bottom right] denotes [100% 100%]. Both
   Lightning CSS and cssnano canonicalize the keyword form to the numeric
   one. *)
let bg336_position_keyword () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  (* Per CSS Backgrounds L3 sec. 2.6 [top left], [left top], and [0% 0%] all
     denote position [0 0]. *)
  Alcotest.(check string)
    "background-position: top left -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: top left }");
  Alcotest.(check string)
    "background-position: left top -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: left top }");
  Alcotest.(check string)
    "background-position: bottom right -> 100% 100%"
    ".x{background-position:100%100%}"
    (normalize ".x { background-position: bottom right }")

(* CSS Cascade L5 section 6.1: when two rules with different selectors share the
   same declaration block, they may be grouped into a selector list with one
   block. The cascade evaluates the grouped form identically because each
   selector contributes the same declared values at its own specificity. Both
   minifiers group same-block rules. *)
let c6_1_selector_grouping () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let grouped =
    normalize ".a { color: red } .b { color: red } .c { color: red }"
  in
  Alcotest.(check bool)
    "consecutive same-block rules group into a selector list" true
    (Astring.String.is_infix ~affix:".a,.b,.c" grouped
    || Astring.String.is_infix ~affix:".a, .b, .c" grouped);
  Alcotest.(check bool)
    "the grouped block has only one declaration appearance" true
    (let rec count from acc =
       match Astring.String.find_sub ~start:from ~sub:"color:red" grouped with
       | None -> acc
       | Some i -> count (i + 1) (acc + 1)
     in
     count 0 0 = 1)

(* CSS Syntax L3 section 4.3.9 (Check if three code points would start an ident
   sequence): vendor-prefixed properties such as [-webkit-transform] and
   [-moz-user-select] use the dashed-ident escape hatch and are unknown to the
   CSS spec. The printer must round-trip them unchanged - both Lightning CSS and
   cssnano keep vendor prefixes. *)
let vendor_prefix_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "-webkit-transform preserved" ".x{-webkit-transform:rotate(45deg)}"
    (normalize ".x { -webkit-transform: rotate(45deg) }");
  Alcotest.(check string)
    "-moz-user-select preserved" ".x{-moz-user-select:none}"
    (normalize ".x { -moz-user-select: none }")

(* CSS Text Decoration 3 section 2.3 defines [text-decoration-color] as the
   standards-track property. [-webkit-text-decoration-color] is compatibility
   syntax: preserve it when authored, but do not synthesize it from the standard
   property or collapse authored prefixed/unprefixed pairs. Lightning CSS,
   esbuild, CSSO, clean-css, and cssnano all keep both spellings distinct. *)
let webkit_decoration_color_compat () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "standard text-decoration-color does not synthesize webkit"
    ".x{text-decoration-color:#00f}"
    (normalize ".x { text-decoration-color: blue }");
  Alcotest.(check string)
    "authored webkit text-decoration-color is preserved"
    ".x{-webkit-text-decoration-color:#00f}"
    (normalize ".x { -webkit-text-decoration-color: blue }");
  Alcotest.(check string)
    "webkit and standard pair remains in source order"
    ".x{-webkit-text-decoration-color:red;text-decoration-color:#00f}"
    (normalize
       ".x { -webkit-text-decoration-color: red; text-decoration-color: blue }");
  Alcotest.(check string)
    "standard and webkit pair remains in source order"
    ".x{text-decoration-color:#00f;-webkit-text-decoration-color:red}"
    (normalize
       ".x { text-decoration-color: blue; -webkit-text-decoration-color: red }");
  Alcotest.(check string)
    "webkit text-decoration inherit fallback survives stylesheet optimize"
    ".x{-webkit-text-decoration:inherit;text-decoration:inherit}"
    (normalize
       ".x { -webkit-text-decoration: inherit; text-decoration: inherit }")

(* CSS Selectors Level 4, section 4.3 (Negation Pseudo-class): the two forms
   [:not(A, B)] and [:not(A):not(B)] are spec-distinct - the first is a single
   negation matching anything that's not A or B, the second chains two
   negations. Both forms must round-trip preserved through the minifier (both
   Lightning CSS and cssnano keep them). *)
let s443_not_form_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":not(.a, .b) preserved" ".x :not(.a,.b){color:red}"
    (normalize ".x :not(.a, .b) { color: red }");
  Alcotest.(check string)
    ":not(.a):not(.b) preserved" ".x :not(.a):not(.b){color:red}"
    (normalize ".x :not(.a):not(.b) { color: red }")

(* CSS Backgrounds and Borders Module Level 3 (border shorthand) and Outline
   Module (outline shorthand): keyword shortcuts like [border: 0], [border:
   none], [outline: 0], [outline: none] and Text Decoration L3 4.1
   [text-decoration: none] must round-trip unchanged. Both Lightning CSS and
   cssnano preserve these forms. *)
let display3_border_keyword_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "border: 0 preserved" ".x{border:0}"
    (normalize ".x { border: 0 }");
  Alcotest.(check string)
    "border: none preserved" ".x{border:none}"
    (normalize ".x { border: none }");
  Alcotest.(check string)
    "outline: 0 preserved" ".x{outline:0}"
    (normalize ".x { outline: 0 }");
  Alcotest.(check string)
    "outline: none preserved" ".x{outline:none}"
    (normalize ".x { outline: none }");
  Alcotest.(check string)
    "text-decoration: none preserved" ".x{text-decoration:none}"
    (normalize ".x { text-decoration: none }")

(* {2 Fidelity tests for the new edges} *)

let fidelity_position_keywords_preserved () =
  pretty_preserves ".x { background-position: top left }" [ "top left" ];
  pretty_preserves ".x { background-position: bottom right }" [ "bottom right" ];
  pretty_preserves ".x { transform-origin: center center }" [ "center center" ]

let fidelity_vendor_prefix_preserved () =
  pretty_preserves ".x { -webkit-transform: rotate(45deg) }"
    [ "-webkit-transform" ];
  pretty_preserves ".x { -moz-user-select: none }" [ "-moz-user-select" ];
  pretty_preserves
    ".x { -webkit-text-decoration-color: red; text-decoration-color: blue }"
    [ "-webkit-text-decoration-color"; "text-decoration-color" ]

let fidelity_not_form_preserved () =
  pretty_preserves ".x :not(.a, .b) { color: red }" [ ":not(.a, .b)" ];
  pretty_preserves ".x :not(.a):not(.b) { color: red }" [ ":not(.a):not(.b)" ]

(* CSS Values and Units Module Level 4, section 7.2 (Time Units): [s] and [ms]
   are the two units of [<time>], with [1s = 1000ms]. Both minifiers
   canonicalize to the shorter spelling - [0s] for any zero, [1s] over [1000ms],
   [.1s] over [100ms]. *)
let v466_time_unit_canonical () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "0ms canonicalizes to 0s" ".x{transition-duration:0s}"
    (normalize ".x { transition-duration: 0ms }");
  Alcotest.(check string)
    "1000ms canonicalizes to 1s" ".x{transition-duration:1s}"
    (normalize ".x { transition-duration: 1000ms }");
  Alcotest.(check string)
    "100ms canonicalizes to .1s" ".x{transition-duration:.1s}"
    (normalize ".x { transition-duration: 100ms }");
  Alcotest.(check string)
    "500ms canonicalizes to .5s" ".x{transition-duration:.5s}"
    (normalize ".x { transition-duration: 500ms }")

(* CSS Values and Units Module Level 4, section 6.2 (Absolute Lengths): absolute
   lengths ([in], [cm], [mm], [pt], [pc], [q]) are mathematically
   inter-convertible to [px] but the conversion produces non-terminating
   decimals or longer spellings in most cases. Industry minifiers (Lightning CSS
   and cssnano) preserve the source unit; the CSSOM-style specified value should
   round-trip unchanged. *)
let v461_absolute_units_minify () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "10cm preserved" ".x{width:10cm}"
    (normalize ".x { width: 10cm }");
  Alcotest.(check string)
    "1in preserved" ".x{width:1in}"
    (normalize ".x { width: 1in }");
  Alcotest.(check string)
    "10pt preserved" ".x{font-size:10pt}"
    (normalize ".x { font-size: 10pt }");
  Alcotest.(check string)
    "1pc preserved" ".x{width:1pc}"
    (normalize ".x { width: 1pc }");
  Alcotest.(check string)
    "25.4mm preserved" ".x{width:25.4mm}"
    (normalize ".x { width: 25.4mm }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): negative lengths
   and percentages are valid in many contexts (margin, transform translate,
   etc.) and must round-trip preserved. The leading [-] is part of the value,
   not a separate token. *)
let v481_negative_units_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "negative -10px preserved" ".x{margin:-10px}"
    (normalize ".x { margin: -10px }");
  Alcotest.(check string)
    "negative -10% preserved" ".x{margin:-10%}"
    (normalize ".x { margin: -10% }");
  Alcotest.(check string)
    "negative -.5em preserved" ".x{margin:-.5em}"
    (normalize ".x { margin: -0.5em }");
  Alcotest.(check string)
    "negative angle -90deg preserved" ".x{transform:rotate(-90deg)}"
    (normalize ".x { transform: rotate(-90deg) }")

(* CSS Values and Units Module Level 4, section 5 (Numeric Data Types): trailing
   zeros after a decimal point are not significant - [10.0px] is the same number
   as [10px], [10.50px] is [10.5px]. Both minifiers normalise to drop trailing
   zeros. *)
let v4_8_trailing_zero_drop () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "10.0px drops trailing zero to 10px" ".x{width:10px}"
    (normalize ".x { width: 10.0px }");
  Alcotest.(check string)
    "10.50px drops trailing zero to 10.5px" ".x{width:10.5px}"
    (normalize ".x { width: 10.50px }");
  Alcotest.(check string)
    "1.0 line-height drops trailing zero to 1" ".x{line-height:1}"
    (normalize ".x { line-height: 1.0 }")

(* CSS Sizing Module Level 4, section 4.1 (aspect-ratio): the [aspect-ratio]
   property accepts [<ratio>] which is two [<number>]s separated by [/]. Both
   minifiers preserve [16/9] (with whitespace dropped) and the single-value form
   [1] (which is shorthand for [1/1]). *)
let sizing4_5_aspect_ratio_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "aspect-ratio: 16 / 9 -> 16/9 (whitespace dropped)" ".x{aspect-ratio:16/9}"
    (normalize ".x { aspect-ratio: 16 / 9 }");
  Alcotest.(check string)
    "aspect-ratio: 1 preserved" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: 1 }")

(* CSS Values and Units Module Level 4, section 6 + spec rejection: a bare
   number without a unit is not a valid [<length>] outside of zero. Both parsers
   reject [width: 10] (no unit), [margin: 5] (no unit). *)
let v481_negative_unit_length () =
  Alcotest.(check bool)
    "width: 10 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "margin: 5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { margin: 5 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "padding: 1.5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { padding: 1.5 }" with
    | Error _ -> true
    | _ -> false)

(* CSS Values and Units Module Level 4, section 7.2 + spec rejection: a bare
   number is not a valid [<time>] - units [s] or [ms] are required. Exception:
   [0] is valid as zero in some contexts but [transition-duration: 1] (no unit)
   must be rejected. *)
let v466_time_unit_required () =
  Alcotest.(check bool)
    "transition-duration: 1 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { transition-duration: 1 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "animation-duration: 1.5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { animation-duration: 1.5 }" with
    | Error _ -> true
    | _ -> false)

(* CSS Values and Units Module Level 4, section 6 + spec rejection: unknown
   length units like [10pp], [10foo] must be rejected as parse errors. *)
let v461_unknown_length_unit () =
  Alcotest.(check bool)
    "width: 10pp (unknown unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10pp }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "width: 10foo (unknown unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10foo }" with
    | Error _ -> true
    | _ -> false)

(* {2 Fidelity tests for unit edges} *)

let fidelity_time_unit_preserved () =
  pretty_preserves ".x { transition-duration: 0ms }" [ "0ms" ];
  pretty_preserves ".x { transition-duration: 1000ms }" [ "1000ms" ];
  pretty_preserves ".x { transition-duration: 100ms }" [ "100ms" ];
  pretty_preserves ".x { transition-duration: .5s }" [ ".5s" ]

let fidelity_absolute_units_preserved () =
  pretty_preserves ".x { width: 10cm }" [ "10cm" ];
  pretty_preserves ".x { width: 1in }" [ "1in" ];
  pretty_preserves ".x { font-size: 10pt }" [ "10pt" ];
  pretty_preserves ".x { width: 1pc }" [ "1pc" ];
  pretty_preserves ".x { width: 25.4mm }" [ "25.4mm" ]

let fidelity_negative_units_preserved () =
  pretty_preserves ".x { margin: -10px }" [ "-10px" ];
  pretty_preserves ".x { margin: -10% }" [ "-10%" ];
  pretty_preserves ".x { transform: rotate(-90deg) }" [ "-90deg" ]

let fidelity_trailing_zero_preserved () =
  pretty_preserves ".x { width: 10.0px }" [ "10.0px" ];
  pretty_preserves ".x { width: 10.50px }" [ "10.50px" ];
  pretty_preserves ".x { line-height: 1.0 }" [ "1.0" ]

let fidelity_aspect_ratio_preserved () =
  pretty_preserves ".x { aspect-ratio: 16 / 9 }" [ "16 / 9" ];
  pretty_preserves ".x { aspect-ratio: 1 }" [ "1" ]

(* CSS Values and Units Module Level 4, section 10.10.1 (Simplification): calc()
   simplifies under spec rules - a single-operand calc collapses to the operand;
   multiplication/division/addition with constants in the same unit fold to the
   result; calc() with a [var()] reference must be preserved because the value
   is unknown at parse time. Both Lightning CSS and cssnano agree on these
   simplifications. *)
let v4102_calc_single () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px) }");
  Alcotest.(check string)
    "calc(-5px) -> -5px" ".x{width:-5px}"
    (normalize ".x { width: calc(-5px) }");
  Alcotest.(check string)
    "calc(1) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(1) }")

let v4_10_2_calc_arithmetic () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px * 2) -> 2px" ".x{width:2px}"
    (normalize ".x { width: calc(1px * 2) }");
  Alcotest.(check string)
    "calc(10px / 2) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(10px / 2) }");
  Alcotest.(check string)
    "calc(2 * 3) -> 6" ".x{aspect-ratio:6}"
    (normalize ".x { aspect-ratio: calc(2 * 3) }");
  (* A property whose value accepts a bare [<number>] (line-height) must still
     read the [<number>] operands of a calc multiplication as numbers, not as
     typed values that the dimension check rejects. *)
  Alcotest.(check string)
    "line-height calc(2 * 3) -> 6" ".x{line-height:6}"
    (normalize ".x { line-height: calc(2 * 3) }");
  Alcotest.(check string)
    "line-height calc(2px * 1) -> 2px" ".x{line-height:2px}"
    (normalize ".x { line-height: calc(2px * 1) }");
  Alcotest.(check string)
    "calc(1px - 1px) -> 0" ".x{width:0}"
    (normalize ".x { width: calc(1px - 1px) }");
  Alcotest.(check string)
    "calc(1px * 0) -> 0" ".x{width:0}"
    (normalize ".x { width: calc(1px * 0) }");
  Alcotest.(check string)
    "a numeric calc cannot be a width" ""
    (normalize ".x { width: calc(0 + 0) }")

let v4102_calc_addition () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2px + 3px) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(1px + 2px + 3px) }");
  Alcotest.(check string)
    "calc((1px + 2px) * 2) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc((1px + 2px) * 2) }");
  Alcotest.(check string)
    "calc(1px*2 + 3px*4) -> 14px" ".x{width:14px}"
    (normalize ".x { width: calc(1px*2 + 3px*4) }")

let v4102_calc_percentage () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(100% - 50%) -> 50%" ".x{width:50%}"
    (normalize ".x { width: calc(100% - 50%) }");
  Alcotest.(check string)
    "calc(50% + 50%) -> 100%" ".x{width:100%}"
    (normalize ".x { width: calc(50% + 50%) }");
  Alcotest.(check string)
    "calc(100% * 2) -> 200%" ".x{width:200%}"
    (normalize ".x { width: calc(100% * 2) }")

(* CSS Values and Units Module Level 4, section 10.10.1: calc() with mixed units
   that cannot be reduced at parse time must be preserved. Examples are
   [calc(100vh - 50px)] (mixed viewport + absolute), [calc(100% - 10px)] (mixed
   percentage + absolute), and any expression containing [var()] - their values
   are not known until resolution time. *)
let v4102_calc_mixed_unit () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(100vh - 50px) preserved" ".x{width:calc(100vh - 50px)}"
    (normalize ".x { width: calc(100vh - 50px) }");
  Alcotest.(check string)
    "calc(100% - 10px) preserved" ".x{width:calc(100% - 10px)}"
    (normalize ".x { width: calc(100% - 10px) }");
  Alcotest.(check string)
    "calc(var(--x) + 10px) preserved" ".x{width:calc(var(--x) + 10px)}"
    (normalize ".x { width: calc(var(--x) + 10px) }")

(* CSS Values L4, section 10 + spec rejection: invalid calc() forms are parse
   errors. [calc()] (empty), [calc(10px +)] (trailing operator), a calc body
   starting with [*] (leading operator) must all be rejected. *)
let v4_10_invalid_calc_rejected () =
  Alcotest.(check bool)
    "calc() with no arguments is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(10px +) trailing operator is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(10px +) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(* 5) leading operator is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(* 5) }" with
    | Error _ -> true
    | _ -> false)

(* {2 Fidelity tests for calc edges} *)

let fidelity_calc_simplifiable_preserved () =
  pretty_preserves ".x { width: calc(1px + 2px) }" [ "calc(1px + 2px)" ];
  pretty_preserves ".x { width: calc(1px * 2) }" [ "calc(1px * 2)" ];
  pretty_preserves ".x { width: calc(100% - 50%) }" [ "calc(100% - 50%)" ]

let fidelity_calc_mixed_unit_preserved () =
  pretty_preserves ".x { width: calc(100vh - 50px) }" [ "calc(100vh - 50px)" ];
  pretty_preserves ".x { width: calc(100% - 10px) }" [ "calc(100% - 10px)" ];
  pretty_preserves ".x { width: calc(var(--x) + 10px) }"
    [ "calc(var(--x) + 10px)" ]

(* CSS Values L4 section 10.10.1: nested calc() collapses to a single calc() and
   all-constant nested forms reduce to a single value. Both minifiers fully
   unwrap. *)
let v4102_calc_nested () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(calc(1px) + calc(2px)) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(calc(1px) + calc(2px)) }");
  Alcotest.(check string)
    "calc(calc(calc(1px))) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(calc(calc(1px))) }");
  Alcotest.(check string)
    "calc(calc(1px + 2px) * 2) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(calc(1px + 2px) * 2) }")

(* CSS Values L4 section 10.2 (min(), max()): when all arguments are constants
   reducible at parse time the result is a single value. Per shortest-wins
   cascade picks the reduced form. *)
let v4107_minmax_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "min(1px, 2px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: min(1px, 2px) }");
  Alcotest.(check string)
    "max(1px, 2px) -> 2px" ".x{width:2px}"
    (normalize ".x { width: max(1px, 2px) }");
  Alcotest.(check string)
    "min(min(1px, 2px), 3px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: min(min(1px, 2px), 3px) }");
  (* CSS Values 4 section 10: [clamp(lo, v, hi)] is [max(lo, min(v, hi))], so a
     clamp of three same-unit literals folds to a single value (returning [lo]
     when [lo > hi]). Mixed units cannot be compared and are preserved. *)
  Alcotest.(check string)
    "clamp(1rem, 2rem, 3rem) -> 2rem" ".x{width:2rem}"
    (normalize ".x { width: clamp(1rem, 2rem, 3rem) }");
  Alcotest.(check string)
    "clamp(3rem, 2rem, 1rem) -> 3rem" ".x{width:3rem}"
    (normalize ".x { width: clamp(3rem, 2rem, 1rem) }");
  Alcotest.(check string)
    "clamp(1rem, 2vw, 3rem) stays (mixed units)"
    ".x{width:clamp(1rem,2vw,3rem)}"
    (normalize ".x { width: clamp(1rem, 2vw, 3rem) }");
  Alcotest.(check string)
    "font-size folds a constant clamp too" ".x{font-size:2rem}"
    (normalize ".x { font-size: clamp(1rem, 2rem, 3rem) }")

(* CSS Selectors L4 section 4.2 (:is()): a single-argument [:is(.x)] matches the
   same elements as bare [.x] with the same specificity. Per shortest-wins
   cascade picks the unwrapped form. *)
let s417_is_unwrap () =
  (* CSS Selectors L4 sec. 4.2: a single-argument [:is(.a)] is spec-equivalent
     to bare [.a] (same match set, same specificity). Under [~minify:true] the
     printer picks the shortest spec-equivalent spelling - the unwrapped form,
     matching Lightning CSS. Non-minified output preserves the wrapper (see
     fidelity_nested_is_preserved). *)
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":is(:is(.a)) unwraps to .a"
    (normalize ".x .a { color: red }")
    (normalize ".x :is(:is(.a)) { color: red }");
  Alcotest.(check string)
    ":not(:is(.a)) unwraps inner :is to :not(.a)"
    (normalize ".x :not(.a) { color: red }")
    (normalize ".x :not(:is(.a)) { color: red }");
  Alcotest.(check string)
    ":is(.a):is(.b) unwraps to .a.b"
    (normalize ".x .a.b { color: red }")
    (normalize ".x :is(.a):is(.b) { color: red }")

(* CSS Selectors L4 section 4.2 again, at the top of a rule selector: [:is()]
   takes the specificity of its most specific argument, so equal-specificity
   arguments make [:is(a, b)] and the selector list [a, b] the same rule. Two
   rules with the same selector are one rule, and factoring reads selector
   nodes, so the split has to be an AST rewrite: printed only, the two rules
   below show one selector, never merge, and a second pass over the output finds
   the merge the first missed. *)
let s417_is_unwrap_top_level () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let two_rules = ":is(a, b) { color: red } a, b { margin: 0 }" in
  Alcotest.(check string)
    ":is(a,b) and a,b are one rule" "a,b{color:red;margin:0}"
    (normalize two_rules);
  Alcotest.(check string)
    "minify is a fixpoint" (normalize two_rules)
    (normalize (normalize two_rules));
  (* A lone argument has nothing to disagree with, and at the top of a rule
     selector it is not landing in a compound, so section 3.1 (a type or
     universal selector comes first in a compound) does not hold it back. *)
  Alcotest.(check string)
    ":is(a) unwraps at the top" "a{color:red}"
    (normalize ":is(a) { color: red }");
  Alcotest.(check string)
    ":is(*) unwraps at the top" "*{color:red}"
    (normalize ":is(*) { color: red }");
  (* Unequal specificity, so the list would weigh an [a] match lighter than the
     wrapper does. *)
  Alcotest.(check string)
    ":is(.x,a) keeps its wrapper" ":is(.x,a){color:red}"
    (normalize ":is(.x, a) { color: red }");
  (* [:where()] contributes zero specificity, so it never becomes a list. *)
  Alcotest.(check string)
    ":where(a,b) keeps its wrapper" ":where(a,b){color:red}"
    (normalize ":where(a, b) { color: red }")

(* CSS Selectors L4 section 4.2 (compound selector): "if it contains a type
   selector or universal selector, that selector must come first". So a
   single-argument [:is(<type>)] unwraps only into a compound it leads: spliced
   anywhere else the two names fuse, and [.a:is(code)] turns into [.acode],
   which matches a class nobody wrote. Same for [:not(:not(<type>))]. *)
let s442_compound_type_first () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ".a:is(code) keeps its wrapper" ".x .a:is(code){color:red}"
    (normalize ".x .a:is(code) { color: red }");
  Alcotest.(check string)
    "div:is(code) keeps its wrapper" ".x div:is(code){color:red}"
    (normalize ".x div:is(code) { color: red }");
  Alcotest.(check string)
    ":is(<ancestor> *):is(code) keeps its wrapper"
    ":is(.a *):is(code){color:red}"
    (normalize ":is(.a *):is(code) { color: red }");
  Alcotest.(check string)
    ":is(code.b) keeps its wrapper" ".a:is(code.b){color:red}"
    (normalize ".a:is(code.b) { color: red }");
  Alcotest.(check string)
    "a namespaced universal keeps its wrapper" ".a:is(svg|*){color:red}"
    (normalize ".a:is(svg|*) { color: red }");
  Alcotest.(check string)
    ":not(:not(code)) keeps its wrappers" ".a:not(:not(code)){color:red}"
    (normalize ".a:not(:not(code)) { color: red }");
  (* Leading its compound the type selector would be where it belongs, and
     unwrapping there is sound - but the rewrite is node-local, so it declines
     rather than guess at a position it cannot see. *)
  Alcotest.(check string)
    ":is(code) leading a compound keeps its wrapper too"
    ".x :is(code).a{color:red}"
    (normalize ".x :is(code).a { color: red }");
  Alcotest.(check string)
    ":is(*) keeps its wrapper" ".a:is(*){color:red}"
    (normalize ".a:is(*) { color: red }");
  (* An argument with no type selector is spliceable wherever the [:is()]
     stands, so those keep unwrapping. *)
  Alcotest.(check string)
    ":is(.b) still unwraps" ".a.b{color:red}"
    (normalize ".a:is(.b) { color: red }");
  Alcotest.(check string)
    ":not(:not(.b)) still folds" ".a.b{color:red}"
    (normalize ".a:not(:not(.b)) { color: red }")

(* CSS Selectors L4 section 6.1: attribute compound selectors drop quotes per
   attribute when the value is a valid identifier. *)
let s462_compound_attr () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "[data-x=\"a\"][data-y=\"b\"] drops quotes per attribute"
    ".x [data-x=a][data-y=b]{color:red}"
    (normalize ".x [data-x=\"a\"][data-y=\"b\"] { color: red }")

(* CSS Selectors L4 section 3.1 (compound selector): chained pseudo- classes
   preserve their order and do not deduplicate. *)
let s442_compound_pseudo_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    ":hover:focus:active preserved" ".x :hover:focus:active{color:red}"
    (normalize ".x :hover:focus:active { color: red }")

(* CSS Transforms L1 section 8: multiple transform functions stack in source
   order; whitespace between them is optional for parsing. Lightning CSS drops
   it; per shortest-wins cascade follows. *)
let transforms1_11_chain_whitespace_dropped () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "transform chain whitespace dropped"
    ".x{transform:translate(10px)translate(20px)}"
    (normalize ".x { transform: translate(10px) translate(20px) }")

(* CSS Custom Properties L1 section 2 (var()): variable references are
   late-bound through the cascade and resolved at computed-value time. The
   optimizer cannot inline a variable without a context that resolves it (a
   [theme] map keyed on custom property names), so the [var()] reference and its
   [calc()] wrapper round-trip preserved. The value-independent CSS Values 4
   sec. 10.10.1 identities ([x * 1], [x + 0], ...) still fold: they hold for
   every possible substitution because the [var()] stays inside calc()'s
   grammar, so [calc(var(--x) * 1)] shortens to [calc(var(--x))] (not to bare
   [var(--x)], which sec. 10.10 forbids without knowing the value). *)
let customprops12_inlining () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x) preserved without context" ".x{color:var(--x)}"
    (normalize ".x { color: var(--x) }");
  Alcotest.(check string)
    "var(--x, red) fallback preserved" ".x{color:var(--x,red)}"
    (normalize ".x { color: var(--x, red) }");
  Alcotest.(check string)
    "calc with var preserved" ".x{width:calc(var(--gap)*2)}"
    (normalize ".x { width: calc(var(--gap) * 2) }");
  Alcotest.(check string)
    "single var inside calc is not equivalent to bare var"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing)) }");
  Alcotest.(check string)
    "calc var times one folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) * 1) }");
  Alcotest.(check string)
    "calc one times var folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(1 * var(--spacing)) }");
  Alcotest.(check string)
    "calc var divided by one folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) / 1) }");
  Alcotest.(check string)
    "calc var plus zero keeps the typed term"
    ".x{padding:calc(var(--spacing) + 0px)}"
    (normalize ".x { padding: calc(var(--spacing) + 0px) }");
  Alcotest.(check string)
    "calc zero plus var keeps the typed term"
    ".x{padding:calc(0px + var(--spacing))}"
    (normalize ".x { padding: calc(0px + var(--spacing)) }");
  Alcotest.(check string)
    "calc var minus zero keeps the typed term"
    ".x{padding:calc(var(--spacing) - 0px)}"
    (normalize ".x { padding: calc(var(--spacing) - 0px) }");
  Alcotest.(check string)
    "calc var-free left subtree may fold before var"
    ".x{padding:calc(3px + var(--spacing))}"
    (normalize ".x { padding: calc((1px + 2px) + var(--spacing)) }");
  Alcotest.(check string)
    "calc var-free right subtree may fold after var"
    ".x{padding:calc(var(--spacing)*6)}"
    (normalize ".x { padding: calc(var(--spacing) * (2 * 3)) }");
  Alcotest.(check string)
    "calc var-free percentage subtree may fold before var"
    ".x{padding:calc(50% - var(--spacing))}"
    (normalize ".x { padding: calc((100% / 2) - var(--spacing)) }");
  Alcotest.(check string)
    "rule defining the variable preserved alongside its use"
    ":root{--brand:red}.x{color:var(--brand)}"
    (normalize ":root { --brand: red } .x { color: var(--brand) }")

(* CSS Custom Properties L1 section 3 (Using Cascading Variables): normal CSS
   minifiers do not know the computed value of [var()] references. Lightning,
   cssnano, csso, clean-css, and esbuild all preserve the wrapper and only
   minify tokens that are still syntax-local, such as color fallbacks. *)
let customprops12_runtime_vars () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font-family var survives normal minification"
    ".font-sans{font-family:var(--font-sans)}"
    (normalize ".font-sans { font-family: var(--font-sans) }");
  Alcotest.(check string)
    "authored fallback survives normal minification"
    ".font-sans{font-family:var(--font-sans,Arial,sans-serif)}"
    (normalize ".font-sans { font-family: var(--font-sans, Arial, sans-serif) }");
  Alcotest.(check string)
    "fallback tokens still canonicalize to shortest valid CSS"
    ".accent{color:var(--brand,red)}"
    (normalize ".accent { color: var(--brand, #ff0000) }");
  Alcotest.(check string)
    "custom property definition is not substituted into its use"
    ":root{--font-sans:ui-sans-serif,system-ui,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (normalize
       ":root { --font-sans: ui-sans-serif, system-ui, sans-serif } .font-sans \
        { font-family: var(--font-sans) }");
  Alcotest.(check bool)
    "runtime var is not rewritten to a static font stack" false
    (let out =
       normalize
         ":root { --font-sans: ui-sans-serif, system-ui, sans-serif } \
          .font-sans { font-family: var(--font-sans) }"
     in
     Astring.String.is_infix
       ~affix:".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}" out)

(* CSS Custom Properties L1 section 3 only substitutes actual [var()] function
   references. Text that merely contains the characters "var(...)" is ordinary
   string or URL payload and remains outside variable inlining. *)
let customprops12_text_payload () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.empty in
  let resolve = function "brand" -> Some "red" | _ -> None in
  Alcotest.(check bool)
    "string payload is not treated as a var() reference" true
    (let out =
       parse ".x { content: \"var(--brand)\" }"
       |> Css.resolve_theme ~theme ~theme_defaults:resolve
       |> Css.to_string ~minify:true
     in
     Astring.String.is_infix ~affix:"var(--brand)" out
     && not (Astring.String.is_infix ~affix:"red" out))

(* CSS Custom Properties L1 section 3 (Resolving Dependency Cycles): a variable
   that references itself directly or indirectly is invalid at computed time. At
   syntax time the chain is preserved - the cycle detection happens in the
   cascade. *)
let customprops15_cycle () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "self-referential var preserved at syntax layer" true
    (let out = normalize ":root { --x: var(--x) }" in
     Astring.String.is_infix ~affix:"--x:var(--x)" out)

(* {2 Fidelity tests for nested and var edges} *)

let fidelity_nested_calc_preserved () =
  pretty_preserves ".x { width: calc(calc(1px) + calc(2px)) }"
    [ "calc(calc(1px)" ];
  pretty_preserves ".x { width: calc(calc(calc(1px))) }"
    [ "calc(calc(calc(1px)))" ]

let fidelity_nested_is_preserved () =
  pretty_preserves ".x :is(:is(.a)) { color: red }" [ ":is(:is(.a))" ];
  pretty_preserves ".x :not(:is(.a)) { color: red }" [ ":not(:is(.a))" ];
  pretty_preserves ".x :is(.a):is(.b) { color: red }" [ ":is(.a):is(.b)" ]

let fidelity_min_max_preserved () =
  pretty_preserves ".x { width: min(1px, 2px) }" [ "min(1px, 2px)" ];
  pretty_preserves ".x { width: max(1px, 2px) }" [ "max(1px, 2px)" ]

let fidelity_compound_pseudo_preserved () =
  pretty_preserves ".x :hover:focus:active { color: red }"
    [ ":hover:focus:active" ]

let fidelity_transform_chain_preserved () =
  pretty_preserves ".x { transform: translate(10px) translate(20px) }"
    [ "translate(10px) translate(20px)" ]

let fidelity_var_preserved () =
  pretty_preserves ".x { color: var(--brand) }" [ "var(--brand)" ];
  pretty_preserves ".x { color: var(--x, red) }" [ "var(--x, red)" ];
  pretty_preserves ".x { width: calc(var(--gap) * 2) }" [ "var(--gap)" ]

(* {2 Easing / timing functions (CSS Easing L1)} *)

(* CSS Easing Module Level 1, section 2 (The easing functions): the named
   keywords [linear], [ease], [ease-in], [ease-out], [ease-in-out] are
   spec-defined aliases for specific [<cubic-bezier>] / step-function values.
   Both Lightning CSS and cssnano canonicalize the cubic-bezier form to the
   named keyword when applicable. *)
let easing1_2_named_alias_canonicalization () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "cubic-bezier(0.25, 0.1, 0.25, 1) -> ease"
    (normalize ".x { transition: 0.3s ease }")
    (normalize ".x { transition: 0.3s cubic-bezier(0.25, 0.1, 0.25, 1) }");
  Alcotest.(check string)
    "steps(1, jump-start) -> step-start"
    (normalize ".x { transition: 0.3s step-start }")
    (normalize ".x { transition: 0.3s steps(1, jump-start) }")

let easing1_2_invalid_rejected () =
  Alcotest.(check bool)
    "cubic-bezier with two args is rejected" true
    (match
       Css.of_string ~strict:true
         ".x { transition: 0.3s cubic-bezier(0.5, 0.5) }"
     with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "steps with no args is rejected" true
    (match Css.of_string ~strict:true ".x { transition: 0.3s steps() }" with
    | Error _ -> true
    | _ -> false)

let fidelity_easing_preserved () =
  pretty_preserves ".x { transition: 0.3s ease }" [ "ease" ];
  pretty_preserves ".x { transition: 0.3s linear }" [ "linear" ];
  pretty_preserves ".x { transition: 0.3s cubic-bezier(0.25, 0.1, 0.25, 1) }"
    [ "cubic-bezier(0.25, 0.1, 0.25, 1)" ]

(* {2 Background shorthand (CSS Backgrounds L3 sec. 2.1)} *)

(* CSS Backgrounds and Borders L3, section 2.1 (background shorthand): the
   shorthand expands to several longhands. Default values ([background-position]
   = [0% 0%], [background-repeat] = [repeat], etc.) may be elided in the
   serialized form. Both minifiers drop default position [0% 0%] when no other
   components require it; per shortest- wins cascade picks the elided form. *)
let bg321_default_pos_elision () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "background: red 0% 0% -> background: red" ".x{background:red}"
    (normalize ".x { background: red 0% 0% }")

let bg321_multi_layer_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "multi-layer background preserves both layers" true
    (let out = normalize ".x { background: red, url(x.png) }" in
     Astring.String.is_infix ~affix:"red" out
     && Astring.String.is_infix ~affix:"url(x.png)" out)

let fidelity_background_preserved () =
  pretty_preserves ".x { background: red 0% 0% }" [ "0% 0%" ];
  pretty_preserves ".x { background: red 50% 50% / cover no-repeat }"
    [ "50% 50%"; "cover"; "no-repeat" ];
  pretty_preserves ".x { background: red, url(x.png) }" [ "red"; "url(x.png)" ]

(* {2 Strings and escapes (CSS Syntax L3 sec. 4.3.7)} *)

(* CSS Syntax L3, section 4.3.7 (Consume an escaped code point): hex escapes
   [\41] decode to the Unicode code point [U+0041 = 'A']. The trailing
   whitespace after a 1-6 digit escape is consumed. The decoded string is
   shorter than the escape; Lightning CSS decodes printable ASCII characters
   where safe. *)
let s3437_string_escape () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "\\41 in string decodes to A" ".x{content:\"A\"}"
    (normalize ".x { content: \"\\41\" }");
  Alcotest.(check string)
    "\\41 followed by space decodes to A" ".x{content:\"A\"}"
    (normalize ".x { content: \"\\41 \" }");
  Alcotest.(check string)
    "\\\\ preserved" ".x{content:\"\\\\\"}"
    (normalize ".x { content: \"\\\\\" }")

(* CSS Syntax 3 sec. 4.3.7 reads an escape as the code point it names, so a
   custom-property name can hold a [;] or a [}]. Serializing an ident writes
   those back escaped (CSS Syntax 3 sec. 2.1): printed raw, the name ends its
   own declaration or closes the rule around it, and cascade's reader stops
   reading what the input meant. *)
(* Print [css] and hold the printer to its own reader: the output parses in
   strict mode, says what the input said, and printing it again is a byte
   fixpoint. The tree comparison stands in for AST equality, which a token
   carrying its source position makes sensitive to how long the escape was.
   Returns the minified output, which is what a caller's spellings are checked
   against. *)
let escape_roundtrip css =
  match Css.of_string ~strict:false css with
  | Error e ->
      Alcotest.failf "parse failed: %s (%s)" css (Cascade.Error.to_string e)
  | Ok parsed ->
      let check ~minify label out =
        match Css.of_string ~strict:true out with
        | Error e ->
            Alcotest.failf "%s output is not readable: %s (%s)" label out
              (Cascade.Error.to_string e)
        | Ok again ->
            Alcotest.(check bool)
              (String.concat "" [ label; " says what "; css; " said" ])
              true
              (Cascade_diff.Css_compare.equal ~mode:`Tree css out);
            Alcotest.(check string)
              (String.concat "" [ label; " is a byte fixpoint: "; out ])
              out
              (Css.to_string ~minify again.Css.stylesheet)
      in
      let minified = Css.to_string ~minify:true parsed.Css.stylesheet in
      check ~minify:true "minified" minified;
      check ~minify:false "pretty" (Css.to_string parsed.Css.stylesheet);
      minified

let check_escape_roundtrips cases =
  List.iter
    (fun (css, expected) ->
      Alcotest.(check string) css expected (escape_roundtrip css))
    cases

let s4370_custom_property_name_escapes () =
  check_escape_roundtrips
    [
      (* The declaration name. *)
      (":root{--x\\3b y:red}", ":root{--x\\;y:red}");
      (":root{--x\\7d y:red}", ":root{--x\\}y:red}");
      (":root{--x\\7b y:red}", ":root{--x\\{y:red}");
      (":root{--x\\3a y:red}", ":root{--x\\:y:red}");
      (":root{--x\\ y:red}", ":root{--x\\ y:red}");
      (":root{--x\\22 y:red}", ":root{--x\\\"y:red}");
      (":root{--x\\\\y:red}", ":root{--x\\\\y:red}");
      (* The [var()] reference name, with each shape of fallback. *)
      (".a{color:var(--x\\7d y)}", ".a{color:var(--x\\}y)}");
      (".a{color:var(--a\\3b b,red)}", ".a{color:var(--a\\;b,red)}");
      ( ".a{color:var(--a\\3b b,var(--c\\3b d))}",
        ".a{color:var(--a\\;b,var(--c\\;d))}" );
      (".a{color:var(--a\\3b b,)}", ".a{color:var(--a\\;b,)}");
      (* The [@property] prelude and the [style()] container query. *)
      ( "@property --x\\3b y{syntax:\"*\";inherits:false}",
        "@property --x\\;y{syntax:\"*\";inherits:false}" );
      ( "@container style(--x\\3b y:red){.a{color:red}}",
        "@container style(--x\\;y:red){.a{color:red}}" );
      (* A name needing no escape keeps its spelling: [--] takes a digit
         straight after it, and a code point CSS Syntax 3 sec. 4.2 admits in an
         ident is written as itself. *)
      (":root{--x:red}.a{color:var(--x)}", ":root{--x:red}.a{color:var(--x)}");
      (":root{--0:red}.a{color:var(--0)}", ":root{--0:red}.a{color:var(--0)}");
      ( ":root{--\\e9 x:red}.a{color:var(--\\e9 x)}",
        ":root{--\xc3\xa9x:red}.a{color:var(--\xc3\xa9x)}" );
    ]

(* CSS Syntax 3 sec. 4.3.7 reads an escape as the code point it names, so any
   ident cascade stores can hold a [;], a [}] or another code point CSS Syntax 3
   sec. 4.2 keeps out of an ident. Serializing an ident writes those back
   escaped (CSS Syntax 3 sec. 2.1), and every prelude below names something with
   a [<custom-ident>] or a [<dashed-ident>]: printed raw the name ends the
   at-rule or closes the block around it. *)
let s4370_at_rule_prelude_name_escapes () =
  check_escape_roundtrips
    [
      (* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*],
         so each dot-separated part takes the escapes and the [.] separators
         stay bare. *)
      ("@layer a\\3b b;", "@layer a\\;b;");
      ("@layer a\\3b b{.x{color:red}}", "@layer a\\;b{.x{color:red}}");
      ("@layer a\\3b b,c\\3b d;", "@layer a\\;b,c\\;d;");
      ("@layer a.b\\3b c;", "@layer a.b\\;c;");
      ("@import\"a.css\"layer(l\\3b x);", "@import\"a.css\"layer(l\\;x);");
      (* CSS Counter Styles 3 sec. 2 [@counter-style <counter-style-name>], and
         sec. 3.1.4 [system: extends <counter-style-name>]. *)
      ( "@counter-style a\\3b b{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style a\\;b{system:cyclic;symbols:\"x\";suffix:\" \"}" );
      ( "@counter-style c{system:extends d\\3b y}",
        "@counter-style c{system:extends d\\;y}" );
      (* CSS Anchor Positioning 1 sec. 4.2 [@position-try <dashed-ident>] and
         CSS Fonts 4 sec. 11.1 [@font-palette-values <dashed-ident>]. *)
      ("@position-try --x\\3b y{top:0}", "@position-try --x\\;y{top:0}");
      ( "@font-palette-values --x\\3b y{font-family:Foo;base-palette:1}",
        "@font-palette-values --x\\;y{font-family:Foo;base-palette:1}" );
      (* CSS Containment 3 sec. 5.2: [@container] names a [<custom-ident>], and
         CSS Fonts 4 sec. 6.5 names a feature value the same way. *)
      ( "@container n\\3b m (width>10px){.a{color:red}}",
        "@container n\\;m (width>10px){.a{color:red}}" );
      ( "@font-feature-values Foo{@styleset{s\\3b x:1}}",
        "@font-feature-values Foo{@styleset{s\\;x:1}}" );
      (* A name needing no escape keeps its spelling, and the leading-digit rule
         of CSS Syntax 3 sec. 4.3.11 still applies. *)
      ("@layer a.b{.x{color:red}}", "@layer a.b{.x{color:red}}");
      ( "@counter-style \\31 a{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style \\31 a{system:cyclic;symbols:\"x\";suffix:\" \"}" );
      ( "@counter-style \\e9 x{system:cyclic;symbols:\"x\";suffix:\" \"}",
        "@counter-style \xc3\xa9x{system:cyclic;symbols:\"x\";suffix:\" \"}" );
    ]

(* The same rule for a name a declaration's value carries: printed raw it ends
   its own declaration, so it is written with the escapes CSS Syntax 3 sec.
   4.3.7 reads back as the same name. *)
let s4370_property_value_name_escapes () =
  check_escape_roundtrips
    [
      (* CSS Anchor Positioning 1 sec. 2.1 / 3.1 / 4.1 name
         [<dashed-ident>]s. *)
      (".a{anchor-name:--a\\3b b}", ".a{anchor-name:--a\\;b}");
      (".a{position-anchor:--p\\3b q}", ".a{position-anchor:--p\\;q}");
      ( ".a{position-try-fallbacks:--f\\3b g}",
        ".a{position-try-fallbacks:--f\\;g}" );
      (* CSS Scroll Animations 1 sec. 2.1 / 3.1 / 4 name [<dashed-ident>]s, and
         CSS Fonts 4 sec. 2.7 [font-palette] takes one. *)
      (".a{animation-timeline:--t\\3b u}", ".a{animation-timeline:--t\\;u}");
      (".a{scroll-timeline-name:--s\\3b t}", ".a{scroll-timeline-name:--s\\;t}");
      (".a{view-timeline-name:--v\\3b w}", ".a{view-timeline-name:--v\\;w}");
      ( ".a{scroll-timeline:--s\\3b t block}",
        ".a{scroll-timeline:--s\\;t block}" );
      (".a{timeline-scope:--z\\3b y}", ".a{timeline-scope:--z\\;y}");
      (".a{font-palette:--f\\3b g}", ".a{font-palette:--f\\;g}");
      (* CSS Transitions 1 sec. 2.1: [transition-property] names a property, and
         a custom property is a [<dashed-ident>]. *)
      (".a{transition-property:--t\\3b x}", ".a{transition-property:--t\\;x}");
      (* [<custom-ident>] names: CSS Containment 3 sec. 3.1, CSS Animations 1
         sec. 4.1, CSS View Transitions 1 sec. 3.1 and 2 sec. 3.2, and CSS Will
         Change 1 sec. 2. *)
      (".a{container-name:c\\3b d}", ".a{container-name:c\\;d}");
      (".a{animation-name:n\\3b m}", ".a{animation-name:n\\;m}");
      (".a{view-transition-name:v\\3b w}", ".a{view-transition-name:v\\;w}");
      (".a{view-transition-class:c\\3b d}", ".a{view-transition-class:c\\;d}");
      (".a{will-change:w\\3b x}", ".a{will-change:w\\;x}");
      (* CSS Lists 3 sec. 3 names a counter with a [<custom-ident>], both where
         it is set and where [counter()] reads it, and sec. 4 names a string the
         same way. *)
      (".a{counter-reset:c\\3b d 1}", ".a{counter-reset:c\\;d 1}");
      (".a{counter-increment:c\\3b d 1}", ".a{counter-increment:c\\;d 1}");
      (".a{content:counter(c\\3b d)}", ".a{content:counter(c\\;d)}");
      (".a{content:string(s\\3b t)}", ".a{content:string(s\\;t)}");
      (* CSS Grid 2 sec. 8.1: a grid line is named by a [<custom-ident>], in a
         placement property and in a track list. *)
      (".a{grid-area:g\\3b h}", ".a{grid-area:g\\;h}");
      (".a{grid-row-start:g\\3b h}", ".a{grid-row-start:g\\;h}");
      (".a{grid-row-start:span g\\3b h}", ".a{grid-row-start:span g\\;h}");
      ( ".a{grid-template-columns:[l\\3b m]1fr}",
        ".a{grid-template-columns:[l\\;m]1fr}" );
      (* A name needing no escape keeps its spelling. *)
      (".a{grid-area:g}", ".a{grid-area:g}");
      (".a{anchor-name:--a}", ".a{anchor-name:--a}");
    ]

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*], so a
   dot inside an ident and a dot between two idents name different layers.
   [@layer a\2e b] is one layer, [@layer a.b] is the sublayer [b] of [a], and
   the printed name says which: a dot an ident carries is written back escaped
   (CSS Syntax 3 sec. 2.1), the separators stay bare. *)
let s641_layer_name_parts () =
  check_escape_roundtrips
    [
      (* One ident holding a dot, against the two idents it would pass for. *)
      ("@layer a\\2e b;", "@layer a\\.b;");
      ("@layer a.b;", "@layer a.b;");
      ("@layer a\\2e b{.x{color:red}}", "@layer a\\.b{.x{color:red}}");
      ("@layer a.b{.x{color:red}}", "@layer a.b{.x{color:red}}");
      ("@layer a\\2e b,a.b;", "@layer a\\.b,a.b;");
      (* A sublayer of the layer named [a.b], and the layer named [a.b.c]. *)
      ("@layer a\\2e b.c;", "@layer a\\.b.c;");
      ("@layer a\\2e b\\2e c;", "@layer a\\.b\\.c;");
      (* The [@import] prelude names a layer the same way. *)
      ("@import\"a.css\"layer(a\\2e b);", "@import\"a.css\"layer(a\\.b);");
      ("@import\"a.css\"layer(a.b);", "@import\"a.css\"layer(a.b);");
      (* Each part still takes the escaping of every other code point. *)
      ("@layer a\\3b b.c\\3b d;", "@layer a\\;b.c\\;d;");
    ]

(* CSS Conditional 3 sec. 6: a [<supports-decl>] holds a declaration, and a
   custom property's name can carry a [;] or a [}] through an escape (CSS Syntax
   3 sec. 4.3.7). The condition is a capability predicate for that exact name,
   so it survives the round trip with the escapes that read it back. *)
let s4370_supports_property_name_escapes () =
  check_escape_roundtrips
    [
      (* The name a [<supports-decl>] tests, in each shape the feature takes: a
         declaration, an empty value, and an operand of [and]. *)
      ( "@supports (--x\\3b y:red){.a{color:red}}",
        "@supports(--x\\;y:red){.a{color:red}}" );
      ( "@supports (--x\\7d y:red){.a{color:red}}",
        "@supports(--x\\}y:red){.a{color:red}}" );
      ( "@supports (--x\\3b y:){.a{color:red}}",
        "@supports(--x\\;y:){.a{color:red}}" );
      ( "@supports (--x\\3b y:red) and (color:red){.a{color:red}}",
        "@supports(--x\\;y:red)and (color:red){.a{color:red}}" );
      ( "@supports not (--x\\3b y:red){.a{color:red}}",
        "@supports not (--x\\;y:red){.a{color:red}}" );
      (* A name needing no escape keeps its spelling. *)
      ( "@supports (--xy:red){.a{color:red}}",
        "@supports(--xy:red){.a{color:red}}" );
      ( "@supports (color:red){.a{color:red}}",
        "@supports(color:red){.a{color:red}}" );
    ]

let fidelity_string_escape_preserved () =
  pretty_preserves ".x { content: \"\\41\" }" [ "\\41" ];
  pretty_preserves ".x { content: \"hello\" }" [ "hello" ];
  pretty_preserves ".x { content: \"\\\\\" }" [ "\\\\" ]

(* {2 Color function spaces (CSS Color L4)} *)

(* CSS Color Module Level 4, sections 10-13 (Color spaces): static in-gamut
   colors may fold to the shortest sRGB spelling under minify. Forms Cascade
   cannot reduce without losing information, such as out-of-gamut colors or
   missing components, stay in functional notation. *)
let color4_10_color_space_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "static lab folds to shortest sRGB form" ".x{color:red}"
    (normalize ".x { color: lab(54.29 80.81 69.89) }");
  Alcotest.(check bool)
    "out-of-gamut oklch preserved" true
    (Astring.String.is_infix ~affix:"oklch"
       (normalize ".x { color: oklch(0.628 0.258 29.23) }"));
  Alcotest.(check bool)
    "none channel preserved" true
    (Astring.String.is_infix ~affix:"color(display-p3 none"
       (normalize ".x { color: color(display-p3 none 0.5 1) }"))

let color4_invalid_color_function_rejected () =
  Alcotest.(check bool)
    "oklch with no args is rejected" true
    (match Css.of_string ~strict:true ".x { color: oklch() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "color() with no space is rejected" true
    (match Css.of_string ~strict:true ".x { color: color(1 0 0) }" with
    | Error _ -> true
    | _ -> false)

let fidelity_color_space_preserved () =
  pretty_preserves ".x { color: oklch(0.628 0.258 29.23) }" [ ".258"; "29.23" ];
  pretty_preserves ".x { color: lab(54.29 80.81 69.89) }" [ "lab" ];
  pretty_preserves ".x { color: color(srgb 1 0 0) }" [ "color(srgb 1 0 0)" ];
  (* Pretty keeps oklab/oklch coefficients in full so the value round-trips,
     matching the [color()] function. *)
  pretty_preserves ".x { color: oklab(21% -0.00316127 -0.0338527 / 0.1) }"
    [ "-.00316127"; "-.0338527" ];
  (* The minified printer keeps them too: rounding a coefficient changes the
     colour, so it is a lossy fold that belongs in [optimize], not in [pp]. A
     consumer that serialises a typed colour with [to_string ~minify:true] (no
     optimize collapse) must get the authored value, matching Tailwind. *)
  let minify_pp css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> Css.to_string ~minify:true parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let oklab =
    minify_pp ".x { color: oklab(21% -0.00316127 -0.0338527 / 0.1) }"
  in
  Alcotest.(check bool)
    "minify pp keeps full oklab precision" true
    (Astring.String.is_infix ~affix:"-.00316127" oklab
    && Astring.String.is_infix ~affix:"-.0338527" oklab);
  let oklch = minify_pp ".x { color: oklch(0.628 0.2584567 29.23) }" in
  Alcotest.(check bool)
    "minify pp keeps full oklch chroma" true
    (Astring.String.is_infix ~affix:".2584567" oklch);
  (* The hue is a coefficient too: at high chroma a rounded hue shifts the
     colour, so the printer keeps it in full and [optimize] does the
     rounding. *)
  let hue = minify_pp ".x { color: oklch(0.628 0.2 264.123456) }" in
  Alcotest.(check bool)
    "minify pp keeps full oklch hue" true
    (Astring.String.is_infix ~affix:"264.123456" hue)

(* {2 @supports (CSS Conditional L4 sec. 2)} *)

(* CSS Conditional 3 sec. 6.1: a feature query asks the browser rendering the
   sheet whether it supports a declaration, so the answer belongs to that
   browser and the guard stays whatever shape the condition takes. Chrome
   answers false to both [(-webkit-hyphens: none)] and [(-moz-orient: inline)];
   a UA older than custom properties answers false to [(--x: y)], which sec. 6.1
   calls supported. *)
let conditional4_2_supports_guard_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet |> Css.optimize |> Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "a prefixed feature query keeps its guard"
    "@supports(-webkit-hyphens:none){.x{color:red}}"
    (normalize "@supports (-webkit-hyphens: none) { .x { color: red } }");
  Alcotest.(check string)
    "a negated prefixed feature query keeps its rule"
    "@supports not (-webkit-hyphens:none){.x{color:red}}"
    (normalize "@supports not (-webkit-hyphens: none) { .x { color: red } }");
  Alcotest.(check string)
    "a prefixed property with a prefixed value keeps its guard"
    "@supports(-webkit-appearance:-apple-pay-button){.x{color:red}}"
    (normalize
       "@supports (-webkit-appearance: -apple-pay-button) { .x { color: red } }");
  Alcotest.(check bool)
    "a prefixed conjunct survives a mixed condition" true
    (Astring.String.is_infix ~affix:"-webkit-hyphens"
       (normalize
          "@supports ((-webkit-hyphens: none) and (not (margin-trim: inline))) \
           { .x { color: red } }"));
  (* A conjunction is one condition, not two facts to settle separately.
     Dropping the conjunct a support table calls always-true rewrites the
     question the author asked the browser. *)
  Alcotest.(check string)
    "a mixed conjunction keeps both conjuncts"
    "@supports(display:grid)and (text-wrap:balance){.x{text-wrap:balance}}"
    (normalize
       "@supports (display: grid) and (text-wrap: balance) { .x { text-wrap: \
        balance } }");
  Alcotest.(check string)
    "a custom property guard is kept" "@supports(--x:y){.x{color:red}}"
    (normalize "@supports (--x: y) { .x { color: red } }");
  Alcotest.(check string)
    "an unprefixed widely-available guard is kept"
    "@supports(display:grid){.x{display:grid}}"
    (normalize "@supports (display: grid) { .x { display: grid } }")

(* CSS Conditional Rules Module Level 4, section 2 (The @supports rule): the
   rule body parses as a rule list (like a stylesheet) and round- trips
   preserved. The supports-condition grammar accepts declarations, [not], [and],
   [or], [selector()], and [font-tech()]. *)
let conditional4_2_supports_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet |> Css.optimize |> Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let grid = "@supports (display: grid) { .x { display: grid } }" in
  Alcotest.(check string)
    "a feature query round-trips" "@supports(display:grid){.x{display:grid}}"
    (normalize grid);
  let not_grid = "@supports not (display: grid) { .x { display: block } }" in
  Alcotest.(check string)
    "a negated feature query round-trips"
    "@supports not (display:grid){.x{display:block}}" (normalize not_grid);
  Alcotest.(check bool)
    "@supports selector(:has(img)) preserved" true
    (Astring.String.is_infix ~affix:"selector(:has(img))"
       (normalize "@supports selector(:has(img)) { .x { color: red } }"));
  Alcotest.(check bool)
    "a boolean condition keeps its operator" true
    (Astring.String.is_infix ~affix:"and"
       (normalize
          "@supports (display: grid) and (color: red) { .x { color: red } }"))

let conditional4_2_supports_invalid_rejected () =
  Alcotest.(check bool)
    "@supports with bare keyword rejected" true
    (match
       Css.of_string ~strict:true "@supports invalid { .x { color: red } }"
     with
    | Ok parsed ->
        (* Bad supports condition is dropped; rule body may or may not survive
           depending on recovery mode. Use the partial parser. *)
        let { Css.warnings; _ } =
          match
            Css.of_string ~strict:false
              "@supports invalid { .x { color: red } }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient @supports recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        List.length warnings >= 1
        || not
             (Astring.String.is_infix ~affix:"@supports"
                (minify parsed.stylesheet))
    | Error _ -> true)

let fidelity_supports_preserved () =
  pretty_preserves "@supports (display: grid) { .x { display: grid } }"
    [ "@supports"; "display: grid" ];
  pretty_preserves "@supports not (display: grid) { .x { display: block } }"
    [ "not" ]

(* {2 Logical Properties (CSS Logical L1)} *)

(* CSS Logical Properties and Values Module Level 1, section 3 (Flow- relative
   box model): logical properties [inline-size], [block-size],
   [margin-inline-start], [padding-block-end], [inset-inline], etc. round-trip
   preserved. The mapping to physical properties is computed-time (depends on
   writing-mode), so the syntax layer keeps the logical spelling. *)
let logical1_3_logical_property_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "inline-size preserved" ".x{inline-size:100px}"
    (normalize ".x { inline-size: 100px }");
  Alcotest.(check string)
    "block-size preserved" ".x{block-size:50px}"
    (normalize ".x { block-size: 50px }");
  Alcotest.(check string)
    "margin-inline-start preserved" ".x{margin-inline-start:10px}"
    (normalize ".x { margin-inline-start: 10px }");
  Alcotest.(check string)
    "margin-inline two-value shorthand preserved" ".x{margin-inline:10px 20px}"
    (normalize ".x { margin-inline: 10px 20px }");
  Alcotest.(check string)
    "padding-block two-value shorthand preserved" ".x{padding-block:5px 10px}"
    (normalize ".x { padding-block: 5px 10px }");
  Alcotest.(check string)
    "inset preserved" ".x{inset:0}"
    (normalize ".x { inset: 0 }");
  Alcotest.(check string)
    "inset-inline preserved" ".x{inset-inline:10px}"
    (normalize ".x { inset-inline: 10px }")

let fidelity_logical_property_preserved () =
  pretty_preserves ".x { inline-size: 100px }" [ "inline-size" ];
  pretty_preserves ".x { margin-inline-start: 10px }" [ "margin-inline-start" ];
  pretty_preserves ".x { padding-block: 5px 10px }" [ "padding-block" ]

(* {2 Container Queries (CSS Containment L3 sec. 4.4)} *)

(* CSS Containment Module Level 3, section 4.4 (Container Queries): the
   [@container] rule supports an optional name and a query expression. Both
   forms round-trip preserved, including the named form [@container card
   (...)]. *)
let containment3_6_container_query_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "@container with min-width preserved" true
    (Astring.String.is_infix ~affix:"@container"
       (normalize "@container (min-width: 400px) { .x { color: red } }"));
  Alcotest.(check bool)
    "@container with name preserved" true
    (Astring.String.is_infix ~affix:"card"
       (normalize "@container card (min-width: 400px) { .x { color: red } }"));
  Alcotest.(check bool)
    "@container with inline-size range preserved" true
    (Astring.String.is_infix ~affix:"inline-size"
       (normalize "@container card (inline-size > 30em) { .x { color: red } }"))

let fidelity_container_query_preserved () =
  pretty_preserves "@container (min-width: 400px) { .x { color: red } }"
    [ "@container"; "min-width: 400px" ];
  pretty_preserves "@container card (inline-size > 30em) { .x { color: red } }"
    [ "card"; "inline-size > 30em" ]

(* {2 Font shorthand (CSS Fonts L4 sec. 2.7)} *)

(* CSS Fonts Module Level 4, section 2.7 (Shorthand font property): the [font]
   shorthand expands to several longhands; the keyword [bold] inside the
   shorthand canonicalizes to [700] under minify per sec. 2.2. System font
   keywords ([caption], [icon], [menu]) round-trip preserved. *)
let fonts4_6_5_font_shorthand () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font: 16px Arial preserved" ".x{font:16px Arial}"
    (normalize ".x { font: 16px Arial }");
  Alcotest.(check bool)
    "font with bold canonicalizes to 700" true
    (Astring.String.is_infix ~affix:"700"
       (normalize ".x { font: italic small-caps bold 16px/1.2 sans-serif }"));
  Alcotest.(check string)
    "font: caption preserved" ".x{font:caption}"
    (normalize ".x { font: caption }");
  Alcotest.(check string)
    "font: icon preserved" ".x{font:icon}"
    (normalize ".x { font: icon }")

let fonts4_invalid_font_rejected () =
  Alcotest.(check bool)
    "font with no family rejected" true
    (match Css.of_string ~strict:true ".x { font: 16px }" with
    | Error _ -> true
    | _ -> false)

let fidelity_font_shorthand_preserved () =
  pretty_preserves ".x { font: 16px Arial }" [ "16px Arial" ];
  pretty_preserves ".x { font: italic small-caps bold 16px/1.2 sans-serif }"
    [ "italic"; "small-caps"; "bold" ];
  pretty_preserves ".x { font: caption }" [ "caption" ]

(* {2 List-style shorthand (CSS Lists L3 sec. 3.6)} *)

(* CSS Lists Module Level 3, section 3.6 (list-style shorthand): the shorthand
   sets [list-style-type], [list-style-position], and [list- style-image].
   Default values may be elided per shortest-wins. *)
let lists3_list_style_shorthand () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "list-style: none preserved" ".x{list-style:none}"
    (normalize ".x { list-style: none }");
  Alcotest.(check string)
    "list-style-type: none preserved" ".x{list-style-type:none}"
    (normalize ".x { list-style-type: none }");
  Alcotest.(check string)
    "list-style-position: inside preserved" ".x{list-style-position:inside}"
    (normalize ".x { list-style-position: inside }")

let fidelity_list_style_preserved () =
  pretty_preserves ".x { list-style: none }" [ "none" ];
  pretty_preserves ".x { list-style: disc inside }" [ "disc"; "inside" ];
  pretty_preserves ".x { list-style-type: none }" [ "list-style-type" ]

(* {2 Cascade origin + !important interaction (Cascade L5 sec. 6.3)} *)

(* CSS Cascading and Inheritance Module Level 5, section 6.3 (Importance): for
   important declarations the origin precedence is inverted - user-agent
   !important > user !important > author !important. The cascade test surface
   validates that important declarations preserve their authored !important
   suffix. *)
let c6_3_important_serialization_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "!important suffix preserved" ".x{color:red!important}"
    (normalize ".x { color: red !important }");
  Alcotest.(check string)
    "!important with whitespace canonicalized to no-space form"
    ".x{color:red!important}"
    (normalize ".x { color: red ! important }")

let fidelity_important_preserved () =
  pretty_preserves ".x { color: red !important }" [ "!important" ]

(* {2 Calc simplification edges (CSS Values L4 section 10)} *)

let v4_10_calc_negative_result () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(5px - 10px) -> -5px" ".x{margin:-5px}"
    (normalize ".x { margin: calc(5px - 10px) }");
  Alcotest.(check string)
    "calc(-1px + 2px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(-1px + 2px) }")

let v4_10_7_operator_precedence () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2 * 3px) -> 7px" ".x{width:7px}"
    (normalize ".x { width: calc(1px + 2 * 3px) }");
  Alcotest.(check string)
    "calc((1 + 2) * 3px) -> 9px" ".x{width:9px}"
    (normalize ".x { width: calc((1 + 2) * 3px) }")

let v4_10_7_identity_operations () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1 * 5px) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(1 * 5px) }");
  Alcotest.(check string)
    "calc(5px * 1) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(5px * 1) }");
  Alcotest.(check string)
    "calc(10px / 1) -> 10px" ".x{width:10px}"
    (normalize ".x { width: calc(10px / 1) }");
  Alcotest.(check string)
    "calc(0 * 100%) keeps its percentage type" ".x{width:0%}"
    (normalize ".x { width: calc(0 * 100%) }")

let v4_10_7_percentage_arithmetic () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(50% * 2) -> 100%" ".x{width:100%}"
    (normalize ".x { width: calc(50% * 2) }");
  Alcotest.(check string)
    "calc(50% / 2) -> 25%" ".x{width:25%}"
    (normalize ".x { width: calc(50% / 2) }");
  Alcotest.(check string)
    "calc(100% - 10% - 20%) -> 70%" ".x{width:70%}"
    (normalize ".x { width: calc(100% - 10% - 20%) }");
  Alcotest.(check string)
    "calc(100% - (10% + 20%)) -> 70%" ".x{width:70%}"
    (normalize ".x { width: calc(100% - (10% + 20%)) }")

let v4_10_7_chained_multiplicative () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px * 2 * 3) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(1px * 2 * 3) }");
  Alcotest.(check string)
    "calc(24px / 2 / 3) -> 4px" ".x{width:4px}"
    (normalize ".x { width: calc(24px / 2 / 3) }");
  Alcotest.(check string)
    "calc(1px*2 - 1px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px*2 - 1px) }")

let v4_10_7_double_negative () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px - -2px) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(1px - -2px) }")

let v4107_mixed_units () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2em) preserved" ".x{width:calc(1px + 2em)}"
    (normalize ".x { width: calc(1px + 2em) }");
  Alcotest.(check string)
    "calc(1px + 1ch) preserved" ".x{width:calc(1px + 1ch)}"
    (normalize ".x { width: calc(1px + 1ch) }");
  Alcotest.(check string)
    "calc(1rem + 2px) preserved" ".x{width:calc(1rem + 2px)}"
    (normalize ".x { width: calc(1rem + 2px) }")

let v4107_math_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(abs(-5px)) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(abs(-5px)) }");
  Alcotest.(check string)
    "calc(min(1px, 2px, 3px)) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(min(1px, 2px, 3px)) }");
  Alcotest.(check string)
    "calc(max(1px, 2px, 3px)) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(max(1px, 2px, 3px)) }")

(* CSS Values 4 sec. 10.7: [abs()] and [hypot()] return their argument's type,
   and sec. 10.9 gives the inverse trig functions an [<angle>]. A [calc()]
   wrapper around one reduced it to a bare coefficient, so [calc(hypot(1px,
   1px))] printed [1.41421356], which is not a [<length>] and is a declaration
   the reader drops. Each expected form is reparsed to prove it survives. *)
let v4107_typed_math_fn_units () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [reparses]") expected (normalize expected))
    [
      ( "calc(hypot(3px, 4px)) -> 5px",
        ".x { width: calc(hypot(3px, 4px)) }",
        ".x{width:5px}" );
      ( "calc(hypot(1px, 1px)) -> 1.41421px",
        ".x { width: calc(hypot(1px, 1px)) }",
        ".x{width:1.41421px}" );
      ( "calc(1px + hypot(3px, 4px)) -> 6px",
        ".x { width: calc(1px + hypot(3px, 4px)) }",
        ".x{width:6px}" );
      ( "calc(hypot(3em, 4em)) -> 5em",
        ".x { width: calc(hypot(3em, 4em)) }",
        ".x{width:5em}" );
      ( "calc(hypot(3%, 4%)) -> 5%",
        ".x { width: calc(hypot(3%, 4%)) }",
        ".x{width:5%}" );
      (* Both arguments are a [<length>], so the call is well typed, but [px]
         and [em] resolve against a font size only the browser knows. *)
      ( "calc(hypot(1px, 1em)) keeps the call",
        ".x { width: calc(hypot(1px, 1em)) }",
        ".x{width:calc(hypot(1px,1em))}" );
      ( "calc(abs(-3px)) -> 3px",
        ".x { width: calc(abs(-3px)) }",
        ".x{width:3px}" );
      (* A [<number>] argument keeps a [<number>] result, which scales a length
         rather than becoming one. *)
      ( "calc(hypot(3, 4) * 1px) -> 5px",
        ".x { width: calc(hypot(3, 4) * 1px) }",
        ".x{width:5px}" );
      ( "calc(hypot(3s, 4s)) -> 5s",
        ".x { transition-duration: calc(hypot(3s, 4s)) }",
        ".x{transition-duration:5s}" );
      ( "calc(abs(-3s)) -> 3s",
        ".x { transition-duration: calc(abs(-3s)) }",
        ".x{transition-duration:3s}" );
      ( "calc(hypot(3deg, 4deg)) -> 5deg",
        ".x { transform: rotate(calc(hypot(3deg, 4deg))) }",
        ".x{transform:rotate(5deg)}" );
      ( "calc(abs(-3deg)) -> 3deg",
        ".x { transform: rotate(calc(abs(-3deg))) }",
        ".x{transform:rotate(3deg)}" );
      ( "calc(atan2(1, 1)) -> 45deg",
        ".x { transform: rotate(calc(atan2(1, 1))) }",
        ".x{transform:rotate(45deg)}" );
      ( "calc(30deg + atan2(1, 1)) -> 75deg",
        ".x { transform: rotate(calc(30deg + atan2(1, 1))) }",
        ".x{transform:rotate(75deg)}" );
      (* [opacity] folds its [calc()] through the untyped numeric reduction,
         which has no unit to rebuild, so the call stays rather than becoming
         the [30] that a browser clamps to full opacity. *)
      ( "calc(abs(-30%)) on an opacity keeps the call",
        ".x { opacity: calc(abs(-30%)) }",
        ".x{opacity:calc(abs(-30%))}" );
    ]

let v4107_numeric_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(sqrt(16)) -> 4" ".x{aspect-ratio:4}"
    (normalize ".x { aspect-ratio: calc(sqrt(16)) }");
  Alcotest.(check string)
    "calc(pow(2, 3)) -> 8" ".x{aspect-ratio:8}"
    (normalize ".x { aspect-ratio: calc(pow(2, 3)) }");
  Alcotest.(check string)
    "calc(hypot(3, 4)) -> 5" ".x{aspect-ratio:5}"
    (normalize ".x { aspect-ratio: calc(hypot(3, 4)) }");
  Alcotest.(check string)
    "calc(sign(5)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(sign(5)) }")

let v4107_math_product_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input))
    [
      ( "calc(100px * hypot(3, 4)) -> 500px",
        ".x { width: calc(100px * hypot(3, 4)) }",
        ".x{width:500px}" );
      ( "calc(1px * pow(2, sqrt(100))) -> 1024px",
        ".x { width: calc(1px * pow(2, sqrt(100))) }",
        ".x{width:1024px}" );
      ( "calc(100px * pow(2, pow(2, 2))) -> 1600px",
        ".x { width: calc(100px * pow(2, pow(2, 2))) }",
        ".x{width:1600px}" );
      ( "calc(1px * log(1)) -> 0",
        ".x { width: calc(1px * log(1)) }",
        ".x{width:0}" );
      ( "calc(1px * log(10, 10)) -> 1px",
        ".x { width: calc(1px * log(10, 10)) }",
        ".x{width:1px}" );
      ( "calc(1px * exp(0)) -> 1px",
        ".x { width: calc(1px * exp(0)) }",
        ".x{width:1px}" );
      ( "calc(1px * log(e)) -> 1px",
        ".x { width: calc(1px * log(e)) }",
        ".x{width:1px}" );
      ( "calc(1px * (e - exp(1))) -> 0",
        ".x { width: calc(1px * (e - exp(1))) }",
        ".x{width:0}" );
      ( "calc(2px * pi) -> 6.28319px",
        ".x { width: calc(2px * pi) }",
        ".x{width:6.28319px}" );
      ( "calc(100px * pi) -> 314.159px",
        ".x { width: calc(100px * pi) }",
        ".x{width:314.159px}" );
      ( "calc(2px / pi) -> .63662px",
        ".x { width: calc(2px / pi) }",
        ".x{width:.63662px}" );
      ( "calc(100px * sin(45deg)) stays symbolic",
        ".x { width: calc(100px * sin(45deg)) }",
        ".x{width:calc(100px*sin(45deg))}" );
      ( "calc(100px * sin(.125turn)) stays symbolic",
        ".x { width: calc(100px * sin(.125turn)) }",
        ".x{width:calc(100px*sin(.125turn))}" );
      ( "calc(100px * sin(pi / 4)) stays symbolic",
        ".x { width: calc(100px * sin(pi / 4)) }",
        ".x{width:calc(100px*sin(pi/4))}" );
      ( "calc(100px * sin(22deg + 23deg)) stays symbolic",
        ".x { width: calc(100px * sin(22deg + 23deg)) }",
        ".x{width:calc(100px*sin(45deg))}" );
      ( "calc(2px * cos(45deg)) stays symbolic",
        ".x { width: calc(2px * cos(45deg)) }",
        ".x{width:calc(2px*cos(45deg))}" );
      ( "calc(2px * tan(45deg)) -> 2px",
        ".x { width: calc(2px * tan(45deg)) }",
        ".x{width:2px}" );
    ]

(* CSS Values 4 sec. 10.13 leaves numeric precision implementation-defined, so
   the budget is Cascade's own: six significant figures for a value Cascade
   computes, and every digit the author wrote for one it did not. The two are
   different values - at a [14px] font [.4285714em] is [6px] while [.428571em]
   is [5.999994px] - so the reduction happens at the fold that produces the
   irrational, never at print time. *)
let authored_precision_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  (* An [@property --n] registration, and the minified form it prints as, so the
     [--n] declaration under it is read as a typed [<number>]. *)
  let registered decl =
    String.concat ""
      [
        "@property --n { syntax: \"<number>\"; inherits: false; initial-value: \
         0 }";
        decl;
      ]
  in
  let registered_min decl =
    String.concat ""
      [
        "@property --n{syntax:\"<number>\";inherits:false;initial-value:0}";
        decl;
      ]
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [idempotent]") expected (normalize expected))
    [
      (* Authored dimensions past six significant figures. *)
      ( "authored em keeps its digits",
        ".x { padding-left: .4285714em }",
        ".x{padding-left:.4285714em}" );
      ( "authored px keeps its magnitude",
        ".x { width: 999999999px }",
        ".x{width:999999999px}" );
      ( "authored time keeps its digits",
        ".x { transition-duration: 1.2345678s }",
        ".x{transition-duration:1.2345678s}" );
      ( "authored angle keeps its digits",
        ".x { transform: rotate(1.2345678deg) }",
        ".x{transform:rotate(1.2345678deg)}" );
      ( "min() keeps the authored operand it selects",
        ".x { width: min(1.41421356px, 2px) }",
        ".x{width:1.41421356px}" );
      (* A folded irrational carries the digits Cascade commits to. *)
      ( "calc(2px * pi) rounds at the fold",
        ".x { width: calc(2px * pi) }",
        ".x{width:6.28319px}" );
      ( "calc(2px / pi) rounds at the fold",
        ".x { width: calc(2px / pi) }",
        ".x{width:.63662px}" );
      ( "calc(1px * sqrt(2)) rounds at the fold",
        ".x { width: calc(1px * sqrt(2)) }",
        ".x{width:1.41421px}" );
      ( "hypot() on lengths rounds at the fold",
        ".x { width: hypot(1px, 1px) }",
        ".x{width:1.41421px}" );
      (* [<number>] is untouched in both directions. *)
      ( "authored line-height keeps its digits",
        ".x { line-height: 1.4285714 }",
        ".x{line-height:1.4285714}" );
      ( "authored opacity keeps its digits",
        ".x { opacity: .12345678 }",
        ".x{opacity:.12345678}" );
      ( "authored unregistered custom property keeps its digits",
        ".x { --raw: 1.4285714 }",
        ".x{--raw:1.4285714}" );
      (* CSS Properties and Values API 1 sec. 2: an [@property] registration
         lifts a [--name] use into the typed [<number>] shape, which must not
         change the author's digits either. *)
      ( "registered <number> keeps its digits",
        registered ".x { --n: 1.4285714 }",
        registered_min ".x{--n:1.4285714}" );
      ( "registered <number> keeps its magnitude",
        registered ".x { --n: 999999999999 }",
        registered_min ".x{--n:999999999999}" );
      ( "registered <number> shortens the fold the printer runs",
        registered ".x { --n: calc(1 / 3) }",
        registered_min ".x{--n:.333333}" );
      (* A [calc()] the optimizer already collapsed reaches the printer as a
         bare coefficient, where the registered shape has to print what an
         untyped [<number>] prints. *)
      ( "registered <number> prints a collapsed fold like an untyped one",
        registered ".x { --n: calc(2 * pi) }",
        registered_min ".x{--n:6.28318531}" );
      ( "untyped <number> prints the same collapsed fold",
        ".x { line-height: calc(2 * pi) }",
        ".x{line-height:6.28318531}" );
    ]

(* The six-significant-figure budget buys a fractional tail, and past 10^6 it
   stops reaching one: the only digits left to spend are integer ones the
   arithmetic got right. [1in] is exactly [96px] and [1pt] exactly [4/3px] (CSS
   Values 4 sec. 6.2), so [calc(1in + 999999999px)] is [1000000095px] and
   rounding it to [1000000000px] moves the box by 95px. A value small enough for
   the budget to reach its fraction still pays it: [1cm] is [4800/127px], whose
   tail is Cascade's own noise. *)
let computed_precision_keeps_integer_digits () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [idempotent]") expected (normalize expected))
    [
      (* An absolute-unit combine past 10^6 keeps every digit. *)
      ( "in + px keeps the whole magnitude",
        ".x { width: calc(1in + 999999999px) }",
        ".x{width:1000000095px}" );
      ( "in + px keeps a mid-range magnitude",
        ".x { width: calc(1in + 12345678px) }",
        ".x{width:12345774px}" );
      ( "pc + px keeps the whole magnitude",
        ".x { width: calc(1pc + 1000000px) }",
        ".x{width:1000016px}" );
      ( "pt + px keeps the third of a pixel",
        ".x { width: calc(1pt + 1000000px) }",
        ".x{width:1000001.33333333px}" );
      ( "hypot() past 10^6 keeps the whole magnitude",
        ".x { width: hypot(999999999px, 1px) }",
        ".x{width:999999999px}" );
      ( "a folded irrational past 10^6 keeps its integer digits",
        ".x { width: calc(1000000px * pi) }",
        ".x{width:3141592.65358979px}" );
      (* Under 10^6 the budget still spends the tail. *)
      ( "cm + px spends the tail",
        ".x { width: calc(1cm + 1px) }",
        ".x{width:38.7953px}" );
      ( "mm + px spends the tail",
        ".x { width: calc(1mm + 1px) }",
        ".x{width:4.77953px}" );
      ( "q + px spends the tail",
        ".x { width: calc(1q + 1px) }",
        ".x{width:1.94488px}" );
      ( "a non-Pythagorean hypot() spends the tail",
        ".x { width: hypot(1px, 2px) }",
        ".x{width:2.23607px}" );
      ( "a Pythagorean hypot() has no tail to spend",
        ".x { width: hypot(3px, 4px) }",
        ".x{width:5px}" );
    ]

let v4107_mod_rem () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(mod(7, 3)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(mod(7, 3)) }");
  Alcotest.(check string)
    "calc(rem(7, 3)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(rem(7, 3)) }")

let v4_10_7_round_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(round(5.5px, 1px)) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(round(5.5px, 1px)) }")

let v4107_division_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px / 0) preserved (the [/] operator prints without surrounding \
     whitespace per shortest-wins)"
    ".x{width:calc(1px/0)}"
    (normalize ".x { width: calc(1px / 0) }")

let v410_invalid_calc_extra () =
  Alcotest.(check bool)
    "calc(1px ++ 2px) double operator rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px ++ 2px) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(1px 2px) missing operator rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px 2px) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(1px + + 2px) two plus rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px + + 2px) }" with
    | Error _ -> true
    | _ -> false)

let fidelity_calc_negative_preserved () =
  pretty_preserves ".x { margin: calc(5px - 10px) }" [ "calc(5px - 10px)" ];
  pretty_preserves ".x { width: calc(1px - -2px) }" [ "calc(1px - -2px)" ]

let fidelity_calc_precedence_preserved () =
  pretty_preserves ".x { width: calc(1px + 2 * 3px) }" [ "1px + 2 * 3px" ];
  pretty_preserves ".x { width: calc((1 + 2) * 3px) }" [ "(1 + 2) * 3px" ]

let fidelity_calc_chained_preserved () =
  pretty_preserves ".x { width: calc(1px * 2 * 3) }" [ "1px * 2 * 3" ];
  pretty_preserves ".x { width: calc(100% - 10% - 20%) }" [ "100% - 10% - 20%" ]

let fidelity_calc_mixed_relative_preserved () =
  pretty_preserves ".x { width: calc(1px + 2em) }" [ "calc(1px + 2em)" ];
  pretty_preserves ".x { width: calc(1px + 1ch) }" [ "calc(1px + 1ch)" ];
  pretty_preserves ".x { width: calc(1rem + 2px) }" [ "calc(1rem + 2px)" ]

let fidelity_calc_math_functions_preserved () =
  pretty_preserves ".x { width: calc(abs(-5px)) }" [ "abs(-5px)" ];
  pretty_preserves ".x { width: calc(min(1px, 2px, 3px)) }"
    [ "min(1px, 2px, 3px)" ];
  (* sqrt() and pow() return unitless numbers, so they need a [<number>]-typed
     property like [aspect-ratio]. *)
  pretty_preserves ".x { aspect-ratio: calc(sqrt(16)) }" [ "sqrt(16)" ];
  pretty_preserves ".x { aspect-ratio: calc(pow(2, 3)) }" [ "pow(2, 3)" ]

let fidelity_calc_math_product_preserved () =
  let remove_spaces s =
    String.to_seq s
    |> Seq.filter (function ' ' | '\n' | '\r' | '\t' -> false | _ -> true)
    |> String.of_seq
  in
  let pretty_preserves_form css fragment =
    match Css.of_string ~strict:false css with
    | Error _ -> Alcotest.failf "failed to parse: %s" css
    | Ok parsed ->
        let printed = Css.to_string parsed.stylesheet in
        Alcotest.(check bool)
          (Fmt.str "non-minified output preserves math form [%s] in [%s]"
             fragment printed)
          true
          (Astring.String.is_infix ~affix:(remove_spaces fragment)
             (remove_spaces printed))
  in
  List.iter
    (fun (css, fragment) -> pretty_preserves_form css fragment)
    [
      (".x { width: calc(100px * hypot(3, 4)) }", "calc(100px * hypot(3, 4))");
      ( ".x { width: calc(1px * pow(2, sqrt(100))) }",
        "calc(1px * pow(2, sqrt(100)))" );
      ( ".x { width: calc(100px * pow(2, pow(2, 2))) }",
        "calc(100px * pow(2, pow(2, 2)))" );
      (".x { width: calc(1px * log(1)) }", "calc(1px * log(1))");
      (".x { width: calc(1px * log(10, 10)) }", "calc(1px * log(10, 10))");
      (".x { width: calc(1px * exp(0)) }", "calc(1px * exp(0))");
      (".x { width: calc(1px * log(e)) }", "calc(1px * log(e))");
      (".x { width: calc(1px * (e - exp(1))) }", "calc(1px * (e - exp(1)))");
      (".x { width: calc(2px * pi) }", "calc(2px * pi)");
      (".x { width: calc(2px / pi) }", "calc(2px / pi)");
      (".x { width: calc(100px * sin(45deg)) }", "calc(100px * sin(45deg))");
      ( ".x { width: calc(100px * sin(.125turn)) }",
        "calc(100px * sin(.125turn))" );
      (".x { width: calc(100px * sin(pi / 4)) }", "calc(100px * sin(pi / 4))");
      ( ".x { width: calc(100px * sin(22deg + 23deg)) }",
        "calc(100px * sin(22deg + 23deg))" );
      (".x { width: calc(2px * cos(45deg)) }", "calc(2px * cos(45deg))");
      (".x { width: calc(2px * tan(45deg)) }", "calc(2px * tan(45deg))");
    ]

(* {2 var() fallback edges (CSS Custom Properties L1)} *)

let customprops12_empty_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x,) (empty fallback) preserved" ".x{color:var(--x,)}"
    (normalize ".x { color: var(--x,) }");
  Alcotest.(check bool)
    "var(--x, ) (whitespace fallback) parses" true
    (match Css.of_string ~strict:false ".x { color: var(--x, ) }" with
    | Ok _ -> true
    | _ -> false);
  Alcotest.(check string)
    "var(--x) (no fallback) preserved" ".x{color:var(--x)}"
    (normalize ".x { color: var(--x) }")

let customprops12_color_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, rgb(255, 0, 0)) -> var(--x, red)" ".x{color:var(--x,red)}"
    (normalize ".x { color: var(--x, rgb(255, 0, 0)) }");
  Alcotest.(check string)
    "var(--x, rgb(0, 0, 0)) -> var(--x, #000)" ".x{color:var(--x,#000)}"
    (normalize ".x { color: var(--x, rgb(0, 0, 0)) }");
  Alcotest.(check string)
    "var(--x, transparent) -> var(--x, #0000)" ".x{color:var(--x,#0000)}"
    (normalize ".x { color: var(--x, transparent) }")

let customprops12_nested_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, var(--y, red)) preserved" ".x{color:var(--x,var(--y,red))}"
    (normalize ".x { color: var(--x, var(--y, red)) }");
  Alcotest.(check string)
    "three-level var chain preserved" ".x{color:var(--x,var(--y,var(--z)))}"
    (normalize ".x { color: var(--x, var(--y, var(--z))) }")

let customprops12_calc_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, calc(1px + 2px)) holds the calc (typed boundary)"
    ".x{width:var(--x,calc(1px + 2px))}"
    (normalize ".x { width: var(--x, calc(1px + 2px)) }")

let customprops12_multi_comma () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "var(--x, red, blue) keeps both fallback tokens" true
    (let out = normalize ".x { color: var(--x, red, blue) }" in
     Astring.String.is_infix ~affix:"red" out
     && Astring.String.is_infix ~affix:"blue" out);
  Alcotest.(check bool)
    "font-family fallback list preserved" true
    (let out =
       normalize
         ".x { font-family: var(--font, \"Helvetica Neue\", sans-serif) }"
     in
     Astring.String.is_infix ~affix:"Helvetica Neue" out
     && Astring.String.is_infix ~affix:"sans-serif" out)

let customprops12_whitespace () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var( --x , red ) drops surrounding whitespace" ".x{color:var(--x,red)}"
    (normalize ".x { color: var( --x , red ) }")

let customprops12_case_sensitive () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--My-Var preserved with original case" ".x{color:var(--My-Var,red)}"
    (normalize ".x { color: var(--My-Var, red) }")

let customprops13_declaration () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--x: 10px preserved" ".x{--x:10px}"
    (normalize ".x { --x: 10px }");
  Alcotest.(check string)
    "--x: red blue preserved" ".x{--x:red blue}"
    (normalize ".x { --x: red blue }");
  Alcotest.(check string)
    "--x: before block close preserves whitespace-token value" ".x{--x: }"
    (normalize ".x { --x: }");
  Alcotest.(check string)
    "--x: ; preserves the whitespace-token value" ".x{--x: }"
    (normalize ".x { --x: ; }")

let customprops13_color_keyword_case_fold () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--c: RED folds to the canonical lower-case colour keyword" ".x{--c:red}"
    (normalize ".x { --c: RED }");
  Alcotest.(check string)
    "--c: ToMaTo folds to the canonical named colour" ".x{--c:tomato}"
    (normalize ".x { --c: ToMaTo }");
  Alcotest.(check string)
    "named colour folds inside a larger token stream"
    ".x{--c:1px solid rebeccapurple}"
    (normalize ".x { --c: 1px solid rebeccaPurple }");
  Alcotest.(check string)
    "--c: AUTO folds like the other colour-position keywords" ".x{--c:auto}"
    (normalize ".x { --c: AUTO }");
  Alcotest.(check string)
    "unrecognised idents keep their source case" ".x{--c:MyToken}"
    (normalize ".x { --c: MyToken }");
  Alcotest.(check string)
    "system colour keywords keep their canonical mixed case"
    ".x{--c:ButtonText}"
    (normalize ".x { --c: ButtonText }")

let normalize_minified css =
  match Css.of_string ~strict:false css with
  | Ok parsed -> minify parsed.stylesheet |> String.trim
  | Error _ -> Alcotest.failf "failed to parse: %s" css

(* CSS Custom Properties L1 section 2 preserves unregistered custom-property
   declarations as opaque token streams. CSS Properties and Values API 1 section
   2 lifts a custom property into a typed value only after an @property
   registration for that name. *)
(* CSS Scroll Snap 1 section 5.2: [scroll-snap-align] takes one or two
   keywords; the reader must stop at the end of the value, not consume the
   declaration's trailing semicolon as a missing second keyword. *)
let scroll_snap_align_trailing_semicolon () =
  Alcotest.(check string)
    "scroll-snap-align value followed by a semicolon parses"
    ".x{scroll-snap-align:none}"
    (normalize_minified ".x { scroll-snap-align: none; }");
  Alcotest.(check string)
    "scroll-snap-align pair followed by a semicolon parses"
    ".x{scroll-snap-align:start end}"
    (normalize_minified ".x { scroll-snap-align: start end; }")

let customprops13_unregistered_font_stack () =
  Alcotest.(check string)
    "unregistered font-stack custom property keeps string tokens"
    ".x{--font-sans:ui-sans-serif,system-ui,sans-serif,\"Segoe UI \
     Symbol\",\"Noto Color Emoji\"}"
    (normalize_minified
       ".x { --font-sans: ui-sans-serif, system-ui, sans-serif, \"Segoe UI \
        Symbol\", \"Noto Color Emoji\" }")

let customprops13_unregistered_theme_tokens () =
  Alcotest.(check string)
    "unregistered theme custom properties keep font strings and calc tokens"
    "@layer theme{:host,:root{--font-mono:ui-monospace,SFMono-Regular,\"Roboto \
     Mono\",\"Courier \
     New\",monospace;--font-sans:ui-sans-serif,system-ui,sans-serif,\"Segoe UI \
     Symbol\",\"Noto Color \
     Emoji\";--text-lg--line-height:calc(1.75/1.125);--text-sm--line-height:calc(1.25/.875)}}"
    (normalize_minified
       "@layer theme { :host, :root { --font-sans: ui-sans-serif, system-ui, \
        sans-serif, \"Segoe UI Symbol\", \"Noto Color Emoji\"; --font-mono: \
        ui-monospace, SFMono-Regular, \"Roboto Mono\", \"Courier New\", \
        monospace; --text-sm--line-height: calc(1.25 / .875); \
        --text-lg--line-height: calc(1.75 / 1.125) } }")

let customprops13_registered_numeric_calc () =
  Alcotest.(check string)
    "unregistered numeric custom property keeps token stream"
    ".x{--text-sm--line-height:calc(1.25/.875)}"
    (normalize_minified ".x { --text-sm--line-height: calc(1.25 / .875) }");
  Alcotest.(check string)
    "registered numeric custom property reduces to typed number"
    "@property \
     --text-sm--line-height{syntax:\"<number>\";inherits:true;initial-value:1}.x{--text-sm--line-height:1.42857}"
    (normalize_minified
       "@property --text-sm--line-height { syntax: \"<number>\"; inherits: \
        true; initial-value: 1 } .x { --text-sm--line-height: calc(1.25 / \
        .875) }")

let customprops13_registered_oklch_chroma () =
  Alcotest.(check string)
    "unregistered OKLCH custom property folds the colour function"
    ".x{--color-zinc-500:#71717b}"
    (normalize_minified ".x { --color-zinc-500: oklch(55.2% .016 285.938) }");
  Alcotest.(check string)
    "registered OKLCH custom property uses typed color minification"
    "@property \
     --color-zinc-500{syntax:\"<color>\";inherits:true;initial-value:#000}.x{--color-zinc-500:#71717b}"
    (normalize_minified
       "@property --color-zinc-500 { syntax: \"<color>\"; inherits: true; \
        initial-value: black } .x { --color-zinc-500: oklch(55.2% .016 \
        285.938) }");
  (* A [@property] registration is document-global regardless of source order
     (CSS Properties and Values API 1 SS 2), so a usage inside [@layer] that
     precedes its [@property] rule (the Tailwind output shape) is still promoted
     and colour-canonicalised. *)
  Alcotest.(check string)
    "registration applies to a prior usage nested in a layer"
    "@layer utilities{.x{--color-zinc-500:#71717b}}@property \
     --color-zinc-500{syntax:\"<color>\";inherits:true;initial-value:#000}"
    (normalize_minified
       "@layer utilities { .x { --color-zinc-500: oklch(55.2% .016 285.938) } \
        } @property --color-zinc-500 { syntax: \"<color>\"; inherits: true; \
        initial-value: black }")

let customprops13_registered_percent_calc () =
  Alcotest.(check string)
    "unregistered percent calc custom property keeps token stream"
    ".x{--tw-translate-x:calc(1/2*100%)}"
    (normalize_minified ".x { --tw-translate-x: calc(1 / 2 * 100%) }");
  Alcotest.(check string)
    "registered percent custom property uses typed calc minification"
    "@property \
     --tw-translate-x{syntax:\"<percentage>\";inherits:false;initial-value:0%}.x{--tw-translate-x:50%}"
    (normalize_minified
       "@property --tw-translate-x { syntax: \"<percentage>\"; inherits: \
        false; initial-value: 0% } .x { --tw-translate-x: calc(1 / 2 * 100%) }")

let customprops13_registered_negative_dimension_calc () =
  Alcotest.(check string)
    "unregistered dimension calc custom property keeps token stream"
    ".x{--tw-tracking:calc(.05em*-1)}"
    (normalize_minified ".x { --tw-tracking: calc(.05em * -1) }");
  Alcotest.(check string)
    "registered length custom property reduces to dimension"
    "@property \
     --tw-tracking{syntax:\"<length>\";inherits:false;initial-value:0px}.x{--tw-tracking:-.05em}"
    (normalize_minified
       "@property --tw-tracking { syntax: \"<length>\"; inherits: false; \
        initial-value: 0px } .x { --tw-tracking: calc(.05em * -1) }")

let customprops13_registered_angle_time_calc () =
  (* <angle>/<time> custom properties fold their calc() whether or not they are
     registered: the unit unambiguously fixes the type, so a complete math
     function reduces to its dimension in every var() site (like a complete
     colour). The registered path uses the typed kind; the unregistered path
     folds the opaque substream. *)
  Alcotest.(check string)
    "unregistered angle calc custom property folds to an angle"
    ".x{--tw-rotate:0deg}"
    (normalize_minified ".x { --tw-rotate: calc(1deg * 0) }");
  Alcotest.(check string)
    "unregistered time calc custom property folds to a duration"
    ".x{--tw-delay:2s}"
    (normalize_minified ".x { --tw-delay: calc(1s * 2) }");
  Alcotest.(check string)
    "registered angle custom property reduces to an angle"
    "@property \
     --tw-rotate{syntax:\"<angle>\";inherits:false;initial-value:0deg}.x{--tw-rotate:0deg}"
    (normalize_minified
       "@property --tw-rotate { syntax: \"<angle>\"; inherits: false; \
        initial-value: 0deg } .x { --tw-rotate: calc(1deg * 0) }");
  Alcotest.(check string)
    "registered time custom property reduces to a duration"
    "@property \
     --tw-delay{syntax:\"<time>\";inherits:false;initial-value:0s}.x{--tw-delay:2s}"
    (normalize_minified
       "@property --tw-delay { syntax: \"<time>\"; inherits: false; \
        initial-value: 0s } .x { --tw-delay: calc(1s * 2) }");
  (* A percentage is ambiguous (length vs number percentage) so an unregistered
     percentage calc stays an opaque token stream. *)
  Alcotest.(check string)
    "unregistered percentage calc custom property stays opaque"
    ".x{--a:calc(50%*2)}"
    (normalize_minified ".x { --a: calc(50% * 2) }")

let customprops13_trigonometric_calc () =
  (* CSS Values 4 sec. 10.4 puts the trigonometric family among the math
     functions, so a complete one reduces in a custom-property stream the way
     the exponential family already does. The inverse functions resolve to an
     <angle>, which is unambiguous in every var() site. *)
  Alcotest.(check string)
    "unregistered arc tangent custom property folds to an angle" ".x{--v:45deg}"
    (normalize_minified ".x { --v: atan2(1, 1) }");
  Alcotest.(check string)
    "unregistered arc cosine custom property folds to an angle" ".x{--v:90deg}"
    (normalize_minified ".x { --v: acos(0) }");
  Alcotest.(check string)
    "unregistered arc sine custom property folds to an angle" ".x{--v:90deg}"
    (normalize_minified ".x { --v: asin(1) }")

let customprops13_shortest_unresolved_calc_spacing () =
  Alcotest.(check string)
    "unregistered unresolved calc custom property keeps token stream"
    ".x{--tw-border-spacing-x:calc(var(--spacing)*4)}"
    (normalize_minified ".x { --tw-border-spacing-x: calc(var(--spacing) * 4) }")

let customprops13_box_shadow_zero_spread () =
  Alcotest.(check string)
    "unregistered box-shadow custom property keeps token stream"
    ".shadow-sm{--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,#0000001a)}"
    (normalize_minified
       ".shadow-sm { --tw-shadow: 0 1px 3px 0 var(--tw-shadow-color, \
        #0000001a) }")

let customprops13_hex_color_folds () =
  Alcotest.(check string)
    "unregistered hex custom property folds to the shortest hex" ".x{--c:#fff}"
    (normalize_minified ".x { --c: #ffffff }");
  (* An opaque alpha hex with no shorter form stays verbatim. *)
  Alcotest.(check string)
    "8-digit hex without a shorter form is unchanged" ".x{--c:#abcdef12}"
    (normalize_minified ".x { --c: #abcdef12 }");
  (* A bare colour keyword is also a valid <custom-ident> in a non-colour
     substitution site, so the fold must never produce a name: red folds to the
     hex form, not to [red], even though [red] is one byte shorter. *)
  Alcotest.(check string)
    "hex folds to hex, never to a named colour" ".x{--c:#f00}"
    (normalize_minified ".x { --c: #ff0000 }");
  Alcotest.(check string)
    "an rgb() function also folds to hex, not a name" ".x{--c:#f00}"
    (normalize_minified ".x { --c: rgb(255 0 0) }");
  (* A hex inside an <image> function (itself a clearly typed substream) folds
     through the recursion. *)
  Alcotest.(check string)
    "hex nested in a gradient folds" ".x{--g:linear-gradient(#f00,#00f)}"
    (normalize_minified ".x { --g: linear-gradient(#ff0000, #0000ff) }")

let customprops13_shortest_oklab_sign_boundaries () =
  Alcotest.(check string)
    "unregistered OKLab custom property folds the colour function"
    ".prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       ".prose { --tw-prose-kbd-shadows: oklab(21% -.003 -.034 / .1) }");
  Alcotest.(check string)
    "unregistered OKLab custom property folds regardless of source sign spacing"
    ".prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       ".prose { --tw-prose-kbd-shadows: oklab(21% -.003-.034 / .1) }");
  Alcotest.(check string)
    "registered OKLab custom property uses typed color minification"
    "@property \
     --tw-prose-kbd-shadows{syntax:\"<color>\";inherits:true;initial-value:#000}.prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       "@property --tw-prose-kbd-shadows { syntax: \"<color>\"; inherits: \
        true; initial-value: black } .prose { --tw-prose-kbd-shadows: \
        oklab(21% -.003 -.034 / .1) }")

let customprops12_invalid_var () =
  Alcotest.(check bool)
    "var() with no arguments is rejected" true
    (match Css.of_string ~strict:true ".x { color: var() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "var(red) (non-dashed) is rejected" true
    (match Css.of_string ~strict:true ".x { color: var(red) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "var(-x, red) (single-dash prefix) is rejected" true
    (match Css.of_string ~strict:true ".x { color: var(-x, red) }" with
    | Error _ -> true
    | _ -> false)

let customprops12_shorthand_calc () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() in shorthand value preserved" ".x{border:var(--border)}"
    (normalize ".x { border: var(--border) }");
  Alcotest.(check string)
    "var() inside calc() preserved" ".x{width:calc(var(--x) + 10px)}"
    (normalize ".x { width: calc(var(--x) + 10px) }");
  Alcotest.(check string)
    "multiple var() in one value preserved" ".x{padding:var(--top)var(--right)}"
    (normalize ".x { padding: var(--top) var(--right) }")

let fidelity_var_fallback_preserved () =
  pretty_preserves ".x { color: var(--x,) }" [ "var(--x,)" ];
  pretty_preserves ".x { color: var(--x, red) }" [ "var(--x, red)" ];
  pretty_preserves ".x { color: var(--x, var(--y, red)) }"
    [ "var(--x, var(--y, red))" ]

let fidelity_calc_fallback () =
  pretty_preserves ".x { width: var(--x, calc(1px + 2px)) }"
    [ "var(--x, calc(1px + 2px))" ]

let fidelity_multi_comma () =
  pretty_preserves ".x { color: var(--x, red, blue) }" [ "var(--x, red, blue)" ];
  pretty_preserves
    ".x { font-family: var(--font, \"Helvetica Neue\", sans-serif) }"
    [ "Helvetica Neue"; "sans-serif" ]

let fidelity_var_name_case_preserved () =
  pretty_preserves ".x { color: var(--My-Var, red) }" [ "--My-Var" ];
  pretty_preserves ".x { color: var(--snake_case_var) }" [ "--snake_case_var" ]

let fidelity_custom_property_decl_preserved () =
  pretty_preserves ":root { --brand: red }" [ "--brand" ];
  pretty_preserves ".x { --x: 10px }" [ "--x" ];
  pretty_preserves ".x { --x: red blue }" [ "red blue" ]

(* {2 Variable inlining via theme + theme_defaults} *)

(* CSS Custom Properties L1: cascade exposes a print-time inlining API via an
   explicit caller-provided context. The [theme] set is a protection set:
   variables listed there stay as [var(--name)]. For variables outside that set,
   only a concrete [theme_defaults] result is positive evidence for inlining;
   [None] means "unknown", not "undefined". *)
let custom_props1_theme_inlining () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_theme = Css.Pp.String_set.empty in
  let resolve_brand = function "brand" -> Some "red" | _ -> None in
  let resolve_gap = function "gap" -> Some "16px" | _ -> None in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--brand) inlines to red when not in theme" ".x{color:red}"
    (render_theme ~theme:no_theme ~theme_defaults:resolve_brand
       (parse ".x { color: var(--brand) }"));
  Alcotest.(check string)
    "var(--gap) inlines to 16px when not in theme" ".x{margin:16px}"
    (render_theme ~theme:no_theme ~theme_defaults:resolve_gap
       (parse ".x { margin: var(--gap) }"));
  Alcotest.(check bool)
    "var() in theme keeps the var() reference" true
    (let theme = Css.Pp.String_set.add "brand" no_theme in
     let out =
       render_theme ~theme ~theme_defaults:resolve_brand
         (parse ".x { color: var(--brand) }")
     in
     Astring.String.is_infix ~affix:"var(--brand)" out)

(* CSS Custom Properties L1 section 2: a fallback is used at computed-value time
   when the referenced custom property is invalid or missing. The print-time
   renderer should not infer that from [theme_defaults] returning [None]; it has
   only learned that the caller did not provide a concrete replacement. *)
let customprops1_unresolved_fallback () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_resolve _ = (None : string option) in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--undef, red) is preserved when resolver has no answer"
    ".x{color:var(--undef,red)}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
       (parse ".x { color: var(--undef, red) }"));
  Alcotest.(check string)
    "nested var() chain is preserved when resolver has no answers"
    ".x{color:var(--a,var(--b,blue))}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
       (parse ".x { color: var(--a, var(--b, blue)) }"));
  Alcotest.(check string)
    "optimize+minify preserves unresolved nested var fallback"
    ".x{color:var(--a,var(--b,#00f))}"
    (normalize_minified ".x { color: var(--a, var(--b, blue)) }")

(* CSS Custom Properties L1: a theme var reachable only through another var()'s
   *typed* fallback ([transition-timing-function: var(--tw-ease, var(--theme))])
   still resolves transitively. Theme resolution is seeded by a structural value
   scan, so the nested var is found even though the typed AST spells that
   fallback as a value rather than a [Var_fallback]; the outer runtime var stays
   live. *)
let customprops1_transitive_fallback () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "default-transition-timing-function" -> Some "ease"
    | _ -> None
  in
  let theme = Css.Pp.String_set.of_list [ "tw-ease" ] in
  Alcotest.(check string)
    "nested theme var in a typed fallback inlines, runtime var stays live"
    ".t{transition-timing-function:var(--tw-ease,ease)}"
    (parse
       ".t { transition-timing-function: var(--tw-ease, \
        var(--default-transition-timing-function)) }"
    |> Css.resolve_theme ~theme ~theme_defaults:resolve
    |> Css.to_string ~minify:true)

(* CSS Properties & Values API: resolve_theme is a partial inline - it
   substitutes resolved theme vars but must leave unrelated [@property]
   registrations untouched. Inlining one theme token previously dropped every
   [@property] in the stylesheet. *)
let customprops1_resolve_keeps_property () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "inlining a theme var keeps an unrelated @property registration"
    "@property --tw-foo{syntax:\"*\";inherits:false}.x{width:24px}"
    (parse
       "@property --tw-foo{syntax:\"*\";inherits:false}.x{width:var(--blur-xl)}"
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:(function
         | "blur-xl" -> Some "24px"
         | _ -> None)
    |> Css.to_string ~minify:true)

(* CSS Custom Properties L1: a theme token already declared in the input on a
   root-scope block ([:root,:host], as Tailwind emits) and resolvable through
   [theme_defaults] inlines from that declaration and is dropped - the existing
   binding must not survive, be duplicated, or block the substitution. The
   declared value wins over the theme default (cascade). *)
let customprops1_inline_declared_root_token () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let blur = function "blur-xl" -> Some "24px" | _ -> None in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:blur
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "a declared :root,:host theme token inlines and its block is dropped"
    ".x{filter:blur(24px)}"
    (render ":root,:host{--blur-xl:24px}.x{filter:blur(var(--blur-xl))}");
  Alcotest.(check string)
    "the declared value wins over the theme default" ".x{filter:blur(9px)}"
    (render ":root{--blur-xl:9px}.x{filter:blur(var(--blur-xl))}")

(* CSS Custom Properties L1 section 2: inlining a [var()] inside a [calc()]
   resolves the variable, and the resulting all-constant calc reduces under
   minify. *)
let customprops1_calc_inline () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function "gap" -> Some "5px" | _ -> None in
  Alcotest.(check string)
    "calc(var(--gap) + 10px) inlines and reduces to 15px" ".x{width:15px}"
    (parse ".x { width: calc(var(--gap) + 10px) }"
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true)

(* CSS Custom Properties L1: a theme var referenced anywhere - inside another
   var's opaque value or inside a utility rule - and resolvable through
   [theme_defaults] is emitted at root scope, the global token's cascade scope.
   It merges into an existing :root,:host block (no spurious extra block), is
   never injected into the element-scoped rule that references it, and an
   unrelated @supports polyfill default is not pulled in. *)
let customprops1_transitive_merge () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.of_list [ "shadow" ] in
  let spacing = function "spacing" -> Some ".25rem" | _ -> None in
  let render ?(theme_defaults = spacing) css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "transitive theme default is folded into the kept variable"
    ":root,:host{--shadow:0 0 .25rem black}"
    (render ":root,:host{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "a theme default referenced from a utility is folded into it"
    ".shadow{--shadow:0 0 .25rem black}"
    (render ".shadow{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "an unrelated @supports polyfill default is not pulled in"
    "@supports(color:lab(0 0 0)){:root{--tw-x:initial}}:root,:host{--shadow:0 \
     0 .25rem black}"
    (render
       "@supports (color: lab(0 0 0)){:root{--tw-x:initial}} \
        :root,:host{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "an unresolved reference is kept live, not materialised"
    ":root,:host{--shadow:0 0 var(--spacing) black}"
    (render
       ~theme_defaults:(fun _ -> None)
       ":root,:host{--shadow:0 0 var(--spacing) black}")

(* CSS Custom Properties L1 sec. 2.1: a custom property's value is a
   [<declaration-value>], and CSS Syntax 3 sec. 8.2 bars an unmatched [}] / [)],
   a top-level [;], a [<bad-string-token>] and an unterminated function from
   one. [theme_defaults] is caller-supplied CSS text, so an answer that breaks
   those rules is not a value the name resolves to: the reference stays live,
   the sibling default still binds, and no rule, at-rule or second declaration
   reaches the output. *)
let theme_defaults_reject_escaping_value () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let src = ".a{color:var(--brand);background-color:var(--ok)}" in
  let defaults brand = function
    | "brand" -> Some brand
    | "ok" -> Some "blue"
    | _ -> None
  in
  let emit brand =
    parse src
    |> Css.resolve_theme ~theme_defaults:(defaults brand)
    |> Css.to_string ~minify:true
  in
  let inline brand =
    parse src
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:(defaults brand)
    |> Css.to_string ~minify:true
  in
  let bound =
    ":root{--ok:blue}.a{color:var(--brand);background-color:var(--ok)}"
  in
  let inlined = ".a{color:var(--brand);background-color:blue}" in
  List.iter
    (fun (what, brand) ->
      Alcotest.(check string)
        (what ^ " binds nothing and leaves the sibling default")
        bound (emit brand);
      Alcotest.(check string)
        (what ^ " stays a live reference while the rest inlines")
        inlined (inline brand))
    [
      ("a value closing the block", "red}");
      ("a value starting a sibling rule", "red}.evil{color:red");
      ("a value starting a rule after a [;]", "red};.evil{color:red");
      ("a value starting an at-rule", "red}@media print{.x{color:red}");
      ("a value carrying a second declaration", "red;--evil:lime");
      ("a value with an unmatched [)]", "red)");
      ("an unterminated function", "rgb(1,2,3");
      ("an unterminated string", "\"abc");
      ("an empty value", "");
    ];
  (* A comment is consumed by tokenization (CSS Syntax 3 sec. 4.3.2), so
     ["red/*"] is the one-token value ["red"] and does bind. *)
  Alcotest.(check string)
    "an unterminated comment leaves a value that binds"
    ":root{--ok:blue;--brand:red}.a{color:var(--brand);background-color:var(--ok)}"
    (emit "red/*")

(* CSS Syntax 3 sec. 4.3.7: a [\X] escape carries any code point into an ident,
   so [var(--x\3b y)] references the theme name ["x;y"]. The default binds under
   that name, written back with the escapes that read it - the one spelling that
   makes the declaration the name it references rather than some other
   declaration ([y:red]) at root scope. *)
let theme_defaults_escaping_name () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (what, src, name, bound, inlined) ->
      let resolve ?theme () =
        parse src
        |> Css.resolve_theme ?theme ~theme_defaults:(fun n ->
            if n = name then Some "red" else None)
        |> Css.to_string ~minify:true
      in
      Alcotest.(check string)
        (what ^ ": binds under the name the reference spells")
        bound (resolve ());
      Alcotest.(check string)
        (what ^ ": resolves the reference it binds")
        inlined
        (resolve ~theme:Css.Pp.String_set.empty ());
      Alcotest.(check string)
        (what ^ ": the binding reads back as itself")
        bound
        (Css.to_string ~minify:true (parse bound)))
    [
      ( "a name carrying a [;]",
        ".a{color:var(--x\\3b y)}",
        "x;y",
        ":root{--x\\;y:red}.a{color:var(--x\\;y)}",
        ".a{color:red}" );
      ( "a name carrying a [}]",
        ".a{color:var(--x\\7d y)}",
        "x}y",
        ":root{--x\\}y:red}.a{color:var(--x\\}y)}",
        ".a{color:red}" );
    ]

(* CSS Custom Properties L1 section 2: a [var()] used inside a fallback list
   ([var(--font, "Helvetica", sans-serif)]) inlines if the variable resolves,
   otherwise the fallback list is kept intact. *)
let customprops1_fallback_list () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve_font = function "font" -> Some "Arial" | _ -> None in
  let no_resolve _ = (None : string option) in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--font, fallback) inlines to Arial when resolved"
    ".x{font-family:Arial}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve_font
       (parse ".x { font-family: var(--font, sans-serif) }"));
  Alcotest.(check bool)
    "var() with multi-comma fallback list keeps fallback when unresolved" true
    (let out =
       render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
         (parse ".x { font-family: var(--font, \"Helvetica\", sans-serif) }")
     in
     Astring.String.is_infix ~affix:"Helvetica" out
     && Astring.String.is_infix ~affix:"sans-serif" out)

(* CSS Custom Properties L1 section 2: a custom-property declaration may itself
   contain [var()] references. Inlining a chain [--accent: var(--brand)]
   requires resolving the inner variable first. *)
let customprops1_value_position () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "brand" -> Some "red"
    | "gap" -> Some "16px"
    | _ -> None
  in
  Alcotest.(check bool)
    "multiple var() in one value all inline" true
    (let out =
       parse ".x { padding: var(--gap) var(--gap) }"
       |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
            ~theme_defaults:resolve
       |> Css.to_string ~minify:true
     in
     Astring.String.is_infix ~affix:"16px 16px" out
     || Astring.String.is_infix ~affix:"16px" out)

(* CSS Custom Properties L1 section 2: explicit theme resolution is a transform
   phase, not merely a print-time spelling hook. If a resolver replacement is
   another [var()] reference, the transform should keep resolving until it
   reaches a concrete value, then normal minified serialization applies to that
   concrete value. *)
let theme_chain_resolution () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "accent" -> Some "var(--brand)"
    | "brand" -> Some "color(srgb 1 0 0)"
    | "gap" -> Some "var(--space)"
    | "space" -> Some "calc(1px + 2px)"
    | "nested-gap" -> Some "var(--nested-space)"
    | "nested-space" -> Some "calc(1px + var(--foo))"
    | "foo" -> Some "2px"
    | "runtime-gap" -> Some "var(--runtime-space)"
    | "runtime-space" -> Some "calc(1px + var(--runtime-foo))"
    | "fallback-gap" -> Some "var(--maybe, var(--space))"
    | "deep-fallback" -> Some "var(--unknown, calc(1px + var(--foo)))"
    | "two-axis" -> Some "var(--x) var(--y)"
    | "x" -> Some "1px"
    | "y" -> Some "calc(1px + 1px)"
    | "shadow-color" -> Some "var(--brand)"
    | _ -> None
  in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  let render_optimized css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.optimize |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "var replacement chains resolve to an authored color function"
    ".x{color:color(srgb 1 0 0)}"
    (render ".x { color: var(--accent) }");
  Alcotest.(check string)
    "resolved color function optimizes to the shortest color" ".x{color:red}"
    (render_optimized ".x { color: var(--accent) }");
  Alcotest.(check string)
    "var replacement chains resolve to calc, then reduce" ".x{width:3px}"
    (render ".x { width: var(--gap) }");
  Alcotest.(check string)
    "nested var inside replacement resolves before calc reduction"
    ".x{width:3px}"
    (render ".x { width: var(--nested-gap) }");
  Alcotest.(check string)
    "unresolved nested var inside replacement stays runtime-dynamic"
    ".x{width:calc(1px + var(--runtime-foo))}"
    (render ".x { width: var(--runtime-gap) }");
  Alcotest.(check string)
    "fallback arm inside replacement resolves when primary stays unknown"
    ".x{width:3px}"
    (render ".x { width: var(--fallback-gap) }");
  Alcotest.(check string)
    "var inside calculated fallback arm resolves, then calc reduces"
    ".x{width:3px}"
    (render ".x { width: var(--deep-fallback) }");
  Alcotest.(check string)
    "replacement with multiple vars resolves every slot" ".x{margin:1px 2px}"
    (render ".x { margin: var(--two-axis) }");
  Alcotest.(check string)
    "nested color replacement resolves through another replacement"
    ".x{text-shadow:0 0 2px color(srgb 1 0 0)}"
    (render ".x { text-shadow: 0 0 2px var(--shadow-color) }");
  Alcotest.(check string)
    "nested color replacement optimizes after resolution"
    ".x{text-shadow:0 0 2px red}"
    (render_optimized ".x { text-shadow: 0 0 2px var(--shadow-color) }")

(* CSS Custom Properties L1 section 3: a real per-element dependency cycle makes
   the variables in that cycle invalid at computed-value time. A caller-provided
   theme resolver is not that full computed-value graph, so a cycle in resolver
   output should stop resolution and preserve the authored runtime expression at
   the boundary instead of looping or guessing that the fallback is safe to
   select statically. *)
let theme_cycle_preserved () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "a" -> Some "var(--b)"
    | "b" -> Some "var(--a)"
    | "outer" -> Some "calc(1px + var(--loop-a))"
    | "loop-a" -> Some "var(--loop-b)"
    | "loop-b" -> Some "var(--loop-a)"
    | "fallback-loop" -> Some "var(--missing, var(--loop-a))"
    | "deep-fallback-loop" -> Some "var(--missing, calc(1px + var(--loop-a)))"
    | _ -> None
  in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "cyclic var replacement preserves the authored fallback boundary"
    ".x{color:var(--a,red)}"
    (render ".x { color: var(--a, red) }");
  Alcotest.(check string)
    "cycle nested inside calc preserves the authored outer expression"
    ".x{width:var(--outer)}"
    (render ".x { width: var(--outer) }");
  Alcotest.(check string)
    "cycle reached through fallback preserves the authored outer expression"
    ".x{color:var(--fallback-loop)}"
    (render ".x { color: var(--fallback-loop) }");
  Alcotest.(check string)
    "cycle inside calculated fallback preserves the authored outer expression"
    ".x{width:var(--deep-fallback-loop)}"
    (render ".x { width: var(--deep-fallback-loop) }")

(* CSS Custom Properties L1 section 2: when the variable's resolved value is a
   color, the color canonicalization rules (hex shorten, named conversion) apply
   to the inlined value as well. *)
let custom_props1_inlined_color_canonicalizes () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "brand" -> Some "#ff0000"
    | "bg" -> Some "rgb(0, 0, 0)"
    | _ -> None
  in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  let render_optimized ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.optimize |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--brand)=#ff0000 canonicalizes to shortest color after inlining"
    ".x{color:#f00}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { color: var(--brand) }"));
  Alcotest.(check string)
    "var(--bg)=rgb(0,0,0) preserves authored function after inlining"
    ".x{background-color:rgb(0 0 0)}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { background-color: var(--bg) }"));
  Alcotest.(check string)
    "var(--bg)=rgb(0,0,0) optimizes to #000 after inlining"
    ".x{background-color:#000}"
    (render_optimized ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { background-color: var(--bg) }"))

(* {2 Fidelity tests for theme inlining} *)

(* Without explicit theme/theme_defaults configuration the printer preserves the
   var() reference unchanged. *)
let fidelity_no_inlining_without_context () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() without theme keeps the reference under non-minified"
    ".x {\n  color: var(--brand);\n}"
    (Css.to_string (parse ".x { color: var(--brand) }"));
  Alcotest.(check bool)
    "var() with fallback under non-minified preserves both" true
    (let out = Css.to_string (parse ".x { color: var(--brand, red) }") in
     Astring.String.is_infix ~affix:"var(--brand" out
     && Astring.String.is_infix ~affix:"red" out)

(* CSS Custom Properties L1 section 2 (Variable Substitution): a [var()]
   fallback is part of the source value until the caller proves a concrete
   replacement. An empty [theme] with [theme_defaults] returning [None] should
   preserve the [var()] wrapper and fallback syntax. The fallback's own tokens
   may still be canonicalized locally. *)
let custom_props1_fallback_resolution_mode () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_resolve _ = (None : string option) in
  let inlined css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:no_resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "var(--undef, red) remains a runtime fallback" ".x{color:var(--undef,red)}"
    (inlined ".x { color: var(--undef, red) }");
  Alcotest.(check string)
    "var(--undef, #ff0000) fallback canonicalizes inside var()"
    ".x{color:var(--undef,#f00)}"
    (inlined ".x { color: var(--undef, #ff0000) }");
  Alcotest.(check string)
    "calc fallback is held inside var() (typed boundary)"
    ".x{width:var(--undef,calc(1px + 2px))}"
    (inlined ".x { width: var(--undef, calc(1px + 2px)) }");
  Alcotest.(check string)
    "nested var() chain remains runtime fallback"
    ".x{color:var(--a,var(--b,var(--c,blue)))}"
    (inlined ".x { color: var(--a, var(--b, var(--c, blue))) }");
  Alcotest.(check string)
    "empty fallback preserved as empty value (cascade-time invalid)"
    ".x{color:var(--undef,)}"
    (inlined ".x { color: var(--undef,) }")

(* CSS Custom Properties L1 section 2: when the variable IS in the theme set,
   the [var()] reference is preserved even when a fallback is available - the
   theme protects the variable from inlining. *)
let custom_props1_theme_protects_var () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.add "brand" Css.Pp.String_set.empty in
  let no_resolve _ = (None : string option) in
  let inlined css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults:no_resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check bool)
    "var(--brand, red) keeps reference when --brand is in theme" true
    (let out = inlined ".x { color: var(--brand, red) }" in
     Astring.String.is_infix ~affix:"var(--brand" out)

(* UX contract for print-time theme substitution: a supplied [theme] set should
   not make every non-theme variable look statically undefined. The set only
   protects names that must stay dynamic; inlining still requires a concrete
   resolver answer. *)
let theme_set_not_undefined () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.empty |> Css.Pp.String_set.add "brand" in
  let no_resolve _ = (None : string option) in
  let resolve_accent = function "accent" -> Some "#ff0000" | _ -> None in
  let render ?(resolve = no_resolve) css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "non-theme var without resolver answer is preserved"
    ".x{color:var(--accent)}"
    (render ".x { color: var(--accent) }");
  Alcotest.(check string)
    "non-theme var fallback without resolver answer is preserved"
    ".x{color:var(--accent,red)}"
    (render ".x { color: var(--accent, red) }");
  Alcotest.(check string)
    "non-theme var with resolver answer inlines" ".x{color:#f00}"
    (render ~resolve:resolve_accent ".x { color: var(--accent) }");
  Alcotest.(check string)
    "non-theme var fallback with resolver answer inlines" ".x{color:#f00}"
    (render ~resolve:resolve_accent ".x { color: var(--accent, blue) }")

let typed_var_default_fidelity () =
  let dx, x =
    Css.var
      ~default:(Css.Zero : Css.length)
      "tw-translate-x" Css.Length (Css.Px 123.)
  in
  let dy, y =
    Css.var
      ~default:(Css.Zero : Css.length)
      "tw-translate-y" Css.Length (Css.Px 456.)
  in
  let stylesheet =
    [
      Css.rule ~selector:(Selector.class_ "x")
        [
          dx;
          dy;
          Css.translate
            (Css.XY ((Css.Var x : Css.length), (Css.Var y : Css.length)));
        ];
    ]
  in
  Alcotest.(check string)
    "typed defaults do not inline under normal stylesheet rendering"
    ".x{--tw-translate-x:123px;--tw-translate-y:456px;translate:var(--tw-translate-x)var(--tw-translate-y)}"
    (minify stylesheet)

let cssom67_no_trailing_semicolon () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let with_trailing = normalize ".a { color: red; padding: 1px; }" in
  let without_trailing = normalize ".a { color: red; padding: 1px }" in
  Alcotest.(check string)
    "trailing semicolon does not affect canonical serialization" with_trailing
    without_trailing;
  Alcotest.(check bool)
    "minified output has no trailing semicolon before }" false
    (Astring.String.is_infix ~affix:";}" with_trailing)

(* An [@container] prelude that does not parse is faulted against the slice of
   the query that failed, so the caret lands inside the query. Every span below
   is counted off the source text: the leading rule is 18 bytes plus a newline,
   so line 2 opens at offset 19 and [@container ] runs to offset 29. *)
let container_condition_error_spans () =
  let check name query (reason, start_pos, end_pos) =
    let input =
      ".ok { color: red }\n@container " ^ query ^ " { .a { color: blue } }\n"
    in
    match Css.of_string ~strict:false input with
    | Error err ->
        Alcotest.failf "%s: lenient parse failed: %s" name
          (Cascade.Error.to_string err)
    | Ok { Css.stylesheet; warnings; _ } -> (
        Alcotest.(check int)
          (name ^ ": sibling rule survives")
          1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] ->
            (match e.Error.kind with
            | Error.Bad_condition { at_rule; reason = got } ->
                Alcotest.(check string)
                  (name ^ ": at-rule") "@container" at_rule;
                Alcotest.(check string) (name ^ ": reason") reason got
            | _ ->
                Alcotest.failf "%s: expected Bad_condition, got %s" name
                  (Error.to_string e));
            Alcotest.(check (pair int int))
              (name ^ ": span") (start_pos, end_pos)
              (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
        | ws ->
            Alcotest.failf "%s: expected one warning, got %d" name
              (List.length ws))
  in
  (* [style()] spans offsets 30-36; an empty argument list has no components of
     its own, so the call itself carries the span. *)
  check "empty style()" "style()" ("empty style() container query", 30, 37);
  (* [scroll-state(] ends at offset 42, so [bogus] spans 43-47. *)
  check "bad scroll-state()" "scroll-state(bogus)"
    ("invalid scroll-state() container query", 43, 48);
  (* [style(] ends at offset 35, so [1px] spans 36-38. *)
  check "bad style() name" "style(1px: red)"
    ("invalid style() container query", 36, 39)

(* One warning off a lenient parse, checked against the at-rule it names, the
   reason it gives and the span it points at. *)
let one_condition_warning name input ~at_rule (reason, start_pos, end_pos) =
  match Css.of_string ~strict:false input with
  | Error err ->
      Alcotest.failf "%s: lenient parse failed: %s" name
        (Cascade.Error.to_string err)
  | Ok { Css.stylesheet; warnings; _ } -> (
      Alcotest.(check int)
        (name ^ ": sibling rule survives")
        1
        (List.length (Css.rule_statements stylesheet));
      match warnings with
      | [ e ] ->
          (match e.Error.kind with
          | Error.Bad_condition { at_rule = got_rule; reason = got } ->
              Alcotest.(check string) (name ^ ": at-rule") at_rule got_rule;
              Alcotest.(check string) (name ^ ": reason") reason got
          | _ ->
              Alcotest.failf "%s: expected Bad_condition, got %s" name
                (Error.to_string e));
          Alcotest.(check (pair int int))
            (name ^ ": span") (start_pos, end_pos)
            (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
      | ws ->
          Alcotest.failf "%s: expected one warning, got %d" name
            (List.length ws))

(* A media query that does not parse is faulted against the slice of the query
   that failed. Spans are counted off the source text: the leading rule is 18
   bytes plus a newline, so line 2 opens at offset 19 and [@media ] runs to
   offset 25. *)
let media_condition_error_spans () =
  let check name query expected =
    let input =
      ".ok { color: red }\n@media " ^ query ^ " { .a { color: blue } }\n"
    in
    one_condition_warning name input ~at_rule:"@media" expected
  in
  (* A lone [not] prefixes nothing; the query itself spans offsets 26-28. *)
  check "prefix with no type" "not" ("expected media type or condition", 26, 29);
  (* The [or] that follows an [and] spans offsets 46-47. *)
  check "mixed operators" "(color) and (hover) or (a)"
    ("mixed 'and'/'or' media condition", 46, 48);
  check_stylesheet ~expected:"@media(bogus !!!){.a{color:blue}}"
    "@media (bogus !!!) { .a { color: blue } }";
  check_import_rule ~expected:"@import\"a.css\"(bogus !!!);"
    "@import url(\"a.css\") (bogus !!!);"

(* An @font-face descriptor whose value does not parse is faulted against the
   value, not against the enclosing block. The descriptor is dropped and the
   rest of the at-rule is kept, so the [src] below keeps the @font-face valid
   and the warning list holds only the descriptor failure. Spans are counted off
   the source text: the leading rule is 18 bytes plus a newline, so line 2 opens
   at offset 19. *)
let descriptor_value_error_spans () =
  let check name descriptor (reason, start_pos, end_pos) =
    let input =
      ".ok { color: red }\n@font-face { font-family: x; src: url(a.woff2); "
      ^ descriptor ^ " }\n"
    in
    match Css.of_string ~strict:false input with
    | Error err ->
        Alcotest.failf "%s: lenient parse failed: %s" name
          (Cascade.Error.to_string err)
    | Ok { Css.stylesheet; warnings; _ } -> (
        Alcotest.(check int)
          (name ^ ": sibling rule survives")
          1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] ->
            (match e.Error.kind with
            | Error.Bad_value { reason = got; _ } ->
                Alcotest.(check string) (name ^ ": reason") reason got
            | _ ->
                Alcotest.failf "%s: expected Bad_value, got %s" name
                  (Error.to_string e));
            Alcotest.(check (pair int int))
              (name ^ ": span") (start_pos, end_pos)
              (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
        | ws ->
            Alcotest.failf "%s: expected one warning, got %d" name
              (List.length ws))
  in
  (* [ascent-override] opens at offset 67, so its value spans 84-86. *)
  check "negative metric override" "ascent-override: -5%"
    ("invalid: metric override", 84, 87);
  (* [size-adjust] opens at offset 67, so its value spans 80-84. *)
  check "size-adjust that is not a percentage" "size-adjust: bogus"
    ("invalid: size-adjust", 80, 85)

(* An @supports condition that does not parse is faulted against the slice of
   the condition that failed, where the reader used to hand back a bare reason
   that the stylesheet re-anchored on the whole prelude. Spans are counted off
   the source text: the leading rule is 18 bytes plus a newline, so line 2 opens
   at offset 19 and [@supports ] runs to offset 28. *)
let supports_condition_error_spans () =
  let check name condition expected =
    let input =
      ".ok { color: red }\n@supports " ^ condition ^ " { .a { color: blue } }\n"
    in
    one_condition_warning name input ~at_rule:"@supports" expected
  in
  (* [extra-junk] follows a complete feature query and spans offsets 45-54. *)
  check "trailing content" "(display: grid) extra-junk"
    ("trailing content", 45, 55);
  (* The [or] that follows an [and] spans offsets 45-46. *)
  check "mixed operators" "(a:b) and (c:d) or (e:f)"
    ("Cannot mix and/or without parentheses in @supports", 45, 47);
  check_stylesheet ~expected:"@supports(){.a{color:blue}}"
    "@supports () { .a { color: blue } }";
  check_stylesheet ~expected:"@supports font-format(bogus){.a{color:blue}}"
    "@supports font-format(bogus) { .a { color: blue } }"

let additional_tests =
  [
    ("check function", `Quick, test_check);
    ("import_rule", `Quick, test_import_rule);
    ("layer_name", `Quick, test_layer_name);
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
    ( "spec CSS Syntax 5.4.2 unknown at-rule block body",
      `Quick,
      s542_unknown_at_rule_block_body );
    ( "spec CSS Syntax 5.5.2 unknown at-rule raw text closes at EOF",
      `Quick,
      s552_unknown_at_rule_eof_closers );
    ( "spec CSS Syntax 4.3.1 unknown at-rule prelude separator",
      `Quick,
      s3431_unknown_at_rule_prelude_separator );
    ( "spec CSS Syntax 4.3.1 unknown at-rule trailing backslash",
      `Quick,
      s3431_unknown_at_rule_trailing_backslash );
    ( "spec CSS Syntax 5.5.2 unknown at-rule constructor",
      `Quick,
      s552_unknown_at_rule_constructor );
    ( "spec CSS Syntax 5.5.2 unknown at-rule constructor reports locally",
      `Quick,
      s552_unknown_at_rule_error_is_local );
    ("spec @-moz-document prelude forms", `Quick, moz_document_prelude_forms);
    (* CSS nesting round-trip tests *)
    ("nesting basic", `Quick, test_nesting_basic);
    ("nesting ampersand hover", `Quick, test_nesting_ampersand_hover);
    ("nesting multiple nested rules", `Quick, test_nesting_multiple);
    ("nesting nested media query", `Quick, test_nesting_media);
    ("nesting deeply nested", `Quick, test_nesting_deep);
    ("nesting with declarations", `Quick, test_nesting_with_declarations);
    ("nesting check_stylesheet", `Quick, test_nesting_check_stylesheet);
    ("nesting invalid rule recovery", `Quick, nesting_invalid_rule_recovery);
    ( "spec nesting selector and conditional edges",
      `Quick,
      spec_nesting_selector_edges );
    ( "spec nesting declaration after a nested rule keeps its place",
      `Quick,
      spec_nesting_declaration_after_nested_rule );
    ( "spec nesting @layer block holds nesting content",
      `Quick,
      spec_nesting_layer_block );
    ( "spec nesting empty @layer keeps block form",
      `Quick,
      spec_nesting_empty_layer_keeps_block );
    ( "spec nesting @starting-style holds nesting content",
      `Quick,
      spec_nesting_starting_style );
    ( "conditional prelude errors name the at-rule and the slice",
      `Quick,
      conditional_prelude_errors );
    ( "spec nesting other group rules hold nesting content",
      `Quick,
      spec_nesting_other_group_rules );
    ( "spec nesting at-rule inside a nested group rule",
      `Quick,
      spec_nesting_at_rule_inside_nested_group );
    ( "spec nesting rejects a non-group at-rule",
      `Quick,
      spec_nesting_rejects_non_group_at_rules );
    ( "spec nesting rejects a non-group at-rule at every depth",
      `Quick,
      spec_nesting_rejects_non_group_at_rules_deep );
    ( "spec nesting rejection keeps the rest of the rule",
      `Quick,
      spec_nesting_rejection_keeps_the_rest );
    ( "spec nesting keeps @view-transition",
      `Quick,
      spec_nesting_keeps_view_transition );
    ( "spec nesting rejects an orphan @else",
      `Quick,
      spec_nesting_rejects_orphan_else );
    ( "spec nesting rejects a top-of-sheet at-rule",
      `Quick,
      spec_nesting_rejects_top_of_sheet_at_rules );
    ( "spec nesting reads an ident prelude in a nested at-rule",
      `Quick,
      spec_nesting_ident_prelude_in_nested_at_rule );
    ( "spec nesting skips stray semicolons in a nested at-rule",
      `Quick,
      spec_nesting_skips_stray_semicolons );
    ( "spec CSS Nesting L1 preserves nested structure",
      `Quick,
      nesting_module_l1_preserves_structure );
    ( "spec cascade 6.2 import preserved verbatim",
      `Quick,
      c6_2_import_preserved_verbatim );
    ( "spec CSS Syntax 4.3.2 source-map pragma is a comment",
      `Quick,
      s3432_sourcemap_comment );
    ( "spec cascade 6.4.4.2 layer statement equiv empty blocks",
      `Quick,
      c6442_empty_blocks_equiv );
    ( "spec cascade 6.4.3 dotted layer equiv nested layer",
      `Quick,
      c643_dotted_nested_layer );
    ( "spec CSS Syntax 4.3.2 printer never emits source-map",
      `Quick,
      s3432_no_sourcemap_print );
    ("spec values 4 6.1 zero length equivalence", `Quick, v461_zero_length_equiv);
    ("spec color 4 12.1 hex shorthand equivalence", `Quick, color4121_hex_equiv);
    ( "spec cascade 6.1 optimizer preserves winning value",
      `Quick,
      c61_keeps_winner );
    ("spec color 4 1.4 color form equivalence", `Quick, color414_form_equiv);
    ( "spec color 4 named/hex minification tie policy",
      `Quick,
      color4_hex_tie_policy );
    ( "spec values 4 6.5 zero percentage length equivalence",
      `Quick,
      v465_zero_percentage_equiv );
    ( "spec CSSOM 6.6.2 declaration serialization",
      `Quick,
      cssom662_decl_serialization );
    ( "spec CSSOM 6.7 no trailing semicolon",
      `Quick,
      cssom67_no_trailing_semicolon );
    ( "spec color 4 6.4 transparent equivalence",
      `Quick,
      color4_6_4_transparent_equivalence );
    ( "spec color 4 6.1 named color case insensitive",
      `Quick,
      color461_named_case );
    ("spec values 4 8.1 number format equivalence", `Quick, v481_number_format);
    ( "spec selectors 4 3.5 universal in compound redundant",
      `Quick,
      s435_universal_redundant );
    ( "spec selectors 4 selector list canonical order is optimize only",
      `Quick,
      selector_list_canonical_order );
    ( "spec scope start/end selector lists canonicalize in optimize",
      `Quick,
      scope_selector_list_canonical );
    ( "spec color 4 3 hue modulo canonicalization",
      `Quick,
      color4_3_hue_modulo_canonicalization );
    ( "spec color 4 1.3 alpha number percentage equivalence",
      `Quick,
      color413_alpha_equiv );
    ( "spec animations 1 7.1 keyframe from/to equivalence",
      `Quick,
      anim171_keyframe_equiv );
    ( "spec values 4 6.1 absolute units preserved under minify",
      `Quick,
      v461_absolute_units_minify );
    ( "spec values 4 6.6 time unit canonicalization",
      `Quick,
      v466_time_unit_canonical );
    ( "spec values 4 8.1 negative units preserved",
      `Quick,
      v481_negative_units_kept );
    ("spec values 4 8 trailing zero drop", `Quick, v4_8_trailing_zero_drop);
    ( "spec sizing 4 5 aspect-ratio preservation",
      `Quick,
      sizing4_5_aspect_ratio_preservation );
    ( "spec values 4 8.1 unit required for length (negative)",
      `Quick,
      v481_negative_unit_length );
    ( "spec values 4 6.6 time unit required (negative)",
      `Quick,
      v466_time_unit_required );
    ( "spec values 4 6.1 unknown length unit rejected (negative)",
      `Quick,
      v461_unknown_length_unit );
    ("fidelity time unit preserved", `Quick, fidelity_time_unit_preserved);
    ( "fidelity absolute units preserved",
      `Quick,
      fidelity_absolute_units_preserved );
    ( "fidelity negative units preserved",
      `Quick,
      fidelity_negative_units_preserved );
    ( "fidelity trailing zero preserved",
      `Quick,
      fidelity_trailing_zero_preserved );
    ("fidelity aspect-ratio preserved", `Quick, fidelity_aspect_ratio_preserved);
    ("spec values 4 10.2 calc single operand", `Quick, v4102_calc_single);
    ("spec values 4 10.2 calc arithmetic", `Quick, v4_10_2_calc_arithmetic);
    ("spec values 4 10.2 calc same-unit addition", `Quick, v4102_calc_addition);
    ( "spec values 4 10.2 calc same-unit percentage",
      `Quick,
      v4102_calc_percentage );
    ( "spec values 4 10.2 calc mixed-unit preserved",
      `Quick,
      v4102_calc_mixed_unit );
    ( "spec values 4 10 invalid calc rejected (negative)",
      `Quick,
      v4_10_invalid_calc_rejected );
    ( "fidelity calc simplifiable preserved",
      `Quick,
      fidelity_calc_simplifiable_preserved );
    ( "fidelity calc mixed-unit preserved",
      `Quick,
      fidelity_calc_mixed_unit_preserved );
    ("spec values 4 10.2 calc nested collapse", `Quick, v4102_calc_nested);
    ( "spec values 4 10.7 min/max constant reduction",
      `Quick,
      v4107_minmax_reduction );
    ("spec selectors 4 17 :is single-argument unwrap", `Quick, s417_is_unwrap);
    ( "spec selectors 4 17 :is top-level unwrap",
      `Quick,
      s417_is_unwrap_top_level );
    ( "spec selectors 4 4.2 compound type selector first",
      `Quick,
      s442_compound_type_first );
    ( "spec selectors 4 6.2 compound attribute canonicalization",
      `Quick,
      s462_compound_attr );
    ( "spec selectors 4 4.2 compound pseudo preserved",
      `Quick,
      s442_compound_pseudo_kept );
    ( "spec transforms 1 11 chain whitespace dropped",
      `Quick,
      transforms1_11_chain_whitespace_dropped );
    ( "spec custom-properties 1 2 var inlining preserved",
      `Quick,
      customprops12_inlining );
    ( "spec custom-properties 1 2 normal minify keeps runtime vars",
      `Quick,
      customprops12_runtime_vars );
    ( "spec custom-properties 1 2 var text payload not inlined",
      `Quick,
      customprops12_text_payload );
    ( "spec custom-properties 1 5 var cycle preserved",
      `Quick,
      customprops15_cycle );
    ("fidelity nested calc preserved", `Quick, fidelity_nested_calc_preserved);
    ("fidelity nested :is preserved", `Quick, fidelity_nested_is_preserved);
    ("fidelity min/max preserved", `Quick, fidelity_min_max_preserved);
    ( "fidelity compound pseudo preserved",
      `Quick,
      fidelity_compound_pseudo_preserved );
    ( "fidelity transform chain preserved",
      `Quick,
      fidelity_transform_chain_preserved );
    ("fidelity var preserved", `Quick, fidelity_var_preserved);
    ( "spec easing 1 2 named alias",
      `Quick,
      easing1_2_named_alias_canonicalization );
    ("spec easing 1 2 invalid (negative)", `Quick, easing1_2_invalid_rejected);
    ("fidelity easing preserved", `Quick, fidelity_easing_preserved);
    ("spec bg 3 2.1 default position elision", `Quick, bg321_default_pos_elision);
    ("spec bg 3 2.1 multi-layer preserved", `Quick, bg321_multi_layer_kept);
    ("fidelity background preserved", `Quick, fidelity_background_preserved);
    ("spec syntax 3 4.3.7 string escape decoding", `Quick, s3437_string_escape);
    ( "spec syntax 3 4.3.7 custom-property name escapes",
      `Quick,
      s4370_custom_property_name_escapes );
    ( "spec syntax 3 4.3.7 at-rule prelude name escapes",
      `Quick,
      s4370_at_rule_prelude_name_escapes );
    ( "spec syntax 3 4.3.7 property value name escapes",
      `Quick,
      s4370_property_value_name_escapes );
    ( "spec conditional 3 2.2 supports property name escapes",
      `Quick,
      s4370_supports_property_name_escapes );
    ("spec cascade 5 6.4.1 layer name parts", `Quick, s641_layer_name_parts);
    ( "fidelity string escape preserved",
      `Quick,
      fidelity_string_escape_preserved );
    ( "spec color 4 10 color space preserved",
      `Quick,
      color4_10_color_space_preserved );
    ( "spec color 4 invalid color function rejected (negative)",
      `Quick,
      color4_invalid_color_function_rejected );
    ("fidelity color space preserved", `Quick, fidelity_color_space_preserved);
    ( "spec conditional 4 2 supports preserved",
      `Quick,
      conditional4_2_supports_preserved );
    ( "spec conditional 4 2 author guard kept",
      `Quick,
      conditional4_2_supports_guard_kept );
    ( "spec conditional 4 2 supports invalid (negative)",
      `Quick,
      conditional4_2_supports_invalid_rejected );
    ("fidelity supports preserved", `Quick, fidelity_supports_preserved);
    ( "spec logical 1 3 logical property preserved",
      `Quick,
      logical1_3_logical_property_preserved );
    ( "fidelity logical property preserved",
      `Quick,
      fidelity_logical_property_preserved );
    ( "spec containment 3 6 container query preserved",
      `Quick,
      containment3_6_container_query_preserved );
    ( "fidelity container query preserved",
      `Quick,
      fidelity_container_query_preserved );
    ("spec fonts 4 6.5 font shorthand", `Quick, fonts4_6_5_font_shorthand);
    ( "spec fonts 4 invalid font rejected (negative)",
      `Quick,
      fonts4_invalid_font_rejected );
    ( "fidelity font shorthand preserved",
      `Quick,
      fidelity_font_shorthand_preserved );
    ("spec lists 3 list-style shorthand", `Quick, lists3_list_style_shorthand);
    ("fidelity list-style preserved", `Quick, fidelity_list_style_preserved);
    ( "spec cascade 6.3 important serialization preserved",
      `Quick,
      c6_3_important_serialization_preserved );
    ("fidelity important preserved", `Quick, fidelity_important_preserved);
    ("spec values 4 10 calc negative result", `Quick, v4_10_calc_negative_result);
    ( "spec values 4 10.7 operator precedence",
      `Quick,
      v4_10_7_operator_precedence );
    ( "spec values 4 10.7 identity operations",
      `Quick,
      v4_10_7_identity_operations );
    ( "spec values 4 10.7 percentage arithmetic",
      `Quick,
      v4_10_7_percentage_arithmetic );
    ( "spec values 4 10.7 chained multiplicative",
      `Quick,
      v4_10_7_chained_multiplicative );
    ("spec values 4 10.7 double negative", `Quick, v4_10_7_double_negative);
    ( "spec values 4 10.7 mixed relative units preserved",
      `Quick,
      v4107_mixed_units );
    ("spec values 4 10.7 math functions reduction", `Quick, v4107_math_reduction);
    ( "spec values 4 10.7 numeric math reduction",
      `Quick,
      v4107_numeric_reduction );
    ( "spec values 4 10.7 typed math function units",
      `Quick,
      v4107_typed_math_fn_units );
    ( "spec values 4 10.7 length times math reduction",
      `Quick,
      v4107_math_product_reduction );
    ( "authored numeric precision preserved",
      `Quick,
      authored_precision_preserved );
    ( "computed numeric precision keeps integer digits",
      `Quick,
      computed_precision_keeps_integer_digits );
    ("spec values 4 10.7 mod/rem reduction", `Quick, v4107_mod_rem);
    ("spec values 4 10.7 round reduction", `Quick, v4_10_7_round_reduction);
    ( "spec values 4 10.7 division by zero preserved",
      `Quick,
      v4107_division_zero );
    ( "spec values 4 10 invalid calc extra rejected (negative)",
      `Quick,
      v410_invalid_calc_extra );
    ( "fidelity calc negative preserved",
      `Quick,
      fidelity_calc_negative_preserved );
    ( "fidelity calc precedence preserved",
      `Quick,
      fidelity_calc_precedence_preserved );
    ("fidelity calc chained preserved", `Quick, fidelity_calc_chained_preserved);
    ( "fidelity calc mixed relative preserved",
      `Quick,
      fidelity_calc_mixed_relative_preserved );
    ( "fidelity calc math functions preserved",
      `Quick,
      fidelity_calc_math_functions_preserved );
    ( "fidelity calc length times math functions preserved",
      `Quick,
      fidelity_calc_math_product_preserved );
    ( "spec custom-properties 1 2 empty fallback preserved",
      `Quick,
      customprops12_empty_fallback );
    ( "spec custom-properties 1 2 color in fallback canonicalizes",
      `Quick,
      customprops12_color_fallback );
    ( "spec custom-properties 1 2 nested var fallback preserved",
      `Quick,
      customprops12_nested_fallback );
    ( "spec custom-properties 1 2 calc in fallback",
      `Quick,
      customprops12_calc_fallback );
    ( "spec custom-properties 1 2 multi-comma fallback preserved",
      `Quick,
      customprops12_multi_comma );
    ( "spec custom-properties 1 2 var whitespace dropped",
      `Quick,
      customprops12_whitespace );
    ( "spec custom-properties 1 2 name case sensitive",
      `Quick,
      customprops12_case_sensitive );
    ( "spec custom-properties 1 3 custom property declaration",
      `Quick,
      customprops13_declaration );
    ( "spec scroll-snap 6.2 trailing semicolon",
      `Quick,
      scroll_snap_align_trailing_semicolon );
    ( "spec custom-properties 1 3 colour keyword case fold",
      `Quick,
      customprops13_color_keyword_case_fold );
    ( "spec custom-properties 1 3 unregistered font stack",
      `Quick,
      customprops13_unregistered_font_stack );
    ( "spec custom-properties 1 3 unregistered theme tokens",
      `Quick,
      customprops13_unregistered_theme_tokens );
    ( "spec custom-properties 1 3 registered numeric calc",
      `Quick,
      customprops13_registered_numeric_calc );
    ( "spec custom-properties 1 3 registered oklch chroma",
      `Quick,
      customprops13_registered_oklch_chroma );
    ( "spec custom-properties 1 3 registered percent calc",
      `Quick,
      customprops13_registered_percent_calc );
    ( "spec custom-properties 1 3 registered negative dimension calc",
      `Quick,
      customprops13_registered_negative_dimension_calc );
    ( "spec custom-properties 1 3 registered angle and time calc",
      `Quick,
      customprops13_registered_angle_time_calc );
    ( "spec custom-properties 1 3 trigonometric calc",
      `Quick,
      customprops13_trigonometric_calc );
    ( "spec custom-properties 1 3 unregistered unresolved calc spacing",
      `Quick,
      customprops13_shortest_unresolved_calc_spacing );
    ( "spec custom-properties 1 3 unregistered box-shadow token stream",
      `Quick,
      customprops13_box_shadow_zero_spread );
    ( "spec custom-properties 1 3 hex colour folds",
      `Quick,
      customprops13_hex_color_folds );
    ( "spec custom-properties 1 3 registered oklab sign boundaries",
      `Quick,
      customprops13_shortest_oklab_sign_boundaries );
    ( "spec custom-properties 1 2 invalid var rejected (negative)",
      `Quick,
      customprops12_invalid_var );
    ( "spec custom-properties 1 2 var in shorthand and calc",
      `Quick,
      customprops12_shorthand_calc );
    ("fidelity var fallback preserved", `Quick, fidelity_var_fallback_preserved);
    ("fidelity var calc in fallback preserved", `Quick, fidelity_calc_fallback);
    ("fidelity var multi-comma fallback preserved", `Quick, fidelity_multi_comma);
    ( "fidelity var name case preserved",
      `Quick,
      fidelity_var_name_case_preserved );
    ( "fidelity custom property declaration preserved",
      `Quick,
      fidelity_custom_property_decl_preserved );
    ( "spec custom-properties 1 theme inlining",
      `Quick,
      custom_props1_theme_inlining );
    ( "spec custom-properties 1 fallback preserved when unknown",
      `Quick,
      customprops1_unresolved_fallback );
    ( "spec custom-properties 1 nested theme var in typed fallback resolves",
      `Quick,
      customprops1_transitive_fallback );
    ( "spec custom-properties 1 resolve keeps unrelated @property",
      `Quick,
      customprops1_resolve_keeps_property );
    ( "spec custom-properties 1 inline declared root-scope token",
      `Quick,
      customprops1_inline_declared_root_token );
    ( "spec custom-properties 1 inlined var in calc simplifies",
      `Quick,
      customprops1_calc_inline );
    ( "spec custom-properties 1 transitive theme var merges in place",
      `Quick,
      customprops1_transitive_merge );
    ( "theme defaults reject a value that escapes its declaration",
      `Quick,
      theme_defaults_reject_escaping_value );
    ( "theme defaults bind a name that needs escaping",
      `Quick,
      theme_defaults_escaping_name );
    ( "spec custom-properties 1 fallback list with inlining",
      `Quick,
      customprops1_fallback_list );
    ( "spec custom-properties 1 var in value position",
      `Quick,
      customprops1_value_position );
    ( "spec custom-properties 1 theme chain resolution",
      `Quick,
      theme_chain_resolution );
    ( "spec custom-properties 1 theme cycle preserved",
      `Quick,
      theme_cycle_preserved );
    ( "spec custom-properties 1 inlined color canonicalizes",
      `Quick,
      custom_props1_inlined_color_canonicalizes );
    ( "fidelity no inlining without context",
      `Quick,
      fidelity_no_inlining_without_context );
    ( "spec custom-properties 1 fallback resolution mode",
      `Quick,
      custom_props1_fallback_resolution_mode );
    ( "spec custom-properties 1 theme protects var",
      `Quick,
      custom_props1_theme_protects_var );
    ( "spec custom-properties 1 theme set is not undefined set",
      `Quick,
      theme_set_not_undefined );
    ( "fidelity typed var default not inlined in stylesheet mode",
      `Quick,
      typed_var_default_fidelity );
    ( "spec color 4 12 rgb clamp canonicalization",
      `Quick,
      color4_12_rgb_clamp_canonicalization );
    ( "spec color 4 1.3 fully opaque alpha collapse",
      `Quick,
      color413_opaque_alpha_collapse );
    ( "spec values 4 8.1 number trailing zero canonicalization",
      `Quick,
      v481_trailing_zero );
    ( "spec fonts 4 5.1.2 font-weight keyword to number",
      `Quick,
      fonts4512_weight_number );
    ( "spec box 4 margin shorthand collapse",
      `Quick,
      box4_margin_shorthand_collapse );
    ( "spec color 4 6.4 transparent canonical shortest",
      `Quick,
      color464_transparent_shortest );
    ( "spec values 4 6.5 zero length canonical shortest",
      `Quick,
      v465_zero_length_shortest );
    ("spec bg 3 5 border-radius collapse shortest", `Quick, bg35_radius_collapse);
    ("spec values 4 10 calc add zero simplification", `Quick, v410_calc_add_zero);
    ( "spec bg 3 3.6 background-position collapse shortest",
      `Quick,
      bg336_bgpos_collapse );
    (* Non-minified fidelity: pretty printer preserves the source spelling. *)
    ("pretty at-rule block bodies", `Quick, pretty_at_rule_block_bodies);
    ("fidelity hex form preserved", `Quick, fidelity_hex_form_preserved);
    ("fidelity color form preserved", `Quick, fidelity_color_form_preserved);
    ( "fidelity keyframe selector preserved",
      `Quick,
      fidelity_keyframe_selector_preserved );
    ( "fidelity universal in compound preserved",
      `Quick,
      fidelity_universal_in_compound_preserved );
    ("fidelity alpha form preserved", `Quick, fidelity_alpha_form_preserved);
    ( "fidelity percentage precision preserved",
      `Quick,
      fidelity_percentage_precision_preserved );
    ("fidelity zero length preserved", `Quick, fidelity_zero_length_preserved);
    ( "fidelity shorthand form preserved",
      `Quick,
      fidelity_shorthand_form_preserved );
    ( "fidelity font-weight keyword preserved",
      `Quick,
      fidelity_font_weight_keyword_preserved );
    ( "spec selectors 4 14 nth-child canonicalization",
      `Quick,
      s4_14_nth_child_canonicalization );
    ( "spec selectors 4 6.2 attribute quote canonicalization",
      `Quick,
      s462_attr_quote_canonical );
    ( "spec values 4 8.1 scientific notation expansion",
      `Quick,
      v481_scientific_notation );
    ("spec values 4 8.1 negative zero canonical", `Quick, v481_negative_zero);
    ( "spec values 4 7 url quote canonicalization",
      `Quick,
      v4_7_url_quote_canonicalization );
    ( "spec values 4 10 calc nested constant simplification",
      `Quick,
      v410_calc_nested_constant );
    ( "spec cascade 6.1 consecutive same-condition merge",
      `Quick,
      c61_same_condition_merge );
    ( "spec cascade 6.1 merge across non-conflicting intervening rule pair",
      `Quick,
      c61_merge_across_nonconflicting );
    ( "fidelity nth-child form preserved",
      `Quick,
      fidelity_nth_child_form_preserved );
    ( "fidelity attribute quotes preserved",
      `Quick,
      fidelity_attribute_quotes_preserved );
    ( "fidelity scientific notation preserved",
      `Quick,
      fidelity_scientific_notation_preserved );
    ("fidelity url quotes preserved", `Quick, fidelity_url_quotes_preserved);
    ("fidelity calc form preserved", `Quick, fidelity_calc_form_preserved);
    ( "spec bg 3 3.6 position keyword canonicalization",
      `Quick,
      bg336_position_keyword );
    ("spec cascade 6.1 selector grouping", `Quick, c6_1_selector_grouping);
    ("vendor prefix preservation", `Quick, vendor_prefix_preservation);
    ( "vendor text-decoration-color compatibility",
      `Quick,
      webkit_decoration_color_compat );
    ("spec selectors 4 4.3 not form preserved", `Quick, s443_not_form_kept);
    ( "spec display 3 border keyword preservation",
      `Quick,
      display3_border_keyword_preservation );
    ( "fidelity position keywords preserved",
      `Quick,
      fidelity_position_keywords_preserved );
    ( "fidelity vendor prefix preserved",
      `Quick,
      fidelity_vendor_prefix_preserved );
    ("fidelity not form preserved", `Quick, fidelity_not_form_preserved);
    ( "fidelity dead property preserved",
      `Quick,
      fidelity_dead_property_preserved );
    ("fidelity empty rule preserved", `Quick, fidelity_empty_rule_preserved);
    ( "fidelity css-wide keywords preserved",
      `Quick,
      fidelity_css_wide_keywords_preserved );
    ( "fidelity incomplete font-face preserved",
      `Quick,
      fidelity_incomplete_font_face_preserved );
    ("css var fallback preserved", `Quick, css_var_fallback_preserved);
    ( "spec cascade 6.4.4 anonymous layers distinct",
      `Quick,
      c644_anonymous_layers_distinct );
    ( "spec cascade 6.3.2 all shorthand preserved",
      `Quick,
      c632_all_shorthand_kept );
    ( "spec cascade 6.4 named layers preserve order",
      `Quick,
      c64_named_layers_order );
    ( "spec cascade 6.1 dead shorthand removed",
      `Quick,
      c6_1_dead_shorthand_removed );
    ("spec cascade 6.1 empty rule removed", `Quick, c6_1_empty_rule_removed);
    ("spec cascade 6.7 css-wide keywords preserved", `Quick, c67_css_wide_kept);
    ( "spec cascade 6.7 css-wide keyword inside list",
      `Quick,
      c67_bad_css_wide_list );
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
        (* Strict [read] would raise on the bad [rgb()]. The partial entry point
           drops just the bad declaration; both rules survive (the empty
           [.a\{\}] and the good [.b]). Per 5.4.4. *)
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
            match Error.snippet e with
            | None -> Alcotest.fail "expected warning with snippet"
            | Some _ -> ())
        | _ -> Alcotest.fail "expected exactly one warning" );
    ( "partial recovery: Css.of_string ~strict:false entry point",
      `Quick,
      fun () ->
        let { Css.stylesheet; warnings; _ } =
          match
            Css.of_string ~strict:false
              ".a { color: red; } .b { color: rgb(300); }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient parse failed: %s"
                (Cascade.Error.to_string err)
        in
        Alcotest.(check int)
          "both rules survive" 2
          (List.length (Css.rule_statements stylesheet));
        Alcotest.(check int) "one warning" 1 (List.length warnings);
        (* filename threads through *)
        match warnings with
        | [ w ] ->
            Alcotest.(check (option string))
              "default filename" (Some "<string>") w.Cascade.Error.filename
        | _ -> Alcotest.fail "expected one warning" );
    ( "of_string ~strict:true rejects unknown at-rule",
      `Quick,
      fun () ->
        match
          Css.of_string ~strict:true
            "@unknown { color: red } .a { color: blue }"
        with
        | Ok _ ->
            Alcotest.fail "strict mode should reject @unknown as a warning"
        | Error _ -> () );
    ( "of_string ~strict:true accepts clean CSS",
      `Quick,
      fun () ->
        match
          Css.of_string ~strict:true ".a { color: red } .b { color: blue }"
        with
        | Ok parsed ->
            let css = minify parsed.stylesheet in
            Alcotest.(check string)
              "strict output" ".a{color:red}.b{color:#00f}" css
        | Error e ->
            Alcotest.failf "strict mode rejected clean CSS: %s"
              (Error.to_string e) );
    ( "of_string default is lenient and returns parse_result warnings",
      `Quick,
      fun () ->
        match
          Css.of_string ~strict:false
            "@unknown { color: red } .a { color: blue }"
        with
        | Ok parsed ->
            Alcotest.(check bool)
              "warning surfaced" true
              (parsed.Css.warnings <> []);
            Alcotest.(check string)
              "recovered output" "@unknown{color:red}.a{color:#00f}"
              (minify parsed.stylesheet)
        | Error e ->
            Alcotest.failf "non-strict mode should not promote warnings: %s"
              (Error.to_string e) );
    ( "of_string ~strict:true rejects bad declaration value",
      `Quick,
      fun () ->
        match Css.of_string ~strict:true ".a { color: rgb(300); }" with
        | Ok _ -> Alcotest.fail "strict mode should reject invalid colour value"
        | Error _ -> () );
    ( "partial recovery: unclosed brace recovered per 5.3.7",
      `Quick,
      fun () ->
        let { Css.stylesheet; warnings = _; _ } =
          match Css.of_string ~strict:false ".btn { color: red;" with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient unclosed-brace recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        Alcotest.(check int)
          "one rule recovered" 1
          (List.length (Css.rule_statements stylesheet)) );
    ( "partial recovery: bad declaration drops, rule survives",
      `Quick,
      fun () ->
        (* CSS Syntax 5.4.4: an invalid declaration is discarded while the
           enclosing rule and its other declarations survive. *)
        let { Css.stylesheet; warnings; _ } =
          match
            Css.of_string ~strict:false
              ".a { color: invalidcolor; color: red; }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient bad-declaration recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        let rules = Css.rule_statements stylesheet in
        Alcotest.(check int) "rule kept" 1 (List.length rules);
        Alcotest.(check int)
          "one warning for the bad decl" 1 (List.length warnings) );
    ( "partial recovery: filename propagates to warnings",
      `Quick,
      fun () ->
        let { Css.warnings; _ } =
          match
            Css.of_string ~strict:false ~filename:"user.css"
              ".a { color: rgb(300); }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient filename propagation parse failed: %s"
                (Cascade.Error.to_string err)
        in
        match warnings with
        | [ w ] ->
            Alcotest.(check (option string))
              "filename" (Some "user.css") w.Cascade.Error.filename
        | _ -> Alcotest.fail "expected one warning" );
    ( "meta: `Full attaches snippets, `None skips them",
      `Quick,
      fun () ->
        let input = ".a { color: rgb(300); }" in
        let full =
          match Css.of_string ~strict:false ~meta:`Full input with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "`Full parse failed: %s"
                (Cascade.Error.to_string err)
        in
        let none =
          match Css.of_string ~strict:false ~meta:`None input with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "`None parse failed: %s"
                (Cascade.Error.to_string err)
        in
        (match full.warnings with
        | [ e ] ->
            Alcotest.(check bool)
              "`Full: snippet present" true
              (Error.snippet e <> None)
        | _ -> Alcotest.fail "expected one warning under `Full");
        match none.warnings with
        | [ e ] ->
            Alcotest.(check bool)
              "`None: snippet skipped" true
              (Error.snippet e = None)
        | _ -> Alcotest.fail "expected one warning under `None" );
    ( "semicolon-terminated at-rule survives partial parse",
      `Quick,
      fun () ->
        (* [@layer base;] parses through section 5.5.2 to an at-rule with [block
           = None]. The replay cursor must still present a terminating [;] to
           [read_layer], otherwise the at-rule is silently dropped. *)
        let { Css.stylesheet; warnings; _ } =
          match Css.of_string ~strict:false "@layer base;" with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient @layer parse failed: %s"
                (Cascade.Error.to_string err)
        in
        let stmts = Css.statements stylesheet in
        Alcotest.(check int) "one statement" 1 (List.length stmts);
        Alcotest.(check int) "no warnings" 0 (List.length warnings) );
    ( "unknown at-rule surfaces as Unknown_at_rule warning",
      `Quick,
      fun () ->
        (* Section 5.4.1: an at-rule with no handler is discarded with a typed
           warning; the surrounding stylesheet continues to parse. *)
        let { Css.stylesheet; warnings; _ } =
          match
            Css.of_string ~strict:false
              "@unknown-rule { color: red; } .a { color: blue; }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient unknown at-rule recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        Alcotest.(check int)
          "good rule survives" 1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] -> (
            match e.Error.kind with
            | Error.Unknown_at_rule name ->
                Alcotest.(check string) "at-rule name" "unknown-rule" name
            | _ ->
                Alcotest.failf "expected Unknown_at_rule, got %s"
                  (Error.to_string e))
        | _ -> Alcotest.fail "expected one warning" );
    ( "malformed @supports surfaces as Bad_condition warning",
      `Quick,
      fun () ->
        (* A bad @supports condition used to raise via the untyped
           [Reader.Parse_error] shape, bypassing the partial-parse contract. It
           now becomes a typed [Bad_condition] warning while surrounding rules
           keep parsing. *)
        let { Css.stylesheet; warnings; _ } =
          match
            Css.of_string ~strict:false
              "@supports not-a-function foo { .a { color: red } } .b { color: \
               blue }"
          with
          | Ok parsed -> parsed
          | Error err ->
              Alcotest.failf "lenient @supports recovery failed: %s"
                (Cascade.Error.to_string err)
        in
        Alcotest.(check int)
          "sibling rule survives" 1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] -> (
            match e.Error.kind with
            | Error.Bad_condition { at_rule; _ } ->
                Alcotest.(check string) "at-rule label" "@supports" at_rule
            | _ ->
                Alcotest.failf "expected Bad_condition, got %s"
                  (Error.to_string e))
        | _ -> Alcotest.fail "expected one warning" );
    ( "an @container query error points at the failing slice",
      `Quick,
      container_condition_error_spans );
    ( "a media query error points at the failing slice",
      `Quick,
      media_condition_error_spans );
    ( "an @font-face descriptor error points at the value",
      `Quick,
      descriptor_value_error_spans );
    ( "an @supports condition error points at the failing slice",
      `Quick,
      supports_condition_error_spans );
  ]

(* Every shape a statement can take, so the walkers are exercised on the block
   at-rules and on the at-rules that hold declarations outside a block. *)
let every_statement_shape =
  "@charset \"utf-8\";\n\
   @import url(a.css);\n\
   @namespace svg url(http://www.w3.org/2000/svg);\n\
   @property --p { syntax: \"<color>\"; inherits: false; initial-value: red }\n\
   @layer base, theme;\n\
   @layer base { .a { color: red; & .b { color: blue } } }\n\
   @media print { .c { color: red } }\n\
   @container card (width > 10px) { .d { color: red } }\n\
   @supports (display: grid) { .e { color: red } }\n\
   @-moz-document url-prefix(\"http://x\") { .f { color: red } }\n\
   @starting-style { .g { color: red } }\n\
   @scope (.h) to (.i) { .j { color: red } }\n\
   @keyframes k { from { color: red } to { color: blue } }\n\
   @-webkit-keyframes wk { from { color: red } }\n\
   @font-face { font-family: F; src: url(f.woff2) }\n\
   @counter-style cs { system: cyclic; symbols: a }\n\
   @page { margin: 1cm }\n\
   @page :first { margin: 1cm; @top-left { content: \"x\" } }\n\
   @font-palette-values --fp { font-family: F }\n\
   @view-transition { navigation: auto }\n\
   @position-try --pt { top: 1px }\n\
   @viewport { width: device-width }\n\
   .k { color: red }\n"

let parse_shapes () =
  match Css.of_string every_statement_shape with
  | Ok { Css.stylesheet; _ } -> stylesheet
  | Error err ->
      Alcotest.failf "shape corpus did not parse: %s"
        (Cascade.Error.to_string err)

let walker_tests =
  [
    ( "map_statement_children keeps an unchanged statement",
      `Quick,
      fun () ->
        (* A rewriting walk short-circuits on physical equality, so a map that
           reallocates a statement it did not change costs the caller the
           sharing its fixed point converges on. *)
        List.iter
          (fun stmt ->
            if not (map_statement_children Fun.id stmt == stmt) then
              Alcotest.failf "children map rebuilt %s"
                (Css.to_string ~minify:true [ stmt ]))
          (parse_shapes ()) );
    ( "map_statement_declarations keeps an unchanged statement",
      `Quick,
      fun () ->
        List.iter
          (fun stmt ->
            if not (map_statement_declarations Fun.id stmt == stmt) then
              Alcotest.failf "declaration map rebuilt %s"
                (Css.to_string ~minify:true [ stmt ]))
          (parse_shapes ()) );
  ]

(* One declaration per place a statement can hold one, each under a property
   name that names only that place, so a walk that misses a place is a missing
   name rather than a smaller count. *)
let every_declaration_place =
  ".el { color: red }\n\
   @media print { .m { display: none } }\n\
   @layer l { .l { float: left } }\n\
   @container (width > 1px) { .c { clear: both } }\n\
   @supports (display: grid) { .s { z-index: 1 } }\n\
   @-moz-document url-prefix(\"x\") { .d { direction: rtl } }\n\
   @starting-style { .ss { opacity: 0 } }\n\
   @scope (.a) to (.b) { .sc { visibility: hidden } }\n\
   @when media(print) { .w { order: 1 } }\n\
   @else { .e { order: 2 } }\n\
   .parent { & .nested { overflow: hidden } }\n\
   @keyframes k { from { rotate: 0deg } }\n\
   @page { size: a4 }\n\
   @page :first { @top-left { content: \"tl\" } }\n\
   @position-try --pt { inset: 1px }\n\
   @supports-condition --sc { padding: 1px }\n"

let parsed source =
  match Css.of_string source with
  | Ok { Css.stylesheet; _ } -> stylesheet
  | Error err ->
      Alcotest.failf "declaration-place corpus did not parse: %s"
        (Cascade.Error.to_string err)

let places () =
  (* [Origin] has no CSS syntax, so it can only be built. *)
  parsed every_declaration_place
  @ [ with_origin Author (parsed ".o { top: 1px }") ]

let properties_folded ?sites () =
  List.sort compare
    (fold_declarations ?sites
       (fun acc decls ->
         List.rev_append (List.map Css.Declaration.property_name decls) acc)
       [] (places ()))

let element_places =
  [
    "clear";
    "color";
    "direction";
    "display";
    "float";
    "opacity";
    "order";
    "order";
    "overflow";
    "top";
    "visibility";
    "z-index";
  ]

let other_places = [ "content"; "inset"; "padding"; "rotate"; "size" ]

let deep_walker_tests =
  [
    ( "fold_declarations reaches every place a declaration sits",
      `Quick,
      fun () ->
        Alcotest.(check (list string))
          "every property"
          (List.sort compare (element_places @ other_places))
          (properties_folded ()) );
    ( "fold_declarations keeps to the sites it is given",
      `Quick,
      fun () ->
        (* The narrow walk names the places it wants rather than the statements
           it expects to meet, so leaving one out is a stated choice. *)
        let sites =
          {
            element_rule = true;
            animation_frame = false;
            page_box = false;
            position_fallback = false;
            condition_test = false;
          }
        in
        Alcotest.(check (list string))
          "element declarations only"
          (List.sort compare element_places)
          (properties_folded ~sites ()) );
    ( "at_declaration_site makes the fold's choice outside the fold",
      `Quick,
      fun () ->
        (* A walk that carries something down the tree recurses itself, so it
           reads the sites rather than passing them; it must land on the same
           declarations the fold does. *)
        let sites =
          {
            element_rule = true;
            animation_frame = false;
            page_box = false;
            position_fallback = false;
            condition_test = false;
          }
        in
        let rec walk acc stmt =
          let acc =
            if at_declaration_site sites stmt then
              List.rev_append
                (List.map Css.Declaration.property_name
                   (statement_declarations stmt))
                acc
            else acc
          in
          List.fold_left walk acc (statement_children stmt)
        in
        Alcotest.(check (list string))
          "the sites the fold folds over"
          (properties_folded ~sites ())
          (List.sort compare (List.fold_left walk [] (places ()))) );
    ( "map_declarations rewrites every place a declaration sits",
      `Quick,
      fun () ->
        let emptied = map_declarations (fun _ -> []) (places ()) in
        Alcotest.(check (list string))
          "nothing left" []
          (fold_declarations
             (fun acc decls ->
               List.rev_append
                 (List.map Css.Declaration.property_name decls)
                 acc)
             [] emptied) );
    ( "map_declarations keeps an unchanged tree",
      `Quick,
      fun () ->
        let block = places () in
        if not (map_declarations Fun.id block == block) then
          Alcotest.fail "declaration map rebuilt an unchanged stylesheet" );
    ( "iter_statements reaches a statement inside every at-rule",
      `Quick,
      fun () ->
        let seen = ref [] in
        iter_statements
          (fun stmt ->
            match stmt with
            | Rule r ->
                seen := Selector.to_string ~minify:true r.selector :: !seen
            | _ -> ())
          (places ());
        Alcotest.(check (list string))
          "every rule"
          (List.sort compare
             [
               ".el";
               ".m";
               ".l";
               ".c";
               ".s";
               ".d";
               ".ss";
               ".sc";
               ".w";
               ".e";
               ".parent";
               "& .nested";
               ".o";
             ])
          (List.sort compare !seen) );
    ( "edit_statements drops a statement inside every at-rule",
      `Quick,
      fun () ->
        let dropped =
          edit_statements (function Rule _ -> Drop | _ -> Keep) (places ())
        in
        Alcotest.(check (list string))
          "no rule left" []
          (fold_statements
             (fun acc stmt ->
               match stmt with
               | Rule r -> Selector.to_string ~minify:true r.selector :: acc
               | _ -> acc)
             [] dropped) );
    ( "edit_statements walks into a replacement",
      `Quick,
      fun () ->
        (* The nested rule is reachable only through the rule that was replaced,
           so it is marked when the walk continues into the replacement rather
           than into the statement it replaced. *)
        let mark = function
          | Rule r -> Replace (Rule { r with merge_key = Some "k" })
          | _ -> Keep
        in
        Alcotest.(check (list string))
          "every rule marked" []
          (fold_statements
             (fun acc stmt ->
               match stmt with
               | Rule r when r.merge_key = None ->
                   Selector.to_string ~minify:true r.selector :: acc
               | _ -> acc)
             []
             (edit_statements mark (places ()))) );
    ( "edit_statements keeps an unchanged tree",
      `Quick,
      fun () ->
        let block = places () in
        if not (edit_statements (fun _ -> Keep) block == block) then
          Alcotest.fail "statement edit rebuilt an unchanged stylesheet" );
  ]

(* --- rendering --- *)

(* [to_string] and the bare formatter run the same printer, so they agree byte
   for byte; the only difference either may have is how the output bytes are
   collected. *)
let one_pass ~minify sheet =
  let pp ctx () = pp_stylesheet ctx sheet in
  Css.Pp.to_string ~minify pp ()

let render_sheet n =
  let b = Buffer.create (n * 96) in
  let out = Fmt.with_buffer b in
  for i = 0 to n - 1 do
    Fmt.pf out
      ".c%d .d%d > span:hover{color:rgb(%d,%d,%d);margin:%dpx \
       %dpx;padding:%dpx;display:flex}@media (min-width:%dpx){.m%d{outline:1px \
       solid #abc}}"
      i i (i mod 256)
      (i * 7 mod 256)
      (i * 13 mod 256)
      (i mod 40)
      (i * 3 mod 40)
      (i mod 20)
      (300 + (i mod 900))
      i
  done;
  Fmt.flush out ();
  Css.of_string_exn (Buffer.contents b)

(* A serialiser walks the tree once. Presizing the buffer from a [Pp.size]
   prepass walks it a second time, and the counter sink skips only the output
   bytes: [normalise], [printable_statements] and every printer below them run
   twice. Buffer growth is amortised, so the second walk buys nothing, and its
   cost is the whole formatter rather than a rounding error - pin it well below
   the 1.8x the prepass measured. *)
let to_string_renders_once minify () =
  let sheet = render_sheet 400 in
  let a_one = measure (fun () -> one_pass ~minify sheet) in
  let a_full = measure (fun () -> to_string ~minify sheet) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx the one-pass walk)" a_one a_full
       (a_full /. a_one))
    true
    (a_one = 0. || a_full < a_one *. 1.25)

let to_string_matches_one_pass minify () =
  let sheet = render_sheet 400 in
  Alcotest.(check string)
    "same bytes" (one_pass ~minify sheet) (to_string ~minify sheet)

let render_tests =
  [
    ( "to_string minified matches the bare formatter",
      `Quick,
      to_string_matches_one_pass true );
    ( "to_string pretty matches the bare formatter",
      `Quick,
      to_string_matches_one_pass false );
    ("to_string renders minified once", `Quick, to_string_renders_once true);
    ("to_string renders pretty once", `Quick, to_string_renders_once false);
  ]

(* {2 Statement equality and fingerprint} *)

(* One statement per shape, spelled in the CSS that produces it. Every shape is
   parsed rather than assembled so the test reads what a stylesheet holds, not
   what a constructor was handed. *)
let parsed_shapes =
  [
    ("rule", ".a{color:red}");
    ("bang comment", "/*! keep */");
    ("charset", "@charset \"UTF-8\";");
    ("import", "@import url(a.css) layer(x) supports(display:grid) screen;");
    ("namespace", "@namespace svg url(http://www.w3.org/2000/svg);");
    ( "property",
      "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}" );
    ("layer statement", "@layer a,b;");
    ("layer block", "@layer a{.x{color:red}}");
    ("media", "@media screen{.x{color:red}}");
    ("container", "@container card (min-width:10px){.x{color:red}}");
    ("supports", "@supports (display:grid){.x{color:red}}");
    ("moz-document", "@-moz-document url-prefix(){.x{color:red}}");
    ("starting-style", "@starting-style{.x{color:red}}");
    ("when", "@when media(width>0px){.x{color:red}}");
    ("supports-condition", "@supports-condition --n{color:red}");
    ("scope", "@scope (.a) to (.b){.x{color:red}}");
    ("keyframes", "@keyframes k{from{opacity:0}to{opacity:1}}");
    ("webkit keyframes", "@-webkit-keyframes k{from{opacity:0}}");
    ("moz keyframes", "@-moz-keyframes k{from{opacity:0}}");
    ("font-face", "@font-face{font-family:A;src:url(a.woff2)}");
    ("counter-style", "@counter-style c{system:cyclic;symbols:\"a\"}");
    ("page with margins", "@page{margin:1cm;@top-left{content:\"x\"}}");
    ( "font-palette-values",
      "@font-palette-values --p{font-family:A;override-colors:0 red}" );
    ("font-feature-values", "@font-feature-values A{@styleset{nice:1}}");
    ("view-transition", "@view-transition{navigation:auto}");
    ("position-try", "@position-try --p{top:0}");
    ("viewport", "@viewport{width:100px}");
    ("unknown at-rule", "@wibble foo{bar:1}");
  ]

let sole_statement css =
  match Css.statements (Css.of_string_exn css) with
  | [ s ] -> s
  | l -> Alcotest.failf "%s: expected one statement, got %d" css (List.length l)

(* The four shapes with no source text of their own. [Declarations] holds the
   bare declarations of a nested block, [Else] needs the [@when] it answers,
   [Origin] is an API-level wrapper the CSS grammar has no spelling for, and the
   parser folds every [@page] into [Page_with_margins]. *)
let bare_declarations () =
  match
    Css.statements (Css.of_string_exn ".a{color:red;@media screen{color:blue}}")
  with
  | [ Rule { nested = [ Media (_, [ (Declarations _ as d) ]) ]; _ } ] -> d
  | _ -> Alcotest.fail "expected bare declarations inside a nested @media"

let else_branch () =
  match
    Css.statements
      (Css.of_string_exn
         "@when media(width>0px){.x{color:red}}@else{.y{color:red}}")
  with
  | [ _; (Else _ as e) ] -> e
  | _ -> Alcotest.fail "expected an @else branch"

let statement_shapes () =
  List.map (fun (label, css) -> (label, sole_statement css)) parsed_shapes
  @ [
      ("bare declarations", bare_declarations ());
      ("else", else_branch ());
      ("origin", with_origin User [ sole_statement ".x{color:red}" ]);
      ("page", Page ([], [ Css.Declaration.of_string "margin:1cm" ]));
    ]

(* Two independent builds of the list, so no answer below can come from physical
   equality of a shared node. *)
let shape_matrix f =
  let left = statement_shapes () and right = statement_shapes () in
  List.concat_map
    (fun (la, sa) -> List.filter_map (fun (lb, sb) -> f la sa lb sb) right)
    left

let statement_equality_separates_every_shape () =
  let wrong =
    shape_matrix (fun la sa lb sb ->
        let same_shape = String.equal la lb in
        if Bool.equal (Css.equal_statement sa sb) same_shape then None
        else Some (String.concat "" [ la; " vs "; lb ]))
  in
  Alcotest.(check (list string))
    "each shape equals its own copy and no other" [] wrong

(* A hash that disagreed with the equality it serves would put one statement in
   two buckets. The converse is not required: a collision is allowed. *)
let statement_hash_agrees_with_equality () =
  let wrong =
    shape_matrix (fun la sa lb sb ->
        if
          Css.equal_statement sa sb
          && Css.hash_statement sa <> Css.hash_statement sb
        then Some (String.concat "" [ la; " vs "; lb ])
        else None)
  in
  Alcotest.(check (list string)) "equal statements hash alike" [] wrong;
  (* A constant hash would satisfy the rule above and be useless. *)
  Alcotest.(check bool)
    "two shapes are not one bucket" false
    (Css.hash_statement (sole_statement ".a{color:red}")
    = Css.hash_statement (sole_statement "@media screen{.x{color:red}}"))

(* What a statement holds is read, not just its shape. *)
let statement_equality_reads_the_payload () =
  let differs label a b =
    if Css.equal_statement (sole_statement a) (sole_statement b) then Some label
    else None
  in
  let folded =
    List.filter_map
      (fun (label, a, b) -> differs label a b)
      [
        ("a selector", ".a{color:red}", ".b{color:red}");
        ("a declaration", ".a{color:red}", ".a{color:blue}");
        ( "a media query",
          "@media screen{.x{color:red}}",
          "@media print{.x{color:red}}" );
        ( "a media block",
          "@media screen{.x{color:red}}",
          "@media screen{.x{color:blue}}" );
        ( "a supports condition",
          "@supports (display:grid){.x{color:red}}",
          "@supports (display:flex){.x{color:red}}" );
        ( "a container name",
          "@container a (min-width:10px){.x{color:red}}",
          "@container b (min-width:10px){.x{color:red}}" );
        ( "a container condition",
          "@container a (min-width:10px){.x{color:red}}",
          "@container a (min-width:20px){.x{color:red}}" );
        ("a layer name", "@layer a{.x{color:red}}", "@layer b{.x{color:red}}");
        ( "a nested rule",
          ".a{color:red;&:hover{color:blue}}",
          ".a{color:red;&:focus{color:blue}}" );
        ( "an import condition",
          "@import url(a.css) supports(display:grid);",
          "@import url(a.css) supports(display:flex);" );
        ( "a @property name",
          "@property --x{syntax:\"*\";inherits:false}",
          "@property --y{syntax:\"*\";inherits:false}" );
        (* A registration whose syntax is not [*] has to declare an initial
           value, so the pair below holds one fixed and moves the syntax. *)
        ( "a @property syntax",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property \
           --x{syntax:\"<length-percentage>\";inherits:false;initial-value:0px}"
        );
        ( "a @property initial value",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:1px}"
        );
        ( "a scope end selector",
          "@scope (.a) to (.b){.x{color:red}}",
          "@scope (.a) to (.c){.x{color:red}}" );
      ]
  in
  Alcotest.(check (list string)) "each pair stays two statements" [] folded

(* The pipeline [cascade --minify] runs: the optimiser over a parsed sheet, then
   the minified printer. *)
let optimized_statement css =
  match Css.statements (Css.optimize (Css.of_string_exn css)) with
  | [ s ] -> s
  | l -> Alcotest.failf "%s: expected one statement, got %d" css (List.length l)

let minified_statement s = Css.to_string ~minify:true (Css.v [ s ])

(* The rule [Declaration.hash] already holds, one level up: a statement that
   minifies to the text another statement minifies to has to hash the same, or a
   table keyed on the fingerprint files one CSS statement under two keys. As
   there, the rule is stated over the canonicalised sheet, and equality is the
   finer relation, true for every pair below as well. *)
let same_text_same_hash label a b =
  let text = minified_statement a in
  Alcotest.(check string)
    (label ^ ": one minified text")
    text (minified_statement b);
  Alcotest.(check int)
    (label ^ ": one hash") (Css.hash_statement a) (Css.hash_statement b);
  Alcotest.(check bool)
    (label ^ ": one statement")
    true (Css.equal_statement a b)

let statement_hash_follows_the_minified_text () =
  let pair label a b =
    same_text_same_hash label (optimized_statement a) (optimized_statement b)
  in
  (* Optional whitespace the printer drops. *)
  pair "declaration spacing" ".a{color : red}" ".a{color:red}";
  pair "selector spacing" ".a > .b{color:red}" ".a>.b{color:red}";
  pair "media spacing" "@media screen and (min-width: 10px){.a{color:red}}"
    "@media screen and (min-width:10px){.a{color:red}}";
  pair "layer spacing" "@layer a {.x{color:red}}" "@layer a{.x{color:red}}";
  pair "keyframe spacing" "@keyframes k{from{opacity: 0}}"
    "@keyframes k{from{opacity:0}}";
  (* A zero length keeps no unit under minify. *)
  pair "zero length" ".a{margin:0px}" ".a{margin:0}";
  (* CSS Color 4 sec. 6.1 gives [red] the sRGB bytes 255, 0, 0, so the three
     spellings below are one colour and canonicalise to one node. *)
  pair "a named colour" ".a{color:red}" ".a{color:#f00}";
  pair "the sRGB function" ".a{color:red}" ".a{color:rgb(255 0 0)}";
  (* An [\@supports] condition keeps the token stream the author wrote, and the
     optimiser canonicalises the whitespace in it, so this pair reaches one
     node. *)
  pair "supports condition spacing"
    "@supports(color:rgba(0, 0, 0, .5)){.b{color:red}}"
    "@supports(color:rgba(0,0,0,.5)){.b{color:red}}"

(* A fingerprint that stopped at the statement's shape would satisfy the rule
   above and file every rule of a kind under one key. *)
let statement_hash_reads_the_descriptors () =
  let differs label a b =
    if
      Css.hash_statement (sole_statement a)
      = Css.hash_statement (sole_statement b)
    then Some label
    else None
  in
  let colliding =
    List.filter_map
      (fun (label, a, b) -> differs label a b)
      [
        ( "@font-face descriptors",
          "@font-face{font-family:A;src:url(a.woff2)}",
          "@font-face{font-family:B;src:url(b.woff2)}" );
        ( "@counter-style descriptors",
          "@counter-style c{system:cyclic;symbols:\"a\"}",
          "@counter-style c{system:cyclic;symbols:\"b\"}" );
        ( "@view-transition descriptors",
          "@view-transition{navigation:auto}",
          "@view-transition{navigation:none}" );
        ( "a keyframe selector",
          "@keyframes k{from{opacity:0}}",
          "@keyframes k{to{opacity:0}}" );
        ( "a @property syntax",
          "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}",
          "@property \
           --x{syntax:\"<length-percentage>\";inherits:false;initial-value:0px}"
        );
        ( "a @-moz-document condition",
          "@-moz-document domain(\"a.test\"){.x{color:red}}",
          "@-moz-document domain(\"b.test\"){.x{color:red}}" );
      ]
  in
  Alcotest.(check (list string)) "each pair keys apart" [] colliding

(* CSS Conditional 3 sec. 6 answers a declaration feature by handing that exact
   declaration to the browser's own parser, so the value spelled in the
   condition is the question. [calc(10px)] and [10.0px] name grammars a browser
   can accept one of and refuse the other: two guards, not one, and the
   optimiser keeps both spellings. *)
let supports_spelling_stays_two_statements () =
  let calc = optimized_statement "@supports(width:calc(10px)){.b{color:red}}" in
  let plain = optimized_statement "@supports(width:10.0px){.b{color:red}}" in
  Alcotest.(check bool)
    "two texts" false
    (String.equal (minified_statement calc) (minified_statement plain));
  Alcotest.(check bool) "two statements" false (Css.equal_statement calc plain);
  (* The control: the same guard over the same block is one statement, so the
     verdict above is about the value and not about the parse. *)
  let calc' =
    optimized_statement "@supports(width:calc(10px)){.b{color:red}}"
  in
  Alcotest.(check bool)
    "the same guard is one statement" true
    (Css.equal_statement calc calc')

let equality_tests =
  [
    ( "equal_statement separates every shape",
      `Quick,
      statement_equality_separates_every_shape );
    ( "hash_statement agrees with equal_statement",
      `Quick,
      statement_hash_agrees_with_equality );
    ( "equal_statement reads the payload",
      `Quick,
      statement_equality_reads_the_payload );
    ( "hash_statement follows the minified text",
      `Quick,
      statement_hash_follows_the_minified_text );
    ( "hash_statement reads the descriptors",
      `Quick,
      statement_hash_reads_the_descriptors );
    ( "an @supports spelling stays two statements",
      `Quick,
      supports_spelling_stays_two_statements );
  ]

let suite =
  ( "stylesheet",
    stylesheet_tests @ additional_tests @ walker_tests @ deep_walker_tests
    @ render_tests @ equality_tests )
