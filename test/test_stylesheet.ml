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
    (fun fmt d ->
      Format.pp_print_string fmt (Css.Declaration.string_of_declaration d))
    ( = )

let check_import_rule =
  check_value_cursor "import_rule" read_import_rule pp_import_rule

let check_declaration =
  check_value_cursor "declaration" Css.Declaration.read_declaration
    (Css.Pp.option Css.Declaration.pp_declaration)

let check_stylesheet =
  check_value_cursor "stylesheet" read_stylesheet pp_stylesheet

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
  neg_cursor read_stylesheet "{color:red}";
  (* Missing selector *)
  neg_cursor read_stylesheet ".btn";
  (* Missing declarations. CSS Syntax sec. 5.3.7 auto-closes [.btn{] so it is
     spec-valid and not asserted here. *)
  neg_cursor read_stylesheet ".btn{color}";
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
  neg_cursor read_stylesheet "@charset 'UTF-8'" (* Wrong charset quotes *)

let string_of_stylesheet s = Css.Stylesheet.pp ~minify:true s

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
  let output = Css.Stylesheet.pp ~minify:true sheet in
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
  let output = Css.Stylesheet.pp ~minify:true sheet in
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
  let output = Css.Stylesheet.pp ~minify:true sheet in
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
  let output = Css.Stylesheet.pp ~minify:true sheet in
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
  let layer_stmt = layer ~name:"utilities" [ statement_of_rule rule ] in
  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  (* Both paths for the same node. minify (pp) does the shortest same-node
     spelling: Hex ff0000 -> #f00. minify+optimize cross-folds to the shortest
     node: Hex ff0000 -> red (Named, shorter). *)
  Alcotest.(check string)
    "layer rule creation (minify)"
    "@layer utilities{.red{background-color:#f00}}"
    (Css.Stylesheet.pp ~minify:true sheet);
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
  let empty = empty_stylesheet in
  Alcotest.(check int) "empty layers" 0 (List.length (layers empty));
  Alcotest.(check int) "empty rules" 0 (List.length (rules empty));
  Alcotest.(check int) "empty media" 0 (List.length (media_queries empty));
  Alcotest.(check int)
    "empty container" 0
    (List.length (container_queries empty))

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
  let output = Css.Stylesheet.pp ~minify:true sheet in

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
  let output = Css.Stylesheet.pp ~minify:true sheet in
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
    let _ = read_stylesheet r in
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
  let layer_stmt = layer ~name:"utilities" [ statement_of_rule rule_obj ] in

  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true sheet in
  Alcotest.(check string)
    "layer pp" "@layer utilities{.blue{color:#00f}}" output;

  (* Test empty layer - per CSS spec, empty @layer statements end with
     semicolon *)
  let empty_layer = layer ~name:"base" [] in
  let empty_sheet = Css.Stylesheet.v [ empty_layer ] in
  let empty_output = Css.Stylesheet.pp ~minify:true empty_sheet in
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

  let output = Css.Stylesheet.pp ~minify:true sheet in
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
  neg_cursor read_import_rule
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
  neg_cursor read_stylesheet "@namespace { url(http://example.test); }";
  neg_cursor read_stylesheet "@namespace svg;"

(** Test [@keyframes] rules *)
let keyframes_case () =
  (* CSS Animations 1 section 7.1: [from] and [to] are spec-equivalent to [0%]
     and [100%]; under [~minify:true] the printer canonicalizes to the shorter
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
  neg_cursor read_stylesheet "@keyframes missing-block"

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
      "@font-face{font-family:emoji;src:url(emoji.woff2);unicode-range:U+1F600-1F64F,U+???}"
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
     keeps the rest of the @font-face, like browsers (CSS Fonts 4 sec. 11.2). *)
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
     maybe; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-variant: \
     common-ligatures no-common-ligatures; }";
  (* A descending font-stretch range is kept like the font-weight / oblique
     ranges below: browsers do not enforce CSS Fonts 4 sec. 11.2. *)
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
  check_stylesheet ~expected:"@page invoice:blank:first{margin:1cm}"
    "@page invoice:blank:first { margin: 1cm }";
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
  (* CSS Animations 1 section 7.1: [from] / [to] / [0%] / [100%] are pairwise
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

let spec_page_margin_descriptor_matrix () =
  check_stylesheet
    ~expected:
      "@page chapter:right{size:letter \
       landscape;margin:1in;@right-top{content:counter(page)}@bottom-center{content:\"Chapter\"}}"
    "@page chapter:right { size: letter landscape; margin: 1in; @right-top { \
     content: counter(page) } @bottom-center { content: \"Chapter\" } }";
  List.iter
    (neg_cursor read_stylesheet)
    [ "@page { @top-center { display: block } }" ];
  check_stylesheet ~expected:"@page:first:left{margin:1cm}"
    "@page :first:left { margin: 1cm }";
  check_stylesheet ~expected:"@page:blank:first{margin:.5cm}"
    "@page :blank:first { margin: 0.5cm }"

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
  List.iter
    (neg_cursor read_stylesheet)
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
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule" 1 (List.length rules);
  let rule = List.hd rules in
  let decls = declarations rule in
  Alcotest.(check int) "rule has two declarations" 2 (List.length decls)

(* Not a roundtrip test *)
let test_read_stylesheet_multiple_rules () =
  let css = ".btn { color: red; } .card { margin: 5px; }" in
  let reader = Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has two rules" 2 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_empty () =
  let css = "" in
  let reader = Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "empty stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_whitespace_only () =
  let css = "   \n\t  " in
  let reader = Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int)
    "whitespace-only stylesheet has no rules" 0 (List.length rules)

(* Not a roundtrip test *)
let test_read_stylesheet_with_comments () =
  let css = "/* comment */ .btn { color: red; } /* another comment */" in
  let reader = Cursor.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule despite comments" 1 (List.length rules)

let string_of_strict_error e = Cascade.Error.to_string e

let strict_accept name css =
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      let strict_output = minify parsed.stylesheet in
      let { Css.stylesheet; warnings } =
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
      let { Css.stylesheet; warnings } =
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

let lenient_recover name css expected min_warnings =
  let { Css.stylesheet; warnings } =
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
      ("unterminated block auto-closes at EOF", ".x { color: red");
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
      ( "scope with end boundary",
        "@scope (.card) to (.footer) { .title { color: red } }" );
      ( "font-face wildcard unicode range",
        "@font-face { font-family: Icons; src: url(icons.woff2); \
         unicode-range: U+4?? }" );
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
      (* Browsers keep a descending font-weight / oblique-angle range, so the
         lenient parse keeps it with a warning; strict turns that into an error
         (CSS Fonts 4 sec. 11.2 wants the first bound <= the second). *)
      ( "font-face descending font-weight range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
         900 100 }" );
      ( "font-face descending oblique angle range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-style: \
         oblique 20deg 10deg }" );
      ( "font-face descending font-stretch range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
         200% 50% }" );
      ( "font-face invalid font-display list",
        "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
         block swap }" );
      ( "font-palette missing base-palette",
        "@font-palette-values --brand { override-colors: 0 red }" );
      ( "counter-style missing system",
        "@counter-style thumbs { symbols: \"*\" }" );
      ( "counter-style cyclic missing symbols",
        "@counter-style thumbs { system: cyclic }" );
      ( "page margin invalid declaration",
        "@page { @top-left { display: block } }" );
      ("keyframes invalid selector", "@keyframes fade { 50px { opacity: 0 } }");
      ("keyframes forbidden name none", "@keyframes none { to { opacity: 1 } }");
      ( "keyframes forbidden css-wide name",
        "@keyframes initial { to { opacity: 1 } }" );
      (* Conditional query grammars. *)
      ( "media ungrouped mixed boolean operators",
        "@media (width) and (height) or (color) { .x { color: red } }" );
      ( "media bad range interval",
        "@media (30em < width > 60em) { .x { color: red } }" );
      ("media dangling not", "@media not { .x { color: red } }");
      ("media dangling and", "@media screen and { .x { color: red } }");
      ("media missing range value", "@media (width >= ) { .x { color: red } }");
      ("container empty style query", "@container style() { .x { color: red } }");
      ( "container empty scroll-state query",
        "@container scroll-state() { .x { color: red } }" );
      ( "supports empty selector function",
        "@supports selector() { .x { color: red } }" );
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
  lenient_recover "unknown at-rule skipped"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    ".ok{color:#00f}" 1;
  lenient_recover "bad selector list drops rule only"
    ".ok { color: green } .bad:not() { color: red } .next { color: blue }"
    ".ok{color:green}.next{color:#00f}" 1;
  lenient_recover "unclosed block auto-closes" ".btn { color: red;"
    ".btn{color:red}" 0

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
    ( "spec strict accepts valid stylesheets",
      `Quick,
      spec_strict_accepts_valid_stylesheets );
    ( "spec strict rejects invalid stylesheets",
      `Quick,
      spec_strict_rejects_invalid_stylesheets );
    ( "spec lenient recovery stylesheets",
      `Quick,
      spec_lenient_recovery_stylesheets );
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
    let _ = read_stylesheet r in
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
  (* CSS Syntax sec. 5.3.7: unclosed blocks auto-close at EOF and the inner
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
    | Ok { Css.stylesheet; warnings = _ } ->
        Alcotest.(check string)
          (name ^ " stylesheet") expected
          (minify stylesheet |> String.trim)
    | Error err ->
        Alcotest.failf "%s should parse strictly: %s" name
          (Cascade.Error.to_string err)
  in
  let check_recovery name css expected min_warnings =
    let { Css.stylesheet; warnings } =
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
    ".ok{color:#00f}" 1

let css_syntax_recovery_structural () =
  let declaration_counts stylesheet =
    Css.rule_statements stylesheet
    |> List.map (fun statement ->
        match Css.as_rule statement with
        | Some (_, declarations, _) -> List.length declarations
        | None -> Alcotest.fail "expected recovered qualified rule")
  in
  let check_counts name css expected_counts min_warnings =
    let { Css.stylesheet; warnings } =
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
  let parse input = read_stylesheet (Cursor.of_string input) in
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
      "@layer reset.type{strong{font-weight:700}}@layer \
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
      "@layer reset.type{strong{font-weight:700}}@layer \
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
      let r = Cursor.of_string input in
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
  neg_cursor read_stylesheet
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
  neg_cursor read_stylesheet "@container style() { .card { color: red } }";
  neg_cursor read_stylesheet
    "@container scroll-state() { .card { color: red } }";
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
      ( "@font-palette-values --dark{font-family:Color \
         Font,Brand;base-palette:dark}",
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
      "@container card (inline-size>30em){.card{grid-template-columns:subgrid}}"
    "@container card (inline-size > 30em) { .card { grid-template-columns: \
     subgrid } }";
  let oklch_support =
    "@supports (color: oklch(50% 0.1 20)) { .accent { color: oklch(50% 0.1 20) \
     } }"
  in
  check_stylesheet
    ~expected:"@supports(color:oklch(50%.1 20)){.accent{color:oklch(50%.1 20)}}"
    oklch_support;
  assert_minify_and_optimize oklch_support
    ~minified:"@supports(color:oklch(50%.1 20)){.accent{color:oklch(50%.1 20)}}"
    ~optimized:".accent{color:#944a4b}";
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
  neg_cursor read_stylesheet ".card { & { & { color: red } } }";
  neg_cursor read_stylesheet "@scope () { .x { color: red } }";
  neg_cursor read_stylesheet "@scope (.x) to () { .x { color: red } }";
  neg_cursor read_stylesheet "@starting-style;"

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

(* CSS Cascade Module Level 6, section 6.4.4.2 (The Layer Statement Rule): the
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

(* CSS Cascade Module Level 6, section 6.4.3 (Nesting Layers): a dotted layer
   name is shorthand for nested layer blocks. The spec says [@layer foo.bar {
   ... }] declares the same nested layer as [@layer foo { @layer bar { ... } }],
   so the rule placed inside either form must end up in the same effective
   cascade layer named [foo.bar]. *)
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
    (inner_rule_text (Css.layer_block "foo.bar" dotted))
    (inner_rule_text (Css.layer_block "foo.bar" nested));
  Alcotest.(check (list string))
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

(* CSS Values and Units Module Level 4, section 6.1 (Distance Units): "A 0
   length is unitless and may be substituted for 0px or 0em (etc.) in any
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

(* CSS Color Module Level 4, section 12.1 (Hex Notation): a 6-digit hex color
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

(* CSS Cascade and Inheritance Module Level 6, section 6.1 (Cascade Sorting
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

(* CSS Color Module Level 4, section 1.4 (Notational Conventions): the named
   color [red], the hex notations [#f00] and [#ff0000], and the rgb function
   [rgb(255, 0, 0)] all denote the same sRGB color. Under industry-standard
   minification (cssnano / Lightning CSS / clean-css) the printer canonicalizes
   to the shortest equivalent form, so all of these resolve to the same
   serialized output. *)
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

(* CSS Values and Units Module Level 4, section 6.5 only allows dropping units
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

(* CSSOM Level 1, section 6.6.2 (Serialize a CSS declaration): the serialized
   form puts ":" between property name and value with no surrounding spaces in
   minified mode, and "!important" follows the value with no extra whitespace.
   The property name is serialized as-is (already lowercased by the syntax layer
   per CSS Syntax sec. 3.3) regardless of input case. *)
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

(* CSS Color Module Level 4, section 6.4 (The "transparent" color keyword): the
   [transparent] keyword is defined as equivalent to [rgba(0, 0, 0, 0)]. The
   8-digit hex [#00000000] (or its 4-digit shorthand [#0000]) is also equivalent
   to that fully-transparent black per section 12.1. The optimizer may pick any
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

(* CSS Values and Units Module Level 4, section 8.1 (Numbers and Numeric Data
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

(* CSS Selectors Module Level 4, section 3.5 (The Universal Selector): "If the
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

(* CSS Color Module Level 4, section 3 (Color Syntax): the [<hue>] component
   accepts angles or numbers and is normalised modulo 360deg. So [hsl(360 100%
   50%)] denotes the same color as [hsl(0 100% 50%)] and as the named color
   [red]; [hsl(720 ...)] is also red. Hue values outside [0, 360) must reduce,
   and the fully-saturated red hue must canonicalize to the named color under
   industry-standard minification. *)
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

(* CSS Color Module Level 4, section 1.3 (Color Component Values): an alpha
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

(* CSS Animations Module Level 1, section 7.1 (Keyframe Selectors): the keyframe
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

(* CSS Color Module Level 4, section 1.3 (Color Component Values): an alpha
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

(* CSS Values and Units Module Level 4, section 8.1 (Numbers): [<number>] tokens
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

(* CSS Fonts Module Level 4, section 5.1.2 (Common Weight Name Mapping): the
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

(* CSS Color Module Level 4, section 6.4 (The transparent keyword): all forms of
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

(* CSS Values and Units Module Level 4, section 6.5 lets zero lengths drop their
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

(* CSS Backgrounds and Borders Module Level 3, section 5 (Border Radius):
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

(* CSS Values and Units Module Level 4, section 10 (Mathematical Expressions):
   [calc(<dimension> + 0)] simplifies to [<dimension>] because adding zero is
   the identity in that dimension. The spec permits the implementation to
   simplify - cssnano takes the freedom; the shortest spec-equivalent form is
   the bare dimension. *)
let v410_calc_add_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 0) simplifies to 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px + 0) }");
  Alcotest.(check string)
    "calc(0 + 1px) simplifies to 1px" ".x{width:1px}"
    (normalize ".x { width: calc(0 + 1px) }");
  Alcotest.(check string)
    "calc(1px + 0px) simplifies to 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px + 0px) }")

(* CSS Backgrounds and Borders Module Level 3, section 3.6
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

(* CSS Color 4 section 12.1 + cascade convention: authored hex colours decode to
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
    ~optimized:".x{color:red}"

(* CSS Color 4 section 1.4 + cascade convention: under non-minified output, the
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

(* CSS Animations 1 section 7.1 + cascade convention: [from] / [to] are
   canonicalized to [0%] / [100%] only under minify; the pretty printer keeps
   the source keyword. *)
let fidelity_keyframe_selector_preserved () =
  pretty_preserves "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"
    [ "from"; "to" ];
  pretty_preserves "@keyframes fade { 0% { opacity: 0 } 100% { opacity: 1 } }"
    [ "0%"; "100%" ]

(* CSS Selectors 4 section 3.5 + cascade convention: stripping [*] in a
   non-solitary compound is a minify-only optimization; the pretty printer keeps
   the universal selector as written. *)
let fidelity_universal_in_compound_preserved () =
  pretty_preserves "*.foo { color: red }" [ "*.foo" ];
  pretty_preserves "*#main { color: red }" [ "*#main" ];
  pretty_preserves "*[data-x] { color: red }" [ "*[data-x]" ]

(* CSS Color 4 section 1.3 + cascade convention: alpha unit form is preserved in
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

(* CSS Values 4 section 6.1 + cascade convention: dropping the unit on a zero
   length is a minify-only optimization; the pretty printer keeps the source
   spelling. *)
let fidelity_zero_length_preserved () =
  pretty_preserves ".x { width: 0px }" [ "0px" ];
  pretty_preserves ".x { width: 0em }" [ "0em" ];
  pretty_preserves ".x { width: 0% }" [ "0%" ];
  pretty_preserves ".x { margin: 0px 0px 0px 0px }" [ "0px 0px 0px 0px" ]

(* CSS Animations 1 section 7.1 + cascade convention: shorthand collapses
   ([margin: 1px 1px 1px 1px] -> [margin: 1px], [border-radius: 0 0 0 0] -> [0])
   are minify-only optimizations; the pretty printer keeps the source
   spelling. *)
let fidelity_shorthand_form_preserved () =
  pretty_preserves ".x { margin: 1px 1px 1px 1px }" [ "1px 1px 1px 1px" ];
  pretty_preserves ".x { padding: 1px 2px 1px 2px }" [ "1px 2px 1px 2px" ];
  pretty_preserves ".x { border-radius: 0 0 0 0 }" [ "0 0 0 0" ];
  pretty_preserves ".x { background-position: 50% 50% }" [ "50% 50%" ]

(* CSS Fonts 4 section 5.1.2 + cascade convention: mapping the [normal] / [bold]
   keywords to the numeric weights [400] / [700] is a minify-only optimization;
   the pretty printer keeps the keyword. *)
let fidelity_font_weight_keyword_preserved () =
  pretty_preserves ".x { font-weight: normal }" [ "normal" ];
  pretty_preserves ".x { font-weight: bold }" [ "bold" ]

(* CSS Selectors Level 4, section 14 (The :nth-child() Pseudo-class): the
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

(* CSS Selectors Level 4, section 6.2 (Attribute selectors): the value in
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

(* CSS Values and Units Module Level 4, section 8.1 (Numbers): scientific
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

(* CSS Values and Units Module Level 4, section 8.1 (Numbers): negative zero is
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

(* CSS Values and Units Module Level 4, section 7 (URLs): the [<url>] type
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

(* CSS Cascading and Inheritance Module Level 6, section 6.1 (Cascade Sorting
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

(* CSS Cascading and Inheritance Module Level 6, section 6.1 (Cascade Sorting
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

(* CSS Cascading and Inheritance Module Level 6, section 6.1: an empty
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

(* CSS Cascading and Inheritance Module Level 6, section 7 (CSS-Wide Keywords):
   the keywords [initial], [inherit], [unset], [revert], and [revert-layer] are
   valid for every property. Implementations must preserve them through
   serialization since they have observable cascade semantics that no shorter
   spelling captures. *)
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

(* CSS Cascading and Inheritance Module Level 6, section 3.2 (The all
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

(* CSS Cascading and Inheritance Module Level 6, section 6.4 (Cascade Layers):
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

(* CSS Cascade L6 section 2.4 (Conditional @import) and CSS Custom Properties L1
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

(* CSS Cascade L6 section 6.4.4 (anonymous @layer): two anonymous layers are
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

(* CSS Backgrounds and Borders Module Level 3, section 3.6
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
  (* Per CSS Backgrounds L3 sec. 3.6 [top left], [left top], and [0% 0%] all
     denote position [0 0]. *)
  Alcotest.(check string)
    "background-position: top left -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: top left }");
  Alcotest.(check string)
    "background-position: left top -> 0 0" ".x{background-position:0 0}"
    (normalize ".x { background-position: left top }");
  Alcotest.(check string)
    "background-position: bottom right -> 100% 100%"
    ".x{background-position:100% 100%}"
    (normalize ".x { background-position: bottom right }")

(* CSS Cascade L6 section 6.1: when two rules with different selectors share the
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

(* CSS Syntax L3 section 4.3.10 (Identifier escapes): vendor-prefixed properties
   such as [-webkit-transform] and [-moz-user-select] use the dashed-ident
   escape hatch and are unknown to the CSS spec. The printer must round-trip
   them unchanged - both Lightning CSS and cssnano keep vendor prefixes. *)
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

(* CSS Values and Units Module Level 4, section 6.6 (Time Units): [s] and [ms]
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

(* CSS Values and Units Module Level 4, section 6.1 (Distance Units): absolute
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

(* CSS Values and Units Module Level 4, section 8.1 (Numbers): negative lengths
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

(* CSS Values and Units Module Level 4, section 8 (Numeric Data Types): trailing
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

(* CSS Sizing Module Level 4, section 5 (aspect-ratio): the [aspect-ratio]
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

(* CSS Values and Units Module Level 4, section 8.1 + spec rejection: a bare
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

(* CSS Values and Units Module Level 4, section 6.6 + spec rejection: a bare
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

(* CSS Values and Units Module Level 4, section 6.1 + spec rejection: unknown
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

(* CSS Values and Units Module Level 4, section 10.2 (Computed Value of calc()):
   calc() simplifies under spec rules - a single-operand calc collapses to the
   operand; multiplication/division/addition with constants in the same unit
   fold to the result; calc() with a [var()] reference must be preserved because
   the value is unknown at parse time. Both Lightning CSS and cssnano agree on
   these simplifications. *)
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
    "calc(0 + 0) -> 0" ".x{width:0}"
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

(* CSS Values and Units Module Level 4, section 10.2: calc() with mixed units
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

(* CSS Values L4 section 10.2: nested calc() collapses to a single calc() and
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

(* CSS Values L4 section 10.7 (min(), max()): when all arguments are constants
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

(* CSS Selectors L4 section 17 (:is()): a single-argument [:is(.x)] matches the
   same elements as bare [.x] with the same specificity. Per shortest-wins
   cascade picks the unwrapped form. *)
let s417_is_unwrap () =
  (* CSS Selectors L4 sec. 17: a single-argument [:is(.a)] is spec-equivalent to
     bare [.a] (same match set, same specificity). Under [~minify:true] the
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

(* CSS Selectors L4 section 6.2: attribute compound selectors drop quotes per
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

(* CSS Selectors L4 section 4.2 (compound selector): chained pseudo- classes
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

(* CSS Transforms L1 section 11: multiple transform functions stack in source
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
   sec. 10.7 identities ([x * 1], [x + 0], ...) still fold: they hold for every
   possible substitution because the [var()] stays inside calc()'s grammar, so
   [calc(var(--x) * 1)] shortens to [calc(var(--x))] (not to bare [var(--x)],
   which sec. 10.10 forbids without knowing the value). *)
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
    "calc var plus zero folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) + 0px) }");
  Alcotest.(check string)
    "calc zero plus var folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(0px + var(--spacing)) }");
  Alcotest.(check string)
    "calc var minus zero folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) - 0px) }");
  Alcotest.(check string)
    "calc nested var identities fold, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc((var(--spacing) * 1) + 0px) }");
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

(* CSS Custom Properties L1 section 5 (Resolving Dependency Cycles): a variable
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

(* CSS Conditional Rules Module Level 4, section 2 (The @supports rule): the
   rule body parses as a rule list (like a stylesheet) and round- trips
   preserved. The supports-condition grammar accepts declarations, [not], [and],
   [or], [selector()], and [font-tech()]. *)
let conditional4_2_supports_preserved () =
  let normalize ?(enforce_spec = false) css =
    match Css.of_string ~strict:false css with
    | Ok parsed ->
        parsed.stylesheet |> Css.optimize ~enforce_spec
        |> Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let grid = "@supports (display: grid) { .x { display: grid } }" in
  Alcotest.(check string)
    "default minify elides baseline @supports" ".x{display:grid}"
    (normalize grid);
  Alcotest.(check string)
    "enforce-spec preserves baseline @supports"
    "@supports(display:grid){.x{display:grid}}"
    (normalize ~enforce_spec:true grid);
  let not_grid = "@supports not (display: grid) { .x { display: block } }" in
  Alcotest.(check string)
    "default minify drops negated baseline @supports" "" (normalize not_grid);
  Alcotest.(check string)
    "enforce-spec preserves negated baseline @supports"
    "@supports not (display:grid){.x{display:block}}"
    (normalize ~enforce_spec:true not_grid);
  Alcotest.(check bool)
    "@supports selector(:has(img)) preserved" true
    (Astring.String.is_infix ~affix:"selector(:has(img))"
       (normalize "@supports selector(:has(img)) { .x { color: red } }"));
  Alcotest.(check bool)
    "enforce-spec preserves boolean @supports" true
    (Astring.String.is_infix ~affix:"and"
       (normalize ~enforce_spec:true
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

(* {2 Container Queries (CSS Containment L3 sec. 6)} *)

(* CSS Containment Module Level 3, section 6 (Container Queries): the
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

(* {2 Font shorthand (CSS Fonts L4 sec. 6.5)} *)

(* CSS Fonts Module Level 4, section 6.5 (Shorthand font property): the [font]
   shorthand expands to several longhands; the keyword [bold] inside the
   shorthand canonicalizes to [700] under minify per sec. 5.1.2. System font
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

(* {2 List-style shorthand (CSS Lists L3 sec. 3)} *)

(* CSS Lists Module Level 3, section 3 (list-style shorthand): the shorthand
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

(* {2 Cascade origin + !important interaction (Cascade L6 sec. 6.3)} *)

(* CSS Cascading and Inheritance Module Level 6, section 6.3 (Importance): for
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
    "calc(0 * 100%) -> 0" ".x{width:0}"
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
(* CSS Scroll Snap 1 section 6.2: [scroll-snap-align] takes one or two
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
    "multiple var() in one value preserved"
    ".x{padding:var(--top) var(--right)}"
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

(* CSS Custom Properties L1 section 2.3: a real per-element dependency cycle
   makes the variables in that cycle invalid at computed-value time. A
   caller-provided theme resolver is not that full computed-value graph, so a
   cycle in resolver output should stop resolution and preserve the authored
   runtime expression at the boundary instead of looping or guessing that the
   fallback is safe to select statically. *)
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

let additional_tests =
  [
    ("check function", `Quick, test_check);
    ("import_rule", `Quick, test_import_rule);
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
    ( "spec CSS Syntax 4.3.1 unknown at-rule prelude separator",
      `Quick,
      s3431_unknown_at_rule_prelude_separator );
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
    ( "spec values 4 10.7 length times math reduction",
      `Quick,
      v4107_math_product_reduction );
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
            match Error.snippet e with
            | None -> Alcotest.fail "expected warning with snippet"
            | Some _ -> ())
        | _ -> Alcotest.fail "expected exactly one warning" );
    ( "partial recovery: Css.of_string ~strict:false entry point",
      `Quick,
      fun () ->
        let { Css.stylesheet; warnings } =
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
              "recovered output" ".a{color:#00f}" (minify parsed.stylesheet)
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
        let { Css.stylesheet; warnings = _ } =
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
        let { Css.stylesheet; warnings } =
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
        (* [@layer base;] parses through section 5.4.2 to an at-rule with [block
           = None]. The replay cursor must still present a terminating [;] to
           [read_layer], otherwise the at-rule is silently dropped. *)
        let { Css.stylesheet; warnings } =
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
        let { Css.stylesheet; warnings } =
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
        let { Css.stylesheet; warnings } =
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
  ]

let suite = ("stylesheet", stylesheet_tests @ additional_tests)
