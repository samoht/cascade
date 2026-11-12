(** Stylesheet tests: rule and at-rule construction, reading and printing. *)

open Cascade
module Selector = Css.Selector
open Css.Stylesheet
open Css_test_helpers
open Stylesheet_helpers

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

(* An [initial-value] carrying var(), attr() or env() has nothing to substitute
   from at registration time, so Chrome 153 drops the
   whole rule at every syntax, the universal one included. The values below the
   substitution cases are the other side of the same filter.

   Sec. 4.1's computational independence is the second filter, and Thomas
   settled on 2026-09-09 that cascade follows Chrome here as it does for the
   substitution half: a non-universal syntax registers its initial value before
   any element exists, so a length that resolves against one cannot be computed.
   Chrome 153 refuses the font-relative units of CSS Values 4 sec. 5.1.1 and the
   container-relative ones of sec. 5.1.4 there, and takes sec. 5.1.3's viewport
   lengths and sec. 5.2's absolute ones, which resolve against the viewport and
   against nothing. The universal syntax stores the value as written and keeps
   every unit. *)
(* Not a roundtrip test *)
let test_property_initial_value_substitution () =
  let expect_error what syntax value =
    expect_property_error what
      (String.concat ""
         [
           "@property --x { syntax: \"";
           syntax;
           "\"; inherits: false; initial-value: ";
           value;
           " }";
         ])
  in
  let substitutions =
    [
      "var(--y)";
      "var(--y, 1px)";
      "attr(data-x)";
      "attr(data-x type(<length>))";
      "env(safe-area-inset-top)";
      "env(--x, 1px)";
      "calc(var(--y) + 1px)";
    ]
  in
  List.iter
    (fun value ->
      expect_error "substitution at <length>" "<length>" value;
      expect_error "substitution at universal" "*" value)
    substitutions;
  (* A substitution function nested in the fallback of another one still counts,
     and a [var(] written inside a string is data rather than a reference. *)
  expect_error "nested substitution" "*" "var(--y, env(safe-area-inset-top))";
  check_stylesheet
    ~expected:
      "@property --x{syntax:\"*\";inherits:false;initial-value:\"var(--y)\"}"
    "@property --x { syntax: \"*\"; inherits: false; initial-value: \
     \"var(--y)\" }";
  check_stylesheet
    ~expected:
      "@property --x{syntax:\"<length>\";inherits:false;initial-value:5px}"
    "@property --x { syntax: \"<length>\"; inherits: false; initial-value: 5px \
     }";
  List.iter
    (fun unit ->
      expect_error "element-relative unit at <length>" "<length>"
        (String.concat "" [ "3"; unit ]))
    [ "em"; "rem"; "ex"; "cap"; "ch"; "ic"; "lh"; "rlh"; "cqw"; "cqi"; "cqmax" ];
  expect_error "element-relative unit inside a calc" "<length>"
    "calc(1px + 2em)";
  expect_error "element-relative unit at <length-percentage>"
    "<length-percentage>" "3em";
  List.iter
    (fun value ->
      check_stylesheet
        ~expected:
          (String.concat ""
             [
               "@property --x{syntax:\"<length>\";inherits:false;initial-value:";
               value;
               "}";
             ])
        (String.concat ""
           [
             "@property --x { syntax: \"<length>\"; inherits: false; \
              initial-value: ";
             value;
             " }";
           ]))
    [ "3vw"; "3vmin"; "3dvw"; "calc(1px + 2px)" ];
  check_stylesheet
    ~expected:"@property --x{syntax:\"*\";inherits:false;initial-value:3em}"
    "@property --x { syntax: \"*\"; inherits: false; initial-value: 3em }";
  check_stylesheet
    ~expected:"@property --x{syntax:\"*\";inherits:false;initial-value:red}"
    "@property --x { syntax: \"*\"; inherits: false; initial-value: red }"

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
       700;font-style:oblique 0deg 10deg;font-stretch:75% \
       125%;font-display:optional;unicode-range:U+25-FF}"
    "@font-face { font-family: Brand; src: local(\"Brand\"), \
     url(\"brand.woff2\") format(\"woff2\") tech(variations); font-weight: 400 \
     700; font-style: oblique 0deg 10deg; font-stretch: 75% 125%; \
     font-display: optional; unicode-range: U+0025-00FF; }";
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
  (* CSS Fonts 4 (ED) sec. 4.4 writes the font property descriptors' grammars
     out in full, and sec. 4.6 gives the settings descriptors the corresponding
     property's values "except that the CSS-wide keywords are omitted". No
     descriptor grammar takes one, so the declaration goes and the rest of the
     rule stays. The descriptors below delegate to the property readers, which
     take them legitimately in property position. *)
  List.iter
    (fun keyword ->
      List.iter
        (fun descriptor ->
          check_stylesheet
            ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
            (String.concat ""
               [
                 "@font-face { font-family: Brand; src: url(font.woff2); ";
                 descriptor;
                 ": ";
                 keyword;
                 "; }";
               ]))
        [
          "font-style";
          "font-weight";
          "font-stretch";
          "font-feature-settings";
          "font-variation-settings";
          "font-variant";
          "font-display";
          "size-adjust";
          "ascent-override";
          "descent-override";
          "line-gap-override";
          "unicode-range";
        ])
    [ "inherit"; "initial"; "unset"; "revert"; "revert-layer" ];
  (* CSS Fonts 4 (ED) sec. 4.4 opens each of the three font property descriptors
     with [auto] and gives it as their initial value, so a variable font is
     asked for its own range rather than told one. Chrome 153 keeps all
     three. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-style:auto}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: auto; }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-weight:auto}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: auto; \
     }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-stretch:auto}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     auto; }";
  (* [auto] is the whole value: sec. 4.4 puts it outside the {1,2} range, so it
     pairs with nothing. *)
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: auto \
     400; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: normal \
     auto; }";
  (* sec. 4.4 spells the font-style descriptor [auto | normal | italic | left |
     right | oblique [<angle>{1,2}]?], so the only range it grants is a pair of
     oblique angles. Two keywords are two values of a grammar that takes one,
     and Chrome 153 drops them. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-style:oblique \
       0deg 10deg}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: \
     oblique 0deg 10deg; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: normal \
     italic; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-style: italic \
     oblique; }";
  (* sec. 4.4 writes the weight descriptor [auto | <font-weight-absolute>{1,2}]
     over [<font-weight-absolute> = normal | bold | <number [1,1000]>], so the
     relative keywords the property takes are not values here: there is no
     inherited weight for them to be relative to. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:Brand;src:url(font.woff2);font-weight:normal \
       bold}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
     normal bold; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
     lighter; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
     bolder; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: 400 \
     lighter; }";
  (* Sec. 4.6 gives these descriptors the property's values "except that the
     CSS-wide keywords are omitted", and an endpoint of the range is one of
     those values, so a keyword is no more a value there than it is alone. The
     one alone is already refused; a pair of them, or one beside a real
     endpoint, was written back as a declaration Chrome 153 drops. *)
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
     inherit inherit; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: 400 \
     inherit; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
     initial unset; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     inherit inherit; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     normal inherit; }";
  check_stylesheet ~expected:"@font-face{font-family:Brand;src:url(font.woff2)}"
    "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
     inherit normal; }";
  (* sec. 4.2 and 4.3 make font-family and src required, so a CSS-wide keyword
     in either costs the whole rule the way any other missing one does. *)
  check_stylesheet ~expected:""
    "@font-face { font-family: inherit; src: url(font.woff2); }";
  check_stylesheet ~expected:"" "@font-face { font-family: Brand; src: unset; }";
  (* Sec. 2.1.1 turns a generic family name away where it could be read as the
     keyword rather than as a name, which is where the name starts: [Foo serif]
     and [Cambria Math] are installed fonts and [serif Foo] is not. The
     [font-family] property reads them that way; the descriptor held every word
     of the sequence to the whole exclusion list, so it dropped a real font
     name. Chrome 153 reads the first two at both and drops the third at
     both. *)
  check_stylesheet
    ~expected:"@font-face{font-family:\"Cambria Math\";src:url(font.woff2)}"
    "@font-face { font-family: Cambria Math; src: url(font.woff2); }";
  check_stylesheet
    ~expected:"@font-face{font-family:\"Foo serif\";src:url(font.woff2)}"
    "@font-face { font-family: Foo serif; src: url(font.woff2); }";
  check_stylesheet ~expected:""
    "@font-face { font-family: serif Foo; src: url(font.woff2); }";
  (* A family name that merely starts with one is a name, not a keyword: sec.
     2.1.1 asks only that a bare identifier not BE a CSS-wide keyword. *)
  check_stylesheet
    ~expected:"@font-face{font-family:inherited Sans;src:url(font.woff2)}"
    "@font-face { font-family: inherited Sans; src: url(font.woff2); }";
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
  (* CSS Paged Media 3 sec. 4.2 writes the prelude as [<page-selector>#], whose
     item is a name, one or more pseudo-pages, or both. An item with neither is
     no selector, so a lone comma and a trailing one both invalidate the rule,
     which Chrome 151 drops. The prelude may be absent altogether. *)
  assert_minify_and_optimize "@page , { margin: 1in; }" ~minified:""
    ~optimized:"";
  assert_minify_and_optimize "@page foo, { margin: 1in; }" ~minified:""
    ~optimized:"";
  check_stylesheet ~expected:"@page foo{margin:1in}"
    "@page foo { margin: 1in; }";
  (* CSS Syntax 3 (ED) sec. 5.5.5 discards a [;] among a block's contents, so a
     stray one costs nothing and the rule after it reads. Chrome 151 reads it as
     the start of a prelude and loses the block instead. *)
  check_stylesheet ~expected:"@layer{a{color:red}}"
    "@layer { ; a { color: red } }";
  check_stylesheet ~expected:"@media all{a{color:red}}"
    "@media all { ; a { color: red } }";
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

let suite =
  ( "stylesheet",
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
      ( "property initial-value substitution",
        `Quick,
        test_property_initial_value_substitution );
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
      ( "spec keyframes shadow colour var",
        `Quick,
        spec_keyframes_shadow_color_var );
      ("page", `Quick, page_case);
      ("page margin edges", `Quick, page_margin_edges);
      ( "spec page margin descriptor matrix",
        `Quick,
        spec_page_margin_descriptor_matrix );
      ("spec page context properties", `Quick, spec_page_context_properties);
      ( "spec page descriptor importance",
        `Quick,
        spec_page_descriptor_importance );
      ("spec page margin box empty", `Quick, spec_page_margin_box_empty);
      ("property rule edges", `Quick, property_rule_edges);
      ( "spec property descriptor matrix",
        `Quick,
        spec_property_descriptor_matrix );
      ("sheet_item", `Quick, sheet_item_case);
      ("ordering", `Quick, ordering);
      (* CSS parsing tests *)
      ("read basic", `Quick, test_read_stylesheet_basic);
      ("read multiple rules", `Quick, test_read_stylesheet_multiple_rules);
      ("read empty", `Quick, test_read_stylesheet_empty);
      ("read whitespace only", `Quick, test_read_stylesheet_whitespace_only);
      ("read with comments", `Quick, test_read_stylesheet_with_comments);
    ] )
