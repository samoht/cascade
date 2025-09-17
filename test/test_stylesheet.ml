(** Tests for CSS stylesheet interface types - CSS/MDN spec compliance *)

open Alcotest
module Selector = Css.Selector
open Css.Stylesheet

(* Stylesheet-specific check function *)
let check_stylesheet ?expected input =
  let expected = Option.value ~default:input expected in
  let r = Css.Reader.of_string input in
  let sheet = read_stylesheet r in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  (* Normalize expected for comparison *)
  let r_exp = Css.Reader.of_string expected in
  let sheet_exp = read_stylesheet r_exp in
  let normalized_expected =
    Css.Stylesheet.pp ~minify:true ~newline:false sheet_exp
  in
  check string
    (Fmt.str "stylesheet roundtrip %s" input)
    normalized_expected output;
  (* Roundtrip stability *)
  let r2 = Css.Reader.of_string output in
  let sheet2 = read_stylesheet r2 in
  let output2 = Css.Stylesheet.pp ~minify:true ~newline:false sheet2 in
  check string (Fmt.str "stylesheet double roundtrip %s" input) output output2

(* One-liner check functions for specific statement types *)
let check_rule ?expected input = check_stylesheet ?expected input
let check_import_rule ?expected input = check_stylesheet ?expected input
let check_config ?expected input = check_stylesheet ?expected input

(* Legacy aliases *)
let check = check_stylesheet
let case input expected = check_stylesheet ~expected input

let of_string css =
  try
    let r = Css.Reader.of_string css in
    Ok (read_stylesheet r)
    (* Internal API *)
  with Css.Reader.Parse_error _ -> Error "boom"

let _string_of_rule r = Css.Pp.to_string ~minify:true pp_rule r
let string_of_stylesheet s = Css.Stylesheet.pp ~minify:true ~newline:false s

(* Helper for testing rule construction *)
let check_construct_rule name expected rule =
  let actual = Css.Pp.to_string ~minify:true pp_rule rule in
  Alcotest.check Alcotest.string name expected actual

(* Helper for testing complete stylesheet *)
let check_stylesheet_helper name expected sheet =
  let actual = string_of_stylesheet sheet in
  Alcotest.check Alcotest.string name expected actual

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

let test_media_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt =
    media ~condition:"screen and (min-width: 768px)" [ Rule r ]
  in
  let sheet = Css.Stylesheet.v [ media_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

let test_container_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let container_stmt =
    container ~name:"sidebar" ~condition:"(min-width: 400px)" [ Rule r ]
  in
  let sheet = Css.Stylesheet.v [ container_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

let test_supports_rule_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let supports_stmt =
    supports ~condition:"(display: grid)" [ Css.Stylesheet.Rule r ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

let test_supports_nested_creation () =
  let decl = Css.Declaration.display Css.Properties.Grid in
  let r = rule ~selector:(Selector.class_ "grid") [ decl ] in
  let nested_supports =
    supports ~condition:"(color: red)" [ Css.Stylesheet.Rule r ]
  in
  let supports_stmt =
    supports ~condition:"(display: grid)" [ Rule r; nested_supports ]
  in
  let sheet = Css.Stylesheet.v [ supports_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

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

let test_layer_rule_creation () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let layer_stmt = layer ~name:"utilities" [ Css.Stylesheet.Rule rule ] in
  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output

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

let helper () =
  (* Test complete stylesheet construction and string representation *)
  let decl = Css.Declaration.display Css.Properties.Block in
  let rule = rule ~selector:(Selector.element "div") [ decl ] in
  let sheet = Css.Stylesheet.v [ Css.Stylesheet.Rule rule ] in
  check_stylesheet_helper "simple stylesheet" "div{display:block}" sheet;

  let media_stmt = media ~condition:"print" [ Css.Stylesheet.Rule rule ] in
  let sheet2 = Css.Stylesheet.v [ media_stmt ] in
  check_stylesheet_helper "media stylesheet" "@media print{div{display:block}}"
    sheet2

let test_empty_stylesheet () =
  let empty = empty_stylesheet in
  Alcotest.(check int) "empty layers" 0 (List.length (layers empty));
  Alcotest.(check int) "empty rules" 0 (List.length (rules empty));
  Alcotest.(check int) "empty media" 0 (List.length (media_queries empty));
  Alcotest.(check int)
    "empty container" 0
    (List.length (container_queries empty))

let construction () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let rule = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt = media ~condition:"screen" [ Css.Stylesheet.Rule rule ] in
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
  let media_stmt = media ~condition:"screen" [ Css.Stylesheet.Rule rule ] in

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

(** Test rule read/pp roundtrip *)
let test_rule_roundtrip () =
  (* Test basic rule using stylesheet parsing since read_rule not exposed *)
  let input = ".test { color: red }" in
  check_stylesheet input;

  (* Test rule construction and pp *)
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "0000ff" })
  in
  let constructed = rule ~selector:(Selector.id "main") [ decl ] in
  let pp_output = Css.Pp.to_string ~minify:true pp_rule constructed in
  Alcotest.(check string)
    "constructed rule" "#main{background-color:#0000ff}" pp_output

(** Test media rule read/pp roundtrip *)
let test_media_roundtrip () =
  let input = "@media screen { .test { color: blue } }" in
  check_stylesheet input

(** Test property rule read/pp roundtrip *)
let test_property_roundtrip () =
  let input =
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: \
     red }"
  in
  check_stylesheet input

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
let test_rule_parsing () =
  (* Test basic rule parsing and pretty-printing *)
  check_rule ".class{color:red}";
  check_rule "#id{margin:10px}";
  check_rule "div{padding:5px 10px}";
  check_rule ".a,.b{display:block}";
  check_rule ~expected:"*{box-sizing:border-box}" "* { box-sizing: border-box }";
  check_rule ~expected:"h1{font-size:2rem}" "h1 { font-size: 2rem; }"

let test_media_parsing () =
  (* Parse a few media forms and assert roundtrip via check_stylesheet *)
  check_stylesheet "@media (min-width: 768px) { .a { display: block } }";
  check_stylesheet
    "@media screen and (max-width: 640px){.btn{font-size:.875rem}}"

let test_supports_parsing () =
  (* Parse basic and nested @supports constructs *)
  check_stylesheet "@supports (display: grid) { .grid { display: grid } }";
  check_stylesheet
    "@supports (display: grid){.grid{display:grid}@supports (color: \
     red){.x{color:red}}}"

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
  case
    "@property --x { syntax: \"<length>\"; inherits: true; initial-value: 0px }"
    canonical;

  (* Permutation 2: inherits, syntax, initial-value *)
  case
    "@property --x { inherits: true; syntax: \"<length>\"; initial-value: 0px }"
    canonical;

  (* Permutation 3: initial-value, inherits, syntax *)
  case
    "@property --x { initial-value: 0px; inherits: true; syntax: \"<length>\" }"
    canonical;

  (* Permutation 4: syntax, initial-value, inherits *)
  case
    "@property --x { syntax: \"<length>\"; initial-value: 0px; inherits: true }"
    canonical;

  (* Permutation 5: inherits, initial-value, syntax *)
  case
    "@property --x { inherits: true; initial-value: 0px; syntax: \"<length>\" }"
    canonical;

  (* Permutation 6: initial-value, syntax, inherits *)
  case
    "@property --x { initial-value: 0px; syntax: \"<length>\"; inherits: true }"
    canonical;

  (* Test with only required descriptors in different orders *)
  let minimal = "@property --y{syntax:\"*\";inherits:false}" in

  case "@property --y { syntax: \"*\"; inherits: false }" minimal;
  case "@property --y { inherits: false; syntax: \"*\" }" minimal

(** Negative helper for [@property] parsing errors *)
let expect_property_error name input =
  let r = Css.Reader.of_string input in
  try
    let _ = read_stylesheet r in
    Alcotest.failf "%s: expected parse error" name
  with Css.Reader.Parse_error _ -> ()

(** Test [@property] missing required descriptors *)
let test_property_missing_descriptors () =
  expect_property_error "missing syntax" "@property --x { inherits: true }";
  expect_property_error "missing inherits"
    "@property --x { syntax: \"<color>\" }";
  (* initial-value is required for non-universal syntax *)
  expect_property_error "missing initial-value for <length>"
    "@property --x { syntax: \"<length>\"; inherits: true }";
  (* But initial-value is optional for universal syntax "*" *)
  case "@property --x { syntax: \"*\"; inherits: false }"
    "@property --x{syntax:\"*\";inherits:false}"

(** Test [@property] invalid inherits values *)
let test_property_invalid_inherits () =
  expect_property_error "invalid inherits value"
    "@property --x { syntax: \"*\"; inherits: maybe }"

(** Test [@property] unknown descriptor handling *)
let test_property_unknown_descriptor () =
  expect_property_error "unknown descriptor"
    "@property --x { syntax: \"*\"; inherits: true; unknown: 1 }"

(** Test [@property] duplicate descriptors: last one wins and prints canonically
*)
let test_property_duplicate_descriptors () =
  (* Test duplicate descriptors *)
  case "@property --dup { inherits: true; syntax: \"*\"; inherits: false; }"
    "@property --dup{syntax:\"*\";inherits:false}";
  case
    "@property --color { syntax: \"<color>\"; initial-value: blue; inherits: \
     true; initial-value: red }"
    "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"

(** Test [@property] with comments/whitespace inside the block *)
let test_property_comments_whitespace () =
  (* Test property with comments *)
  case
    "@property --gap { /* allow any */ syntax: \"*\"; /* ws */  inherits: true \
     }"
    "@property --gap{syntax:\"*\";inherits:true}"

(** Test supports rule read/pp roundtrip *)
let test_supports_roundtrip () =
  let input = "@supports (display: grid) { .grid { display: grid } }" in
  check_stylesheet input

(** Test layer pp *)
let test_layer_pp () =
  let decl =
    Css.Declaration.color (Css.Values.Hex { hash = true; value = "0000ff" })
  in
  let rule_obj = rule ~selector:(Selector.class_ "blue") [ decl ] in
  let layer_stmt = layer ~name:"utilities" [ Rule rule_obj ] in

  let sheet = Css.Stylesheet.v [ layer_stmt ] in
  let output = Css.Stylesheet.pp ~minify:true ~newline:false sheet in
  check_stylesheet output;

  (* Test empty layer *)
  let empty_layer = layer ~name:"base" [] in
  let empty_sheet = Css.Stylesheet.v [ empty_layer ] in
  let empty_output =
    Css.Stylesheet.pp ~minify:true ~newline:false empty_sheet
  in
  check_stylesheet empty_output

(** Test keyframes read/pp roundtrip *)
let test_keyframes_roundtrip () =
  let input = "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }" in
  check_stylesheet input

(** Test font-face read/pp roundtrip *)
let test_font_face_roundtrip () =
  let input = "@font-face { font-family: MyFont; src: url(font.woff2); }" in
  check_stylesheet input

(** Test page read/pp roundtrip *)
let test_page_roundtrip () =
  let input = "@page :first { margin: 1in }" in
  check_stylesheet input

(** Test complete stylesheet pp *)
let pp_case () =
  let decl =
    Css.Declaration.background_color
      (Css.Values.Hex { hash = true; value = "ff0000" })
  in
  let r = rule ~selector:(Selector.class_ "red") [ decl ] in
  let media_stmt = media ~condition:"screen" [ Rule r ] in
  let prop =
    property ~syntax:Css.Variables.Color
      ~initial_value:(Css.Values.Named Css.Values.Blue) "--primary"
  in

  let sheet = Css.Stylesheet.v [ Rule r; media_stmt; prop ] in

  let output = Css.Stylesheet.pp ~minify:true sheet in
  check_stylesheet output

(** Test [@charset] rules *)
let charset_case () =
  (* Test charset roundtrip *)
  check_stylesheet "@charset \"UTF-8\";"

(** Test [@import] rules *)
let import_case () =
  (* Test various import forms *)
  check_stylesheet "@import 'styles.css';";
  check_stylesheet "@import url(utilities.css) layer(utilities);";
  check_stylesheet "@import 'print.css' print;"

(** Test [@namespace] rules *)
let namespace_case () =
  (* Test namespace roundtrips *)
  check_stylesheet "@namespace url(http://www.w3.org/1999/xhtml);"

(** Test [@keyframes] rules *)
let keyframes_case () =
  (* Test keyframes roundtrip *)
  check_stylesheet "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  check_stylesheet "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }"

(** Test [@font-face] rules *)
let font_face_case () =
  (* Test font-face roundtrip *)
  check_stylesheet
    "@font-face { font-family: MyCustomFont; src: url('font.woff2'); \
     font-display: swap; }"

(** Test [@page] rules *)
let page_case () =
  (* Test page roundtrip *)
  check_stylesheet "@page { margin: 1in; }";
  check_stylesheet "@page :first { margin: 2in; }"

(** Test sheet_item variants *)
let sheet_item_case () =
  (* Test that we can parse stylesheets with various statement types *)
  let test_statements =
    [
      "@charset \"UTF-8\";";
      "@import 'test.css';";
      ".class { color: red; }";
      "@media print { .class { color: black; } }";
      "@layer base { .btn { padding: 10px; } }";
      "@property --var { syntax: \"*\"; inherits: false; }";
    ]
  in
  List.iter check_stylesheet test_statements

(** Test stylesheet ordering constraints *)
let ordering () =
  (* Per CSS spec, certain at-rules must appear in specific order *)
  (* Test that we can parse stylesheets with at-rules in the correct order *)
  let input =
    "@charset \"UTF-8\";\n@import 'base.css';\n.btn { color: red; }"
  in
  check_stylesheet input

let test_read_stylesheet_basic () =
  let css = ".btn { color: red; padding: 10px; }" in
  let reader = Css.Reader.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule" 1 (List.length rules);
  let rule = List.hd rules in
  let decls = declarations rule in
  Alcotest.(check int) "rule has two declarations" 2 (List.length decls)

let test_read_stylesheet_multiple_rules () =
  let css = ".btn { color: red; } .card { margin: 5px; }" in
  let reader = Css.Reader.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has two rules" 2 (List.length rules)

let test_read_stylesheet_empty () =
  let css = "" in
  let reader = Css.Reader.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "empty stylesheet has no rules" 0 (List.length rules)

let test_read_stylesheet_whitespace_only () =
  let css = "   \n\t  " in
  let reader = Css.Reader.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int)
    "whitespace-only stylesheet has no rules" 0 (List.length rules)

let test_read_stylesheet_with_comments () =
  let css = "/* comment */ .btn { color: red; } /* another comment */" in
  let reader = Css.Reader.of_string css in
  let sheet = read_stylesheet reader in
  let rules = rules sheet in
  Alcotest.(check int) "has one rule despite comments" 1 (List.length rules)

let test_read_stylesheet_error_recovery () =
  (* According to CSS spec, element selectors can be any valid identifier.
     "invalid-css-here .card" is valid CSS (element selector + descendant
     combinator + class). We need actual invalid CSS syntax to test error
     handling. *)
  let css = ".btn { color: red; } { margin: 5px; }" in
  (* Missing selector before { *)
  let reader = Css.Reader.of_string css in
  (* Should fail on invalid CSS without recovery *)
  try
    let _sheet = read_stylesheet reader in
    Alcotest.fail "Expected parsing to fail on invalid CSS"
  with Css.Reader.Parse_error _ ->
    (* This is expected - parsing should fail *)
    ()

let test_of_string () =
  let css = ".btn { color: red; }" in
  match of_string css with
  | Ok sheet ->
      let rules = rules sheet in
      Alcotest.(check int) "of_string works" 1 (List.length rules)
  | Error msg -> Alcotest.fail ("of_string failed: " ^ msg)

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

  check_stylesheet ".btn { color: rgb(300, 300, 300); }";

  check_stylesheet ".btn { color: rgba(255, 0, 0); }";

  check_stylesheet ".btn { color: rgb(50%, 100, 50%); }"

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

  (* Unicode and encoding errors *)
  test_invalid_css ".btn { content: '\\'; }" "incomplete escape sequence";

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
    ("rule creation", `Quick, test_rule_creation);
    ("media rule creation", `Quick, test_media_rule_creation);
    ("container rule creation", `Quick, test_container_rule_creation);
    ("supports rule creation", `Quick, test_supports_rule_creation);
    ("supports nested creation", `Quick, test_supports_nested_creation);
    ("property rule creation", `Quick, test_property_rule_creation);
    ("layer rule creation", `Quick, test_layer_rule_creation);
    ("construct rule helper", `Quick, test_construct_rule_helper);
    ("stylesheet helper", `Quick, helper);
    ("rule parsing", `Quick, test_rule_parsing);
    ("media parsing", `Quick, test_media_parsing);
    ("supports parsing", `Quick, test_supports_parsing);
    ("empty stylesheet", `Quick, test_empty_stylesheet);
    ("stylesheet construction", `Quick, construction);
    ("stylesheet items conversion", `Quick, items_conversion);
    ("concat stylesheets", `Quick, test_concat_stylesheets);
    ("default decl of property rule", `Quick, test_default_property_rule);
    ("property composite syntax", `Quick, test_property_composite_syntax);
    (* Roundtrip tests *)
    ("rule roundtrip", `Quick, test_rule_roundtrip);
    ("media roundtrip", `Quick, test_media_roundtrip);
    ("property roundtrip", `Quick, test_property_roundtrip);
    ("property permutations", `Quick, test_property_permutations);
    ("property missing descriptors", `Quick, test_property_missing_descriptors);
    ("property invalid inherits", `Quick, test_property_invalid_inherits);
    ("property unknown descriptor", `Quick, test_property_unknown_descriptor);
    ( "property duplicate descriptors",
      `Quick,
      test_property_duplicate_descriptors );
    ("property comments/whitespace", `Quick, test_property_comments_whitespace);
    ("supports roundtrip", `Quick, test_supports_roundtrip);
    ("layer pp", `Quick, test_layer_pp);
    ("keyframes roundtrip", `Quick, test_keyframes_roundtrip);
    ("font-face roundtrip", `Quick, test_font_face_roundtrip);
    ("page roundtrip", `Quick, test_page_roundtrip);
    ("stylesheet pp", `Quick, pp_case);
    (* New CSS/MDN spec compliance tests *)
    ("charset", `Quick, charset_case);
    ("import", `Quick, import_case);
    ("namespace", `Quick, namespace_case);
    ("keyframes", `Quick, keyframes_case);
    ("font_face", `Quick, font_face_case);
    ("page", `Quick, page_case);
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
let test_check () =
  (* Test basic stylesheet parsing using check function *)
  check ".test { color: red }";
  check "@media screen { .test { color: blue } }"

let test_import_rule () =
  check_import_rule "@import 'test.css';";
  check_import_rule "@import url('styles.css') screen;"

let test_config () =
  (* Test config parsing - basic cases *)
  check_config "@charset \"UTF-8\";";
  check_config "@import 'test.css';"

(** Additional positive tests *)
let test_advanced_selectors () =
  check_stylesheet ".btn:hover { color: blue; }";
  check_stylesheet ".btn::before { content: 'icon'; }";
  check_stylesheet ".btn[data-type='primary'] { background: blue; }";
  check_stylesheet ".parent > .child { margin: 0; }";
  check_stylesheet ".sibling + .next { padding: 10px; }";
  check_stylesheet ".element ~ .general-sibling { color: red; }"

let test_advanced_properties () =
  check_stylesheet ".box { transform: rotate(45deg) scale(1.2); }";
  check_stylesheet ".grid { display: grid; grid-template-columns: 1fr 2fr; }";
  check_stylesheet ".flex { display: flex; justify-content: space-between; }";
  check_stylesheet ".shadow { box-shadow: 0 4px 8px rgba(0,0,0,0.2); }";
  check_stylesheet
    ".gradient { background: linear-gradient(to right, red, blue); }"

let test_complex_values () =
  check_stylesheet ".calc { width: calc(100% - 20px); }";
  check_stylesheet ".multi { margin: 10px 20px 30px 40px; }";
  check_stylesheet ".var { color: var(--primary-color, blue); }";
  check_stylesheet ".clamp { font-size: clamp(1rem, 2vw, 2rem); }";
  check_stylesheet ".minmax { width: minmax(200px, 1fr); }"

let test_nested_rules () =
  check_stylesheet
    "@media (min-width: 768px) { @supports (display: grid) { .grid { display: \
     grid; } } }";
  check_stylesheet
    "@layer base { @media print { .print-only { display: block; } } }";
  check_stylesheet
    "@container (width > 400px) { @media (orientation: landscape) { .landscape \
     { color: green; } } }"

(** Negative tests for invalid CSS *)
let expect_parse_error input =
  let r = Css.Reader.of_string input in
  try
    let _ = read_stylesheet r in
    Alcotest.failf "Expected parse error for: %s" input
  with Css.Reader.Parse_error _ -> ()

let test_invalid_selectors () =
  expect_parse_error "..double-class { color: red; }";
  expect_parse_error "# { color: red; }";
  expect_parse_error ". { color: red; }";
  expect_parse_error "[invalid-attr { color: red; }";
  expect_parse_error ".class:invalid-pseudo { color: red; }"

let test_invalid_properties () =
  expect_parse_error ".btn { unknown-property: value; }";
  expect_parse_error ".btn { color: invalid-color; }";
  expect_parse_error ".btn { display: invalid-display; }";
  expect_parse_error ".btn { width: 100invalid; }";
  expect_parse_error ".btn { margin: px; }"

let test_invalid_syntax () =
  expect_parse_error ".btn { color: red ";
  expect_parse_error ".btn color: red; }";
  expect_parse_error "{ color: red; }";
  expect_parse_error ".btn { : red; }";
  expect_parse_error ".btn { color red; }"

let test_invalid_at_rules () =
  expect_parse_error "@unknown-rule { .btn { color: red; } }";
  expect_parse_error "@media { .btn { color: red; } }";
  expect_parse_error "@property { syntax: 'color'; inherits: true; }";
  expect_parse_error "@property --var { invalid-descriptor: value; }";
  expect_parse_error "@keyframes { 0% { opacity: 0; } }"

let test_invalid_functions () =
  expect_parse_error ".btn { color: rgb(300); }";
  expect_parse_error ".btn { transform: rotate(); }";
  expect_parse_error ".btn { width: calc(100% +); }";
  expect_parse_error ".btn { background: url(; }"

(* Test layer parsing roundtrip stability *)
let test_layer_roundtrip () =
  let test_css input =
    let r = Css.Reader.of_string input in
    try
      let stylesheet = Css.Stylesheet.read r in
      let roundtrip =
        String.trim
          (Css.Stylesheet.to_string ~minify:true ~header:false stylesheet)
      in
      Alcotest.(check string) ("layer roundtrip for " ^ input) input roundtrip
    with Css.Reader.Parse_error err ->
      Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Css.pp_parse_error err)
  in
  test_css "@layer components,utilities;";
  test_css "@layer components{}@layer utilities{}"

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
    (* Negative tests *)
    ("invalid selectors", `Quick, test_invalid_selectors);
    ("invalid properties", `Quick, test_invalid_properties);
    ("invalid syntax", `Quick, test_invalid_syntax);
    ("invalid at-rules", `Quick, test_invalid_at_rules);
    ("invalid functions", `Quick, test_invalid_functions);
    ("layer roundtrip", `Quick, test_layer_roundtrip);
  ]

let suite = ("stylesheet", stylesheet_tests @ additional_tests)
