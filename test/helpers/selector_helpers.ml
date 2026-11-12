open Cascade
open Css.Selector
open Css_test_helpers

let check = check_value_cursor "selector" read pp

(* Helper for checking invalid selectors *)
let check_invalid name exn_msg f =
  check_raises name (Invalid_argument exn_msg) f

let check_construct expected selector =
  check_construct expected (to_string ~minify:true) expected selector

let check_pretty_to expected input =
  let actual = to_string ~minify:false (of_string input) in
  Alcotest.(check string) ("pretty " ^ input) expected actual

let check_spec_tuple name expected actual =
  Alcotest.(check int) (name ^ " ids") expected.ids actual.ids;
  Alcotest.(check int) (name ^ " classes") expected.classes actual.classes;
  Alcotest.(check int) (name ^ " elements") expected.elements actual.elements

(* Extra minifier invariant: reparse and preserve specificity. *)
let check_minified_to expected input =
  let original = of_string input in
  let minified = to_string ~minify:true original in
  Alcotest.(check string) ("minify " ^ input) expected minified;
  let reparsed = of_string minified in
  let expected_ast = of_string expected in
  Alcotest.(check bool)
    ("minified selector reparses to expected AST: " ^ input)
    true
    (equal reparsed expected_ast);
  check_spec_tuple
    ("specificity preserved: " ^ input)
    (specificity original) (specificity reparsed)

(* Extra minifier invariant: minify is idempotent (a second pass produces no
   further change), and specificity is preserved. Strict AST equality between
   [original] and [reparsed] does not hold for inputs the minifier rewrites
   (e.g. CSS Selectors 4 sec. 5.2 lets the printer drop a redundant [*] from a
   compound), so we compare the minified form against itself after re-parsing
   and re-minifying. *)
let check_minified_equiv input =
  let original = of_string input in
  let minified = to_string ~minify:true original in
  let reparsed = of_string minified in
  let reminified = to_string ~minify:true reparsed in
  Alcotest.(check string)
    ("minified selector is idempotent: " ^ input)
    minified reminified;
  check_spec_tuple
    ("specificity preserved: " ^ input)
    (specificity original) (specificity reparsed)
