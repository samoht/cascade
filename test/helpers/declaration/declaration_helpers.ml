(** Helpers shared by the declaration test suites. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers

(* One-liner check functions for each type *)
let check_declaration ?minify ?roundtrip ?expected ?optimized input =
  check_value_cursor "declaration" read_declaration (Css.Pp.option pp) ?minify
    ?roundtrip ?expected input;
  match optimized with
  | None -> ()
  | Some into ->
      (* held = the pp-minified declaration (the [expected] held form, or the
         already-minified [input]); just-minify must hold it, optimize folds. *)
      decl_optimizes_to ~held:(Option.value ~default:input expected) ~into input

let check = check_declaration

let check_specified_value name css expected =
  let decl = Css.Declaration.of_string css in
  Alcotest.(check string)
    name expected
    (Css.Declaration.string_of_value ~minify:false decl)

let check_declarations input expected_count =
  let r = Cursor.of_string input in
  let decls = read_declarations r in
  Alcotest.check int
    (Fmt.str "declarations count %s" input)
    expected_count (List.length decls);
  decls
