(** Helpers shared by the stylesheet test suites. *)

open Cascade
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
