(** Tests for CSS pretty-printing module *)

open Alcotest
open Css

(* Helper function for checking pp output *)
let check_pp ?(minify = false) name pp ?expected input =
  let expected = Option.value ~default:input expected in
  let result = Pp.to_string ~minify pp input in
  check string name expected result

let check_pp_minified name pp ?expected input =
  check_pp ~minify:true name pp ?expected input

let check_pp_pretty name pp ?expected input =
  check_pp ~minify:false name pp ?expected input

let check_float ?(minify = false) ?expected input =
  let expected = Option.value ~default:(string_of_float input) expected in
  let result = Pp.to_string ~minify Pp.float input in
  check string (Fmt.str "float %f" input) expected result

let check_float_n ?(minify = false) n ?expected input =
  let expected = Option.value ~default:(string_of_float input) expected in
  let result = Pp.to_string ~minify (Pp.float_n n) input in
  check string (Fmt.str "float_n %d %f" n input) expected result

let test_minified () = check_pp_minified "string output" Pp.string "test"
let test_pretty () = check_pp_pretty "string output" Pp.string "test"

let test_indent () =
  check_pp_pretty "indented string" (Pp.indent Pp.string) ~expected:"  test"
    "test"

let test_block () =
  check_pp_minified "braces" (Pp.braces Pp.string) ~expected:"{content}"
    "content";
  check_pp_pretty "braces pretty" (Pp.braces Pp.string)
    ~expected:"{\n  content\n}" "content"

let test_list () =
  let pp_list = Pp.list ~sep:Pp.comma Pp.string in
  let result = Pp.to_string ~minify:true pp_list [ "a"; "b"; "c" ] in
  check string "comma list" "a,b,c" result;

  let result = Pp.to_string ~minify:false pp_list [ "a"; "b"; "c" ] in
  check string "comma list pretty" "a, b, c" result;

  (* Empty list *)
  let result = Pp.to_string ~minify:true pp_list [] in
  check string "empty list" "" result;

  (* Single item *)
  let result = Pp.to_string ~minify:true pp_list [ "single" ] in
  check string "single item" "single" result

let test_float () =
  (* Basic floats *)
  check_float 3.14159 ~expected:"3.14159";
  check_float 42.0 ~expected:"42";
  check_float 1.0 ~expected:"1";
  check_float (-3.14) ~expected:"-3.14"

let test_float_leading_zero () =
  (* Pretty keeps leading zero *)
  check_float 0.5 ~expected:"0.5";
  check_float 0.25 ~expected:"0.25";
  check_float 0.125 ~expected:"0.125";

  (* Minified drops leading zero *)
  check_float ~minify:true 0.5 ~expected:".5";
  check_float ~minify:true 0.25 ~expected:".25";
  check_float ~minify:true 0.125 ~expected:".125"

let test_float_negative_leading_zero () =
  (* Pretty negative keeps zero *)
  check_float (-0.5) ~expected:"-0.5";
  check_float (-0.25) ~expected:"-0.25";

  (* Minified negative drops zero *)
  check_float ~minify:true (-0.5) ~expected:"-.5";
  check_float ~minify:true (-0.25) ~expected:"-.25"

let test_float_zero_and_nan_inf () =
  (* Zero handling *)
  check_float 0.0 ~expected:"0";
  check_float (-0.0) ~expected:"0";

  (* Infinity clamping *)
  check_float infinity ~expected:"3.40282e38";
  check_float neg_infinity ~expected:"-3.40282e38";

  (* NaN becomes 0 *)
  check_float nan ~expected:"0"

let test_float_rounding_and_trim () =
  (* Rounding with float_n *)
  check_float_n 3 1.23456 ~expected:"1.235";
  (* Round up *)
  check_float_n 3 1.23444 ~expected:"1.234";
  (* Round down *)
  check_float_n 4 1.2300 ~expected:"1.23";
  (* Trim trailing zeros *)
  check_float_n 2 1.995 ~expected:"2";

  (* Round to integer *)

  (* Minified with float_n *)
  check_float_n ~minify:true 3 0.25 ~expected:".25";
  check_float_n ~minify:true 2 0.999 ~expected:"1"

let test_cond () =
  let pp = Pp.cond (fun ctx -> not (Pp.minified ctx)) Pp.string Pp.nop in

  (* Conditional shows in pretty mode *)
  check_pp_pretty "conditional pretty" pp ~expected:"test" "test";

  (* Conditional hidden in minified mode *)
  check_pp_minified "conditional minified" pp ~expected:"" "test"

let test_space_if_pretty () =
  (* No space in minified *)
  let minified = Pp.to_string ~minify:true Pp.space_if_pretty () in
  check string "minified space" "" minified;

  (* Space in pretty *)
  let pretty = Pp.to_string ~minify:false Pp.space_if_pretty () in
  check string "pretty space" " " pretty

let test_combinations () =
  (* Combined formatters *)
  let pp_indented_braces = Pp.indent (Pp.braces Pp.string) in
  check_pp_pretty "indented braces" pp_indented_braces
    ~expected:"  {\n    content\n}" "content";

  (* List with indent *)
  let pp_indented_list = Pp.indent (Pp.list ~sep:Pp.comma Pp.string) in
  let result = Pp.to_string ~minify:false pp_indented_list [ "a"; "b"; "c" ] in
  check string "indented list" "  a, b, c" result;

  (* Nested braces *)
  let pp_nested = Pp.braces (Pp.braces Pp.string) in
  check_pp_minified "nested braces" pp_nested ~expected:"{{inner}}" "inner"

let suite =
  [
    ( "pp",
      [
        Alcotest.test_case "minified output" `Quick test_minified;
        Alcotest.test_case "pretty output" `Quick test_pretty;
        Alcotest.test_case "indentation" `Quick test_indent;
        Alcotest.test_case "block formatting" `Quick test_block;
        Alcotest.test_case "list formatting" `Quick test_list;
        Alcotest.test_case "float formatting" `Quick test_float;
        Alcotest.test_case "float leading zero" `Quick test_float_leading_zero;
        Alcotest.test_case "float negative leading zero" `Quick
          test_float_negative_leading_zero;
        Alcotest.test_case "float zero/nan/inf" `Quick
          test_float_zero_and_nan_inf;
        Alcotest.test_case "float rounding and trim" `Quick
          test_float_rounding_and_trim;
        Alcotest.test_case "conditional output" `Quick test_cond;
        Alcotest.test_case "space_if_pretty" `Quick test_space_if_pretty;
        Alcotest.test_case "combinations" `Quick test_combinations;
      ] );
  ]
