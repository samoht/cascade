(** Tests for CSS Values parsing *)

open Alcotest
open Css.Values

let check_value type_name reader pp_func ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let result = reader t in
  let pp_str = Css.Pp.to_string pp_func result in
  check string (Fmt.str "%s %s" type_name input) expected pp_str

let check_length = check_value "length" read_length pp_length
let check_color = check_value "color" read_color pp_color
let check_angle = check_value "angle" read_angle pp_angle
let check_duration = check_value "duration" read_duration pp_duration
let check_percentage = check_value "percentage" read_percentage pp_percentage
let check_calc = check_value "calc" (read_calc read_length) (pp_calc pp_length)

let test_length_parsing () =
  (* Basic units *)
  check_length "10px";
  check_length ~expected:"0" "0px";
  check_length "-10px";
  check_length "2.5rem";
  check_length "0.5em";
  check_length "-1.5em";
  check_length "100%";
  check_length ~expected:"0" "0%";
  check_length "-50%";

  (* Viewport units *)
  check_length "50vh";
  check_length "100vw";

  (* Container units *)
  check_length "1ch";
  check_length "2lh";

  (* Keywords *)
  check_length "auto";
  check_length "inherit";
  check_length "max-content";
  check_length "min-content";
  check_length "fit-content";
  check_length "from-font";

  (* Edge cases *)
  check_length "0";
  check_length ~expected:"0.5rem" ".5rem";
  check_length "999999px";
  check_length "-999999px";
  check_length "0.000001em";
  check_length "0.0000001rem";
  check_length "999999999px";
  check_length "-999px";
  check_length ".5px"

let test_color_parsing () =
  (* Hex colors with # *)
  check_color "#fff";
  check_color "#FFF";
  check_color "#000";
  check_color "#123";
  check_color "#abc";
  check_color "#ABC";
  check_color "#123456";
  check_color "#abcdef";
  check_color "#ABCDEF";
  check_color "#000000";
  check_color "#ffffff";
  check_color "#FFFFFF";
  (* Additional named colors *)
  check_color "rebeccapurple";
  check_color "aliceblue";

  (* Named colors - all variants *)
  check_color "red";
  check_color "blue";
  check_color "green";
  check_color "white";
  check_color "black";
  check_color "yellow";
  check_color "cyan";
  check_color "magenta";
  check_color "gray";
  check_color "grey";
  check_color "orange";
  check_color "purple";
  check_color "pink";
  check_color "silver";
  check_color "maroon";
  check_color "fuchsia";
  check_color "lime";
  check_color "olive";
  check_color "navy";
  check_color "teal";
  check_color "aqua";

  (* Special keywords *)
  check_color "transparent";
  check_color "currentcolor";
  check_color "inherit";

  (* RGB functions - various formats *)
  check_color ~expected:"rgb(255 0 0)" "rgb(255, 0, 0)";
  check_color ~expected:"rgb(0 0 0)" "rgb(0, 0, 0)";
  check_color ~expected:"rgb(255 255 255)" "rgb(255, 255, 255)";
  check_color ~expected:"rgb(128 128 128)" "rgb(128, 128, 128)";

  (* RGBA with alpha *)
  check_color ~expected:"rgb(255 0 0 / 0.5)" "rgba(255, 0, 0, 0.5)";
  check_color ~expected:"rgb(255 0 0 / 0)" "rgba(255, 0, 0, 0)";
  check_color ~expected:"rgb(255 0 0 / 1)" "rgba(255, 0, 0, 1)";
  check_color ~expected:"rgb(0 0 0 / 0.25)" "rgba(0, 0, 0, 0.25)";
  check_color ~expected:"rgb(128 128 128 / 0.75)" "rgba(128, 128, 128, 0.75)"

let test_angle_parsing () =
  (* Degrees *)
  check_angle "45deg";
  check_angle "0deg";
  check_angle "360deg";
  check_angle "-45deg";
  check_angle "90.5deg";
  check_angle ~expected:"0.5deg" ".5deg";

  (* Radians *)
  check_angle "1.5rad";
  check_angle "0rad";
  check_angle "3.14159rad";
  check_angle "-1.5rad";

  (* Turns *)
  check_angle "0.25turn";
  check_angle "0turn";
  check_angle "1turn";
  check_angle "-0.5turn";
  check_angle "2.5turn";

  (* Gradians *)
  check_angle "100grad";
  check_angle "0grad";
  check_angle "400grad";
  check_angle "-200grad";

  (* Edge cases *)
  check_angle "999999deg";
  check_angle "-360deg";
  check_angle ~expected:"0.25deg" ".25deg"

let test_duration_parsing () =
  (* Seconds *)
  check_duration "1s";
  check_duration "0s";
  check_duration "0.5s";
  check_duration ~expected:"0.25s" ".25s";
  check_duration "10s";
  check_duration "999s";

  (* Milliseconds *)
  check_duration "500ms";
  check_duration "0ms";
  check_duration "1ms";
  check_duration "1000ms";
  check_duration "50.5ms";
  check_duration "999999ms";
  check_duration ~expected:"0.1s" ".1s"

let test_percentage_parsing () =
  check_percentage "50%";
  check_percentage "100%";
  check_percentage "0%";
  check_percentage "12.5%";
  check_percentage "99.99%";
  check_percentage "200%";
  check_percentage "0.01%";
  check_percentage ~expected:"0.5%" ".5%";
  check_percentage "0.0001%";
  check_percentage "-50%";
  check_percentage ~expected:"0.01%" ".01%"

let test_default_units_and_unitless () =
  (* Angle without unit defaults to deg when printed *)
  check_angle ~expected:"90deg" "90";
  (* Duration without unit defaults to ms when printed *)
  check_duration ~expected:"150ms" "150";
  (* Unitless non-zero length is preserved as a number *)
  check_length "1.5"

let test_calc_expressions () =
  let cases =
    [
      (* Basic operations *)
      "calc(100% - 20px)";
      "calc(50vh + 10px)";
      "calc(2em * 3)";
      "calc(100% / 4)";
      (* Nested and associative chains *)
      "calc(10px + 20px + 30px)";
      "calc(100% - 50% - 25%)";
      (* Edge cases with zero *)
      "calc(0px + 10px)";
      "calc(100% - 0px)";
      "calc(0 * 100px)";
      (* Mixed units *)
      "calc(1rem + 2em + 3px)";
      "calc(100vw - 2rem)";
      "calc(50% + 25vw)";
      (* Precedence *)
      "calc(100% - 20px * 0.5)";
      "calc(10px + 5em / 2)";
      "calc(1em * 2 + 3px)";
      "calc(2 * 3px + 4px)";
      "calc(10px / 2 - 1px)";
      (* Parentheses to force grouping *)
      "calc((100% - 20px) / 2)";
      "calc(100% * 0.5 + 10px * 2)";
      "calc(50vh - 10px * 3 + 5rem)";
      "calc((10px + 20px) * 2)";
    ]
  in
  List.iter check_calc cases

let test_var_in_color () =
  let t = Css.Reader.of_string "var(--primary-color)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "primary-color" var.name
  | _ -> fail "Expected Var variant for var() expression"

let test_var_with_fallback () =
  let t = Css.Reader.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "theme-color" var.name
  | _ -> fail "Expected Var for var() expression"

let test_var_with_color_keyword_fallback () =
  let t = Css.Reader.of_string "var(--custom-color, red)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "custom-color" var.name
  | _ -> fail "Expected Var with red fallback"

let test_var_with_rgb_fallback () =
  let t = Css.Reader.of_string "var(--brand-color, rgb(255, 0, 0))" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "brand-color" var.name
  | _ -> fail "Expected Var with rgb fallback"

let test_var_fallback_in_output () =
  let t = Css.Reader.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  let output = Css.Pp.to_string pp_color color in
  check string "var with fallback output" "var(--theme-color, #007bff)" output

let test_var_in_calc_with_fallback () =
  let t = Css.Reader.of_string "calc(100% - var(--gap, 20px))" in
  let calc_expr = read_calc read_length t in
  match calc_expr with
  | Expr (left, Sub, right) -> (
      match (left, right) with
      | Val (Pct p), Var var ->
          Alcotest.(check (float 0.01)) "percentage" 100.0 p;
          check string "var name in calc" "gap" var.name
      | _, _ -> fail "Expected Pct(100) on left and Var on right")
  | _ -> fail "Expected subtraction expression"

let test_var_in_calc () =
  let t = Css.Reader.of_string "calc(100% - var(--spacing))" in
  let calc_expr = read_calc read_length t in
  match calc_expr with
  | Expr (left, Sub, right) -> (
      match (left, right) with
      | Val (Pct p), Var var ->
          Alcotest.(check (float 0.01)) "percentage" 100.0 p;
          check string "var name in calc" "spacing" var.name
      | Val (Pct _), _ -> fail "Expected Var(--spacing) on right"
      | _, _ -> fail "Expected Pct(100) on left and Var on right")
  | _ -> fail "Expected subtraction expression"

let test_minified_value_formatting () =
  (* Leading zero drop in minified output for values *)
  let s = Css.Pp.to_string ~minify:true pp_length (Rem 0.5) in
  check string "minified rem" ".5rem" s;
  let s = Css.Pp.to_string ~minify:true pp_number (Float 0.5) in
  check string "minified number" ".5" s;
  (* Duration formatting remains stable in minified mode *)
  let s = Css.Pp.to_string ~minify:true pp_duration (Ms 500) in
  check string "minified ms" "500ms" s;
  (* Zero stays zero without unit *)
  let s = Css.Pp.to_string ~minify:true pp_length Zero in
  check string "minified zero" "0" s

let test_regular_value_formatting () =
  let s = Css.Pp.to_string pp_length (Rem 0.5) in
  check string "regular rem keeps 0" "0.5rem" s;
  let s = Css.Pp.to_string pp_number (Float 0.5) in
  check string "regular number keeps 0" "0.5" s;
  let s = Css.Pp.to_string pp_number (Int 10) in
  check string "regular int" "10" s

let test_var_parsing_and_printing () =
  (* Test var() parsing and printing *)
  check_color "var(--primary-color, #007bff)";

  (* Test var in length context *)
  check_length "var(--spacing, 10px)"

let test_var_default_inline () =
  (* When inline printing is enabled and a default is present, pp_var should
     inline the default value instead of var(). *)
  let v : length var = var_ref ~default:(Px 10.) "spacing" in
  let len : length = Var v in
  let s = Css.Pp.to_string ~minify:true ~inline:true pp_length len in
  check string "inline var default" "10px" s

let test_float_value_formatting () =
  (* Test float formatting with leading zeros *)
  check_length "0.5rem";
  check_length ~expected:"0.5rem" ".5rem";
  check_length ~expected:"-0.5rem" "-.5rem";

  (* Test with angles *)
  check_angle ~expected:"-0.5turn" "-.5turn"

let test_var_with_multiple_fallbacks () =
  (* Test var() with multiple fallback values - round-trip test *)
  check_color "var(--custom-color, red)";
  check_length "var(--custom-size, 10px)";
  check_angle "var(--custom-angle, 45deg)";
  check_duration "var(--custom-time, 1s)"

let test_calc_with_other_types () =
  (* Test calc with angles *)
  let check_calc_angle =
    check_value "calc_angle" (read_calc read_angle) (pp_calc pp_angle)
  in

  check_calc_angle "calc(180deg + 0.5turn)";
  check_calc_angle "calc(90deg * 2)";
  check_calc_angle "calc(360deg / 4)";

  (* Test calc with durations *)
  let check_calc_duration =
    check_value "calc_duration" (read_calc read_duration) (pp_calc pp_duration)
  in

  check_calc_duration "calc(1s + 500ms)";
  check_calc_duration "calc(2s - 500ms)";
  check_calc_duration "calc(100ms * 10)";

  (* Test calc with percentages *)
  let check_calc_percentage =
    check_value "calc_percentage"
      (read_calc read_percentage)
      (pp_calc pp_percentage)
  in

  check_calc_percentage "calc(50% + 25%)";
  check_calc_percentage "calc(100% / 2)";
  check_calc_percentage "calc(25% * 3)"

let suite =
  [
    ( "values",
      [
        test_case "parse lengths" `Quick test_length_parsing;
        test_case "parse colors" `Quick test_color_parsing;
        test_case "parse angles" `Quick test_angle_parsing;
        test_case "parse durations" `Quick test_duration_parsing;
        test_case "parse percentages" `Quick test_percentage_parsing;
        test_case "default units and unitless" `Quick
          test_default_units_and_unitless;
        test_case "calc expressions" `Quick test_calc_expressions;
        test_case "var() in color context" `Quick test_var_in_color;
        test_case "var() with fallback" `Quick test_var_with_fallback;
        test_case "var() with color keyword fallback" `Quick
          test_var_with_color_keyword_fallback;
        test_case "var() with rgb fallback" `Quick test_var_with_rgb_fallback;
        test_case "var() fallback in output" `Quick test_var_fallback_in_output;
        test_case "var() in calc with fallback" `Quick
          test_var_in_calc_with_fallback;
        test_case "var() in calc expressions" `Quick test_var_in_calc;
        (* Additional value tests *)
        test_case "var parsing and printing" `Quick
          test_var_parsing_and_printing;
        test_case "var default inline" `Quick test_var_default_inline;
        test_case "float value formatting" `Quick test_float_value_formatting;
        test_case "minified value formatting" `Quick
          test_minified_value_formatting;
        test_case "regular value formatting" `Quick
          test_regular_value_formatting;
        test_case "var with multiple fallbacks" `Quick
          test_var_with_multiple_fallbacks;
        test_case "calc with other types" `Quick test_calc_with_other_types;
      ] );
  ]
