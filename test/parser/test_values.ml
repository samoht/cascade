(** Tests for CSS Values parsing *)

open Alcotest

(** Helper module for converting CSS values to strings *)
module To_string = struct
  let length v = Css.Pp.to_string Css.pp_length v
  let color v = Css.Pp.to_string Css.pp_color v
  let angle v = Css.Pp.to_string Css.pp_angle v
  let duration v = Css.Pp.to_string Css.pp_duration v
  let calc pp_value v = Css.Pp.to_string (Css.pp_calc pp_value) v
end

let parse_and_pp parser pp s =
  let t = Css_parser.Reader.of_string s in
  pp (parser t)

let test_length_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_length t in
    let pp_str = To_string.length result in
    check string (Fmt.str "length %s" input) expected pp_str
  in

  test "10px" "10px";
  test "2.5rem" "2.5rem";
  test "100%" "100%";
  test "50vh" "50vh";
  test "auto" "auto";
  test "0" "0";
  test "max-content" "max-content";
  test "min-content" "min-content";
  test "fit-content" "fit-content"

let test_color_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_color t in
    let pp_str = To_string.color result in
    check string (Fmt.str "color %s" input) expected pp_str
  in

  test "#fff" "#fff";
  test "#123456" "#123456";
  test "red" "red";
  test "blue" "blue";
  test "transparent" "transparent";
  test "currentcolor" "currentcolor";
  test "rgb(255, 0, 0)" "rgb(255 0 0)";
  test "rgba(255, 0, 0, 0.5)" "rgb(255 0 0 / 0.5)"

let test_angle_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_angle t in
    let pp_str = To_string.angle result in
    check string (Fmt.str "angle %s" input) expected pp_str
  in

  test "45deg" "45deg";
  test "1.5rad" "1.5rad";
  test "0.25turn" "0.25turn";
  test "100grad" "100grad"

let test_duration_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_duration t in
    let pp_str = To_string.duration result in
    check string (Fmt.str "duration %s" input) expected pp_str
  in

  test "1s" "1s";
  test "500ms" "500ms";
  test "0.5s" "0.5s"

let test_percentage_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_percentage t in
    check (float 0.01) (Fmt.str "percentage %s" input) expected result
  in

  test "50%" 50.0;
  test "100%" 100.0;
  test "12.5%" 12.5

let test_calc_parsing () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let calc_expr = Css_parser.Values.read_calc t in
    let result = Css.Calc calc_expr in
    let pp_str = To_string.length result in
    check string (Fmt.str "calc %s" input) expected pp_str
  in

  test "calc(100% - 20px)" "calc(100% - 20px)";
  test "calc(50vh + 10px)" "calc(50vh + 10px)";
  test "calc(2em * 3)" "calc(2em * 3)"

let test_round_trip_length () =
  let round_trip input =
    let t = Css_parser.Reader.of_string input in
    let parsed = Css_parser.Values.read_length t in
    let output = To_string.length parsed in
    let t2 = Css_parser.Reader.of_string output in
    let reparsed = Css_parser.Values.read_length t2 in
    let output2 = To_string.length reparsed in
    check string (Fmt.str "round-trip %s" input) output output2
  in

  round_trip "10px";
  round_trip "2.5rem";
  round_trip "100%";
  round_trip "auto"

let test_round_trip_color () =
  let round_trip input =
    let t = Css_parser.Reader.of_string input in
    let parsed = Css_parser.Values.read_color t in
    let output = To_string.color parsed in
    let t2 = Css_parser.Reader.of_string output in
    let reparsed = Css_parser.Values.read_color t2 in
    let output2 = To_string.color reparsed in
    check string (Fmt.str "round-trip %s" input) output output2
  in

  round_trip "red";
  round_trip "#fff";
  round_trip "transparent"

let test_var_in_color () =
  (* Test parsing var() in color context *)
  let t = Css_parser.Reader.of_string "var(--primary-color)" in
  let color = Css_parser.Values.read_color t in
  (* Should return a Var variant with the variable name *)
  match color with
  | Css.Var var -> check string "var name" "primary-color" (Css.var_name var)
  | _ -> fail "Expected Var variant for var() expression"

let test_var_with_fallback () =
  (* Test parsing var() with fallback value *)
  let t = Css_parser.Reader.of_string "var(--theme-color, #007bff)" in
  let color = Css_parser.Values.read_color t in
  match color with
  | Css.Var var -> check string "var name" "theme-color" (Css.var_name var)
  (* TODO: Once fallback is implemented, should verify the fallback is parsed *)
  (* For now, we just verify the variable name is correct *)
  | _ -> fail "Expected Var for var() expression"

let test_var_with_color_keyword_fallback () =
  (* Test var() with color keyword as fallback *)
  let t = Css_parser.Reader.of_string "var(--custom-color, red)" in
  let color = Css_parser.Values.read_color t in
  match color with
  | Css.Var var -> check string "var name" "custom-color" (Css.var_name var)
  (* TODO: Should parse and store fallback value 'red' *)
  | _ -> fail "Expected Var with red fallback"

let test_var_with_rgb_fallback () =
  (* Test var() with rgb() function as fallback *)
  let t = Css_parser.Reader.of_string "var(--brand-color, rgb(255, 0, 0))" in
  let color = Css_parser.Values.read_color t in
  match color with
  | Css.Var var -> check string "var name" "brand-color" (Css.var_name var)
  (* Fallback should be parsed and stored *)
  | _ -> fail "Expected Var with rgb fallback"

let test_var_fallback_in_output () =
  (* Test that fallback values are preserved in the output *)
  let t = Css_parser.Reader.of_string "var(--theme-color, #007bff)" in
  let color = Css_parser.Values.read_color t in
  let output = To_string.color color in
  (* The output should include the fallback value *)
  check string "var with fallback output" "var(--theme-color, #007bff)" output

let test_var_in_calc_with_fallback () =
  (* Test var() in calc with fallback value *)
  let t = Css_parser.Reader.of_string "calc(100% - var(--gap, 20px))" in
  let calc_expr = Css_parser.Values.read_calc t in
  match calc_expr with
  | Css.Expr (left, Css.Sub, right) -> (
      match (left, right) with
      | Css.Val (Css.Pct p), Css.Var var ->
          check (float 0.01) "percentage" 100.0 p;
          check string "var name in calc" "gap" (Css.var_name var)
          (* TODO: Should parse and store fallback value 20px *)
      | _, _ -> fail "Expected Pct(100) on left and Var on right")
  | _ -> fail "Expected subtraction expression"

let test_var_in_calc () =
  (* Test parsing var() in calc expressions *)
  let t = Css_parser.Reader.of_string "calc(100% - var(--spacing))" in
  let calc_expr = Css_parser.Values.read_calc t in
  (* Should create Expr(Val(Pct 100), Sub, Var(--spacing)) *)
  match calc_expr with
  | Css.Expr (left, Css.Sub, right) -> (
      match (left, right) with
      | Css.Val (Css.Pct p), Css.Var var ->
          check (float 0.01) "percentage" 100.0 p;
          check string "var name in calc" "spacing" (Css.var_name var)
      | Css.Val (Css.Pct _), _ -> fail "Expected Var(--spacing) on right"
      | _, _ -> fail "Expected Pct(100) on left and Var on right")
  | _ -> fail "Expected subtraction expression"

let test_additional_named_colors () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let result = Css_parser.Values.read_color t in
    let pp_str = To_string.color result in
    check string (Fmt.str "color %s" input) expected pp_str
  in

  (* Test additional named colors *)
  test "orange" "orange";
  test "pink" "pink";
  test "silver" "silver";
  test "maroon" "maroon";
  test "navy" "navy"

let test_calc_operations () =
  let test input expected =
    let t = Css_parser.Reader.of_string input in
    let calc_expr = Css_parser.Values.read_calc t in
    let result = Css.Calc calc_expr in
    let pp_str = To_string.length result in
    check string (Fmt.str "calc %s" input) expected pp_str
  in

  (* Test operator precedence and associations *)
  test "calc(100% - 20px * 0.5)" "calc(100% - 20px * 0.5)";
  test "calc(10px + 5em / 2)" "calc(10px + 5em / 2)";
  test "calc(1em * 2 + 3px)" "calc(1em * 2 + 3px)"

let tests =
  [
    test_case "parse lengths" `Quick test_length_parsing;
    test_case "parse colors" `Quick test_color_parsing;
    test_case "parse angles" `Quick test_angle_parsing;
    test_case "parse durations" `Quick test_duration_parsing;
    test_case "parse percentages" `Quick test_percentage_parsing;
    test_case "parse calc expressions" `Quick test_calc_parsing;
    test_case "round-trip lengths" `Quick test_round_trip_length;
    test_case "round-trip colors" `Quick test_round_trip_color;
    test_case "additional named colors" `Quick test_additional_named_colors;
    test_case "calc operations" `Quick test_calc_operations;
    test_case "var() in color context" `Quick test_var_in_color;
    test_case "var() with fallback" `Quick test_var_with_fallback;
    test_case "var() with color keyword fallback" `Quick
      test_var_with_color_keyword_fallback;
    test_case "var() with rgb fallback" `Quick test_var_with_rgb_fallback;
    test_case "var() fallback in output" `Quick test_var_fallback_in_output;
    test_case "var() in calc with fallback" `Quick
      test_var_in_calc_with_fallback;
    test_case "var() in calc expressions" `Quick test_var_in_calc;
  ]
