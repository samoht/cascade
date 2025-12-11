(** Tests for CSS Values parsing *)

open Alcotest
open Css.Values
open Test_helpers

(* One-liner check functions for each CSS value type *)
let check_length = check_value "length" read_length pp_length
let check_color = check_value "color" read_color pp_color
let check_angle = check_value "angle" read_angle pp_angle
let check_duration = check_value "duration" read_duration pp_duration
let check_percentage = check_value "percentage" read_percentage pp_percentage
let check_number = check_value "number" read_number pp_number

let check_transition_behavior =
  check_value "transition_behavior" read_transition_behavior
    pp_transition_behavior

let check_length_percentage =
  check_value "length_percentage" read_length_percentage pp_length_percentage

let check_number_percentage =
  check_value "number_percentage" read_number_percentage pp_number_percentage

let check_color_space =
  check_value "color_space" read_color_space pp_color_space

let check_hue = check_value "hue" read_hue pp_hue
let check_color_name = check_value "color_name" read_color_name pp_color_name
let check_alpha = check_value "alpha" read_alpha pp_alpha

let check_hue_interpolation =
  check_value "hue_interpolation" read_hue_interpolation pp_hue_interpolation

let check_calc_op = check_value "calc_op" read_calc_op pp_calc_op
let check_component = check_value "component" read_component pp_component
let check_channel = check_value "channel" read_channel pp_channel
let check_rgb = check_value "rgb" read_rgb pp_rgb

let check_system_color =
  check_value "system_color" read_system_color pp_system_color

let test_length () =
  (* Basic units *)
  check_length "10px";
  check_length ~expected:"0" "0px";
  check_length "-10px";
  check_length "2.5rem";
  check_length ~expected:".5em" "0.5em";
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
  check_length ".5rem";
  check_length "999999px";
  check_length "-999999px";
  check_length ~expected:".000001em" "0.000001em";
  check_length ~expected:".0000001rem" "0.0000001rem";
  check_length "999999999px";
  check_length "-999px";
  check_length ".5px";

  (* Edge cases for very small values *)
  check_length ~expected:".00001px" "0.00001px";
  check_length ~expected:"-.001em" "-0.001em";

  (* Float formatting with lengths *)
  check_length ~expected:".5rem" "0.5rem";
  check_length "-.5rem";

  (* Test var in length context *)
  check_length ~expected:"var(--spacing,10px)" "var(--spacing, 10px)";

  (* Var with length fallback *)
  check_length ~expected:"var(--custom-size,10px)" "var(--custom-size, 10px)";

  (* Nested var in length fallback *)
  check_length ~expected:"var(--gap,var(--gap2,10px))"
    "var(--gap, var(--gap2, 10px))";

  (* Var with empty fallback *)
  check_length ~expected:"var(--size,)" "var(--size,)";

  (* Additional absolute length units *)
  check_length "10cm";
  check_length "10mm";
  check_length "10q";
  check_length "1in";
  check_length "12pt";
  check_length "1pc";

  (* Relative glyph units *)
  check_length "2ex";
  check_length "2cap";
  check_length "2ic";
  check_length "2rlh";

  (* Viewport units *)
  check_length "10vmin";
  check_length "10vmax";
  check_length "10vi";
  check_length "10vb";

  (* Dynamic/Large/Small viewport units *)
  check_length "10dvh";
  check_length "10dvw";
  check_length "10dvmin";
  check_length "10dvmax";
  check_length "10lvh";
  check_length "10lvw";
  check_length "10lvmin";
  check_length "10lvmax";
  check_length "10svh";
  check_length "10svw";
  check_length "10svmin";
  check_length "10svmax";

  (* Zero normalization for new units *)
  check_length ~expected:"0" "0cm";
  check_length ~expected:"0" "0vi";
  check_length ~expected:"0" "0svh";

  (* Calc boundary test for minified printing - expressions should remain
     unchanged *)
  check_length "calc(100% - 0)";
  check_length "calc(10px + 0)";
  check_length "calc(0 + 10px)";

  neg read_length "invalid";
  neg read_length "abc";
  neg read_length "10";
  neg read_length "10pp";
  neg read_length "";
  (* Non-negative length contexts *)
  neg read_non_negative_length "-5px";
  (* Invalid calc expressions *)
  neg (read_calc read_length) "calc()";
  neg (read_calc read_length) "calc(10px +)"

let test_color () =
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

  (* Modern color notations *)
  (* Hue 'deg' unit is default and should be dropped in minified output *)
  check_color ~expected:"hsl(180 50% 25%)" "hsl(180deg 50% 25%)";
  (* Same for hwb hue: drop default 'deg' on minify *)
  check_color ~expected:"hwb(90 10% 20%)" "hwb(90deg 10% 20%)";
  check_color ~expected:"hsl(180 50% 25%/.5)" "hsl(180 50% 25% / 0.5)";
  check_color "hwb(90 10% 20%)";
  check_color ~expected:"hwb(90 10% 20%/.25)" "hwb(90 10% 20% / 0.25)";
  (* Alpha in percent preserves the % format *)
  check_color ~expected:"hsl(180 50% 25%/30%)" "hsl(180deg 50% 25% / 30%)";
  check_color "color(srgb 1 0 0)";
  check_color ~expected:"color(display-p3 .8 .2 .1/.5)"
    "color(display-p3 0.8 0.2 0.1 / 0.5)";
  check_color ~expected:"color(oklab 50% .1 -.05)" "color(oklab 50% 0.1 -0.05)";
  check_color "color(lch 50% 40 120)";
  check_color ~expected:"color(xyz .3 .4 .5)" "color(xyz 0.3 0.4 0.5)";
  (* Additional color functions and forms *)
  check_color ~expected:"oklch(50% .2 30)" "oklch(50% 0.2 30)";
  check_color "rgb(100% 0% 0%)";
  check_color ~expected:"oklab(50% .1 -.05)" "oklab(50% 0.1 -0.05)";
  check_color "lch(50% 40 120)";
  check_color ~expected:"rgb(255 0 0/50%)" "rgb(255 0 0 / 50%)";

  (* Mixed channel formats in modern rgb() syntax *)
  (* Mix percentage and absolute values across channels *)
  check_color "rgb(50% 128 0)";
  check_color "rgb(255 0% 0)";
  check_color "rgb(0 0 50%)";
  (* Mixed channels with alpha (numeric) *)
  check_color ~expected:"rgb(50% 128 0/.5)" "rgb(50% 128 0 / 0.5)";

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
  check_color ~expected:"currentColor" "currentcolor";
  check_color "inherit";

  (* Test var() parsing in color context *)
  check_color "var(--primary-color)";

  (* Var with color fallback *)
  check_color ~expected:"var(--custom-color,red)" "var(--custom-color, red)";

  (* Nested var in color fallback *)
  check_color ~expected:"var(--primary,var(--secondary,red))"
    "var(--primary, var(--secondary, red))";

  (* Var with empty fallback *)
  check_color ~expected:"var(--color,)" "var(--color,)";

  (* Custom properties inline mode tests with complex color fallbacks *)
  check_color ~expected:"var(--theme-primary,hsl(210 75% 50%))"
    "var(--theme-primary, hsl(210deg 75% 50%))";
  check_color ~expected:"var(--accent,rgb(255 0 128/80%))"
    "var(--accent, rgb(255 0 128 / 80%))";

  (* RGB functions - various formats *)
  check_color ~expected:"rgb(255 0 0)" "rgb(255, 0, 0)";
  check_color ~expected:"rgb(0 0 0)" "rgb(0, 0, 0)";
  check_color ~expected:"rgb(255 255 255)" "rgb(255, 255, 255)";
  check_color ~expected:"rgb(128 128 128)" "rgb(128, 128, 128)";

  (* RGBA with alpha *)
  check_color ~expected:"rgb(255 0 0/.5)" "rgba(255, 0, 0, 0.5)";
  check_color ~expected:"rgb(255 0 0/0)" "rgba(255, 0, 0, 0)";
  check_color ~expected:"rgb(255 0 0/1)" "rgba(255, 0, 0, 1)";
  check_color ~expected:"rgb(0 0 0/.25)" "rgba(0, 0, 0, 0.25)";
  check_color ~expected:"rgb(128 128 128/.75)" "rgba(128, 128, 128, 0.75)";
  neg read_color "invalid";
  neg read_color "abc";
  neg read_color "#gg";
  check_color ~expected:"rgb(255 0 0)" "rgb(256, 0, 0)";
  check_color ~expected:"hsl(1 50% 50%)" "hsl(361, 50%, 50%)";
  neg read_color "";
  (* Unknown color keyword *)
  neg read_color "notacolor"

let test_angle () =
  (* Degrees *)
  check_angle "45deg";
  check_angle "0deg";
  check_angle "360deg";
  check_angle "-45deg";
  check_angle "90.5deg";
  check_angle ".5deg";

  (* Radians *)
  check_angle "1.5rad";
  check_angle "0rad";
  check_angle "3.14159rad";
  check_angle "-1.5rad";

  (* Turns *)
  check_angle ~expected:".25turn" "0.25turn";
  check_angle "0turn";
  check_angle "1turn";
  check_angle ~expected:"-.5turn" "-0.5turn";
  check_angle "2.5turn";

  (* Gradians *)
  check_angle "100grad";
  check_angle "0grad";
  check_angle "400grad";
  check_angle "-200grad";

  (* Edge cases *)
  check_angle "999999deg";
  check_angle "-360deg";
  check_angle ".25deg";

  (* Float formatting with angles *)
  check_angle "-.5turn";

  (* Var with angle fallback *)
  check_angle ~expected:"var(--custom-angle,45deg)" "var(--custom-angle, 45deg)";

  (* Var with empty fallback *)
  check_angle ~expected:"var(--angle,)" "var(--angle,)";
  neg read_angle "invalid";
  neg read_angle "45";
  neg read_angle "90";
  neg read_angle "45px";
  neg read_angle "abc";
  neg read_angle "";
  neg read_angle "360.5.5deg"

let test_duration () =
  (* Seconds *)
  check_duration "1s";
  check_duration "0s";
  check_duration ~expected:".5s" "0.5s";
  check_duration ".25s";
  check_duration "10s";
  check_duration "999s";

  (* Milliseconds - normalize to seconds when shorter *)
  check_duration ~expected:".5s" "500ms";
  (* 500ms -> .5s is shorter *)
  check_duration ~expected:"0s" "0ms";
  (* 0ms -> 0s is shorter *)
  check_duration "1ms";
  (* 1ms is shorter than .001s *)
  check_duration ~expected:"1s" "1000ms";
  (* 1000ms -> 1s is shorter *)
  check_duration "50.5ms";
  (* 50.5ms is shorter than .0505s *)
  check_duration ~expected:"999.999s" "999999ms";
  (* 999999ms -> 999.999s *)
  check_duration ".1s";

  (* Durations must have units in CSS *)
  check_duration ~expected:".15s" "150ms";
  (* 150ms -> .15s is shorter *)
  check_duration "1.5s";

  (* Var with duration fallback *)
  check_duration ~expected:"var(--custom-time,1s)" "var(--custom-time, 1s)";

  (* Var with empty fallback *)
  check_duration ~expected:"var(--time,)" "var(--time,)";
  neg read_duration "invalid";
  neg read_duration "1";
  neg read_duration "1px";
  neg read_duration "abc";
  neg read_duration "";
  neg read_duration "-1s";
  neg read_duration "10xs"

let test_percentage () =
  check_percentage "50%";
  check_percentage "100%";
  (* Per CSS spec, 0% can be written as just "0" (CSS Values Level 4) *)
  check_percentage ~expected:"0" "0%";
  check_percentage "12.5%";
  check_percentage "99.99%";
  check_percentage "200%";
  check_percentage ~expected:".01%" "0.01%";
  check_percentage ".5%";
  check_percentage ~expected:".0001%" "0.0001%";
  check_percentage "-50%";
  check_percentage ".01%";
  (* Variables in percentages *)
  check_percentage ~expected:"var(--percentage,50%)" "var(--percentage, 50%)";
  neg read_percentage "invalid";
  neg read_percentage "50";
  neg read_percentage "10";
  neg read_percentage "abc";
  neg read_percentage "";
  neg read_percentage "50px"

(* Not a roundtrip test *)
let test_var_in_color () =
  let t = Css.Reader.of_string "var(--primary-color)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "primary-color" var.name
  | _ -> fail "Expected Var variant for var() expression"

(* Not a roundtrip test *)
let test_var_with_fallback () =
  let t = Css.Reader.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "theme-color" var.name
  | _ -> fail "Expected Var for var() expression"

(* Not a roundtrip test *)
let test_var_color_keyword_fallback () =
  let t = Css.Reader.of_string "var(--custom-color, red)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "custom-color" var.name
  | _ -> fail "Expected Var with red fallback"

(* Not a roundtrip test *)
let test_var_with_rgb_fallback () =
  let t = Css.Reader.of_string "var(--brand-color, rgb(255, 0, 0))" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "brand-color" var.name
  | _ -> fail "Expected Var with rgb fallback"

(* Not a roundtrip test *)
let test_var_fallback_in_output () =
  let t = Css.Reader.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  let output = Css.Pp.to_string pp_color color in
  check string "var with fallback output" "var(--theme-color, #007bff)" output

(* Not a roundtrip test *)
let test_var_in_calc_fallback () =
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

(* Not a roundtrip test *)
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

(* Not a roundtrip test *)
let test_minified_value_formatting () =
  (* Leading zero drop in minified output for values *)
  let s = Css.Pp.to_string ~minify:true pp_length (Rem 0.5) in
  check string "minified rem" ".5rem" s;
  let s = Css.Pp.to_string ~minify:true pp_number (Num 0.5) in
  check string "minified number" ".5" s;
  (* Duration normalizes to shorter form in minified mode *)
  let s = Css.Pp.to_string ~minify:true pp_duration (Ms 500.) in
  check string "minified ms" ".5s" s;
  (* 500ms -> .5s is shorter *)
  (* Zero stays zero without unit *)
  let s = Css.Pp.to_string ~minify:true pp_length Zero in
  check string "minified zero" "0" s

(* Not a roundtrip test *)
let test_regular_value_formatting () =
  let s = Css.Pp.to_string pp_length (Rem 0.5) in
  check string "regular rem keeps 0" "0.5rem" s;
  let s = Css.Pp.to_string pp_number (Num 0.5) in
  check string "regular number keeps 0" "0.5" s;
  let s = Css.Pp.to_string pp_number (Num 10.) in
  check string "regular int" "10" s

(* Not a roundtrip test *)
let test_var_default_inline () =
  (* When inline printing is enabled and a default is present, pp_var should
     inline the default value instead of var(). *)
  let v : length var = var_ref ~default:(Px 10.) "spacing" in
  let len : length = Var v in
  let s = Css.Pp.to_string ~minify:true ~inline:true pp_length len in
  check string "inline var default" "10px" s

(* Not a roundtrip test *)
let test_color_oklch_printing () =
  let open Css.Values in
  let c = oklch 50.0 0.123 30.0 in
  let s = Css.Pp.to_string pp_color c in
  Alcotest.(check string) "oklch printing" "oklch(50% 0.123 30)" s

(* Not a roundtrip test *)
let test_color_mix_printing () =
  let open Css.Values in
  let c1 = rgb 255 0 0 in
  let c2 = rgb 0 0 255 in
  let mix = color_mix ~in_space:Display_p3 ~percent1:30 ~percent2:70 c1 c2 in
  let s = Css.Pp.to_string pp_color mix in
  Alcotest.(check string)
    "color-mix printing"
    "color-mix(in display-p3, rgb(255 0 0) 30%, rgb(0 0 255) 70%)" s

(* Not a roundtrip test *)
let test_var_in_calc_types () =
  let open Css.Values in
  (* Angle var in calc *)
  let t = Css.Reader.of_string "calc(90deg + var(--angle, 0.5turn))" in
  let calc_expr = read_calc read_angle t in
  (match calc_expr with
  | Expr (Val (Deg 90.), Add, Var v) ->
      Alcotest.(check string) "var name" "angle" v.name
  | _ -> Alcotest.fail "Expected angle var in calc");
  (* Duration var in calc *)
  let t = Css.Reader.of_string "calc(1s + var(--dur, 500ms))" in
  let calc_expr = read_calc read_duration t in
  (match calc_expr with
  | Expr (Val (S 1.), Add, Var v) ->
      Alcotest.(check string) "var name" "dur" v.name
  | _ -> Alcotest.fail "Expected duration var in calc");
  (* Percentage var in calc *)
  let t = Css.Reader.of_string "calc(50% + var(--p, 25%))" in
  let calc_expr = read_calc read_percentage t in
  match calc_expr with
  | Expr (Val (Pct 50.), Add, Var v) ->
      Alcotest.(check string) "var name" "p" v.name
  | _ -> Alcotest.fail "Expected percentage var in calc"

(* Not a roundtrip test *)
let test_number_var_printing () =
  let open Css.Values in
  let v : number var = var_ref "scale" in
  let n : number = Var v in
  let s = Css.Pp.to_string pp_number n in
  Alcotest.(check string) "number var printing" "var(--scale)" s

(* Not a roundtrip test *)
let test_var_empty_fallback () =
  let open Css.Values in
  (* Test parsing empty fallback - check it's recognized as Empty *)
  let t = Css.Reader.of_string "var(--test,)" in
  let color = read_color t in
  match color with
  | Var var -> (
      match var.fallback with
      | Empty ->
          (* Success - correctly parsed as Empty *)
          let output = Css.Pp.to_string pp_color color in
          check string "empty fallback output" "var(--test,)" output
      | None -> fail "Expected Empty fallback, got None"
      | Fallback _ -> fail "Expected Empty fallback, got Fallback"
      | Var_fallback _ -> fail "Expected Empty fallback, got Var_fallback")
  | _ -> fail "Expected Var variant"

(* Tests for newly added check functions *)

let test_length_percentage () =
  check_length_percentage "10px";
  check_length_percentage "50%";
  check_length_percentage "0";
  neg read_length_percentage "invalid";
  neg read_length_percentage "abc";
  neg read_length_percentage ""

let test_number_percentage () =
  check_number_percentage "1.5";
  check_number_percentage "50%";
  check_number_percentage "0";
  check_number_percentage "100%";
  (* Variable references *)
  check_number_percentage "var(--my-number)";
  check_number_percentage ~expected:"var(--my-pct,75%)" "var(--my-pct, 75%)";
  check_number_percentage ~expected:"var(--fallback,2)" "var(--fallback, 2.0)";
  (* Calc expressions *)
  check_number_percentage "calc(50% + 25%)";
  check_number_percentage ~expected:"calc(1.5*100%)" "calc(1.5 * 100%)";
  check_number_percentage "calc(100% - 25%)";
  (* Invalid inputs *)
  neg read_number_percentage "invalid";
  neg read_number_percentage "abc";
  neg read_number_percentage ""

let test_color_space () =
  check_color_space "srgb";
  check_color_space "display-p3";
  check_color_space "rec2020";
  neg read_color_space "invalid";
  neg read_color_space "abc";
  neg read_color_space ""

let test_hue () =
  check_hue ~expected:"180" "180deg";
  check_hue ~expected:".5turn" "0.5turn";
  check_hue "200grad";
  check_hue ~expected:"3.14159rad" "3.14159rad";
  neg read_hue "invalid";
  neg read_hue "abc";
  check_hue "180";
  neg read_hue ""

let test_color_name () =
  check_color_name "red";
  check_color_name "blue";
  check_color_name "rebeccapurple";
  neg read_color_name "invalid";
  neg read_color_name "notacolor";
  neg read_color_name "123";
  neg read_color_name ""

let test_alpha () =
  check_alpha ~expected:".5" "0.5";
  check_alpha "50%";
  check_alpha "1";
  check_alpha "0";
  neg read_alpha "invalid";
  neg read_alpha "abc";
  check_alpha ~expected:"1" "1.5";
  check_alpha ~expected:"0" "-0.5";
  check_alpha ~expected:"100%" "150%";
  neg read_alpha "1px"

let test_hue_interpolation () =
  check_hue_interpolation "shorter";
  check_hue_interpolation "longer";
  check_hue_interpolation "increasing";
  check_hue_interpolation "decreasing";
  neg read_hue_interpolation "invalid";
  neg read_hue_interpolation "abc";
  neg read_hue_interpolation ""

let test_calc_op () =
  check_calc_op ~expected:" + " "+";
  check_calc_op ~expected:" - " "-";
  check_calc_op "*";
  check_calc_op "/";
  neg read_calc_op "invalid";
  neg read_calc_op "abc";
  neg read_calc_op "++";
  neg read_calc_op "";
  neg read_calc_op "="

let test_number () =
  check_number "42";
  check_number "3.14";
  check_number "0";
  check_number "-5";
  neg read_number "invalid";
  neg read_number "abc";
  neg read_number "";
  neg read_number "1px"

let test_transition_behavior () =
  check_transition_behavior "normal";
  check_transition_behavior "allow-discrete";
  neg read_transition_behavior "inherit";
  neg read_transition_behavior "invalid";
  neg read_transition_behavior ""

let test_component () =
  (* Component tests - various color component values *)
  check_component "50%";
  check_component "128";
  check_component ~expected:".5" "0.5";
  neg read_component "invalid";
  neg read_component "abc";
  check_component ~expected:"0" "-1";
  (* Clamped in output *)
  check_component ~expected:"255" "256";
  (* Clamped in output *)
  check_component ~expected:"100%" "150%" (* Clamped in output *)

let test_channel () =
  check_channel "255";
  check_channel "50%";
  check_channel ~expected:".5" "0.5";
  neg read_channel "invalid";
  neg read_channel "abc";
  check_channel ~expected:"255" "256";
  check_channel ~expected:"0" "-1";
  check_channel ~expected:"100%" "150%";
  neg read_channel ""

let test_rgb () =
  (* RGB channel values *)
  check_rgb "255 0 0";
  check_rgb "128 128 128";
  check_rgb "0 255 0";
  check_rgb "50% 0% 100%";
  check_rgb "255 50% 0";
  (* RGB with variables *)
  check_rgb "var(--r) 0 0";
  check_rgb "var(--rgb-channels)";
  neg read_rgb "invalid";
  neg read_rgb "abc";
  neg read_rgb "";
  neg read_rgb "255"

let test_system_color () =
  check_system_color "AccentColor";
  check_system_color "CanvasText";
  check_system_color "Highlight";
  check_system_color "ButtonFace";
  check_system_color "Field";
  neg read_system_color "";
  neg read_system_color "invalid-color"

let value_tests =
  [
    test_case "system_color" `Quick test_system_color;
    test_case "length" `Quick test_length;
    test_case "color" `Quick test_color;
    test_case "angle" `Quick test_angle;
    test_case "duration" `Quick test_duration;
    test_case "percentage" `Quick test_percentage;
    test_case "var() in color context" `Quick test_var_in_color;
    test_case "var() with fallback" `Quick test_var_with_fallback;
    test_case "var() with color keyword fallback" `Quick
      test_var_color_keyword_fallback;
    test_case "var() with rgb fallback" `Quick test_var_with_rgb_fallback;
    test_case "var() fallback in output" `Quick test_var_fallback_in_output;
    test_case "var() in calc with fallback" `Quick test_var_in_calc_fallback;
    test_case "var() in calc expressions" `Quick test_var_in_calc;
    (* Additional value tests *)
    test_case "var default inline" `Quick test_var_default_inline;
    test_case "minified value formatting" `Quick test_minified_value_formatting;
    test_case "regular value formatting" `Quick test_regular_value_formatting;
    test_case "oklch printing" `Quick test_color_oklch_printing;
    test_case "color-mix printing" `Quick test_color_mix_printing;
    test_case "var in calc other types" `Quick test_var_in_calc_types;
    test_case "number var printing" `Quick test_number_var_printing;
    test_case "var() with empty fallback" `Quick test_var_empty_fallback;
    (* New type tests *)
    test_case "length_percentage" `Quick test_length_percentage;
    test_case "number_percentage" `Quick test_number_percentage;
    test_case "color_space" `Quick test_color_space;
    test_case "hue" `Quick test_hue;
    test_case "color_name" `Quick test_color_name;
    test_case "alpha" `Quick test_alpha;
    test_case "hue_interpolation" `Quick test_hue_interpolation;
    test_case "calc_op" `Quick test_calc_op;
    test_case "number" `Quick test_number;
    test_case "transition_behavior" `Quick test_transition_behavior;
    test_case "component" `Quick test_component;
    test_case "channel" `Quick test_channel;
    test_case "rgb" `Quick test_rgb;
  ]

let suite = ("values", value_tests)
