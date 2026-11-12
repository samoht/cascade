open Cascade
open Css.Values
open Css_test_helpers

(* One-liner check functions for each CSS value type *)
let check_length = check_value_cursor "length" read_length pp_length

(* Always assert both paths. pp serializes the parsed colour ([expected], held);
   optimize+minify canonicalizes it ([optimized]). When [optimized] is omitted
   the colour has no shorter spec-equivalent spelling (already canonical, or a
   modern colour that is out of the sRGB gamut and so cannot losslessly fold to
   hex), and optimize must preserve the held form. *)
let check_color ?minify ?roundtrip ?expected ?optimized input =
  check_value_cursor "color" read_color pp_color ?minify ?roundtrip ?expected
    input;
  let held = Option.value ~default:input expected in
  let into = Option.value ~default:held optimized in
  decl_optimizes ~prop:"color" ~held ~into input

let check_angle = check_value_cursor "angle" read_angle pp_angle
let check_duration = check_value_cursor "duration" read_duration pp_duration

let check_percentage =
  check_value_cursor "percentage" read_percentage pp_percentage

let check_number = check_value_cursor "number" read_number pp_number
let check_math_arg = check_value_cursor "math_arg" read_math_arg pp_math_arg
let check_component = check_value_cursor "component" read_component pp_component
let check_channel = check_value_cursor "channel" read_channel pp_channel
let check_rgb = check_value_cursor "rgb" read_rgb pp_rgb
