(** Tests for CSS Values parsing *)

open Alcotest
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

let check_transition_behavior =
  check_value_cursor "transition_behavior" read_transition_behavior
    pp_transition_behavior

let check_length_percentage =
  check_value_cursor "length_percentage" read_length_percentage
    pp_length_percentage

let check_number_percentage =
  check_value_cursor "number_percentage" read_number_percentage
    pp_number_percentage

let check_color_space =
  check_value_cursor "color_space" read_color_space pp_color_space

let check_hue = check_value_cursor "hue" read_hue pp_hue

let check_color_name =
  check_value_cursor "color_name" read_color_name pp_color_name

let check_alpha = check_value_cursor "alpha" read_alpha pp_alpha

let check_hue_interpolation =
  check_value_cursor "hue_interpolation" read_hue_interpolation
    pp_hue_interpolation

let check_calc_op = check_value_cursor "calc_op" read_calc_op pp_calc_op

let check_component_values =
  check_value_cursor "component_values" read_component_values
    pp_component_values

let check_invalid_value =
  check_value_cursor "invalid_value" read_invalid_value pp_invalid_value

let check_math_const =
  check_value_cursor "math_const" read_math_const pp_math_const

let check_math_arg = check_value_cursor "math_arg" read_math_arg pp_math_arg
let check_component = check_value_cursor "component" read_component pp_component
let check_channel = check_value_cursor "channel" read_channel pp_channel
let check_rgb = check_value_cursor "rgb" read_rgb pp_rgb

let check_system_color =
  check_value_cursor "system_color" read_system_color pp_system_color

let check_attr_syntax =
  check_value_cursor "attr_syntax" read_attr_syntax pp_attr_syntax

let check_attr_type = check_value_cursor "attr_type" read_attr_type pp_attr_type

let test_length () =
  (* Basic units. pp holds the authored unit verbatim, re-spelling only the
     same-node float (leading-zero strip); dropping the unit from a zero length
     (0px -> 0) is a node change and an optimize transform. *)
  check_length "10px";
  check_length "0px";
  check_length "-10px";
  check_length "2.5rem";
  check_length ~expected:".5em" "0.5em";
  check_length "-1.5em";
  check_length "100%";
  (* 0% is a percentage, not the length 0: a percentage against an indefinite
     basis need not resolve to 0 (e.g. height:0% with an auto-height container
     acts as auto), so 0% is never dropped, in pp or optimize. *)
  check_length "0%";
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
  (* Legacy vendor-prefixed intrinsic sizing keywords (Bootstrap,
     Fontsource). *)
  check_length "-webkit-max-content";
  check_length "-webkit-min-content";
  check_length "-webkit-fit-content";
  check_length "-moz-max-content";
  check_length "-moz-min-content";
  check_length "-moz-fit-content";
  check_length "from-font";

  (* Edge cases *)
  check_length "0";
  check_length ".5rem";
  check_length "999999px";
  check_length "-999999px";
  check_length ~expected:".000001em" "0.000001em";
  check_length ~expected:".0000001rem" "0.0000001rem";
  (* CSS Values leaves numeric precision/range implementation-defined; the
     printer rounds this edge value to the nearest representable decimal. *)
  check_length ~expected:"1000000000px" "999999999px";
  check_length "-999px";
  check_length ".5px";

  (* Edge cases for very small values *)
  check_length ~expected:".00001px" "0.00001px";
  check_length ~expected:"-.001em" "-0.001em";
  (* Below the printer's fixed-point floor: a nonzero magnitude must keep its
     digits, not round to 0. *)
  check_length ~expected:".000000001em" "0.000000001em";
  check_length ~expected:".000000001em" "1e-9em";
  check_length ~expected:".0000000012em" "0.0000000012em";
  check_length ~expected:"-.000000001em" "-0.000000001em";

  (* Float formatting with lengths *)
  check_length ~expected:".5rem" "0.5rem";
  check_length "-.5rem";

  (* CSS Values 4 sec. 6.1: a signed zero is the same value as unsigned zero, so
     any zero collapses to the canonical 0 (the unit is kept). *)
  check_length ~expected:"0px" "-0px";
  check_length ~expected:"0px" "+0px";
  check_length ~expected:"0px" "0.0px";
  check_length ~expected:"0px" "-0.0px";
  check_length ~expected:"0" "-0";

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

  (* Zero length keeps its unit in pp; dropping it (0cm -> 0) is optimize. *)
  check_length "0cm";
  check_length "0vi";
  check_length "0svh";

  (* calc() is held verbatim by pp (a typed boundary); the zero-operand
     simplification [calc(X + 0)] -> [X] is an optimize fold. *)
  check_length "calc(100% - 0)";
  check_length "calc(10px + 0)";
  check_length "calc(0 + 10px)";

  (* optimize+minify strips the unit from a zero length and folds calc; 0% stays
     a percentage. *)
  decl_optimizes ~prop:"width" ~held:"0px" ~into:"0" "0px";
  decl_optimizes ~prop:"width" ~held:"0cm" ~into:"0" "0cm";
  decl_optimizes ~prop:"width" ~held:"0vi" ~into:"0" "0vi";
  decl_optimizes ~prop:"width" ~held:"0svh" ~into:"0" "0svh";
  decl_optimizes ~prop:"width" ~held:"0%" ~into:"0%" "0%";
  decl_optimizes ~prop:"width" ~held:"calc(100% - 0)" ~into:"100%"
    "calc(100% - 0)";
  decl_optimizes ~prop:"width" ~held:"calc(10px + 0)" ~into:"10px"
    "calc(10px + 0)";
  decl_optimizes ~prop:"width" ~held:"calc(0 + 10px)" ~into:"10px"
    "calc(0 + 10px)";

  neg_cursor read_length "invalid";
  neg_cursor read_length "abc";
  neg_cursor read_length "10";
  neg_cursor read_length "10pp";
  neg_cursor read_length "";
  (* Non-negative length contexts *)
  neg_cursor read_non_negative_length "-5px";
  (* Invalid calc expressions *)
  neg_cursor (read_calc read_length) "calc()";
  neg_cursor (read_calc read_length) "calc(10px +)"

let test_color () =
  (* Hex colors with #. Per CSS Color 4 section 12.1 the printer canonicalizes
     [#rrggbb] to [#rgb] under [~minify:true] when each channel pair matches,
     and lowercases the digits to match the consensus across Lightning CSS,
     esbuild, csso, cssnano, and clean-css. *)
  check_color "#fff";
  check_color ~expected:"#fff" "#FFF";
  check_color "#000";
  check_color "#123";
  check_color "#abc";
  check_color ~expected:"#abc" "#ABC";
  check_color "#123456";
  check_color "#abcdef";
  check_color ~expected:"#abcdef" "#ABCDEF";
  check_color ~expected:"#000" "#000000";
  check_color ~expected:"#fff" "#ffffff";
  check_color ~expected:"#fff" "#FFFFFF";
  (* Additional named colors *)
  check_color ~optimized:"#639" "rebeccapurple";
  check_color ~optimized:"#f0f8ff" "aliceblue";

  (* Modern color notations. pp preserves the authored function node; the
     equivalent hex form is the optimize+minify oracle. *)
  check_color ~expected:"hsl(180 50% 25%)" ~optimized:"#206060"
    "hsl(180deg 50% 25%)";
  check_color ~expected:"hwb(90 10% 20%)" ~optimized:"#73cc1a"
    "hwb(90deg 10% 20%)";
  check_color ~expected:"hsl(180 50% 25%/.5)" ~optimized:"#20606080"
    "hsl(180 50% 25% / 0.5)";
  check_color ~expected:"hwb(90 10% 20%)" ~optimized:"#73cc1a" "hwb(90 10% 20%)";
  check_color ~expected:"hwb(90 10% 20%/.25)" ~optimized:"#73cc1a40"
    "hwb(90 10% 20% / 0.25)";
  (* CSS Color 4 section 1.3: alpha [<percentage>] is spec-equivalent to the
     corresponding [<number>] in [\[0, 1\]]; the printer canonicalizes to the
     number form per cssnano. *)
  check_color ~expected:"hsl(180 50% 25%/30%)" ~optimized:"#2060604d"
    "hsl(180deg 50% 25% / 30%)";
  check_color ~optimized:"red" "color(srgb 1 0 0)";
  check_color ~expected:"color(display-p3 .8 .2 .1/.5)"
    "color(display-p3 0.8 0.2 0.1 / 0.5)";
  check_color ~expected:"color(oklab .5 .1-.05)" "color(oklab 50% 0.1 -0.05)";
  check_color ~expected:"color(lch .5 40 120)" "color(lch 50% 40 120)";
  check_color ~expected:"color(xyz .3 .4 .5)" ~optimized:"#5cb8b5"
    "color(xyz 0.3 0.4 0.5)";
  (* Additional color functions and forms *)
  check_color ~expected:"oklch(50%.2 30)" ~optimized:"#ba0d01"
    "oklch(50% 0.2 30)";
  (* Per CSS Color 4 section 1.4 the printer canonicalizes a percentage rgb()
     form to the equivalent named/hex spelling. *)
  check_color ~expected:"rgb(100% 0% 0%)" ~optimized:"red" "rgb(100% 0% 0%)";
  check_color ~expected:"oklab(50%.1-.05)" ~optimized:"#88497e"
    "oklab(50% 0.1 -0.05)";
  check_color ~expected:"lch(50%40 120)" ~optimized:"#638038" "lch(50% 40 120)";
  check_color ~expected:"rgb(255 0 0/50%)" ~optimized:"#ff000080"
    "rgb(255 0 0 / 50%)";

  (* Mixed channel formats in modern rgb() syntax. Per CSS Color 4 section 1.4
     the printer canonicalizes a fully-opaque rgb() to the equivalent named/hex
     form when shorter, regardless of input channel format. *)
  check_color ~optimized:"olive" "rgb(50% 128 0)";
  check_color ~optimized:"red" "rgb(255 0% 0)";
  check_color ~optimized:"navy" "rgb(0 0 50%)";
  (* Mixed channels with alpha (numeric) *)
  check_color ~expected:"rgb(50% 128 0/.5)" ~optimized:"#80800080"
    "rgb(50% 128 0 / 0.5)";

  (* Named colors - all variants. Under minification, named colors serialize to
     the shortest spec-equivalent spelling; equal-length named/hex ties use the
     hex spelling to match the documented minified-mode policy. *)
  check_color "red";
  check_color ~optimized:"#00f" "blue";
  check_color "green";
  check_color ~optimized:"#fff" "white";
  check_color ~optimized:"#000" "black";
  check_color ~optimized:"#ff0" "yellow";
  check_color ~optimized:"#0ff" "cyan";
  check_color ~optimized:"#f0f" "magenta";
  check_color "gray";
  check_color ~optimized:"gray" "grey";
  check_color "orange";
  check_color "purple";
  check_color "pink";
  check_color "silver";
  check_color "maroon";
  check_color ~optimized:"#f0f" "fuchsia";
  check_color ~optimized:"#0f0" "lime";
  check_color "olive";
  check_color "navy";
  check_color "teal";
  check_color ~optimized:"#0ff" "aqua";

  (* Special keywords. Per CSS Color 4 section 6.4 [transparent] canonicalizes
     to the shortest spec-equivalent spelling [#0000] under minify. *)
  check_color ~optimized:"#0000" "transparent";
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
    ~optimized:"var(--theme-primary,#2080df)"
    "var(--theme-primary, hsl(210deg 75% 50%))";
  check_color ~expected:"var(--accent,rgb(255 0 128/80%))"
    ~optimized:"var(--accent,#ff0080cc)" "var(--accent, rgb(255 0 128 / 80%))";

  (* RGB functions - various formats. Per CSS Color 4 section 1.4 the printer
     canonicalizes a fully-opaque rgb() to the named-or-hex equivalent when
     shorter. *)
  check_color ~expected:"rgb(255 0 0)" ~optimized:"red" "rgb(255, 0, 0)";
  check_color ~expected:"rgb(0 0 0)" ~optimized:"#000" "rgb(0, 0, 0)";
  check_color ~expected:"rgb(255 255 255)" ~optimized:"#fff"
    "rgb(255, 255, 255)";
  check_color ~expected:"rgb(128 128 128)" ~optimized:"gray"
    "rgb(128, 128, 128)";

  (* RGBA decodes to the modern rgb(... / alpha) node; optimize then folds to
     the shortest hex/named form. *)
  check_color ~expected:"rgb(255 0 0/.5)" ~optimized:"#ff000080"
    "rgba(255, 0, 0, 0.5)";
  check_color ~expected:"rgb(255 0 0/0)" ~optimized:"#f000" "rgba(255, 0, 0, 0)";
  check_color ~expected:"rgb(255 0 0/1)" ~optimized:"red" "rgba(255, 0, 0, 1)";
  check_color ~expected:"rgb(0 0 0/.25)" ~optimized:"#00000040"
    "rgba(0, 0, 0, 0.25)";
  check_color ~expected:"rgb(128 128 128/.75)" ~optimized:"#808080bf"
    "rgba(128, 128, 128, 0.75)";
  neg_cursor read_color "invalid";
  neg_cursor read_color "abc";
  neg_cursor read_color "#gg";
  check_color ~expected:"rgb(255 0 0)" ~optimized:"red" "rgb(256, 0, 0)";
  check_color ~expected:"hsl(1 50% 50%)" ~optimized:"#bf4240"
    "hsl(361, 50%, 50%)";
  neg_cursor read_color "";
  (* Unknown color keyword *)
  neg_cursor read_color "notacolor"

let lossless_color () =
  decl_lossless ~prop:"color" ~into:"oklch(.552 .016 285.938)"
    "oklch(55.2% .016 285.938)";
  decl_lossless ~prop:"color" ~into:"color-mix(in oklab,red,#00f)"
    "color-mix(in oklab, red 50%, blue 50%)";
  decl_lossless ~prop:"color" ~into:"red" "rgb(255 0 0)";
  decl_lossless ~prop:"color" ~into:"#0003" "rgb(0 0 0 / .2)";
  (* The README claim's other half: --lossless is a --minify modifier, so
     without --minify pretty keeps the authored percentage spelling and the
     exact hue (only same-node leading zeros drop); default minify would round
     to 285.9. *)
  check_value_cursor "color" read_color pp_color ~minify:false
    ~expected:"oklch(55.2% .016 285.938)" "oklch(55.2% 0.016 285.938)"

let test_angle () =
  (* pp holds the authored angle unit verbatim: the unit is part of the value
     node and cross-unit conversion is lossy (1rad has no exact degree
     spelling), so [360deg]<->[1turn], [400grad]->[360deg] etc. are optimize
     transforms, not pp. pp only re-spells the same-node float (leading/trailing
     zero strip). Unitless 0 is not a valid <angle>, so even [0deg] keeps its
     unit. *)

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

  (* Turns - same-node float re-spelling only (drop the leading zero) *)
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

  (* optimize+minify converts to the shortest spelling (ties prefer deg). *)
  decl_optimizes ~prop:"rotate" ~held:"360deg" ~into:"1turn" "360deg";
  decl_optimizes ~prop:"rotate" ~held:".25turn" ~into:"90deg" "0.25turn";
  decl_optimizes ~prop:"rotate" ~held:"2.5turn" ~into:"900deg" "2.5turn";
  decl_optimizes ~prop:"rotate" ~held:"100grad" ~into:"90deg" "100grad";
  decl_optimizes ~prop:"rotate" ~held:"400grad" ~into:"1turn" "400grad";
  decl_optimizes ~prop:"rotate" ~held:"-200grad" ~into:"-180deg" "-200grad";
  decl_optimizes ~prop:"rotate" ~held:"-360deg" ~into:"-1turn" "-360deg";

  (* A [deg -> turn] conversion divides by 360, which floors a small but
     non-zero angle to [0turn] once the printer caps the mantissa, silently
     zeroing it. It is only taken when the printed form still denotes the same
     angle, so a small angle keeps [deg] rather than collapsing to [0turn]. *)
  decl_optimizes ~prop:"rotate" ~held:".000001deg" ~into:".000001deg"
    "0.000001deg";
  decl_optimizes ~prop:"rotate" ~held:".0001deg" ~into:".0001deg" "0.0001deg";

  (* CSS Values 4 §10.7: scaling a typed <angle> by a unitless number folds to a
     concrete angle (then picks the shortest unit), so calc(1deg * -45) ->
     -45deg matches what the browser computes. Both operand orders fold. *)
  decl_optimizes ~prop:"rotate" ~held:"calc(1deg*-45)" ~into:"-45deg"
    "calc(1deg * -45)";
  decl_optimizes ~prop:"rotate" ~held:"calc(45deg*2)" ~into:"90deg"
    "calc(45deg * 2)";
  decl_optimizes ~prop:"rotate" ~held:"calc(-45*1deg)" ~into:"-45deg"
    "calc(-45 * 1deg)";
  decl_optimizes ~prop:"rotate" ~held:"calc(1turn*.5)" ~into:".5turn"
    "calc(1turn * 0.5)";
  (* Division folds only when the quotient is exact: 90/2 reduces, 45/7 keeps
     the calc() so no precision is lost. *)
  decl_optimizes ~prop:"rotate" ~held:"calc(90deg/2)" ~into:"45deg"
    "calc(90deg / 2)";
  decl_optimizes ~prop:"rotate" ~held:"calc(45deg/7)" ~into:"calc(45deg/7)"
    "calc(45deg / 7)";
  (* A var() operand is a runtime substitution boundary and stays unfolded; only
     the static 45deg*2 sub-expression reduces. *)
  decl_optimizes ~prop:"rotate" ~held:"calc(var(--x)*1deg)"
    ~into:"calc(var(--x)*1deg)" "calc(var(--x) * 1deg)";
  decl_optimizes ~prop:"rotate" ~held:"calc(45deg*2*var(--x))"
    ~into:"calc(90deg*var(--x))" "calc(45deg * 2 * var(--x))";
  (* Same-unit add/sub of two angles combines into one (CSS Values 4 §10.7);
     cross-unit operands stay unfolded. *)
  decl_optimizes ~prop:"rotate" ~held:"calc(45deg + 45deg)" ~into:"90deg"
    "calc(45deg + 45deg)";
  decl_optimizes ~prop:"rotate" ~held:"calc(90deg - 45deg)" ~into:"45deg"
    "calc(90deg - 45deg)";

  (* Var with angle fallback *)
  check_angle ~expected:"var(--custom-angle,45deg)" "var(--custom-angle, 45deg)";

  (* Var with empty fallback *)
  check_angle ~expected:"var(--angle,)" "var(--angle,)";
  neg_cursor read_angle "invalid";
  neg_cursor read_angle "45";
  neg_cursor read_angle "90";
  neg_cursor read_angle "45px";
  neg_cursor read_angle "abc";
  neg_cursor read_angle "";
  neg_cursor read_angle "360.5.5deg"

let test_duration () =
  (* CSS Values 4 section 6.6: [<time>] requires a unit. s and ms convert
     exactly (1s = 1000ms), so a duration decodes to one canonical magnitude and
     pp prints its shortest spelling - whichever of s/ms is shorter - as a
     same-node choice. (Contrast <angle>, where rad does not convert exactly, so
     the units stay distinct nodes and conversion is an optimize transform.) *)
  check_duration "1s";
  check_duration "0s";
  check_duration ~expected:".5s" "0.5s";
  check_duration ".25s";
  check_duration "10s";
  check_duration "999s";

  check_duration ~expected:".5s" "500ms";
  check_duration ~expected:"0s" "0ms";
  check_duration "1ms";
  check_duration ~expected:"1s" "1000ms";
  check_duration ~expected:".0505s" "50.5ms";
  check_duration ~expected:"999.999s" "999999ms";
  check_duration ".1s";
  check_duration ~expected:".15s" "150ms";
  check_duration "1.5s";

  (* CSS Values 4 §10.7: a typed <time> scales by a number and same-unit
     operands combine. s and ms stay distinct, and an inexact division keeps the
     calc() so no precision is lost. *)
  decl_optimizes ~prop:"transition-duration" ~held:"calc(1s*2)" ~into:"2s"
    "calc(1s * 2)";
  decl_optimizes ~prop:"transition-duration" ~held:"calc(2s/2)" ~into:"1s"
    "calc(2s / 2)";
  decl_optimizes ~prop:"transition-duration" ~held:"calc(.5s + .5s)" ~into:"1s"
    "calc(0.5s + 0.5s)";
  decl_optimizes ~prop:"transition-duration" ~held:"calc(1s/3)"
    ~into:"calc(1s/3)" "calc(1s / 3)";

  (* Var with duration fallback *)
  check_duration ~expected:"var(--custom-time,1s)" "var(--custom-time, 1s)";

  (* Var with empty fallback *)
  check_duration ~expected:"var(--time,)" "var(--time,)";
  neg_cursor read_duration "invalid";
  neg_cursor read_duration "1";
  neg_cursor read_duration "1px";
  neg_cursor read_duration "abc";
  neg_cursor read_duration "";
  neg_cursor read_duration "-1s";
  neg_cursor read_duration "10xs"

let test_percentage () =
  check_percentage "50%";
  check_percentage "100%";
  (* CSS Values 4 §6.5 only lets a [<length>] zero drop the unit; a zero
     [<percentage>] keeps the [%] (otherwise it would type as a [<number>] and
     the dimension/percentage grammars would reject it). *)
  check_percentage "0%";
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
  neg_cursor read_percentage "invalid";
  neg_cursor read_percentage "50";
  neg_cursor read_percentage "10";
  neg_cursor read_percentage "abc";
  neg_cursor read_percentage "";
  neg_cursor read_percentage "50px"

(* Not a roundtrip test *)
let test_var_in_color () =
  let t = Cursor.of_string "var(--primary-color)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "primary-color" var.name
  | _ -> fail "Expected Var variant for var() expression"

(* Not a roundtrip test *)
let test_var_with_fallback () =
  let t = Cursor.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "theme-color" var.name
  | _ -> fail "Expected Var for var() expression"

(* Not a roundtrip test *)
let test_var_color_keyword_fallback () =
  let t = Cursor.of_string "var(--custom-color, red)" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "custom-color" var.name
  | _ -> fail "Expected Var with red fallback"

(* Not a roundtrip test *)
let test_var_with_rgb_fallback () =
  let t = Cursor.of_string "var(--brand-color, rgb(255, 0, 0))" in
  let color = read_color t in
  match color with
  | Var var -> check string "var name" "brand-color" var.name
  | _ -> fail "Expected Var with rgb fallback"

(* Not a roundtrip test *)
let test_var_fallback_in_output () =
  let t = Cursor.of_string "var(--theme-color, #007bff)" in
  let color = read_color t in
  let output = Css.Pp.to_string pp_color color in
  check string "var with fallback output" "var(--theme-color, #007bff)" output

(* Not a roundtrip test *)
let test_var_in_calc_fallback () =
  let t = Cursor.of_string "calc(100% - var(--gap, 20px))" in
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
  let t = Cursor.of_string "calc(100% - var(--spacing))" in
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
  check string "regular rem drops leading 0" ".5rem" s;
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
  Alcotest.(check string) "oklch printing" "oklch(50% .123 30)" s

(* Not a roundtrip test *)
let test_color_mix_printing () =
  let open Css.Values in
  let c1 = rgb 255 0 0 in
  let c2 = rgb 0 0 255 in
  let mix = color_mix ~in_space:Display_p3 ~percent1:30. ~percent2:70. c1 c2 in
  let s = Css.Pp.to_string pp_color mix in
  Alcotest.(check string)
    "color-mix printing"
    "color-mix(in display-p3, rgb(255 0 0) 30%, rgb(0 0 255) 70%)" s

(* Not a roundtrip test *)
let test_var_in_calc_types () =
  let open Css.Values in
  (* Angle var in calc *)
  let t = Cursor.of_string "calc(90deg + var(--angle, 0.5turn))" in
  let calc_expr = read_calc read_angle t in
  (match calc_expr with
  | Expr (Val (Deg 90.), Add, Var v) ->
      Alcotest.(check string) "var name" "angle" v.name
  | _ -> Alcotest.fail "Expected angle var in calc");
  (* Duration var in calc *)
  let t = Cursor.of_string "calc(1s + var(--dur, 500ms))" in
  let calc_expr = read_calc read_duration t in
  (match calc_expr with
  | Expr (Val (S 1.), Add, Var v) ->
      Alcotest.(check string) "var name" "dur" v.name
  | _ -> Alcotest.fail "Expected duration var in calc");
  (* Percentage var in calc *)
  let t = Cursor.of_string "calc(50% + var(--p, 25%))" in
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
  let t = Cursor.of_string "var(--test,)" in
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
      | Var_fallback _ -> fail "Expected Empty fallback, got Var_fallback"
      | _ -> fail "Expected Empty fallback, got other")
  | _ -> fail "Expected Var variant"

(* Tests for newly added check functions *)

let test_length_percentage () =
  check_length_percentage "10px";
  check_length_percentage "50%";
  check_length_percentage "0";
  check_length_percentage "0%";
  (* pp holds the unit; stripping a zero length to unitless 0 is an optimize
     transform (type change <length> -> <number>). *)
  check_length_percentage "0px";
  decl_optimizes ~prop:"width" ~held:"0px" ~into:"0" "0px";
  neg_cursor read_length_percentage "invalid";
  neg_cursor read_length_percentage "abc";
  neg_cursor read_length_percentage ""

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
  neg_cursor read_number_percentage "invalid";
  neg_cursor read_number_percentage "abc";
  neg_cursor read_number_percentage ""

let test_color_space () =
  check_color_space "srgb";
  check_color_space "display-p3";
  check_color_space "rec2020";
  neg_cursor read_color_space "invalid";
  neg_cursor read_color_space "abc";
  neg_cursor read_color_space ""

let test_hue () =
  check_hue ~expected:"180" "180deg";
  check_hue ~expected:"180" "0.5turn";
  check_hue ~expected:"180" "200grad";
  check_hue ~expected:"180" "3.14159rad";
  neg_cursor read_hue "invalid";
  neg_cursor read_hue "abc";
  check_hue "180";
  neg_cursor read_hue ""

let test_color_name () =
  check_color_name "red";
  check_color_name "blue";
  check_color_name "rebeccapurple";
  neg_cursor read_color_name "invalid";
  neg_cursor read_color_name "notacolor";
  neg_cursor read_color_name "123";
  neg_cursor read_color_name ""

let test_alpha () =
  (* pp preserves percentage alpha spelling; optimize owns percentage->number
     conversion in colour contexts. *)
  check_alpha ~expected:".5" "0.5";
  check_alpha "50%";
  decl_optimizes ~prop:"color" ~into:"#00000080" "rgb(0 0 0 / 50%)";
  check_alpha "1";
  check_alpha "0";
  neg_cursor read_alpha "invalid";
  neg_cursor read_alpha "abc";
  check_alpha ~expected:"1" "1.5";
  check_alpha ~expected:"0" "-0.5";
  check_alpha ~expected:"100%" "150%";
  decl_optimizes ~prop:"color" ~into:"#000" "rgb(0 0 0 / 150%)";
  (* CSS Values 4 §10.7: an alpha calc() resolves - a scaled or added alpha
     folds, and a percentage alpha that reaches 100% drops the channel. *)
  decl_optimizes ~prop:"color" ~into:"#ff000080" "rgb(255 0 0 / calc(0.5 * 1))";
  decl_optimizes ~prop:"color" ~into:"#ff00004d"
    "rgb(255 0 0 / calc(0.2 + 0.1))";
  decl_optimizes ~prop:"color" ~into:"red" "rgb(255 0 0 / calc(50% * 2))";
  neg_cursor read_alpha "1px"

let test_hue_interpolation () =
  check_hue_interpolation "shorter";
  check_hue_interpolation "longer";
  check_hue_interpolation "increasing";
  check_hue_interpolation "decreasing";
  neg_cursor read_hue_interpolation "invalid";
  neg_cursor read_hue_interpolation "abc";
  neg_cursor read_hue_interpolation ""

let test_calc_op () =
  (* Per CSS Values 4 section 10.7 the [+] and [-] operators require surrounding
     whitespace; [*] and [/] do not. Per shortest-wins the printer omits the
     spaces around [*] and [/] (matches cssnano). *)
  check_calc_op ~expected:" + " "+";
  check_calc_op ~expected:" - " "-";
  check_calc_op "*";
  check_calc_op "/";
  neg_cursor read_calc_op "invalid";
  neg_cursor read_calc_op "abc";
  neg_cursor read_calc_op "++";
  neg_cursor read_calc_op "";
  neg_cursor read_calc_op "="

let test_number () =
  check_number "42";
  check_number "3.14";
  check_number "0";
  check_number "-5";
  neg_cursor read_number "invalid";
  neg_cursor read_number "abc";
  neg_cursor read_number "";
  neg_cursor read_number "1px"

let test_transition_behavior () =
  check_transition_behavior "normal";
  check_transition_behavior "allow-discrete";
  neg_cursor read_transition_behavior "inherit";
  neg_cursor read_transition_behavior "invalid";
  neg_cursor read_transition_behavior ""

let test_component () =
  (* Component tests - various color component values *)
  check_component ~expected:".5" "50%";
  check_component "128";
  check_component ~expected:".5" "0.5";
  neg_cursor read_component "invalid";
  neg_cursor read_component "abc";
  check_component ~expected:"0" "-1";
  (* Clamped in output *)
  check_component ~expected:"255" "256";
  (* Clamped in output *)
  check_component ~expected:"1" "150%" (* Clamped in output *)

let test_channel () =
  check_channel "255";
  check_channel "50%";
  check_channel ~expected:".5" "0.5";
  neg_cursor read_channel "invalid";
  neg_cursor read_channel "abc";
  check_channel ~expected:"255" "256";
  check_channel ~expected:"0" "-1";
  check_channel ~expected:"100%" "150%";
  neg_cursor read_channel ""

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
  neg_cursor read_rgb "invalid";
  neg_cursor read_rgb "abc";
  neg_cursor read_rgb "";
  neg_cursor read_rgb "255"

let test_system_color () =
  check_system_color ~expected:"accentcolor" "AccentColor";
  check_system_color ~expected:"canvastext" "CanvasText";
  check_system_color ~expected:"highlight" "Highlight";
  check_system_color ~expected:"buttonface" "ButtonFace";
  check_system_color ~expected:"field" "Field";
  neg_cursor read_system_color "";
  neg_cursor read_system_color "invalid-color"

let spec_values_color_current () =
  check_color ~expected:"oklch(50%.2 none)" ~optimized:"oklch(.5 .2 none)"
    "oklch(50% 0.2 none)";
  check_color ~optimized:"rgb(from rebeccapurple r g b)"
    "rgb(from rebeccapurple r g b)";
  check_color ~expected:"contrast-color(white)"
    ~optimized:"contrast-color(#fff)" "contrast-color(white)";
  check_color ~expected:"light-dark(black,white)"
    ~optimized:"light-dark(#000,#fff)" "light-dark(black, white)";
  check_duration ~expected:"calc(sibling-index()*100ms)"
    "calc(sibling-index() * 100ms)";
  neg_cursor read_color "rgb(from r g b)";
  neg_cursor read_color "contrast-color()";
  neg_cursor read_color "light-dark(black)"

let spec_values_l45_math_color () =
  check_length "1cqw";
  check_length "1cqh";
  check_length "1cqi";
  check_length "1cqb";
  check_length "1cqmin";
  check_length "1cqmax";
  check_length ~expected:"calc-size(auto,size + 1rem)"
    "calc-size(auto, size + 1rem)";
  check_length ~expected:"anchor-size(width)" "anchor-size(width)";
  check_length ~expected:"anchor(--tooltip width,10px)"
    "anchor(--tooltip width, 10px)";
  check_number ~expected:"calc(1 + sibling-index())" "calc(1 + sibling-index())";
  check_number ~expected:"calc(sibling-count() - 1)" "calc(sibling-count() - 1)";
  check_percentage ~expected:"calc(50% + 10%)" "calc(50% + 10%)";
  check_color ~expected:"color-mix(in oklab,red 40%,blue)" ~optimized:"#7551b6"
    "color-mix(in oklab, red 40%, blue)";
  check_color ~expected:"color-mix(in srgb longer hue,red,blue)"
    ~optimized:"color-mix(in srgb longer hue,red,#00f)"
    "color-mix(in srgb longer hue, red, blue)";
  (* CSS Color 5 §3: the color-mix weight may be a [calc()]. A constant folds to
     a plain percentage; a [var()]-bearing one is kept verbatim. The weight
     reader previously rejected any [calc()] here. *)
  check_color ~expected:"color-mix(in srgb,red 30%,blue)"
    ~optimized:"color-mix(in srgb,red 30%,#00f)"
    "color-mix(in srgb, red calc(30%), blue)";
  check_color ~expected:"color-mix(in srgb,var(--c) calc(var(--o)*100%),blue)"
    ~optimized:"color-mix(in srgb,var(--c) calc(var(--o)*100%),#00f)"
    "color-mix(in srgb, var(--c) calc(var(--o) * 100%), blue)";
  (* Both the colour and the percentage are [var()]: the leading var is the
     colour, so source order is preserved (regression: the two were swapped,
     landing the alpha var in the colour slot). *)
  check_color ~expected:"color-mix(in oklab,var(--a) var(--b),blue)"
    ~optimized:"color-mix(in oklab,var(--a) var(--b),#00f)"
    "color-mix(in oklab, var(--a) var(--b), blue)";
  (* A [var()] followed by a concrete colour can only be the percentage, so it
     is read percentage-first and re-emitted colour-first. *)
  check_color ~expected:"color-mix(in oklab,red var(--p),blue)"
    ~optimized:"color-mix(in oklab,red var(--p),#00f)"
    "color-mix(in oklab, var(--p) red, blue)";
  check_color ~expected:"hsl(0 50% 50%)" ~optimized:"#bf4040"
    "hsl(none 50% 50%)";
  check_color ~expected:"rgb(from var(--c) r g b/.5)"
    "rgb(from var(--c) r g b / 50%)";
  check_color ~expected:"color(display-p3 none .5 1)"
    "color(display-p3 none 0.5 1)";
  neg_cursor read_length "calc-size()";
  neg_cursor read_length "anchor()";
  neg_cursor read_length "anchor-size()";
  neg_cursor read_number "sibling-index(1)";
  neg_cursor read_color "color-mix(in oklab)";
  neg_cursor read_color "color-mix(in oklab, red calc(50%) calc(20%), blue)";
  neg_cursor read_color "rgb(from red r g)";
  neg_cursor read_color "color(display-p3 1 0)"

let spec_color5_function_edges () =
  check_color ~expected:"lab(50%10 20)" ~optimized:"#907055" "lab(50% 10 20)";
  check_color ~expected:"lch(50%20 30)" ~optimized:"#976c67" "lch(50% 20 30)";
  check_color ~expected:"oklab(50%.1 .2)" ~optimized:"oklab(.5 .1 .2)"
    "oklab(50% 0.1 0.2)";
  check_color ~expected:"oklch(50%.1 20/.5)" ~optimized:"#944a4b80"
    "oklch(50% 0.1 20 / 0.5)";
  (* CSS Color 4 sec. 9.2/9.3: an oklab/oklch lightness number out of [0,1]
     clamps (like lab/lch on 0-100), instead of dropping the declaration. *)
  check_color ~expected:"oklch(1 .1 30)" "oklch(1.5 0.1 30)";
  check_color ~expected:"oklch(0 .1 30)" "oklch(-0.5 0.1 30)";
  check_color ~expected:"oklab(0 .1 .1)" "oklab(-1 0.1 0.1)";
  check_color ~expected:"color(srgb 1 0 0/.5)" ~optimized:"#ff000080"
    "color(srgb 1 0 0 / 0.5)";
  check_color ~expected:"color(rec2020 .1 .2 .3)" "color(rec2020 0.1 0.2 0.3)";
  check_color ~expected:"color-mix(in lch longer hue,red 30%,blue)"
    ~optimized:"color-mix(in lch longer hue,red 30%,#00f)"
    "color-mix(in lch longer hue, red 30%, blue)";
  check_color ~expected:"color-mix(in hsl shorter hue,red,blue 40%)"
    ~optimized:"color-mix(in hsl,red,#00f 40%)"
    "color-mix(in hsl shorter hue, red, blue 40%)";
  (* CSS Color 4 sec. 9.2/9.3: lab/lch lightness uses a 0-100 number scale (a
     bare number L and the same number as a percentage are equal), so mixing two
     L=70 colours keeps L=70, not 70*100. Both results are in the sRGB gamut and
     fold to hex. *)
  check_color ~expected:"color-mix(in lch,lch(70 50 30),lch(70 50 60))"
    ~optimized:"#f2906d" "color-mix(in lch, lch(70 50 30), lch(70 50 60))";
  check_color ~expected:"color-mix(in lab,lab(50 10 10),lab(70-10 5))"
    ~optimized:"#959083" "color-mix(in lab, lab(50 10 10), lab(70 -10 5))";
  (* CSS Color 5 sec. 3: the default hue interpolation is shorter; mixing hues
     350 and 10 takes the 20-degree arc through 0, not the linear average
     180. *)
  check_color ~expected:"color-mix(in oklch,oklch(.7 .15 350),oklch(.7 .15 10))"
    ~optimized:"#e7729b"
    "color-mix(in oklch, oklch(0.7 0.15 350), oklch(0.7 0.15 10))";
  check_color ~expected:"color-mix(in lch,lch(70 50 350),lch(70 50 10))"
    ~optimized:"#fc84ae" "color-mix(in lch, lch(70 50 350), lch(70 50 10))";
  (* CSS Color 4 sec. 12.3: [color-mix(in srgb, ...)] interpolates premultiplied
     channels then un-premultiplies by the interpolated alpha; the <100%-sum
     scaling applies only to the result alpha. *)
  (* Opaque 50/50: straight average. *)
  check_color ~expected:"color-mix(in srgb,#fff 25%,#000 75%)"
    ~optimized:"#404040" "color-mix(in srgb, #fff 25%, #000 75%)";
  (* A transparent component must not bleed its zero channels: un-premultiply
     restores the opaque operand's colour, only the alpha halves. *)
  check_color ~expected:"color-mix(in srgb,#944a4b 50%,#0000)"
    ~optimized:"#944a4b80" "color-mix(in srgb, #944a4b 50%, #0000)";
  check_color ~expected:"color-mix(in srgb,#0000,red)" ~optimized:"#ff000080"
    "color-mix(in srgb, #0000, red)";
  check_color ~expected:"color-mix(in srgb,transparent,red)"
    ~optimized:"#ff000080" "color-mix(in srgb, transparent, red)";
  (* Sec. 12.3 premultiplication applies in every interpolation space, not just
     sRGB. Mixing #0088cc 50% with transparent must restore #0088cc at half
     alpha in oklab/oklch/lab/lch too, never bleed transparent's zero
     coordinates in and darken the result. *)
  check_color ~expected:"color-mix(in oklab,#08c 50%,transparent)"
    ~optimized:"#0088cc80" "color-mix(in oklab, #0088cc 50%, transparent)";
  check_color ~expected:"color-mix(in oklch,#08c 50%,transparent)"
    ~optimized:"#0088cc80" "color-mix(in oklch, #0088cc 50%, transparent)";
  check_color ~expected:"color-mix(in lab,#08c 50%,transparent)"
    ~optimized:"#0088cc80" "color-mix(in lab, #0088cc 50%, transparent)";
  check_color ~expected:"color-mix(in lch,#08c 50%,transparent)"
    ~optimized:"#0088cc80" "color-mix(in lch, #0088cc 50%, transparent)";
  (* No explicit percentage: the 50/50 default restores the colour the same
     way. *)
  check_color ~expected:"color-mix(in oklab,#08c,transparent)"
    ~optimized:"#0088cc80" "color-mix(in oklab, #0088cc, transparent)";
  (* A zero-chroma operand (transparent) has a powerless hue in the polar spaces
     (sec. 12.2): it carries the opaque operand's hue over instead of
     interpolating toward an undefined angle, so red at half alpha stays red. *)
  check_color ~expected:"color-mix(in oklch,red 50%,transparent)"
    ~optimized:"#ff000080" "color-mix(in oklch, red 50%, transparent)";
  (* Typed lab-family operands with partial alpha route through the
     premultiplied path too: mixing a colour 50% with a fully transparent one
     restores it at half alpha. *)
  check_color ~expected:"color-mix(in oklab,oklab(60%.1-.1),oklab(30%0 0/0))"
    ~optimized:"#9f63ba80"
    "color-mix(in oklab, oklab(60% 0.1 -0.1), oklab(30% 0 0 / 0))";
  (* Percentages summing to <100% scale only the result alpha (here 60%), not
     the colour channels. *)
  check_color ~expected:"color-mix(in srgb,red 30%,blue 30%)"
    ~optimized:"#80008099" "color-mix(in srgb, red 30%, blue 30%)";
  (* A modern-space argument resolves and the mix folds in a single pass
     (idempotent minify), not only once both operands are already hex. *)
  check_color ~expected:"color-mix(in srgb,oklch(50%.1 30) 50%,transparent)"
    ~optimized:"#944b4080"
    "color-mix(in srgb, oklch(50% 0.1 30) 50%, transparent)";
  check_color ~expected:"color-mix(in oklab,var(--a),var(--b))"
    ~optimized:"color-mix(in oklab,var(--a),var(--b))"
    "color-mix(in oklab, var(--a), var(--b))";
  check_color ~expected:"color-mix(var(--a),var(--b))"
    "color-mix(var(--a), var(--b))";
  neg_cursor read_color "lab(50% 10)";
  neg_cursor read_color "lch(50% 20)";
  neg_cursor read_color "oklch(50% .1 20 /)";
  neg_cursor read_color "color(unknown 1 0 0)";
  neg_cursor read_color "color-mix(in, red, blue)";
  neg_cursor read_color "color-mix(in srgb red blue)"

let spec_color_invalid_mutation_matrix () =
  List.iter
    (fun input -> neg_cursor read_color input)
    [
      "#12";
      "#12345";
      "#123456789";
      "#ggg";
      "rgb()";
      "rgb(1 2)";
      "rgb(1, 2 3)";
      "rgb(1 2 3 4)";
      "hsl(0 50)";
      "hsl(0, 50%, 50% 1)";
      "hwb(0 0%)";
      "hwb(0 0% 0% 0%)";
      "lab(50% 10)";
      "lab(50% 10 20 30)";
      "lch(50% 20)";
      "oklab(50% .1)";
      "oklch(50% .1 20 /)";
      "color()";
      "color(display-p3 1 0)";
      "color(unknown 1 0 0)";
      "color(display-p3 1 0 0 1 2)";
      "color-mix(in srgb, red,)";
      "color-mix(in srgb, red 20% 30%, blue)";
      "light-dark(black)";
      "light-dark(black, white, red)";
      "rgb(from red r g)";
      "rgb(from red r g b extra)";
      "contrast-color()";
      "transparent()";
      "currentColor()";
      "rgb(255, 0, 0 / 50%)";
      "rgb(255 0 0, 50%)";
      "hsl(0deg, 50% 50%)";
      "hsl(0 50% 50%, .5)";
      "hwb(0deg, 0%, 0%)";
      "lab(50% 10px 20)";
      "lch(50% red 30)";
      "oklab(50% .1px .2)";
      "oklch(50% .1 red)";
      "color(srgb 1 0 / .5)";
      "color(srgb 1 0 0 / / .5)";
      "color-mix(in bogus, red, blue)";
      "color-mix(in srgb, red -10%, blue)";
      "color-mix(in srgb, red 110%, blue)";
      "light-dark()";
      "rgb(from red r g b /)";
    ]

let spec_math_function_edges () =
  (* pp holds typed math-function nodes; optimize owns the all-constant fold. *)
  check_length ~expected:"round(10px,3px)" "round(nearest, 10px, 3px)";
  decl_optimizes ~prop:"width" ~held:"round(10px,3px)" ~into:"9px"
    "round(nearest, 10px, 3px)";
  check_length ~expected:"mod(10px,3px)" "mod(10px, 3px)";
  decl_optimizes ~prop:"margin" ~held:"mod(10px,3px)" ~into:"1px"
    "mod(10px, 3px)";
  check_length ~expected:"rem(10px,3px)" "rem(10px, 3px)";
  decl_optimizes ~prop:"margin" ~held:"rem(10px,3px)" ~into:"1px"
    "rem(10px, 3px)";
  check_length ~expected:"hypot(3px,4px)" "hypot(3px, 4px)";
  decl_optimizes ~prop:"width" ~held:"hypot(3px,4px)" ~into:"5px"
    "hypot(3px, 4px)";
  check_length ~expected:"abs(-10px)" "abs(-10px)";
  decl_optimizes ~prop:"margin" ~held:"abs(-10px)" ~into:"10px" "abs(-10px)";
  check_length ~expected:"sign(10px)" "sign(10px)";
  check_number ~expected:"round(up,1.2,1)" "round(up, 1.2, 1)";
  check_number ~expected:"mod(10,3)" "mod(10, 3)";
  check_number ~expected:"hypot(3,4)" "hypot(3, 4)";
  check_number ~expected:"pow(2,3)" "pow(2, 3)";
  check_number ~expected:"sqrt(4)" "sqrt(4)";
  check_number ~expected:"sin(30deg)" "sin(30deg)";
  neg_cursor read_length "round(nearest, 10px)";
  neg_cursor read_length "mod(10px)";
  neg_cursor read_number "pow(2)";
  neg_cursor read_number "sqrt()";
  neg_cursor read_number "sin()"

let test_attr_syntax () =
  check_attr_syntax "<length>";
  check_attr_syntax "<length-percentage>";
  check_attr_syntax "<color>";
  check_attr_syntax "<number>";
  check_attr_syntax "<percentage>";
  neg_cursor read_attr_syntax "<integer>";
  neg_cursor read_attr_syntax "length"

let test_attr_type () =
  check_attr_type "type(<length>)";
  check_attr_type "type(<color>)";
  check_attr_type "raw-string";
  check_attr_type "number";
  check_attr_type "px";
  check_attr_type "%";
  neg_cursor read_attr_type "type(<integer>)";
  neg_cursor read_attr_type "type()"

let test_component_values () =
  check_component_values ~expected:"foo(1,2)" "foo(1, 2)";
  check_component_values ~expected:"a b" "a   b";
  check_component_values "\"x y\""

let test_invalid_value () =
  check_invalid_value ~expected:"asin(1deg)" "asin(1deg)";
  check_invalid_value ~expected:"foo(1,2)" "foo(1, 2)"

let test_math_const () =
  check_math_const "pi";
  check_math_const "e";
  check_math_const "infinity";
  check_math_const "-infinity";
  check_math_const ~expected:"NaN" "nan";
  neg_cursor read_math_const "tau"

let test_math_arg () =
  check_math_arg "1";
  check_math_arg "1vw";
  check_math_arg "pi";
  check_math_arg "1 + 2";
  check_math_arg "sqrt(4)";
  neg_cursor read_math_arg "foo"

let value_tests =
  [
    test_case "system_color" `Quick test_system_color;
    test_case "length" `Quick test_length;
    test_case "color" `Quick test_color;
    test_case "lossless color" `Quick lossless_color;
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
    test_case "component_values" `Quick test_component_values;
    test_case "invalid_value" `Quick test_invalid_value;
    test_case "math_const" `Quick test_math_const;
    test_case "math_arg" `Quick test_math_arg;
    test_case "number" `Quick test_number;
    test_case "attr_syntax" `Quick test_attr_syntax;
    test_case "attr_type" `Quick test_attr_type;
    test_case "transition_behavior" `Quick test_transition_behavior;
    test_case "component" `Quick test_component;
    test_case "channel" `Quick test_channel;
    test_case "rgb" `Quick test_rgb;
    test_case "spec values and color current-work" `Quick
      spec_values_color_current;
    test_case "spec values level 4/5 math and color edges" `Quick
      spec_values_l45_math_color;
    test_case "spec color 5 function edges" `Quick spec_color5_function_edges;
    test_case "spec color invalid mutation matrix" `Quick
      spec_color_invalid_mutation_matrix;
    test_case "spec math function edges" `Quick spec_math_function_edges;
  ]

let suite = ("values", value_tests)
