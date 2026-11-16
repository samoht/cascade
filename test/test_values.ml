(** Tests for CSS Values parsing *)

open Alcotest
open Cascade
open Css.Values
open Css_test_helpers
open Values_helpers

(* Not a [check_<type>]: the sizing keywords are lengths, read through the same
   [length] parser with its own grammar switch on. *)
let length_with_sizing =
  check_value_cursor "length" (read_length ~sizing:true) pp_length

let check_transition_behavior =
  check_value_cursor "transition_behavior" read_transition_behavior
    pp_transition_behavior

let check_length_percentage =
  check_value_cursor "length_percentage" read_length_percentage
    pp_length_percentage

let check_number_percentage =
  check_value_cursor "number_percentage" read_number_percentage
    pp_number_percentage

let check_component_values =
  check_value_cursor "component_values" read_component_values
    pp_component_values

let check_invalid_value =
  check_value_cursor "invalid_value" read_invalid_value pp_invalid_value

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
  (* CSS Sizing 3 sec. 5 gives the intrinsic sizes to the sizing properties, so
     the reader takes them only where the property does and the default refuses
     them. *)
  length_with_sizing "max-content";
  length_with_sizing "min-content";
  length_with_sizing "fit-content";
  (* Legacy vendor-prefixed intrinsic sizing keywords (Bootstrap,
     Fontsource). *)
  length_with_sizing "-webkit-max-content";
  length_with_sizing "-webkit-min-content";
  length_with_sizing "-webkit-fit-content";
  length_with_sizing "-moz-max-content";
  length_with_sizing "-moz-min-content";
  length_with_sizing "-moz-fit-content";
  length_with_sizing "from-font";
  List.iter
    (fun keyword ->
      neg_cursor (fun t -> ignore (read_length t : Css.Values.length)) keyword)
    [ "max-content"; "min-content"; "fit-content"; "from-font" ];

  (* Edge cases *)
  check_length "0";
  check_length ".5rem";
  check_length "999999px";
  check_length "-999999px";
  check_length ~expected:".000001em" "0.000001em";
  check_length ~expected:".0000001rem" "0.0000001rem";
  (* CSS Values leaves numeric precision implementation-defined, but an authored
     magnitude is not Cascade's to pick: [1000000000px] is a pixel away from
     what the author wrote. *)
  check_length "999999999px";
  check_length ".4285714em";
  check_length "1.5714286em";
  check_length "1.0000001px";
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

  (* CSS Values 4 sec. 5.3: a signed zero is the same value as unsigned zero, so
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

  (* CSS Values 4 sec. 10.9: a unitless zero inside a math function has the
     [<number>] type. It cannot be added to a [<length-percentage>], and a lone
     numeric result cannot satisfy [<length-percentage>] either. *)
  neg_cursor read_length "calc(100% - 0)";
  neg_cursor read_length "calc(10px + 0)";
  neg_cursor read_length "calc(0 + 10px)";
  neg_cursor read_length "calc(0)";

  (* optimize+minify strips the unit from a zero length and folds calc; 0% stays
     a percentage. *)
  decl_optimizes ~prop:"width" ~held:"0px" ~into:"0" "0px";
  decl_optimizes ~prop:"width" ~held:"0cm" ~into:"0" "0cm";
  decl_optimizes ~prop:"width" ~held:"0vi" ~into:"0" "0vi";
  decl_optimizes ~prop:"width" ~held:"0svh" ~into:"0" "0svh";
  decl_optimizes ~prop:"width" ~held:"0%" ~into:"0%" "0%";
  (* Typed zero terms remain valid and keep contributing their types. *)
  check_length "calc(1em + 0px)";
  check_length "calc(1px + 0%)";
  check_length "calc(var(--size) + 0)";

  (* CSS Values 4 sec. 10.8 makes a parenthesised operand a whole [<calc-sum>],
     so a trailing token inside one invalidates the value rather than shortening
     it to the prefix; browsers drop both declarations. *)
  neg_cursor read_length "calc((1px 2px))";
  neg_cursor read_length "min((1px 2px))";

  (* CSS Values 4 sec. 10.8: [<calc-value>] is a number, a dimension, a
     percentage, a [<calc-keyword>] or a parenthesised [<calc-sum>], and sec.
     10.7 limits [<calc-keyword>] to [e], [pi], [infinity], [-infinity] and
     [NaN]. A CSS-wide keyword (sec. 4.1.1 gives it the whole declaration value
     alone) and a sizing keyword or function are none of those, so a math
     operand takes none of them. *)
  neg_cursor read_length "calc(auto)";
  neg_cursor read_length "calc(inherit)";
  neg_cursor read_length "calc(initial)";
  neg_cursor read_length "calc(unset)";
  neg_cursor read_length "calc(revert)";
  neg_cursor read_length "calc(revert-layer)";
  neg_cursor read_length "calc(calc(auto))";
  neg_cursor read_length "calc(1px + initial)";
  neg_cursor read_length "calc(fit-content(20rem))";
  neg_cursor read_length "min(unset,1px)";
  neg_cursor read_length "min(1px,auto)";
  neg_cursor read_length "clamp(1px,inherit,3px)";
  neg_cursor read_length "abs(auto)";
  neg_cursor read_length "round(auto,1px)";
  neg_cursor read_length "hypot(auto)";
  neg_cursor read_length "mod(auto,1px)";
  neg_cursor read_length "rem(auto,1px)";

  (* CSS Values 4 sec. 10.2: the arguments of [min()] / [max()] / [clamp()]
     "must have a consistent type or else the function is invalid", and sec.
     10.9 makes a unitless zero a [<number>], not a length. *)
  neg_cursor read_length "min(0,1px)";
  neg_cursor read_length "max(0,1px)";
  neg_cursor read_length "clamp(0px,0,100px)";

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

  (* CSS Values 4 sec. 10.10.1: scaling a typed <angle> by a unitless number
     folds to a concrete angle (then picks the shortest unit), so calc(1deg *
     -45) -> -45deg matches what the browser computes. Both operand orders
     fold. *)
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
  (* Same-unit add/sub of two angles combines into one (CSS Values 4 sec.
     10.10.1); cross-unit operands stay unfolded. *)
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
  (* CSS Values 4 section 7.2: [<time>] requires a unit, and [ms] / [s] are
     interchangeable. Pure minified serialization chooses the shorter exact
     spelling without running the optimizer. *)
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

  (* CSS Values 4 sec. 10.10.1: a typed <time> scales by a number and same-unit
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
  neg_cursor ~allow_partial:true read_duration "-1s";
  neg_cursor read_duration "10xs"

let test_percentage () =
  check_percentage "50%";
  check_percentage "100%";
  (* CSS Values 4 sec. 6 only lets a [<length>] zero drop the unit; a zero
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
let test_minified_value_formatting () =
  (* Leading zero drop in minified output for values *)
  let s = Css.Pp.to_string ~minify:true pp_length (Rem 0.5) in
  check string "minified rem" ".5rem" s;
  let s = Css.Pp.to_string ~minify:true pp_number (Num 0.5) in
  check string "minified number" ".5" s;
  (* The duration printer chooses the shorter exact unit in minified mode. *)
  let s = Css.Pp.to_string ~minify:true pp_duration (Ms 500.) in
  check string "minified ms" ".5s" s;
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

(* Minified output is input: every declaration Cascade writes has to read back
   through Cascade's own grammar, with no warning and no second fold. The ten
   properties below each folded a math function to a literal their own reader
   refuses, so each one wrote CSS it could not read. *)
let decl_reads_back ~prop input =
  let wrap v = String.concat "" [ ".x{"; prop; ":"; v; "}" ] in
  let minify p = Css.to_string ~minify:true (Css.optimize p) |> String.trim in
  match Css.of_string ~strict:false (wrap input) with
  | Error _ -> Alcotest.failf "parse failed: %s" (wrap input)
  | Ok p -> (
      let out = minify p.stylesheet in
      match Css.of_string ~strict:true out with
      | Error e ->
          Alcotest.failf "%s wrote %S, which it refuses to read: %s"
            (wrap input) out (Error.to_string e)
      | Ok back ->
          (* A second pass must not move: a value that folds again was never
             minified, it was only half-folded. *)
          Alcotest.(check string)
            (String.concat "" [ wrap input; " is a fixed point" ])
            out (minify back.stylesheet))

let spec_minified_output_reads_back () =
  let reads_back prop = List.iter (decl_reads_back ~prop) in
  reads_back "aspect-ratio" [ "sign(-1px)"; "calc(-1)"; "round(-3,2)"; "1" ];
  reads_back "border-radius" [ "calc(-1px)"; "calc(-1px * 2)"; "1px" ];
  reads_back "flex" [ "sign(-1px)"; "calc(-1)"; "calc(1)" ];
  reads_back "flex-grow" [ "sign(-1px)"; "calc(-1)"; "round(-3,2)" ];
  reads_back "flex-shrink" [ "sign(-1px)"; "calc(-1)"; "calc(2)" ];
  reads_back "line-height" [ "sign(-1px)"; "calc(-1)"; "calc(1.5)" ];
  reads_back "tab-size" [ "sign(-1px)"; "calc(-1)"; "round(-3,2)" ];
  reads_back "animation-iteration-count"
    [ "sign(-1px)"; "calc(-1)"; "sign(-1px),2" ];
  reads_back "animation" [ "sign(-1px)"; "calc(-1)"; "2s linear calc(1 + 2)" ];
  reads_back "stroke-dasharray" [ "calc(-1px)"; "sign(-1px)"; "calc(1px)" ];
  reads_back "stroke-width" [ "calc(-1px)"; "calc(2px - 3px)"; "sign(-1px)" ];
  reads_back "hyphenate-limit-chars" [ "sign(-1px)"; "calc(-1)"; "calc(4)" ];
  (* The durations the sibling range guard already keeps, pinned here so the
     fixed-point half of the property covers them too. *)
  reads_back "transition-duration" [ "calc(120ms)"; "calc(1s + 200ms)" ];
  reads_back "interest-delay" [ "calc(-1s)" ]

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

let suite =
  ( "values",
    [
      test_case "length" `Quick test_length;
      test_case "angle" `Quick test_angle;
      test_case "duration" `Quick test_duration;
      test_case "percentage" `Quick test_percentage;
      test_case "minified value formatting" `Quick
        test_minified_value_formatting;
      test_case "regular value formatting" `Quick test_regular_value_formatting;
      (* New type tests *)
      test_case "length_percentage" `Quick test_length_percentage;
      test_case "number_percentage" `Quick test_number_percentage;
      test_case "component_values" `Quick test_component_values;
      test_case "invalid_value" `Quick test_invalid_value;
      test_case "number" `Quick test_number;
      test_case "attr_syntax" `Quick test_attr_syntax;
      test_case "attr_type" `Quick test_attr_type;
      test_case "transition_behavior" `Quick test_transition_behavior;
      test_case "spec minified output reads back" `Quick
        spec_minified_output_reads_back;
    ] )
