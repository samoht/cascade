(** Stylesheet tests: units and math functions under minify. *)

open Cascade
open Stylesheet_helpers

(* CSS Values and Units Module Level 4, section 7.2 (Time Units): [s] and [ms]
   are the two units of [<time>], with [1s = 1000ms]. Both minifiers
   canonicalize to the shorter spelling - [0s] for any zero, [1s] over [1000ms],
   [.1s] over [100ms]. *)
let v466_time_unit_canonical () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "0ms canonicalizes to 0s" ".x{transition-duration:0s}"
    (normalize ".x { transition-duration: 0ms }");
  Alcotest.(check string)
    "1000ms canonicalizes to 1s" ".x{transition-duration:1s}"
    (normalize ".x { transition-duration: 1000ms }");
  Alcotest.(check string)
    "100ms canonicalizes to .1s" ".x{transition-duration:.1s}"
    (normalize ".x { transition-duration: 100ms }");
  Alcotest.(check string)
    "500ms canonicalizes to .5s" ".x{transition-duration:.5s}"
    (normalize ".x { transition-duration: 500ms }")

(* CSS Values and Units Module Level 4, section 6.2 (Absolute Lengths): absolute
   lengths ([in], [cm], [mm], [pt], [pc], [q]) are mathematically
   inter-convertible to [px] but the conversion produces non-terminating
   decimals or longer spellings in most cases. Industry minifiers (Lightning CSS
   and cssnano) preserve the source unit; the CSSOM-style specified value should
   round-trip unchanged. *)
let v461_absolute_units_minify () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "10cm preserved" ".x{width:10cm}"
    (normalize ".x { width: 10cm }");
  Alcotest.(check string)
    "1in preserved" ".x{width:1in}"
    (normalize ".x { width: 1in }");
  Alcotest.(check string)
    "10pt preserved" ".x{font-size:10pt}"
    (normalize ".x { font-size: 10pt }");
  Alcotest.(check string)
    "1pc preserved" ".x{width:1pc}"
    (normalize ".x { width: 1pc }");
  Alcotest.(check string)
    "25.4mm preserved" ".x{width:25.4mm}"
    (normalize ".x { width: 25.4mm }")

(* CSS Values and Units Module Level 4, section 5.3 (Numbers): negative lengths
   and percentages are valid in many contexts (margin, transform translate,
   etc.) and must round-trip preserved. The leading [-] is part of the value,
   not a separate token. *)
let v481_negative_units_kept () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "negative -10px preserved" ".x{margin:-10px}"
    (normalize ".x { margin: -10px }");
  Alcotest.(check string)
    "negative -10% preserved" ".x{margin:-10%}"
    (normalize ".x { margin: -10% }");
  Alcotest.(check string)
    "negative -.5em preserved" ".x{margin:-.5em}"
    (normalize ".x { margin: -0.5em }");
  Alcotest.(check string)
    "negative angle -90deg preserved" ".x{transform:rotate(-90deg)}"
    (normalize ".x { transform: rotate(-90deg) }")

(* CSS Values and Units Module Level 4, section 5 (Numeric Data Types): trailing
   zeros after a decimal point are not significant - [10.0px] is the same number
   as [10px], [10.50px] is [10.5px]. Both minifiers normalise to drop trailing
   zeros. *)
let v4_8_trailing_zero_drop () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "10.0px drops trailing zero to 10px" ".x{width:10px}"
    (normalize ".x { width: 10.0px }");
  Alcotest.(check string)
    "10.50px drops trailing zero to 10.5px" ".x{width:10.5px}"
    (normalize ".x { width: 10.50px }");
  Alcotest.(check string)
    "1.0 line-height drops trailing zero to 1" ".x{line-height:1}"
    (normalize ".x { line-height: 1.0 }")

(* CSS Sizing Module Level 4, section 4.1 (aspect-ratio): the [aspect-ratio]
   property accepts [<ratio>] which is two [<number>]s separated by [/]. Both
   minifiers preserve [16/9] (with whitespace dropped) and the single-value form
   [1] (which is shorthand for [1/1]). *)
let sizing4_5_aspect_ratio_preservation () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "aspect-ratio: 16 / 9 -> 16/9 (whitespace dropped)" ".x{aspect-ratio:16/9}"
    (normalize ".x { aspect-ratio: 16 / 9 }");
  Alcotest.(check string)
    "aspect-ratio: 1 preserved" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: 1 }")

(* CSS Values and Units Module Level 4, section 6 + spec rejection: a bare
   number without a unit is not a valid [<length>] outside of zero. Both parsers
   reject [width: 10] (no unit), [margin: 5] (no unit). *)
let v481_negative_unit_length () =
  Alcotest.(check bool)
    "width: 10 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "margin: 5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { margin: 5 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "padding: 1.5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { padding: 1.5 }" with
    | Error _ -> true
    | _ -> false)

(* CSS Values and Units Module Level 4, section 7.2 + spec rejection: a bare
   number is not a valid [<time>] - units [s] or [ms] are required. Exception:
   [0] is valid as zero in some contexts but [transition-duration: 1] (no unit)
   must be rejected. *)
let v466_time_unit_required () =
  Alcotest.(check bool)
    "transition-duration: 1 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { transition-duration: 1 }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "animation-duration: 1.5 (no unit) is rejected" true
    (match Css.of_string ~strict:true ".x { animation-duration: 1.5 }" with
    | Error _ -> true
    | _ -> false)

(* CSS Values and Units Module Level 4, section 6 + spec rejection: unknown
   length units like [10pp], [10foo] must be rejected as parse errors. *)
let v461_unknown_length_unit () =
  Alcotest.(check bool)
    "width: 10pp (unknown unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10pp }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "width: 10foo (unknown unit) is rejected" true
    (match Css.of_string ~strict:true ".x { width: 10foo }" with
    | Error _ -> true
    | _ -> false)

(* {2 Fidelity tests for unit edges} *)

let fidelity_time_unit_preserved () =
  pretty_preserves ".x { transition-duration: 0ms }" [ "0ms" ];
  pretty_preserves ".x { transition-duration: 1000ms }" [ "1000ms" ];
  pretty_preserves ".x { transition-duration: 100ms }" [ "100ms" ];
  pretty_preserves ".x { transition-duration: .5s }" [ ".5s" ]

let fidelity_absolute_units_preserved () =
  pretty_preserves ".x { width: 10cm }" [ "10cm" ];
  pretty_preserves ".x { width: 1in }" [ "1in" ];
  pretty_preserves ".x { font-size: 10pt }" [ "10pt" ];
  pretty_preserves ".x { width: 1pc }" [ "1pc" ];
  pretty_preserves ".x { width: 25.4mm }" [ "25.4mm" ]

let fidelity_negative_units_preserved () =
  pretty_preserves ".x { margin: -10px }" [ "-10px" ];
  pretty_preserves ".x { margin: -10% }" [ "-10%" ];
  pretty_preserves ".x { transform: rotate(-90deg) }" [ "-90deg" ]

let fidelity_trailing_zero_preserved () =
  pretty_preserves ".x { width: 10.0px }" [ "10.0px" ];
  pretty_preserves ".x { width: 10.50px }" [ "10.50px" ];
  pretty_preserves ".x { line-height: 1.0 }" [ "1.0" ]

let fidelity_aspect_ratio_preserved () =
  pretty_preserves ".x { aspect-ratio: 16 / 9 }" [ "16 / 9" ];
  pretty_preserves ".x { aspect-ratio: 1 }" [ "1" ]

(* CSS Values and Units Module Level 4, section 10.10.1 (Simplification): calc()
   simplifies under spec rules - a single-operand calc collapses to the operand;
   multiplication/division/addition with constants in the same unit fold to the
   result; calc() with a [var()] reference must be preserved because the value
   is unknown at parse time. Both Lightning CSS and cssnano agree on these
   simplifications. *)
let v4102_calc_single () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px) }");
  (* CSS Values 4 (ED) sec. 10.3 keeps a [calc()] valid where the property's
     range is exceeded and clamps at used-value time, so [width: calc(-5px)]
     computes to [0px] in Chrome 146 and [width: -5px] is dropped there. The
     call stays on where the range starts at zero and folds where it does
     not. *)
  Alcotest.(check string)
    "calc(-5px) on a non-negative property keeps the call"
    ".x{width:calc(-5px)}"
    (normalize ".x { width: calc(-5px) }");
  Alcotest.(check string)
    "calc(-5px) -> -5px" ".x{margin-left:-5px}"
    (normalize ".x { margin-left: calc(-5px) }");
  Alcotest.(check string)
    "calc(1) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(1) }")

let v4_10_2_calc_arithmetic () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px * 2) -> 2px" ".x{width:2px}"
    (normalize ".x { width: calc(1px * 2) }");
  Alcotest.(check string)
    "calc(10px / 2) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(10px / 2) }");
  Alcotest.(check string)
    "calc(2 * 3) -> 6" ".x{aspect-ratio:6}"
    (normalize ".x { aspect-ratio: calc(2 * 3) }");
  (* A property whose value accepts a bare [<number>] (line-height) must still
     read the [<number>] operands of a calc multiplication as numbers, not as
     typed values that the dimension check rejects. *)
  Alcotest.(check string)
    "line-height calc(2 * 3) -> 6" ".x{line-height:6}"
    (normalize ".x { line-height: calc(2 * 3) }");
  Alcotest.(check string)
    "line-height calc(2px * 1) -> 2px" ".x{line-height:2px}"
    (normalize ".x { line-height: calc(2px * 1) }");
  Alcotest.(check string)
    "calc(1px - 1px) -> 0" ".x{width:0}"
    (normalize ".x { width: calc(1px - 1px) }");
  Alcotest.(check string)
    "calc(1px * 0) -> 0" ".x{width:0}"
    (normalize ".x { width: calc(1px * 0) }");
  Alcotest.(check string)
    "a numeric calc cannot be a width" ""
    (normalize ".x { width: calc(0 + 0) }")

let v4102_calc_addition () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2px + 3px) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(1px + 2px + 3px) }");
  Alcotest.(check string)
    "calc((1px + 2px) * 2) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc((1px + 2px) * 2) }");
  Alcotest.(check string)
    "calc(1px*2 + 3px*4) -> 14px" ".x{width:14px}"
    (normalize ".x { width: calc(1px*2 + 3px*4) }")

let v4102_calc_percentage () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(100% - 50%) -> 50%" ".x{width:50%}"
    (normalize ".x { width: calc(100% - 50%) }");
  Alcotest.(check string)
    "calc(50% + 50%) -> 100%" ".x{width:100%}"
    (normalize ".x { width: calc(50% + 50%) }");
  Alcotest.(check string)
    "calc(100% * 2) -> 200%" ".x{width:200%}"
    (normalize ".x { width: calc(100% * 2) }")

(* CSS Values and Units Module Level 4, section 10.10.1: calc() with mixed units
   that cannot be reduced at parse time must be preserved. Examples are
   [calc(100vh - 50px)] (mixed viewport + absolute), [calc(100% - 10px)] (mixed
   percentage + absolute), and any expression containing [var()] - their values
   are not known until resolution time. *)
let v4102_calc_mixed_unit () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(100vh - 50px) preserved" ".x{width:calc(100vh - 50px)}"
    (normalize ".x { width: calc(100vh - 50px) }");
  Alcotest.(check string)
    "calc(100% - 10px) preserved" ".x{width:calc(100% - 10px)}"
    (normalize ".x { width: calc(100% - 10px) }");
  Alcotest.(check string)
    "calc(var(--x) + 10px) preserved" ".x{width:calc(var(--x) + 10px)}"
    (normalize ".x { width: calc(var(--x) + 10px) }")

(* CSS Values L4, section 10 + spec rejection: invalid calc() forms are parse
   errors. [calc()] (empty), [calc(10px +)] (trailing operator), a calc body
   starting with [*] (leading operator) must all be rejected. *)
let v4_10_invalid_calc_rejected () =
  Alcotest.(check bool)
    "calc() with no arguments is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(10px +) trailing operator is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(10px +) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(* 5) leading operator is rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(* 5) }" with
    | Error _ -> true
    | _ -> false)

(* {2 Fidelity tests for calc edges} *)

let fidelity_calc_simplifiable_preserved () =
  pretty_preserves ".x { width: calc(1px + 2px) }" [ "calc(1px + 2px)" ];
  pretty_preserves ".x { width: calc(1px * 2) }" [ "calc(1px * 2)" ];
  pretty_preserves ".x { width: calc(100% - 50%) }" [ "calc(100% - 50%)" ]

let fidelity_calc_mixed_unit_preserved () =
  pretty_preserves ".x { width: calc(100vh - 50px) }" [ "calc(100vh - 50px)" ];
  pretty_preserves ".x { width: calc(100% - 10px) }" [ "calc(100% - 10px)" ];
  pretty_preserves ".x { width: calc(var(--x) + 10px) }"
    [ "calc(var(--x) + 10px)" ]

(* CSS Values L4 section 10.10.1: nested calc() collapses to a single calc() and
   all-constant nested forms reduce to a single value. Both minifiers fully
   unwrap. *)
let v4102_calc_nested () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(calc(1px) + calc(2px)) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(calc(1px) + calc(2px)) }");
  Alcotest.(check string)
    "calc(calc(calc(1px))) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(calc(calc(1px))) }");
  Alcotest.(check string)
    "calc(calc(1px + 2px) * 2) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(calc(1px + 2px) * 2) }")

(* CSS Values L4 section 10.2 (min(), max()): when all arguments are constants
   reducible at parse time the result is a single value. Per shortest-wins
   cascade picks the reduced form. *)
let v4107_minmax_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "min(1px, 2px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: min(1px, 2px) }");
  Alcotest.(check string)
    "max(1px, 2px) -> 2px" ".x{width:2px}"
    (normalize ".x { width: max(1px, 2px) }");
  Alcotest.(check string)
    "min(min(1px, 2px), 3px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: min(min(1px, 2px), 3px) }");
  (* CSS Values 4 section 10: [clamp(lo, v, hi)] is [max(lo, min(v, hi))], so a
     clamp of three same-unit literals folds to a single value (returning [lo]
     when [lo > hi]). Mixed units cannot be compared and are preserved. *)
  Alcotest.(check string)
    "clamp(1rem, 2rem, 3rem) -> 2rem" ".x{width:2rem}"
    (normalize ".x { width: clamp(1rem, 2rem, 3rem) }");
  Alcotest.(check string)
    "clamp(3rem, 2rem, 1rem) -> 3rem" ".x{width:3rem}"
    (normalize ".x { width: clamp(3rem, 2rem, 1rem) }");
  Alcotest.(check string)
    "clamp(1rem, 2vw, 3rem) stays (mixed units)"
    ".x{width:clamp(1rem,2vw,3rem)}"
    (normalize ".x { width: clamp(1rem, 2vw, 3rem) }");
  Alcotest.(check string)
    "font-size folds a constant clamp too" ".x{font-size:2rem}"
    (normalize ".x { font-size: clamp(1rem, 2rem, 3rem) }")

(* {2 Fidelity tests for nested and var edges} *)

let fidelity_nested_calc_preserved () =
  pretty_preserves ".x { width: calc(calc(1px) + calc(2px)) }"
    [ "calc(calc(1px)" ];
  pretty_preserves ".x { width: calc(calc(calc(1px))) }"
    [ "calc(calc(calc(1px)))" ]

let fidelity_min_max_preserved () =
  pretty_preserves ".x { width: min(1px, 2px) }" [ "min(1px, 2px)" ];
  pretty_preserves ".x { width: max(1px, 2px) }" [ "max(1px, 2px)" ]

(* {2 Calc simplification edges (CSS Values L4 section 10)} *)

let v4_10_calc_negative_result () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(5px - 10px) -> -5px" ".x{margin:-5px}"
    (normalize ".x { margin: calc(5px - 10px) }");
  Alcotest.(check string)
    "calc(-1px + 2px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(-1px + 2px) }")

let v4_10_7_operator_precedence () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2 * 3px) -> 7px" ".x{width:7px}"
    (normalize ".x { width: calc(1px + 2 * 3px) }");
  Alcotest.(check string)
    "calc((1 + 2) * 3px) -> 9px" ".x{width:9px}"
    (normalize ".x { width: calc((1 + 2) * 3px) }")

let v4_10_7_identity_operations () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1 * 5px) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(1 * 5px) }");
  Alcotest.(check string)
    "calc(5px * 1) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(5px * 1) }");
  Alcotest.(check string)
    "calc(10px / 1) -> 10px" ".x{width:10px}"
    (normalize ".x { width: calc(10px / 1) }");
  Alcotest.(check string)
    "calc(0 * 100%) keeps its percentage type" ".x{width:0%}"
    (normalize ".x { width: calc(0 * 100%) }")

let v4_10_7_percentage_arithmetic () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(50% * 2) -> 100%" ".x{width:100%}"
    (normalize ".x { width: calc(50% * 2) }");
  Alcotest.(check string)
    "calc(50% / 2) -> 25%" ".x{width:25%}"
    (normalize ".x { width: calc(50% / 2) }");
  Alcotest.(check string)
    "calc(100% - 10% - 20%) -> 70%" ".x{width:70%}"
    (normalize ".x { width: calc(100% - 10% - 20%) }");
  Alcotest.(check string)
    "calc(100% - (10% + 20%)) -> 70%" ".x{width:70%}"
    (normalize ".x { width: calc(100% - (10% + 20%)) }")

let v4_10_7_chained_multiplicative () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px * 2 * 3) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(1px * 2 * 3) }");
  Alcotest.(check string)
    "calc(24px / 2 / 3) -> 4px" ".x{width:4px}"
    (normalize ".x { width: calc(24px / 2 / 3) }");
  Alcotest.(check string)
    "calc(1px*2 - 1px) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(1px*2 - 1px) }")

let v4_10_7_double_negative () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px - -2px) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(1px - -2px) }")

let v4107_mixed_units () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px + 2em) preserved" ".x{width:calc(1px + 2em)}"
    (normalize ".x { width: calc(1px + 2em) }");
  Alcotest.(check string)
    "calc(1px + 1ch) preserved" ".x{width:calc(1px + 1ch)}"
    (normalize ".x { width: calc(1px + 1ch) }");
  Alcotest.(check string)
    "calc(1rem + 2px) preserved" ".x{width:calc(1rem + 2px)}"
    (normalize ".x { width: calc(1rem + 2px) }")

let v4107_math_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(abs(-5px)) -> 5px" ".x{width:5px}"
    (normalize ".x { width: calc(abs(-5px)) }");
  Alcotest.(check string)
    "calc(min(1px, 2px, 3px)) -> 1px" ".x{width:1px}"
    (normalize ".x { width: calc(min(1px, 2px, 3px)) }");
  Alcotest.(check string)
    "calc(max(1px, 2px, 3px)) -> 3px" ".x{width:3px}"
    (normalize ".x { width: calc(max(1px, 2px, 3px)) }")

(* CSS Values 4 sec. 10.7: [abs()] and [hypot()] return their argument's type,
   and sec. 10.9 gives the inverse trig functions an [<angle>]. A [calc()]
   wrapper around one reduced it to a bare coefficient, so [calc(hypot(1px,
   1px))] printed [1.41421356], which is not a [<length>] and is a declaration
   the reader drops. Each expected form is reparsed to prove it survives. *)
let v4107_typed_math_fn_units () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [reparses]") expected (normalize expected))
    [
      ( "calc(hypot(3px, 4px)) -> 5px",
        ".x { width: calc(hypot(3px, 4px)) }",
        ".x{width:5px}" );
      ( "calc(hypot(1px, 1px)) -> 1.41421px",
        ".x { width: calc(hypot(1px, 1px)) }",
        ".x{width:1.41421px}" );
      ( "calc(1px + hypot(3px, 4px)) -> 6px",
        ".x { width: calc(1px + hypot(3px, 4px)) }",
        ".x{width:6px}" );
      ( "calc(hypot(3em, 4em)) -> 5em",
        ".x { width: calc(hypot(3em, 4em)) }",
        ".x{width:5em}" );
      ( "calc(hypot(3%, 4%)) -> 5%",
        ".x { width: calc(hypot(3%, 4%)) }",
        ".x{width:5%}" );
      (* Both arguments are a [<length>], so the call is well typed, but [px]
         and [em] resolve against a font size only the browser knows. *)
      ( "calc(hypot(1px, 1em)) keeps the call",
        ".x { width: calc(hypot(1px, 1em)) }",
        ".x{width:calc(hypot(1px,1em))}" );
      ( "calc(abs(-3px)) -> 3px",
        ".x { width: calc(abs(-3px)) }",
        ".x{width:3px}" );
      (* A [<number>] argument keeps a [<number>] result, which scales a length
         rather than becoming one. *)
      ( "calc(hypot(3, 4) * 1px) -> 5px",
        ".x { width: calc(hypot(3, 4) * 1px) }",
        ".x{width:5px}" );
      ( "calc(hypot(3s, 4s)) -> 5s",
        ".x { transition-duration: calc(hypot(3s, 4s)) }",
        ".x{transition-duration:5s}" );
      ( "calc(abs(-3s)) -> 3s",
        ".x { transition-duration: calc(abs(-3s)) }",
        ".x{transition-duration:3s}" );
      ( "calc(hypot(3deg, 4deg)) -> 5deg",
        ".x { transform: rotate(calc(hypot(3deg, 4deg))) }",
        ".x{transform:rotate(5deg)}" );
      ( "calc(abs(-3deg)) -> 3deg",
        ".x { transform: rotate(calc(abs(-3deg))) }",
        ".x{transform:rotate(3deg)}" );
      ( "calc(atan2(1, 1)) -> 45deg",
        ".x { transform: rotate(calc(atan2(1, 1))) }",
        ".x{transform:rotate(45deg)}" );
      ( "calc(30deg + atan2(1, 1)) -> 75deg",
        ".x { transform: rotate(calc(30deg + atan2(1, 1))) }",
        ".x{transform:rotate(75deg)}" );
      (* [opacity] folds its [calc()] through the untyped numeric reduction,
         which has no unit to rebuild, so the call stays rather than becoming
         the [30] that a browser clamps to full opacity. *)
      ( "calc(abs(-30%)) on an opacity keeps the call",
        ".x { opacity: calc(abs(-30%)) }",
        ".x{opacity:calc(abs(-30%))}" );
    ]

let v4107_numeric_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(sqrt(16)) -> 4" ".x{aspect-ratio:4}"
    (normalize ".x { aspect-ratio: calc(sqrt(16)) }");
  Alcotest.(check string)
    "calc(pow(2, 3)) -> 8" ".x{aspect-ratio:8}"
    (normalize ".x { aspect-ratio: calc(pow(2, 3)) }");
  Alcotest.(check string)
    "calc(hypot(3, 4)) -> 5" ".x{aspect-ratio:5}"
    (normalize ".x { aspect-ratio: calc(hypot(3, 4)) }");
  Alcotest.(check string)
    "calc(sign(5)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(sign(5)) }")

let v4107_math_product_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input))
    [
      ( "calc(100px * hypot(3, 4)) -> 500px",
        ".x { width: calc(100px * hypot(3, 4)) }",
        ".x{width:500px}" );
      ( "calc(1px * pow(2, sqrt(100))) -> 1024px",
        ".x { width: calc(1px * pow(2, sqrt(100))) }",
        ".x{width:1024px}" );
      ( "calc(100px * pow(2, pow(2, 2))) -> 1600px",
        ".x { width: calc(100px * pow(2, pow(2, 2))) }",
        ".x{width:1600px}" );
      ( "calc(1px * log(1)) -> 0",
        ".x { width: calc(1px * log(1)) }",
        ".x{width:0}" );
      ( "calc(1px * log(10, 10)) -> 1px",
        ".x { width: calc(1px * log(10, 10)) }",
        ".x{width:1px}" );
      ( "calc(1px * exp(0)) -> 1px",
        ".x { width: calc(1px * exp(0)) }",
        ".x{width:1px}" );
      ( "calc(1px * log(e)) -> 1px",
        ".x { width: calc(1px * log(e)) }",
        ".x{width:1px}" );
      ( "calc(1px * (e - exp(1))) -> 0",
        ".x { width: calc(1px * (e - exp(1))) }",
        ".x{width:0}" );
      ( "calc(2px * pi) -> 6.28319px",
        ".x { width: calc(2px * pi) }",
        ".x{width:6.28319px}" );
      ( "calc(100px * pi) -> 314.159px",
        ".x { width: calc(100px * pi) }",
        ".x{width:314.159px}" );
      ( "calc(2px / pi) -> .63662px",
        ".x { width: calc(2px / pi) }",
        ".x{width:.63662px}" );
      ( "calc(100px * sin(45deg)) stays symbolic",
        ".x { width: calc(100px * sin(45deg)) }",
        ".x{width:calc(100px*sin(45deg))}" );
      ( "calc(100px * sin(.125turn)) stays symbolic",
        ".x { width: calc(100px * sin(.125turn)) }",
        ".x{width:calc(100px*sin(.125turn))}" );
      ( "calc(100px * sin(pi / 4)) stays symbolic",
        ".x { width: calc(100px * sin(pi / 4)) }",
        ".x{width:calc(100px*sin(pi/4))}" );
      ( "calc(100px * sin(22deg + 23deg)) stays symbolic",
        ".x { width: calc(100px * sin(22deg + 23deg)) }",
        ".x{width:calc(100px*sin(45deg))}" );
      ( "calc(2px * cos(45deg)) stays symbolic",
        ".x { width: calc(2px * cos(45deg)) }",
        ".x{width:calc(2px*cos(45deg))}" );
      ( "calc(2px * tan(45deg)) -> 2px",
        ".x { width: calc(2px * tan(45deg)) }",
        ".x{width:2px}" );
    ]

(* CSS Values 4 sec. 10.13 leaves numeric precision implementation-defined, so
   the budget is Cascade's own: six significant figures for a value Cascade
   computes, and every digit the author wrote for one it did not. The two are
   different values - at a [14px] font [.4285714em] is [6px] while [.428571em]
   is [5.999994px] - so the reduction happens at the fold that produces the
   irrational, never at print time. *)
let authored_precision_preserved () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  (* An [@property --n] registration, and the minified form it prints as, so the
     [--n] declaration under it is read as a typed [<number>]. *)
  let registered decl =
    String.concat ""
      [
        "@property --n { syntax: \"<number>\"; inherits: false; initial-value: \
         0 }";
        decl;
      ]
  in
  let registered_min decl =
    String.concat ""
      [
        "@property --n{syntax:\"<number>\";inherits:false;initial-value:0}";
        decl;
      ]
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [idempotent]") expected (normalize expected))
    [
      (* Authored dimensions past six significant figures. *)
      ( "authored em keeps its digits",
        ".x { padding-left: .4285714em }",
        ".x{padding-left:.4285714em}" );
      ( "authored px keeps its magnitude",
        ".x { width: 999999999px }",
        ".x{width:999999999px}" );
      ( "authored time keeps its digits",
        ".x { transition-duration: 1.2345678s }",
        ".x{transition-duration:1.2345678s}" );
      ( "authored angle keeps its digits",
        ".x { transform: rotate(1.2345678deg) }",
        ".x{transform:rotate(1.2345678deg)}" );
      ( "min() keeps the authored operand it selects",
        ".x { width: min(1.41421356px, 2px) }",
        ".x{width:1.41421356px}" );
      (* A folded irrational carries the digits Cascade commits to. *)
      ( "calc(2px * pi) rounds at the fold",
        ".x { width: calc(2px * pi) }",
        ".x{width:6.28319px}" );
      ( "calc(2px / pi) rounds at the fold",
        ".x { width: calc(2px / pi) }",
        ".x{width:.63662px}" );
      ( "calc(1px * sqrt(2)) rounds at the fold",
        ".x { width: calc(1px * sqrt(2)) }",
        ".x{width:1.41421px}" );
      ( "hypot() on lengths rounds at the fold",
        ".x { width: hypot(1px, 1px) }",
        ".x{width:1.41421px}" );
      (* [<number>] is untouched in both directions. *)
      ( "authored line-height keeps its digits",
        ".x { line-height: 1.4285714 }",
        ".x{line-height:1.4285714}" );
      ( "authored opacity keeps its digits",
        ".x { opacity: .12345678 }",
        ".x{opacity:.12345678}" );
      ( "authored unregistered custom property keeps its digits",
        ".x { --raw: 1.4285714 }",
        ".x{--raw:1.4285714}" );
      (* CSS Properties and Values API 1 sec. 2: an [@property] registration
         lifts a [--name] use into the typed [<number>] shape, which must not
         change the author's digits either. *)
      ( "registered <number> keeps its digits",
        registered ".x { --n: 1.4285714 }",
        registered_min ".x{--n:1.4285714}" );
      ( "registered <number> keeps its magnitude",
        registered ".x { --n: 999999999999 }",
        registered_min ".x{--n:999999999999}" );
      ( "registered <number> shortens the fold the printer runs",
        registered ".x { --n: calc(1 / 3) }",
        registered_min ".x{--n:.333333}" );
      (* A [calc()] the optimizer already collapsed reaches the printer as a
         bare coefficient, where the registered shape has to print what an
         untyped [<number>] prints. *)
      ( "registered <number> prints a collapsed fold like an untyped one",
        registered ".x { --n: calc(2 * pi) }",
        registered_min ".x{--n:6.28318531}" );
      ( "untyped <number> prints the same collapsed fold",
        ".x { line-height: calc(2 * pi) }",
        ".x{line-height:6.28318531}" );
    ]

(* The six-significant-figure budget buys a fractional tail, and past 10^6 it
   stops reaching one: the only digits left to spend are integer ones the
   arithmetic got right. [1in] is exactly [96px] and [1pt] exactly [4/3px] (CSS
   Values 4 sec. 6.2), so [calc(1in + 999999999px)] is [1000000095px] and
   rounding it to [1000000000px] moves the box by 95px. A value small enough for
   the budget to reach its fraction still pays it: [1cm] is [4800/127px], whose
   tail is Cascade's own noise. *)
let computed_precision_keeps_integer_digits () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (normalize input);
      Alcotest.(check string)
        (name ^ " [idempotent]") expected (normalize expected))
    [
      (* An absolute-unit combine past 10^6 keeps every digit. *)
      ( "in + px keeps the whole magnitude",
        ".x { width: calc(1in + 999999999px) }",
        ".x{width:1000000095px}" );
      ( "in + px keeps a mid-range magnitude",
        ".x { width: calc(1in + 12345678px) }",
        ".x{width:12345774px}" );
      ( "pc + px keeps the whole magnitude",
        ".x { width: calc(1pc + 1000000px) }",
        ".x{width:1000016px}" );
      ( "pt + px keeps the third of a pixel",
        ".x { width: calc(1pt + 1000000px) }",
        ".x{width:1000001.33333333px}" );
      ( "hypot() past 10^6 keeps the whole magnitude",
        ".x { width: hypot(999999999px, 1px) }",
        ".x{width:999999999px}" );
      ( "a folded irrational past 10^6 keeps its integer digits",
        ".x { width: calc(1000000px * pi) }",
        ".x{width:3141592.65358979px}" );
      (* Under 10^6 the budget still spends the tail. *)
      ( "cm + px spends the tail",
        ".x { width: calc(1cm + 1px) }",
        ".x{width:38.7953px}" );
      ( "mm + px spends the tail",
        ".x { width: calc(1mm + 1px) }",
        ".x{width:4.77953px}" );
      ( "q + px spends the tail",
        ".x { width: calc(1q + 1px) }",
        ".x{width:1.94488px}" );
      ( "a non-Pythagorean hypot() spends the tail",
        ".x { width: hypot(1px, 2px) }",
        ".x{width:2.23607px}" );
      ( "a Pythagorean hypot() has no tail to spend",
        ".x { width: hypot(3px, 4px) }",
        ".x{width:5px}" );
    ]

let v4107_mod_rem () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(mod(7, 3)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(mod(7, 3)) }");
  Alcotest.(check string)
    "calc(rem(7, 3)) -> 1" ".x{aspect-ratio:1}"
    (normalize ".x { aspect-ratio: calc(rem(7, 3)) }")

let v4_10_7_round_reduction () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(round(5.5px, 1px)) -> 6px" ".x{width:6px}"
    (normalize ".x { width: calc(round(5.5px, 1px)) }")

let v4107_division_zero () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "calc(1px / 0) preserved (the [/] operator prints without surrounding \
     whitespace per shortest-wins)"
    ".x{width:calc(1px/0)}"
    (normalize ".x { width: calc(1px / 0) }")

let v410_invalid_calc_extra () =
  Alcotest.(check bool)
    "calc(1px ++ 2px) double operator rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px ++ 2px) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(1px 2px) missing operator rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px 2px) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "calc(1px + + 2px) two plus rejected" true
    (match Css.of_string ~strict:true ".x { width: calc(1px + + 2px) }" with
    | Error _ -> true
    | _ -> false)

let fidelity_calc_negative_preserved () =
  pretty_preserves ".x { margin: calc(5px - 10px) }" [ "calc(5px - 10px)" ];
  pretty_preserves ".x { width: calc(1px - -2px) }" [ "calc(1px - -2px)" ]

let fidelity_calc_precedence_preserved () =
  pretty_preserves ".x { width: calc(1px + 2 * 3px) }" [ "1px + 2 * 3px" ];
  pretty_preserves ".x { width: calc((1 + 2) * 3px) }" [ "(1 + 2) * 3px" ]

let fidelity_calc_chained_preserved () =
  pretty_preserves ".x { width: calc(1px * 2 * 3) }" [ "1px * 2 * 3" ];
  pretty_preserves ".x { width: calc(100% - 10% - 20%) }" [ "100% - 10% - 20%" ]

let fidelity_calc_mixed_relative_preserved () =
  pretty_preserves ".x { width: calc(1px + 2em) }" [ "calc(1px + 2em)" ];
  pretty_preserves ".x { width: calc(1px + 1ch) }" [ "calc(1px + 1ch)" ];
  pretty_preserves ".x { width: calc(1rem + 2px) }" [ "calc(1rem + 2px)" ]

let fidelity_calc_math_functions_preserved () =
  pretty_preserves ".x { width: calc(abs(-5px)) }" [ "abs(-5px)" ];
  pretty_preserves ".x { width: calc(min(1px, 2px, 3px)) }"
    [ "min(1px, 2px, 3px)" ];
  (* sqrt() and pow() return unitless numbers, so they need a [<number>]-typed
     property like [aspect-ratio]. *)
  pretty_preserves ".x { aspect-ratio: calc(sqrt(16)) }" [ "sqrt(16)" ];
  pretty_preserves ".x { aspect-ratio: calc(pow(2, 3)) }" [ "pow(2, 3)" ]

let fidelity_calc_math_product_preserved () =
  let remove_spaces s =
    String.to_seq s
    |> Seq.filter (function ' ' | '\n' | '\r' | '\t' -> false | _ -> true)
    |> String.of_seq
  in
  let pretty_preserves_form css fragment =
    match Css.of_string ~strict:false css with
    | Error _ -> Alcotest.failf "failed to parse: %s" css
    | Ok parsed ->
        let printed = Css.to_string parsed.stylesheet in
        Alcotest.(check bool)
          (Fmt.str "non-minified output preserves math form [%s] in [%s]"
             fragment printed)
          true
          (Astring.String.is_infix ~affix:(remove_spaces fragment)
             (remove_spaces printed))
  in
  List.iter
    (fun (css, fragment) -> pretty_preserves_form css fragment)
    [
      (".x { width: calc(100px * hypot(3, 4)) }", "calc(100px * hypot(3, 4))");
      ( ".x { width: calc(1px * pow(2, sqrt(100))) }",
        "calc(1px * pow(2, sqrt(100)))" );
      ( ".x { width: calc(100px * pow(2, pow(2, 2))) }",
        "calc(100px * pow(2, pow(2, 2)))" );
      (".x { width: calc(1px * log(1)) }", "calc(1px * log(1))");
      (".x { width: calc(1px * log(10, 10)) }", "calc(1px * log(10, 10))");
      (".x { width: calc(1px * exp(0)) }", "calc(1px * exp(0))");
      (".x { width: calc(1px * log(e)) }", "calc(1px * log(e))");
      (".x { width: calc(1px * (e - exp(1))) }", "calc(1px * (e - exp(1)))");
      (".x { width: calc(2px * pi) }", "calc(2px * pi)");
      (".x { width: calc(2px / pi) }", "calc(2px / pi)");
      (".x { width: calc(100px * sin(45deg)) }", "calc(100px * sin(45deg))");
      ( ".x { width: calc(100px * sin(.125turn)) }",
        "calc(100px * sin(.125turn))" );
      (".x { width: calc(100px * sin(pi / 4)) }", "calc(100px * sin(pi / 4))");
      ( ".x { width: calc(100px * sin(22deg + 23deg)) }",
        "calc(100px * sin(22deg + 23deg))" );
      (".x { width: calc(2px * cos(45deg)) }", "calc(2px * cos(45deg))");
      (".x { width: calc(2px * tan(45deg)) }", "calc(2px * tan(45deg))");
    ]

let suite =
  ( "stylesheet_calc",
    [
      ( "spec values 4 6.1 absolute units preserved under minify",
        `Quick,
        v461_absolute_units_minify );
      ( "spec values 4 6.6 time unit canonicalization",
        `Quick,
        v466_time_unit_canonical );
      ( "spec values 4 8.1 negative units preserved",
        `Quick,
        v481_negative_units_kept );
      ("spec values 4 8 trailing zero drop", `Quick, v4_8_trailing_zero_drop);
      ( "spec sizing 4 5 aspect-ratio preservation",
        `Quick,
        sizing4_5_aspect_ratio_preservation );
      ( "spec values 4 8.1 unit required for length (negative)",
        `Quick,
        v481_negative_unit_length );
      ( "spec values 4 6.6 time unit required (negative)",
        `Quick,
        v466_time_unit_required );
      ( "spec values 4 6.1 unknown length unit rejected (negative)",
        `Quick,
        v461_unknown_length_unit );
      ("fidelity time unit preserved", `Quick, fidelity_time_unit_preserved);
      ( "fidelity absolute units preserved",
        `Quick,
        fidelity_absolute_units_preserved );
      ( "fidelity negative units preserved",
        `Quick,
        fidelity_negative_units_preserved );
      ( "fidelity trailing zero preserved",
        `Quick,
        fidelity_trailing_zero_preserved );
      ( "fidelity aspect-ratio preserved",
        `Quick,
        fidelity_aspect_ratio_preserved );
      ("spec values 4 10.2 calc single operand", `Quick, v4102_calc_single);
      ("spec values 4 10.2 calc arithmetic", `Quick, v4_10_2_calc_arithmetic);
      ("spec values 4 10.2 calc same-unit addition", `Quick, v4102_calc_addition);
      ( "spec values 4 10.2 calc same-unit percentage",
        `Quick,
        v4102_calc_percentage );
      ( "spec values 4 10.2 calc mixed-unit preserved",
        `Quick,
        v4102_calc_mixed_unit );
      ( "spec values 4 10 invalid calc rejected (negative)",
        `Quick,
        v4_10_invalid_calc_rejected );
      ( "fidelity calc simplifiable preserved",
        `Quick,
        fidelity_calc_simplifiable_preserved );
      ( "fidelity calc mixed-unit preserved",
        `Quick,
        fidelity_calc_mixed_unit_preserved );
      ("spec values 4 10.2 calc nested collapse", `Quick, v4102_calc_nested);
      ( "spec values 4 10.7 min/max constant reduction",
        `Quick,
        v4107_minmax_reduction );
      ("fidelity nested calc preserved", `Quick, fidelity_nested_calc_preserved);
      ("fidelity min/max preserved", `Quick, fidelity_min_max_preserved);
      ( "spec values 4 10 calc negative result",
        `Quick,
        v4_10_calc_negative_result );
      ( "spec values 4 10.7 operator precedence",
        `Quick,
        v4_10_7_operator_precedence );
      ( "spec values 4 10.7 identity operations",
        `Quick,
        v4_10_7_identity_operations );
      ( "spec values 4 10.7 percentage arithmetic",
        `Quick,
        v4_10_7_percentage_arithmetic );
      ( "spec values 4 10.7 chained multiplicative",
        `Quick,
        v4_10_7_chained_multiplicative );
      ("spec values 4 10.7 double negative", `Quick, v4_10_7_double_negative);
      ( "spec values 4 10.7 mixed relative units preserved",
        `Quick,
        v4107_mixed_units );
      ( "spec values 4 10.7 math functions reduction",
        `Quick,
        v4107_math_reduction );
      ( "spec values 4 10.7 numeric math reduction",
        `Quick,
        v4107_numeric_reduction );
      ( "spec values 4 10.7 typed math function units",
        `Quick,
        v4107_typed_math_fn_units );
      ( "spec values 4 10.7 length times math reduction",
        `Quick,
        v4107_math_product_reduction );
      ( "authored numeric precision preserved",
        `Quick,
        authored_precision_preserved );
      ( "computed numeric precision keeps integer digits",
        `Quick,
        computed_precision_keeps_integer_digits );
      ("spec values 4 10.7 mod/rem reduction", `Quick, v4107_mod_rem);
      ("spec values 4 10.7 round reduction", `Quick, v4_10_7_round_reduction);
      ( "spec values 4 10.7 division by zero preserved",
        `Quick,
        v4107_division_zero );
      ( "spec values 4 10 invalid calc extra rejected (negative)",
        `Quick,
        v410_invalid_calc_extra );
      ( "fidelity calc negative preserved",
        `Quick,
        fidelity_calc_negative_preserved );
      ( "fidelity calc precedence preserved",
        `Quick,
        fidelity_calc_precedence_preserved );
      ( "fidelity calc chained preserved",
        `Quick,
        fidelity_calc_chained_preserved );
      ( "fidelity calc mixed relative preserved",
        `Quick,
        fidelity_calc_mixed_relative_preserved );
      ( "fidelity calc math functions preserved",
        `Quick,
        fidelity_calc_math_functions_preserved );
      ( "fidelity calc length times math functions preserved",
        `Quick,
        fidelity_calc_math_product_preserved );
    ] )
