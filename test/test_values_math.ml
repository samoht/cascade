open Alcotest
open Cascade
open Css.Values
open Css_test_helpers
open Values_helpers

let check_calc_op = check_value_cursor "calc_op" read_calc_op pp_calc_op

let check_math_const =
  check_value_cursor "math_const" read_math_const pp_math_const

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

(* Parenthesis removal is an AST normalization, not a printer choice. The
   printer preserves the parsed tree; normalization drops only groups whose
   removal does not change precedence or associativity. *)
let calc_parenthesis_layering () =
  let render value = Css.Pp.to_string ~minify:true pp_length value in
  let parse source = read_length (Cursor.of_string source) in
  let cases =
    [
      ( "calc((1px + var(--a)) - 3px)",
        "calc((1px + var(--a)) - 3px)",
        "calc(1px + var(--a) - 3px)" );
      ( "calc((1px - var(--a)) * 3)",
        "calc((1px - var(--a))*3)",
        "calc((1px - var(--a))*3)" );
      ( "calc((100% - var(--a)) / 2)",
        "calc((100% - var(--a))/2)",
        "calc((100% - var(--a))/2)" );
      ( "calc(1px - (var(--a) + 3px))",
        "calc(1px - (var(--a) + 3px))",
        "calc(1px - (var(--a) + 3px))" );
    ]
  in
  List.iter
    (fun (source, printed, optimized) ->
      let parsed = parse source in
      check string "printer preserves parsed groups" printed (render parsed);
      check string "normalizer owns redundant groups" optimized
        (render (normalize_length parsed)))
    cases

(* Not a roundtrip test *)
let test_var_default_inline () =
  (* When inline printing is enabled and a default is present, pp_var should
     inline the default value instead of var(). *)
  let v : length var = var_ref ~default:(Px 10.) "spacing" in
  let len : length = Var v in
  let s = Css.Pp.to_string ~minify:true ~inline:true pp_length len in
  check string "inline var default" "10px" s

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

let test_calc_op () =
  (* Per CSS Values 4 section 10.8 the [+] and [-] operators require surrounding
     whitespace; [*] and [/] do not. Per shortest-wins the printer omits the
     spaces around [*] and [/] (matches cssnano). *)
  check_calc_op ~expected:" + " "+";
  check_calc_op ~expected:" - " "-";
  check_calc_op "*";
  check_calc_op "/";
  neg_cursor read_calc_op "invalid";
  neg_cursor read_calc_op "abc";
  neg_cursor ~allow_partial:true read_calc_op "++";
  neg_cursor read_calc_op "";
  neg_cursor read_calc_op "="

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
  (* CSS Color 5 sec. 3: the color-mix weight may be a [calc()]. A constant
     folds to a plain percentage; a [var()]-bearing one is kept verbatim. The
     weight reader previously rejected any [calc()] here. *)
  check_color ~expected:"color-mix(in srgb,red 30%,blue)"
    ~optimized:"color-mix(in srgb,red 30%,#00f)"
    "color-mix(in srgb, red calc(30%), blue)";
  check_color ~expected:"color-mix(in srgb,var(--c) calc(var(--o)*100%),blue)"
    ~optimized:"color-mix(in srgb,var(--c) calc(var(--o)*100%),#00f)"
    "color-mix(in srgb, var(--c) calc(var(--o) * 100%), blue)";
  (* A percentage token in a var fallback is still a typed color-mix weight, so
     it takes the same numeric minification as a top-level weight. *)
  check_color ~expected:"color-mix(in srgb,red var(--p,30%),blue)"
    ~optimized:"color-mix(in srgb,red var(--p,30%),#00f)"
    "color-mix(in srgb, red var(--p, 30.0%), blue)";
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
  check_color ~expected:"hsl(0 50%50%)" ~optimized:"#bf4040" "hsl(none 50% 50%)";
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
  (* CSS Color 4 (ED) sec. 5.1 gives rgb() and rgba() the same grammar and its
     Changes section calls them aliases of each other, and sec. 16.2.2 uses the
     rgb() form wherever the alpha is implicit. A var() standing for the whole
     component list carries no alpha of its own, so the two spellings are one
     value and rgb() is the shorter. *)
  decl_optimizes ~prop:"color" ~held:"rgb(var(--x))" ~into:"rgb(var(--x))"
    "rgba(var(--x))";

  (* CSS Values 4 (ED) sec. 10.3: "round(<rounding-strategy>?, A, B)", and the
     strategy defaults to [nearest], so that one alone is the spelling the
     shorter form already carries and the other three have to be printed. Every
     round() the suite prints elsewhere folds to a literal first, so the printer
     only ever sees [nearest]: these keep a var() in the value so the call
     survives to the output. *)
  check_length "round(up,var(--x),1px)";
  check_length "round(down,var(--x),1px)";
  check_length "round(to-zero,var(--x),1px)";
  check_length ~expected:"round(var(--x),1px)" "round(nearest,var(--x),1px)";
  (* Each numeric type prints round() itself, so each needs its own case: the
     length, duration and angle printers all default the strategy away. *)
  check_duration "round(up,var(--x),1s)";
  check_duration ~expected:"round(var(--x),1s)" "round(nearest,var(--x),1s)";
  check_angle "round(to-zero,var(--x),1deg)";
  check_angle ~expected:"round(var(--x),1deg)" "round(nearest,var(--x),1deg)";

  (* The printer serialises a dimension to six significant digits, so a fold
     whose result needs more of them would print as a value the input did not
     carry: [1px / 512] is [.001953125px] exactly, and the literal
     [0.001953125px] prints [.00195313px]. CSS Values 4 (ED) sec. 10.13 asks a
     math function to serialise its own arguments, so keeping the call is what
     keeps the value, and the fold's precision budget has to be the printer's
     rather than a number of its own. *)
  decl_optimizes ~prop:"width" ~held:"calc(1px/512)" ~into:"calc(1px/512)"
    "calc(1px / 512)";
  decl_optimizes ~prop:"width" ~held:"calc(1px/64)" ~into:".015625px"
    "calc(1px / 64)";

  (* CSS Values 4 (ED) sec. 10.6: "The sign(A) function ... returns -1 if A's
     numeric value is negative, +1 if A's numeric value is positive, 0+ if A's
     numeric value is 0+, and 0- if A's numeric value is 0-." Zero is its own
     answer rather than the negative one, and margin takes the result either
     way, so the fold is visible where a non-negative property would hide it. *)
  decl_optimizes ~prop:"margin" ~held:"calc(sign(0)*1px)" ~into:"0"
    "calc(sign(0) * 1px)";
  decl_optimizes ~prop:"margin" ~held:"calc(sign(5)*1px)" ~into:"1px"
    "calc(sign(5) * 1px)";
  decl_optimizes ~prop:"margin" ~held:"calc(sign(-5)*1px)" ~into:"-1px"
    "calc(sign(-5) * 1px)";
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

(* CSS Values 4 (ED) sec. 10.12 checks a property's range on the value a math
   function resolves to rather than on each operand, the exception [calc()]
   already had: a negative operand under a non-negative property is fine. *)
let spec_math_operand_range () =
  (* <length> and <length-percentage>, all at properties whose range is
     [0,inf]. *)
  decl_optimizes ~prop:"width" ~held:"abs(-1px)" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"padding-top" ~held:"abs(-1px)" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"font-size" ~held:"abs(-1px)" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"border-top-left-radius" ~held:"abs(-1px)" ~into:"1px"
    "abs(-1px)";
  decl_optimizes ~prop:"width" ~held:"mod(-5px,2px)" ~into:"1px"
    "mod(-5px, 2px)";
  (* Sec. 10.6 gives [abs(A)] the type of its argument and [sign(A)] a <number>
     whatever the argument is, so [abs()] stands at a <length> and [sign()]
     never does. *)
  neg_cursor read_length "sign(10px)";
  neg_cursor read_length "sign(-1px)";
  neg_cursor read_length_percentage "sign(-1px)"

(* Sec. 10.1: a math function "can be used wherever a <number>, <dimension>, or
   <percentage> is allowed", so a bare call reads wherever the same call wrapped
   in [calc()] does; [calc()] is one math function among several, not the gate
   to the others. *)
let spec_bare_math_functions () =
  (* The [sign()] sec. 10.6 keeps out of a <length> is exactly what an
     <opacity-value> takes. *)
  decl_optimizes ~prop:"opacity" ~into:"-1" "sign(-1px)";
  decl_optimizes ~prop:"opacity" ~into:"8" "pow(2,3)";
  decl_optimizes ~prop:"opacity" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"opacity" ~into:"5" "hypot(3,4)";
  (* A bare call and a [calc()]-wrapped one agree on acceptance and on the value
     they fold to. *)
  decl_optimizes ~prop:"opacity" ~into:".5" "abs(-.5)";
  decl_optimizes ~prop:"opacity" ~into:".5" "calc(abs(-.5))";
  decl_optimizes ~prop:"zoom" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"flex-grow" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"font-weight" ~into:"8" "pow(2,3)";
  decl_optimizes ~prop:"aspect-ratio" ~into:"2" "sqrt(4)";
  (* Sec. 10.9: a math function resolving to <number> stands wherever an
     <integer> is accepted. *)
  decl_optimizes ~prop:"z-index" ~into:"1" "abs(-1)";
  decl_optimizes ~prop:"order" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"grid-row-start" ~into:"1" "abs(-1)";
  (* CSS Backgrounds 3 sec. 3.3 spells <line-width> without a percentage, which
     is a narrower slot than <length> but still one a math function stands
     in. *)
  decl_optimizes ~prop:"border-top-width" ~into:"5px" "hypot(3px,4px)";
  decl_optimizes ~prop:"outline-width" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"column-rule-width" ~into:"1px" "abs(-1px)"

(* Sec. 10.5 [hypot()] and sec. 10.6 [abs()] return "the same type as the
   input", and sec. 10.2 gives the comparison and stepped-value functions their
   arguments' type too: what the call answers, not the name it is spelled with,
   decides the slots it stands in. Both directions are pinned per function, a
   <number> argument at a <number> slot and a <length> one. *)
let spec_math_result_type () =
  let module P = Css.Properties in
  (* <number> in, <number> out: the call is the number it resolves to. *)
  decl_optimizes ~prop:"opacity" ~into:".2" "min(.2,.5)";
  decl_optimizes ~prop:"opacity" ~into:".5" "max(.2,.5)";
  decl_optimizes ~prop:"opacity" ~into:".5" "clamp(.1,.5,.9)";
  decl_optimizes ~prop:"opacity" ~into:"2" "round(1.5,1)";
  decl_optimizes ~prop:"opacity" ~into:"1" "mod(5,2)";
  decl_optimizes ~prop:"opacity" ~into:"1" "rem(5,2)";
  decl_optimizes ~prop:"opacity" ~into:"5" "hypot(3,4)";
  decl_optimizes ~prop:"opacity" ~into:".5" "abs(-.5)";
  (* <length> in, <length> out: the same call is no longer a <number>, so the
     <number> slot refuses it rather than shedding the unit. *)
  neg_cursor P.read_opacity "min(1px,2px)";
  neg_cursor P.read_opacity "max(1px,2px)";
  neg_cursor P.read_opacity "clamp(1px,2px,3px)";
  neg_cursor P.read_opacity "round(1.5px,1px)";
  neg_cursor P.read_opacity "mod(5px,2px)";
  neg_cursor P.read_opacity "rem(5px,2px)";
  neg_cursor P.read_opacity "hypot(3px,4px)";
  neg_cursor P.read_opacity "abs(-1px)";
  (* The same pair at the <integer> slots of sec. 10.9. *)
  decl_optimizes ~prop:"z-index" ~into:"1" "min(1,2)";
  decl_optimizes ~prop:"order" ~into:"5" "hypot(3,4)";
  decl_optimizes ~prop:"grid-row-end" ~into:"1" "abs(-1)";
  neg_cursor P.read_z_index "abs(-1px)";
  neg_cursor P.read_order "abs(-1px)";
  neg_cursor P.read_order "hypot(3px,4px)";
  neg_cursor P.read_grid_line "abs(-1px)";
  neg_cursor P.read_column_count "abs(-1px)";
  neg_cursor P.read_zoom "abs(-1px)";
  neg_cursor P.read_shape_image_threshold "abs(-1px)";
  neg_cursor P.read_flex_factor "abs(-1px)";
  neg_cursor P.read_font_weight "abs(-1px)";
  (* Sec. 10.6 gives [sign(A)] a <number> "whatever the input calculation's
     type", the mirror of [abs()]: it narrows to a number where [abs()]
     preserves, so a <length> argument reads at the <number> slot and the call
     itself never stands at a <length> one. *)
  decl_optimizes ~prop:"opacity" ~into:"-1" "sign(-1px)";
  neg_cursor read_length "sign(-1px)";
  (* An <opacity-value> is <number> | <percentage>, so a call answering a
     <percentage> is one of the two types the slot takes and resolves against
     the number the way the literal does. *)
  decl_optimizes ~prop:"opacity" ~into:".5" "abs(-50%)";
  decl_optimizes ~prop:"shape-image-threshold" ~into:".5" "abs(-50%)";
  (* CSS Backgrounds 3 sec. 6.2 spells <border-image-slice> over [<number> |
     <percentage>] with no length among them, so the slot answers the same way
     an <opacity-value> does, longhand and shorthand alike. *)
  decl_optimizes ~prop:"border-image-slice" ~into:"abs(-1)" "abs(-1)";
  decl_optimizes ~prop:"border-image-slice" ~into:"30%" "30%";
  decl_optimizes ~prop:"border-image-slice" ~into:"calc(abs(-30%))" "abs(-30%)";
  neg_cursor P.read_border_image_slice "abs(-1px)";
  neg_cursor P.read_border_image_slice "hypot(3px,4px)";
  neg_cursor P.read_border_image_slice "calc(abs(-1px))";
  neg_cursor P.read_border_image "abs(-1px)";
  (* Sec. 6.3 and 6.4 give <border-image-width> and <border-image-outset> a
     length, so the call the slice slot refuses stands at both. *)
  decl_optimizes ~prop:"border-image-width" ~into:"abs(-1px)" "abs(-1px)";
  decl_optimizes ~prop:"border-image-outset" ~into:"abs(-1px)" "abs(-1px)";
  (* A <length> slot takes what it refuses at a <number> one. *)
  decl_optimizes ~prop:"width" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"width" ~into:"5px" "hypot(3px,4px)";
  (* [line-height] is <number> | <length-percentage>, so the length the call
     answers is one of its types and the call stands, where the same call at an
     <opacity-value> does not. *)
  decl_optimizes ~prop:"line-height" ~into:"calc(abs(-1px))" "abs(-1px)";
  decl_optimizes ~prop:"line-height" ~into:"2" "abs(-2)";
  (* Sec. 10.1 puts [calc()] among the math functions rather than above them, so
     the wrapped spelling answers the same type and lands the same way. *)
  neg_cursor P.read_z_index "calc(abs(-1px))";
  neg_cursor P.read_opacity "calc(abs(-1px))";
  neg_cursor P.read_opacity "calc(hypot(3px,4px))";
  decl_optimizes ~prop:"z-index" ~into:"1" "calc(abs(-1))";
  decl_optimizes ~prop:"opacity" ~into:".5" "calc(abs(-.5))";
  decl_optimizes ~prop:"width" ~into:"1px" "calc(abs(-1px))"

(* CSS Values 4 (ED) sec. 10.9 Type Checking: at a [+] or [-] sub-expression the
   two argument types are added, and "if this returns failure, the entire
   calculation's type is failure". A call answering a [<number>] therefore fails
   the whole sum beside a dimension, and stands nowhere a [<number>] does not.
   Chrome 153 rejects every row here, cascade's own [calc(2 - 1px)] among
   them. *)
let spec_math_sum_type_check () =
  let module P = Css.Properties in
  (* A [<number>] call on either side of a [<length>] sum fails the sum. *)
  neg_cursor read_length "calc(sqrt(4) - 1px)";
  neg_cursor read_length "calc(1px - sqrt(4))";
  neg_cursor read_length "calc(sqrt(4) + 1px)";
  neg_cursor read_length "calc(sign(-1px) - 1px)";
  (* The failure is the sum's, not the call's: the same call multiplied by a
     dimension is the dimension sec. 10.9 gives a product. *)
  check_length ~minify:false "calc(sqrt(4) * 1px)";
  check_length ~minify:false ~expected:"calc(sqrt(4) * 1px)" "calc(sqrt(4)*1px)";
  (* A [<number>] call standing alone at a [<length>] slot fails the same way,
     wrapped in [calc()] or not. *)
  neg_cursor read_length "abs(-1)";
  neg_cursor read_length "calc(abs(-1))";
  neg_cursor read_length "calc(sign(-1px))";
  neg_cursor P.read_border_width "abs(-1)";
  neg_cursor P.read_border_width "calc(sign(-1px))";
  (* Sec. 10.4 gives the inverse trigonometric functions an [<angle>], so they
     are the one scalar-folding family that is not a [<number>]: the sum they
     sit in keeps them where the [<number>] families fail it. *)
  decl_optimizes ~prop:"rotate" ~into:"55deg" "calc(atan(1) + 10deg)";
  decl_optimizes ~prop:"rotate" ~into:"40deg" "calc(atan2(1,1) - 5deg)";
  decl_optimizes ~prop:"rotate" ~into:"45deg" "atan(1)";
  (* Sec. 10.4 [sin()], [cos()] and [tan()] answer a [<number>] whatever went
     in, so they fail the [<angle>] sum the inverse family holds. *)
  neg_cursor read_angle "calc(sin(0) + 10deg)";
  neg_cursor read_angle "calc(sqrt(4) + 10deg)";
  (* Sec. 10.9 checks a [<time>] sum the same way. *)
  neg_cursor read_duration "calc(sqrt(4) + 1s)";
  check_duration ~minify:false "calc(sqrt(4) * 1s)";
  (* A [<number>] sum stays a [<number>] and lands where one lands. *)
  decl_optimizes ~prop:"opacity" ~into:"1" "calc(sqrt(4) - 1)";
  decl_optimizes ~prop:"z-index" ~into:"2" "calc(2)";
  (* A dimension sum is untouched by the check. *)
  decl_optimizes ~prop:"width" ~into:"3px" "calc(1px + 2px)";
  decl_optimizes ~prop:"width" ~into:"calc(100% - 10px)" "calc(100% - 10px)"

(* CSS Values 4 (ED) sec. 10.9 spells the arguments of [round()], [mod()] and
   [rem()] as [<calc-sum>], so a dimension is one of them, and sec. 10.2 gives
   the call the type its arguments have. The stepped-value functions therefore
   stand wherever sec. 10.5 [hypot()] does, at a [<line-width>] as at a
   [<length>], and answer the [<number>] slot the same way it does. *)
let spec_stepped_value_dimensions () =
  let module P = Css.Properties in
  decl_optimizes ~prop:"border-top-width" ~into:"2px" "round(1.5px,1px)";
  decl_optimizes ~prop:"border-top-width" ~into:"1px" "mod(5px,2px)";
  decl_optimizes ~prop:"border-top-width" ~into:"1px" "rem(5px,2px)";
  decl_optimizes ~prop:"outline-width" ~into:"2px" "round(1.5px,1px)";
  decl_optimizes ~prop:"column-rule-width" ~into:"1px" "mod(5px,2px)";
  decl_optimizes ~prop:"-webkit-text-stroke-width" ~into:"1px" "rem(5px,2px)";
  (* Sec. 10.1 puts [calc()] among the math functions rather than above them, so
     the wrapped spelling lands the same way the bare one does. *)
  decl_optimizes ~prop:"border-top-width" ~into:"2px" "calc(round(1.5px,1px))";
  decl_optimizes ~prop:"border-top-width" ~into:"1px" "calc(mod(5px,2px))";
  decl_optimizes ~prop:"border-top-width" ~into:"1px" "calc(rem(5px,2px))";
  (* A [<number>] argument keeps answering a [<number>], which is what the
     [<line-width>] slot refuses and an [<opacity-value>] takes. *)
  decl_optimizes ~prop:"opacity" ~into:"2" "round(1.5,1)";
  decl_optimizes ~prop:"opacity" ~into:"1" "mod(5,2)";
  decl_optimizes ~prop:"opacity" ~into:"1" "rem(5,2)";
  neg_cursor P.read_opacity "round(1.5px,1px)";
  neg_cursor P.read_opacity "mod(5px,2px)";
  neg_cursor P.read_opacity "rem(5px,2px)";
  neg_cursor P.read_border_width "round(1.5,1)";
  (* Sec. 10.2 requires the arguments to have a consistent type, so a step in
     one type and a value in another is no calculation at all. *)
  neg_cursor P.read_border_width "round(1.5px,1)";
  neg_cursor P.read_border_width "mod(5px,2)"

(* Sec. 10.1 makes a math function usable wherever a [<number>], [<dimension>]
   or [<percentage>] is, so a reader that takes a literal there takes a call
   too, and sec. 10.12 checks the property's range on the value the call
   resolves to rather than refusing it: a call folding outside the range keeps
   its wrapper, which is the only spelling the value has left. Chrome 153 reads
   every row below and computes the call's own value.

   Sec. 10.7.1 keeps [pi] shorter than the coefficient it names, so a call that
   reduces to the constant keeps it. *)
let spec_math_at_the_remaining_readers () =
  let module P = Css.Properties in
  (* SVG 2 sec. 13.5.3 [stroke-width] is [<length-percentage> | <number>]: the
     call lands in the branch its own type names, and the [0,inf] range keeps
     the negative one wrapped. *)
  decl_optimizes ~prop:"stroke-width" ~into:"1" "calc(1)";
  decl_optimizes ~prop:"stroke-width" ~into:"calc(pi)" "calc(pi)";
  decl_optimizes ~prop:"stroke-width" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"stroke-width" ~into:"8" "pow(2,3)";
  decl_optimizes ~prop:"stroke-width" ~into:"1" "abs(-1px)";
  (* SVG 2 sec. 13.5.5 [stroke-miterlimit], [0,inf]. *)
  decl_optimizes ~prop:"stroke-miterlimit" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"stroke-miterlimit" ~into:"calc(-1)" "sign(-1px)";
  (* CSS Inline 3 sec. 5.1 [initial-letter], [1,inf] on the size. *)
  decl_optimizes ~prop:"initial-letter" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"initial-letter" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"initial-letter" ~into:"2 3" "calc(1 + 1) 3";
  (* CSS Fonts 5 sec. 2.5 [font-size-adjust], [0,inf]. *)
  decl_optimizes ~prop:"font-size-adjust" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"font-size-adjust" ~into:"calc(-1)" "sign(-1px)";
  (* SVG 2 sec. 11.3 reads a bare coefficient at [baseline-shift] as a length in
     user units, so the slot takes a call of either type. *)
  decl_optimizes ~prop:"baseline-shift" ~into:"calc(2)" "sqrt(4)";
  decl_optimizes ~prop:"baseline-shift" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"baseline-shift" ~into:"1px" "min(1px,2px)";
  (* CSS Transforms 2 sec. 5 [scale] takes any [<number-percentage>], so no
     range keeps a call wrapped here. *)
  decl_optimizes ~prop:"scale" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"scale" ~into:"-1" "sign(-1px)";
  (* The [<integer>] slots of CSS Multicol 2 sec. 4.2 and the [-webkit-line-
     clamp] alias, all [1,inf]. *)
  decl_optimizes ~prop:"-webkit-line-clamp" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"-webkit-line-clamp" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"column-count" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"column-count" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"column-count" ~into:"calc(pi)" "calc(pi)";
  decl_optimizes ~prop:"columns" ~into:"2" "sqrt(4)";
  decl_optimizes ~prop:"columns" ~into:"calc(-1)" "sign(-1px)";
  (* The [<percentage>] slots: CSS Fonts 4 sec. 2.3 and CSS Size Adjustment 1
     sec. 3. The [@font-face] descriptor reads through the same
     [font-stretch]. *)
  decl_optimizes ~prop:"font-stretch" ~into:"50%" "calc(50%)";
  decl_optimizes ~prop:"text-size-adjust" ~into:"50%" "calc(50%)";
  decl_optimizes ~prop:"-webkit-text-size-adjust" ~into:"50%" "calc(50%)";
  (* Sec. 10.7 gives [min()], [max()] and [clamp()] [<calc-sum>] arguments and
     the arguments' own type, exactly as sec. 10.9 does for the stepped
     functions, so a percentage slot takes them where it takes [calc()]. Chrome
     153 reads each of these and computes the comparison. *)
  decl_optimizes ~prop:"font-stretch" ~into:"50%" "min(50%,75%)";
  decl_optimizes ~prop:"font-stretch" ~into:"75%" "max(50%,75%)";
  decl_optimizes ~prop:"font-stretch" ~into:"60%" "clamp(50%,60%,75%)";
  decl_optimizes ~prop:"text-size-adjust" ~into:"50%" "clamp(10%,50%,90%)";
  decl_optimizes ~prop:"-webkit-text-size-adjust" ~into:"10%" "min(10%,50%)";
  (* The same call over lengths, which the typed length readers already took, so
     the two paths agree rather than one admitting what the other refuses. *)
  decl_optimizes ~prop:"width" ~into:"1px" "min(1px,2px)";
  decl_optimizes ~prop:"width" ~into:"2px" "max(1px,2px)";
  (* CSS UI 5 sec. 7.2 [interest-delay] is [<time [0s,inf]>], so the negative
     one Chrome computes as [0s] keeps its wrapper where the literal is
     dropped. *)
  decl_optimizes ~prop:"interest-delay" ~into:"calc(-1s)" "calc(-1s)";
  decl_optimizes ~prop:"interest-delay" ~into:"calc(-2s)" "calc(-1s * 2)";
  decl_optimizes ~prop:"interest-delay" ~into:"calc(-.5s)" "calc(-1s / 2)";
  decl_optimizes ~prop:"interest-delay" ~into:"calc(-2s)" "calc(2 * -1s)";
  decl_optimizes ~prop:"interest-delay" ~into:"1s" "calc(1s)";
  (* Sec. 10.8 gives a math operand no keyword, and the literal keeps the range
     the call is exempt from. *)
  neg_cursor P.read_stroke_width "calc(inherit)";
  neg_cursor P.read_stroke_width "-1";
  neg_cursor P.read_column_count "calc(inherit)";
  neg_cursor P.read_font_size_adjust "-1";
  neg_cursor P.read_initial_letter "calc(inherit)";
  neg_cursor P.read_interest_delay "-1s";
  (* Sec. 10.12 does not lift the range off a neighbouring property: the two
     durations Chrome refuses whatever the spelling stay refused. *)
  neg_cursor read_duration "calc(-1s)"

(* Sec. 10.9 types a calculation and sec. 10.1 admits it only where that type
   stands, so a call answering its arguments' type reaches a [<number>] or
   [<percentage>] slot only when those arguments were numbers or percentages:
   sec. 10.5 [hypot()], sec. 10.6 [abs()] and the sec. 10.2 stepped functions
   over lengths name a [<length>] and belong to none of the slots below. Sec.
   10.8 spells a [<calc-value>] as a number, a dimension, a percentage, a
   [<calc-keyword>] or a parenthesised sum, so no keyword of the property's own
   grammar is an operand either. Chrome 153 drops every row here. *)
let spec_math_type_at_the_number_percentage_readers () =
  let module P = Css.Properties in
  (* CSS Transforms 2 sec. 5 [scale] is [none | [ <number> | <percentage>
     ]{1,3}] and names no length. *)
  neg_cursor P.read_scale "abs(-1px)";
  neg_cursor P.read_scale "hypot(3px,4px)";
  neg_cursor P.read_scale "mod(5px,2px)";
  neg_cursor P.read_scale "round(1.5px,1px)";
  neg_cursor P.read_scale "calc(abs(-1px))";
  (* CSS Fonts 4 sec. 2.3.1 [font-stretch] is the [font-width] alias, [normal |
     <percentage [0,inf]> | <keyword>], and names no length. *)
  neg_cursor P.read_font_stretch "abs(-1px)";
  neg_cursor P.read_font_stretch "hypot(3px,4px)";
  neg_cursor P.read_font_stretch "mod(5px,2px)";
  neg_cursor P.read_font_stretch "round(1.5px,1px)";
  neg_cursor P.read_font_stretch "calc(abs(-1px))";
  (* CSS Size Adjustment 1 sec. 3 [text-size-adjust] is [none | auto |
     <percentage [0,inf]>], and names no length. The [-webkit-] twin reads
     through the same reader. *)
  neg_cursor P.read_text_size_adjust "abs(-1px)";
  neg_cursor P.read_text_size_adjust "hypot(3px,4px)";
  neg_cursor P.read_text_size_adjust "mod(5px,2px)";
  neg_cursor P.read_text_size_adjust "round(1.5px,1px)";
  neg_cursor P.read_text_size_adjust "calc(abs(-1px))";
  (* A keyword operand is no [<calc-value>]. Unwrapping one wrote back a live
     [inherit] where the browser had dropped the declaration outright. *)
  neg_cursor P.read_font_stretch "calc(inherit)";
  neg_cursor P.read_font_stretch "calc(normal)";
  neg_cursor P.read_font_stretch "calc(condensed)";
  neg_cursor P.read_text_size_adjust "calc(inherit)";
  neg_cursor P.read_text_size_adjust "calc(none)";
  neg_cursor P.read_text_size_adjust "calc(2 * auto)";
  neg_cursor P.read_text_size_adjust "calc(auto * 2)";
  (* The types each slot does take. A percentage-typed call stands at all three,
     and the [<number>] one only where the grammar spells a [<number>]. *)
  decl_optimizes ~prop:"scale" ~into:"2" "calc(2)";
  decl_optimizes ~prop:"scale" ~into:"1%" "abs(-1%)";
  decl_optimizes ~prop:"font-stretch" ~into:"50%" "calc(50%)";
  decl_optimizes ~prop:"font-stretch" ~into:"calc(abs(-1%))" "abs(-1%)";
  decl_optimizes ~prop:"text-size-adjust" ~into:"50%" "calc(50%)";
  decl_optimizes ~prop:"text-size-adjust" ~into:"calc(abs(-1%))" "abs(-1%)";
  neg_cursor P.read_font_stretch "calc(2)";
  neg_cursor P.read_text_size_adjust "calc(2)"

(* Sec. 9.1 is explicit that "width: -5px is not equivalent to width:
   calc(-5px)", because "out-of-range values specified literally are invalid at
   parse-time", and sec. 10.13 drops the wrapper only "of a computed value or
   later". A minifier serialises specified values, so a call folding outside the
   property's [0,inf] range keeps its wrapper: the literal underneath is a
   spelling the property's own grammar refuses. The arithmetic still folds, so
   what is kept is the shortest call, not the authored one. Chrome 153 reads
   every wrapped row below and drops every unwrapped one. *)
let spec_math_range_keeps_the_call () =
  (* CSS Sizing 4 sec. 5 gives [<ratio>] the [0,inf] numbers of sec. 6.5. *)
  decl_optimizes ~prop:"aspect-ratio" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"aspect-ratio" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"aspect-ratio" ~into:"calc(-2)" "round(-3,2)";
  decl_optimizes ~prop:"aspect-ratio" ~into:"1" "calc(1)";
  decl_optimizes ~prop:"aspect-ratio" ~into:"1" "1";
  (* CSS Backgrounds 3 sec. 4.1 gives each radius a [0,inf]
     [<length-percentage>]; the shorthand carries the range its longhands
     carry. *)
  decl_optimizes ~prop:"border-radius" ~into:"calc(-1px)" "calc(-1px)";
  (* Sec. 10.12 clamps a call past the range at computed-value time and keeps
     the declaration, so the arithmetic folds and the wrapper stays around the
     result: the length family answers here the way the number and time families
     already do at [tab-size] and [interest-delay]. Chrome 153 gives
     [calc(-2px)] for both rows below and [calc(-10px)] for [width: calc(-5px -
     5px)], and drops the bare literal [-10px]. *)
  decl_optimizes ~prop:"border-radius" ~into:"calc(-2px)" "calc(-1px * 2)";
  decl_optimizes ~prop:"padding" ~into:"calc(-2px)" "calc(-1px * 2)";
  decl_optimizes ~prop:"width" ~into:"calc(-10px)" "calc(-5px - 5px)";
  decl_optimizes ~prop:"border-radius" ~into:"1px" "calc(1px)";
  (* CSS Flexbox 1 sec. 7.2 gives both factors a [0,inf] [<number>], through the
     shorthand and the two longhands alike. *)
  decl_optimizes ~prop:"flex" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"flex" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"flex" ~into:"1" "calc(1)";
  decl_optimizes ~prop:"flex-grow" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"flex-grow" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"flex-grow" ~into:"calc(-2)" "round(-3,2)";
  decl_optimizes ~prop:"flex-grow" ~into:"2" "calc(2)";
  decl_optimizes ~prop:"flex-shrink" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"flex-shrink" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"flex-shrink" ~into:"2" "calc(2)";
  (* CSS Inline 3 sec. 2.2 gives [line-height] a [0,inf] number and length. *)
  decl_optimizes ~prop:"line-height" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"line-height" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"line-height" ~into:"1.5" "calc(1.5)";
  (* CSS Text 4 sec. 4.2 [tab-size], [0,inf]. *)
  decl_optimizes ~prop:"tab-size" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"tab-size" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"tab-size" ~into:"calc(-2)" "round(-3,2)";
  decl_optimizes ~prop:"tab-size" ~into:"4" "calc(4)";
  (* CSS Animations 1 sec. 3.4 [animation-iteration-count], [0,inf], and the
     shorthand slot that reads through it. *)
  decl_optimizes ~prop:"animation-iteration-count" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"animation-iteration-count" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"animation-iteration-count" ~into:"calc(-1),2"
    "sign(-1px),2";
  decl_optimizes ~prop:"animation-iteration-count" ~into:"2" "calc(2)";
  (* The shorthand normalises the slots it holds, so the count folds where the
     longhand folds instead of waiting for a second pass over the output. *)
  decl_optimizes ~prop:"animation" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"animation" ~into:"2s linear 3" "2s linear calc(1 + 2)";
  (* SVG 2 sec. 13.5.4 makes a negative dash length an error, so each item
     carries the range whichever branch of [<length-percentage> | <number>] it
     took. *)
  decl_optimizes ~prop:"stroke-dasharray" ~into:"calc(-1px)" "calc(-1px)";
  decl_optimizes ~prop:"stroke-dasharray" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"stroke-dasharray" ~into:"1px" "calc(1px)";
  (* SVG 2 sec. 13.5.3 calls a negative [stroke-width] invalid, so the call
     stays around the folded result the way it does at every other length slot.
     Chrome 153 gives [calc(-1px)] for both rows. *)
  decl_optimizes ~prop:"stroke-width" ~into:"calc(-1px)" "calc(-1px)";
  decl_optimizes ~prop:"stroke-width" ~into:"calc(-1px)" "calc(2px - 3px)";
  (* CSS Text 4 sec. 6.2 gives the three [hyphenate-limit-chars] counts a
     [1,inf] [<integer>]. *)
  decl_optimizes ~prop:"hyphenate-limit-chars" ~into:"calc(-1)" "sign(-1px)";
  decl_optimizes ~prop:"hyphenate-limit-chars" ~into:"calc(-1)" "calc(-1)";
  decl_optimizes ~prop:"hyphenate-limit-chars" ~into:"4" "calc(4)";
  (* The folds that stay folds: a call landing inside the range has no reason to
     keep a wrapper, and a property with no lower bound never grows one. *)
  decl_optimizes ~prop:"width" ~into:"3px" "calc(1px + 2px)";
  decl_optimizes ~prop:"opacity" ~into:".5" "calc(.5)";
  decl_optimizes ~prop:"width" ~into:"3.14159px" "calc(pi * 1px)";
  decl_optimizes ~prop:"z-index" ~into:"2" "calc(2)";
  decl_optimizes ~prop:"width" ~into:"calc(-10px)" "calc(-10px)";
  decl_optimizes ~prop:"border-width" ~into:"calc(-1px)" "calc(-1px)";
  decl_optimizes ~prop:"width" ~into:"1px" "abs(-1px)";
  decl_optimizes ~prop:"margin" ~into:"-10px" "calc(-10px)";
  decl_optimizes ~prop:"scale" ~into:"-1" "sign(-1px)"

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

(* ignore-test: the calc operator grammar spans every math function. *)
let test_calc_operator_whitespace () =
  (* CSS Values 4 (ED) sec. 10.8 Syntax: "whitespace is required on both sides
     of the + and - operators. (The * and / operators can be used without white
     space around them.)" A one-sided sum is not a math function at all, so the
     value is dropped rather than re-emitted in the valid spelling. *)
  neg_cursor read_length "calc(100%- 10px)";
  neg_cursor read_length "calc(100%+ 10px)";
  neg_cursor read_length "calc(100% -10px)";
  neg_cursor read_length "calc(1px- 2px)";
  neg_cursor read_length "calc(1px -(2px))";
  neg_cursor read_length "-webkit-calc(100%- 10px)";
  (* A parenthesised operand on either side of the operator. *)
  neg_cursor read_length "calc((1px)- 2px)";
  neg_cursor read_length "calc((1px)+ 2px)";
  neg_cursor read_length "calc((1px) -(2px))";
  (* The rule holds inside a parenthesised sub-expression as well as around
     it. *)
  neg_cursor read_length "calc((1px + 2px)- 3px)";
  neg_cursor read_length "calc((1px+ 2px) - 3px)";
  (* var() and a nested math function are operands like any other. *)
  neg_cursor read_length "calc(var(--a)- 2px)";
  neg_cursor read_length "calc(var(--a)+ 2px)";
  neg_cursor read_length "calc(min(1px,2px)- 3px)";
  neg_cursor read_length "calc(min(1px,2px)+ 3px)";
  neg_cursor read_length "calc(sqrt(4)- 1px)";
  (* A product binds tighter, so the sum operator after it still needs its own
     whitespace. *)
  neg_cursor read_length "calc(1px*2- 1px)";
  (* The <calc-sum> in a math function argument obeys the same rule. *)
  neg_cursor read_length "calc(sqrt(4- 1) * 1px)";
  neg_cursor read_math_arg "1- 2";
  neg_cursor read_math_arg "4+ 1";
  (* A sign absorbed into a number token is a different failure: there is no
     delimiter to space out, and [calc(1 +2)] is rejected for leaving [+2]
     unconsumed. *)
  neg_cursor read_length "calc(1 +2)";

  (* The accepted spellings, checked against the pretty form: minify drops the
     optional whitespace around [*] and [/] and the redundant parentheses,
     which is a separate concern from which spellings parse. *)
  (* [*] and [/] take whitespace on neither, one or both sides. *)
  check_length ~minify:false ~expected:"calc(2px * 1.5)" "calc(2px*1.5)";
  check_length ~minify:false ~expected:"calc(2px * 1.5)" "calc(2px *1.5)";
  check_length ~minify:false ~expected:"calc(2px * 1.5)" "calc(2px* 1.5)";
  check_length ~minify:false "calc(2px * 1.5)";
  check_length ~minify:false ~expected:"calc(2px / 1.5)" "calc(2px/1.5)";
  check_length ~minify:false ~expected:"calc(2px / 1.5)" "calc(2px /1.5)";
  check_length ~minify:false ~expected:"calc(2px / 1.5)" "calc(2px/ 1.5)";
  check_length ~minify:false "calc(2px / 1.5)";
  (* A sign glued to the first operand of a term is unary, not a sum. *)
  check_length ~minify:false "calc(-1px + 2px)";
  check_length ~minify:false "calc((-1px))";
  (* The spaced spelling of each rejection above stays valid. *)
  check_length ~minify:false "calc(100% - 10px)";
  check_length ~minify:false "calc(1px - (2px))";
  check_length ~minify:false "calc((1px) - (2px))";
  check_length ~minify:false "calc((1px + 2px) - 3px)";
  check_length ~minify:false "calc(min(1px, 2px) - 3px)";
  (* [calc(sqrt(4) - 1px)] is the one rejection above whose spaced spelling does
     not come back: sec. 10.9 Type Checking adds the types at a [-], and a
     [<number>] beside a [<length>] returns failure, which is the whole
     calculation's type. The whitespace rule keeps its math-function operand in
     the [min(1px, 2px)] row above. *)
  neg_cursor read_length "calc(sqrt(4) - 1px)";
  check_length ~minify:false "calc(sqrt(4 - 1) * 1px)";
  check_length ~minify:false "calc(1px * 2 - 1px)";
  check_length ~minify:false ~expected:"calc(100% - 10px)"
    "-webkit-calc(100% - 10px)";
  check_math_arg "1 - 2";
  check_math_arg "4 + 1"

let suite =
  ( "values_math",
    [
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
      test_case "calc parenthesis layering" `Quick calc_parenthesis_layering;
      test_case "var in calc other types" `Quick test_var_in_calc_types;
      test_case "number var printing" `Quick test_number_var_printing;
      test_case "var() with empty fallback" `Quick test_var_empty_fallback;
      test_case "calc_op" `Quick test_calc_op;
      test_case "math_const" `Quick test_math_const;
      test_case "math_arg" `Quick test_math_arg;
      test_case "calc operator whitespace" `Quick test_calc_operator_whitespace;
      test_case "spec values level 4/5 math and color edges" `Quick
        spec_values_l45_math_color;
      test_case "spec math function edges" `Quick spec_math_function_edges;
      test_case "spec math operand range" `Quick spec_math_operand_range;
      test_case "spec bare math functions" `Quick spec_bare_math_functions;
      test_case "spec math result type" `Quick spec_math_result_type;
      test_case "spec math sum type check" `Quick spec_math_sum_type_check;
      test_case "spec stepped value dimensions" `Quick
        spec_stepped_value_dimensions;
      test_case "spec math at the remaining readers" `Quick
        spec_math_at_the_remaining_readers;
      test_case "spec math type at the number percentage readers" `Quick
        spec_math_type_at_the_number_percentage_readers;
      test_case "spec math range keeps the call" `Quick
        spec_math_range_keeps_the_call;
    ] )
