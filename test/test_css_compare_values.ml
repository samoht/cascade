open Cascade
open Css_compare_helpers

(* A custom property's stream substitutes the number a token holds, not the
   digits it was written with, so [-0.5] and [-.5] are one value wherever the
   stream lands. *)
let equal_canonical_custom_signed_leading_zero () =
  Alcotest.(check bool)
    "a signed number's leading zero is not a difference" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a{--t:calc(var(--s)*-.5);translate:var(--t)}"
       ".a{--t:calc(var(--s)*-0.5);translate:var(--t)}")

(* SVG 2 sec. 13.5.3: [stroke-width] takes a [<number>] in user units, and a CSS
   pixel is one user unit, so [1] and [1px] draw the same stroke. Tailwind
   writes [stroke-1] unitless and tw writes [1px]. *)
let equal_canonical_stroke_width_number () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a unitless stroke width is its pixel length" true
    (equal ".a{stroke-width:1px}" ".a{stroke-width:1}");
  Alcotest.(check bool)
    "a fractional width too" true
    (equal ".a{stroke-width:1.5px}" ".a{stroke-width:1.5}");
  Alcotest.(check bool)
    "a different width still differs" false
    (equal ".a{stroke-width:1px}" ".a{stroke-width:2}")

(* CSS Values 4 sec. 10.9: an infinite [calc()] result clamps to the largest
   value the property can hold, and so does a finite length past that bound, so
   [calc(infinity * 1px)] and [3.40282e38px] are the same radius. Tailwind
   writes [rounded-full] with the first and lightningcss with the second. *)
let equal_canonical_infinite_length () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "an infinite length is the largest one written out" true
    (equal ".a{border-radius:3.40282e38px}"
       ".a{border-radius:calc(infinity * 1px)}");
  Alcotest.(check bool)
    "a negative infinity is not a positive one" false
    (equal ".a{margin-top:calc(infinity * 1px)}"
       ".a{margin-top:calc(-infinity * 1px)}")

(* A static product over a percentage in [flex-basis] is the percentage it
   computes, as it already is in [width]. *)
let equal_canonical_flex_basis_percentage_calc () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a static percentage product is its percentage" true
    (equal ".a{flex-basis:50%}" ".a{flex-basis:calc(.5 * 100%)}");
  Alcotest.(check bool)
    "a different percentage still differs" false
    (equal ".a{flex-basis:50%}" ".a{flex-basis:calc(.25 * 100%)}")

(* CSS Values 4 sec. 7.2: [s] and [ms] name one quantity, so a [<time>] token in
   an unregistered custom property's stream is the same time under either unit,
   as it already is once the property is registered [<time>]. Tailwind writes
   [--tw-duration:.1s] where lightningcss writes [100ms]. A container [style()]
   query on an unregistered property compares the tokens as written, so a
   property such a query names keeps its spelling. *)
let equal_canonical_custom_time_units () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a time in a custom stream is one value under either unit" true
    (equal ".a{--d:.1s}" ".a{--d:100ms}");
  Alcotest.(check bool)
    "a time inside a function too" true
    (equal ".a{--t:all .15s ease}" ".a{--t:all 150ms ease}");
  Alcotest.(check bool)
    "a different time still differs" false
    (equal ".a{--d:.1s}" ".a{--d:10ms}");
  Alcotest.(check bool)
    "a property a style() query names keeps its tokens" false
    (equal ".a{--d:.1s}@container style(--d:100ms){.b{color:red}}"
       ".a{--d:100ms}@container style(--d:100ms){.b{color:red}}")

(* CSS Values 4 sec. 6.1: [deg], [grad], [rad] and [turn] are units of one
   dimension, so an angle is one value under any of them. A [turn] or [grad]
   converts to degrees exactly; a [rad] goes through pi, and the projection
   spends on it the six-significant-figure budget a quotient already takes, so
   [1.5rad] is the [85.9437deg] lightningcss writes. Tailwind writes [.5turn]
   where lightningcss writes [180deg], in a custom stream and in [rotate] alike;
   [--lossless] keeps the unit as written. *)
let equal_canonical_angle_units () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a turn in a custom stream is its degrees" true
    (equal ".a{--tw-mask-conic-position:.5turn}"
       ".a{--tw-mask-conic-position:180deg}");
  Alcotest.(check bool)
    "a radian in rotate is its degrees under the budget" true
    (equal ".a{rotate:85.9437deg}" ".a{rotate:1.5rad}");
  Alcotest.(check bool)
    "a radian in a transform function too" true
    (equal ".a{transform:rotate(1.5rad)}" ".a{transform:rotate(85.9437deg)}");
  Alcotest.(check bool)
    "a turn in rotate too" true
    (equal ".a{rotate:.5turn}" ".a{rotate:180deg}");
  Alcotest.(check bool)
    "a radian in a custom stream too" true
    (equal ".a{--r:1.5rad}" ".a{--r:85.9437deg}");
  Alcotest.(check bool)
    "a grad inside a function too" true
    (equal ".a{--t:rotate(200grad)}" ".a{--t:rotate(180deg)}");
  Alcotest.(check bool)
    "a different angle still differs" false
    (equal ".a{rotate:1.5rad}" ".a{rotate:85.9deg}");
  Alcotest.(check bool)
    "a property a style() query names keeps its tokens" false
    (equal ".a{--d:.5turn}@container style(--d:180deg){.b{color:red}}"
       ".a{--d:180deg}@container style(--d:180deg){.b{color:red}}");
  let lossless =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless:true
  in
  Alcotest.(check bool)
    "lossless keeps a radian apart from its degrees" false
    (lossless ".a{rotate:85.9437deg}" ".a{rotate:1.5rad}");
  Alcotest.(check bool)
    "and a custom stream's unit as written" false
    (lossless ".a{--r:1.5rad}" ".a{--r:85.9437deg}")

(* CSS Color 5 sec. 4.1: a relative colour whose channels are the origin's own
   keywords is the origin in that space, with the alpha the call names. Tailwind
   writes a shadow colour as [oklab(from rgb(0 0 0 / .1) l a b / 20%)] and
   lightningcss folds it to the black it is; the projection has to read the two
   as one colour, in a colour longhand and in a [var()] fallback of a shadow
   stream alike. A channel the call rewrites is a different colour. *)
let equal_canonical_relative_color_pass_through () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "pass-through channels over a hex origin are the origin" true
    (equal ".a{color:#0003}" ".a{color:oklab(from #0000001a l a b/.2)}");
  Alcotest.(check bool)
    "over an rgb() origin with a percentage alpha too" true
    (equal ".a{color:#0003}" ".a{color:oklab(from rgb(0 0 0/.1) l a b/20%)}");
  Alcotest.(check bool)
    "the origin's alpha carries through an omitted slot" true
    (equal ".a{color:#0000001a}" ".a{color:oklab(from #0000001a l a b)}");
  Alcotest.(check bool)
    "in a shadow's var() fallback" true
    (equal ".a{box-shadow:0 1px 2px var(--c,#0003)}"
       ".a{box-shadow:0 1px 2px var(--c,oklab(from rgb(0 0 0/.1) l a b/20%))}");
  Alcotest.(check bool)
    "a rewritten channel is another colour" false
    (equal ".a{color:#0003}"
       ".a{color:oklab(from #0000001a calc(l + .5) a b/.2)}");
  Alcotest.(check bool)
    "a different alpha still differs" false
    (equal ".a{color:#0003}" ".a{color:oklab(from #0000001a l a b/.5)}")

(* CSS Color 5 sec. 6: [light-dark()] computes to one of its two colours by the
   element's used colour scheme, so a [color-mix()] or a relative colour over a
   [light-dark()] argument is the [light-dark()] of that operation over each
   branch, and each branch folds as the plain colour it is. Tailwind writes a
   scheme-aware colour at half alpha as the mix and lightningcss as the
   [light-dark()] of the two hexes. A mix a branch cannot fold stays as written,
   so the two spellings of it still report. *)
let equal_canonical_light_dark_mix () =
  let equal = Cascade_diff.Css_compare.equal ~mode:`Canonical in
  Alcotest.(check bool)
    "a mix over a light-dark() is the light-dark() of the mixes" true
    (equal ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:color-mix(in oklab,light-dark(red,#00f) 50%,#0000)}");
  Alcotest.(check bool)
    "in the second argument too" true
    (equal ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:color-mix(in oklab,#0000 50%,light-dark(red,#00f))}");
  Alcotest.(check bool)
    "and in both" true
    (equal ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:color-mix(in oklab,light-dark(red,#00f) \
        50%,light-dark(#0000,#fff0))}");
  Alcotest.(check bool)
    "a relative colour over a light-dark() origin" true
    (equal ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:oklab(from light-dark(red,#00f) l a b/.5)}");
  Alcotest.(check bool)
    "in a custom stream too" true
    (equal ".a{--c:light-dark(#ff000080,#0000ff80)}"
       ".a{--c:color-mix(in oklab,light-dark(red,#00f) 50%,#0000)}");
  Alcotest.(check bool)
    "a different proportion is another pair of colours" false
    (equal ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:color-mix(in oklab,light-dark(red,#00f) 25%,#0000)}");
  Alcotest.(check bool)
    "a mix no branch can fold stays as written" false
    (equal
       ".a{background-color:light-dark(color-mix(in oklab,red \
        50%,var(--x)),color-mix(in oklab,#00f 50%,var(--x)))}"
       ".a{background-color:color-mix(in oklab,light-dark(red,#00f) \
        50%,var(--x))}");
  let lossless =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless:true
  in
  Alcotest.(check bool)
    "lossless keeps the mix" false
    (lossless ".a{background-color:light-dark(#ff000080,#0000ff80)}"
       ".a{background-color:color-mix(in oklab,light-dark(red,#00f) 50%,#0000)}")

(* CSS Color 4 sec. 10.2: [color(srgb r g b)] scales each channel by 255, so
   [color(srgb 1 0 0)] and [rgb(255 0 0)] are one colour in two spellings.
   [--lossless] keeps a modern colour function on output, which leaves the
   projection reading the spelling as a difference. The fold is exact-only: a
   channel that does not land on a whole byte, and a colour in another gamut,
   stay distinct. *)
let equal_canonical_lossless_exact_srgb () =
  let equal a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless:true a b
  in
  Alcotest.(check bool)
    "color(srgb 1 0 0) is rgb(255 0 0)" true
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:rgb(255,0,0)}");
  Alcotest.(check bool)
    "color(srgb 1 0 0) is #f00" true
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:#f00}");
  Alcotest.(check bool)
    "color(srgb .2 .4 .6) is rgb(51 102 153)" true
    (equal ".a{color:color(srgb .2 .4 .6)}" ".a{color:rgb(51,102,153)}");
  Alcotest.(check bool)
    "the alpha scales exactly too" true
    (equal ".a{color:color(srgb 1 0 0/.5)}" ".a{color:rgba(255,0,0,.5)}");
  (* The spelling folds wherever the declaration sits: an at-rule that groups
     rules holds the same declarations a caller could have written at the top
     level. *)
  Alcotest.(check bool)
    "the fold reaches inside @starting-style" true
    (equal "@starting-style{.a{color:color(srgb 1 0 0)}}"
       "@starting-style{.a{color:#f00}}");
  Alcotest.(check bool)
    "the fold reaches inside @scope" true
    (equal "@scope (.p){.a{color:color(srgb 1 0 0)}}"
       "@scope (.p){.a{color:#f00}}");
  (* display-p3 red is a different colour, not a different spelling. *)
  Alcotest.(check bool)
    "color(display-p3 1 0 0) still differs from rgb(255 0 0)" false
    (equal ".a{color:color(display-p3 1 0 0)}" ".a{color:rgb(255,0,0)}");
  (* One byte off is a difference, and .501 does not scale to a whole byte, so
     neither folds onto the other. *)
  Alcotest.(check bool)
    "color(srgb 1 0 0) still differs from rgb(254 0 0)" false
    (equal ".a{color:color(srgb 1 0 0)}" ".a{color:rgb(254,0,0)}");
  Alcotest.(check bool)
    "an off-grid channel keeps its function" false
    (equal ".a{color:color(srgb .501 0 0)}" ".a{color:maroon}")

(* Canonical comparison applies the optimizer's precision mode symmetrically to
   both inputs. The default six-significant-figure budget therefore equates the
   minifier's static line-height fold, while [--lossless] keeps the authored
   quotient distinct from every finite approximation. *)
let canonical_numeric_division_follows_precision_mode () =
  let equal ?(lossless = false) a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless a b
  in
  Alcotest.(check bool)
    "an exact quotient folds" true
    (equal ".a{line-height:calc(28/14)}" ".a{line-height:2}");
  Alcotest.(check bool)
    "the default precision budget matches minified output" true
    (equal ".a{line-height:calc(28/18)}" ".a{line-height:1.55556}");
  Alcotest.(check bool)
    "even the nearest emitted decimal is outside zero tolerance" false
    (equal ".a{line-height:calc(28/18)}" ".a{line-height:1.55555556}");
  Alcotest.(check bool)
    "lossless keeps an exact numeric boundary" false
    (equal ~lossless:true ".a{line-height:calc(28/18)}"
       ".a{line-height:1.55556}");
  (* The budget is one budget: a non-terminating quotient folds the same way in
     a length or a percentage as in a number. Tailwind writes [w-1/3] as [calc(1
     / 3 * 100%)] and lightningcss as [33.3333%], and the two render within a
     layout unit of each other, which no display shows. *)
  Alcotest.(check bool)
    "a fraction of a percentage folds under the budget" true
    (equal ".a{width:33.3333%}" ".a{width:calc(1/3 * 100%)}");
  Alcotest.(check bool)
    "in flex-basis and inset too" true
    (equal ".a{flex-basis:66.6667%;top:16.6667%}"
       ".a{flex-basis:calc(2/3 * 100%);top:calc(1/6 * 100%)}");
  Alcotest.(check bool)
    "a length quotient too" true
    (equal ".a{width:3.33333px}" ".a{width:calc(10px/3)}");
  Alcotest.(check bool)
    "a different fraction still differs" false
    (equal ".a{width:33.3333%}" ".a{width:calc(1/4 * 100%)}");
  Alcotest.(check bool)
    "lossless keeps the quotient" false
    (equal ~lossless:true ".a{width:33.3333%}" ".a{width:calc(1/3 * 100%)}")

(* Filter Effects 1 sec. 6.1 makes an omitted [hue-rotate()] argument 0, and
   [hue-rotate] names a filter function and nothing else, so the two spellings
   are one value wherever the stream is substituted. *)
let canonical_custom_hue_rotate_zero () =
  Alcotest.(check bool)
    "hue-rotate() and hue-rotate(0deg) agree in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a{--f:hue-rotate();filter:var(--f)}"
       ".a{--f:hue-rotate(0deg);filter:var(--f)}");
  (* Any zero angle, since the unit does not survive normalisation. *)
  Alcotest.(check bool)
    "a zero turn agrees too" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a{--f:hue-rotate()}"
       ".a{--f:hue-rotate(0turn)}");
  (* A non-zero angle is a different filter. *)
  Alcotest.(check bool)
    "a non-zero angle still differs" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a{--f:hue-rotate()}"
       ".a{--f:hue-rotate(90deg)}")

(* ===== semantic equivalence tests ===== *)

let semantically_equivalent_color_mix_values () =
  Alcotest.(check bool)
    "transparent and #0000 are equivalent inside color-mix" true
    (Cascade_diff.Css_compare.equivalent_value ~property:"color"
       color_mix_transparent color_mix_transparent_hex)

let semantically_equivalent_color_mix_css () =
  let expected = ".a { color: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { color: " ^ color_mix_transparent_hex ^ " }" in
  Alcotest.(check bool)
    "CSS with equivalent color-mix values is semantically equivalent" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_property_tokens () =
  let expected = ".a { --tw-ring-color: transparent }" in
  let actual = ".a { --tw-ring-color: #0000 }" in
  Alcotest.(check bool)
    "unregistered custom property token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let registered_custom_property_tokens () =
  let expected =
    "@property --tw-ring-color { syntax: \"<color>\"; inherits: true; \
     initial-value: transparent } .a { --tw-ring-color: transparent }"
  in
  let actual =
    "@property --tw-ring-color { syntax: \"<color>\"; inherits: true; \
     initial-value: transparent } .a { --tw-ring-color: #0000 }"
  in
  Alcotest.(check bool)
    "registered color custom properties compare after typed canonicalization"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_color_mix_tokens () =
  (* color-mix() is unconditionally a colour wherever it is substituted, so two
     spellings of the same colour-mix are equal under canonical comparison even
     inside an unregistered custom property. transparent and #0000 are the same
     colour, so these two forms compare equal. *)
  let expected = ".a { --tw-gradient: " ^ color_mix_transparent ^ " }" in
  let actual = ".a { --tw-gradient: " ^ color_mix_transparent_hex ^ " }" in
  Alcotest.(check bool)
    "custom color-mix spellings of one colour compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_color_function_alpha () =
  (* oklab() is unconditionally a colour, so an alpha written as 25% or .25 is
     the same value; canonical comparison equates them inside a custom-property
     body. *)
  Alcotest.(check bool)
    "oklab alpha 25% and .25 compare equal in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --s: oklab(50% 0 0 / 25%) }" ".a { --s: oklab(50% 0 0 / .25) }");
  (* A bare colour keyword could be a custom-ident in a non-colour substitution
     context, so it stays opaque: transparent and #0000 are not equated. *)
  Alcotest.(check bool)
    "bare transparent and #0000 stay distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a { --c: transparent }"
       ".a { --c: #0000 }")

let canonical_custom_shadow_color () =
  (* A complete shadow grammar fixes its final keyword's type to <color>, even
     when the shadow is carried through an unregistered custom property. *)
  Alcotest.(check bool)
    "named and hex shadow colours compare equal in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ":root { --drop-shadow-glow: 0 0 8px red }"
       ":root { --drop-shadow-glow: 0 0 8px #f00 }");
  (* An arbitrary identifier after the lengths is not a valid shadow colour and
     must remain opaque. *)
  Alcotest.(check bool)
    "a non-colour custom identifier stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical ".a { --x: 0 0 8px red }"
       ".a { --x: 0 0 8px custom-ident }")

let canonical_relative_color_origin () =
  (* A relative colour function fixes its origin's type to <color>, so
     equivalent spellings of that origin must compare equal, including when the
     whole function sits inside an otherwise opaque custom-property value. *)
  let parsed =
    Cascade.Css.Values.read_color
      (Cascade.Cursor.of_string "oklab(from red l a b / var(--x))")
  in
  Alcotest.(check bool)
    "relative-colour parser retains a typed origin" true
    (match parsed with
    | Cascade.Css.Relative_color
        ("oklab", Cascade.Css.Named Cascade.Css.Red, "l a b/var(--x)") ->
        true
    | _ -> false);
  Alcotest.(check bool)
    "named and hex relative-colour origins compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { color: oklab(from red l a b / var(--x)) }"
       ".a { color: oklab(from #f00 l a b / var(--x)) }");
  Alcotest.(check bool)
    "named and hex relative-rgb origins compare equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { color: rgb(from red r g b / var(--x)) }"
       ".a { color: rgb(from #f00 r g b / var(--x)) }");
  Alcotest.(check bool)
    "relative-colour origins compare equal inside a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --s: 0 0 8px oklab(from red l a b / var(--x)) }"
       ".a { --s: 0 0 8px oklab(from #f00 l a b / var(--x)) }")

let canonical_fully_transparent_missing_oklab () =
  let equivalent property =
    Cascade_diff.Css_compare.equal ~mode:`Canonical
      (Fmt.str ".a { %s: oklab(0%% none none / 0) }" property)
      (Fmt.str ".a { %s: #0000 }" property)
  in
  Alcotest.(check bool)
    "fully transparent missing-component oklab equals #0000 in a colour \
     property"
    true
    (equivalent "background-color");
  Alcotest.(check bool)
    "fully transparent missing-component oklab equals #0000 in a custom \
     property"
    true
    (equivalent "--tw-gradient-via");
  Alcotest.(check bool)
    "non-transparent missing-component oklab stays distinct from #0000" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { background-color: oklab(0% none none / .5) }"
       ".a { background-color: #0000 }")

(* CSS Color 4 sec. 4.4: outside the situations that combine two colours, "a
   missing component behaves as a zero value, in the appropriate unit for that
   component", and that explicitly covers rendering the colour and converting it
   to another space. So an achromatic OKLab written with [none] chroma axes -
   what lightningcss emits for a colour it produced by conversion - is the same
   colour as the hex Cascade folds it to, and the projection has to say so.

   Sec. 13.3 is the carve-out: where two colours are interpolated, a missing
   component takes the other colour's analogous component instead of a zero, so
   the two spellings are two different ramps. The fold therefore fires only on a
   colour standing as a whole colour-longhand value, never on one the value
   itself feeds to an interpolation. *)
let canonical_missing_color_components_as_zero () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "missing OKLab axes read as zero against the hex" true
    (equal ".x{outline-color:oklab(0% none none / .5)}"
       ".x{outline-color:#00000080}");
  Alcotest.(check bool)
    "the zero-spelled axes still read as that hex" true
    (equal ".x{outline-color:oklab(0% 0 0 / .5)}" ".x{outline-color:#00000080}");
  Alcotest.(check bool)
    "the two spellings agree with each other" true
    (equal ".x{outline-color:oklab(0% none none / .5)}"
       ".x{outline-color:oklab(0% 0 0 / .5)}");
  (* Nothing about the fold is specific to black: a missing axis is a zero at
     any lightness. *)
  Alcotest.(check bool)
    "a missing axis reads as zero away from black" true
    (equal ".x{outline-color:oklab(50% none none)}"
       ".x{outline-color:oklab(50% 0 0)}");
  (* Sec. 4.4.1: the hue of a zero-chroma OKLCh is powerless, so the two
     spellings are one colour there as well. *)
  Alcotest.(check bool)
    "a missing powerless hue reads as zero" true
    (equal ".x{outline-color:oklch(50% 0 none)}"
       ".x{outline-color:oklch(50% 0 0)}");
  (* A different colour stays a different colour: the resolved axes do not move
     the lightness or the alpha. *)
  Alcotest.(check bool)
    "the resolved colour keeps its alpha" false
    (equal ".x{outline-color:oklab(0% none none / .5)}"
       ".x{outline-color:#00000040}");
  (* Resolving the sentinel is a respelling inside the colour, so it applies
     under [--lossless] too, and stops where that mode stops: the OKLab does not
     cross into sRGB. *)
  let lossless a b =
    Cascade_diff.Css_compare.equal ~mode:`Canonical ~lossless:true a b
  in
  Alcotest.(check bool)
    "lossless resolves the axes" true
    (lossless ".x{outline-color:oklab(0% none none / .5)}"
       ".x{outline-color:oklab(0% 0 0 / .5)}");
  Alcotest.(check bool)
    "lossless keeps the OKLab out of sRGB" false
    (lossless ".x{outline-color:oklab(0% none none / .5)}"
       ".x{outline-color:#00000080}")

(* The sec. 13.3 side of the line. At each of these positions the stylesheet
   itself pairs the colour with another one, so [none] and [0] name two
   different results and the projection must keep them apart. *)
let canonical_missing_components_survive_interpolation () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "a gradient stop keeps its missing axes" false
    (equal
       ".x{background-image:linear-gradient(in oklab,oklab(0% none \
        none),oklab(100% .2 .1))}"
       ".x{background-image:linear-gradient(in oklab,#000,oklab(100% .2 .1))}");
  Alcotest.(check bool)
    "a color-mix operand keeps its missing axes" false
    (equal ".x{color:color-mix(in oklab,oklab(0% none none) 50%,var(--y))}"
       ".x{color:color-mix(in oklab,#000 50%,var(--y))}");
  Alcotest.(check bool)
    "a keyframe keeps its missing axes" false
    (equal "@keyframes k{from{color:oklab(0% none none)}to{color:#fff}}"
       "@keyframes k{from{color:#000}to{color:#fff}}");
  Alcotest.(check bool)
    "a @starting-style value keeps its missing axes" false
    (equal "@starting-style{.x{color:oklab(0% none none)}}"
       "@starting-style{.x{color:#000}}");
  (* A mix Cascade can fold reports the same fact through the folded colour: the
     missing axes take the other operand's, so the two mixes are two colours. *)
  Alcotest.(check bool)
    "a folded mix carries the missing axes forward" false
    (equal
       ".x{color:color-mix(in oklab,oklab(0% none none) 50%,oklab(100% .2 .1))}"
       ".x{color:color-mix(in oklab,#000 50%,oklab(100% .2 .1))}")

(* A rule that transitions the property it also sets makes that declaration one
   endpoint of an interpolation, which is the sec. 13.3 carve-out again: the
   [none] axes take the other endpoint's, the zeros stay zero, and the browser
   walks two different ramps. The property list is what decides, not the
   duration: a fragment is compared without the rest of the page's cascade, so
   another sheet can supply the duration this one leaves at its initial. *)
let canonical_missing_components_under_transition () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "the shorthand naming the property keeps the axes apart" false
    (equal ".x{color:oklab(0% none none/.5);transition:color 1s}"
       ".x{color:#00000080;transition:color 1s}");
  Alcotest.(check bool)
    "the longhand naming the property keeps the axes apart" false
    (equal ".x{color:oklab(0% none none/.5);transition-property:color}"
       ".x{color:#00000080;transition-property:color}");
  Alcotest.(check bool)
    "all names every property, this one included" false
    (equal ".x{color:oklab(0% none none/.5);transition-property:all}"
       ".x{color:#00000080;transition-property:all}");
  (* A shorthand in [transition-property] transitions the longhands it covers,
     so [background] reaches [background-color]. *)
  Alcotest.(check bool)
    "a transitioned shorthand reaches its colour longhand" false
    (equal
       ".x{background-color:oklab(0% none none/.5);transition:background 1s}"
       ".x{background-color:#00000080;transition:background 1s}");
  (* Nothing interpolates the colour here, so sec. 4.4 reads the missing axes as
     zero and the two spellings are one colour again. *)
  Alcotest.(check bool)
    "a transition on another property leaves the fold alone" true
    (equal ".x{color:oklab(0% none none/.5);transition:opacity 1s}"
       ".x{color:#00000080;transition:opacity 1s}");
  Alcotest.(check bool)
    "none transitions nothing" true
    (equal ".x{color:oklab(0% none none/.5);transition-property:none}"
       ".x{color:#00000080;transition-property:none}");
  (* The static case the fold exists for. *)
  Alcotest.(check bool)
    "a rule with no transition still folds" true
    (equal ".x{color:oklab(0% none none/.5)}" ".x{color:#00000080}");
  (* The blocks that exist to be an interpolation endpoint are skipped whole,
     transition or no transition. *)
  Alcotest.(check bool)
    "a keyframe keeps its missing axes" false
    (equal "@keyframes k{from{color:oklab(0% none none)}to{color:#fff}}"
       "@keyframes k{from{color:#000}to{color:#fff}}");
  Alcotest.(check bool)
    "a @starting-style value keeps its missing axes" false
    (equal "@starting-style{.x{color:oklab(0% none none)}}"
       "@starting-style{.x{color:#000}}")

(* The same sec. 4.4 reading in a custom property: a colour function in the
   stream is a colour wherever the stream substitutes, so its missing axes are
   the zeros the longhand's are. lightningcss writes Tailwind's [color-mix(in
   srgb, rgb(0 0 0) 50%, transparent)] as an OKLab with [none] chroma axes where
   tw writes the hex. The carve-outs hold as for a longhand: a rule
   transitioning the property, a keyframe and a [@starting-style] block keep the
   axes as written. *)
let canonical_custom_missing_color_components_as_zero () =
  let equal a b = Cascade_diff.Css_compare.equal ~mode:`Canonical a b in
  Alcotest.(check bool)
    "missing OKLab axes in a custom stream read as zero against the hex" true
    (equal ".a{--tw-mask-top-from-color:oklab(0% none none/.5)}"
       ".a{--tw-mask-top-from-color:#00000080}");
  Alcotest.(check bool)
    "a missing OKLCh chroma and hue too" true
    (equal ".a{--c:oklch(0% none none/.5)}" ".a{--c:#00000080}");
  Alcotest.(check bool)
    "a missing Lab axis too" true
    (equal ".a{--c:lab(0% none none/.5)}" ".a{--c:#00000080}");
  Alcotest.(check bool)
    "a missing LCh chroma and hue too" true
    (equal ".a{--c:lch(0% none none/.5)}" ".a{--c:#00000080}");
  Alcotest.(check bool)
    "the resolved colour keeps its alpha" false
    (equal ".a{--c:oklab(0% none none/.5)}" ".a{--c:#00000040}");
  Alcotest.(check bool)
    "a rule transitioning the property keeps the axes apart" false
    (equal ".a{--c:oklab(0% none none/.5);transition:--c 1s}"
       ".a{--c:#00000080;transition:--c 1s}");
  Alcotest.(check bool)
    "a keyframe keeps its missing axes" false
    (equal "@keyframes k{from{--c:oklab(0% none none)}to{--c:#fff}}"
       "@keyframes k{from{--c:#000}to{--c:#fff}}")

let canonical_custom_font_family_quotes () =
  (* A quoted multi-word font name and the unquoted ident sequence substitute
     identically into font-family, so canonical comparison equates them inside a
     custom-property body even though both forms stay verbatim on output. *)
  Alcotest.(check bool)
    "quoted and unquoted multi-word font name compare equal in a custom \
     property"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       {|:root { --font-sans: ui-sans-serif, system-ui, "Noto Color Emoji" }|}
       {|:root { --font-sans: ui-sans-serif, system-ui, Noto Color Emoji }|});
  (* A single-word string could be a custom-ident in a non-font substitution
     context, so it stays opaque: "foo" and foo are not equated. *)
  Alcotest.(check bool)
    "single-word quoted string stays distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical {|.a { --x: "foo" }|}
       {|.a { --x: foo }|})

let canonical_custom_calc_percentage () =
  let expected = ".a { --tw-translate-x: calc(1 / 2 * 100%) }" in
  let actual = ".a { --tw-translate-x: 50% }" in
  Alcotest.(check bool)
    "unregistered custom calc token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_custom_calc_constant_fold () =
  (* A math function whose operands are all constants reduces to the same number
     in every substitution site, so [calc(2.25/1.875)] and [1.2] compare equal
     inside a custom property. A calc that carries units or a var() does not
     reduce and stays opaque. *)
  Alcotest.(check bool)
    "constant number calc folds in a custom property" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --lh: calc(2.25 / 1.875) }" ".a { --lh: 1.2 }");
  Alcotest.(check bool)
    "calc with a var stays distinct in a custom property" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a { --x: calc(var(--y) * 2) }" ".a { --x: 2 }")

let semantic_with_recovered_warning () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent; \
     color: " ^ color_mix_transparent ^ " }"
  in
  let actual =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: #0000; color: "
    ^ color_mix_transparent_hex ^ " }"
  in
  Alcotest.(check bool)
    "canonical comparison falls back to lenient canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_var_fallback () =
  let expected =
    ".a { --tw-ring-shadow: 0 0 0 1px var(--tw-ring-color, transparent) }"
  in
  let actual =
    ".a { --tw-ring-shadow: 0 0 0 1px var(--tw-ring-color, #0000) }"
  in
  Alcotest.(check bool)
    "shadow colour var fallbacks compare after typed canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_nested_functions () =
  let expected =
    ".a { --tw-image: linear-gradient(transparent, " ^ color_mix_transparent
    ^ ") }"
  in
  let actual =
    ".a { --tw-image: linear-gradient(#0000, " ^ color_mix_transparent_hex
    ^ ") }"
  in
  Alcotest.(check bool)
    "nested custom function token streams are compared after minification" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_custom_oklab_sign_boundaries () =
  let expected =
    ".prose { --tw-prose-kbd-shadows: oklab(21% -.00316127 -.0338527 / .1) }"
  in
  let actual = ".prose { --tw-prose-kbd-shadows: oklab(21%-.003 -.034/.1) }" in
  Alcotest.(check bool)
    "unregistered OKLab custom property token streams stay opaque" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_prose_shadow_color_var () =
  let selector =
    ".hover\\:prose:hover \
     :where(kbd):not(:where([class~=not-prose],[class~=not-prose] *))"
  in
  let expected =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 0 0 1px var(--tw-prose-kbd-shadows),0 3px 0 \
       var(--tw-prose-kbd-shadows)}.hover\\:prose:hover{--tw-prose-kbd-shadows:oklab(21%-.003 \
       -.034/.1)}}}"
  in
  let actual =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 0 0 1px var(--tw-prose-kbd-shadows),0 3px \
       var(--tw-prose-kbd-shadows)}.hover\\:prose:hover{--tw-prose-kbd-shadows:oklab(21%-.003 \
       -.034/.1)}}}"
  in
  Alcotest.(check bool)
    "unregistered custom property cannot justify zero blur elision before var()"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_prose_shadow_unknown_var () =
  let selector =
    ".hover\\:prose:hover \
     :where(kbd):not(:where([class~=not-prose],[class~=not-prose] *))"
  in
  let expected =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 3px 0 var(--tw-prose-kbd-shadows)}}}"
  in
  let actual =
    "@layer utilities{@media (hover: hover){" ^ selector
    ^ "{box-shadow:0 3px var(--tw-prose-kbd-shadows)}}}"
  in
  Alcotest.(check bool)
    "unknown custom property cannot justify zero blur elision before var()"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_before_content_redundant_seed () =
  let selector = ".before\\:content-\\[\\\"\\>\\\"\\]:before" in
  let expected =
    "@layer utilities{" ^ selector
    ^ "{--tw-content:\">\";content:var(--tw-content)}}"
  in
  let actual =
    "@layer utilities{" ^ selector
    ^ "{content:var(--tw-content);--tw-content:\">\";content:var(--tw-content)}}"
  in
  Alcotest.(check bool)
    "leading duplicate content var() is dead before later content declaration"
    true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_minified css =
  match Css.of_string ~strict:true css with
  | Ok { Css.stylesheet; _ } -> Css.to_string ~minify:true stylesheet
  | Error error -> Alcotest.fail (Error.to_string error)

let canonical_overflow_trailing_ws () =
  let expected = ".sr-only { overflow: hidden; }" in
  let actual = ".sr-only{overflow:hidden}" in
  Alcotest.(check string)
    "pretty overflow declaration survives canonicalization"
    ".sr-only{overflow:hidden}"
    (canonical_minified expected);
  Alcotest.(check string)
    "minified overflow declaration survives canonicalization"
    ".sr-only{overflow:hidden}"
    (canonical_minified actual);
  Alcotest.(check bool)
    "pretty and minified overflow compare canonically equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* CSS Values L4 sec. 10.10: inside math functions (calc/min/max/clamp/mod/rem/
   round/sin/cos/tan/asin/acos/atan/atan2/pow/sqrt/hypot/log/exp/abs/sign),
   whitespace is OPTIONAL around *, /, (, ), and ,; it is REQUIRED around + and
   - (sign disambiguation, var(--a)-var(--b)). Whitespace inside the math
   grammar is purely lexical with no consumer-context dependence, so canonical
   comparison normalizes it - including inside custom-property values, where the
   calc body is otherwise round-tripped verbatim. *)

let canonical_bare_ratio_whitespace_equal () =
  (* The motivating cross-tool case: Tailwind authors [--aspect-video: 16 / 9]
     while a typed ratio serialises as [16/9]; the whitespace around [/] is
     insignificant wherever the stream is substituted, so canonical comparison
     folds it even outside a math function. *)
  let expected = ".x{--aspect-video: 16 / 9}" in
  let actual = ".x{--aspect-video:16/9}" in
  Alcotest.(check bool)
    "bare ratio whitespace around / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_calc_mul_div_whitespace_equal () =
  let expected = ".x{--v:calc(1 / 2 * 100%)}" in
  let actual = ".x{--v:calc(1/2*100%)}" in
  Alcotest.(check bool)
    "calc(...) whitespace around * and / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* A [var()] fallback the slot's reader could not type is a
   [<declaration-value>] that slot's grammar reads once substituted, so the
   whitespace beside the alpha slash of a colour written there is spelling, as
   it is at a bare colour slot where the reader types it. The shadow readers
   take the third slot as a length, which is where a colour fallback lands
   untyped; a bare typed slot holds the same stream. Both compare the way the
   browser paints them. *)
let canonical_typed_slot_fallback_slash_whitespace_equal () =
  let check name expected actual =
    Alcotest.(check bool)
      name true
      (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)
  in
  check "text-shadow oklab fallback alpha slash whitespace"
    ".a{text-shadow:12px 12px var(--c, oklab(59.982% -.067 -.124 / .25))}"
    ".a{text-shadow:12px 12px var(--c, oklab(59.982% -.067 -.124/.25))}";
  check "box-shadow rgb fallback alpha slash whitespace"
    ".a{box-shadow:12px 12px var(--c, rgb(1 2 3 / .25))}"
    ".a{box-shadow:12px 12px var(--c, rgb(1 2 3/.25))}";
  check "typed colour slot fallback alpha slash whitespace"
    ".a{color:var(--c, oklab(59.982% -.067 -.124 / .25))}"
    ".a{color:var(--c, oklab(59.982% -.067 -.124/.25))}";
  check "bare typed slot untyped fallback slash whitespace"
    ".a{width:var(--w, rgb(1 2 3 / .5))}" ".a{width:var(--w, rgb(1 2 3/.5))}";
  (* The separator sec. 10.8 requires around a math [+] or [-] stays a
     difference: without it the fallback is a different token stream. *)
  Alcotest.(check bool)
    "untyped fallback math sign whitespace stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a{color:var(--c, calc(1px - 2px))}"
       ".a{color:var(--c, calc(1px -2px))}")

(* A [var()] that ends a shadow's length run with a colour for its fallback is
   the colour slot's reference: the browser substitutes that colour wherever the
   custom property is unset. Typed, its fallback compares as a colour, so [25%]
   and [.25] are one alpha there as they are at a bare colour slot. *)
let canonical_shadow_color_var_fallback_alpha_equal () =
  let check name expected actual =
    Alcotest.(check bool)
      name true
      (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)
  in
  check "text-shadow relative oklab fallback alpha spelling"
    ".a{text-shadow:1px 1px var(--c, oklab(from red l a b / 25%))}"
    ".a{text-shadow:1px 1px var(--c, oklab(from red l a b/.25))}";
  check "box-shadow rgb fallback alpha spelling"
    ".a{box-shadow:1px 1px var(--c, rgb(1 2 3 / 25%))}"
    ".a{box-shadow:1px 1px var(--c, rgb(1 2 3/.25))}";
  check "box-shadow blur then fallback alpha spelling"
    ".a{box-shadow:1px 1px 2px var(--c, rgb(1 2 3 / 25%)) inset}"
    ".a{box-shadow:inset 1px 1px 2px var(--c, rgb(1 2 3/.25))}";
  check "drop-shadow fallback alpha spelling"
    ".a{filter:drop-shadow(1px 1px var(--c, rgb(1 2 3 / 25%)))}"
    ".a{filter:drop-shadow(1px 1px var(--c, rgb(1 2 3/.25)))}";
  check "shadow list fallback alpha spelling"
    ".a{box-shadow:1px 1px var(--c, rgb(1 2 3 / 25%)),2px 2px var(--d, #0004)}"
    ".a{box-shadow:1px 1px var(--c, rgb(1 2 3/.25)),2px 2px var(--d, \
     #00000044)}"

let canonical_calc_paren_whitespace_equal () =
  let expected = ".x{--v:calc( ( 1 / 2 ) * 100% )}" in
  let actual = ".x{--v:calc((1/2)*100%)}" in
  Alcotest.(check bool)
    "calc(...) whitespace around ( and ) canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_min_comma_whitespace_equal () =
  let expected = ".x{--v:min(50px, 100% / 2)}" in
  let actual = ".x{--v:min(50px,100%/2)}" in
  Alcotest.(check bool)
    "min(...) whitespace around , and / canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_clamp_commas_whitespace_equal () =
  let expected = ".x{--v:clamp(1rem, 4vw, 2rem)}" in
  let actual = ".x{--v:clamp(1rem,4vw,2rem)}" in
  Alcotest.(check bool)
    "clamp(...) comma whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_round_strategy_whitespace_equal () =
  (* round(...) takes a rounding-strategy keyword (up/down/nearest/to-zero) as
     its first argument; the , whitespace rule applies the same as elsewhere. *)
  let expected = ".x{--v:round(up, 3.5, 1)}" in
  let actual = ".x{--v:round(up,3.5,1)}" in
  Alcotest.(check bool)
    "round(<strategy>, ...) comma whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_trig_whitespace_equal () =
  let expected = ".x{--v:sin( 45deg )}" in
  let actual = ".x{--v:sin(45deg)}" in
  Alcotest.(check bool)
    "trig function ( and ) whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_nested_math_whitespace_equal () =
  let expected = ".x{--v:min( 50px, calc( 100% / 2 ) )}" in
  let actual = ".x{--v:min(50px,calc(100%/2))}" in
  Alcotest.(check bool)
    "nested math-function whitespace canonicalizes equal recursively" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let canonical_tailwind_translate_calc_equal () =
  (* The original motivating case: Tailwind's translate-x-1/2 emits calc(1 / 2 *
     100%) into --tw-translate-x, while cascade's pp_calc produces
     calc(1/2*100%). Custom-property values don't go through pp_calc so the
     divergence is purely whitespace inside the math body - canonical comparison
     must fold it. *)
  let expected = ".x{--tw-translate-x:calc(1 / 2 * 100%)}" in
  let actual = ".x{--tw-translate-x:calc(1/2*100%)}" in
  Alcotest.(check bool)
    "Tailwind --tw-translate-x:calc(...) whitespace canonicalizes equal" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* SAFETY: + and - whitespace is REQUIRED by the spec and must NOT be normalized
   away. The stripped form is illegal (parses as a different token sequence:
   100% followed by -var ident, then (--a)), and treating it equal would
   silently mask a real bug. *)
let canonical_calc_plus_minus_whitespace_distinct () =
  let expected = ".x{--v:calc(100% - var(--a))}" in
  let actual = ".x{--v:calc(100%-var(--a))}" in
  Alcotest.(check bool)
    "calc(...) + and - whitespace stays distinct (required for parse)" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Fonts L4 sec. 2.1.1: a font family name is either a <string> or a sequence of
   <custom-ident>s joined with single spaces; the two forms are equivalent for
   matching whenever the unquoted form would be valid identifiers. The
   equivalence is consumer-dependent (it only holds in font-family-typed
   contexts), so cascade applies the fold at the typed leaf and at
   font-family-registered custom properties, but never at unregistered custom
   properties (consumer unknown). *)

let canonical_font_family_quoted_unquoted_equal () =
  let expected = ".x{font-family:\"Segoe UI Symbol\"}" in
  let actual = ".x{font-family:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "typed font-family: quoted string equals unquoted ident sequence" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* SAFETY: a quoted generic-family token is a literal font name of that
   spelling, not the generic family - they must stay distinct. Same applies to
   sans-serif/monospace/cursive/fantasy/system-ui/ui-*/math/emoji/fangsong, and
   to CSS-wide keywords like "inherit"/"initial"/etc. *)
let canonical_font_family_generic_keyword_distinct () =
  let expected = ".x{font-family:\"serif\"}" in
  let actual = ".x{font-family:serif}" in
  Alcotest.(check bool)
    "typed font-family: quoted \"serif\" literal stays distinct from generic"
    false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* Unregistered custom property: the consumer is unknown (var(--font) could land
   in content: where the quoted/unquoted distinction is observable), so the fold
   must not apply - same call as the calc number_percentage handling for
   unregistered custom props. *)
let canonical_unregistered_custom_font_distinct () =
  let expected = ".x{--font:\"Segoe UI Symbol\"}" in
  let actual = ".x{--font:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "unregistered --font custom property: quoted/unquoted stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

(* [<custom-ident>+#] chains two multipliers where CSS Properties and Values API
   1 (ED) sec. 5.2 allows one per syntax component, so the registration is
   invalid and [--font] stays an ordinary unregistered custom property. The
   unregistered case above already settles that state: the consumer is unknown,
   so the two spellings stay distinct. No registration reaches the font-family
   position that would make them one name either, [<family-name>] not being
   among the syntax component names ED sec. 5.1 supports. *)
let canonical_invalid_registration_font_distinct () =
  let registration =
    "@property \
     --font{syntax:\"<custom-ident>+#\";inherits:true;initial-value:serif}"
  in
  let expected = registration ^ ".x{--font:\"Segoe UI Symbol\"}" in
  let actual = registration ^ ".x{--font:Segoe UI Symbol}" in
  Alcotest.(check bool)
    "invalid @property registration: quoted/unquoted stays distinct" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_legacy_pseudo_element_alias () =
  let legacy =
    "@layer utilities{.prose :where(blockquote \
     p:first-of-type):not(:where([class~=not-prose],[class~=not-prose] \
     *)):before{content:\"\"}}"
  in
  let modern =
    "@layer utilities{.prose :where(blockquote \
     p:first-of-type):not(:where([class~=not-prose],[class~=not-prose] \
     *))::before{content:\"\"}}"
  in
  match
    (Cascade_diff.Css_compare.diff ~mode:`Canonical legacy modern)
      .Cascade_diff.Css_compare.result
  with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ -> Alcotest.fail "expected :before and ::before to canonicalize equally"

let semantic_vendor_recovered () =
  let expected =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: transparent }"
  in
  let actual =
    "@unknown { color: red } .a { -webkit-tap-highlight-color: #0000 }"
  in
  Alcotest.(check bool)
    "vendor color aliases survive lenient canonicalization" true
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_transparent_string_is_literal () =
  let expected = ".a { --tw-content: \"transparent\" }" in
  let actual = ".a { --tw-content: \"#0000\" }" in
  Alcotest.(check bool)
    "quoted transparent is not a color alias" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantic_transparent_ident_boundary () =
  let expected = ".a { --tw-filter: transparentize(0.5) }" in
  let actual = ".a { --tw-filter: #0000ize(0.5) }" in
  Alcotest.(check bool)
    "transparent identifier substrings are not color aliases" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical expected actual)

let semantically_equivalent_rejects_different_colors () =
  Alcotest.(check bool)
    "different color values are not semantically equivalent" false
    (Cascade_diff.Css_compare.equivalent_value ~property:"color" "red" "blue")

(* [equivalent_value] gives its two values the declaration context [property]
   names. CSS Syntax 3 sec. 4.3.7 lets an escape carry a [;] or a [}] into a
   custom property's name, and written raw into that context such a name closes
   the rule and takes both values with it, so any two of them read back the
   same. *)
let semantically_equivalent_escaping_property () =
  List.iter
    (fun property ->
      Alcotest.(check bool)
        (String.concat "" [ property; ": red and blue stay different" ])
        false
        (Cascade_diff.Css_compare.equivalent_value ~property "red" "blue");
      Alcotest.(check bool)
        (String.concat ""
           [ property; ": the same colour spelled twice agrees" ])
        true
        (Cascade_diff.Css_compare.equivalent_value ~property "#ffffff" "#fff"))
    [ "--x"; "--x;y"; "--x}y"; "--x y" ]

let suite =
  ( "css_compare_values",
    [
      Alcotest.test_case "semantically equivalent color-mix values" `Quick
        semantically_equivalent_color_mix_values;
      Alcotest.test_case "semantically equivalent color-mix CSS" `Quick
        semantically_equivalent_color_mix_css;
      Alcotest.test_case "canonical custom property tokens" `Quick
        canonical_custom_property_tokens;
      Alcotest.test_case "registered custom property tokens" `Quick
        registered_custom_property_tokens;
      Alcotest.test_case "canonical custom color-mix tokens" `Quick
        canonical_custom_color_mix_tokens;
      Alcotest.test_case "canonical custom color-function alpha" `Quick
        canonical_custom_color_function_alpha;
      Alcotest.test_case "canonical custom shadow colour" `Quick
        canonical_custom_shadow_color;
      Alcotest.test_case "canonical relative-colour origin" `Quick
        canonical_relative_color_origin;
      Alcotest.test_case "canonical fully transparent missing oklab" `Quick
        canonical_fully_transparent_missing_oklab;
      Alcotest.test_case "canonical missing colour components as zero" `Quick
        canonical_missing_color_components_as_zero;
      Alcotest.test_case "canonical missing components under interpolation"
        `Quick canonical_missing_components_survive_interpolation;
      Alcotest.test_case "canonical missing components under a transition"
        `Quick canonical_missing_components_under_transition;
      Alcotest.test_case "canonical missing components in a custom property"
        `Quick canonical_custom_missing_color_components_as_zero;
      Alcotest.test_case "canonical custom font-family quotes" `Quick
        canonical_custom_font_family_quotes;
      Alcotest.test_case "canonical custom calc percentage" `Quick
        canonical_custom_calc_percentage;
      Alcotest.test_case "canonical custom calc constant fold" `Quick
        canonical_custom_calc_constant_fold;
      Alcotest.test_case "semantic with recovered warning" `Quick
        semantic_with_recovered_warning;
      Alcotest.test_case "semantic custom var fallback" `Quick
        semantic_custom_var_fallback;
      Alcotest.test_case "semantic custom nested functions" `Quick
        semantic_custom_nested_functions;
      Alcotest.test_case "semantic custom OKLab sign boundaries" `Quick
        semantic_custom_oklab_sign_boundaries;
      Alcotest.test_case "semantic hover prose kbd shadow color var" `Quick
        semantic_prose_shadow_color_var;
      Alcotest.test_case "semantic hover prose kbd shadow unknown var" `Quick
        semantic_prose_shadow_unknown_var;
      Alcotest.test_case "semantic before content redundant seed" `Quick
        semantic_before_content_redundant_seed;
      Alcotest.test_case "canonical overflow before semicolon" `Quick
        canonical_overflow_trailing_ws;
      Alcotest.test_case "canonical bare ratio whitespace" `Quick
        canonical_bare_ratio_whitespace_equal;
      Alcotest.test_case "canonical calc *,/ whitespace" `Quick
        canonical_calc_mul_div_whitespace_equal;
      Alcotest.test_case "canonical typed slot fallback slash whitespace" `Quick
        canonical_typed_slot_fallback_slash_whitespace_equal;
      Alcotest.test_case "canonical shadow colour var fallback alpha" `Quick
        canonical_shadow_color_var_fallback_alpha_equal;
      Alcotest.test_case "canonical calc paren whitespace" `Quick
        canonical_calc_paren_whitespace_equal;
      Alcotest.test_case "canonical min comma whitespace" `Quick
        canonical_min_comma_whitespace_equal;
      Alcotest.test_case "canonical clamp commas whitespace" `Quick
        canonical_clamp_commas_whitespace_equal;
      Alcotest.test_case "canonical round strategy whitespace" `Quick
        canonical_round_strategy_whitespace_equal;
      Alcotest.test_case "canonical trig whitespace" `Quick
        canonical_trig_whitespace_equal;
      Alcotest.test_case "canonical nested math whitespace" `Quick
        canonical_nested_math_whitespace_equal;
      Alcotest.test_case "canonical Tailwind translate calc whitespace" `Quick
        canonical_tailwind_translate_calc_equal;
      Alcotest.test_case "canonical calc +/- whitespace stays distinct" `Quick
        canonical_calc_plus_minus_whitespace_distinct;
      Alcotest.test_case "canonical typed font-family quoted vs unquoted" `Quick
        canonical_font_family_quoted_unquoted_equal;
      Alcotest.test_case "canonical font-family generic keyword distinct" `Quick
        canonical_font_family_generic_keyword_distinct;
      Alcotest.test_case "canonical unregistered --font distinct" `Quick
        canonical_unregistered_custom_font_distinct;
      Alcotest.test_case "canonical invalid --font registration distinct" `Quick
        canonical_invalid_registration_font_distinct;
      Alcotest.test_case "semantic legacy pseudo-element alias" `Quick
        semantic_legacy_pseudo_element_alias;
      Alcotest.test_case "semantic vendor color with recovered warning" `Quick
        semantic_vendor_recovered;
      Alcotest.test_case "semantic transparent string is literal" `Quick
        semantic_transparent_string_is_literal;
      Alcotest.test_case "semantic transparent ident boundary" `Quick
        semantic_transparent_ident_boundary;
      Alcotest.test_case "semantically equivalent rejects different colors"
        `Quick semantically_equivalent_rejects_different_colors;
      Alcotest.test_case "semantically equivalent under an escaping property"
        `Quick semantically_equivalent_escaping_property;
      Alcotest.test_case "canonical folds a zero hue-rotate in a custom value"
        `Quick canonical_custom_hue_rotate_zero;
      Alcotest.test_case "canonical custom signed leading zero" `Quick
        equal_canonical_custom_signed_leading_zero;
      Alcotest.test_case "canonical stroke width number" `Quick
        equal_canonical_stroke_width_number;
      Alcotest.test_case "canonical infinite length" `Quick
        equal_canonical_infinite_length;
      Alcotest.test_case "canonical flex-basis percentage calc" `Quick
        equal_canonical_flex_basis_percentage_calc;
      Alcotest.test_case "canonical custom time units" `Quick
        equal_canonical_custom_time_units;
      Alcotest.test_case "canonical angle units" `Quick
        equal_canonical_angle_units;
      Alcotest.test_case "canonical relative colour pass-through" `Quick
        equal_canonical_relative_color_pass_through;
      Alcotest.test_case "canonical light-dark mix" `Quick
        equal_canonical_light_dark_mix;
      Alcotest.test_case "canonical lossless equates exact srgb spellings"
        `Quick equal_canonical_lossless_exact_srgb;
      Alcotest.test_case "canonical numeric precision modes" `Quick
        canonical_numeric_division_follows_precision_mode;
    ] )
