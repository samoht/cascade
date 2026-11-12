(** Declaration tests: custom properties, token boundaries, [!important],
    invalid values and CSS-wide keywords. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

let custom_properties () =
  (* Basic custom properties *)
  check_declaration ~expected:"--color:red" "--color: red";
  check_declaration ~expected:"--my-var:10px" "--my-var: 10px";
  check_declaration ~expected:"--complex:1px solid black"
    "--complex: 1px solid black";

  (* With var() references *)
  check_declaration ~expected:"--primary:var(--base-color)"
    "--primary: var(--base-color)";
  check_declaration ~expected:"--size:calc(var(--base) * 2)"
    ~optimized:"--size:calc(var(--base)*2)" "--size: calc(var(--base) * 2)";
  check_declaration ~expected:"--aspect-video:16 / 9"
    ~optimized:"--aspect-video:16/9" "--aspect-video: 16 / 9";
  check_declaration ~expected:"--stops:var(--from) var(--from-position)"
    "--stops: var(--from) var(--from-position)";
  check_declaration ~expected:"--fallback:var(--undefined,10px)"
    "--fallback: var(--undefined, 10px)";

  (* var() with empty fallback - declaration level coverage *)
  check_declaration ~expected:"color:var(--x,)" "color: var(--x,)";
  check_declaration ~expected:"background:var(--bg,)" "background: var(--bg,)";
  (* CSS Custom Properties for Cascading Variables 1 sec. 3: [var()] takes a
     name and an optional [, <fallback>]; content after the name without a
     leading comma is not part of the grammar (CSS Syntax 3 sec. 8.2 invalidates
     the declaration). [Cursor.call] did not require [var()]'s reader to consume
     its whole sub-cursor, so [var(--x 10px)] silently dropped [ 10px] and
     answered [var(--x)]. *)
  neg_cursor read_declaration "color:var(--x 10px)";
  (* CSS Color 5 sec. 4.1 substitutes a relative colour's channel keyword as a
     [<number>], so adding a percentage or an angle to one is the same type
     error it is anywhere else in a math function. Chrome 153 refuses every one
     of these and takes the multiplying forms above. *)
  neg_cursor read_declaration "color: rgb(from red calc(r + 10%) g b)";
  neg_cursor read_declaration "color: hsl(from red calc(h + 90deg) s l)";
  neg_cursor read_declaration "color: hsl(from red h calc(s + 10%) l)";
  neg_cursor read_declaration "color: hwb(from red h calc(w + 10%) b)";
  neg_cursor read_declaration "color: lab(from red calc(l + 10%) a b)";
  neg_cursor read_declaration "color: lch(from red l c calc(h + 90deg))";
  neg_cursor read_declaration "color: oklab(from red calc(l + 10%) a b)";
  neg_cursor read_declaration "color: color(from red srgb calc(r + 10%) g b)";
  check_declaration ~expected:"color:var(--x)" "color: var( --x )"

(* A numeric token's source spelling is not part of its value. Opaque custom-
   and unknown-property streams therefore use the same shortest numeric spelling
   as typed values, while retaining separators when dropping a sign would
   otherwise merge two tokens. *)
let opaque_numeric_tokens () =
  check_declaration ~expected:"--t:1px" "--t: 1.0px";
  check_declaration ~expected:"--t:.5px" "--t: 0.5px";
  check_declaration ~expected:"--t:.5" "--t: .50";
  check_declaration ~expected:"--t:1px" "--t: 01px";
  check_declaration ~expected:"--t:1" "--t: +1";
  check_declaration ~expected:"-x-y:.5%" "-x-y: 0.50%";
  check_declaration ~expected:"--t:x 1" "--t: x +1";
  check_declaration ~expected:"--t:1 2px" "--t: 1 +2px";
  check_declaration ~expected:"--t:- 1" "--t: - +1"

(* CSS Values 4 (ED) sec. 10.8 "Syntax": inside a math function whitespace is
   required on both sides of the [+] and [-] operators, while [*] and [/] may be
   written without any. A custom property and an unknown property both carry an
   opaque token stream that is minified token by token, so deleting that
   whitespace hands the browser a declaration it drops on the floor. *)
let math_sign_whitespace () =
  check_declaration ~expected:"--t:calc(100% - 10px)"
    ~optimized:"--t:calc(100% - 10px)" "--t: calc(100% - 10px)";
  check_declaration ~expected:"--t:calc(100% + 10px)"
    ~optimized:"--t:calc(100% + 10px)" "--t: calc(100% + 10px)";
  check_declaration ~expected:"--t:calc((100%) - 10px)"
    ~optimized:"--t:calc((100%) - 10px)" "--t: calc((100%) - 10px)";
  check_declaration ~expected:"--t:calc(min(1px,2px) - 3px)"
    ~optimized:"--t:calc(min(1px,2px) - 3px)" "--t: calc(min(1px,2px) - 3px)";
  check_declaration ~expected:"--t:calc(50% + var(--a))"
    ~optimized:"--t:calc(50% + var(--a))" "--t: calc(50% + var(--a))";
  (* An unknown property holds the same opaque stream. *)
  check_declaration ~expected:"-x-y:calc(100% - 10px)"
    ~optimized:"-x-y:calc(100% - 10px)" "-x-y: calc(100% - 10px)";
  check_declaration ~expected:"-x-y:calc(min(1px,2px) - 3px)"
    ~optimized:"-x-y:calc(min(1px,2px) - 3px)" "-x-y: calc(min(1px,2px) - 3px)";
  (* The rule is about a [+] / [-] delim token. [calc(1 +2)] lexes as two number
     tokens with nothing between them but whitespace, so it keeps folding. *)
  check_declaration ~expected:"--t:calc(1+2)" ~optimized:"--t:calc(1+2)"
    "--t: calc(1 +2)";
  (* The reader keeps what it read: the whitespace-free spelling is a different
     (invalid) declaration, and minifying never inserts a separator. *)
  check_declaration ~expected:"--t:calc(100%- 10px)"
    ~optimized:"--t:calc(100%- 10px)" "--t: calc(100%- 10px)";
  (* Whitespace that no grammar asks for still folds. *)
  check_declaration ~expected:"--t:16 / 9" ~optimized:"--t:16/9" "--t: 16 / 9";
  check_declaration ~expected:"--t:calc(var(--base) * 2)"
    ~optimized:"--t:calc(var(--base)*2)" "--t: calc(var(--base) * 2)";
  check_declaration ~expected:"--a:cubic-bezier(.4,0,.6,1) infinite"
    ~optimized:"--a:cubic-bezier(.4,0,.6,1)infinite"
    "--a: cubic-bezier(.4,0,.6,1) infinite";
  check_declaration ~expected:"--t:100%x" ~optimized:"--t:100%x" "--t: 100% x"

(* A [var()] fallback the slot's reader could not type is still read by that
   slot's grammar once substituted, so the whitespace beside its [/] is
   spelling, and the printer drops it the way it does in any typed value. A
   custom property's own value keeps it (above), as its consumer is unknown. A
   shadow's length run holds such a stream where a colour fallback cannot move
   to the colour slot, as when another length follows the reference. *)
let typed_slot_fallback_whitespace () =
  check_declaration
    ~expected:"text-shadow:12px 12px var(--c,oklab(59.982%-.067-.124/.25)) 1px"
    ~optimized:"text-shadow:12px 12px var(--c,oklab(59.982%-.067-.124/.25)) 1px"
    "text-shadow: 12px 12px var(--c, oklab(59.982% -.067 -.124 / .25)) 1px";
  check_declaration ~expected:"box-shadow:12px 12px var(--c,rgb(1 2 3/.25)) 1px"
    ~optimized:"box-shadow:12px 12px var(--c,rgb(1 2 3/.25)) 1px"
    "box-shadow: 12px 12px var(--c, rgb(1 2 3 / .25)) 1px";
  check_declaration ~expected:"width:var(--w,rgb(1 2 3/.5))"
    ~optimized:"width:var(--w,rgb(1 2 3/.5))" "width: var(--w, rgb(1 2 3 / .5))";
  (* Sec. 10.8 still requires the space around a math [+] or [-]. *)
  check_declaration ~expected:"color:var(--c,calc(1px - 2px))"
    ~optimized:"color:var(--c,calc(1px - 2px))"
    "color: var(--c, calc(1px - 2px))"

(* CSS Syntax 3 (ED) sec. 4.3.9 "Would start an identifier" and sec. 4.3.10
   "Would start a number" fix what a token absorbs on re-lexing. A [+] is
   neither a name code point nor a continuation of the number in front of it, so
   a plus-signed numeric always re-lexes on its own; a [-] is a name code point,
   so it only does after a number. A [(] joins the ident before it and nothing
   else. Writing a separator into any of those pairs hands [var()] a token
   sequence the source never held, which is the one thing minification may not
   do. *)
let inserted_token_boundary () =
  (* A plus-signed numeric after a name, a number or a dimension. *)
  check_declaration ~expected:"--t:x 1px+2px" ~optimized:"--t:x 1px+2px"
    "--t: x 1px+2px";
  check_declaration ~expected:"--t:span+2" ~optimized:"--t:span+2" "--t: span+2";
  check_declaration ~expected:"--t:#abc+1px" ~optimized:"--t:#abc+1px"
    "--t: #abc+1px";
  check_declaration ~expected:"--t:@foo+2px" ~optimized:"--t:@foo+2px"
    "--t: @foo+2px";
  check_declaration ~expected:"--t:1e3+2%" ~optimized:"--t:1e3+2%" "--t: 1e3+2%";
  (* A minus-signed numeric after a number: the number cannot absorb the [-]. *)
  check_declaration ~expected:"--t:9-9px" ~optimized:"--t:9-9px" "--t: 9-9px";
  (* A parenthesised block only makes a function token of a preceding ident. *)
  check_declaration ~expected:"--t:1px(a)" ~optimized:"--t:1px(a)" "--t: 1px(a)";
  check_declaration ~expected:"--t:#abc(a)" ~optimized:"--t:#abc(a)"
    "--t: #abc(a)";
  check_declaration ~expected:"--t:@foo(a)" ~optimized:"--t:@foo(a)"
    "--t: @foo(a)";
  (* An unknown property carries the same opaque stream. *)
  check_declaration ~expected:"-x-y:1px+2px" ~optimized:"-x-y:1px+2px"
    "-x-y: 1px+2px";
  (* Controls. A [-] after a dimension is read into the unit, so the separator
     carries the boundary and stays. *)
  check_declaration ~expected:"--t:x 1px -2px" ~optimized:"--t:x 1px -2px"
    "--t: x 1px -2px";
  check_declaration ~expected:"-x-y:1px -2px" ~optimized:"-x-y:1px -2px"
    "-x-y: 1px -2px";
  (* An unsigned numeric merges into the number or the unit before it. *)
  check_declaration ~expected:"--t:1 2px" ~optimized:"--t:1 2px" "--t: 1 2px";
  check_declaration ~expected:"--t:1px solid" ~optimized:"--t:1px solid"
    "--t: 1px solid";
  check_declaration ~expected:"--t:foo bar" ~optimized:"--t:foo bar"
    "--t: foo bar";
  (* [ident(] is a function token, and a hash absorbs a following name. *)
  check_declaration ~expected:"--t:translate (1px)"
    ~optimized:"--t:translate (1px)" "--t: translate (1px)";
  check_declaration ~expected:"--t:#abc var(--x)" ~optimized:"--t:#abc var(--x)"
    "--t: #abc var(--x)";
  (* [/] then [*] would open a comment (CSS Syntax 3 (ED) sec. 4.3.2). *)
  check_declaration ~expected:"-x-y:a/ *b" ~optimized:"-x-y:a/ *b"
    "-x-y: a / *b";
  (* Two numbers already fold: the second carries its own sign. *)
  check_declaration ~expected:"-x-y:1+2" ~optimized:"-x-y:1+2" "-x-y: 1 +2"

let important () =
  (* Standard properties with !important *)
  check_declaration ~expected:"color:red!important" "color: red !important";
  check_declaration ~expected:"display:none!important"
    "display: none !important";
  check_declaration ~expected:"width:100px!important" "width: 100px !important";
  check_declaration ~expected:"margin:auto!important" "margin: auto !important";

  (* Custom properties with !important *)
  check_declaration ~expected:"--custom:value!important"
    "--custom: value !important";

  (* Spacing/format variations for !important, including comments and casing *)
  check_declaration ~expected:"color:red!important" "color: red!important";
  check_declaration ~expected:"color:red!important" "color: red ! important";
  check_declaration ~expected:"color:red!important"
    "color: red   !   important   ";
  check_declaration ~expected:"color:red!important" "color: red !IMPORTANT";
  check_declaration ~expected:"color:red!important"
    "color: red! /*x*/ important";
  check_declaration ~expected:"margin:10px!important"
    "margin: 10px!/**/important";
  (* Multiple spaces should be valid *)
  check_declaration ~expected:"color:blue!important"
    ~optimized:"color:#00f!important" "color: blue !   important";
  (* Invalid/dangling/duplicate !important should be rejected *)
  none_cursor read_declaration "color: red !;";
  none_cursor read_declaration "color: red !notimportant;";
  none_cursor read_declaration "color: red !important !important;";
  none_cursor read_declaration "color: red !importent;";
  check_declaration ~expected:"color:red!important" "color: red! important";
  (* Valid per CSS spec *)
  none_cursor read_declaration "color: red !IMPORTANT!;"

let invalid () =
  let neg = none_cursor read_declaration in
  let typed_invalid input =
    let cursor = Cursor.of_string input in
    match read_declaration cursor with
    | Some declaration ->
        Alcotest.(check bool)
          (input ^ " is represented as invalid")
          true
          (Css.Declaration.is_invalid declaration)
    | None -> Alcotest.failf "expected a typed invalid declaration for %S" input
    | exception Error.Parse_error _ ->
        Alcotest.failf "expected a typed invalid declaration for %S" input
  in
  (* Unknown property names are syntactically valid declarations. *)
  check_declaration ~expected:"not-a-property:value" "not-a-property: value";
  (* Invalid property names *)
  neg "123invalid: value";
  neg ": value";
  (* Invalid values for known properties *)
  none_cursor read_declaration "color: not-a-color";
  neg "display: not-a-display";
  neg "position: nowhere";
  neg "width: invalid";
  (* Type mismatches *)
  neg "opacity: red";
  neg "z-index: blue";
  neg "font-weight: green";
  neg "font-family: default";
  neg "font-family: system-ui default";
  (* CSS Fonts 4 sec. 2.1.1: a generic family heads the alternative the property
     reads instead of a [<font-family-name>], so it is turned away as the first
     word of a sequence. WPT css-fonts/parsing/font-family-invalid pins the
     first of these, and Chrome 153 refuses all four. *)
  neg "font-family: cursive serif";
  neg "font-family: system-ui system-ui";
  neg "font-family: serif serif";
  neg "font-family: serif Foo";
  (* CSS Values 4 sec. 4.2 excludes a CSS-wide keyword and [default] from
     [<custom-ident>] itself, so those are turned away at every word rather than
     only at the first. Chrome takes [Foo inherit] as a name; the exclusion is
     on the type, and cascade holds the strict side. *)
  neg "font-family: inherit inherit";
  neg "font-family: Foo inherit";
  neg "font-family: Foo default";
  typed_invalid "font-family: Arial, inherit";
  typed_invalid "font-family: revert-layer, serif";
  neg "font-family: system-ui revert-layer, serif";

  (* CSS-wide keywords mixing - should fail when mixed with other values *)
  neg "margin: inherit 10px";
  neg "padding: 10px inherit";
  neg "color: red inherit";
  neg "font-size: unset 12px";
  neg "opacity: revert 0.5";
  neg "z-index: revert-layer 10";

  (* CSS Backgrounds 3 (ED) sec. 5.4 gives [border-image-outset] a number or a
     length per side and no keyword, and sec. 5.3 lets [border-image-width] add
     [auto] and nothing else. The generic length reader takes a keyword list of
     its own, [stretch] included, which is a repeat keyword here. *)
  neg "border-image-outset: stretch";
  neg "border-image-outset: 0 stretch";
  neg "border-image-outset: auto";
  neg "border-image-width: stretch";
  neg "border-image-width: 1 min-content";

  (* CSS Multicol 2 sec. 4.2 gives [column-height] a length and no percentage,
     which Chrome 146 refuses, and sec. 4.4 gives [column-wrap] three
     keywords. *)
  neg "column-height: 50%";
  neg "column-height: nowrap";
  neg "column-height: min-content";
  neg "column-height: none";
  neg "column-height: normal";
  neg "column-wrap: sideways";
  neg "column-wrap: 10px";

  (* CSS Backgrounds 3 (ED) sec. 3.3: [<line-width>] is [<length [0,inf]> | thin
     | medium | thick] and takes no percentage, in math or out of it. Chrome 146
     refuses one. *)
  neg "border-width: 50%";
  neg "border-top-width: 50%";
  neg "outline-width: 200%";
  neg "column-rule-width: 10%";
  neg "-webkit-text-stroke-width: 20%";
  neg "border-width: calc(50%)";

  (* CSS Values 4 sec. 10.8 gives [<calc-value>] a number, a dimension, a
     percentage, a [<calc-keyword>] or a parenthesised [<calc-sum>], and sec.
     10.9 makes the calculation's type failure for anything else. A CSS-wide
     keyword, a sizing keyword or a sizing function is none of those, so a math
     operand takes none of them. Chrome 153 drops each of these declarations;
     unwrapping one to its bare keyword would make a live [width: inherit] out
     of a value the browser ignores. *)
  neg "width: calc(inherit)";
  neg "height: calc(auto)";
  neg "width: calc(initial)";
  neg "width: calc(unset)";
  neg "width: calc(revert)";
  neg "width: calc(1px + initial)";
  neg "width: calc(calc(auto))";
  neg "width: calc(fit-content(20rem))";
  neg "width: min(unset,1px)";
  neg "width: min(1px,auto)";
  neg "border-width: calc(medium)";
  neg "border-width: calc(thin)";
  neg "border-width: calc(thick)";
  neg "outline-width: calc(thin)";
  neg "outline-width: min(thin,1px)";
  (* The same rule over the properties that hand their own value reader to the
     calc leaf: the sizing keywords of [flex-basis] and [flex], the absolute
     sizes of [font-size], the [normal] of [line-height] and the CSS-wide
     keywords all five [<opacity-value>] properties take. *)
  neg "flex-basis: calc(auto)";
  neg "flex-basis: calc(content)";
  neg "flex-basis: calc(min-content)";
  neg "flex-basis: calc(1px + initial)";
  neg "flex: calc(auto)";
  neg "flex: calc(content)";
  neg "flex: calc(inherit)";
  neg "flex: calc(1px + initial)";
  neg "font-size: calc(inherit)";
  neg "font-size: calc(1px + initial)";
  neg "line-height: calc(normal)";
  neg "line-height: calc(inherit)";
  neg "line-height: calc(1px + initial)";
  neg "opacity: calc(inherit)";
  neg "fill-opacity: calc(inherit)";
  neg "flood-opacity: calc(inherit)";
  neg "stop-opacity: calc(inherit)";
  neg "stroke-opacity: calc(inherit)";
  neg "shape-image-threshold: calc(inherit)";
  (* A keyword operand is no more valid for being arithmetic: sec. 10.9 fails
     the whole calculation's type, not just the bare-keyword spelling. *)
  neg "font-size: calc(medium)";
  neg "font-size: calc(2 * medium)";
  neg "font-size: calc(medium * 2)";
  neg "font-size: calc(medium / 2)";
  neg "font-size: calc(medium + 0px)";
  neg "font-size: calc(medium + 1em)";
  neg "font-size: calc((medium) + (1px))";
  (* Each keyword still reads on its own, and a well-typed operand still folds:
     [line-height] takes a unitless [<number>], so [calc(1.5)] stays valid where
     [calc(normal)] does not. *)
  check ~expected:"flex-basis:auto" "flex-basis: auto";
  check ~expected:"flex-basis:content" "flex-basis: content";
  check ~expected:"flex-basis:min-content" "flex-basis: min-content";
  check ~expected:"font-size:medium" "font-size: medium";
  check ~expected:"font-size:inherit" "font-size: inherit";
  check ~expected:"line-height:normal" "line-height: normal";
  check ~expected:"line-height:1.5" "line-height: calc(1.5)";
  check ~expected:"flex-basis:calc(50% - 10px)" "flex-basis: calc(50% - 10px)";
  check ~expected:"opacity:inherit" "opacity: inherit";
  check ~expected:"opacity:.5" "opacity: 0.5";
  check ~expected:"fill-opacity:.5" "fill-opacity: 50%";

  (* CSS Values 4 sec. 10.2: the arguments of [min()] / [max()] / [clamp()]
     "must have a consistent type or else the function is invalid", and sec.
     10.9 gives a unitless zero inside a math function the [<number>] type. *)
  neg "width: min(0,1px)";
  neg "width: clamp(0px,0,100px)";
  (* [<line-width>] runs the same check: without it a mistyped argument folds to
     a wrong value rather than dropping, so [min(0,1px)] became [0]. The
     longhands, the shorthands that carry one and a nested math argument all
     read through the same leaf. *)
  neg "border-width: min(0,1px)";
  neg "border-width: max(0,1px)";
  neg "outline-width: clamp(0px,0,3px)";
  neg "column-rule-width: min(1,1px)";
  neg "column-rule-width: clamp(0px,1,100px)";
  neg "column-rule-width: calc(min(1,1px) + max(1px,2px))";
  neg "-webkit-text-stroke-width: min(1,1px)";
  neg "-webkit-text-stroke-width: clamp(0px,1,100px)";
  neg "outline: min(0,1px) solid red";
  neg "border-right: min(0,1px) solid red";
  neg "border-inline-end: min(0,1px) solid red";
  neg "outline: calc(min(0,1px) + max(1px,2px)) solid red";
  check ~expected:"border-width:min(1px,2em)" "border-width: min(1px,2em)";
  check ~expected:"column-rule-width:min(1px,2em)"
    "column-rule-width: min(1px,2em)";
  check ~expected:"-webkit-text-stroke-width:2px"
    "-webkit-text-stroke-width: 2px";
  check ~expected:"outline:min(1px,2em) solid red"
    "outline: min(1px,2em) solid red";
  check ~expected:"border-right:1px solid red" "border-right: 1px solid red";

  (* CSS Sizing 3 sec. 5 gives the intrinsic sizes to the sizing properties, so
     a property reading a plain length does not take them. Chrome 146 refuses
     each of these. *)
  neg "top: min-content";
  neg "top: normal";
  neg "margin-top: none";
  neg "inset: min-content";
  neg "column-width: min-content";
  neg "background-size: min-content";

  (* CSS Values 4 (ED) sec. 6.2 omits the unit only for a zero length, so a bare
     [0] is neither a time nor an angle. Cascade used to read one as [0s] or
     [0deg], turning input Chrome 146 refuses into a working declaration. *)
  neg "transition-duration: 0";
  neg "transition-delay: 0";
  neg "animation-duration: 0";
  neg "animation-delay: 0";
  neg "rotate: 0";
  neg "offset-rotate: 0";

  (* Each of these is a [<length>] in its own specification and takes no
     percentage: CSS Transforms 2 sec. 3 for the perspective, CSS Tables 3 sec.
     4.3.1 for the spacing, CSS Multicol 2 sec. 4.1 for the column width, CSS
     Backgrounds 3 sec. 5.4 for the outset and CSS Sizing 4 sec. 5 for the
     intrinsic size. Chrome 146 refuses each. *)
  neg "perspective: 50%";
  neg "border-spacing: 50%";
  neg "column-width: 50%";
  neg "border-image-outset: 50%";
  neg "contain-intrinsic-size: 50%";
  neg "contain-intrinsic-width: 200%";
  neg "tab-size: 50%";
  neg "overflow-clip-margin: 50%";
  (* CSS Overflow 4 sec. 3.2 is [<visual-box> || <length>], with no range on the
     length, so a negative one pulls the clip edge inside the box. *)
  check_declaration ~expected:"overflow-clip-margin:-1px"
    "overflow-clip-margin: -1px";
  neg "column-width: -5px"

let spec_property_grammar_table_expansion () =
  (* Cross-spec property grammar vectors. This grows toward an exhaustive table:
     each property area gets both accepted values and values that should be
     rejected by that property's grammar. *)
  let positive =
    [
      ("display", "inline flow-root");
      ("display", "list-item flow-root");
      ("position", "sticky");
      ("inset", "1px 2px 3px 4px");
      ("inset-inline", "auto 10%");
      ("box-sizing", "border-box");
      ("width", "fit-content(20rem)");
      ("min-width", "min-content");
      ("max-width", "stretch");
      ("aspect-ratio", "16/9");
      ("contain", "layout paint");
      ("container-type", "inline-size");
      ("container-name", "card");
      ("container", "card / inline-size");
      ("overflow", "clip");
      ("overflow-clip-margin", "content-box 1px");
      ("overscroll-behavior", "contain none");
      ("scroll-snap-type", "x mandatory");
      ("scrollbar-width", "thin");
      ("scrollbar-color", "red blue");
      ("margin", "anchor-size(width)");
      ("padding", "max(1rem, 2vw)");
      ("border", "1px solid currentColor");
      ("border-radius", "10px 20px / 30px 40px");
      ("border-image", "linear-gradient(red, blue) 30");
      ("background", "url(bg.png) no-repeat center / cover border-box");
      ("background-position", "left 10px top 20px");
      ("box-shadow", "0 1px 2px rgb(0 0 0 / .2)");
      ("clip-path", "inset(10px round 2px)");
      ("shape-outside", "circle(50%)");
      ("shape-margin", "1rem");
      ("color", "color(display-p3 1 0 0)");
      ("color", "light-dark(black, white)");
      ("accent-color", "auto");
      ("opacity", ".5");
      ("opacity", "2");
      ("mix-blend-mode", "multiply");
      ("filter", "blur(5px) contrast(120%)");
      ("font", "italic small-caps bold 16px/1.5 serif");
      ("font-size", "clamp(1rem, 2vw, 2rem)");
      ("font-weight", "650");
      ("font-weight", "1000");
      ("font-stretch", "75%");
      ("font-feature-settings", "\"kern\" 1");
      ("font-variation-settings", "\"wght\" 650");
      ("font-palette", "--brand");
      ("text-wrap-mode", "wrap");
      ("text-wrap-style", "balance");
      ("white-space", "preserve nowrap");
      ("line-height", "normal");
      ("word-break", "break-word");
      ("writing-mode", "vertical-rl");
      ("text-combine-upright", "digits 2");
      ("text-combine-upright", "digits 3");
      ("transform", "translateX(10px) rotate(45deg) scale(1.2)");
      ("translate", "10px 20px");
      ("rotate", "1 0 0 45deg");
      ("scale", "1.2 2");
      ("perspective-origin", "left 10px top 20px");
      ("transition", "opacity 1s ease-in .2s");
      ("transition-behavior", "allow-discrete");
      ("animation", "fade 1s linear 2 alternate both running");
      ("animation-timeline", "scroll()");
      ("animation-range", "entry 10% exit 90%");
      ("view-transition-name", "card");
      ("grid-template-columns", "subgrid");
      ("grid-template-rows", "masonry");
      ("grid-auto-flow", "row dense");
      ("gap", "1rem 2rem");
      ("flex", "1 1 0");
      ("flex-flow", "row wrap");
      ("place-content", "center space-between");
      ("place-items", "start stretch");
      ("place-self", "auto center");
      ("list-style", "square inside");
      ("content", "open-quote attr(title) close-quote");
      ("counter-reset", "section 1");
      ("counter-increment", "section");
      ("resize", "both");
      ("cursor", "url(cursor.cur), pointer");
      ("user-select", "none");
      ("appearance", "none");
      ("pointer-events", "auto");
      ("anchor-name", "--tooltip");
      ("position-anchor", "--tooltip");
      ("position-area", "top span-right");
      ("position-try-fallbacks", "--below, flip-block");
      ("object-fit", "cover");
      ("object-position", "left 10px top 20px");
      ("mask", "url(mask.svg) center / contain no-repeat");
      ("mask-type", "luminance");
    ]
  in
  List.iter
    (fun (property, value) ->
      let input = property ^ ":" ^ value in
      let expected, optimized =
        match (property, value) with
        | "container", "card / inline-size" ->
            (Some "container:card/inline-size", None)
        | "padding", "max(1rem, 2vw)" -> (Some "padding:max(1rem,2vw)", None)
        | "border-radius", "10px 20px / 30px 40px" ->
            (Some "border-radius:10px 20px/30px 40px", None)
        | "border-image", "linear-gradient(red, blue) 30" ->
            ( Some "border-image:linear-gradient(red,blue)30",
              Some "border-image:linear-gradient(red,blue)30" )
        | "background", "url(bg.png) no-repeat center / cover border-box" ->
            ( Some "background:url(bg.png)center/cover no-repeat border-box",
              Some "background:url(bg.png)50%/cover no-repeat border-box" )
        | "scrollbar-color", "red blue" ->
            (Some "scrollbar-color:red blue", Some "scrollbar-color:red #00f")
        | "border", "1px solid currentColor" ->
            (Some "border:1px solid currentColor", Some "border:1px solid")
        | "background-position", "left 10px top 20px" ->
            ( Some "background-position:left 10px top 20px",
              Some "background-position:10px 20px" )
        | "box-shadow", "0 1px 2px rgb(0 0 0 / .2)" ->
            ( Some "box-shadow:0 1px 2px rgb(0 0 0/.2)",
              Some "box-shadow:0 1px 2px #0003" )
        | "color", "light-dark(black, white)" ->
            ( Some "color:light-dark(black,white)",
              Some "color:light-dark(#000,#fff)" )
        | "filter", "blur(5px) contrast(120%)" ->
            (Some "filter:blur(5px)contrast(120%)", None)
        | "font-size", "clamp(1rem, 2vw, 2rem)" ->
            (Some "font-size:clamp(1rem,2vw,2rem)", None)
        | "transform", "translateX(10px) rotate(45deg) scale(1.2)" ->
            (Some "transform:translateX(10px)rotate(45deg)scale(1.2)", None)
        | "rotate", "1 0 0 45deg" -> (Some "rotate:x 45deg", None)
        | "font", "italic small-caps bold 16px/1.5 serif" ->
            ( Some "font:italic small-caps bold 16px/1.5 serif",
              Some "font:italic small-caps 700 16px/1.5 serif" )
        | "animation", "fade 1s linear 2 alternate both running" ->
            (Some "animation:fade 1s linear 2 alternate both", None)
        | "animation-range", "entry 10% exit 90%" ->
            (Some "animation-range:entry 10%exit 90%", None)
        | "display", "inline flow-root" ->
            (Some "display:inline flow-root", Some "display:inline-block")
        | "display", "list-item flow-root" ->
            (* CSS Display 3 (ED) sec. 2 combines the list-item components with
               [&&], so the two spellings are one value; sec. 2.3 leaves the
               [block] outside unwritten. *)
            (Some "display:flow-root list-item", None)
        | "position-try-fallbacks", "--below, flip-block" ->
            (Some "position-try-fallbacks:--below,flip-block", None)
        | "cursor", "url(cursor.cur), pointer" ->
            (Some "cursor:url(cursor.cur),pointer", None)
        | "mask", "url(mask.svg) center / contain no-repeat" ->
            (Some "mask:url(mask.svg)center/contain no-repeat", None)
        | _ -> (None, None)
      in
      check_declaration ?expected ?optimized input)
    positive;
  let negative =
    [
      ("display", "block inline flex");
      ("position", "sticky absolute");
      ("inset-inline", "1px 2px 3px");
      ("box-sizing", "padding-box");
      ("width", "-1px");
      ("min-width", "-1px");
      ("aspect-ratio", "16 /");
      ("contain", "none layout");
      ("container-type", "inline-size size");
      ("container", "/ inline-size");
      ("overflow", "visible clip scroll");
      ("overscroll-behavior", "contain none auto");
      ("scroll-snap-type", "mandatory x");
      ("scrollbar-width", "wide");
      ("scrollbar-color", "red");
      ("padding", "-1px");
      ("border", "solid solid");
      ("border-radius", "10px /");
      ("border-image", "none none");
      ("background-size", "cover contain");
      ("background-position", "left right");
      ("box-shadow", "inset inset 0 0 red");
      ("shape-margin", "-1px");
      ("color", "light-dark(black)");
      ("mix-blend-mode", "normal multiply");
      ("filter", "blur(1px, 2px)");
      ("font", "bold serif");
      ("font-stretch", "-10%");
      ("font-feature-settings", "\"kern\" maybe");
      ("font-variation-settings", "\"wght\"");
      ("font-palette", "1");
      ("text-wrap-style", "loud");
      ("white-space", "normal pre");
      ("line-height", "-1");
      ("writing-mode", "vertical");
      ("transform", "rotate()");
      ("translate", "10px 20px 30px 40px");
      ("rotate", "1 0 45deg");
      ("scale", "1 2 3 4");
      ("transition", "opacity ease ease");
      ("transition-behavior", "allow-discrete normal");
      ("animation", "1s 2s 3s");
      ("view-transition-name", "none card");
      ("grid-auto-flow", "dense dense");
      ("gap", "-1px");
      ("flex", "1 2 3 4");
      ("flex-flow", "row column");
      ("place-content", "left");
      ("content", "open-quote none");
      ("counter-reset", "none section");
      ("resize", "block inline both");
      ("cursor", "url(cursor.cur)");
      ("user-select", "all none");
      ("anchor-name", "tooltip");
      ("position-anchor", "tooltip");
      ("position-area", "top top");
      ("object-fit", "cover contain");
      ("mask-type", "alpha luminance");
    ]
  in
  List.iter
    (fun (property, value) ->
      neg_cursor read_declaration (property ^ ":" ^ value))
    negative

let edge_cases () =
  (* Empty/whitespace values where valid *)
  check_declaration ~expected:"content:\"\"" "content: \"\"";
  check_declaration ~expected:"content:\" \"" "content: \" \"";

  (* Complex calc expressions *)
  (* Cases with / operator - should be minified without spaces per CSS spec *)
  check_declaration ~expected:"width:calc(100% + 10px)"
    "width: -webkit-calc(100% + 10px)";
  check_declaration ~expected:"margin-left:calc(-1*5px)"
    ~optimized:"margin-left:-5px" "margin-left: -webkit-calc(-1 * 5px)";
  check_declaration ~expected:"width:calc((100% - 20px)/2)"
    "width: calc((100% - 20px) / 2)";
  check_declaration ~expected:"height:calc(100vh - calc(50px + 1em))"
    "height: calc(100vh - calc(50px + 1em))";

  (* Per CSS Values 4 section 10.10.1 the printer fully simplifies all-constant
     calcs and reduces multiplicative subexpressions against same-unit
     operands. *)
  check_declaration ~expected:"width:calc((10px + 5px)*2)"
    "width: calc((10px + 5px) * 2)";
  check_declaration ~expected:"height:calc(100% - 10px*2)"
    "height: calc(100% - 10px * 2)";
  check_declaration ~expected:"top:calc(50% - (20px + 10px)*1.5)"
    "top: calc(50% - (20px + 10px) * 1.5)";

  (* Very long values *)
  let long_shadow =
    "0 1px 2px rgba(0,0,0,0.1), 0 2px 4px rgba(0,0,0,0.1), "
    ^ "0 4px 8px rgba(0,0,0,0.1), 0 8px 16px rgba(0,0,0,0.1)"
  in
  check_declaration
    ~expected:
      "box-shadow:0 1px 2px rgb(0 0 0/.1),0 2px 4px rgb(0 0 0/.1),0 4px 8px \
       rgb(0 0 0/.1),0 8px 16px rgb(0 0 0/.1)"
    ~optimized:
      "box-shadow:0 1px 2px #0000001a,0 2px 4px #0000001a,0 4px 8px \
       #0000001a,0 8px 16px #0000001a"
    ("box-shadow: " ^ long_shadow)

let css_wide_keywords () =
  (* All properties accept CSS-wide keywords per spec *)
  check_declaration ~expected:"display:inherit" "display: inherit";
  check_declaration ~expected:"margin:unset" "margin: unset";
  check_declaration ~expected:"width:revert-layer" "width: revert-layer";
  check_declaration ~expected:"color:revert" "color: revert"

let spec_cascade7_defaulting () =
  (* CSS Cascade section 7: defaulting keywords are CSS-wide values. They are
     valid as the entire value of any property, including the [all] shorthand,
     and invalid when mixed with other component values. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_negative

let spec_cascade3_shorthands () =
  (* CSS Cascade section 3: shorthand declarations set all of their longhand
     sub-properties as if expanded in place. Omitted sub-properties are reset to
     their initial values unless the individual shorthand says otherwise. *)
  check_declaration ~expected:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:1em 2em" "padding: 1em 2em";
  check_declaration ~expected:"background:green" "background: green";
  check_declaration ~expected:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"font:bold 12pt/14pt Helvetica"
    ~optimized:"font:700 12pt/14pt Helvetica" "font: bold 12pt/14pt Helvetica";
  check_declaration ~expected:"margin:inherit" "margin: inherit";
  check_declaration ~expected:"padding:initial" "padding: initial";
  check_declaration ~expected:"background:unset" "background: unset";
  check_declaration ~expected:"border:revert" "border: revert";
  check_declaration ~expected:"font:revert-layer" "font: revert-layer";
  neg_cursor read_declaration "margin: inherit 1px";
  neg_cursor read_declaration "padding: 1px initial";
  neg_cursor read_declaration "background: green inherit";
  neg_cursor read_declaration "border: 1px solid revert";
  neg_cursor read_declaration "font: bold inherit 12pt Helvetica"

(* A parse error names the property the declaration wrote. [page-break-*]
   minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] property it aliases,
   which is a different property with its own values, so a diagnostic rendered
   under minify reports the failure against a property the author never wrote
   and cannot tell the two apart. *)
let error_names_the_property_written () =
  let message input =
    let r = Cursor.of_string input in
    match read_declaration r with
    | _ -> Alcotest.failf "%s: expected Parse_error but none was raised" input
    | exception Cursor.Parse_error e -> Error.to_string e
  in
  List.iter
    (fun (input, written, alias) ->
      let msg = message input in
      Alcotest.(check bool)
        (input ^ " is reported against " ^ written)
        true
        (Astring.String.is_infix ~affix:("bad value for " ^ written ^ ":") msg);
      Alcotest.(check bool)
        (input ^ " is not reported against " ^ alias)
        false
        (Astring.String.is_infix ~affix:("bad value for " ^ alias ^ ":") msg))
    [
      ("page-break-before: bogus", "page-break-before", "break-before");
      ("page-break-after: bogus", "page-break-after", "break-after");
      ("page-break-inside: bogus", "page-break-inside", "break-inside");
    ]

let spec_cascade3_aliasing () =
  (* CSS Cascade section 3.1: legacy shorthands behave as shorthands at parse
     time but are not selected for serialization. The spec example maps
     page-break-before: always to break-before: page. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.alias_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.alias_negative

let spec_break3_page_break_var () =
  (* CSS Fragmentation 3 sec. 3.4 aliases [page-break-*] to [break-*] through a
     value mapping table: [auto | left | right | avoid] map to themselves and
     [always] maps to [page]. The table is keyed on the value, so a [var()]
     declaration has no alias to take: substitution happens at computed-value
     time, and the one mapping that is not the identity has no [break-*]
     spelling to fall back on. The declaration names [page-break-*] and stays
     there, in both modes. *)
  List.iter
    (fun (input, pretty, minified) ->
      check_declaration ~minify:false ~expected:pretty input;
      check_declaration ~expected:minified input)
    [
      ( "page-break-before: var(--x)",
        "page-break-before: var(--x)",
        "page-break-before:var(--x)" );
      ( "page-break-after: var(--x)",
        "page-break-after: var(--x)",
        "page-break-after:var(--x)" );
      ( "page-break-inside: var(--x)",
        "page-break-inside: var(--x)",
        "page-break-inside:var(--x)" );
      (* The fallback shows what the alias cannot carry: an unset [--x] leaves
         [always], which no [break-*] property accepts. *)
      ( "page-break-after: var(--x, always)",
        "page-break-after: var(--x, always)",
        "page-break-after:var(--x,always)" );
      ( "page-break-inside: var(--x) !important",
        "page-break-inside: var(--x) !important",
        "page-break-inside:var(--x)!important" );
    ];
  (* A value the table does map still takes the alias, unmoved. *)
  List.iter
    (fun (input, pretty, minified) ->
      check_declaration ~minify:false ~expected:pretty input;
      check_declaration ~expected:minified input)
    [
      ( "page-break-before: always",
        "page-break-before: always",
        "break-before:page" );
      ( "page-break-after: always",
        "page-break-after: always",
        "break-after:page" );
      ("page-break-after: avoid", "page-break-after: avoid", "break-after:avoid");
      ("page-break-before: left", "page-break-before: left", "break-before:left");
      ( "page-break-inside: avoid",
        "page-break-inside: avoid",
        "break-inside:avoid" );
      ("page-break-inside: auto", "page-break-inside: auto", "break-inside:auto");
      (* CSS Cascade 5 sec. 7.3 gives every property the CSS-wide keywords, and
         the alias and its target take each one alike. *)
      ( "page-break-after: initial",
        "page-break-after: initial",
        "break-after:initial" );
      ( "page-break-before: unset",
        "page-break-before: unset",
        "break-before:unset" );
      ( "page-break-after: revert",
        "page-break-after: revert",
        "break-after:revert" );
      ( "page-break-inside: revert-layer",
        "page-break-inside: revert-layer",
        "break-inside:revert-layer" );
      ( "page-break-inside: inherit",
        "page-break-inside: inherit",
        "break-inside:inherit" );
    ];
  (* The [var()] declaration is the property it names, not an opaque one that
     happens to spell the same: it answers [same_property] against a keyword
     declaration of that property, the way every other property does. *)
  List.iter
    (fun (var_decl, keyword_decl) ->
      Alcotest.(check bool)
        (var_decl ^ " names the same property as " ^ keyword_decl)
        true
        (same_property
           (Css.Declaration.of_string var_decl)
           (Css.Declaration.of_string keyword_decl)))
    [
      ("page-break-before: var(--x)", "page-break-before: left");
      ("page-break-after: var(--x)", "page-break-after: always");
      ("page-break-inside: var(--x)", "page-break-inside: avoid");
      ("break-after: var(--x)", "break-after: page");
    ]

let spec_cascade3_all () =
  (* CSS Cascade section 3.2: [all] is a shorthand that accepts only CSS-wide
     keywords and resets all CSS properties except direction, unicode-bidi, and
     custom properties. The parser surface can verify the allowed value set. *)
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.serialization_row) ->
      if String.starts_with ~prefix:"all:" row.expected then
        check_declaration ~expected:row.expected row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_positive;
  List.iter
    (fun (row : Cascade_spec_inventory.Declaration_grammar.invalid_row) ->
      if String.starts_with ~prefix:"all:" row.input then
        neg_cursor read_declaration row.input)
    Cascade_spec_inventory.Declaration_grammar.css_wide_negative

let comments () =
  (* Comments around colon and inside values *)
  check_declaration ~expected:"color:red" "color/*c*/:/**/red";
  check_declaration ~expected:"width:calc(100% - 10px)"
    "width:/*x*/calc(100% - 10px)";
  check_declaration ~expected:"color:red!important" "color: red!/**/important"

let unit_case () =
  (* Units are ASCII case-insensitive per spec *)
  check_declaration ~expected:"width:10px" "width: 10PX";
  check_declaration ~expected:"margin:1em" "margin: 1EM"

let number_formats () =
  (* Leading dot numbers are valid; scientific notation is also valid per CSS
     spec *)
  check_declaration ~expected:"opacity:.5" "opacity: .5";
  (* Scientific notation IS valid in CSS per the spec *)
  check_declaration ~expected:"opacity:100" "opacity: 1e2"

let integer_precision () =
  check_declaration "order:9007199254740993";
  check_declaration "grid-template-columns:repeat(9007199254740993,1px)";
  List.iter
    (neg_cursor read_declaration)
    [
      "order:999999999999999999999999999999999999";
      "grid-template-columns:repeat(999999999999999999999999999999999999,1px)";
    ]

let unterminated () =
  (* CSS Syntax 5.3.7 / 4.3.5 auto-close unterminated strings, brackets and
     function calls at EOF. Assert the recovered declaration matches the shape
     an explicit closer would have produced -- the parser must not silently drop
     content. *)
  check_declaration ~expected:"content:\"abc\"" "content: \"abc";
  (* The cursor parser preserves the auto-closed inner parens. The stylesheet
     recovery path drops this incomplete declaration before normalization. *)
  check_declaration ~expected:"width:calc(100% - (10px))"
    "width: calc(100% - (10px)";
  (* Declaration-level recovery can accept the unterminated color function; the
     stylesheet parser drops it, so assert the optimize oracle directly through
     [Declaration.normalize]. *)
  let input = "color: rgb(0, 0, 0" in
  check_declaration ~expected:"color:rgb(0 0 0)" input;
  let c = Cursor.of_string input in
  let decl =
    match read_declaration c with
    | Some decl -> decl
    | None -> Alcotest.fail "expected declaration recovery"
  in
  Alcotest.(check string)
    "unterminated rgb declaration normalize" "color:#000"
    (decl |> Css.Declaration.normalize |> Css.Declaration.to_string ~minify:true);
  (* A missing semicolon between two declarations in a block remains a parse
     error. *)
  Css_test_helpers.neg_cursor Css.Declaration.read_block
    "{ color:red margin:10px; }"

let custom_property_values () =
  (* Balanced braces in custom property values *)
  check_specified_value "custom property block specified" "--x: { a: b; }"
    "{ a: b; }";
  check_declaration ~expected:"--x:{a:b;}" "--x: { a: b; }";
  (* Semicolons inside strings are fine *)
  check_declaration ~expected:"--x:\"a;b\"" "--x: \"a;b\"";
  (* var() usage in standard properties, with and without fallback *)
  check_declaration ~expected:"color:var(--c,red)" "color: var(--c, red)";
  check_declaration ~expected:"width:var(--w,10px)" "width: var(--w, 10px)";
  check_declaration ~expected:"margin:var(--m)" "margin: var(--m)";
  (* CSS Custom Properties: var() fallbacks are parsed as declaration-value
     token streams. They are not eagerly type-checked against the destination
     property grammar at parse time. *)
  check_declaration ~expected:"z-index:var(--spec-value,{color:red;})"
    "z-index: var(--spec-value, { color: red; })"

(* A typed custom property carries its layer as metadata; the layer does not
   select a different CSS value serialization. In particular, the historical
   [theme] layer and any caller-defined layer print the same font-family
   value. *)
let typed_custom_font_family_layer_printing () =
  let render layer =
    let declaration, _ =
      Css.var ~layer "font-body" Css.Font_family
        (Css.font_stack [ Css.Name "Inter"; Css.Sans_serif ])
    in
    Css.Declaration.to_string ~minify:true declaration
  in
  let theme = render "theme" in
  Alcotest.(check string)
    "typed font family" "--font-body:Inter,sans-serif" theme;
  Alcotest.(check string)
    "layer-independent serialization" theme (render "utilities")

let suite =
  ( "declaration_validity",
    [
      (* !important handling *)
      test_case "important" `Quick important;
      test_case "custom properties" `Quick custom_properties;
      test_case "opaque numeric tokens" `Quick opaque_numeric_tokens;
      test_case "math sign whitespace" `Quick math_sign_whitespace;
      test_case "typed slot fallback whitespace" `Quick
        typed_slot_fallback_whitespace;
      test_case "inserted token boundary" `Quick inserted_token_boundary;
      test_case "custom property values" `Quick custom_property_values;
      test_case "typed custom font family layers print alike" `Quick
        typed_custom_font_family_layer_printing;
      test_case "spec property grammar table expansion" `Quick
        spec_property_grammar_table_expansion;
      test_case "unterminated parsing" `Quick unterminated;
      test_case "invalid declarations" `Quick invalid;
      (* Spec details and edge cases *)
      test_case "CSS-wide keywords" `Quick css_wide_keywords;
      test_case "spec cascade 3 shorthand properties" `Quick
        spec_cascade3_shorthands;
      test_case "spec cascade 3.1 property aliasing" `Quick
        spec_cascade3_aliasing;
      test_case "spec break 3.4 page-break alias with a var" `Quick
        spec_break3_page_break_var;
      test_case "a parse error names the property written" `Quick
        error_names_the_property_written;
      test_case "spec cascade 3.2 all property" `Quick spec_cascade3_all;
      test_case "spec cascade 7 defaulting keywords" `Quick
        spec_cascade7_defaulting;
      test_case "comments handling" `Quick comments;
      test_case "unit case-insensitivity" `Quick unit_case;
      test_case "number formats" `Quick number_formats;
      test_case "integer precision" `Quick integer_precision;
      test_case "edge cases" `Quick edge_cases;
    ] )
