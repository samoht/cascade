(** Property tests: borders, outlines, interaction, scrolling and SVG painting.
*)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_border_style =
  check_value_cursor "border-style" read_border_style pp_border_style
    ~roundtrip:true

let check_border =
  check_value_cursor "border" read_border pp_border ~roundtrip:true

let check_border_shorthand =
  check_value_cursor "border_shorthand" read_border_shorthand
    pp_border_shorthand

let check_touch_action =
  check_value_cursor "touch-action" read_touch_action pp_touch_action

let check_scroll_behavior =
  check_value_cursor "scroll-behavior" read_scroll_behavior pp_scroll_behavior

let check_svg_paint = check_value_cursor "svg-paint" read_svg_paint pp_svg_paint
let check_fill_rule = check_value_cursor "fill-rule" read_fill_rule pp_fill_rule

let check_stroke_width =
  check_value_cursor "stroke-width" read_stroke_width pp_stroke_width

let check_stroke_linecap =
  check_value_cursor "stroke-linecap" read_stroke_linecap pp_stroke_linecap

let check_stroke_linejoin =
  check_value_cursor "stroke-linejoin" read_stroke_linejoin pp_stroke_linejoin

let check_stroke_miterlimit =
  check_value_cursor "stroke-miterlimit" read_stroke_miterlimit
    pp_stroke_miterlimit

let check_vector_effect_keyword =
  check_value_cursor "vector-effect-keyword" read_vector_effect_keyword
    pp_vector_effect_keyword

let check_vector_effect_space =
  check_value_cursor "vector-effect-space" read_vector_effect_space
    pp_vector_effect_space

let check_vector_effect =
  check_value_cursor "vector-effect" read_vector_effect pp_vector_effect

let check_paint_order_keyword =
  check_value_cursor "paint-order-keyword" read_paint_order_keyword
    pp_paint_order_keyword

let check_paint_order =
  check_value_cursor "paint-order" read_paint_order pp_paint_order

let check_dash_length =
  check_value_cursor "dash-length" read_dash_length pp_dash_length

let check_stroke_dashoffset =
  check_value_cursor "stroke-dashoffset" read_stroke_dashoffset
    pp_stroke_dashoffset

let check_stroke_dasharray =
  check_value_cursor "stroke-dasharray" read_stroke_dasharray
    pp_stroke_dasharray

let check_webkit_appearance =
  check_value_cursor "-webkit-appearance" read_webkit_appearance
    pp_webkit_appearance

let check_forced_color_adjust =
  check_value_cursor "forced-color-adjust" read_forced_color_adjust
    pp_forced_color_adjust

let check_outline_style =
  check_value_cursor "outline-style" read_outline_style pp_outline_style

let check_overscroll_behavior =
  check_value_cursor "overscroll-behavior" read_overscroll_behavior
    pp_overscroll_behavior

let check_border_image_repeat =
  check_value_cursor "border-image-repeat" read_border_image_repeat
    pp_border_image_repeat

let check_border_image_width =
  check_value_cursor "border-image-width" read_border_image_width
    pp_border_image_width

let check_border_image_outset =
  check_value_cursor "border-image-outset" read_border_image_outset
    pp_border_image_outset

let check_cursor = check_value_cursor "cursor" read_cursor pp_cursor

let check_border_width =
  check_value_cursor "border_width" read_border_width pp_border_width

let check_border_radius =
  check_value_cursor "border_radius" read_border_radius pp_border_radius

let check_outline = check_value_cursor "outline" read_outline pp_outline

let check_outline_shorthand =
  check_value_cursor "outline_shorthand" read_outline_shorthand
    pp_outline_shorthand

let check_color_scheme =
  check_value_cursor "color_scheme" read_color_scheme pp_color_scheme

let test_border_style () =
  check_border_style "none";
  check_border_style "solid";
  check_border_style "dashed";
  check_border_style "dotted";
  check_border_style "double";
  check_border_style "groove";
  check_border_style "ridge";
  check_border_style "inset";
  check_border_style "outset";
  check_border_style "hidden";
  neg_cursor read_border_style "invalid-style";
  (* multiple values *)
  neg_cursor ~allow_partial:true read_border_style "solid dashed";
  (* typo *)
  neg_cursor read_border_style "soild";
  (* width, not style *)
  neg_cursor read_border_style "1px"

let test_border () =
  (* Test individual components *)
  check_border "2px";
  check_border "solid";
  check_border "red";
  (* Test combinations *)
  check_border "1px solid";
  check_border "2px red";
  check_border "solid red";
  check_border "1px solid red";
  (* Test with different order - parser should normalize *)
  check_border ~expected:"2px solid red" "red solid 2px";
  check_border ~expected:"2px solid" "solid 2px";
  (* Test with zero width *)
  check_border "0 solid";
  check_border ~expected:"0 solid black" "0 solid black";
  decl_optimizes ~prop:"border" ~held:"0 solid black" ~into:"0 solid#000"
    "0 solid black";
  (* Test with inherit/initial *)
  check_border "inherit";
  check_border "initial";
  neg_cursor ~allow_partial:true read_border "invalid-border";
  (* multiple widths *)
  neg_cursor ~allow_partial:true read_border "1px 2px";
  (* duplicate style *)
  neg_cursor read_border "solid solid";
  (* multiple colors *)
  neg_cursor ~allow_partial:true read_border "red blue";
  (* too many values *)
  neg_cursor read_border "1px solid red blue"

let test_cursor () =
  check_cursor "pointer";
  check_cursor "default";
  check_cursor "text";
  check_cursor "wait";
  check_cursor "help";
  (* Spec-compliant url + hotspot + fallback keyword *)
  check_cursor ~expected:"url(./cursor.cur) 4 12,pointer"
    "url(./cursor.cur) 4 12, pointer";
  check_cursor ~expected:"url(a.cur),url(b.cur) 1 2,move"
    "url(\"a.cur\"), url(b.cur) 1 2, move";
  (* A quoted [url()] with the hotspot placed inside its own parens still
     parses, and prints with the hotspot moved after the call like every other
     form (CSS UI 4 sec. 5.4: [<url> [<x> <y>]?]). A third number past the
     hotspot has nowhere to go in that grammar; [Cursor.call] did not require
     the reader to consume its whole sub-cursor, so it was silently dropped
     instead of invalidating the declaration. *)
  check_cursor ~expected:"url(a.cur) 1 2,move" "url(\"a.cur\" 1 2), move";
  neg_cursor read_cursor "url(\"a.cur\" 1 2 3), pointer";
  neg_cursor read_cursor "invalid-cursor";
  neg_cursor read_cursor "url(cursor.cur)";
  (* missing fallback *)
  neg_cursor ~allow_partial:true read_cursor "pointer default";
  (* multiple keywords without url *)
  (* negative hotspot *)
  neg_cursor read_cursor "url(cursor.cur) -1 -1, pointer"

let test_border_width () =
  check_border_width "thin";
  check_border_width "medium";
  check_border_width "thick";
  check_border_width "2px";
  check_border_width ".0625rem";
  (* border-width takes a <length>, and CSS Values 4 sec. 6.1 and 6.2 give the
     viewport-percentage and container-query units the same standing there as
     [vh]. Every unit [length] names is a border width. *)
  check_border_width "3vi";
  check_border_width "3vb";
  check_border_width "3dvh";
  check_border_width "3svw";
  check_border_width "3lvmin";
  check_border_width "2cqw";
  check_border_width "2cqi";
  check_border_width "2cqmax";
  (* Every property whose width component is a border width reads those units:
     the four physical longhands, their logical counterparts, and the 1-4 value
     shorthand. *)
  decl_optimizes ~prop:"border-top-width" ~held:"3dvh" ~into:"3dvh" "3dvh";
  decl_optimizes ~prop:"border-right-width" ~held:"2cqi" ~into:"2cqi" "2cqi";
  decl_optimizes ~prop:"border-bottom-width" ~held:"3svw" ~into:"3svw" "3svw";
  decl_optimizes ~prop:"border-left-width" ~held:"3lvmin" ~into:"3lvmin"
    "3lvmin";
  decl_optimizes ~prop:"border-block-start-width" ~held:"3dvh" ~into:"3dvh"
    "3dvh";
  decl_optimizes ~prop:"border-inline-end-width" ~held:"2cqw" ~into:"2cqw"
    "2cqw";
  decl_optimizes ~prop:"border-width" ~held:"3dvh 2cqi" ~into:"3dvh 2cqi"
    "3dvh 2cqi";
  (* Two units with no fixed ratio leave both bounds standing: [dvh] is a
     fraction of the viewport and [px] is absolute (CSS Values 4 sec. 6.1 and
     6.2), so neither argument of the comparison is redundant. *)
  check_border_width "min(3dvh,4px)";
  check_border_width "max(3dvh,4px)";
  check_border_width "clamp(1dvh,3dvh,4px)";
  (* Sec. 10.2 spells a comparison's argument [<calc-sum>], and sec. 10.1 makes
     a lone [var()] one, so the operand needs no [calc()] around it: the value
     is printed back as it was written, which is what [width] over the same
     production already does. *)
  check_border_width "min(var(--x),1px)";
  check_border_width "max(var(--x),1px)";
  check_border_width "clamp(var(--x),2px,3px)";
  (* One unit does compare with itself, and a container-query length grows with
     its multiplier, so the smaller multiple is the minimum. The fold is a
     node-changing rewrite (two different ASTs would otherwise print the same
     text and hash differently), so it runs in optimize, not pp: fmt/minify
     alone hold the authored min()/max(), only minify+optimize collapses it. *)
  decl_optimizes ~prop:"border-width" ~held:"min(2cqi,3cqi)" ~into:"2cqi"
    "min(2cqi,3cqi)";
  decl_optimizes ~prop:"border-width" ~held:"max(3dvh,4dvh)" ~into:"4dvh"
    "max(3dvh,4dvh)";
  (* A bound nothing else measures stops only its own group: [1px] and [2px]
     stay on one scale (CSS Values 4 sec. 6.2), so they collapse. *)
  decl_optimizes ~prop:"border-width" ~held:"max(1px,3dvh,2px)"
    ~into:"max(2px,3dvh)" "max(1px,3dvh,2px)";
  (* A zero-valued border-width collapses the unit like any other zero length
     (border-width: 0px -> 0), matching width/outline-width. Holds for the
     longhand, the logical longhand, and the 1-4 value shorthand; non-zero
     values keep their unit. *)
  decl_optimizes ~prop:"border-width" ~held:"0px" ~into:"0" "0px";
  decl_optimizes ~prop:"border-right-width" ~held:"0px" ~into:"0" "0px";
  decl_optimizes ~prop:"border-inline-start-width" ~held:"0px" ~into:"0" "0px";
  decl_optimizes ~prop:"border-width" ~held:"0px 1px 0 2rem"
    ~into:"0 1px 0 2rem" "0px 1px 0 2rem";
  decl_optimizes ~prop:"border-width" ~held:"2px" ~into:"2px" "2px";
  (* Whitespace around an argument is not an argument, so the comparison still
     reads as two bounds. *)
  check_border_width ~expected:"max(3dvh,4px)" "max( 3dvh , 4px )";
  (* CSS Values 4 sec. 10.2 gives [min()] / [max()] a comma-separated list of
     [<calc-sum>] and [clamp()] exactly three arguments. An argument that is no
     [<calc-sum>], a second bound with no comma before it, and a fourth
     [clamp()] argument each make the function invalid, and CSS Syntax 3 sec.
     8.2 makes an invalid value invalidate the declaration. Reading arguments up
     to the first unreadable one and comparing those is a different function:
     [min(3px,red,1px)] answers [3px] where the bound that decides the minimum
     is [1px]. *)
  neg_cursor read_border_width "max(1px,red)";
  neg_cursor read_border_width "min(3px,red,1px)";
  neg_cursor read_border_width "max(1px 2px)";
  neg_cursor read_border_width "clamp(1px,2px,3px,4px)";
  neg_cursor read_border_width "invalid-width";
  neg_cursor read_border_width "-2px";
  (* negative width *)
  neg_cursor ~allow_partial:true read_border_width "thick thin";
  (* multiple values *)
  neg_cursor read_border_width "2";
  (* missing unit *)
  (* not a valid keyword *)
  neg_cursor read_border_width "heavy"

let test_border_radius () =
  check_border_radius "1px";
  check_border_radius "1px 2px";
  check_border_radius "1px 2px 3px 4px";
  check_border_radius ~expected:"1px/2px" "1px / 2px";
  check_border_radius ~expected:"1px 2px/3px 4px" "1px 2px / 3px 4px";
  neg_cursor read_border_radius "";
  neg_cursor read_border_radius "1px /";
  neg_cursor read_border_radius "-1px"

let test_border_shorthand () =
  check_border_shorthand "1px";
  check_border_shorthand "solid";
  check_border_shorthand "red";
  check_border_shorthand "1px solid";
  check_border_shorthand "1px solid red";
  check_border_shorthand ~expected:"solid red" "red solid";
  check_border_shorthand ~expected:"2px dashed blue" "blue 2px dashed";
  decl_optimizes ~prop:"border" ~held:"2px dashed blue" ~into:"2px dashed#00f"
    "blue 2px dashed";
  neg_cursor ~allow_partial:true read_border_shorthand "1px 2px"

let test_user_select () =
  check_user_select "none";
  check_user_select "auto";
  check_user_select "text";
  check_user_select "all";
  check_user_select "contain";
  neg_cursor read_user_select "invalid-select"

let test_pointer_events () =
  check_pointer_events "auto";
  check_pointer_events "none";
  check_pointer_events "visiblepainted";
  check_pointer_events "visiblefill";
  check_pointer_events "visiblestroke";
  check_pointer_events "visible";
  check_pointer_events "painted";
  check_pointer_events "fill";
  check_pointer_events "stroke";
  check_pointer_events "all";
  check_pointer_events "inherit";
  neg_cursor read_pointer_events "invalid-events"

let test_touch_action () =
  check_touch_action "auto";
  check_touch_action "none";
  check_touch_action "pan-x";
  check_touch_action "pan-y";
  check_touch_action "manipulation";
  check_touch_action "inherit";
  neg_cursor read_touch_action "invalid-action"

let test_resize () =
  check_resize "none";
  check_resize "both";
  check_resize "horizontal";
  check_resize "vertical";
  check_resize "block";
  check_resize "inline";
  check_resize "inherit";
  neg_cursor read_resize "invalid-resize"

let test_scroll_behavior () =
  check_scroll_behavior "auto";
  check_scroll_behavior "smooth";
  check_scroll_behavior "inherit";
  neg_cursor read_scroll_behavior "invalid-behavior"

let test_scroll_snap_align () =
  check_scroll_snap_align "none";
  check_scroll_snap_align "start";
  check_scroll_snap_align "end";
  check_scroll_snap_align "center";
  neg_cursor read_scroll_snap_align "invalid-align"

let test_scroll_snap_stop () =
  check_scroll_snap_stop "normal";
  check_scroll_snap_stop "always";
  check_scroll_snap_stop "inherit";
  neg_cursor read_scroll_snap_stop "invalid-stop"

let test_scroll_snap_axis () =
  check_scroll_snap_axis "x";
  check_scroll_snap_axis "y";
  check_scroll_snap_axis "inline";
  check_scroll_snap_axis "block";
  check_scroll_snap_axis "both";
  check_scroll_snap_axis "none";
  neg_cursor read_scroll_snap_axis "invalid-axis"

let test_scroll_snap_strictness () =
  check_scroll_snap_strictness "proximity";
  check_scroll_snap_strictness "mandatory";
  neg_cursor read_scroll_snap_strictness "invalid-strictness"

let test_scroll_snap_type () =
  check_scroll_snap_type "none";
  check_scroll_snap_type "inherit";
  check_scroll_snap_type "x mandatory";
  check_scroll_snap_type "y mandatory";
  check_scroll_snap_type "inline mandatory";
  check_scroll_snap_type "block mandatory";
  check_scroll_snap_type "both mandatory";
  check_scroll_snap_type "x proximity";
  check_scroll_snap_type "y proximity";
  check_scroll_snap_type "inline proximity";
  check_scroll_snap_type "block proximity";
  check_scroll_snap_type "both proximity";
  neg_cursor read_scroll_snap_type "invalid-type"

let test_overscroll_behavior () =
  check_overscroll_behavior "auto";
  check_overscroll_behavior "contain";
  check_overscroll_behavior "none";
  check_overscroll_behavior "inherit";
  neg_cursor read_overscroll_behavior "invalid-behavior"

let test_svg_paint () =
  check_svg_paint "none";
  check_svg_paint "currentcolor";
  check_svg_paint "red";
  check_svg_paint ~expected:"url(#grad)red" "url(#grad) red";
  neg_cursor read_svg_paint "invalid-paint"

let test_fill_rule () =
  check_fill_rule "nonzero";
  check_fill_rule "evenodd";
  check_fill_rule "inherit";
  check_fill_rule "var(--r)";
  neg_cursor read_fill_rule "even-odd";
  neg_cursor ~allow_partial:true read_fill_rule "nonzero evenodd"

(* SVG 2 sec. 13.5.3 gives [stroke-width] [<length-percentage> | <number>]: a
   bare number is a width in user units, not a CSS [<length>], and a negative
   value is invalid. *)
let test_stroke_width () =
  check_stroke_width "1.5";
  check_stroke_width "0";
  check_stroke_width "1.5px";
  check_stroke_width "50%";
  check_stroke_width "var(--w)";
  neg_cursor read_stroke_width "-1";
  neg_cursor read_stroke_width "-1px";
  neg_cursor read_stroke_width "thick"

let test_stroke_linecap () =
  check_stroke_linecap "butt";
  check_stroke_linecap "round";
  check_stroke_linecap "square";
  check_stroke_linecap "var(--c)";
  neg_cursor read_stroke_linecap "flat"

let test_stroke_linejoin () =
  check_stroke_linejoin "miter";
  check_stroke_linejoin "miter-clip";
  check_stroke_linejoin "round";
  check_stroke_linejoin "bevel";
  check_stroke_linejoin "arcs";
  neg_cursor read_stroke_linejoin "mitre"

let test_stroke_miterlimit () =
  check_stroke_miterlimit "1";
  check_stroke_miterlimit "4";
  check_stroke_miterlimit "10.5";
  check_stroke_miterlimit "var(--m)";
  (* SVG 2 sec. 13.5.5 makes only a negative miter limit illegal; the SVG 1.1
     "at least 1" rule went because CSS parsers never enforced it. *)
  check_stroke_miterlimit "0";
  check_stroke_miterlimit ~expected:".5" "0.5";
  neg_cursor read_stroke_miterlimit "-1";
  neg_cursor read_stroke_miterlimit "4px"

let test_vector_effect_keyword () =
  check_vector_effect_keyword "non-scaling-stroke";
  check_vector_effect_keyword "non-scaling-size";
  check_vector_effect_keyword "non-rotation";
  check_vector_effect_keyword "fixed-position";
  neg_cursor read_vector_effect_keyword "screen"

let test_vector_effect_space () =
  check_vector_effect_space "viewport";
  check_vector_effect_space "screen";
  neg_cursor read_vector_effect_space "non-scaling-stroke"

let test_vector_effect () =
  check_vector_effect "none";
  check_vector_effect "non-scaling-stroke";
  check_vector_effect "non-scaling-stroke screen";
  check_vector_effect "non-scaling-stroke fixed-position";
  check_vector_effect "var(--v)";
  neg_cursor read_vector_effect "bogus"

let test_paint_order_keyword () =
  check_paint_order_keyword "fill";
  check_paint_order_keyword "stroke";
  check_paint_order_keyword "markers";
  neg_cursor read_paint_order_keyword "normal"

let test_paint_order () =
  check_paint_order "normal";
  check_paint_order "stroke";
  check_paint_order "markers stroke";
  check_paint_order "var(--p)";
  (* [||] takes each operand at most once. *)
  neg_cursor read_paint_order "bogus"

let test_dash_length () =
  check_dash_length "4";
  check_dash_length "4px";
  check_dash_length "10%";
  neg_cursor read_dash_length "red"

let test_stroke_dashoffset () =
  check_stroke_dashoffset "0";
  check_stroke_dashoffset "4";
  check_stroke_dashoffset "4px";
  check_stroke_dashoffset "10%";
  check_stroke_dashoffset "var(--o)";
  check_stroke_dashoffset "-2px";
  (* CSS Values 4 sec. 10.1 puts a math function wherever its type is, and both
     halves of the dash grammar are a type. *)
  check_stroke_dashoffset "calc(1 + 2)";
  check_stroke_dashoffset "calc(1px + 2px)";
  neg_cursor read_stroke_dashoffset "none"

let test_stroke_dasharray () =
  check_stroke_dasharray "none";
  check_stroke_dasharray "4";
  check_stroke_dasharray "4 2";
  check_stroke_dasharray "4px 2px";
  check_stroke_dasharray "10% 5%";
  (* Comma and whitespace are the same separator here. *)
  check_stroke_dasharray ~expected:"4 2" "4, 2";
  check_stroke_dasharray "var(--d)";
  check_stroke_dasharray "calc(1 + 2) 4";
  check_stroke_dasharray "calc(1px + 2px) 4%";
  neg_cursor read_stroke_dasharray "red";
  (* SVG 2 sec. 13.3 gives each dash a non-negative value; only
     [stroke-dashoffset] takes a signed one. *)
  neg_cursor read_stroke_dasharray "-1";
  neg_cursor read_stroke_dasharray "-1px"

let test_webkit_appearance () =
  check_webkit_appearance "none";
  check_webkit_appearance "auto";
  check_webkit_appearance "button";
  check_webkit_appearance "textfield";
  (* The prefixed property is Chrome's alias for [appearance], so it takes the
     [base-select] of CSS Basic User Interface 4 sec. 5.1. *)
  check_webkit_appearance "base-select";
  check_webkit_appearance "inherit";
  neg_cursor read_webkit_appearance "invalid-appearance"

let test_forced_color_adjust () =
  check_forced_color_adjust "none";
  check_forced_color_adjust "auto";
  check_forced_color_adjust "inherit";
  neg_cursor read_forced_color_adjust "invalid-adjust"

let test_print_color_adjust () =
  check_print_color_adjust "economy";
  check_print_color_adjust "exact";
  neg_cursor read_print_color_adjust "invalid-adjust"

let test_appearance () =
  check_appearance "none";
  check_appearance "auto";
  check_appearance "button";
  check_appearance "textfield";
  check_appearance "menulist";
  check_appearance "inherit";
  neg_cursor read_appearance "invalid-appearance"

let test_border_image_repeat () =
  check_border_image_repeat "stretch";
  check_border_image_repeat "repeat";
  check_border_image_repeat "stretch repeat";
  check_border_image_repeat "inherit";
  neg_cursor read_border_image_repeat "blue"

let test_border_image_width () =
  check_border_image_width "1";
  check_border_image_width "10px";
  check_border_image_width "auto";
  check_border_image_width "1 2";
  (* CSS Backgrounds 3 sec. 5.3 writes the number half as <number [0,inf]>,
     which a calc() satisfies. *)
  check_border_image_width "calc(1 + 2)";
  check_border_image_width "calc(1 + 2) 3";
  check_border_image_width "inherit";
  neg_cursor read_border_image_width "blue"

let test_border_image_outset () =
  check_border_image_outset "1";
  check_border_image_outset "calc(1 + 2)";
  check_border_image_outset "10px";
  check_border_image_outset "1 2";
  check_border_image_outset "inherit";
  neg_cursor read_border_image_outset "blue"

let test_outline_style () =
  check_outline_style "none";
  check_outline_style "solid";
  check_outline_style "dashed";
  check_outline_style "dotted";
  check_outline_style "double";
  check_outline_style "groove";
  check_outline_style "ridge";
  check_outline_style "inset";
  check_outline_style "outset";
  check_outline_style "inherit";
  neg_cursor read_outline_style "invalid-style"

let test_color_scheme () =
  check_color_scheme "normal";
  check_color_scheme "light";
  check_color_scheme "dark";
  check_color_scheme "light dark";
  (* Color Adjust 1 SS 2.1: <custom-ident> is in the grammar for forward
     compatibility, so unknown idents are accepted. *)
  check_color_scheme "future-scheme";
  (* Color Adjust 1 SS 2.1: [only] is unordered relative to the scheme keywords,
     but the canonical serialization (CSSOM, browsers, Tailwind) puts the scheme
     first and [only] last. *)
  check_color_scheme "dark only";
  check_color_scheme "light only";
  check_color_scheme ~expected:"dark only" "only dark";
  check_color_scheme ~expected:"light only" "only light";
  check_color_scheme ~expected:"light dark only" "only light dark";
  neg_cursor read_color_scheme "normal light"

let test_outline () =
  check_outline "none";
  check_outline "inherit";
  check_outline "initial";
  check_outline "solid";
  check_outline "2px solid red";
  neg_cursor read_outline "invalid-outline-value"

let test_outline_shorthand () =
  check_outline_shorthand "solid";
  check_outline_shorthand "2px solid";
  check_outline_shorthand "2px solid red";
  neg_cursor read_outline_shorthand "invalid-outline-value"

let suite =
  ( "properties_ui",
    [
      test_case "border-style" `Quick test_border_style;
      test_case "border" `Quick test_border;
      test_case "scroll-behavior" `Quick test_scroll_behavior;
      test_case "scroll-snap-align" `Quick test_scroll_snap_align;
      test_case "scroll-snap-stop" `Quick test_scroll_snap_stop;
      test_case "scroll-snap-axis" `Quick test_scroll_snap_axis;
      test_case "scroll-snap-strictness" `Quick test_scroll_snap_strictness;
      test_case "scroll-snap-type" `Quick test_scroll_snap_type;
      test_case "border-image-repeat" `Quick test_border_image_repeat;
      test_case "border-image-width" `Quick test_border_image_width;
      test_case "border-image-outset" `Quick test_border_image_outset;
      test_case "overscroll-behavior" `Quick test_overscroll_behavior;
      test_case "outline" `Quick test_outline;
      test_case "outline_shorthand" `Quick test_outline_shorthand;
      test_case "cursor" `Quick test_cursor;
      test_case "border_width" `Quick test_border_width;
      test_case "border_radius" `Quick test_border_radius;
      test_case "border_shorthand" `Quick test_border_shorthand;
      test_case "user_select" `Quick test_user_select;
      test_case "pointer_events" `Quick test_pointer_events;
      test_case "touch_action" `Quick test_touch_action;
      test_case "resize" `Quick test_resize;
      test_case "scroll_behavior" `Quick test_scroll_behavior;
      test_case "scroll_snap_align" `Quick test_scroll_snap_align;
      test_case "scroll_snap_stop" `Quick test_scroll_snap_stop;
      test_case "scroll_snap_strictness" `Quick test_scroll_snap_strictness;
      test_case "scroll_snap_type" `Quick test_scroll_snap_type;
      test_case "overscroll_behavior" `Quick test_overscroll_behavior;
      test_case "svg_paint" `Quick test_svg_paint;
      test_case "fill_rule" `Quick test_fill_rule;
      test_case "stroke_width" `Quick test_stroke_width;
      test_case "stroke_linecap" `Quick test_stroke_linecap;
      test_case "stroke_linejoin" `Quick test_stroke_linejoin;
      test_case "stroke_miterlimit" `Quick test_stroke_miterlimit;
      test_case "vector_effect_keyword" `Quick test_vector_effect_keyword;
      test_case "vector_effect_space" `Quick test_vector_effect_space;
      test_case "vector_effect" `Quick test_vector_effect;
      test_case "paint_order_keyword" `Quick test_paint_order_keyword;
      test_case "paint_order" `Quick test_paint_order;
      test_case "dash_length" `Quick test_dash_length;
      test_case "stroke_dashoffset" `Quick test_stroke_dashoffset;
      test_case "stroke_dasharray" `Quick test_stroke_dasharray;
      test_case "webkit_appearance" `Quick test_webkit_appearance;
      test_case "forced_color_adjust" `Quick test_forced_color_adjust;
      test_case "print_color_adjust" `Quick test_print_color_adjust;
      test_case "appearance" `Quick test_appearance;
      test_case "outline_style" `Quick test_outline_style;
      test_case "color_scheme" `Quick test_color_scheme;
    ] )
