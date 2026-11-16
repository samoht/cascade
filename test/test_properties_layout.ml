(** Property tests: display, positioning, flex, grid, alignment, columns and
    fragmentation. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_display =
  check_value_cursor "display" read_display pp_display ~roundtrip:true

let check_z_index =
  check_value_cursor "z-index" read_z_index pp_z_index ~roundtrip:true

let check_flex_wrap = check_value_cursor "flex-wrap" read_flex_wrap pp_flex_wrap

let check_justify_self =
  check_value_cursor "justify_self" read_justify_self pp_justify_self

let check_align_content =
  check_value_cursor "align_content" read_align_content pp_align_content

let check_justify_items =
  check_value_cursor "justify_items" read_justify_items pp_justify_items

let check_flex_basis =
  check_value_cursor "flex_basis" read_flex_basis pp_flex_basis

let check_table_layout =
  check_value_cursor "table-layout" read_table_layout pp_table_layout

let check_border_collapse =
  check_value_cursor "border-collapse" read_border_collapse pp_border_collapse

let check_container_type =
  check_value_cursor "container-type" read_container_type pp_container_type

let check_contain = check_value_cursor "contain" read_contain pp_contain
let check_isolation = check_value_cursor "isolation" read_isolation pp_isolation

let check_webkit_box_orient =
  check_value_cursor "-webkit-box-orient" read_webkit_box_orient
    pp_webkit_box_orient

let check_moz_orient =
  check_value_cursor "-moz-orient" read_moz_orient pp_moz_orient

let check_aspect_ratio =
  check_value_cursor "aspect-ratio" read_aspect_ratio pp_aspect_ratio

let check_grid_auto_flow =
  check_value_cursor "grid-auto-flow" read_grid_auto_flow pp_grid_auto_flow

let check_grid_auto_flow_component =
  check_value_cursor "grid-auto-flow component" read_grid_auto_flow_component
    pp_grid_auto_flow_component

let check_grid_flex_math =
  check_value_cursor "grid-flex-math" read_grid_flex_math pp_grid_flex_math

let roundtrip_grid_auto_rows =
  check_value_cursor "grid-auto-rows" read_grid_auto_tracks pp_grid_template

let roundtrip_grid_auto_columns =
  check_value_cursor "grid-auto-columns" read_grid_auto_tracks pp_grid_template

let check_align_items =
  check_value_cursor "align-items" read_align_items pp_align_items

let check_justify_content =
  check_value_cursor "justify-content" read_justify_content pp_justify_content

let check_flex = check_value_cursor "flex" read_flex pp_flex

let check_column_width =
  check_value_cursor "column-width" read_column_width pp_column_width

let check_position_try =
  check_value_cursor "position-try" read_position_try pp_position_try

let check_grid_area = check_value_cursor "grid-area" read_grid_area pp_grid_area

let check_place_items =
  check_value_cursor "place-items" read_place_items pp_place_items

let check_place_content =
  check_value_cursor "place-content" read_place_content pp_place_content

let check_position_value =
  check_value_cursor "position_value" read_position_value pp_position_value

let check_gap = check_value_cursor "gap" read_gap pp_gap

let check_box_decoration_break =
  check_value_cursor "box_decoration_break" read_box_decoration_break
    pp_box_decoration_break

let check_break_value =
  check_value_cursor "break_value" read_break_value pp_break_value

let check_break_inside_value =
  check_value_cursor "break_inside_value" read_break_inside_value
    pp_break_inside_value

let check_page_break_value =
  check_value_cursor "page_break_value" read_page_break_value
    pp_page_break_value

let check_page_break_inside_value =
  check_value_cursor "page_break_inside_value" read_page_break_inside_value
    pp_page_break_inside_value

let check_page_size = check_value_cursor "page_size" read_page_size pp_page_size

let check_page_size_name =
  check_value_cursor "page_size_name" read_page_size_name pp_page_size_name

let check_page_size_orientation =
  check_value_cursor "page_size_orientation" read_page_size_orientation
    pp_page_size_orientation

let check_opacity = check_value_cursor "opacity" read_opacity pp_opacity
let check_order = check_value_cursor "order" read_order pp_order

let test_display () =
  check_display "none";
  check_display "block";
  check_display "inline";
  check_display "inline-block";
  check_display "flex";
  check_display "inline-flex";
  check_display "grid";
  check_display "inline-grid";
  (* CSS Grid 3 (ED, 2 September 2026) sec. 2.2 adds two values to [display]:
     "New values: grid-lanes | inline-grid-lanes". They establish grid lanes
     layout at block and inline level. *)
  check_display "grid-lanes";
  check_display "inline-grid-lanes";
  check_display "flow-root";
  check_display "table";
  check_display "table-row";
  check_display "table-cell";
  check_display "table-caption";
  check_display "table-column";
  check_display "table-column-group";
  check_display "table-footer-group";
  check_display "table-header-group";
  check_display "table-row-group";
  check_display "inline-table";
  check_display "list-item";
  (* CSS Display 3 (ED) sec. 2 defines [<display-listitem>] as
     [<display-outside>? && [ flow | flow-root ]? && list-item], all three
     order-free, so the inside keyword reads whether it comes before or after
     the [list-item]. sec. 2.3: "If no inner display type value is specified,
     the principal box's inner display type defaults to flow. If no outer
     display type value is specified, the principal box's outer display type
     defaults to block", so each default is left unwritten. *)
  check_display "flow-root list-item";
  check_display ~expected:"flow-root list-item" "list-item flow-root";
  check_display ~expected:"block list-item" "flow list-item";
  check_display ~expected:"block list-item" "list-item flow";
  check_display "inline flow-root list-item";
  check_display ~expected:"inline flow-root list-item"
    "list-item inline flow-root";
  (* [list-item] is not a [<display-outside>] and appears once in the
     production, so neither a second [list-item] nor an inside outside the [flow
     | flow-root] pair is a display value. *)
  neg_cursor ~allow_partial:true read_display "list-item list-item";
  neg_cursor ~allow_partial:true read_display "list-item table";
  check_display "contents";
  (* Intentional legacy: accepted for compatibility in some engines *)
  check_display "-webkit-box";
  (* CSS-wide keyword supported by this reader *)
  check_display "unset";
  neg_cursor read_display "invalid-display";
  (* sec. 2.2 states the two as whole [display] values and adds neither to
     [<display-inside>], so neither pairs with a [<display-outside>]. *)
  neg_cursor ~allow_partial:true read_display "block grid-lanes";
  neg_cursor ~allow_partial:true read_display "inline inline-grid-lanes";
  (* multiple values *)
  neg_cursor ~allow_partial:true read_display "block inline";
  neg_cursor read_display "flex-";
  neg_cursor read_display "";
  neg_cursor ~allow_partial:true read_display "123"

let test_position () =
  check_position "static";
  check_position "relative";
  check_position "absolute";
  check_position "fixed";
  check_position "sticky";
  check_position "-webkit-sticky";
  neg_cursor ~allow_partial:true read_position "invalid-position";
  (* multiple values *)
  neg_cursor ~allow_partial:true read_position "absolute relative";
  (* incomplete sticky *)
  neg_cursor read_position "stick";
  (* wrong form *)
  neg_cursor read_position "relatively"

let test_overflow () =
  check_overflow "visible";
  check_overflow "hidden";
  check_overflow "scroll";
  check_overflow "auto";
  check_overflow "clip";
  check_overflow "visible hidden";
  neg_cursor read_overflow "invalid-overflow";
  (* axis-specific not valid here *)
  neg_cursor read_overflow "scroll-x";
  (* not a valid overflow value *)
  neg_cursor read_overflow "none";
  (* overflow accepts at most two axes *)
  neg_cursor read_overflow "visible hidden scroll"

let test_zoom () =
  check_zoom "normal";
  check_zoom "reset";
  check_zoom "50%";
  check_zoom "1.5";
  neg_cursor read_zoom "not-a-zoom";
  (* CSS Viewport 1 sec. 3 spells the property [normal | reset | <number
     [0,inf]> | <percentage [0,inf]>]. *)
  neg_cursor read_zoom "-1";
  neg_cursor read_zoom "-10%"

let test_visibility () =
  check_visibility "visible";
  check_visibility "hidden";
  check_visibility "collapse";
  neg_cursor read_visibility "invalid-visibility";
  (* wrong keyword *)
  neg_cursor read_visibility "invisible";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_visibility "hidden visible";
  (* display value, not visibility *)
  neg_cursor read_visibility "none"

let test_z_index () =
  check_z_index "auto";
  check_z_index "10";
  check_z_index "-1";
  neg_cursor read_z_index "invalid";
  (* float not allowed *)
  neg_cursor read_z_index "1.5";
  (* no units allowed *)
  neg_cursor read_z_index "10px";
  (* duplicate *)
  neg_cursor ~allow_partial:true read_z_index "auto auto"

let test_flex_direction () =
  check_flex_direction "row";
  check_flex_direction "row-reverse";
  check_flex_direction "column";
  check_flex_direction "column-reverse";
  neg_cursor read_flex_direction "diagonal";
  (* multiple values *)
  neg_cursor ~allow_partial:true read_flex_direction "row column";
  (* incomplete *)
  neg_cursor read_flex_direction "reverse";
  neg_cursor read_flex_direction "column-";
  neg_cursor read_flex_direction "row-diagonal"

let test_flex_wrap () =
  check_flex_wrap "nowrap";
  check_flex_wrap "wrap";
  check_flex_wrap "wrap-reverse";
  neg_cursor read_flex_wrap "invalid-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_flex_wrap "wrap nowrap";
  (* doesn't exist *)
  neg_cursor read_flex_wrap "wrap-around";
  (* incomplete *)
  neg_cursor read_flex_wrap "reverse"

let test_align_self () =
  check_align_self "auto";
  check_align_self "flex-start";
  check_align_self "flex-end";
  check_align_self "center";
  check_align_self "baseline";
  check_align_self "stretch";
  neg_cursor read_align_self "invalid-align";
  neg_cursor read_align_self "left";
  (* not valid for align-self *)
  neg_cursor ~allow_partial:true read_align_self "flex-start flex-end";
  (* multiple *)
  check_align_self "start"

let test_table_layout () =
  check_table_layout "auto";
  check_table_layout "fixed";
  check_table_layout "inherit";
  neg_cursor read_table_layout "invalid-layout";
  neg_cursor ~allow_partial:true read_table_layout "auto fixed";
  (* both values *)
  neg_cursor read_table_layout "static";
  (* position value *)
  (* not a valid value *)
  neg_cursor read_table_layout "flexible"

let test_border_collapse () =
  check_border_collapse "collapse";
  check_border_collapse "separate";
  check_border_collapse "inherit";
  neg_cursor read_border_collapse "invalid-collapse";
  neg_cursor ~allow_partial:true read_border_collapse "collapse separate";
  (* both values *)
  neg_cursor read_border_collapse "collapsed";
  (* wrong form *)
  neg_cursor read_border_collapse "none"

let test_gap () =
  check_gap "10px";
  check_gap "1rem 2rem";
  check_gap "0px";
  decl_optimizes ~prop:"gap" ~held:"0px" ~into:"0" "0px";
  neg_cursor read_gap "invalid-gap";
  neg_cursor read_gap "-10px";
  (* negative gap *)
  neg_cursor ~allow_partial:true read_gap "10px 20px 30px";
  (* too many values *)
  (* not a valid gap value *)
  neg_cursor read_gap "auto"

let test_grid_auto_flow () =
  check_grid_auto_flow "row";
  check_grid_auto_flow "column";
  check_grid_auto_flow "dense";
  neg_cursor read_grid_auto_flow "invalid-flow";
  neg_cursor read_grid_auto_flow "row column";
  (* contradictory *)
  neg_cursor read_grid_auto_flow "sparse";
  (* opposite of dense, not valid *)
  (* not a valid value *)
  neg_cursor read_grid_auto_flow "horizontal"

let test_grid_auto_flow_component () =
  check_grid_auto_flow_component "row";
  check_grid_auto_flow_component "column";
  check_grid_auto_flow_component "dense";
  check_grid_auto_flow_component "var(--flow,dense)";
  neg_cursor read_grid_auto_flow_component "auto-flow";
  neg_cursor read_grid_auto_flow_component "sparse"

let test_justify_self () =
  check_justify_self "auto";
  check_justify_self "normal";
  check_justify_self "stretch";
  check_justify_self "center";
  check_justify_self "start";
  check_justify_self "end";
  check_justify_self "flex-start";
  check_justify_self "flex-end";
  check_justify_self "self-start";
  check_justify_self "self-end";
  check_justify_self "left";
  check_justify_self "right";
  check_justify_self "baseline";
  check_justify_self "first baseline";
  check_justify_self "last baseline";
  check_justify_self "unsafe center";
  check_justify_self "unsafe start";
  (* Spec: safe alignment modifier *)
  check_justify_self "safe center";
  neg_cursor read_justify_self "invalid";
  neg_cursor read_justify_self "safe safe"

let test_align_content () =
  check_align_content "normal";
  check_align_content "baseline";
  check_align_content "first baseline";
  check_align_content "last baseline";
  check_align_content "center";
  check_align_content "start";
  check_align_content "end";
  check_align_content "flex-start";
  check_align_content "flex-end";
  check_align_content "unsafe center";
  check_align_content "safe center";
  check_align_content "space-between";
  check_align_content "space-around";
  check_align_content "space-evenly";
  check_align_content "stretch";
  neg_cursor read_align_content "invalid"

let test_justify_items () =
  check_justify_items "normal";
  check_justify_items "stretch";
  check_justify_items "center";
  check_justify_items "start";
  check_justify_items "end";
  check_justify_items "flex-start";
  check_justify_items "flex-end";
  check_justify_items "self-start";
  check_justify_items "self-end";
  check_justify_items "left";
  check_justify_items "right";
  check_justify_items "baseline";
  check_justify_items "first baseline";
  check_justify_items "last baseline";
  check_justify_items "unsafe center";
  check_justify_items "safe end";
  neg_cursor read_justify_items "invalid-justify";
  neg_cursor ~allow_partial:true read_justify_items "left right";
  (* contradictory *)
  neg_cursor read_justify_items "unsafe unsafe";
  (* duplicate modifier *)
  (* not a valid value *)
  neg_cursor read_justify_items "middle"

let test_flex_basis () =
  check_flex_basis "auto";
  check_flex_basis "content";
  check_flex_basis "0";
  (* pp holds 0px; the zero-length strip is optimize. 0% stays (flex-basis 0% is
     not the length 0). *)
  check_flex_basis "0px";
  decl_optimizes ~prop:"flex-basis" ~held:"0px" ~into:"0" "0px";
  check_flex_basis "0%";
  check_flex_basis "100px";
  check_flex_basis "50%";
  check_flex_basis "inherit";
  (* flex-basis is a <length-percentage>, so it takes var() and calc(); a calc()
     that cannot resolve to a constant is preserved. *)
  check_flex_basis "var(--b)";
  check_flex_basis "calc(100% - 10px)";
  check_flex_basis "calc(100% - var(--x))";
  neg_cursor read_flex_basis "invalid";
  neg_cursor read_flex_basis "-100px"

let test_position_value () =
  check_position_value "center";
  check_position_value "left top";
  check_position_value "top 20px left 10px";
  check_position_value ~roundtrip:true ~expected:"50%25%" "50% 25%";
  check_position_value "inherit";
  neg_cursor ~allow_partial:true read_position_value "invalid-position";
  neg_cursor read_position_value "foo 1px bar 2px";
  neg_cursor ~allow_partial:true read_position_value "left 1px right 2px";
  neg_cursor ~allow_partial:true read_position_value "left 1px middle";
  neg_cursor ~allow_partial:true read_position_value "top 1px bottom";
  neg_cursor ~allow_partial:true read_position_value "left 1px top";
  neg_cursor ~allow_partial:true read_position_value "left top 1px"

let test_box_sizing () =
  check_box_sizing "border-box";
  check_box_sizing "content-box";
  check_box_sizing "inherit";
  neg_cursor read_box_sizing "invalid-sizing"

let test_content_visibility () =
  check_content_visibility "visible";
  check_content_visibility "auto";
  check_content_visibility "hidden";
  check_content_visibility "inherit";
  neg_cursor read_content_visibility "invalid-visibility"

let test_container_type () =
  check_container_type "normal";
  check_container_type "inline-size";
  check_container_type "size";
  neg_cursor read_container_type "invalid-type"

let test_container_shorthand () =
  check_container_shorthand "normal";
  check_container_shorthand "inline-size";
  check_container_shorthand "size";
  check_container_shorthand "sidebar";
  check_container_shorthand "sidebar / inline-size"
    ~expected:"sidebar/inline-size";
  check_container_shorthand "header / size" ~expected:"header/size";
  neg_cursor read_container_shorthand "/ size";
  (* Missing name before / *)
  neg_cursor read_container_shorthand "sidebar / invalid"
(* Invalid type *)

let test_contain () =
  check_contain "none";
  check_contain "strict";
  check_contain "content";
  check_contain "size";
  check_contain "layout";
  check_contain "style";
  check_contain "paint";
  (* Spec: combinations are allowed; test canonical combos *)
  check_contain "size layout style paint";
  check_contain "layout paint";
  check_contain "size style";
  neg_cursor read_contain "invalid-contain"

let test_isolation () =
  check_isolation "auto";
  check_isolation "isolate";
  check_isolation "inherit";
  neg_cursor read_isolation "invalid-isolation"

let test_webkit_box_orient () =
  check_webkit_box_orient "horizontal";
  check_webkit_box_orient "vertical";
  check_webkit_box_orient "inherit";
  neg_cursor read_webkit_box_orient "invalid-orient"

let test_moz_orient () =
  check_moz_orient "inline";
  check_moz_orient "block";
  check_moz_orient "horizontal";
  check_moz_orient "vertical";
  check_moz_orient "inherit";
  neg_cursor read_moz_orient "invalid-orient"

let test_clear () =
  check_clear "none";
  check_clear "left";
  check_clear "right";
  check_clear "both";
  neg_cursor read_clear "invalid-clear"

let test_float_side () =
  check_float_side "none";
  check_float_side "left";
  check_float_side "right";
  check_float_side "inline-start";
  check_float_side "inline-end";
  check_float_side "inherit";
  neg_cursor read_float_side "invalid-float"

let test_align_items () =
  check_align_items "stretch";
  check_align_items "flex-start";
  check_align_items "flex-end";
  check_align_items "center";
  check_align_items "baseline";
  neg_cursor read_align_items "invalid-align";
  neg_cursor read_align_items "diagonal"

let test_aspect_ratio () =
  check_aspect_ratio "auto";
  check_aspect_ratio "16/9";
  check_aspect_ratio "1.5";
  check_aspect_ratio "1";
  check_aspect_ratio "inherit";
  neg_cursor read_aspect_ratio "invalid-ratio";
  (* CSS Sizing 4 sec. 5 takes a [<ratio>], whose CSS Values 4 sec. 6.5 numbers
     carry a [0,inf] range. *)
  neg_cursor read_aspect_ratio "-1"

let test_flex () =
  check_flex "1";
  check_flex ~expected:"1 1 0" "1 1 0";
  (* pp holds the 0px basis; optimize strips the unit to 0 (without collapsing 1
     1 0 -> 1, since flex-basis 0 is not the default 0%). *)
  check_flex ~expected:"1 1 0px" "1 1 0px";
  decl_optimizes ~prop:"flex" ~held:"1 1 0px" ~into:"1 1 0" "1 1 0px";
  check_flex ~expected:"1 auto" "1 1 auto";
  check_flex "none";
  check_flex "auto";
  check_flex "inherit";
  (* var() is opaque, and a calc() in the grow position is held unfolded by the
     reader and serialized lexically by pp - no evaluation at parse. Folding the
     constant calc to 3 is an optimize+minify transform, asserted there. *)
  check_flex "var(--f)";
  check_flex "calc(1 + 2) 1 0";
  (* CSS Flexbox 1 sec. 7.1.1 joins the factor group and the basis with a [||],
     so the basis may lead; an omitted shrink is 1. Chrome 153 computes each of
     these to the same three longhands as the factor-first spelling. *)
  check_flex ~expected:"1 50%" "50% 1";
  check_flex ~expected:"1 2 50%" "50% 1 2";
  check_flex ~expected:"2 10px" "10px 2";
  check_flex ~expected:"1 auto" "auto 1";
  check_flex ~expected:"1 content" "content 1";
  (* The basis sits on one side of the [||] or the other, never between the two
     factors, and [none] is the whole value on its own. *)
  neg_cursor ~allow_partial:true read_flex "1 50% 2";
  neg_cursor ~allow_partial:true read_flex "50% 1 2 3";
  neg_cursor ~allow_partial:true read_flex "none 1";
  neg_cursor read_flex "invalid-flex"

let test_column_width () =
  check_column_width "auto";
  check_column_width "200px";
  check_column_width "inherit";
  neg_cursor read_column_width "small-caps"

let test_column_count () =
  check_column_count "auto";
  check_column_count "3";
  check_column_count "inherit";
  neg_cursor read_column_count "small-caps"

let test_position_try () =
  check_position_try "inherit";
  check_position_try "--foo";
  check_position_try "most-width --bar";
  (* Sec. 6.1 lets a fallback entry be a [<position-area>], and sec. 6.3 makes
     the fallbacks the required half of the shorthand. *)
  check_position_try "center";
  check_position_try "most-width none";
  check_position_try ~expected:"--a,center" "--a, center";
  neg_cursor ~allow_partial:true read_position_try "normal";
  neg_cursor ~allow_partial:true read_position_try "most-width";
  neg_cursor ~allow_partial:true read_position_try "123"

let test_grid_area () =
  check_grid_area "1/2/3/4";
  check_grid_area ~expected:"1/2/3/4" "1 / 2 / 3 / 4";
  check_grid_area "auto";
  check_grid_area "span 2";
  neg_cursor read_grid_area "1/2/3/4/5";
  (* CSS Grid 2 (ED) sec. 8.4 composes four [<grid-line>]s, so every slot
     carries the sec. 8.3 [<custom-ident>] exclusions. *)
  check_grid_area ~expected:"a/b/c/d" "a / b / c / d";
  neg_cursor ~allow_partial:true read_grid_area "3 auto/1/2/3";
  neg_cursor ~allow_partial:true read_grid_area "1/2/3/span default"

let test_grid_line () =
  check_grid_line "auto";
  check_grid_line "1";
  check_grid_line "span 2";
  check_grid_line "main-start";
  check_grid_line "content-end";
  check_grid_line "inherit";
  neg_cursor read_grid_line "span";
  (* The ordinary integer branch in sec. 8.3 excludes zero, with or without a
     line name. Negative indexes remain valid. *)
  neg_cursor read_grid_line "0";
  neg_cursor read_grid_line "0 foo";
  (* CSS Grid 2 (ED) sec. 8.3 restricts the integer in a span to [1,inf].
     Negative integers and zero are invalid in either operand order. *)
  neg_cursor read_grid_line "span 0";
  neg_cursor read_grid_line "0 span";
  neg_cursor read_grid_line "span -1";
  neg_cursor read_grid_line "-1 span";
  (* CSS Grid 2 (ED) sec. 8.3 gives the span branch as [span && [ <integer
     [1,inf]> || <custom-ident> ]]. CSS Values 4 (ED) sec. 2.2 makes both [&&]
     and [||] reorderable, so [span] sits on either side of the group and the
     group's own two parts come in either order. Sec. 8.3 sets "Canonical order:
     per grammar", which fixes the serialisation at [span <integer>
     <custom-ident>]. *)
  check_grid_line "span foo";
  check_grid_line "span 3 foo";
  check_grid_line ~expected:"span 3 foo" "span foo 3";
  check_grid_line ~expected:"span 3" "3 span";
  check_grid_line ~expected:"span foo" "foo span";
  check_grid_line ~expected:"span 3 foo" "3 foo span";
  check_grid_line ~expected:"span 3 foo" "foo 3 span";
  (* [span] is a keyword, so it matches in any ASCII case (sec. 4.1). *)
  check_grid_line ~expected:"span 3" "3 SPAN";
  (* Sec. 2.2 reorders only components in the same grouping, so [span] cannot
     sit between the two parts of the bracketed [||] group, and only one [span]
     is part of the value. *)
  neg_cursor ~allow_partial:true read_grid_line "3 span foo";
  neg_cursor ~allow_partial:true read_grid_line "foo span 3";
  neg_cursor ~allow_partial:true read_grid_line "span 3 span";
  neg_cursor read_grid_line "span span";
  (* CSS Grid 2 (ED) sec. 8.3: "In all the above productions, the
     [<custom-ident>] additionally excludes the keywords span and auto." CSS
     Values 4 (ED) sec. 4.2 excludes the CSS-wide keywords and the reserved
     [default] from every [<custom-ident>], in all ASCII case permutations. *)
  neg_cursor ~allow_partial:true read_grid_line "3 auto";
  neg_cursor ~allow_partial:true read_grid_line "3 AUTO";
  neg_cursor read_grid_line "span auto";
  neg_cursor ~allow_partial:true read_grid_line "span 2 auto";
  neg_cursor ~allow_partial:true read_grid_line "auto span";
  neg_cursor ~allow_partial:true read_grid_line "3 span auto";
  neg_cursor ~allow_partial:true read_grid_line "auto span 3";
  neg_cursor ~allow_partial:true read_grid_line "3 auto span";
  neg_cursor read_grid_line "default";
  neg_cursor read_grid_line "span default";
  neg_cursor read_grid_line "default span";
  neg_cursor ~allow_partial:true read_grid_line "3 default span";
  neg_cursor read_grid_line "initial span";
  neg_cursor ~allow_partial:true read_grid_line "3 default";
  neg_cursor ~allow_partial:true read_grid_line "3 initial";
  neg_cursor ~allow_partial:true read_grid_line "3 inherit";
  neg_cursor ~allow_partial:true read_grid_line "3 unset";
  neg_cursor ~allow_partial:true read_grid_line "3 revert";
  neg_cursor ~allow_partial:true read_grid_line "3 revert-layer";
  neg_cursor ~allow_partial:true read_grid_line "3 revert-rule";
  (* CSS Cascade 5 (ED) sec. 7.3: explicit defaulting takes the whole
     declaration, so a CSS-wide keyword is a [<grid-line>] on its own only. *)
  check_grid_line "initial";
  check_grid_line "revert-rule";
  neg_cursor read_grid_line "initial 3";
  (* [<grid-line>] has no [none] and no [dense] keyword, so neither is excluded
     from its [<custom-ident>]. *)
  check_grid_line "none";
  check_grid_line "3 none"

(* CSS Grid 2 (ED) sec. 8.4: grid-row / grid-column pair the same [<grid-line>]
   twice, so both slots carry the sec. 8.3 [<custom-ident>] exclusions. *)
let test_grid_line_pair () =
  check_grid_line_pair ~expected:"span foo/4" "span foo / 4";
  check_grid_line_pair ~expected:"span 3/4" "3 span / 4";
  check_grid_line_pair ~expected:"span foo/span 2 bar" "foo span / span 2 bar";
  neg_cursor ~allow_partial:true read_grid_line_pair "3 auto / 4";
  neg_cursor ~allow_partial:true read_grid_line_pair "3 span auto / 4";
  neg_cursor read_grid_line_pair "span auto / 2"

let test_grid_template () =
  check_grid_template "none";
  check_grid_template "auto";
  check_grid_template "10px";
  check_grid_template "100px 200px";
  check_grid_template "1fr 2fr";
  check_grid_template "auto auto";
  check_grid_template "inherit";
  (* CSS Grid 2 sec. 7.2 spells a [<track-breadth>] as [<length-percentage
     [0,inf]> | <flex [0,inf]> | min-content | max-content | auto], so a
     negative length is no breadth, inside [minmax()] either. *)
  neg_cursor read_grid_template "-2px";
  neg_cursor read_grid_template "minmax(-1px, 1fr)";
  (* CSS Grid 2 sec. 7.2: a track is any <length-percentage>, so a calc(), a
     var() inside a calc(), or a less common unit is a valid track, carried by
     the [Length] track. *)
  check_grid_template ~roundtrip:true ~expected:"calc(var(--spacing)*4)"
    "calc(var(--spacing) * 4)";
  check_grid_template ~roundtrip:true ~expected:"calc(var(--x)*2)1fr"
    "calc(var(--x) * 2) 1fr";
  check_grid_template "calc(100px + 1rem)";
  (* CSS Values 4 sec. 10.8 lets a math function resolve to <flex>, and CSS Grid
     2 sec. 7.2.1 accepts that type as a track breadth. Keep the function typed
     as a flex track instead of routing it through the length reader. *)
  check_grid_template ~roundtrip:true ~expected:"calc(1fr*2)" "calc(1fr * 2)";
  check_grid_template ~roundtrip:true ~expected:"min(1fr,2fr)" "min(1fr, 2fr)";
  check_grid_template ~roundtrip:true "clamp(100px,1fr,300px)";
  check_grid_template ~roundtrip:true ~expected:"min(100px,200px)"
    "min(100px, 200px)";
  check_grid_template ~roundtrip:true ~expected:"repeat(2,calc(1fr*2))"
    "repeat(2, calc(1fr * 2))";
  check_grid_template ~roundtrip:true ~expected:"calc(1fr*2)/min(1fr,2fr)"
    "calc(1fr * 2) / min(1fr, 2fr)";
  roundtrip_grid_auto_rows ~roundtrip:true ~expected:"calc(1fr*2)"
    "calc(1fr * 2)";
  roundtrip_grid_auto_columns ~roundtrip:true ~expected:"min(1fr,2fr)"
    "min(1fr, 2fr)";
  check_grid_template "10cm";
  check_grid_template ~expected:"minmax(calc(var(--x)*2),1fr)"
    "minmax(calc(var(--x) * 2), 1fr)";
  check_grid_template "repeat(2,1fr)";
  check_grid_template "repeat( 2 , 1fr )" ~expected:"repeat(2,1fr)";
  neg_cursor read_grid_template "invalid-template";
  (* CSS Grid 1 sec. 7.2.3 gives [repeat()] a count and a track list; trailing
     junk after the last track is invalid (CSS Syntax 3 sec. 8.2). [Cursor.call]
     did not require the reader to consume its whole sub-cursor, so it read up
     to the first unreadable track and answered with a truncated [repeat()]
     instead. *)
  neg_cursor read_grid_template "repeat(2,1fr red)";
  neg_cursor read_grid_template "repeat(2,1fr,red)";
  (* Same [Cursor.call] gap in the two comparison functions #627 did not reach:
     CSS Grid 1 sec. 7.2.3 gives [minmax()] exactly two [<track-breadth>] and
     [fit-content()] exactly one [<length-percentage>]; a trailing argument is
     invalid (CSS Syntax 3 sec. 8.2). *)
  check_grid_template "fit-content(10px)";
  check_grid_template "fit-content( 10px )" ~expected:"fit-content(10px)";
  neg_cursor read_grid_template "minmax(1px,2px,3px)";
  neg_cursor read_grid_template "fit-content(10px 20px)";
  (* CSS Grid 2 (ED) sec. 7.2.2: [<line-names> = '[' <custom-ident>* ']'], and
     "A line name cannot be span or auto, i.e. the [<custom-ident>] in the
     [<line-names>] production excludes the keywords span and auto." CSS Values
     4 (ED) sec. 4.2 excludes the CSS-wide keywords and the reserved [default]
     from every [<custom-ident>]. Each exclusion is on a keyword, so it holds in
     all ASCII case permutations. *)
  check_grid_template ~expected:"[a]1px" "[a] 1px";
  check_grid_template ~expected:"[a b]1px" "[a b] 1px";
  check_grid_template ~expected:"1px[a]" "1px [a]";
  neg_cursor read_grid_template "[span] 1px";
  neg_cursor read_grid_template "[SPAN] 1px";
  neg_cursor read_grid_template "[auto] 1px";
  neg_cursor read_grid_template "[Auto] 1px";
  neg_cursor read_grid_template "[default] 1px";
  neg_cursor read_grid_template "[DeFaUlT] 1px";
  neg_cursor read_grid_template "[initial] 1px";
  neg_cursor read_grid_template "[inherit] 1px";
  neg_cursor read_grid_template "[unset] 1px";
  neg_cursor read_grid_template "[revert] 1px";
  neg_cursor read_grid_template "[revert-layer] 1px";
  neg_cursor read_grid_template "[revert-rule] 1px";
  neg_cursor read_grid_template "[a span] 1px";
  neg_cursor ~allow_partial:true read_grid_template "1px [span]";
  (* Nothing else is excluded: the brackets make the position unambiguous, so a
     track keyword is an ordinary line name, and [<custom-ident>*] matches the
     empty list. *)
  check_grid_template ~expected:"[]1px" "[] 1px";
  check_grid_template ~expected:"[none]1px" "[none] 1px";
  check_grid_template ~expected:"[dense]1px" "[dense] 1px";
  check_grid_template ~expected:"[auto-fill]1px" "[auto-fill] 1px";
  (* sec. 7.2.3: [repeat()] holds a track list, so its line-name positions carry
     the same exclusions. *)
  check_grid_template ~expected:"repeat(2,[a]1px)" "repeat(2, [a] 1px)";
  neg_cursor read_grid_template "repeat(2, [span] 1px)";
  neg_cursor read_grid_template "repeat(2, 1px [auto])";
  (* sec. 7.4: the [<string>] form of grid-template keeps its value as raw text,
     and its bracketed positions are [<line-names>] just the same. *)
  check_grid_template ~expected:"[a]\"x\" 1px" "[a] \"x\" 1px";
  neg_cursor read_grid_template "[span] \"x\" 1px";
  neg_cursor read_grid_template "\"x\" 1px [auto]";
  (* CSS Grid 2 (ED) sec. 7.2: [<track-list> = [ <line-names>? [ <track-size> |
     <track-repeat> ] ]+ <line-names>?] places a [<line-names>] before a track
     size or at the very end, so a track list carries at least one track size
     and never two [<line-names>] in a row. sec. 7.2.3 gives the [repeat()] body
     the same shape. *)
  check_grid_template ~expected:"[a]1px[b]" "[a] 1px [b]";
  check_grid_template ~expected:"1px[a]2px" "1px [a] 2px";
  check_grid_template ~expected:"repeat(2,1px[a])" "repeat(2, 1px [a])";
  neg_cursor read_grid_template "[a]";
  neg_cursor read_grid_template "[a] [b]";
  neg_cursor read_grid_template "[a] [b] 1px";
  neg_cursor read_grid_template "1px [a] [b]";
  neg_cursor read_grid_template "1px [a] [b] 2px";
  neg_cursor read_grid_template "1px / [a]";
  neg_cursor read_grid_template "repeat(2,[a])";
  neg_cursor read_grid_template "repeat(2,[a] [b] 1px)"

let test_grid_flex_math () =
  check_grid_flex_math ~roundtrip:true ~expected:"calc(1fr*2)" "calc(1fr * 2)";
  check_grid_flex_math ~roundtrip:true ~expected:"min(1fr,2fr)" "min(1fr, 2fr)";
  check_grid_flex_math ~roundtrip:true "clamp(100px,1fr,300px)";
  neg_cursor read_grid_flex_math "calc(100px)"

let test_grid_template_areas () =
  check_grid_template_areas ~expected:"\"nav main\"\". foot\""
    "\"nav  main\" \".    foot\"";
  check_grid_template_areas ~expected:"\". .\"" "\".  .\"";
  check_grid_template_areas ~minify:false
    ~expected:"\"nav  main\" \".    foot\"" "\"nav  main\" \".    foot\"";
  neg_cursor read_grid_template_areas "\"nav/main\"";
  neg_cursor read_grid_template_areas "\"nav main\" \"foot\"";
  neg_cursor read_grid_template_areas "\"a .\" \". a\""

let test_justify_content () =
  check_justify_content "flex-start";
  check_justify_content "flex-end";
  check_justify_content "center";
  check_justify_content "space-between";
  check_justify_content "space-around";
  check_justify_content "space-evenly";
  neg_cursor read_justify_content "invalid-justify";
  neg_cursor read_justify_content "aroundish"

let test_place_content () =
  check_place_content "center";
  check_place_content "start end";
  check_place_content "flex-start center";
  check_place_content "inherit";
  (* CSS Align 3 sec. 4.2 and 7.2: a baseline position is valid only in the
     align-content slot. When it is the sole value, justify-content defaults to
     start instead of copying it. *)
  check_place_content ~expected:"baseline start" "baseline";
  check_place_content ~expected:"first baseline start" "first baseline";
  check_place_content ~expected:"last baseline start" "last baseline";
  check_place_content "first baseline center";
  neg_cursor ~allow_partial:true read_place_content "center baseline";
  neg_cursor ~allow_partial:true read_place_content "baseline first";
  neg_cursor read_place_content "invalid-place"

let test_place_items () =
  check_place_items "stretch";
  check_place_items "start end";
  check_place_items ~expected:"center" "center center";
  check_place_items "inherit";
  (* css-align-3 (ED) sec. 4.2: <baseline-position> = [ first | last ]? &&
     baseline, in either half of the sec. 7.3 shorthand. *)
  check_place_items "baseline";
  check_place_items "first baseline";
  check_place_items "last baseline";
  check_place_items "first baseline center";
  check_place_items "center last baseline";
  check_place_items "first baseline last baseline";
  (* The && is order-free, but the modifier is only read before the keyword. *)
  neg_cursor ~allow_partial:true read_place_items "baseline first";
  neg_cursor ~allow_partial:true read_place_items "baseline last";
  neg_cursor read_place_items "invalid-place";
  (* sec. 5.2 spells the shorthand [<'align-items'> <'justify-items'>?], and
     [stretch] is one value of each, so the slot after it is an ordinary
     justify-items value rather than a repeat of the keyword. Chrome 153 reads
     each of these and computes align-items: stretch beside the second half. *)
  check_place_items "stretch center";
  check_place_items "stretch normal";
  check_place_items "stretch safe center";
  check_place_items ~expected:"stretch" "stretch stretch";
  check_place_items "center stretch"

let test_box_decoration_break () =
  check_box_decoration_break "clone";
  check_box_decoration_break "slice";
  neg_cursor read_box_decoration_break "invalid-value"

let test_break_value () =
  check_break_value "auto";
  check_break_value "avoid";
  check_break_value "page";
  check_break_value "column";
  check_break_value "inherit";
  neg_cursor read_break_value "invalid-break"

let test_break_inside_value () =
  check_break_inside_value "auto";
  check_break_inside_value "avoid";
  check_break_inside_value "avoid-page";
  check_break_inside_value "avoid-column";
  check_break_inside_value "inherit";
  neg_cursor read_break_inside_value "invalid-break-inside"

let test_page_break_value () =
  check_page_break_value "auto";
  check_page_break_value "always";
  check_page_break_value "avoid";
  check_page_break_value "left";
  check_page_break_value "right";
  check_page_break_value "inherit";
  neg_cursor read_page_break_value "page";
  neg_cursor read_page_break_value "avoid-page"

let test_page_break_inside_value () =
  check_page_break_inside_value "auto";
  check_page_break_inside_value "avoid";
  check_page_break_inside_value "inherit";
  neg_cursor read_page_break_inside_value "always";
  neg_cursor read_page_break_inside_value "avoid-page"

let test_page_size_name () =
  check_page_size_name ~expected:"A5" "a5";
  check_page_size_name ~expected:"A4" "a4";
  check_page_size_name ~expected:"A3" "a3";
  check_page_size_name ~expected:"JIS-B5" "jis-b5";
  check_page_size_name "letter";
  check_page_size_name "legal";
  neg_cursor read_page_size_name "portrait";
  neg_cursor read_page_size_name "tabloid"

let test_page_size_orientation () =
  check_page_size_orientation "portrait";
  check_page_size_orientation "landscape";
  neg_cursor read_page_size_orientation "a4";
  neg_cursor read_page_size_orientation "sideways"

let test_page_size () =
  check_page_size "auto";
  check_page_size "8.5in 11in";
  check_page_size ~expected:"A4 landscape" "a4 landscape";
  (* CSS Paged Media 3 sec. 6.4 sizes a page in [<length [0,inf]>{1,2} | auto |
     [ <page-size> || [ portrait | landscape ]]], so a percentage, a sizing
     keyword and a negative are no page size, and CSS Cascade 5 sec. 7.3 gives
     the descriptor the CSS-wide keywords. *)
  check_page_size "calc(10px + 1em)";
  check_page_size "initial";
  check_page_size "unset";
  check_page_size "revert-layer";
  neg_cursor read_page_size "50%";
  neg_cursor read_page_size "-1px";
  neg_cursor read_page_size "fit-content(20rem)";
  neg_cursor ~allow_partial:true read_page_size "10px 20%";
  check_page_size "letter portrait";
  check_page_size "landscape";
  check_page_size "inherit";
  neg_cursor read_page_size "";
  neg_cursor read_page_size "a4 sideways";
  neg_cursor read_page_size "8.5in 11in 12in"

let test_columns_value () =
  check_columns_value "auto";
  check_columns_value "2";
  check_columns_value "inherit";
  neg_cursor read_columns_value "invalid-columns";
  (* CSS Multicol 2 sec. 4.1 spells [column-width] as [auto | <length [0,inf]>],
     so a percentage and a negative are no column width. *)
  neg_cursor read_columns_value "-1px";
  neg_cursor read_columns_value "50%";
  (* A percentage nested in math is still a percentage, so the width slot takes
     no math whose own type is one. Chrome 153 refuses each of these three and
     takes the two below. *)
  neg_cursor read_columns_value "calc(50% + 25%)";
  neg_cursor read_columns_value "calc(50% - 25%)";
  neg_cursor read_columns_value "clamp(1px,50%,2px)";
  check_columns_value "calc(1px + 2em)";
  check_columns_value "min(1px,2px)"

let test_field_sizing () =
  check_field_sizing "content";
  check_field_sizing "fixed";
  check_field_sizing "inherit";
  neg_cursor read_field_sizing "invalid-field-sizing"

let test_opacity () =
  check_opacity ~expected:".5" "0.5";
  check_opacity "1";
  check_opacity "0";
  (* A <percentage> operand inside an opacity calc() is the number it denotes
     (50% = .5), so the calc parses (was rejected) and folds like any number. *)
  check_opacity ~expected:"calc(.5*2)" "calc(50% * 2)";
  decl_optimizes ~prop:"opacity" ~held:"calc(.5*2)" ~into:"1" "calc(50% * 2)";
  decl_optimizes ~prop:"opacity" ~held:"calc(.5 + .25)" ~into:".75"
    "calc(50% + 25%)";
  decl_optimizes ~prop:"opacity" ~held:"calc(.5*var(--x))"
    ~into:"calc(.5*var(--x))" "calc(50% * var(--x))";
  neg_cursor read_opacity "invalid-opacity";
  (* CSS Values 4 sec. 11.4 gives [abs()]/[sign()] exactly one [<calc-sum>]
     argument; [Cursor.call] did not require the reader to consume its whole
     sub-cursor, so e.g. [abs(.5 .5)] read only the first [.5] and answered
     [abs(.5)] instead of invalidating the declaration. An [<opacity-value>]
     math function resolves to its number as it reads, the way [min()] and
     [calc(abs())] already do here. *)
  check_opacity ~expected:".5" "abs(.5)";
  check_opacity ~expected:"1" "sign(.5)";
  check_opacity ~expected:".5" "abs( .5 )";
  neg_cursor read_opacity "abs(.5 .5)";
  neg_cursor read_opacity "sign(.5 red)"

let test_order () =
  check_order "1";
  check_order "-2";
  check_order "100";
  neg_cursor read_order "invalid-order"

let suite =
  ( "properties_layout",
    [
      test_case "display" `Quick test_display;
      test_case "position" `Quick test_position;
      test_case "overflow" `Quick test_overflow;
      test_case "zoom" `Quick test_zoom;
      test_case "visibility" `Quick test_visibility;
      test_case "z-index" `Quick test_z_index;
      test_case "flex-direction" `Quick test_flex_direction;
      test_case "flex-wrap" `Quick test_flex_wrap;
      test_case "align-self" `Quick test_align_self;
      test_case "table-layout" `Quick test_table_layout;
      test_case "border-collapse" `Quick test_border_collapse;
      test_case "content-visibility" `Quick test_content_visibility;
      test_case "isolation" `Quick test_isolation;
      (* Additional coverage for missing readers *)
      test_case "grid auto-flow component" `Quick test_grid_auto_flow_component;
      test_case "grid auto-flow" `Quick test_grid_auto_flow;
      test_case "grid template" `Quick test_grid_template;
      test_case "grid template areas" `Quick test_grid_template_areas;
      test_case "grid line" `Quick test_grid_line;
      test_case "grid line pair" `Quick test_grid_line_pair;
      test_case "align-items" `Quick test_align_items;
      test_case "justify-content" `Quick test_justify_content;
      test_case "place-items" `Quick test_place_items;
      test_case "place-content" `Quick test_place_content;
      test_case "flex" `Quick test_flex;
      test_case "column-width" `Quick test_column_width;
      test_case "column-count" `Quick test_column_count;
      test_case "position-try" `Quick test_position_try;
      test_case "grid-area" `Quick test_grid_area;
      test_case "aspect-ratio" `Quick test_aspect_ratio;
      test_case "justify_self" `Quick test_justify_self;
      test_case "align_content_values" `Quick test_align_content;
      test_case "justify_items" `Quick test_justify_items;
      test_case "flex_basis" `Quick test_flex_basis;
      test_case "gap" `Quick test_gap;
      test_case "position_value" `Quick test_position_value;
      test_case "box_sizing" `Quick test_box_sizing;
      test_case "content_visibility" `Quick test_content_visibility;
      test_case "container_type" `Quick test_container_type;
      test_case "container_shorthand" `Quick test_container_shorthand;
      test_case "contain" `Quick test_contain;
      test_case "isolation" `Quick test_isolation;
      test_case "webkit_box_orient" `Quick test_webkit_box_orient;
      test_case "moz_orient" `Quick test_moz_orient;
      test_case "clear" `Quick test_clear;
      test_case "float_side" `Quick test_float_side;
      test_case "align_items" `Quick test_align_items;
      test_case "aspect_ratio" `Quick test_aspect_ratio;
      test_case "flex" `Quick test_flex;
      test_case "grid_line" `Quick test_grid_line;
      test_case "grid_line_pair" `Quick test_grid_line_pair;
      test_case "grid_flex_math" `Quick test_grid_flex_math;
      test_case "grid_template" `Quick test_grid_template;
      test_case "justify_content" `Quick test_justify_content;
      test_case "place_content" `Quick test_place_content;
      test_case "place_items" `Quick test_place_items;
      test_case "box_decoration_break" `Quick test_box_decoration_break;
      test_case "break_value" `Quick test_break_value;
      test_case "break_inside_value" `Quick test_break_inside_value;
      test_case "page_break_value" `Quick test_page_break_value;
      test_case "page_break_inside_value" `Quick test_page_break_inside_value;
      test_case "page_size" `Quick test_page_size;
      test_case "page_size_name" `Quick test_page_size_name;
      test_case "page_size_orientation" `Quick test_page_size_orientation;
      test_case "columns_value" `Quick test_columns_value;
      test_case "field_sizing" `Quick test_field_sizing;
      test_case "opacity" `Quick test_opacity;
      test_case "order" `Quick test_order;
    ] )
