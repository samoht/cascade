open Alcotest

let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Generic check function for property values *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
  let v2 = reader t2 in
  let s2 = to_string pp v2 in
  check string (Fmt.str "roundtrip %s %s" name input) s s2

(* Check functions for each type - one-liner definitions *)
let check_display =
  check_value "display" Css.Properties.pp_display Css.Properties.read_display

let check_position =
  check_value "position" Css.Properties.pp_position Css.Properties.read_position

let check_overflow =
  check_value "overflow" Css.Properties.pp_overflow Css.Properties.read_overflow

let check_border_style =
  check_value "border-style" Css.Properties.pp_border_style
    Css.Properties.read_border_style

let check_visibility =
  check_value "visibility" Css.Properties.pp_visibility
    Css.Properties.read_visibility

let check_z_index =
  check_value "z-index" Css.Properties.pp_z_index Css.Properties.read_z_index

let check_flex_direction =
  check_value "flex-direction" Css.Properties.pp_flex_direction
    Css.Properties.read_flex_direction

let check_flex_wrap =
  check_value "flex-wrap" Css.Properties.pp_flex_wrap
    Css.Properties.read_flex_wrap

let check_align =
  check_value "align" Css.Properties.pp_align Css.Properties.read_align

let check_align_self =
  check_value "align-self" Css.Properties.pp_align_self
    Css.Properties.read_align_self

let check_justify =
  check_value "justify" Css.Properties.pp_justify Css.Properties.read_justify

let check_font_style =
  check_value "font-style" Css.Properties.pp_font_style
    Css.Properties.read_font_style

let check_text_align =
  check_value "text-align" Css.Properties.pp_text_align
    Css.Properties.read_text_align

let check_text_decoration_style =
  check_value "text-decoration-style" Css.Properties.pp_text_decoration_style
    Css.Properties.read_text_decoration_style

let check_text_overflow =
  check_value "text-overflow" Css.Properties.pp_text_overflow
    Css.Properties.read_text_overflow

let check_text_wrap =
  check_value "text-wrap" Css.Properties.pp_text_wrap
    Css.Properties.read_text_wrap

let check_white_space =
  check_value "white-space" Css.Properties.pp_white_space
    Css.Properties.read_white_space

let check_word_break =
  check_value "word-break" Css.Properties.pp_word_break
    Css.Properties.read_word_break

let check_overflow_wrap =
  check_value "overflow-wrap" Css.Properties.pp_overflow_wrap
    Css.Properties.read_overflow_wrap

let check_hyphens =
  check_value "hyphens" Css.Properties.pp_hyphens Css.Properties.read_hyphens

let check_line_height =
  check_value "line-height" Css.Properties.pp_line_height
    Css.Properties.read_line_height

(* Additional check functions for list, table, and other types *)
let check_list_style_type =
  check_value "list-style-type" Css.Properties.pp_list_style_type
    Css.Properties.read_list_style_type

let check_list_style_position =
  check_value "list-style-position" Css.Properties.pp_list_style_position
    Css.Properties.read_list_style_position

let check_list_style_image =
  check_value "list-style-image" Css.Properties.pp_list_style_image
    Css.Properties.read_list_style_image

let check_table_layout =
  check_value "table-layout" Css.Properties.pp_table_layout
    Css.Properties.read_table_layout

let check_border_collapse =
  check_value "border-collapse" Css.Properties.pp_border_collapse
    Css.Properties.read_border_collapse

let check_user_select =
  check_value "user-select" Css.Properties.pp_user_select
    Css.Properties.read_user_select

let check_pointer_events =
  check_value "pointer-events" Css.Properties.pp_pointer_events
    Css.Properties.read_pointer_events

let check_touch_action =
  check_value "touch-action" Css.Properties.pp_touch_action
    Css.Properties.read_touch_action

let check_resize =
  check_value "resize" Css.Properties.pp_resize Css.Properties.read_resize

let check_box_sizing =
  check_value "box-sizing" Css.Properties.pp_box_sizing
    Css.Properties.read_box_sizing

let check_object_fit =
  check_value "object-fit" Css.Properties.pp_object_fit
    Css.Properties.read_object_fit

let check_content_visibility =
  check_value "content-visibility" Css.Properties.pp_content_visibility
    Css.Properties.read_content_visibility

let check_container_type =
  check_value "container-type" Css.Properties.pp_container_type
    Css.Properties.read_container_type

let check_contain =
  check_value "contain" Css.Properties.pp_contain Css.Properties.read_contain

let check_isolation =
  check_value "isolation" Css.Properties.pp_isolation
    Css.Properties.read_isolation

let check_scroll_behavior =
  check_value "scroll-behavior" Css.Properties.pp_scroll_behavior
    Css.Properties.read_scroll_behavior

(* More check functions for complex types *)
let check_scroll_snap_align =
  check_value "scroll-snap-align" Css.Properties.pp_scroll_snap_align
    Css.Properties.read_scroll_snap_align

let check_scroll_snap_stop =
  check_value "scroll-snap-stop" Css.Properties.pp_scroll_snap_stop
    Css.Properties.read_scroll_snap_stop

let check_scroll_snap_type =
  check_value "scroll-snap-type" Css.Properties.pp_scroll_snap_type
    Css.Properties.read_scroll_snap_type

let check_svg_paint =
  check_value "svg-paint" Css.Properties.pp_svg_paint
    Css.Properties.read_svg_paint

let check_direction =
  check_value "direction" Css.Properties.pp_direction
    Css.Properties.read_direction

let check_unicode_bidi =
  check_value "unicode-bidi" Css.Properties.pp_unicode_bidi
    Css.Properties.read_unicode_bidi

let check_writing_mode =
  check_value "writing-mode" Css.Properties.pp_writing_mode
    Css.Properties.read_writing_mode

let check_webkit_appearance =
  check_value "-webkit-appearance" Css.Properties.pp_webkit_appearance
    Css.Properties.read_webkit_appearance

let check_webkit_font_smoothing =
  check_value "-webkit-font-smoothing" Css.Properties.pp_webkit_font_smoothing
    Css.Properties.read_webkit_font_smoothing

let check_moz_osx_font_smoothing =
  check_value "-moz-osx-font-smoothing" Css.Properties.pp_moz_osx_font_smoothing
    Css.Properties.read_moz_osx_font_smoothing

let check_webkit_box_orient =
  check_value "-webkit-box-orient" Css.Properties.pp_webkit_box_orient
    Css.Properties.read_webkit_box_orient

let check_forced_color_adjust =
  check_value "forced-color-adjust" Css.Properties.pp_forced_color_adjust
    Css.Properties.read_forced_color_adjust

let check_appearance =
  check_value "appearance" Css.Properties.pp_appearance
    Css.Properties.read_appearance

let check_clear =
  check_value "clear" Css.Properties.pp_clear Css.Properties.read_clear

let check_float_side =
  check_value "float" Css.Properties.pp_float_side
    Css.Properties.read_float_side

let check_text_decoration_skip_ink =
  check_value "text-decoration-skip-ink"
    Css.Properties.pp_text_decoration_skip_ink
    Css.Properties.read_text_decoration_skip_ink

let check_vertical_align =
  check_value "vertical-align" Css.Properties.pp_vertical_align
    Css.Properties.read_vertical_align

let check_outline_style =
  check_value "outline-style" Css.Properties.pp_outline_style
    Css.Properties.read_outline_style

let check_font_family =
  check_value "font-family" Css.Properties.pp_font_family
    Css.Properties.read_font_family

let check_font_stretch =
  check_value "font-stretch" Css.Properties.pp_font_stretch
    Css.Properties.read_font_stretch

let check_font_variant_numeric =
  check_value "font-variant-numeric" Css.Properties.pp_font_variant_numeric
    Css.Properties.read_font_variant_numeric

let check_font_feature_settings =
  check_value "font-feature-settings" Css.Properties.pp_font_feature_settings
    Css.Properties.read_font_feature_settings

let check_font_variation_settings =
  check_value "font-variation-settings"
    Css.Properties.pp_font_variation_settings
    Css.Properties.read_font_variation_settings

let check_backface_visibility =
  check_value "backface-visibility" Css.Properties.pp_backface_visibility
    Css.Properties.read_backface_visibility

let check_scale =
  check_value "scale" Css.Properties.pp_scale Css.Properties.read_scale

let check_timing_function =
  check_value "timing-function" Css.Properties.pp_timing_function
    Css.Properties.read_timing_function

let check_transition_property =
  check_value "transition-property" Css.Properties.pp_transition_property
    Css.Properties.read_transition_property

let check_transition =
  check_value "transition" Css.Properties.pp_transition
    Css.Properties.read_transition

let check_animation_direction =
  check_value "animation-direction" Css.Properties.pp_animation_direction
    Css.Properties.read_animation_direction

let check_animation_fill_mode =
  check_value "animation-fill-mode" Css.Properties.pp_animation_fill_mode
    Css.Properties.read_animation_fill_mode

let check_animation_iteration_count =
  check_value "animation-iteration-count"
    Css.Properties.pp_animation_iteration_count
    Css.Properties.read_animation_iteration_count

let check_animation_play_state =
  check_value "animation-play-state" Css.Properties.pp_animation_play_state
    Css.Properties.read_animation_play_state

let check_animation =
  check_value "animation" Css.Properties.pp_animation
    Css.Properties.read_animation

let check_blend_mode =
  check_value "mix-blend-mode" Css.Properties.pp_blend_mode
    Css.Properties.read_blend_mode

let check_text_shadow =
  check_value "text-shadow" Css.Properties.pp_text_shadow
    Css.Properties.read_text_shadow

let check_box_shadow =
  check_value "box-shadow" Css.Properties.pp_box_shadow
    Css.Properties.read_box_shadow

let check_box_shadows =
  check_value "box-shadows"
    (Css.Pp.list ~sep:Css.Pp.comma Css.Properties.pp_box_shadow)
    Css.Properties.read_box_shadows

let check_filter =
  check_value "filter" Css.Properties.pp_filter Css.Properties.read_filter

let check_background_attachment =
  check_value "background-attachment" Css.Properties.pp_background_attachment
    Css.Properties.read_background_attachment

let check_background_repeat =
  check_value "background-repeat" Css.Properties.pp_background_repeat
    Css.Properties.read_background_repeat

let check_background_size =
  check_value "background-size" Css.Properties.pp_background_size
    Css.Properties.read_background_size

let check_background_image =
  check_value "background-image" Css.Properties.pp_background_image
    Css.Properties.read_background_image

let check_overscroll_behavior =
  check_value "overscroll-behavior" Css.Properties.pp_overscroll_behavior
    Css.Properties.read_overscroll_behavior

let check_aspect_ratio =
  check_value "aspect-ratio" Css.Properties.pp_aspect_ratio
    Css.Properties.read_aspect_ratio

let check_content =
  check_value "content" Css.Properties.pp_content Css.Properties.read_content

let check_grid_auto_flow =
  check_value "grid-auto-flow" Css.Properties.pp_grid_auto_flow
    Css.Properties.read_grid_auto_flow

let check_grid_track_size =
  check_value "grid-track-size" Css.Properties.pp_grid_track_size
    Css.Properties.read_grid_track_size

let check_grid_template =
  check_value "grid-template" Css.Properties.pp_grid_template
    Css.Properties.read_grid_template

let check_grid_line =
  check_value "grid-line" Css.Properties.pp_grid_line
    Css.Properties.read_grid_line

let check_align_items =
  check_value "align-items" Css.Properties.pp_align_items
    Css.Properties.read_align_items

let check_justify_content =
  check_value "justify-content" Css.Properties.pp_justify_content
    Css.Properties.read_justify_content

let check_flex =
  check_value "flex" Css.Properties.pp_flex Css.Properties.read_flex

let check_place_items =
  check_value "place-items" Css.Properties.pp_place_items
    Css.Properties.read_place_items

let check_place_content =
  check_value "place-content" Css.Properties.pp_place_content
    Css.Properties.read_place_content

let check_transform =
  check_value "transform" Css.Properties.pp_transform
    Css.Properties.read_transform

let check_gradient_direction =
  check_value "gradient-direction" Css.Properties.pp_gradient_direction
    Css.Properties.read_gradient_direction

let check_gradient_stop =
  check_value "gradient-stop" Css.Properties.pp_gradient_stop
    Css.Properties.read_gradient_stop

(* Helper for any_property type *)
let pp_any_property : Css.Properties.any_property Css.Pp.t =
 fun ctx (Css.Properties.Prop p) -> Css.Properties.pp_property ctx p

let check_property =
  check_value "property" pp_any_property Css.Properties.read_property

(* Test functions that use the check_<type> functions *)
let test_display () =
  check_display "none";
  check_display "block";
  check_display "inline";
  check_display "inline-block";
  check_display "flex";
  check_display "inline-flex";
  check_display "grid";
  check_display "inline-grid";
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
  check_display "contents";
  check_display "-webkit-box"

let test_position () =
  check_position "static";
  check_position "relative";
  check_position "absolute";
  check_position "fixed";
  check_position "sticky"

let test_overflow () =
  check_overflow "visible";
  check_overflow "hidden";
  check_overflow "scroll";
  check_overflow "auto";
  check_overflow "clip"

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
  check_border_style "hidden"

let test_visibility () =
  check_visibility "visible";
  check_visibility "hidden";
  check_visibility "collapse"

let test_z_index () =
  check_z_index "auto";
  check_z_index "10";
  check_z_index "-1"

let test_flex_direction () =
  check_flex_direction "row";
  check_flex_direction "row-reverse";
  check_flex_direction "column";
  check_flex_direction "column-reverse"

let test_flex_wrap () =
  check_flex_wrap "nowrap";
  check_flex_wrap "wrap";
  check_flex_wrap "wrap-reverse"

let test_align () =
  check_align "normal";
  check_align "start";
  check_align "end";
  check_align "center";
  check_align "stretch"

let test_align_self () =
  check_align_self "auto";
  check_align_self "flex-start";
  check_align_self "flex-end";
  check_align_self "center";
  check_align_self "baseline";
  check_align_self "stretch"

let test_justify () =
  check_justify "auto";
  check_justify "normal";
  check_justify "stretch";
  check_justify "center";
  check_justify "start";
  check_justify "end";
  check_justify "flex-start";
  check_justify "flex-end";
  check_justify "self-start";
  check_justify "self-end";
  check_justify "left";
  check_justify "right";
  check_justify "baseline";
  check_justify "inherit"

let test_font_style () =
  check_font_style "normal";
  check_font_style "italic";
  check_font_style "oblique";
  check_font_style "inherit"

let test_text_align () =
  check_text_align "left";
  check_text_align "right";
  check_text_align "center";
  check_text_align "justify";
  check_text_align "start";
  check_text_align "end";
  check_text_align "inherit"

let test_text_decoration_style () =
  check_text_decoration_style "solid";
  check_text_decoration_style "double";
  check_text_decoration_style "dotted";
  check_text_decoration_style "dashed";
  check_text_decoration_style "wavy";
  check_text_decoration_style "inherit"

let test_text_overflow () =
  check_text_overflow "clip";
  check_text_overflow "ellipsis";
  check_text_overflow "inherit"

let test_text_wrap () =
  check_text_wrap "wrap";
  check_text_wrap "nowrap";
  check_text_wrap "balance";
  check_text_wrap "pretty";
  check_text_wrap "inherit"

let test_white_space () =
  check_white_space "normal";
  check_white_space "nowrap";
  check_white_space "pre";
  check_white_space "pre-wrap";
  check_white_space "pre-line";
  check_white_space "break-spaces";
  check_white_space "inherit"

let test_word_break () =
  check_word_break "normal";
  check_word_break "break-all";
  check_word_break "keep-all";
  check_word_break "break-word";
  check_word_break "inherit"

let test_overflow_wrap () =
  check_overflow_wrap "normal";
  check_overflow_wrap "break-word";
  check_overflow_wrap "anywhere";
  check_overflow_wrap "inherit"

let test_hyphens () =
  check_hyphens "none";
  check_hyphens "manual";
  check_hyphens "auto";
  check_hyphens "inherit"

let test_line_height () =
  (* Only test values supported by the simplified reader *)
  check_line_height "normal";
  check_line_height "inherit";
  check_line_height "1.5";
  check_line_height "120%"

let test_list_style () =
  check_list_style_type "none";
  check_list_style_type "disc";
  check_list_style_type "circle";
  check_list_style_type "square";
  check_list_style_type "decimal";
  check_list_style_type "lower-alpha";
  check_list_style_type "upper-alpha";
  check_list_style_type "lower-roman";
  check_list_style_type "upper-roman";
  check_list_style_position "inside";
  check_list_style_position "outside";
  check_list_style_position "inherit";
  check_list_style_image "none";
  check_list_style_image "inherit";
  check_list_style_image "url(https://example.com/x.png)"

let test_table_layout () =
  check_table_layout "auto";
  check_table_layout "fixed";
  check_table_layout "inherit"

let test_border_collapse () =
  check_border_collapse "collapse";
  check_border_collapse "separate";
  check_border_collapse "inherit"

let test_user_pointer () =
  check_user_select "none";
  check_user_select "auto";
  check_user_select "text";
  check_user_select "all";
  check_user_select "contain";
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
  check_pointer_events "inherit"

let test_touch_resize_box_sizing () =
  check_touch_action "auto";
  check_touch_action "none";
  check_touch_action "pan-x";
  check_touch_action "pan-y";
  check_touch_action "manipulation";
  check_touch_action "inherit";
  check_resize "none";
  check_resize "both";
  check_resize "horizontal";
  check_resize "vertical";
  check_resize "block";
  check_resize "inline";
  check_resize "inherit";
  check_box_sizing "border-box";
  check_box_sizing "content-box";
  check_box_sizing "inherit"

let test_object_and_content_visibility () =
  check_object_fit "fill";
  check_object_fit "contain";
  check_object_fit "cover";
  check_object_fit "none";
  check_object_fit "scale-down";
  check_object_fit "inherit";
  check_content_visibility "visible";
  check_content_visibility "auto";
  check_content_visibility "hidden";
  check_content_visibility "inherit"

let test_container_and_contain () =
  check_container_type "normal";
  check_container_type "inline-size";
  check_container_type "size";
  check_contain "none";
  check_contain "strict";
  check_contain "content";
  check_contain "size";
  check_contain "layout";
  check_contain "style";
  check_contain "paint"

let test_isolation_and_scroll () =
  check_isolation "auto";
  check_isolation "isolate";
  check_isolation "inherit";
  check_scroll_behavior "auto";
  check_scroll_behavior "smooth";
  check_scroll_behavior "inherit"

let test_scroll_snap () =
  check_scroll_snap_align "none";
  check_scroll_snap_align "start";
  check_scroll_snap_align "end";
  check_scroll_snap_align "center";
  check_scroll_snap_stop "normal";
  check_scroll_snap_stop "always";
  check_scroll_snap_stop "inherit";
  check_scroll_snap_type "none"

let test_svg_direction_writing () =
  check_svg_paint "none";
  check_svg_paint "currentcolor";
  check_direction "ltr";
  check_direction "rtl";
  check_direction "inherit";
  check_unicode_bidi "normal";
  check_unicode_bidi "embed";
  check_unicode_bidi "isolate";
  check_unicode_bidi "bidi-override";
  check_unicode_bidi "isolate-override";
  check_unicode_bidi "plaintext";
  check_unicode_bidi "inherit";
  check_writing_mode "horizontal-tb";
  check_writing_mode "vertical-rl";
  check_writing_mode "vertical-lr";
  check_writing_mode "inherit"

let test_vendor_misc () =
  check_webkit_appearance "none";
  check_webkit_appearance "auto";
  check_webkit_appearance "button";
  check_webkit_appearance "textfield";
  check_webkit_appearance "inherit";
  check_webkit_font_smoothing "auto";
  check_webkit_font_smoothing "antialiased";
  check_webkit_font_smoothing "subpixel-antialiased";
  check_webkit_font_smoothing "inherit";
  check_moz_osx_font_smoothing "auto";
  check_moz_osx_font_smoothing "grayscale";
  check_moz_osx_font_smoothing "inherit";
  check_webkit_box_orient "horizontal";
  check_webkit_box_orient "vertical";
  check_webkit_box_orient "inherit";
  check_forced_color_adjust "none";
  check_forced_color_adjust "auto";
  check_forced_color_adjust "inherit";
  check_appearance "none";
  check_appearance "auto";
  check_appearance "button";
  check_appearance "textfield";
  check_appearance "menulist";
  check_appearance "inherit"

let test_clear_float_vertical_outline () =
  check_clear "none";
  check_clear "left";
  check_clear "right";
  check_clear "both";
  check_float_side "none";
  check_float_side "left";
  check_float_side "right";
  check_float_side "inline-start";
  check_float_side "inline-end";
  check_float_side "inherit";
  check_text_decoration_skip_ink "auto";
  check_text_decoration_skip_ink "none";
  check_text_decoration_skip_ink "all";
  check_text_decoration_skip_ink "inherit";
  check_vertical_align "baseline";
  check_vertical_align "top";
  check_vertical_align "middle";
  check_vertical_align "bottom";
  check_vertical_align "text-top";
  check_vertical_align "text-bottom";
  check_vertical_align "sub";
  check_vertical_align "super";
  check_vertical_align "inherit";
  check_outline_style "none";
  check_outline_style "solid";
  check_outline_style "dashed";
  check_outline_style "dotted";
  check_outline_style "double";
  check_outline_style "groove";
  check_outline_style "ridge";
  check_outline_style "inset";
  check_outline_style "outset";
  check_outline_style "auto"

let test_fonts_misc_effects () =
  check_font_family "sans-serif";
  check_font_family "serif";
  check_font_family "monospace";
  check_font_family "cursive";
  check_font_family "fantasy";
  check_font_family "system-ui";
  check_font_stretch "normal";
  check_font_stretch "50%";
  check_font_stretch "ultra-condensed";
  check_font_stretch "ultra-expanded";
  check_font_stretch "inherit";
  check_font_variant_numeric "normal";
  check_font_variant_numeric "lining-nums";
  check_font_variant_numeric "tabular-nums";
  check_font_feature_settings "normal";
  check_font_feature_settings "inherit";
  check_font_feature_settings "\"kern\"";
  check_font_variation_settings "normal";
  check_font_variation_settings "inherit";
  check_font_variation_settings "\"wght\"";
  check_backface_visibility "visible";
  check_backface_visibility "hidden";
  check_backface_visibility "inherit";
  check_scale "none";
  check_scale "1";
  check_scale ~expected:".5" "0.5";
  check_timing_function "ease";
  check_timing_function "linear";
  check_timing_function "ease-in";
  check_timing_function "ease-out";
  check_timing_function "ease-in-out";
  check_timing_function "step-start";
  check_timing_function "step-end";
  check_transition_property "all";
  check_transition_property "none";
  check_transition_property "opacity";
  check_transition "opacity";
  check_transition "transform";
  check_animation_direction "normal";
  check_animation_direction "reverse";
  check_animation_direction "alternate";
  check_animation_direction "alternate-reverse";
  check_animation_fill_mode "none";
  check_animation_fill_mode "forwards";
  check_animation_fill_mode "backwards";
  check_animation_fill_mode "both";
  check_animation_iteration_count "infinite";
  check_animation_iteration_count "1";
  check_animation_iteration_count "2.5";
  check_animation_play_state "running";
  check_animation_play_state "paused";
  check_animation "fade";
  check_animation "spin";
  check_blend_mode "normal";
  check_blend_mode "multiply";
  check_blend_mode "screen";
  check_blend_mode "overlay";
  check_blend_mode "darken";
  check_blend_mode "lighten";
  check_blend_mode "color-dodge";
  check_blend_mode "color-burn";
  check_blend_mode "hard-light";
  check_blend_mode "soft-light";
  check_blend_mode "difference";
  check_blend_mode "exclusion";
  check_blend_mode "hue";
  check_blend_mode "saturation";
  check_blend_mode "color";
  check_blend_mode "luminosity"

let test_shadows_filters_background () =
  check_text_shadow "none";
  check_text_shadow "inherit";
  check_text_shadow "2px 2px";
  (* h-offset v-offset *)
  check_text_shadow "2px 2px 4px";
  (* h-offset v-offset blur *)
  check_text_shadow "2px 2px red";
  (* h-offset v-offset color *)
  check_text_shadow "2px 2px 4px red";
  (* h-offset v-offset blur color *)
  check_text_shadow "red 2px 2px" ~expected:"2px 2px red";
  (* color first normalizes to end *)
  check_text_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  (* color first normalizes to end *)
  check_text_shadow "-2px -2px";
  (* negative offsets *)
  check_text_shadow "0 0 10px";

  (* zero offsets with blur *)

  (* Box-shadow tests - comprehensive coverage *)
  check_box_shadow "none";
  check_box_shadow "inherit";
  check_box_shadow "2px 2px";
  (* h-offset v-offset *)
  check_box_shadow "2px 2px 4px";
  (* h-offset v-offset blur *)
  check_box_shadow "2px 2px 4px 1px";
  (* h-offset v-offset blur spread *)
  check_box_shadow "2px 2px red";
  (* h-offset v-offset color *)
  check_box_shadow "2px 2px 4px red";
  (* h-offset v-offset blur color *)
  check_box_shadow "2px 2px 4px 1px red";
  (* h-offset v-offset blur spread color *)
  check_box_shadow "red 2px 2px" ~expected:"2px 2px red";
  (* color first normalizes to end *)
  check_box_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  (* color first normalizes to end *)
  check_box_shadow "red 2px 2px 4px 1px" ~expected:"2px 2px 4px 1px red";
  (* color first normalizes to end *)
  check_box_shadow "inset 2px 2px";
  (* inset h-offset v-offset *)
  check_box_shadow "inset 2px 2px 4px";
  (* inset h-offset v-offset blur *)
  check_box_shadow "inset 2px 2px 4px 1px";
  (* inset h-offset v-offset blur spread *)
  check_box_shadow "inset 2px 2px red";
  (* inset h-offset v-offset color *)
  check_box_shadow "inset 2px 2px 4px red";
  (* inset h-offset v-offset blur color *)
  check_box_shadow "inset 2px 2px 4px 1px red";
  (* inset h-offset v-offset blur spread color *)
  check_box_shadow "inset red 2px 2px" ~expected:"inset 2px 2px red";
  (* color normalizes to end *)
  check_box_shadow "inset red 2px 2px 4px" ~expected:"inset 2px 2px 4px red";
  (* color normalizes to end *)
  check_box_shadow "inset red 2px 2px 4px 1px"
    ~expected:"inset 2px 2px 4px 1px red";
  (* color normalizes to end *)
  (* List variant for shadows *)
  check_box_shadows "none";
  check_box_shadows "none,none";
  check_box_shadows "2px 2px,4px 4px red";

  check_filter "none";
  check_background_attachment "scroll";
  check_background_attachment "fixed";
  check_background_attachment "local";
  check_background_attachment "inherit";
  check_background_repeat "repeat";
  check_background_repeat "repeat-x";
  check_background_repeat "repeat-y";
  check_background_repeat "no-repeat";
  check_background_repeat "space";
  check_background_repeat "round";
  check_background_repeat "inherit";
  check_background_size "auto";
  check_background_size "cover";
  check_background_size "contain";
  check_background_size "inherit";
  check_background_image "none";
  check_background_image "url(./img.png)"

let test_property_names () =
  let to_s : type a. a Css.Properties.property -> string =
   fun prop -> Css.Pp.to_string Css.Properties.pp_property prop
  in
  (* Test color properties *)
  check string "property name" "background-color"
    (to_s Css.Properties.Background_color);
  check string "property name" "color" (to_s Css.Properties.Color);
  check string "property name" "border-color" (to_s Css.Properties.Border_color);
  check string "property name" "outline-color"
    (to_s Css.Properties.Outline_color);
  (* Test border style property *)
  check string "property name" "border-style" (to_s Css.Properties.Border_style);
  (* Test length properties *)
  check string "property name" "padding-left" (to_s Css.Properties.Padding_left);
  check string "property name" "margin-top" (to_s Css.Properties.Margin_top);
  check string "property name" "width" (to_s Css.Properties.Width);
  check string "property name" "height" (to_s Css.Properties.Height);
  check string "property name" "font-size" (to_s Css.Properties.Font_size);
  check string "property name" "line-height" (to_s Css.Properties.Line_height);
  (* Test other properties *)
  check string "property name" "display" (to_s Css.Properties.Display);
  check string "property name" "position" (to_s Css.Properties.Position);
  check string "property name" "visibility" (to_s Css.Properties.Visibility);
  check string "property name" "z-index" (to_s Css.Properties.Z_index);
  check string "property name" "transform" (to_s Css.Properties.Transform);
  check string "property name" "cursor" (to_s Css.Properties.Cursor)

(* Roundtrip tests for property names via read_property/pp_property *)
let test_property_read_pp_roundtrip () =
  check_property "width";
  check_property "height";
  check_property "color";
  check_property "background-color";
  check_property "display";
  check_property "position";
  check_property "overflow";
  check_property "margin";
  check_property "padding";
  check_property "border-width";
  check_property "font-size"

let test_pp_property_value () =
  let pp = Css.Properties.pp_property_value in
  check string "width 10px" "10px"
    (to_string pp (Css.Properties.Width, Css.Values.Px 10.));
  check string "color red" "red"
    (to_string pp (Css.Properties.Color, Css.Values.Named Css.Values.Red));
  check string "background-image list" "url(./x.png), none"
    (to_string pp
       ( Css.Properties.Background_image,
         [ Css.Properties.Url "./x.png"; Css.Properties.None ] ));
  check string "transform none" "none"
    (to_string pp (Css.Properties.Transform, [ Css.Properties.None ]));
  check string "content hello" "\"hello\""
    (to_string pp (Css.Properties.Content, Css.Properties.String "hello"))

let test_grid_template_line () =
  check_grid_template "none";
  check_grid_template "subgrid";
  check_grid_line "auto";
  (* span with number requires space per CSS spec *)
  check_grid_line "span 2";
  check_grid_line "3";
  check_grid_line "name"

let test_place_align_justify_flex () =
  check_align_items "center";
  check_align_items "flex-start";
  check_align_items "flex-end";
  check_align_items "baseline";
  check_align_items "stretch";
  check_justify_content "center";
  check_justify_content "flex-start";
  check_justify_content "flex-end";
  check_justify_content "space-between";
  check_justify_content "space-around";
  check_justify_content "space-evenly";
  check_flex "none";
  check_flex "auto";
  check_flex "initial";
  check_flex "1";
  check_flex "2";
  check_place_items "center";
  check_place_items "stretch baseline"

let test_transform () =
  (* Transform tests - comprehensive coverage *)
  check_transform "none";

  (* Translate functions *)
  check_transform "translateX(10px)";
  check_transform "translateX(-50%)";
  check_transform "translateY(2em)";
  check_transform "translateZ(100px)";
  check_transform "translate(10px)";
  check_transform "translate(10px, 20px)" ~expected:"translate(10px,20px)";
  check_transform "translate3d(10px, 20px, 30px)"
    ~expected:"translate3d(10px,20px,30px)";

  (* Rotate functions *)
  check_transform "rotate(45deg)";
  check_transform "rotate(0.5turn)" ~expected:"rotate(.5turn)";
  check_transform "rotate(3.14rad)";
  check_transform "rotateX(45deg)";
  check_transform "rotateY(90deg)";
  check_transform "rotateZ(180deg)";
  check_transform "rotate3d(1, 0, 0, 45deg)" ~expected:"rotate3d(1,0,0,45deg)";
  check_transform "rotate3d(0, 1, 0, 90deg)" ~expected:"rotate3d(0,1,0,90deg)";
  check_transform "rotate3d(1, 1, 1, 60deg)" ~expected:"rotate3d(1,1,1,60deg)";

  (* Scale functions *)
  check_transform "scale(2)";
  check_transform "scale(0.5)" ~expected:"scale(.5)";
  check_transform "scale(2, 3)" ~expected:"scale(2,3)";
  check_transform "scaleX(2)";
  check_transform "scaleY(0.5)" ~expected:"scaleY(.5)";
  check_transform "scaleZ(1.5)";
  check_transform "scale3d(2, 3, 4)" ~expected:"scale3d(2,3,4)";

  (* Skew functions *)
  check_transform "skew(30deg)";
  check_transform "skew(30deg, 45deg)" ~expected:"skew(30deg,45deg)";
  check_transform "skewX(45deg)";
  check_transform "skewY(30deg)";

  (* Matrix functions *)
  check_transform "matrix(1, 0, 0, 1, 0, 0)" ~expected:"matrix(1,0,0,1,0,0)";
  check_transform "matrix(0.866, 0.5, -0.5, 0.866, 0, 0)"
    ~expected:"matrix(.866,.5,-.5,.866,0,0)";
  check_transform "matrix3d(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)"
    ~expected:"matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)"

let test_gradients_direction_stop () =
  check_gradient_direction "to right";
  check_gradient_direction "45deg";
  check_gradient_stop "red"

let test_overscroll_aspect_content () =
  check_overscroll_behavior "auto";
  check_overscroll_behavior "contain";
  check_overscroll_behavior "none";
  check_overscroll_behavior "inherit";
  check_aspect_ratio "auto";
  check_aspect_ratio "inherit";
  (* content: bare id prints as quoted string, set expected accordingly *)
  check_content ~expected:"\"hello\"" "hello";
  check_content "none"

let test_grid_auto_flow () =
  check_grid_auto_flow "row";
  check_grid_auto_flow "column";
  check_grid_auto_flow "dense"

let test_grid_track_size () =
  (* Conservative inputs supported by current reader *)
  check_grid_track_size "auto";
  check_grid_track_size "10px"

(* Any-property value printing for representative properties *)
let test_pp_property_value_samples () =
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = Css.Properties.pp_property_value in
  let check_pp name expected prop value =
    let actual = to_s ppv (prop, value) in
    check string name expected actual
  in
  check_pp "width 10px" "10px" Css.Properties.Width (Css.Values.Px 10.);
  check string "color red" "red"
    (to_s ppv (Css.Properties.Color, Css.Values.Named Css.Values.Red));
  let imgs : Css.Properties.background_image list = [ Url "./x.png"; None ] in
  check string "background-image list" "url(./x.png),none"
    (to_s ppv (Css.Properties.Background_image, imgs));
  check string "transform none" "none"
    (to_s ppv
       (Css.Properties.Transform, ([ None ] : Css.Properties.transform list)));
  check string "content hello" "\"hello\""
    (to_s ppv
       (Css.Properties.Content, (String "hello" : Css.Properties.content)))

let test_negative_property_values () =
  let open Css.Reader in
  let neg reader s label =
    let r = of_string s in
    check bool label true (Option.is_none (try_parse reader r))
  in
  neg Css.Properties.read_align_items "diagonal" "align-items invalid";
  neg Css.Properties.read_justify_content "aroundish" "justify-content invalid";
  neg Css.Properties.read_grid_auto_flow "stack" "grid-auto-flow invalid";
  neg Css.Properties.read_aspect_ratio "1" "aspect-ratio missing /";
  neg Css.Properties.read_overscroll_behavior "bounce"
    "overscroll-behavior invalid";
  neg Css.Properties.read_background_size "bogus" "background-size invalid";
  neg Css.Properties.read_content_visibility "supervisible"
    "content-visibility invalid";
  neg Css.Properties.read_box_shadow "10px" "box-shadow missing v-offset";
  neg Css.Properties.read_transform "invalidfunc()" "transform invalid function"

let test_typed_positions () =
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = Css.Properties.pp_property_value in
  (* background-position: list *)
  check string "bg-pos center" "center"
    (to_s ppv (Css.Properties.Background_position, [ Css.Properties.Center ]));
  check string "bg-pos left, top" "left center,center top"
    (to_s ppv
       ( Css.Properties.Background_position,
         [ Css.Properties.pos_left; Css.Properties.pos_top ] ));
  (* object-position: single *)
  check string "object-pos bottom" "center bottom"
    (to_s ppv (Css.Properties.Object_position, Css.Properties.pos_bottom));
  (* transform-origin: 2D and 3D *)
  check string "origin left top" "left top"
    (to_s ppv
       ( Css.Properties.Transform_origin,
         Css.Properties.origin Css.Properties.Left Css.Properties.Top ));
  check string "origin3d right bottom 10px" "right bottom 10px"
    (to_s ppv
       ( Css.Properties.Transform_origin,
         Css.Properties.origin3d Css.Properties.Right Css.Properties.Bottom
           (Css.Values.Px 10.) ))

let tests =
  [
    test_case "display" `Quick test_display;
    test_case "position" `Quick test_position;
    test_case "overflow" `Quick test_overflow;
    test_case "border-style" `Quick test_border_style;
    test_case "visibility" `Quick test_visibility;
    test_case "z-index" `Quick test_z_index;
    test_case "flex-direction" `Quick test_flex_direction;
    test_case "flex-wrap" `Quick test_flex_wrap;
    test_case "align" `Quick test_align;
    test_case "align-self" `Quick test_align_self;
    test_case "justify" `Quick test_justify;
    test_case "font-style" `Quick test_font_style;
    test_case "text-align" `Quick test_text_align;
    test_case "text-decoration-style" `Quick test_text_decoration_style;
    test_case "text-overflow" `Quick test_text_overflow;
    test_case "text-wrap" `Quick test_text_wrap;
    test_case "white-space" `Quick test_white_space;
    test_case "word-break" `Quick test_word_break;
    test_case "overflow-wrap" `Quick test_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
    test_case "line-height" `Quick test_line_height;
    test_case "list-style" `Quick test_list_style;
    test_case "table-layout" `Quick test_table_layout;
    test_case "border-collapse" `Quick test_border_collapse;
    test_case "user/pointer" `Quick test_user_pointer;
    test_case "touch/resize/box-sizing" `Quick test_touch_resize_box_sizing;
    test_case "object/content-visibility" `Quick
      test_object_and_content_visibility;
    test_case "container/contain" `Quick test_container_and_contain;
    test_case "isolation/scroll" `Quick test_isolation_and_scroll;
    test_case "scroll-snap" `Quick test_scroll_snap;
    test_case "svg+direction+writing" `Quick test_svg_direction_writing;
    test_case "vendor/misc" `Quick test_vendor_misc;
    test_case "clear/float/vertical/outline" `Quick
      test_clear_float_vertical_outline;
    test_case "fonts/misc/effects" `Quick test_fonts_misc_effects;
    test_case "shadows/filters/background" `Quick
      test_shadows_filters_background;
    test_case "property names" `Quick test_property_names;
    test_case "read_property/pp_property roundtrip" `Quick
      test_property_read_pp_roundtrip;
    (* Negative property tokens via try_parse to assert failure behavior *)
    test_case "negative property values" `Quick test_negative_property_values;
    test_case "pp_property_value samples" `Quick test_pp_property_value_samples;
    test_case "typed positions helpers" `Quick test_typed_positions;
    (* Additional coverage for missing readers *)
    test_case "grid auto-flow" `Quick test_grid_auto_flow;
    test_case "grid track-size" `Quick test_grid_track_size;
    test_case "grid template+line" `Quick test_grid_template_line;
    test_case "place/align/justify/flex" `Quick test_place_align_justify_flex;
    test_case "transform" `Quick test_transform;
    test_case "gradients (direction/stop)" `Quick test_gradients_direction_stop;
    test_case "overscroll/aspect/content" `Quick test_overscroll_aspect_content;
  ]

let suite = [ ("properties", tests) ]
