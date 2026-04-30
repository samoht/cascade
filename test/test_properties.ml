open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers

let check_display =
  check_value_cursor "display" read_display pp_display ~roundtrip:true

let check_position =
  check_value_cursor "position" read_position pp_position ~roundtrip:true

let check_overflow =
  check_value_cursor "overflow" read_overflow pp_overflow ~roundtrip:true

let check_border_style =
  check_value_cursor "border-style" read_border_style pp_border_style
    ~roundtrip:true

let check_border =
  check_value_cursor "border" read_border pp_border ~roundtrip:true

let check_visibility =
  check_value_cursor "visibility" read_visibility pp_visibility ~roundtrip:true

let check_z_index =
  check_value_cursor "z-index" read_z_index pp_z_index ~roundtrip:true

let check_flex_direction =
  check_value_cursor "flex-direction" read_flex_direction pp_flex_direction
    ~roundtrip:true

let check_flex_wrap = check_value_cursor "flex-wrap" read_flex_wrap pp_flex_wrap

let check_align_self =
  check_value_cursor "align-self" read_align_self pp_align_self

let check_font_style =
  check_value_cursor "font-style" read_font_style pp_font_style

let check_font_display =
  check_value_cursor "font-display" read_font_display pp_font_display

let check_unicode_range =
  check_value_cursor "unicode-range" read_unicode_range pp_unicode_range

let check_text_align =
  check_value_cursor "text-align" read_text_align pp_text_align

let check_text_decoration_style =
  check_value_cursor "text-decoration-style" read_text_decoration_style
    pp_text_decoration_style

let check_text_overflow =
  check_value_cursor "text-overflow" read_text_overflow pp_text_overflow

let check_text_wrap = check_value_cursor "text-wrap" read_text_wrap pp_text_wrap

let check_white_space =
  check_value_cursor "white-space" read_white_space pp_white_space

let check_word_break =
  check_value_cursor "word-break" read_word_break pp_word_break

let check_text_decoration_shorthand =
  check_value_cursor "text_decoration_shorthand" read_text_decoration_shorthand
    pp_text_decoration_shorthand

let check_justify_self =
  check_value_cursor "justify_self" read_justify_self pp_justify_self

let check_align_content =
  check_value_cursor "align_content" read_align_content pp_align_content

let check_border_shorthand =
  check_value_cursor "border_shorthand" read_border_shorthand
    pp_border_shorthand

let check_justify_items =
  check_value_cursor "justify_items" read_justify_items pp_justify_items

let check_transition_shorthand =
  check_value_cursor "transition_shorthand" read_transition_shorthand
    pp_transition_shorthand

let check_flex_basis =
  check_value_cursor "flex_basis" read_flex_basis pp_flex_basis

let check_background_shorthand =
  check_value_cursor "background_shorthand" read_background_shorthand
    pp_background_shorthand

let check_animation_shorthand =
  check_value_cursor "animation_shorthand" read_animation_shorthand
    pp_animation_shorthand

let check_text_decoration_line =
  check_value_cursor "text_decoration_line" read_text_decoration_line
    pp_text_decoration_line

let check_text_size_adjust =
  check_value_cursor "text_size_adjust" read_text_size_adjust
    pp_text_size_adjust

let check_any_property =
  check_value_cursor "any_property" read_any_property pp_any_property

let check_overflow_wrap =
  check_value_cursor "overflow-wrap" read_overflow_wrap pp_overflow_wrap

let check_hyphens = check_value_cursor "hyphens" read_hyphens pp_hyphens

let check_line_height =
  check_value_cursor "line-height" read_line_height pp_line_height

let check_list_style_type =
  check_value_cursor "list-style-type" read_list_style_type pp_list_style_type

let check_list_style_position =
  check_value_cursor "list-style-position" read_list_style_position
    pp_list_style_position

let check_list_style_image =
  check_value_cursor "list-style-image" read_list_style_image
    pp_list_style_image

let check_table_layout =
  check_value_cursor "table-layout" read_table_layout pp_table_layout

let check_border_collapse =
  check_value_cursor "border-collapse" read_border_collapse pp_border_collapse

let check_user_select =
  check_value_cursor "user-select" read_user_select pp_user_select

let check_pointer_events =
  check_value_cursor "pointer-events" read_pointer_events pp_pointer_events

let check_touch_action =
  check_value_cursor "touch-action" read_touch_action pp_touch_action

let check_resize = check_value_cursor "resize" read_resize pp_resize

let check_box_sizing =
  check_value_cursor "box-sizing" read_box_sizing pp_box_sizing

let check_object_fit =
  check_value_cursor "object-fit" read_object_fit pp_object_fit

let check_content_visibility =
  check_value_cursor "content-visibility" read_content_visibility
    pp_content_visibility

let check_container_type =
  check_value_cursor "container-type" read_container_type pp_container_type

let check_container_shorthand ?expected input =
  let expected = Option.value ~default:input expected in
  let c = Css.Cursor.of_string input in
  let value = read_container_shorthand c in
  let serialized = Css.Pp.to_string ~minify:true pp_container_shorthand value in
  Alcotest.(check string) (Fmt.str "container %s" input) expected serialized;
  let expected_value =
    read_container_shorthand (Css.Cursor.of_string expected)
  in
  Alcotest.(check bool)
    (Fmt.str "container structural expected %s" input)
    true (value = expected_value);
  let reparsed = read_container_shorthand (Css.Cursor.of_string serialized) in
  Alcotest.(check bool)
    (Fmt.str "container structural roundtrip %s" input)
    true (value = reparsed)

let check_contain = check_value_cursor "contain" read_contain pp_contain
let check_isolation = check_value_cursor "isolation" read_isolation pp_isolation

let check_scroll_behavior =
  check_value_cursor "scroll-behavior" read_scroll_behavior pp_scroll_behavior

let check_scroll_snap_align =
  check_value_cursor "scroll-snap-align" read_scroll_snap_align
    pp_scroll_snap_align

let check_scroll_snap_stop =
  check_value_cursor "scroll-snap-stop" read_scroll_snap_stop
    pp_scroll_snap_stop

let check_scroll_snap_type =
  check_value_cursor "scroll-snap-type" read_scroll_snap_type
    pp_scroll_snap_type

let check_svg_paint = check_value_cursor "svg-paint" read_svg_paint pp_svg_paint
let check_direction = check_value_cursor "direction" read_direction pp_direction

let check_unicode_bidi =
  check_value_cursor "unicode-bidi" read_unicode_bidi pp_unicode_bidi

let check_writing_mode =
  check_value_cursor "writing-mode" read_writing_mode pp_writing_mode

let check_webkit_appearance =
  check_value_cursor "-webkit-appearance" read_webkit_appearance
    pp_webkit_appearance

let check_webkit_font_smoothing =
  check_value_cursor "-webkit-font-smoothing" read_webkit_font_smoothing
    pp_webkit_font_smoothing

let check_moz_osx_font_smoothing =
  check_value_cursor "-moz-osx-font-smoothing" read_moz_osx_font_smoothing
    pp_moz_osx_font_smoothing

let check_webkit_box_orient =
  check_value_cursor "-webkit-box-orient" read_webkit_box_orient
    pp_webkit_box_orient

let check_moz_orient =
  check_value_cursor "-moz-orient" read_moz_orient pp_moz_orient

let check_forced_color_adjust =
  check_value_cursor "forced-color-adjust" read_forced_color_adjust
    pp_forced_color_adjust

let check_print_color_adjust =
  check_value_cursor "print-color-adjust" read_print_color_adjust
    pp_print_color_adjust

let check_appearance =
  check_value_cursor "appearance" read_appearance pp_appearance

let check_clear = check_value_cursor "clear" read_clear pp_clear
let check_float_side = check_value_cursor "float" read_float_side pp_float_side

let check_text_decoration_skip_ink =
  check_value_cursor "text-decoration-skip-ink" read_text_decoration_skip_ink
    pp_text_decoration_skip_ink

let check_vertical_align =
  check_value_cursor "vertical-align" read_vertical_align pp_vertical_align

let check_outline_style =
  check_value_cursor "outline-style" read_outline_style pp_outline_style

let check_font_family =
  check_value_cursor "font-family" read_font_family pp_font_family

let check_font_stretch =
  check_value_cursor "font-stretch" read_font_stretch pp_font_stretch

let check_font_variant_numeric =
  check_value_cursor "font-variant-numeric" read_font_variant_numeric
    pp_font_variant_numeric

let check_font_feature_settings =
  check_value_cursor "font-feature-settings" read_font_feature_settings
    pp_font_feature_settings

let check_font_variation_settings =
  check_value_cursor "font-variation-settings" read_font_variation_settings
    pp_font_variation_settings

let check_backface_visibility =
  check_value_cursor "backface-visibility" read_backface_visibility
    pp_backface_visibility

let check_scale = check_value_cursor "scale" read_scale pp_scale

let check_background_box =
  check_value_cursor "background_box" read_background_box pp_background_box

let check_background =
  check_value_cursor "background" read_background pp_background

let check_steps_direction =
  check_value_cursor "steps-direction" read_steps_direction pp_steps_direction

let check_timing_function =
  check_value_cursor "timing-function" read_timing_function pp_timing_function

let check_transition_property_value =
  check_value_cursor "transition-property-value" read_transition_property_value
    pp_transition_property_value

let check_transition_property =
  check_value_cursor "transition-property" read_transition_property
    pp_transition_property

let check_transition_behavior =
  check_value_cursor "transition-behavior" read_transition_behavior
    pp_transition_behavior

let check_transition =
  check_value_cursor "transition" read_transition pp_transition

let check_animation_direction =
  check_value_cursor "animation-direction" read_animation_direction
    pp_animation_direction

let check_animation_fill_mode =
  check_value_cursor "animation-fill-mode" read_animation_fill_mode
    pp_animation_fill_mode

let check_animation_iteration_count =
  check_value_cursor "animation-iteration-count" read_animation_iteration_count
    pp_animation_iteration_count

let check_animation_play_state =
  check_value_cursor "animation-play-state" read_animation_play_state
    pp_animation_play_state

let check_animation = check_value_cursor "animation" read_animation pp_animation

let check_blend_mode =
  check_value_cursor "mix-blend-mode" read_blend_mode pp_blend_mode

let check_text_shadow =
  check_value_cursor "text-shadow" read_text_shadow pp_text_shadow

let check_shadow = check_value_cursor "shadow" read_shadow pp_shadow
let check_filter = check_value_cursor "filter" read_filter pp_filter

let check_background_attachment =
  check_value_cursor "background-attachment" read_background_attachment
    pp_background_attachment

let check_background_repeat =
  check_value_cursor "background-repeat" read_background_repeat
    pp_background_repeat

let check_background_size =
  check_value_cursor "background-size" read_background_size pp_background_size

let check_background_image =
  check_value_cursor "background-image" read_background_image
    pp_background_image

let check_background_position =
  check_value_cursor "background-position" read_background_position
    pp_background_position

let check_overscroll_behavior =
  check_value_cursor "overscroll-behavior" read_overscroll_behavior
    pp_overscroll_behavior

let check_aspect_ratio =
  check_value_cursor "aspect-ratio" read_aspect_ratio pp_aspect_ratio

let check_content = check_value_cursor "content" read_content pp_content

let check_grid_auto_flow =
  check_value_cursor "grid-auto-flow" read_grid_auto_flow pp_grid_auto_flow

let check_grid_template =
  check_value_cursor "grid-template" read_grid_template pp_grid_template

let check_grid_line = check_value_cursor "grid-line" read_grid_line pp_grid_line

let check_align_items =
  check_value_cursor "align-items" read_align_items pp_align_items

let check_justify_content =
  check_value_cursor "justify-content" read_justify_content pp_justify_content

let check_flex = check_value_cursor "flex" read_flex pp_flex

let check_place_items =
  check_value_cursor "place-items" read_place_items pp_place_items

let check_place_content =
  check_value_cursor "place-content" read_place_content pp_place_content

let check_transform = check_value_cursor "transform" read_transform pp_transform

let check_transforms =
  check_value_cursor "transforms" read_transforms pp_transforms

let check_gradient_direction =
  check_value_cursor "gradient-direction" read_gradient_direction
    pp_gradient_direction

let check_gradient_stop =
  check_value_cursor "gradient-stop" read_gradient_stop pp_gradient_stop

let check_radial_shape =
  check_value_cursor "radial_shape" read_radial_shape pp_radial_shape

let check_radial_size =
  check_value_cursor "radial_size" read_radial_size pp_radial_size

let check_radial_gradient_config =
  check_value_cursor "radial_gradient_config" read_radial_gradient_config
    pp_radial_gradient_config

let check_conic_gradient_config =
  check_value_cursor "conic_gradient_config" read_conic_gradient_config
    pp_conic_gradient_config

let check_color_interpolation =
  check_value_cursor "color_interpolation" read_color_interpolation
    pp_color_interpolation

let check_position_value =
  check_value_cursor "position_value" read_position_value pp_position_value

let check_translate_value =
  check_value_cursor "translate_value" read_translate_value pp_translate_value

let check_font_weight =
  check_value_cursor "font_weight" read_font_weight pp_font_weight

let check_cursor = check_value_cursor "cursor" read_cursor pp_cursor

let check_scroll_snap_axis =
  check_value_cursor "scroll_snap_axis" read_scroll_snap_axis
    pp_scroll_snap_axis

let check_scroll_snap_strictness =
  check_value_cursor "scroll_snap_strictness" read_scroll_snap_strictness
    pp_scroll_snap_strictness

let check_transform_style =
  check_value_cursor "transform_style" read_transform_style pp_transform_style

let check_font_variant_numeric_token =
  check_value_cursor "font_variant_numeric_token"
    read_font_variant_numeric_token pp_font_variant_numeric_token

let check_transform_origin =
  check_value_cursor "transform_origin" read_transform_origin
    pp_transform_origin

let check_gap = check_value_cursor "gap" read_gap pp_gap

let check_text_decoration =
  check_value_cursor "text_decoration" read_text_decoration pp_text_decoration

let check_border_width =
  check_value_cursor "border_width" read_border_width pp_border_width

let check_border_radius =
  check_value_cursor "border_radius" read_border_radius pp_border_radius

let check_text_transform =
  check_value_cursor "text_transform" read_text_transform pp_text_transform

let check_will_change =
  check_value_cursor "will_change" read_will_change pp_will_change

let check_clip = check_value_cursor "clip" read_clip pp_clip
let check_clip_path = check_value_cursor "clip_path" read_clip_path pp_clip_path

let check_perspective_origin =
  check_value_cursor "perspective_origin" read_perspective_origin
    pp_perspective_origin

let check_quotes = check_value_cursor "quotes" read_quotes pp_quotes
let check_outline = check_value_cursor "outline" read_outline pp_outline

let check_outline_shorthand =
  check_value_cursor "outline_shorthand" read_outline_shorthand
    pp_outline_shorthand

let check_css_wide = check_value_cursor "css_wide" read_css_wide pp_css_wide

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

let check_timeline_axis =
  check_value_cursor "timeline_axis" read_timeline_axis pp_timeline_axis

let check_timeline_shorthand =
  check_value_cursor "timeline_shorthand" read_timeline_shorthand
    pp_timeline_shorthand

let check_caption_side =
  check_value_cursor "caption_side" read_caption_side pp_caption_side

let check_color_scheme =
  check_value_cursor "color_scheme" read_color_scheme pp_color_scheme

let check_columns_value =
  check_value_cursor "columns_value" read_columns_value pp_columns_value

let check_field_sizing =
  check_value_cursor "field_sizing" read_field_sizing pp_field_sizing

let check_font_size = check_value_cursor "font_size" read_font_size pp_font_size
let check_mask_box = check_value_cursor "mask_box" read_mask_box pp_mask_box

let check_mask_composite =
  check_value_cursor "mask_composite" read_mask_composite pp_mask_composite

let check_mask_mode = check_value_cursor "mask_mode" read_mask_mode pp_mask_mode
let check_mask_type = check_value_cursor "mask_type" read_mask_type pp_mask_type
let check_opacity = check_value_cursor "opacity" read_opacity pp_opacity
let check_order = check_value_cursor "order" read_order pp_order

let check_rotate_value =
  check_value_cursor "rotate_value" read_rotate_value pp_rotate_value

let check_transform_box =
  check_value_cursor "transform_box" read_transform_box pp_transform_box

let check_webkit_line_clamp =
  check_value_cursor "webkit_line_clamp" read_webkit_line_clamp
    pp_webkit_line_clamp

let check_webkit_mask_composite =
  check_value_cursor "webkit_mask_composite" read_webkit_mask_composite
    pp_webkit_mask_composite

let check_webkit_mask_source_type =
  check_value_cursor "webkit_mask_source_type" read_webkit_mask_source_type
    pp_webkit_mask_source_type

(* Length-percentage tests for width/height using the value reader/printer *)

(* Helper for property-value pairs printing *)
let check_property_value expected (prop, value) =
  let pp = pp_property_value in
  let to_string f = Css.Pp.to_string ~minify:true f in
  let actual = to_string pp (prop, value) in
  let name = Fmt.str "%s value" (Css.Pp.to_string pp_property prop) in
  check string name expected actual

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
  (* Intentional legacy: accepted for compatibility in some engines *)
  check_display "-webkit-box";
  (* CSS-wide keyword supported by this reader *)
  check_display "unset";
  neg_cursor read_display "invalid-display";
  (* multiple values *)
  neg_cursor read_display "block inline";
  neg_cursor read_display "flex-";
  neg_cursor read_display "";
  neg_cursor read_display "123"

let test_position () =
  check_position "static";
  check_position "relative";
  check_position "absolute";
  check_position "fixed";
  check_position "sticky";
  neg_cursor read_position "invalid-position";
  (* multiple values *)
  neg_cursor read_position "absolute relative";
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
  neg_cursor read_border_style "solid dashed";
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
  check_border "0 solid black";
  (* Test with inherit/initial *)
  check_border "inherit";
  check_border "initial";
  neg_cursor read_border "invalid-border";
  (* multiple widths *)
  neg_cursor read_border "1px 2px";
  (* duplicate style *)
  neg_cursor read_border "solid solid";
  (* multiple colors *)
  neg_cursor read_border "red blue";
  (* too many values *)
  neg_cursor read_border "1px solid red blue"

let test_visibility () =
  check_visibility "visible";
  check_visibility "hidden";
  check_visibility "collapse";
  neg_cursor read_visibility "invalid-visibility";
  (* wrong keyword *)
  neg_cursor read_visibility "invisible";
  (* contradictory *)
  neg_cursor read_visibility "hidden visible";
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
  neg_cursor read_z_index "auto auto"

let test_flex_direction () =
  check_flex_direction "row";
  check_flex_direction "row-reverse";
  check_flex_direction "column";
  check_flex_direction "column-reverse";
  neg_cursor read_flex_direction "diagonal";
  (* multiple values *)
  neg_cursor read_flex_direction "row column";
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
  neg_cursor read_flex_wrap "wrap nowrap";
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
  neg_cursor read_align_self "flex-start flex-end";
  (* multiple *)
  check_align_self "start"

let test_font_style () =
  check_font_style "normal";
  check_font_style "italic";
  check_font_style "oblique";
  check_font_style "oblique 45deg";
  check_font_style "inherit";
  neg_cursor read_font_style "invalid";
  neg_cursor read_font_style "italics";
  (* common typo *)
  neg_cursor read_font_style "normal italic"

let test_font_display () =
  check_font_display "auto";
  check_font_display "block";
  check_font_display "swap";
  check_font_display "fallback";
  check_font_display "optional";
  neg_cursor read_font_display "invalid";
  neg_cursor read_font_display "inline";
  neg_cursor read_font_display "auto block"

let test_unicode_range () =
  (* Single code points per CSS spec *)
  check_unicode_range ~expected:"U+0" "U+0000";
  check_unicode_range ~expected:"U+26" "U+26";
  (* ampersand example from MDN *)
  check_unicode_range ~expected:"U+FF" "U+00FF";
  (* case insensitive *)
  check_unicode_range ~expected:"U+ABCD" "U+abcd";
  (* Code point ranges per CSS spec *)
  check_unicode_range ~expected:"U+0-FF" "U+0000-00FF";
  check_unicode_range ~expected:"U+25-FF" "U+0025-00FF";
  (* MDN example format *)
  check_unicode_range ~expected:"U+20-7F" "U+0020-007F";
  (* ASCII printable range *)
  check_unicode_range ~expected:"U+A0-A0FF" "U+A0-A0FF";
  neg_cursor read_unicode_range "invalid";
  neg_cursor read_unicode_range "U+";
  neg_cursor read_unicode_range "U+GGGG";
  neg_cursor read_unicode_range "U+1234-";
  neg_cursor read_unicode_range "U+1234-GGGG";
  neg_cursor read_unicode_range "1234";
  neg_cursor read_unicode_range "+1234";
  neg_cursor read_unicode_range "U1234";
  neg_cursor read_unicode_range "U+12345-1234"

let test_text_align () =
  check_text_align "left";
  check_text_align "right";
  check_text_align "center";
  check_text_align "justify";
  check_text_align "start";
  check_text_align "end";
  check_text_align "inherit";
  neg_cursor read_text_align "invalid-align";
  (* vertical align, not text align *)
  neg_cursor read_text_align "middle";
  (* contradictory *)
  neg_cursor read_text_align "left right";
  (* wrong form *)
  neg_cursor read_text_align "justified"

let test_text_decoration_style () =
  check_text_decoration_style "solid";
  check_text_decoration_style "double";
  check_text_decoration_style "dotted";
  check_text_decoration_style "dashed";
  check_text_decoration_style "wavy";
  check_text_decoration_style "inherit";
  neg_cursor read_text_decoration_style "invalid-style";
  (* multiple styles *)
  neg_cursor read_text_decoration_style "solid dotted";
  (* typo *)
  neg_cursor read_text_decoration_style "wavey";
  neg_cursor read_text_decoration_style "underline"

let test_text_overflow () =
  check_text_overflow "clip";
  check_text_overflow "ellipsis";
  check_text_overflow "clip ellipsis";
  check_text_overflow "inherit";
  neg_cursor read_text_overflow "invalid-overflow";
  neg_cursor read_text_overflow "hidden";
  (* literal ellipsis not valid *)
  neg_cursor read_text_overflow "...";
  (* CSS Overflow 4 text-overflow accepts at most two markers *)
  neg_cursor read_text_overflow "clip ellipsis clip"

let test_text_wrap () =
  check_text_wrap "wrap";
  check_text_wrap "nowrap";
  check_text_wrap "balance";
  check_text_wrap "pretty";
  check_text_wrap "inherit";
  neg_cursor read_text_wrap "invalid-wrap";
  (* contradictory *)
  neg_cursor read_text_wrap "wrap nowrap";
  (* wrong form *)
  neg_cursor read_text_wrap "no-wrap";
  neg_cursor read_text_wrap "balanced"

let test_white_space () =
  check_white_space "normal";
  check_white_space "nowrap";
  check_white_space "pre";
  check_white_space "pre-wrap";
  check_white_space "pre-line";
  check_white_space "break-spaces";
  check_white_space "inherit";
  neg_cursor read_white_space "invalid-space";
  (* hyphenated form incorrect *)
  neg_cursor read_white_space "no-wrap";
  (* contradictory *)
  neg_cursor read_white_space "normal nowrap";
  (* incomplete *)
  neg_cursor read_white_space "preserve"

let test_word_break () =
  check_word_break "normal";
  check_word_break "break-all";
  check_word_break "keep-all";
  (* Intentional legacy: word-break: break-word is non-standard, kept for
     compatibility; modern alternative is overflow-wrap:anywhere *)
  check_word_break "break-word";
  check_word_break "inherit";
  neg_cursor read_word_break "invalid-break";
  (* incomplete *)
  neg_cursor read_word_break "break";
  (* different property *)
  neg_cursor read_word_break "word-wrap";
  (* contradictory *)
  neg_cursor read_word_break "normal break-all"

let test_overflow_wrap () =
  check_overflow_wrap "normal";
  check_overflow_wrap "break-word";
  check_overflow_wrap "anywhere";
  check_overflow_wrap "inherit";
  neg_cursor read_overflow_wrap "invalid-wrap";
  neg_cursor read_overflow_wrap "normal break-word";
  (* contradictory *)
  neg_cursor read_overflow_wrap "breakword";
  (* missing hyphen *)
  (* not a valid value *)
  neg_cursor read_overflow_wrap "everywhere"

let test_hyphens () =
  check_hyphens "none";
  check_hyphens "manual";
  check_hyphens "auto";
  check_hyphens "inherit";
  neg_cursor read_hyphens "invalid-hyphens";
  neg_cursor read_hyphens "true";
  (* boolean not valid *)
  neg_cursor read_hyphens "auto manual";
  (* contradictory *)
  (* wrong form *)
  neg_cursor read_hyphens "hyphenate"

let test_line_height () =
  (* Only test values supported by the simplified reader *)
  check_line_height "normal";
  check_line_height "inherit";
  check_line_height "1.5";
  check_line_height "120%";
  neg_cursor read_line_height "invalid";
  neg_cursor read_line_height "-1.5";
  (* negative line-height *)
  (* multiple values *)
  neg_cursor read_line_height "normal 1.5"

let test_table_layout () =
  check_table_layout "auto";
  check_table_layout "fixed";
  check_table_layout "inherit";
  neg_cursor read_table_layout "invalid-layout";
  neg_cursor read_table_layout "auto fixed";
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
  neg_cursor read_border_collapse "collapse separate";
  (* both values *)
  neg_cursor read_border_collapse "collapsed";
  (* wrong form *)
  neg_cursor read_border_collapse "none"

(* Verifies property constructors map to correct CSS property names *)
(* Not a roundtrip test *)
let test_property_names () =
  let to_s : type a. a property -> string =
   fun prop -> Css.Pp.to_string pp_property prop
  in
  (* Test color properties *)
  check string "property name" "background-color" (to_s Background_color);
  check string "property name" "color" (to_s Color);
  check string "property name" "border-color" (to_s Border_color);
  check string "property name" "outline-color" (to_s Outline_color);
  (* Test border style property *)
  check string "property name" "border-style" (to_s Border_style);
  (* Test length properties *)
  check string "property name" "padding-left" (to_s Padding_left);
  check string "property name" "margin-top" (to_s Margin_top);
  check string "property name" "width" (to_s Width);
  check string "property name" "height" (to_s Height);
  check string "property name" "font-size" (to_s Font_size);
  check string "property name" "line-height" (to_s Line_height);
  (* Test other properties *)
  check string "property name" "display" (to_s Display);
  check string "property name" "position" (to_s Position);
  check string "property name" "visibility" (to_s Visibility);
  check string "property name" "z-index" (to_s Z_index);
  check string "property name" "transform" (to_s Transform);
  check string "property name" "cursor" (to_s Cursor)

(* Verifies property-value pairs print correctly *)
(* Not a roundtrip test *)
let test_pp_property_value () =
  check_property_value "10px" (Width, Css.Values.Length (Css.Values.Px 10.));
  check_property_value "red" (Color, Css.Values.Named Css.Values.Red);
  check_property_value "url(./x.png),none"
    (Background_image, [ Url "./x.png"; None ]);
  check_property_value "none" (Transform, [ None ]);
  check_property_value "\"hello\"" (Content, String "hello");
  (* Additional samples *)
  let to_s f = Css.Pp.to_string ~minify:true f in
  let ppv = pp_property_value in
  check string "color red" "red"
    (to_s ppv (Color, Css.Values.Named Css.Values.Red));
  let imgs : background_image list = [ Url "./x.png"; None ] in
  check string "background-image list" "url(./x.png),none"
    (to_s ppv (Background_image, imgs));
  check string "transform none" "none" (to_s ppv (Transform, [ None ]));
  check string "content hello" "\"hello\""
    (to_s ppv (Content, (String "hello" : content)))

let test_transform () =
  check_transform "none";
  check_transform "translateX(10px)";
  check_transform "translateX(-50%)";
  check_transform "translateY(2em)";
  check_transform "translateZ(100px)";
  check_transform "translate(10px)";
  check_transform "translate(10px, 20px)" ~expected:"translate(10px,20px)";
  check_transform "translate3d(10px, 20px, 30px)"
    ~expected:"translate3d(10px,20px,30px)";
  check_transform "rotate(45deg)";
  check_transform "rotate(0.5turn)" ~expected:"rotate(.5turn)";
  check_transform "rotate(3.14rad)";
  check_transform "rotateX(45deg)";
  check_transform "rotateY(90deg)";
  check_transform "rotateZ(180deg)";
  check_transform "rotate3d(1, 0, 0, 45deg)" ~expected:"rotate3d(1,0,0,45deg)";
  check_transform "rotate3d(0, 1, 0, 90deg)" ~expected:"rotate3d(0,1,0,90deg)";
  check_transform "rotate3d(1, 1, 1, 60deg)" ~expected:"rotate3d(1,1,1,60deg)";
  check_transform "scale(2)";
  check_transform "scale(0.5)" ~expected:"scale(.5)";
  check_transform "scale(2, 3)" ~expected:"scale(2,3)";
  check_transform "scaleX(2)";
  check_transform "scaleY(0.5)" ~expected:"scaleY(.5)";
  check_transform "scaleZ(1.5)";
  check_transform "scale3d(2, 3, 4)" ~expected:"scale3d(2,3,4)";
  check_transform "skew(30deg)";
  check_transform "skew(30deg, 45deg)" ~expected:"skew(30deg,45deg)";
  check_transform "skewX(45deg)";
  check_transform "skewY(30deg)";
  check_transform "matrix(1, 0, 0, 1, 0, 0)" ~expected:"matrix(1,0,0,1,0,0)";
  check_transform "matrix(0.866, 0.5, -0.5, 0.866, 0, 0)"
    ~expected:"matrix(.866,.5,-.5,.866,0,0)";
  check_transform "matrix3d(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)"
    ~expected:"matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)";
  (* var() tests - single transform functions can be variables *)
  check_transform "var(--my-transform)";
  check_transform "var(--my-transform,none)";
  check_transform "var(--tw-rotate-x,)";
  check_transform "var(--tw-scale,scale(1))";
  neg_cursor read_transform "invalidfunc()";
  neg_cursor read_transform "translate3d(10px,20px)";
  neg_cursor read_transform "scale3d(1,2)";
  neg_cursor read_transform "matrix(1,2,3,4,5)";
  neg_cursor read_transform "matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0)"

let test_transforms () =
  (* Test transforms (transform list) with multiple values and vars *)
  (* Tailwind concatenates adjacent var() transform functions without spaces *)
  check_transforms ~expected:"var(--tw-rotate-x,)var(--tw-rotate-y,)"
    "var(--tw-rotate-x,) var(--tw-rotate-y,)";
  check_transforms
    ~expected:"var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)";
  check_transforms
    ~expected:
      "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)var(--tw-skew-x,)var(--tw-skew-y,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)var(--tw-skew-x,)var(--tw-skew-y,)";
  check_transforms "translateX(10px) var(--my-rotate)";
  check_transforms "var(--translate) scale(2)";
  check_transforms "rotate(45deg) scale(1.5)";
  check_transforms "none";
  (* Test transforms variable reference (whole list as var) *)
  check_transforms "var(--my-transforms)";
  check_transforms "var(--my-transforms,none)";
  neg_cursor read_transforms "invalidfunc()";
  neg_cursor read_transforms "translate3d(10px,20px) scale(2)"

let test_gap () =
  check_gap "10px";
  check_gap "1rem 2rem";
  check_gap ~expected:"0" "0px";
  neg_cursor read_gap "invalid-gap";
  neg_cursor read_gap "-10px";
  (* negative gap *)
  neg_cursor read_gap "10px 20px 30px";
  (* too many values *)
  (* not a valid gap value *)
  neg_cursor read_gap "auto"

let test_font_variant_numeric_token () =
  check_font_variant_numeric_token "normal";
  check_font_variant_numeric_token "ordinal";
  check_font_variant_numeric_token "slashed-zero";
  neg_cursor read_font_variant_numeric_token "invalid-token";
  neg_cursor read_font_variant_numeric_token "normal ordinal";
  (* contradictory *)
  neg_cursor read_font_variant_numeric_token "slashed-zeros";
  (* wrong form *)
  (* not a valid value *)
  neg_cursor read_font_variant_numeric_token "diagonal-zero"

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

let test_background_box () =
  check_background_box "border-box";
  check_background_box "padding-box";
  check_background_box "content-box";
  neg_cursor read_background_box "invalid-box";
  neg_cursor read_background_box "margin-box";
  (* doesn't exist for background *)
  neg_cursor read_background_box "border-box padding-box";
  (* multiple *)
  neg_cursor read_background_box "borderbox";
  (* missing hyphen *)
  (* SVG value, not background *)
  neg_cursor read_background_box "fill-box"

let test_background () =
  check_background "red";
  check_background "url(image.png)";
  check_background ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  check_background ~expected:"url(image.png) center/cover no-repeat fixed red"
    "red url(image.png) center/cover no-repeat fixed";
  check_background "none";
  neg_cursor read_background "invalid-background";
  neg_cursor read_background "red blue";
  (* multiple colors without gradient *)
  (* Per CSS Images Module, [url()] with an empty URL is spec-valid. *)
  neg_cursor read_background "center center center";
  (* too many positions *)
  (* invalid position syntax *)
  neg_cursor read_background "10px 20px 30px 40px 50px"

let test_font_weight () =
  check_font_weight "normal";
  check_font_weight "bold";
  check_font_weight "700";
  check_font_weight "1000";
  check_font_weight "lighter";
  neg_cursor read_font_weight "invalid-weight";
  neg_cursor read_font_weight "1001";
  (* out of range *)
  neg_cursor read_font_weight "normal bold";
  (* multiple values *)
  (* not a valid keyword *)
  neg_cursor read_font_weight "extra-bold"

let test_text_transform () =
  check_text_transform "uppercase";
  check_text_transform "lowercase";
  check_text_transform "capitalize";
  check_text_transform "none";
  neg_cursor read_text_transform "invalid-transform";
  neg_cursor read_text_transform "uppercase lowercase";
  (* contradictory *)
  neg_cursor read_text_transform "upper-case";
  (* wrong form *)
  (* not a valid value *)
  neg_cursor read_text_transform "title-case"

let test_text_decoration_line () =
  check_text_decoration_line "underline";
  check_text_decoration_line "overline";
  check_text_decoration_line "line-through";
  neg_cursor read_text_decoration_line "invalid-line";
  neg_cursor read_text_decoration_line "strikethrough";
  (* wrong name *)
  neg_cursor read_text_decoration_line "underline overline underline";
  (* duplicate *)
  (* that's a style, not a line *)
  neg_cursor read_text_decoration_line "wavy"

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
  neg_cursor read_cursor "invalid-cursor";
  neg_cursor read_cursor "url(cursor.cur)";
  (* missing fallback *)
  neg_cursor read_cursor "pointer default";
  (* multiple keywords without url *)
  (* negative hotspot *)
  neg_cursor read_cursor "url(cursor.cur) -1 -1, pointer"

let test_border_width () =
  check_border_width "thin";
  check_border_width "medium";
  check_border_width "thick";
  check_border_width "2px";
  neg_cursor read_border_width "invalid-width";
  neg_cursor read_border_width "-2px";
  (* negative width *)
  neg_cursor read_border_width "thick thin";
  (* multiple values *)
  neg_cursor read_border_width "2";
  (* missing unit *)
  (* not a valid keyword *)
  neg_cursor read_border_width "heavy"

let test_border_radius () =
  check_border_radius "1px";
  check_border_radius "1px 2px";
  check_border_radius "1px 2px 3px 4px";
  check_border_radius "1px / 2px";
  check_border_radius "1px 2px / 3px 4px";
  neg_cursor read_border_radius "";
  neg_cursor read_border_radius "1px /";
  neg_cursor read_border_radius "-1px"

let test_text_decoration () =
  check_text_decoration "underline";
  check_text_decoration "line-through";
  check_text_decoration ~expected:"none" "none";
  neg_cursor read_text_decoration "invalid-decoration";
  neg_cursor read_text_decoration "underline line-through underline";
  (* duplicate - per CSS spec, || combinator means each component at most
     once *)
  neg_cursor read_text_decoration "solid";
  (* that's a style *)
  (* that's a color *)
  neg_cursor read_text_decoration "red"

let test_text_decoration_shorthand () =
  (* Test individual parts *)
  check_text_decoration_shorthand "underline";
  check_text_decoration_shorthand "underline solid";
  check_text_decoration_shorthand "underline solid red";
  check_text_decoration_shorthand "underline solid red 2px";
  (* Test multiple lines *)
  check_text_decoration_shorthand ~expected:"underline overline"
    "underline overline";
  check_text_decoration_shorthand ~expected:"underline overline dashed"
    "underline overline dashed";
  (* Test order independence *)
  check_text_decoration_shorthand ~expected:"underline solid red"
    "red solid underline";
  check_text_decoration_shorthand ~expected:"underline wavy blue 3px"
    "3px wavy blue underline";
  neg_cursor read_text_decoration_shorthand "invalid-decoration";
  neg_cursor read_text_decoration_shorthand "underline underline";
  (* duplicate line *)
  neg_cursor read_text_decoration_shorthand "solid solid";
  (* duplicate style *)
  (* multiple colors *)
  neg_cursor read_text_decoration_shorthand "red blue"

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

let test_border_shorthand () =
  check_border_shorthand "1px";
  check_border_shorthand "solid";
  check_border_shorthand "red";
  check_border_shorthand "1px solid";
  check_border_shorthand "1px solid red";
  check_border_shorthand ~expected:"solid red" "red solid";
  check_border_shorthand ~expected:"2px dashed blue" "blue 2px dashed";
  neg_cursor read_border_shorthand "1px 2px"

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
  neg_cursor read_justify_items "left right";
  (* contradictory *)
  neg_cursor read_justify_items "unsafe unsafe";
  (* duplicate modifier *)
  (* not a valid value *)
  neg_cursor read_justify_items "middle"

let test_transition_shorthand () =
  check_transition_shorthand "all";
  check_transition_shorthand "opacity 1s";
  check_transition_shorthand "opacity 1s ease-in";
  check_transition_shorthand ~expected:"opacity 1s ease-in .5s"
    "opacity 1s ease-in 0.5s";
  check_transition_shorthand "width 2s";
  check_transition_shorthand ~expected:"all .3s linear" "all 0.3s linear";
  neg_cursor read_transition_shorthand "2invalid";
  neg_cursor read_transition_shorthand "-1s";
  (* negative duration *)
  neg_cursor read_transition_shorthand "opacity";
  (* missing duration *)
  (* too many durations *)
  neg_cursor read_transition_shorthand "1s 2s 3s 4s"

let test_flex_basis () =
  check_flex_basis "auto";
  check_flex_basis "content";
  check_flex_basis "0";
  check_flex_basis "100px";
  check_flex_basis "50%";
  check_flex_basis "inherit";
  neg_cursor read_flex_basis "invalid";
  neg_cursor read_flex_basis "-100px"

let test_background_shorthand () =
  check_background_shorthand "red";
  check_background_shorthand "url(image.png)";
  check_background_shorthand "center";
  check_background_shorthand "no-repeat";
  check_background_shorthand "repeat repeat";
  check_background_shorthand ~expected:"url(image.png) red" "red url(image.png)";
  check_background_shorthand ~expected:"url(image.png) center"
    "url(image.png) center";
  check_background_shorthand ~expected:"url(image.png) center no-repeat red"
    "red url(image.png) center no-repeat";
  neg_cursor read_background_shorthand "invalid invalid";
  neg_cursor read_background_shorthand "red blue green";
  (* multiple colors *)
  (* incomplete size syntax *)
  neg_cursor read_background_shorthand "center/";
  (* a single background layer accepts only one repeat-style *)
  neg_cursor read_background_shorthand "repeat repeat repeat"

let test_animation_shorthand () =
  (* Valid CSS per spec - all components optional *)
  (* duration only, name defaults to none *)
  check_animation_shorthand "1s";
  (* name only, duration defaults to 0s *)
  check_animation_shorthand "slide";
  check_animation_shorthand "slide 1s";
  check_animation_shorthand "slide 1s ease-in";
  check_animation_shorthand ~expected:"slide 1s ease-in .5s infinite"
    "slide 1s ease-in 0.5s infinite";
  check_animation_shorthand "slide 1s infinite";
  check_animation_shorthand "slide 1s reverse";
  check_animation_shorthand "slide 1s forwards";
  check_animation_shorthand "slide 1s paused";

  (* Invalid cases *)
  (* invalid time unit *)
  neg_cursor read_animation_shorthand "2invalid";
  (* negative duration *)
  neg_cursor read_animation_shorthand "slide -1s";
  (* too many time values *)
  neg_cursor read_animation_shorthand "slide 1s 2s 3s 4s 5s"

let test_any_property () =
  check_any_property "display";
  check_any_property "position";
  check_any_property "color";
  check_any_property "width";
  check_any_property "margin";
  check_any_property "padding";
  check_any_property "font-size";
  neg_cursor read_any_property "not-a-property";
  neg_cursor read_any_property "font size";
  (* space instead of hyphen *)
  neg_cursor read_any_property "_private";
  (* invalid start *)
  neg_cursor read_any_property "123-prop";
  (* starts with number *)
  (* empty string *)
  neg_cursor read_any_property ""

let test_list_style_type () =
  check_list_style_type "none";
  check_list_style_type "disc";
  check_list_style_type "circle";
  check_list_style_type "square";
  check_list_style_type "decimal";
  check_list_style_type "lower-alpha";
  check_list_style_type "upper-alpha";
  check_list_style_type "lower-roman";
  check_list_style_type "upper-roman";
  neg_cursor read_list_style_type "invalid-style"

let test_list_style_position () =
  check_list_style_position "inside";
  check_list_style_position "outside";
  check_list_style_position "inherit";
  neg_cursor read_list_style_position "middle"

let test_list_style_image () =
  check_list_style_image "none";
  check_list_style_image "inherit";
  check_list_style_image "url(https://example.com/x.png)";
  neg_cursor read_list_style_image "invalid-url"

let test_vertical_align () =
  check_vertical_align "baseline";
  check_vertical_align "top";
  check_vertical_align "middle";
  check_vertical_align "bottom";
  check_vertical_align "text-top";
  check_vertical_align "text-bottom";
  check_vertical_align "sub";
  check_vertical_align "super";
  check_vertical_align "inherit";
  neg_cursor read_vertical_align "invalid-align"

let test_font_family () =
  check_font_family "sans-serif";
  check_font_family "serif";
  check_font_family "monospace";
  check_font_family "cursive";
  check_font_family "fantasy";
  check_font_family "system-ui";
  (* Per CSS spec, arbitrary font family names are valid (both quoted and
     unquoted identifiers) *)
  check_font_family "invalid-font";
  check_font_family "\"Times New Roman\"";
  check_font_family "Arial";
  (* Test actual invalid cases *)
  neg_cursor read_font_family "123invalid";
  (* identifier can't start with number *)
  neg_cursor read_font_family ""

let test_font_stretch () =
  check_font_stretch "normal";
  check_font_stretch "50%";
  check_font_stretch "ultra-condensed";
  check_font_stretch "ultra-expanded";
  check_font_stretch "inherit";
  neg_cursor read_font_stretch "invalid-stretch"

let test_font_variant_numeric () =
  check_font_variant_numeric "normal";
  check_font_variant_numeric "lining-nums";
  check_font_variant_numeric "tabular-nums";
  neg_cursor read_font_variant_numeric "invalid-variant"

let test_font_feature_settings () =
  check_font_feature_settings "normal";
  check_font_feature_settings "inherit";
  check_font_feature_settings "\"kern\"";
  check_font_feature_settings "\"liga\" 0";
  neg_cursor read_font_feature_settings "invalid-feature"

let test_font_variation_settings () =
  check_font_variation_settings "normal";
  check_font_variation_settings "inherit";
  check_font_variation_settings "\"wght\" 400";
  neg_cursor read_font_variation_settings "invalid-variation"

let test_transform_style () =
  check_transform_style "flat";
  check_transform_style "preserve-3d";
  check_transform_style "inherit";
  neg_cursor read_transform_style "invalid-style"

let test_backface_visibility () =
  check_backface_visibility "visible";
  check_backface_visibility "hidden";
  check_backface_visibility "inherit";
  neg_cursor read_backface_visibility "invalid-visibility"

let test_scale () =
  check_scale "none";
  check_scale "1";
  check_scale ~expected:".5" "0.5";
  check_scale ~expected:"1.5 2" "1.5 2.0";
  check_scale ~expected:".8 .8 1.2" "0.8 0.8 1.2";
  (* var() tests - when all values are vars, they're concatenated without
     spaces *)
  check_scale "var(--my-scale)";
  check_scale "var(--my-scale,1)";
  check_scale "var(--my-scale-x) var(--my-scale-y)"
    ~expected:"var(--my-scale-x)var(--my-scale-y)";
  check_scale "var(--tw-scale-x)var(--tw-scale-y)";
  check_scale "var(--tw-scale-x,1)var(--tw-scale-y,1)";
  check_scale "var(--x,) var(--y,)" ~expected:"var(--x,)var(--y,)";
  check_scale "var(--x,1) var(--y,1) var(--z,1)"
    ~expected:"var(--x,1)var(--y,1)var(--z,1)";
  neg_cursor read_scale "2scale"

let test_steps_direction () =
  check_steps_direction "jump-start";
  check_steps_direction "jump-end";
  check_steps_direction "jump-none";
  check_steps_direction "jump-both";
  check_steps_direction "start";
  check_steps_direction "end";
  neg_cursor read_steps_direction "invalid-direction";
  neg_cursor read_steps_direction "jump";
  neg_cursor read_steps_direction "middle"

let test_timing_function () =
  check_timing_function "ease";
  check_timing_function "linear";
  check_timing_function "ease-in";
  check_timing_function "ease-out";
  check_timing_function "ease-in-out";
  check_timing_function "step-start";
  check_timing_function "step-end";
  check_timing_function ~expected:"cubic-bezier(.1,.7,1,.1)"
    "cubic-bezier(0.1, 0.7, 1.0, 0.1)";
  neg_cursor read_timing_function "cubic-bezier()"

let test_transition_property_value () =
  check_transition_property_value "all";
  check_transition_property_value "none";
  check_transition_property_value "opacity";
  check_transition_property_value "transform";
  (* Arbitrary identifier should be accepted (inert if non-animatable) *)
  check_transition_property_value "invalid-transition";
  neg_cursor read_transition_property_value ""

let test_transition_property () =
  check_transition_property "all";
  check_transition_property "none";
  check_transition_property "opacity";
  check_transition_property "transform";
  (* Arbitrary identifier should be accepted (inert if non-animatable) *)
  check_transition_property "invalid-transition";
  neg_cursor read_transition_property ""

let test_transition_behavior () =
  check_transition_behavior "normal";
  check_transition_behavior "allow-discrete";
  check_transition_behavior "inherit";
  neg_cursor read_transition_behavior "invalid";
  neg_cursor read_transition_behavior ""

let test_transition () =
  check_transition "inherit";
  check_transition "initial";
  check_transition "none";
  neg_cursor read_transition "2invalid"

let test_animation_direction () =
  check_animation_direction "normal";
  check_animation_direction "reverse";
  check_animation_direction "alternate";
  check_animation_direction "alternate-reverse";
  neg_cursor read_animation_direction "invalid-direction"

let test_animation_fill_mode () =
  check_animation_fill_mode "none";
  check_animation_fill_mode "forwards";
  check_animation_fill_mode "backwards";
  check_animation_fill_mode "both";
  neg_cursor read_animation_fill_mode "invalid-fill"

let test_animation_iteration_count () =
  check_animation_iteration_count "1";
  check_animation_iteration_count "infinite";
  check_animation_iteration_count "2.5";
  neg_cursor read_animation_iteration_count "invalid-count"

let test_animation_play_state () =
  check_animation_play_state "running";
  check_animation_play_state "paused";
  neg_cursor read_animation_play_state "invalid-state"

let test_animation () =
  check_animation "inherit";
  check_animation "initial";
  check_animation "none";
  check_animation "slide-in 1s";
  check_animation "my-animation 2s ease-in 1s infinite alternate";
  (* Test invalid animation shorthand according to CSS spec *)
  neg_cursor read_animation "1s 2s 3s";
  (* More than 2 time values *)
  neg_cursor read_animation "-2s";
  (* Negative duration is invalid *)
  neg_cursor read_animation "2s -1" (* Negative iteration count is invalid *)

let test_blend_mode () =
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
  check_blend_mode "luminosity";
  neg_cursor read_blend_mode "invalid-blend"

let test_background_attachment () =
  check_background_attachment "scroll";
  check_background_attachment "fixed";
  check_background_attachment "local";
  check_background_attachment "inherit";
  neg_cursor read_background_attachment "invalid-attachment"

let test_background_repeat () =
  check_background_repeat "repeat";
  check_background_repeat "space";
  check_background_repeat "round";
  check_background_repeat "no-repeat";
  check_background_repeat "repeat-x";
  check_background_repeat "repeat-y";
  check_background_repeat "inherit";
  neg_cursor read_background_repeat "invalid-repeat"

let test_background_size () =
  check_background_size "auto";
  check_background_size "cover";
  check_background_size "contain";
  check_background_size "50px";
  check_background_size "50%";
  check_background_size "inherit";
  neg_cursor read_background_size "invalid-size"

let test_gradient_direction () =
  check_gradient_direction "to top";
  check_gradient_direction "to right";
  check_gradient_direction "to bottom";
  check_gradient_direction "to left";
  neg_cursor read_gradient_direction "invalid-direction"

let test_gradient_stop () =
  (* Basic color stops *)
  check_gradient_stop "red";
  check_gradient_stop ~expected:"blue50%" "blue 50%";
  check_gradient_stop ~expected:"#ff573325%" "#ff5733 25%";
  check_gradient_stop ~expected:"rgb(255 0 0)10px" "rgb(255,0,0) 10px";

  (* Double position stops *)
  check_gradient_stop ~expected:"green20%40%" "green 20% 40%";

  (* Hint positions *)
  check_gradient_stop "50%";
  check_gradient_stop "10px";

  (* CSS variables in gradient stops *)
  check_gradient_stop "var(--tw-gradient-from)";
  check_gradient_stop ~expected:"var(--color-blue-500)50%"
    "var(--color-blue-500) 50%";

  (* Complex var with fallback *)
  check_gradient_stop
    ~expected:"var(--tw-gradient-from)var(--tw-gradient-from-position)"
    "var(--tw-gradient-from) var(--tw-gradient-from-position)";

  (* Multiple vars in sequence (as used in Tailwind gradients) *)
  check_gradient_stop ~expected:"var(--tw-gradient-position)"
    "var(--tw-gradient-position)";

  (* Nested var with complex fallback *)
  check_gradient_stop
    ~expected:
      "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))"
    "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
     var(--tw-gradient-from) var(--tw-gradient-from-position), \
     var(--tw-gradient-to) var(--tw-gradient-to-position))";

  (* Invalid stops *)
  neg_cursor read_gradient_stop "invalid-stop";
  neg_cursor read_gradient_stop "red blue";
  (* Two colors without position *)
  neg_cursor read_gradient_stop "50% 25%" (* Two percentages without color *)

let test_color_interpolation () =
  check_color_interpolation "in oklab";
  check_color_interpolation "in oklch";
  check_color_interpolation "in srgb";
  check_color_interpolation "in hsl";
  check_color_interpolation "in lab";
  check_color_interpolation "in lch";
  neg_cursor read_color_interpolation "oklab";
  neg_cursor read_color_interpolation "in unknown";
  neg_cursor read_color_interpolation "in"

let test_background_image () =
  check_background_image "none";
  check_background_image "url(image.jpg)";
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  neg_cursor read_background_image "invalid-image"

let test_radial_shape () =
  check_radial_shape "circle";
  check_radial_shape "ellipse";
  neg_cursor read_radial_shape "square"

let test_radial_size () =
  check_radial_size "closest-side";
  check_radial_size "farthest-side";
  check_radial_size "closest-corner";
  check_radial_size "farthest-corner";
  check_radial_size "10px";
  check_radial_size ~expected:"50% 25%" "50% 25%";
  neg_cursor read_radial_size "invalid-size"

let test_radial_gradient_config () =
  check_radial_gradient_config "circle";
  check_radial_gradient_config "ellipse";
  check_radial_gradient_config "circle closest-side";
  check_radial_gradient_config ~expected:"circle at center" "circle at center";
  neg_cursor read_radial_gradient_config "invalid-config"

let test_conic_gradient_config () =
  check_conic_gradient_config "from 45deg";
  check_conic_gradient_config "at center";
  check_conic_gradient_config "from 90deg at left top";
  neg_cursor read_conic_gradient_config "";
  neg_cursor read_conic_gradient_config "center";
  neg_cursor read_conic_gradient_config "from"

let test_background_position () =
  check_background_position "center";
  check_background_position "left top";
  check_background_position "right .5rem center";
  check_background_position "50% 25%";
  check_background_position "inherit";
  neg_cursor read_background_position "invalid-position"

let test_position_value () =
  check_position_value "center";
  check_position_value "left top";
  check_position_value "50% 25%";
  check_position_value "inherit";
  neg_cursor read_position_value "invalid-position"

let test_translate_value () =
  check_translate_value "none";
  check_translate_value "10px";
  check_translate_value "10px 20px";
  check_translate_value "10px 20px 30px";
  check_translate_value "50% 100%";
  check_translate_value "var(--my-translate)";
  check_translate_value ~expected:"var(--x)var(--y)" "var(--x) var(--y)";
  neg_cursor read_translate_value "invalid-translate"

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

let test_box_sizing () =
  check_box_sizing "border-box";
  check_box_sizing "content-box";
  check_box_sizing "inherit";
  neg_cursor read_box_sizing "invalid-sizing"

let test_object_fit () =
  check_object_fit "fill";
  check_object_fit "contain";
  check_object_fit "cover";
  check_object_fit "none";
  check_object_fit "scale-down";
  check_object_fit "inherit";
  neg_cursor read_object_fit "invalid-fit"

let test_content () =
  check_content "\"text\"";
  check_content "none";
  check_content "normal";
  check_content "open-quote";
  check_content "close-quote";
  neg_cursor read_content "invalid-content"

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
  check_svg_paint "url(#grad) red";
  neg_cursor read_svg_paint "invalid-paint"

let test_direction () =
  check_direction "ltr";
  check_direction "rtl";
  check_direction "inherit";
  neg_cursor read_direction "invalid-direction"

let test_unicode_bidi () =
  check_unicode_bidi "normal";
  check_unicode_bidi "embed";
  check_unicode_bidi "isolate";
  check_unicode_bidi "bidi-override";
  check_unicode_bidi "isolate-override";
  check_unicode_bidi "plaintext";
  check_unicode_bidi "inherit";
  neg_cursor read_unicode_bidi "invalid-bidi"

let test_writing_mode () =
  check_writing_mode "horizontal-tb";
  check_writing_mode "vertical-rl";
  check_writing_mode "vertical-lr";
  check_writing_mode "inherit";
  neg_cursor read_writing_mode "invalid-mode"

let test_webkit_appearance () =
  check_webkit_appearance "none";
  check_webkit_appearance "auto";
  check_webkit_appearance "button";
  check_webkit_appearance "textfield";
  check_webkit_appearance "inherit";
  neg_cursor read_webkit_appearance "invalid-appearance"

let test_webkit_font_smoothing () =
  check_webkit_font_smoothing "auto";
  check_webkit_font_smoothing "antialiased";
  check_webkit_font_smoothing "subpixel-antialiased";
  check_webkit_font_smoothing "inherit";
  neg_cursor read_webkit_font_smoothing "invalid-smoothing"

let test_moz_osx_font_smoothing () =
  check_moz_osx_font_smoothing "auto";
  check_moz_osx_font_smoothing "grayscale";
  check_moz_osx_font_smoothing "inherit";
  neg_cursor read_moz_osx_font_smoothing "invalid-smoothing"

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

let test_text_size_adjust () =
  check_text_size_adjust "none";
  check_text_size_adjust "auto";
  check_text_size_adjust "100%";
  check_text_size_adjust "inherit";
  neg_cursor read_text_size_adjust "invalid";
  neg_cursor read_text_size_adjust "-50%"

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

let test_text_decoration_skip_ink () =
  check_text_decoration_skip_ink "auto";
  check_text_decoration_skip_ink "none";
  check_text_decoration_skip_ink "all";
  check_text_decoration_skip_ink "inherit";
  neg_cursor read_text_decoration_skip_ink "invalid-skip"

let test_transform_origin () =
  check_transform_origin "center";
  check_transform_origin "left top";
  check_transform_origin "50% 25%";
  check_transform_origin "50% 50% 10px";
  check_transform_origin "inherit";
  neg_cursor read_transform_origin "invalid-origin"

let test_text_shadow () =
  check_text_shadow "none";
  check_text_shadow "inherit";
  check_text_shadow "2px 2px";
  check_text_shadow "2px 2px 4px";
  check_text_shadow "2px 2px red";
  check_text_shadow "2px 2px 4px red";
  check_text_shadow "red 2px 2px" ~expected:"2px 2px red";
  check_text_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  check_text_shadow "-2px -2px";
  check_text_shadow "0 0 10px";
  neg_cursor read_text_shadow "invalid-shadow"

let test_filter () =
  check_filter "none";
  check_filter "blur(5px)";
  check_filter ~expected:"blur(5px) contrast(1.2)" "blur(5px) contrast(1.2)";
  check_filter ~expected:"hue-rotate(30deg) opacity(.5)"
    "hue-rotate(30deg) opacity(0.5)";
  check_filter ~expected:"drop-shadow(2px 4px 6px red)"
    "drop-shadow(2px 4px 6px red)";
  neg_cursor read_filter "invalid-filter"

let test_shadow () =
  check_shadow "none";
  check_shadow "2px 2px";
  check_shadow "2px 2px 4px";
  check_shadow "2px 2px 4px 1px";
  check_shadow "2px 2px red";
  check_shadow "2px 2px 4px red";
  check_shadow "2px 2px 4px 1px red";
  check_shadow "inset 2px 2px";
  check_shadow "inset 2px 2px 4px";
  check_shadow "inset 2px 2px 4px 1px";
  check_shadow "inset 2px 2px red";
  check_shadow "inset 2px 2px 4px red";
  check_shadow "inset 2px 2px 4px 1px red";
  check_shadow "red 2px 2px" ~expected:"2px 2px red";
  check_shadow "red 2px 2px 4px" ~expected:"2px 2px 4px red";
  check_shadow "red 2px 2px 4px 1px" ~expected:"2px 2px 4px 1px red";
  check_shadow "red inset 2px 2px" ~expected:"inset 2px 2px red";
  check_shadow "red inset 2px 2px 4px" ~expected:"inset 2px 2px 4px red";
  check_shadow "red inset 2px 2px 4px 1px" ~expected:"inset 2px 2px 4px 1px red";
  (* Test compact printing - when blur and spread are not provided, should print
     compactly *)
  check_shadow "0 0 #0000" ~expected:"0 0 #0000";
  (* Ensure we don't get verbose 4-value format for simple cases *)
  check_shadow "0 0 rgba(0,0,0,0)" ~expected:"0 0 rgb(0 0 0/0)";
  check_shadow "inherit";
  neg_cursor read_shadow "invalid-shadow";
  neg_cursor read_shadow "10px"

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
  neg_cursor read_aspect_ratio "invalid-ratio"

let test_flex () =
  check_flex "1";
  check_flex ~expected:"1 auto" "1 1 auto";
  check_flex "none";
  check_flex "auto";
  check_flex "inherit";
  neg_cursor read_flex "invalid-flex"

let test_grid_line () =
  check_grid_line "auto";
  check_grid_line "1";
  check_grid_line "span 2";
  check_grid_line "main-start";
  check_grid_line "content-end";
  check_grid_line "inherit";
  neg_cursor read_grid_line "span"

let test_grid_template () =
  check_grid_template "none";
  check_grid_template "auto";
  check_grid_template "10px";
  check_grid_template "100px 200px";
  check_grid_template "1fr 2fr";
  check_grid_template "auto auto";
  check_grid_template "inherit";
  neg_cursor read_grid_template "invalid-template"

let test_justify_content () =
  check_justify_content "flex-start";
  check_justify_content "flex-end";
  check_justify_content "center";
  check_justify_content "space-between";
  check_justify_content "space-around";
  check_justify_content "space-evenly";
  neg_cursor read_justify_content "invalid-justify";
  neg_cursor read_justify_content "aroundish"

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

let test_place_content () =
  check_place_content "center";
  check_place_content "start end";
  check_place_content "flex-start center";
  check_place_content "inherit";
  neg_cursor read_place_content "invalid-place"

let test_place_items () =
  check_place_items "stretch";
  check_place_items "start end";
  check_place_items "center center";
  check_place_items "inherit";
  neg_cursor read_place_items "invalid-place"

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
  check_page_size "letter portrait";
  check_page_size "landscape";
  check_page_size "inherit";
  neg_cursor read_page_size "";
  neg_cursor read_page_size "a4 sideways";
  neg_cursor read_page_size "8.5in 11in 12in"

let test_timeline_axis () =
  check_timeline_axis "block";
  check_timeline_axis "inline";
  check_timeline_axis "x";
  check_timeline_axis "y";
  neg_cursor read_timeline_axis "z";
  neg_cursor read_timeline_axis "auto"

let test_timeline_shorthand () =
  check_timeline_shorthand "--main block";
  check_timeline_shorthand "--scroll inline";
  check_timeline_shorthand "--x x";
  neg_cursor read_timeline_shorthand "main block";
  neg_cursor read_timeline_shorthand "--main";
  neg_cursor read_timeline_shorthand "--main z"

let test_caption_side () =
  check_caption_side "top";
  check_caption_side "bottom";
  check_caption_side "inherit";
  neg_cursor read_caption_side "invalid-caption"

let test_color_scheme () =
  check_color_scheme "normal";
  check_color_scheme "light";
  check_color_scheme "dark";
  check_color_scheme "light dark";
  neg_cursor read_color_scheme "invalid-scheme"

let test_columns_value () =
  check_columns_value "auto";
  check_columns_value "2";
  check_columns_value "inherit";
  neg_cursor read_columns_value "invalid-columns"

let test_field_sizing () =
  check_field_sizing "content";
  check_field_sizing "fixed";
  check_field_sizing "inherit";
  neg_cursor read_field_sizing "invalid-field-sizing"

let test_font_size () =
  check_font_size "16px";
  check_font_size "small";
  check_font_size "medium";
  check_font_size "large";
  check_font_size "inherit";
  neg_cursor read_font_size "invalid-font-size"

let test_mask_box () =
  check_mask_box "border-box";
  check_mask_box "content-box";
  check_mask_box "padding-box";
  check_mask_box "fill-box";
  check_mask_box "inherit";
  neg_cursor read_mask_box "invalid-mask-box"

let test_mask_composite () =
  check_mask_composite "add";
  check_mask_composite "subtract";
  check_mask_composite "intersect";
  check_mask_composite "exclude";
  check_mask_composite "inherit";
  neg_cursor read_mask_composite "invalid-composite"

let test_mask_mode () =
  check_mask_mode "alpha";
  check_mask_mode "luminance";
  check_mask_mode "match-source";
  check_mask_mode "inherit";
  neg_cursor read_mask_mode "invalid-mode"

let test_mask_type () =
  check_mask_type "alpha";
  check_mask_type "luminance";
  check_mask_type "inherit";
  neg_cursor read_mask_type "invalid-mask-type"

let test_opacity () =
  check_opacity ~expected:".5" "0.5";
  check_opacity "1";
  check_opacity "0";
  neg_cursor read_opacity "invalid-opacity"

let test_order () =
  check_order "1";
  check_order "-2";
  check_order "100";
  neg_cursor read_order "invalid-order"

let test_rotate_value () =
  check_rotate_value "45deg";
  check_rotate_value "none";
  check_rotate_value "x 45deg";
  check_rotate_value "y 90deg";
  neg_cursor read_rotate_value "invalid-rotate"

let test_transform_box () =
  check_transform_box "content-box";
  check_transform_box "border-box";
  check_transform_box "fill-box";
  check_transform_box "stroke-box";
  check_transform_box "view-box";
  check_transform_box "inherit";
  neg_cursor read_transform_box "invalid-transform-box"

let test_webkit_line_clamp () =
  check_webkit_line_clamp "2";
  check_webkit_line_clamp "1";
  check_webkit_line_clamp "unset";
  neg_cursor read_webkit_line_clamp "invalid-clamp"

let test_webkit_mask_composite () =
  check_webkit_mask_composite "source-over";
  check_webkit_mask_composite "xor";
  check_webkit_mask_composite "source-in";
  check_webkit_mask_composite "source-out";
  check_webkit_mask_composite "inherit";
  neg_cursor read_webkit_mask_composite "invalid-composite"

let test_webkit_mask_source_type () =
  check_webkit_mask_source_type "alpha";
  check_webkit_mask_source_type "luminance";
  check_webkit_mask_source_type "auto";
  check_webkit_mask_source_type "inherit";
  neg_cursor read_webkit_mask_source_type "invalid-source-type"

let test_css_wide () =
  check_css_wide "initial";
  check_css_wide "inherit";
  check_css_wide "unset";
  check_css_wide "revert";
  check_css_wide "revert-layer";
  neg_cursor read_css_wide "red";
  neg_cursor read_css_wide ""

let spec_property_grammar_edges () =
  check_font_feature_settings "\"kern\" on";
  check_font_feature_settings "\"liga\" off, \"calt\" 1";
  check_font_variation_settings "\"wght\" 650, \"wdth\" 75";
  check_timing_function "linear(0, .25 50%, 1)";
  check_timing_function "steps(4, jump-none)";
  check_transform "translate(10px, 20%)" ~expected:"translate(10px,20%)";
  check_transform "rotate(1 0 0 45deg)";
  check_transform "scale(1.2 0.8)";
  check_transforms "translate(10px,20%) rotate(45deg) scale(1.2)";
  check_container_shorthand "card / inline-size" ~expected:"card/inline-size";
  check_container_shorthand "card / normal" ~expected:"card/normal";
  check_scroll_snap_type "x mandatory";
  check_scroll_snap_type "block proximity";
  check_clip_path "path(\"M 0 0 L 10 10\")";
  check_clip_path "xywh(0 0 100% 100% round 10px)";
  check_content "counter(page)";
  check_content "counters(section, \".\")";
  neg_cursor read_font_feature_settings "\"kern\" 2";
  neg_cursor read_font_variation_settings "\"wg\" 400";
  neg_cursor read_timing_function "linear()";
  neg_cursor read_timing_function "steps(0, jump-end)";
  neg_cursor read_transform "rotate(1 0 45deg)";
  neg_cursor read_transform "scale(1 2 3 4)";
  neg_cursor read_container_shorthand "card / inline-size / size";
  neg_cursor read_scroll_snap_type "mandatory x";
  neg_cursor read_clip_path "xywh(0 0)";
  neg_cursor read_content "counter()"

let spec_ui_property_edges () =
  check_text_wrap "pretty";
  check_text_wrap "balance";
  check_white_space "break-spaces";
  check_word_break "auto-phrase";
  check_overflow_wrap "anywhere";
  check_hyphens "manual";
  check_field_sizing "content";
  check_appearance "base-select";
  check_user_select "contain";
  neg_cursor read_text_wrap "pretty balance";
  neg_cursor read_white_space "normal nowrap pre";
  neg_cursor read_word_break "auto phrase";
  neg_cursor read_overflow_wrap "break-word anywhere";
  neg_cursor read_hyphens "soft";
  neg_cursor read_field_sizing "auto content";
  neg_cursor read_appearance "base button";
  neg_cursor read_user_select "none text"

let spec_mask_clip_property_edges () =
  check_mask_box "view-box";
  check_mask_box "stroke-box";
  check_mask_mode "match-source";
  check_mask_composite "exclude";
  check_webkit_mask_composite "source-over";
  check_webkit_mask_source_type "auto";
  check_clip_path "shape(from 0 0, line to 100% 0, close)";
  check_clip_path "polygon(0 0, 100% 0, 100% 100%)";
  neg_cursor read_mask_box "margin-box";
  neg_cursor read_mask_mode "source";
  neg_cursor read_mask_composite "add subtract";
  neg_cursor read_webkit_mask_composite "add";
  neg_cursor read_webkit_mask_source_type "match-source";
  neg_cursor read_clip_path "polygon()"

let tests =
  [
    test_case "display" `Quick test_display;
    test_case "position" `Quick test_position;
    test_case "overflow" `Quick test_overflow;
    test_case "border-style" `Quick test_border_style;
    test_case "border" `Quick test_border;
    test_case "visibility" `Quick test_visibility;
    test_case "z-index" `Quick test_z_index;
    test_case "flex-direction" `Quick test_flex_direction;
    test_case "flex-wrap" `Quick test_flex_wrap;
    test_case "align-self" `Quick test_align_self;
    test_case "font-style" `Quick test_font_style;
    test_case "font-display" `Quick test_font_display;
    test_case "unicode-range" `Quick test_unicode_range;
    test_case "text-align" `Quick test_text_align;
    test_case "text-decoration-style" `Quick test_text_decoration_style;
    test_case "text-overflow" `Quick test_text_overflow;
    test_case "text-wrap" `Quick test_text_wrap;
    test_case "white-space" `Quick test_white_space;
    test_case "word-break" `Quick test_word_break;
    test_case "overflow-wrap" `Quick test_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
    test_case "line-height" `Quick test_line_height;
    test_case "list-style-type" `Quick test_list_style_type;
    test_case "list-style-position" `Quick test_list_style_position;
    test_case "list-style-image" `Quick test_list_style_image;
    test_case "table-layout" `Quick test_table_layout;
    test_case "border-collapse" `Quick test_border_collapse;
    test_case "object-fit" `Quick test_object_fit;
    test_case "content-visibility" `Quick test_content_visibility;
    test_case "isolation" `Quick test_isolation;
    test_case "scroll-behavior" `Quick test_scroll_behavior;
    test_case "scroll-snap-align" `Quick test_scroll_snap_align;
    test_case "scroll-snap-stop" `Quick test_scroll_snap_stop;
    test_case "scroll-snap-axis" `Quick test_scroll_snap_axis;
    test_case "scroll-snap-strictness" `Quick test_scroll_snap_strictness;
    test_case "scroll-snap-type" `Quick test_scroll_snap_type;
    test_case "property names" `Quick test_property_names;
    (* Additional coverage for missing readers *)
    test_case "grid auto-flow" `Quick test_grid_auto_flow;
    test_case "grid template" `Quick test_grid_template;
    test_case "grid line" `Quick test_grid_line;
    test_case "align-items" `Quick test_align_items;
    test_case "justify-content" `Quick test_justify_content;
    test_case "place-items" `Quick test_place_items;
    test_case "place-content" `Quick test_place_content;
    test_case "flex" `Quick test_flex;
    test_case "transform" `Quick test_transform;
    test_case "transforms" `Quick test_transforms;
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "gradient stop" `Quick test_gradient_stop;
    test_case "color interpolation" `Quick test_color_interpolation;
    test_case "overscroll-behavior" `Quick test_overscroll_behavior;
    test_case "aspect-ratio" `Quick test_aspect_ratio;
    test_case "content" `Quick test_content;
    test_case "background_box" `Quick test_background_box;
    test_case "background shorthand" `Quick test_background_shorthand;
    test_case "background-attachment" `Quick test_background_attachment;
    test_case "background-repeat" `Quick test_background_repeat;
    test_case "background-size" `Quick test_background_size;
    test_case "background-image" `Quick test_background_image;
    test_case "filter" `Quick test_filter;
    test_case "pp property value" `Quick test_pp_property_value;
    test_case "spec current property grammar edges" `Quick
      spec_property_grammar_edges;
    test_case "spec UI property edges" `Quick spec_ui_property_edges;
    test_case "spec mask and clip property edges" `Quick
      spec_mask_clip_property_edges;
  ]

let test_will_change () =
  check_will_change "auto";
  check_will_change "scroll-position";
  check_will_change "contents";
  check_will_change "transform";
  check_will_change "opacity";
  neg_cursor read_will_change "123invalid"

let test_clip () =
  check_clip "auto";
  check_clip ~expected:"rect(0,10px,20px,30px)" "rect(0px,10px,20px,30px)";
  neg_cursor read_clip "invalid-clip"

let test_clip_path () =
  check_clip_path "none";
  check_clip_path "url(clip.svg)";
  (* inset() with 1-4 values like margin/padding shorthand *)
  check_clip_path "inset(50%)";
  (* 1 value: all sides *)
  check_clip_path "inset(10px)";
  check_clip_path "inset(10% 20%)";
  (* 2 values: top/bottom, left/right *)
  check_clip_path "inset(10% 20% 30%)";
  (* 3 values: top, left/right, bottom *)
  check_clip_path ~expected:"inset(0 10px 20px 30px)"
    "inset(0px 10px 20px 30px)";
  (* 4 values *)
  check_clip_path "circle(50px)";
  check_clip_path "ellipse(25px 50px)";
  check_clip_path ~expected:"polygon(0 0,100px 0,50px 100px)"
    "polygon(0px 0px,100px 0px,50px 100px)";
  neg_cursor read_clip_path "";
  neg_cursor read_clip_path "invalid"

let test_perspective_origin () =
  check_perspective_origin "center";
  check_perspective_origin "top";
  check_perspective_origin "bottom";
  check_perspective_origin "left";
  check_perspective_origin "right";
  check_perspective_origin "50px 100px";
  neg_cursor read_perspective_origin ""

let test_quotes () =
  check_quotes "auto";
  check_quotes "none";
  neg_cursor read_quotes "invalid-quotes-value"

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

let additional_tests =
  [
    test_case "will_change" `Quick test_will_change;
    test_case "clip" `Quick test_clip;
    test_case "clip_path" `Quick test_clip_path;
    test_case "perspective_origin" `Quick test_perspective_origin;
    test_case "quotes" `Quick test_quotes;
    test_case "outline" `Quick test_outline;
    test_case "outline_shorthand" `Quick test_outline_shorthand;
    test_case "background" `Quick test_background;
    test_case "font_family" `Quick test_font_family;
    test_case "text_shadow" `Quick test_text_shadow;
    test_case "font_weight" `Quick test_font_weight;
    test_case "text_transform" `Quick test_text_transform;
    test_case "text_decoration_line" `Quick test_text_decoration_line;
    test_case "text_decoration" `Quick test_text_decoration;
    test_case "cursor" `Quick test_cursor;
    test_case "border_width" `Quick test_border_width;
    test_case "border_radius" `Quick test_border_radius;
    (* New test cases *)
    test_case "text_decoration_shorthand" `Quick test_text_decoration_shorthand;
    test_case "justify_self" `Quick test_justify_self;
    test_case "align_content_values" `Quick test_align_content;
    test_case "border_shorthand" `Quick test_border_shorthand;
    test_case "justify_items" `Quick test_justify_items;
    test_case "transition_shorthand" `Quick test_transition_shorthand;
    test_case "flex_basis" `Quick test_flex_basis;
    test_case "background_shorthand" `Quick test_background_shorthand;
    test_case "animation_shorthand" `Quick test_animation_shorthand;
    test_case "text_size_adjust" `Quick test_text_size_adjust;
    test_case "any_property" `Quick test_any_property;
    test_case "gap" `Quick test_gap;
    test_case "font_variant_numeric_token" `Quick
      test_font_variant_numeric_token;
    test_case "list_style_type" `Quick test_list_style_type;
    test_case "list_style_position" `Quick test_list_style_position;
    test_case "list_style_image" `Quick test_list_style_image;
    test_case "vertical_align" `Quick test_vertical_align;
    test_case "font_stretch" `Quick test_font_stretch;
    test_case "font_variant_numeric" `Quick test_font_variant_numeric;
    test_case "font_feature_settings" `Quick test_font_feature_settings;
    test_case "font_variation_settings" `Quick test_font_variation_settings;
    test_case "transform_style" `Quick test_transform_style;
    test_case "backface_visibility" `Quick test_backface_visibility;
    test_case "scale" `Quick test_scale;
    test_case "steps_direction" `Quick test_steps_direction;
    test_case "timing_function" `Quick test_timing_function;
    test_case "transition_property_value" `Quick test_transition_property_value;
    test_case "transition_property" `Quick test_transition_property;
    test_case "transition_behavior" `Quick test_transition_behavior;
    test_case "transition" `Quick test_transition;
    test_case "animation_direction" `Quick test_animation_direction;
    test_case "animation_fill_mode" `Quick test_animation_fill_mode;
    test_case "animation_iteration_count" `Quick test_animation_iteration_count;
    test_case "animation_play_state" `Quick test_animation_play_state;
    test_case "animation" `Quick test_animation;
    test_case "blend_mode" `Quick test_blend_mode;
    test_case "background_attachment" `Quick test_background_attachment;
    test_case "background_repeat" `Quick test_background_repeat;
    test_case "background_size" `Quick test_background_size;
    test_case "gradient_direction" `Quick test_gradient_direction;
    test_case "gradient_stop" `Quick test_gradient_stop;
    test_case "conic_gradient_config" `Quick test_conic_gradient_config;
    test_case "radial_shape" `Quick test_radial_shape;
    test_case "radial_size" `Quick test_radial_size;
    test_case "radial_gradient_config" `Quick test_radial_gradient_config;
    test_case "background_image" `Quick test_background_image;
    test_case "background_position" `Quick test_background_position;
    test_case "position_value" `Quick test_position_value;
    test_case "translate_value" `Quick test_translate_value;
    test_case "user_select" `Quick test_user_select;
    test_case "pointer_events" `Quick test_pointer_events;
    test_case "touch_action" `Quick test_touch_action;
    test_case "resize" `Quick test_resize;
    test_case "box_sizing" `Quick test_box_sizing;
    test_case "object_fit" `Quick test_object_fit;
    test_case "content" `Quick test_content;
    test_case "content_visibility" `Quick test_content_visibility;
    test_case "container_type" `Quick test_container_type;
    test_case "container_shorthand" `Quick test_container_shorthand;
    test_case "contain" `Quick test_contain;
    test_case "isolation" `Quick test_isolation;
    test_case "scroll_behavior" `Quick test_scroll_behavior;
    test_case "scroll_snap_align" `Quick test_scroll_snap_align;
    test_case "scroll_snap_stop" `Quick test_scroll_snap_stop;
    test_case "scroll_snap_strictness" `Quick test_scroll_snap_strictness;
    test_case "scroll_snap_type" `Quick test_scroll_snap_type;
    test_case "overscroll_behavior" `Quick test_overscroll_behavior;
    test_case "svg_paint" `Quick test_svg_paint;
    test_case "direction" `Quick test_direction;
    test_case "unicode_bidi" `Quick test_unicode_bidi;
    test_case "writing_mode" `Quick test_writing_mode;
    test_case "webkit_appearance" `Quick test_webkit_appearance;
    test_case "webkit_font_smoothing" `Quick test_webkit_font_smoothing;
    test_case "moz_osx_font_smoothing" `Quick test_moz_osx_font_smoothing;
    test_case "webkit_box_orient" `Quick test_webkit_box_orient;
    test_case "moz_orient" `Quick test_moz_orient;
    test_case "forced_color_adjust" `Quick test_forced_color_adjust;
    test_case "print_color_adjust" `Quick test_print_color_adjust;
    test_case "appearance" `Quick test_appearance;
    test_case "clear" `Quick test_clear;
    test_case "float_side" `Quick test_float_side;
    test_case "text_decoration_skip_ink" `Quick test_text_decoration_skip_ink;
    test_case "transform_origin" `Quick test_transform_origin;
    test_case "shadow" `Quick test_shadow;
    test_case "shadow" `Quick test_shadow;
    test_case "align_items" `Quick test_align_items;
    test_case "aspect_ratio" `Quick test_aspect_ratio;
    test_case "flex" `Quick test_flex;
    test_case "grid_line" `Quick test_grid_line;
    test_case "grid_template" `Quick test_grid_template;
    test_case "justify_content" `Quick test_justify_content;
    test_case "outline_style" `Quick test_outline_style;
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
    test_case "timeline_axis" `Quick test_timeline_axis;
    test_case "timeline_shorthand" `Quick test_timeline_shorthand;
    test_case "caption_side" `Quick test_caption_side;
    test_case "color_scheme" `Quick test_color_scheme;
    test_case "columns_value" `Quick test_columns_value;
    test_case "field_sizing" `Quick test_field_sizing;
    test_case "font_size" `Quick test_font_size;
    test_case "mask_box" `Quick test_mask_box;
    test_case "mask_composite" `Quick test_mask_composite;
    test_case "mask_mode" `Quick test_mask_mode;
    test_case "mask_type" `Quick test_mask_type;
    test_case "opacity" `Quick test_opacity;
    test_case "order" `Quick test_order;
    test_case "rotate_value" `Quick test_rotate_value;
    test_case "transform_box" `Quick test_transform_box;
    test_case "webkit_line_clamp" `Quick test_webkit_line_clamp;
    test_case "webkit_mask_composite" `Quick test_webkit_mask_composite;
    test_case "webkit_mask_source_type" `Quick test_webkit_mask_source_type;
    test_case "css_wide" `Quick test_css_wide;
  ]

let suite = ("properties", tests @ additional_tests)
