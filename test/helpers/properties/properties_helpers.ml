(** Checkers shared by the property test suites. *)

open Cascade
open Css.Properties
open Css_test_helpers

let check_position =
  check_value_cursor "position" read_position pp_position ~roundtrip:true

let check_overflow =
  check_value_cursor "overflow" read_overflow pp_overflow ~roundtrip:true

let check_visibility =
  check_value_cursor "visibility" read_visibility pp_visibility ~roundtrip:true

let check_flex_direction =
  check_value_cursor "flex-direction" read_flex_direction pp_flex_direction
    ~roundtrip:true

let check_align_self =
  check_value_cursor "align-self" read_align_self pp_align_self

let check_text_align =
  check_value_cursor "text-align" read_text_align pp_text_align

let check_text_decoration_style =
  check_value_cursor "text-decoration-style" read_text_decoration_style
    pp_text_decoration_style

let check_text_wrap = check_value_cursor "text-wrap" read_text_wrap pp_text_wrap

let check_white_space =
  check_value_cursor "white-space" read_white_space pp_white_space

let check_word_break =
  check_value_cursor "word-break" read_word_break pp_word_break

let check_overflow_wrap =
  check_value_cursor "overflow-wrap" read_overflow_wrap pp_overflow_wrap

let check_hyphens = check_value_cursor "hyphens" read_hyphens pp_hyphens

let check_user_select =
  check_value_cursor "user-select" read_user_select pp_user_select

let check_pointer_events =
  check_value_cursor "pointer-events" read_pointer_events pp_pointer_events

let check_resize = check_value_cursor "resize" read_resize pp_resize

let check_box_sizing =
  check_value_cursor "box-sizing" read_box_sizing pp_box_sizing

let check_object_fit =
  check_value_cursor "object-fit" read_object_fit pp_object_fit

let check_content_visibility =
  check_value_cursor "content-visibility" read_content_visibility
    pp_content_visibility

let check_container_shorthand ?expected input =
  let expected = Option.value ~default:input expected in
  let c = Cursor.of_string input in
  let value = read_container_shorthand c in
  let serialized = Css.Pp.to_string ~minify:true pp_container_shorthand value in
  Alcotest.(check string) (Fmt.str "container %s" input) expected serialized;
  let expected_value = read_container_shorthand (Cursor.of_string expected) in
  Alcotest.(check bool)
    (Fmt.str "container structural expected %s" input)
    true
    (equal_container_shorthand value expected_value);
  let reparsed = read_container_shorthand (Cursor.of_string serialized) in
  Alcotest.(check bool)
    (Fmt.str "container structural roundtrip %s" input)
    true
    (equal_container_shorthand value reparsed)

let check_scroll_snap_align =
  check_value_cursor "scroll-snap-align" read_scroll_snap_align
    pp_scroll_snap_align

let check_scroll_snap_stop =
  check_value_cursor "scroll-snap-stop" read_scroll_snap_stop
    pp_scroll_snap_stop

let check_scroll_snap_type =
  check_value_cursor "scroll-snap-type" read_scroll_snap_type
    pp_scroll_snap_type

let check_direction = check_value_cursor "direction" read_direction pp_direction

let check_writing_mode =
  check_value_cursor "writing-mode" read_writing_mode pp_writing_mode

let check_print_color_adjust =
  check_value_cursor "print-color-adjust" read_print_color_adjust
    pp_print_color_adjust

let check_appearance =
  check_value_cursor "appearance" read_appearance pp_appearance

let check_clear = check_value_cursor "clear" read_clear pp_clear
let check_float_side = check_value_cursor "float" read_float_side pp_float_side

let check_font_feature_settings =
  check_value_cursor "font-feature-settings" read_font_feature_settings
    pp_font_feature_settings

let check_font_variation_settings =
  check_value_cursor "font-variation-settings" read_font_variation_settings
    pp_font_variation_settings

let check_timing_function =
  check_value_cursor "timing-function" read_timing_function pp_timing_function

let check_content = check_value_cursor "content" read_content pp_content

let check_grid_template =
  check_value_cursor "grid-template" read_grid_template pp_grid_template

let check_grid_line = check_value_cursor "grid-line" read_grid_line pp_grid_line

let check_column_count =
  check_value_cursor "column-count" read_column_count pp_column_count

let check_transform = check_value_cursor "transform" read_transform pp_transform

let check_transforms =
  check_value_cursor "transforms" read_transforms pp_transforms

let check_scroll_snap_axis =
  check_value_cursor "scroll_snap_axis" read_scroll_snap_axis
    pp_scroll_snap_axis

let check_scroll_snap_strictness =
  check_value_cursor "scroll_snap_strictness" read_scroll_snap_strictness
    pp_scroll_snap_strictness

let check_clip_path = check_value_cursor "clip_path" read_clip_path pp_clip_path

let check_columns_value =
  check_value_cursor "columns_value" read_columns_value pp_columns_value

let check_field_sizing =
  check_value_cursor "field_sizing" read_field_sizing pp_field_sizing

let check_mask_box = check_value_cursor "mask_box" read_mask_box pp_mask_box

let check_mask_composite =
  check_value_cursor "mask_composite" read_mask_composite pp_mask_composite

let check_mask_mode = check_value_cursor "mask_mode" read_mask_mode pp_mask_mode

let check_webkit_mask_composite =
  check_value_cursor "webkit_mask_composite" read_webkit_mask_composite
    pp_webkit_mask_composite

let check_webkit_mask_source_type =
  check_value_cursor "webkit_mask_source_type" read_webkit_mask_source_type
    pp_webkit_mask_source_type

let check_east_asian_feature =
  check_value_cursor "east_asian_feature" read_east_asian_feature
    pp_east_asian_feature

let check_grid_line_pair =
  check_value_cursor "grid_line_pair" read_grid_line_pair pp_grid_line_pair

let check_grid_template_areas =
  check_value_cursor "grid_template_areas" read_grid_template_areas
    pp_grid_template_areas

let check_ray_size = check_value_cursor "ray_size" read_ray_size pp_ray_size
let check_tab_size = check_value_cursor "tab_size" read_tab_size pp_tab_size
let check_zoom = check_value_cursor "zoom" read_zoom pp_zoom
