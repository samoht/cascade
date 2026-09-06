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

(* CSS Syntax 3 sec. 5.5.11 is the one caller that asks the tokenizer for
   unicode ranges, so these vectors read as that descriptor's value does. *)
let check_unicode_range =
  check_value_cursor ~unicode_ranges:true "unicode-range" read_unicode_range
    pp_unicode_range

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

let check_font_feature_value =
  check_value_cursor "font-feature-value" read_font_feature_value
    pp_font_feature_value

let check_font_feature_setting =
  check_value_cursor "font-feature-setting" read_font_feature_setting
    pp_font_feature_setting

let check_font_feature_settings =
  check_value_cursor "font-feature-settings" read_font_feature_settings
    pp_font_feature_settings

let check_font_variation_setting =
  check_value_cursor "font-variation-setting" read_font_variation_setting
    pp_font_variation_setting

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

let check_filter_function =
  check_value_cursor "filter function" read_filter_function pp_filter_function

let check_background_attachment =
  check_value_cursor "background-attachment" read_background_attachment
    pp_background_attachment

let check_background_repeat =
  check_value_cursor "background-repeat" read_background_repeat
    pp_background_repeat

(* The standalone longhand (comma-separated layer list) goes through the [_list]
   readers; the single-value [check_*] above is the per-layer / shorthand
   path. *)
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

let check_grid_auto_flow_component =
  check_value_cursor "grid-auto-flow component" read_grid_auto_flow_component
    pp_grid_auto_flow_component

let check_grid_template =
  check_value_cursor "grid-template" read_grid_template pp_grid_template

let check_grid_flex_math =
  check_value_cursor "grid-flex-math" read_grid_flex_math pp_grid_flex_math

let roundtrip_grid_auto_rows =
  check_value_cursor "grid-auto-rows" read_grid_auto_tracks pp_grid_template

let roundtrip_grid_auto_columns =
  check_value_cursor "grid-auto-columns" read_grid_auto_tracks pp_grid_template

let check_grid_line = check_value_cursor "grid-line" read_grid_line pp_grid_line

let check_align_items =
  check_value_cursor "align-items" read_align_items pp_align_items

let check_justify_content =
  check_value_cursor "justify-content" read_justify_content pp_justify_content

let check_flex = check_value_cursor "flex" read_flex pp_flex

let check_font_variant_css21 =
  check_value_cursor "font-variant-css21" read_font_variant_css21
    pp_font_variant_css21

let check_column_width =
  check_value_cursor "column-width" read_column_width pp_column_width

let check_column_count =
  check_value_cursor "column-count" read_column_count pp_column_count

let check_position_try =
  check_value_cursor "position-try" read_position_try pp_position_try

let check_border_image_repeat =
  check_value_cursor "border-image-repeat" read_border_image_repeat
    pp_border_image_repeat

let check_border_image_width =
  check_value_cursor "border-image-width" read_border_image_width
    pp_border_image_width

let check_border_image_outset =
  check_value_cursor "border-image-outset" read_border_image_outset
    pp_border_image_outset

let check_list_style =
  check_value_cursor "list-style" read_list_style pp_list_style

let check_list_style_shorthand =
  check_value_cursor "list-style-shorthand" read_list_style_shorthand
    pp_list_style_shorthand

let check_grid_area = check_value_cursor "grid-area" read_grid_area pp_grid_area
let check_font = check_value_cursor "font" read_font pp_font

let check_font_shorthand =
  check_value_cursor "font-shorthand" read_font_shorthand pp_font_shorthand

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

let check_gradient_position =
  check_value_cursor "gradient-position" read_gradient_position
    pp_gradient_position

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

let read_required_hue_interpolation_method t =
  match read_hue_interpolation_method t with
  | Some value -> value
  | None -> Cursor.err_expected t "hue-interpolation-method"

let check_hue_interpolation_method =
  check_value_cursor "hue_interpolation_method"
    read_required_hue_interpolation_method pp_hue_interpolation_method

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

let check_text_indent_value =
  check_value_cursor "text_indent_value" read_text_indent_value
    pp_text_indent_value

let check_text_transform_case =
  check_value_cursor "text_transform_case" read_text_transform_case
    pp_text_transform_case

let check_symbols_type =
  check_value_cursor "symbols_type" read_symbols_type pp_symbols_type

let check_list_style_symbol =
  check_value_cursor "list_style_symbol" read_list_style_symbol
    pp_list_style_symbol

let check_mask_border_mode =
  check_value_cursor "mask_border_mode" read_mask_border_mode
    pp_mask_border_mode

let check_clip_geometry_box =
  check_value_cursor "clip_geometry_box" read_clip_geometry_box
    pp_clip_geometry_box

let check_clip_path_extent =
  check_value_cursor "clip_path_extent" read_clip_path_extent
    pp_clip_path_extent

let check_clip_path_fill_rule =
  check_value_cursor "clip_path_fill_rule" read_clip_path_fill_rule
    pp_clip_path_fill_rule

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
  check_value_cursor "axis" read_timeline_axis pp_timeline_axis

let check_timeline_shorthand =
  check_value_cursor "timeline_shorthand" read_timeline_shorthand
    pp_timeline_shorthand

let check_view_timeline_shorthand =
  check_value_cursor "view_timeline_shorthand" read_view_timeline_shorthand
    pp_view_timeline_shorthand

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

let check_alignment_baseline =
  check_value_cursor "alignment_baseline" read_alignment_baseline
    pp_alignment_baseline

let check_anchor_name =
  check_value_cursor "anchor_name" read_anchor_name pp_anchor_name

let check_animation_composition =
  check_value_cursor "animation_composition" read_animation_composition
    pp_animation_composition

let check_animation_composition_item =
  check_value_cursor "animation_composition_item"
    read_animation_composition_item pp_animation_composition_item

let check_animation_name =
  check_value_cursor "animation_name" read_animation_name pp_animation_name

let check_animation_range =
  check_value_cursor "animation_range" read_animation_range pp_animation_range

let check_animation_range_item =
  check_value_cursor "animation_range_item" read_animation_range_item
    pp_animation_range_item

let check_animation_range_name =
  check_value_cursor "animation_range_name" read_animation_range_name
    pp_animation_range_name

let check_animation_timeline =
  check_value_cursor "animation_timeline" read_animation_timeline
    pp_animation_timeline

let check_baseline_shift =
  check_value_cursor "baseline_shift" read_baseline_shift pp_baseline_shift

let check_baseline_source =
  check_value_cursor "baseline_source" read_baseline_source pp_baseline_source

let check_border_image =
  check_value_cursor "border_image" read_border_image pp_border_image

let check_border_image_outset_item =
  check_value_cursor "border_image_outset_item" read_border_image_outset_item
    pp_border_image_outset_item

let check_border_image_repeat_keyword =
  check_value_cursor "border_image_repeat_keyword"
    read_border_image_repeat_keyword pp_border_image_repeat_keyword

let check_border_image_slice =
  check_value_cursor "border_image_slice" read_border_image_slice
    pp_border_image_slice

let check_border_image_slice_offsets =
  check_value_cursor "border_image_slice_offsets"
    read_border_image_slice_offsets pp_border_image_slice_offsets

let check_border_image_slice_item =
  check_value_cursor "border_image_slice_item" read_border_image_slice_item
    pp_border_image_slice_item

let check_border_image_width_item =
  check_value_cursor "border_image_width_item" read_border_image_width_item
    pp_border_image_width_item

let check_border_spacing =
  check_value_cursor "border_spacing" read_border_spacing pp_border_spacing

let check_caret = check_value_cursor "caret" read_caret pp_caret

let check_caret_animation =
  check_value_cursor "caret_animation" read_caret_animation pp_caret_animation

let check_caret_shape =
  check_value_cursor "caret_shape" read_caret_shape pp_caret_shape

let check_column_span =
  check_value_cursor "column_span" read_column_span pp_column_span

let check_contain_intrinsic_longhand =
  check_value_cursor "contain_intrinsic_longhand"
    read_contain_intrinsic_longhand pp_contain_intrinsic_longhand

let check_contain_intrinsic_size =
  check_value_cursor "contain_intrinsic_size" read_contain_intrinsic_size
    pp_contain_intrinsic_size

let check_contain_intrinsic_size_item =
  check_value_cursor "contain_intrinsic_size_item"
    read_contain_intrinsic_size_item pp_contain_intrinsic_size_item

let check_container_name =
  check_value_cursor "container_name" read_container_name pp_container_name

let check_counter_item =
  check_value_cursor "counter_item" read_counter_item pp_counter_item

let check_counter_set =
  check_value_cursor "counter_set" read_counter_set pp_counter_set

let check_dominant_baseline =
  check_value_cursor "dominant_baseline" read_dominant_baseline
    pp_dominant_baseline

let check_flex_factor =
  check_value_cursor "flex_factor" read_flex_factor pp_flex_factor

let check_flex_flow = check_value_cursor "flex_flow" read_flex_flow pp_flex_flow

let check_font_kerning =
  check_value_cursor "font_kerning" read_font_kerning pp_font_kerning

let check_font_language_override =
  check_value_cursor "font_language_override" read_font_language_override
    pp_font_language_override

let check_font_optical_sizing =
  check_value_cursor "font_optical_sizing" read_font_optical_sizing
    pp_font_optical_sizing

let check_font_palette =
  check_value_cursor "font_palette" read_font_palette pp_font_palette

let check_font_size_adjust =
  check_value_cursor "font_size_adjust" read_font_size_adjust
    pp_font_size_adjust

let check_font_size_adjust_metric =
  check_value_cursor "font_size_adjust_metric" read_font_size_adjust_metric
    pp_font_size_adjust_metric

let check_font_synthesis =
  check_value_cursor "font_synthesis" read_font_synthesis pp_font_synthesis

let check_font_synthesis_feature =
  check_value_cursor "font_synthesis_feature" read_font_synthesis_feature
    pp_font_synthesis_feature

let check_font_synthesis_position =
  check_value_cursor "font_synthesis_position" read_font_synthesis_position
    pp_font_synthesis_position

let check_font_synthesis_small_caps =
  check_value_cursor "font_synthesis_small_caps" read_font_synthesis_small_caps
    pp_font_synthesis_small_caps

let check_font_synthesis_style =
  check_value_cursor "font_synthesis_style" read_font_synthesis_style
    pp_font_synthesis_style

let check_font_synthesis_weight =
  check_value_cursor "font_synthesis_weight" read_font_synthesis_weight
    pp_font_synthesis_weight

let check_font_variant =
  check_value_cursor "font_variant" read_font_variant pp_font_variant

let check_font_variant_alternates =
  check_value_cursor "font_variant_alternates" read_font_variant_alternates
    pp_font_variant_alternates

let check_white_space_collapse =
  check_value_cursor "white_space_collapse" read_white_space_collapse
    pp_white_space_collapse

let check_column_height =
  check_value_cursor "column_height" read_column_height pp_column_height

let check_column_wrap =
  check_value_cursor "column_wrap" read_column_wrap pp_column_wrap

let check_webkit_text_stroke =
  check_value_cursor "webkit_text_stroke" read_webkit_text_stroke
    pp_webkit_text_stroke

let check_font_variant_caps =
  check_value_cursor "font_variant_caps" read_font_variant_caps
    pp_font_variant_caps

let check_font_variant_east_asian =
  check_value_cursor "font_variant_east_asian" read_font_variant_east_asian
    pp_font_variant_east_asian

let check_east_asian_feature =
  check_value_cursor "east_asian_feature" read_east_asian_feature
    pp_east_asian_feature

let check_font_variant_emoji =
  check_value_cursor "font_variant_emoji" read_font_variant_emoji
    pp_font_variant_emoji

let check_font_variant_ligature =
  check_value_cursor "font_variant_ligature" read_font_variant_ligature
    pp_font_variant_ligature

let check_font_variant_ligatures =
  check_value_cursor "font_variant_ligatures" read_font_variant_ligatures
    pp_font_variant_ligatures

let check_font_variant_position =
  check_value_cursor "font_variant_position" read_font_variant_position
    pp_font_variant_position

let check_glyph_orientation_vertical =
  check_value_cursor "glyph_orientation_vertical"
    read_glyph_orientation_vertical pp_glyph_orientation_vertical

let check_grid_line_pair =
  check_value_cursor "grid_line_pair" read_grid_line_pair pp_grid_line_pair

let check_grid_template_areas =
  check_value_cursor "grid_template_areas" read_grid_template_areas
    pp_grid_template_areas

let check_hyphenate_limit_chars =
  check_value_cursor "hyphenate_limit_chars" read_hyphenate_limit_chars
    pp_hyphenate_limit_chars

let check_image_orientation =
  check_value_cursor "image_orientation" read_image_orientation
    pp_image_orientation

let check_image_rendering =
  check_value_cursor "image_rendering" read_image_rendering pp_image_rendering

let check_image_resolution =
  check_value_cursor "image_resolution" read_image_resolution
    pp_image_resolution

let check_initial_letter =
  check_value_cursor "initial_letter" read_initial_letter pp_initial_letter

let check_initial_letter_align =
  check_value_cursor "initial_letter_align" read_initial_letter_align
    pp_initial_letter_align

let check_initial_letter_align_keyword =
  check_value_cursor "initial_letter_align_keyword"
    read_initial_letter_align_keyword pp_initial_letter_align_keyword

let check_initial_letter_wrap =
  check_value_cursor "initial_letter_wrap" read_initial_letter_wrap
    pp_initial_letter_wrap

let check_inline_sizing =
  check_value_cursor "inline_sizing" read_inline_sizing pp_inline_sizing

let check_interactivity =
  check_value_cursor "interactivity" read_interactivity pp_interactivity

let check_interest_delay =
  check_value_cursor "interest_delay" read_interest_delay pp_interest_delay

let check_interpolate_size =
  check_value_cursor "interpolate_size" read_interpolate_size
    pp_interpolate_size

let check_line_break =
  check_value_cursor "line_break" read_line_break pp_line_break

let check_line_fit_edge =
  check_value_cursor "line_fit_edge" read_line_fit_edge pp_line_fit_edge

let check_line_fit_edge_keyword =
  check_value_cursor "line_fit_edge_keyword" read_line_fit_edge_keyword
    pp_line_fit_edge_keyword

let check_logical_border_color =
  check_value_cursor "logical_border_color" read_logical_border_color
    pp_logical_border_color

let check_logical_border_style =
  check_value_cursor "logical_border_style" read_logical_border_style
    pp_logical_border_style

let check_logical_border_width =
  check_value_cursor "logical_border_width" read_logical_border_width
    pp_logical_border_width

let check_margin_trim =
  check_value_cursor "margin_trim" read_margin_trim pp_margin_trim

let check_margin_trim_axis =
  check_value_cursor "margin_trim_axis" read_margin_trim_axis
    pp_margin_trim_axis

let check_margin_trim_edge =
  check_value_cursor "margin_trim_edge" read_margin_trim_edge
    pp_margin_trim_edge

let check_mask = check_value_cursor "mask" read_mask pp_mask

let check_mask_layer =
  check_value_cursor "mask_layer" read_mask_layer pp_mask_layer

let check_min_intrinsic_sizing =
  check_value_cursor "min_intrinsic_sizing" read_min_intrinsic_sizing
    pp_min_intrinsic_sizing

let check_min_intrinsic_sizing_keyword =
  check_value_cursor "min_intrinsic_sizing_keyword"
    read_min_intrinsic_sizing_keyword pp_min_intrinsic_sizing_keyword

let check_nav = check_value_cursor "nav" read_nav pp_nav
let check_nav_scope = check_value_cursor "nav_scope" read_nav_scope pp_nav_scope

let check_object_view_box =
  check_value_cursor "object_view_box" read_object_view_box pp_object_view_box

let check_offset_path =
  check_value_cursor "offset_path" read_offset_path pp_offset_path

let check_offset =
  check_value_cursor "offset" read_offset pp_offset ~roundtrip:true

let check_offset_target =
  check_value_cursor "offset_target" read_offset_target pp_offset_target
    ~roundtrip:true

let check_offset_anchor =
  check_value_cursor "offset_anchor" read_offset_anchor pp_offset_anchor

let check_offset_position =
  check_value_cursor "offset_position" read_offset_position pp_offset_position

let check_offset_rotate =
  check_value_cursor "offset_rotate" read_offset_rotate pp_offset_rotate

let check_offset_rotate_mode =
  check_value_cursor "offset_rotate_mode" read_offset_rotate_mode
    pp_offset_rotate_mode

let check_overflow_anchor =
  check_value_cursor "overflow_anchor" read_overflow_anchor pp_overflow_anchor

let check_overflow_clip_box =
  check_value_cursor "overflow_clip_box" read_overflow_clip_box
    pp_overflow_clip_box

let check_overflow_clip_margin =
  check_value_cursor "overflow_clip_margin" read_overflow_clip_margin
    pp_overflow_clip_margin

let check_overlay = check_value_cursor "overlay" read_overlay pp_overlay

let check_position_anchor =
  check_value_cursor "position_anchor" read_position_anchor pp_position_anchor

let check_position_area =
  check_value_cursor "position_area" read_position_area pp_position_area

let check_position_area_keyword =
  check_value_cursor "position_area_keyword" read_position_area_keyword
    pp_position_area_keyword

let check_position_try_fallback =
  check_value_cursor "position_try_fallback" read_position_try_fallback
    pp_position_try_fallback

let check_position_try_fallbacks =
  check_value_cursor "position_try_fallbacks" read_position_try_fallbacks
    pp_position_try_fallbacks

let check_position_try_order =
  check_value_cursor "position_try_order" read_position_try_order
    pp_position_try_order

let check_position_visibility =
  check_value_cursor "position_visibility" read_position_visibility
    pp_position_visibility

let check_position_visibility_condition =
  check_value_cursor "position_visibility_condition"
    read_position_visibility_condition pp_position_visibility_condition

let check_ray = check_value_cursor "ray" read_ray pp_ray
let check_ray_size = check_value_cursor "ray_size" read_ray_size pp_ray_size

let check_resolution =
  check_value_cursor "resolution" read_resolution pp_resolution

let check_ruby_align =
  check_value_cursor "ruby_align" read_ruby_align pp_ruby_align

let check_ruby_merge =
  check_value_cursor "ruby_merge" read_ruby_merge pp_ruby_merge

let check_ruby_overhang =
  check_value_cursor "ruby_overhang" read_ruby_overhang pp_ruby_overhang

let check_ruby_position =
  check_value_cursor "ruby_position" read_ruby_position pp_ruby_position

let check_ruby_position_keyword =
  check_value_cursor "ruby_position_keyword" read_ruby_position_keyword
    pp_ruby_position_keyword

let check_scrollbar_color =
  check_value_cursor "scrollbar_color" read_scrollbar_color pp_scrollbar_color

let check_scrollbar_gutter =
  check_value_cursor "scrollbar_gutter" read_scrollbar_gutter
    pp_scrollbar_gutter

let check_scrollbar_width =
  check_value_cursor "scrollbar_width" read_scrollbar_width pp_scrollbar_width

let check_shape_image_threshold =
  check_value_cursor "shape_image_threshold" read_shape_image_threshold
    pp_shape_image_threshold

let check_tab_size = check_value_cursor "tab_size" read_tab_size pp_tab_size
let check_zoom = check_value_cursor "zoom" read_zoom pp_zoom
let check_text_box = check_value_cursor "text_box" read_text_box pp_text_box

let check_text_box_edge =
  check_value_cursor "text_box_edge" read_text_box_edge pp_text_box_edge

let check_text_box_edge_keyword =
  check_value_cursor "text_box_edge_keyword" read_text_box_edge_keyword
    pp_text_box_edge_keyword

let check_text_box_trim =
  check_value_cursor "text_box_trim" read_text_box_trim pp_text_box_trim

let check_text_combine_upright =
  check_value_cursor "text_combine_upright" read_text_combine_upright
    pp_text_combine_upright

let check_text_decoration_skip =
  check_value_cursor "text_decoration_skip" read_text_decoration_skip
    pp_text_decoration_skip

let check_text_decoration_skip_box =
  check_value_cursor "text_decoration_skip_box" read_text_decoration_skip_box
    pp_text_decoration_skip_box

let check_text_decoration_skip_inset =
  check_value_cursor "text_decoration_skip_inset"
    read_text_decoration_skip_inset pp_text_decoration_skip_inset

let check_text_decoration_skip_self =
  check_value_cursor "text_decoration_skip_self" read_text_decoration_skip_self
    pp_text_decoration_skip_self

let check_text_decoration_skip_space =
  check_value_cursor "text_decoration_skip_space"
    read_text_decoration_skip_space pp_text_decoration_skip_space

let check_text_decoration_skip_spaces =
  check_value_cursor "text_decoration_skip_spaces"
    read_text_decoration_skip_spaces pp_text_decoration_skip_spaces

let check_text_emphasis =
  check_value_cursor "text_emphasis" read_text_emphasis pp_text_emphasis

let check_text_emphasis_fill =
  check_value_cursor "text_emphasis_fill" read_text_emphasis_fill
    pp_text_emphasis_fill

let check_text_emphasis_line =
  check_value_cursor "text_emphasis_line" read_text_emphasis_line
    pp_text_emphasis_line

let check_text_emphasis_position =
  check_value_cursor "text_emphasis_position" read_text_emphasis_position
    pp_text_emphasis_position

let check_text_emphasis_shape =
  check_value_cursor "text_emphasis_shape" read_text_emphasis_shape
    pp_text_emphasis_shape

let check_text_emphasis_side =
  check_value_cursor "text_emphasis_side" read_text_emphasis_side
    pp_text_emphasis_side

let check_text_emphasis_skip =
  check_value_cursor "text_emphasis_skip" read_text_emphasis_skip
    pp_text_emphasis_skip

let check_text_emphasis_skip_keyword =
  check_value_cursor "text_emphasis_skip_keyword"
    read_text_emphasis_skip_keyword pp_text_emphasis_skip_keyword

let check_text_emphasis_style =
  check_value_cursor "text_emphasis_style" read_text_emphasis_style
    pp_text_emphasis_style

let check_text_orientation =
  check_value_cursor "text_orientation" read_text_orientation
    pp_text_orientation

let check_text_spacing_trim =
  check_value_cursor "text_spacing_trim" read_text_spacing_trim
    pp_text_spacing_trim

let check_text_underline_position =
  check_value_cursor "text_underline_position" read_text_underline_position
    pp_text_underline_position

let check_text_underline_position_keyword =
  check_value_cursor "text_underline_position_keyword"
    read_text_underline_position_keyword pp_text_underline_position_keyword

let check_text_wrap_mode =
  check_value_cursor "text_wrap_mode" read_text_wrap_mode pp_text_wrap_mode

let check_text_wrap_style =
  check_value_cursor "text_wrap_style" read_text_wrap_style pp_text_wrap_style

let check_timeline_inset =
  check_value_cursor "timeline_inset" read_timeline_inset pp_timeline_inset

let check_timeline_inset_item =
  check_value_cursor "timeline_inset_item" read_timeline_inset_item
    pp_timeline_inset_item

let check_timeline_name =
  check_value_cursor "name" read_timeline_name pp_timeline_name

let check_timeline_shorthand_item =
  check_value_cursor "timeline_shorthand_item" read_timeline_shorthand_item
    pp_timeline_shorthand_item

let check_view_timeline_shorthand_item =
  check_value_cursor "view_timeline_shorthand_item"
    read_view_timeline_shorthand_item pp_view_timeline_shorthand_item

let check_view_transition_class =
  check_value_cursor "view_transition_class" read_view_transition_class
    pp_view_transition_class

let check_view_transition_name =
  check_value_cursor "view_transition_name" read_view_transition_name
    pp_view_transition_name

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
  neg_cursor read_zoom "not-a-zoom"

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

let test_font_style () =
  check_font_style "normal";
  check_font_style "italic";
  check_font_style "oblique";
  check_font_style "oblique 45deg";
  check_font_style "inherit";
  neg_cursor read_font_style "invalid";
  neg_cursor read_font_style "italics";
  (* common typo *)
  neg_cursor ~allow_partial:true read_font_style "normal italic"

let test_font_display () =
  check_font_display "auto";
  check_font_display "block";
  check_font_display "swap";
  check_font_display "fallback";
  check_font_display "optional";
  neg_cursor read_font_display "invalid";
  neg_cursor read_font_display "inline";
  neg_cursor ~allow_partial:true read_font_display "auto block"

let test_unicode_range () =
  (* Single code points per CSS spec *)
  check_unicode_range ~expected:"U+0" "U+0000";
  check_unicode_range ~expected:"U+26" "U+26";
  (* ampersand example from MDN *)
  check_unicode_range ~expected:"U+FF" "U+00FF";
  (* case insensitive *)
  check_unicode_range ~expected:"U+ABCD" "U+abcd";
  (* Code point ranges per CSS spec *)
  check_unicode_range ~expected:"U+??" "U+0000-00FF";
  check_unicode_range ~expected:"U+25-FF" "U+0025-00FF";
  (* MDN example format *)
  check_unicode_range ~expected:"U+20-7F" "U+0020-007F";
  (* ASCII printable range *)
  check_unicode_range ~expected:"U+A0-A0FF" "U+A0-A0FF";
  neg_cursor read_unicode_range "invalid";
  neg_cursor read_unicode_range "U+";
  neg_cursor read_unicode_range "U+GGGG";
  neg_cursor ~allow_partial:true read_unicode_range "U+1234-";
  neg_cursor ~allow_partial:true read_unicode_range "U+1234-GGGG";
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
  check_text_align "match-parent";
  check_text_align "-webkit-match-parent";
  check_text_align "inherit";
  neg_cursor read_text_align "invalid-align";
  (* vertical align, not text align *)
  neg_cursor read_text_align "middle";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_text_align "left right";
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
  neg_cursor ~allow_partial:true read_text_decoration_style "solid dotted";
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
  neg_cursor ~allow_partial:true read_text_overflow "clip ellipsis clip"

let test_text_wrap () =
  check_text_wrap "wrap";
  check_text_wrap "nowrap";
  check_text_wrap "auto";
  check_text_wrap "balance";
  check_text_wrap "stable";
  check_text_wrap "pretty";
  check_text_wrap "nowrap balance";
  check_text_wrap ~expected:"nowrap balance" "balance nowrap";
  check_text_wrap "inherit";
  neg_cursor read_text_wrap "invalid-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_text_wrap "wrap nowrap";
  neg_cursor ~allow_partial:true read_text_wrap "balance pretty";
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
  check_white_space "collapse";
  check_white_space "inherit";
  neg_cursor read_white_space "invalid-space";
  (* hyphenated form incorrect *)
  neg_cursor read_white_space "no-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_white_space "normal nowrap";
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
  neg_cursor ~allow_partial:true read_word_break "normal break-all"

let test_overflow_wrap () =
  check_overflow_wrap "normal";
  check_overflow_wrap "break-word";
  check_overflow_wrap "anywhere";
  check_overflow_wrap "inherit";
  neg_cursor read_overflow_wrap "invalid-wrap";
  neg_cursor ~allow_partial:true read_overflow_wrap "normal break-word";
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
  neg_cursor ~allow_partial:true read_hyphens "auto manual";
  (* contradictory *)
  (* wrong form *)
  neg_cursor read_hyphens "hyphenate"

let test_line_height () =
  (* Only test values supported by the simplified reader *)
  check_line_height "normal";
  check_line_height "inherit";
  check_line_height "1.5";
  check_line_height "120%";
  decl_optimizes ~prop:"line-height" ~held:"calc(28/18)" ~into:"1.55556"
    "calc(28 / 18)";
  decl_lossless ~prop:"line-height" ~into:"calc(28/18)" "calc(28 / 18)";
  decl_optimizes ~prop:"line-height" ~held:"calc(10 + 8)" ~into:"18"
    "calc(10 + 8)";
  decl_optimizes ~prop:"line-height" ~held:"calc(7*4)" ~into:"28" "calc(7 * 4)";
  decl_optimizes ~prop:"line-height" ~held:"calc((28/18))" ~into:"1.55556"
    "calc((28 / 18))";
  decl_optimizes ~prop:"line-height" ~held:"calc(28/var(--x))"
    ~into:"calc(28/var(--x))" "calc(28 / var(--x))";
  neg_cursor read_line_height "invalid";
  neg_cursor read_line_height "-1.5";
  (* CSS Inline 3 sec. 5.1 spells the property [normal | <number [0,inf]> |
     <length-percentage [0,inf]>], so every length unit reads and nothing else
     does. *)
  check_line_height "2ch";
  check_line_height "3cqw";
  neg_cursor read_line_height "0s";
  neg_cursor read_line_height "45deg";
  neg_cursor read_line_height "10zz";
  (* negative line-height *)
  (* multiple values *)
  neg_cursor ~allow_partial:true read_line_height "normal 1.5"

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
  (* pp holds the authored transform function and only minifies whitespace;
     folding rotateZ/rotate3d to the shorter equivalent function is an optimize
     transform, not a pp serialization. *)
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
  check_transform "skew( 10deg , 20deg )" ~expected:"skew(10deg,20deg)";
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
  neg_cursor read_transform "matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0)";
  (* CSS Transforms 1 sec. 13.2 gives [skew()] one or two [<angle>], and
     [matrix()]/[matrix3d()] take exactly six/sixteen [<number>] (sec. 12.1, CSS
     Transforms 2 sec. 12.2); a trailing argument outside that grammar is
     invalid (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require these
     readers to consume their whole sub-cursor, so each read only its prefix and
     answered with a truncated function, e.g. [skew(10deg,red)] as
     [skew(10deg)]. *)
  neg_cursor read_transform "skew(10deg,red)";
  neg_cursor read_transform "matrix(1,2,3,4,5,6,red)";
  neg_cursor read_transform "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,red)";
  (* A seventeenth, well-formed number is the same over-count as [red] above,
     not a different bug: the exact-arity match already rejects it, with or
     without the [expect_eof] fix. *)
  neg_cursor read_transform "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,2)";
  (* The same gap in [Cursor.call] itself (not just the readers #617/#627
     already patched): CSS Transforms 1 sec. 7.1 gives each one-argument
     function exactly one [<length-percentage>] / [<angle>] / [<number>], CSS
     Transforms 2 sec. 12.1-12.2 gives the 3d/axis forms a fixed comma count,
     and none of these readers checked that its sub-cursor was exhausted, so
     e.g. [translateX(10px red)] read only [10px] and answered
     [translateX(10px)] instead of invalidating the declaration. *)
  List.iter
    (neg_cursor read_transform)
    [
      "translateX(10px red)";
      "translateY(10px red)";
      "translateZ(10px red)";
      "translate3d(10px,20px,30px,red)";
      "translate(10px red)";
      "translate(10px,20px,red)";
      "rotateX(45deg red)";
      "rotateY(45deg red)";
      "rotateZ(45deg red)";
      "rotate(45deg red)";
      "rotate3d(1,0,0,45deg,red)";
      "scaleX(2 red)";
      "scaleY(2 red)";
      "scaleZ(2 red)";
      "scale3d(2,3,4,red)";
      "skewX(10deg red)";
      "skewY(10deg red)";
    ];
  (* Controls: whitespace directly inside the parens still stands. *)
  check_transform "translateX( 10px )" ~expected:"translateX(10px)";
  check_transform "rotate( 45deg )" ~expected:"rotate(45deg)";
  check_transform "scaleX( 2 )" ~expected:"scaleX(2)"

let test_transforms () =
  (* Adjacent [var()] references in a transform list keep a single separating
     space. Both Lightning CSS and cssnano preserve the space here because a
     [var()] expansion at computed-value time may require whitespace to delimit
     transform functions; concrete function chains can drop the space, [var()]
     chains cannot. *)
  check_transforms ~expected:"var(--tw-rotate-x,) var(--tw-rotate-y,)"
    "var(--tw-rotate-x,) var(--tw-rotate-y,)";
  check_transforms
    ~expected:"var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)";
  check_transforms
    ~expected:
      "var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) \
       var(--tw-skew-x,) var(--tw-skew-y,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)var(--tw-skew-x,)var(--tw-skew-y,)";
  (* Per CSS Transforms 1 section 8 the printer drops whitespace between
     back-to-back transform functions under minify. *)
  check_transforms ~expected:"translateX(10px)var(--my-rotate)"
    "translateX(10px) var(--my-rotate)";
  check_transforms ~expected:"var(--translate)scale(2)"
    "var(--translate) scale(2)";
  check_transforms ~expected:"rotate(45deg)scale(1.5)"
    "rotate(45deg) scale(1.5)";
  check_transforms "none";
  (* Test transforms variable reference (whole list as var) *)
  check_transforms "var(--my-transforms)";
  check_transforms "var(--my-transforms,none)";
  neg_cursor read_transforms "invalidfunc()";
  neg_cursor read_transforms "translate3d(10px,20px) scale(2)"

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

let test_font_variant_numeric_token () =
  check_font_variant_numeric_token "normal";
  check_font_variant_numeric_token "ordinal";
  check_font_variant_numeric_token "slashed-zero";
  neg_cursor read_font_variant_numeric_token "invalid-token";
  neg_cursor ~allow_partial:true read_font_variant_numeric_token
    "normal ordinal";
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

let test_grid_auto_flow_component () =
  check_grid_auto_flow_component "row";
  check_grid_auto_flow_component "column";
  check_grid_auto_flow_component "dense";
  check_grid_auto_flow_component "var(--flow,dense)";
  neg_cursor read_grid_auto_flow_component "auto-flow";
  neg_cursor read_grid_auto_flow_component "sparse"

let test_background_box () =
  check_background_box "border-box";
  check_background_box "padding-box";
  check_background_box "content-box";
  (* Comma-separated layers are valid; space-separated is not. *)
  decl_optimizes ~prop:"background-clip"
    ~into:"border-box,padding-box,content-box"
    "border-box,padding-box,content-box";
  neg_cursor read_background_box "invalid-box";
  neg_cursor read_background_box "margin-box";
  (* doesn't exist for background *)
  neg_cursor ~allow_partial:true read_background_box "border-box padding-box";
  (* multiple *)
  neg_cursor read_background_box "borderbox";
  (* missing hyphen *)
  (* SVG value, not background *)
  neg_cursor read_background_box "fill-box"

let test_background () =
  check_background "red";
  check_background "url(image.png)";
  (* pp holds the authored side keyword and Named blue; the side->angle and
     Named->hex folds are optimize transforms. *)
  check_background ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background" ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  check_background ~expected:"url(image.png)center/cover no-repeat fixed red"
    "red url(image.png) center/cover no-repeat fixed";
  (* Multi-layer shorthand (CSS Backgrounds 3 sec. 2.1): the layer comma
     separates layers and must not be eaten by a per-component reader. [repeat]
     is the default and folds away; the second layer's [space] stays. *)
  decl_optimizes ~prop:"background" ~into:"url(a.png),url(b.png)space"
    "url(a.png) repeat,url(b.png) space";
  (* pp holds the [none] keyword; the fold to the equivalent, and shorter, [0 0]
     layer is an optimize transform (test_declaration pins it). *)
  check_background "none";
  neg_cursor read_background "invalid-background";
  neg_cursor ~allow_partial:true read_background "red blue";
  (* multiple colors without gradient *)
  (* Per CSS Images Module, [url()] with an empty URL is spec-valid. *)
  neg_cursor read_background "center center center";
  (* too many positions *)
  (* invalid position syntax *)
  neg_cursor read_background "10px 20px 30px 40px 50px"

let test_font_weight () =
  (* CSS Fonts 4 (ED) sec. 2.2 makes [normal] and [bold] the numbers 400 and
     700, but swapping one for the other is a node change, so pp prints the
     keyword it was handed and the optimizer folds (test_declaration pins the
     fold). *)
  check_font_weight "normal";
  check_font_weight "bold";
  check_font_weight "700";
  check_font_weight "650.5";
  check_font_weight "1000";
  check_font_weight "lighter";
  neg_cursor read_font_weight "invalid-weight";
  neg_cursor read_font_weight "0.5";
  neg_cursor read_font_weight "1001";
  (* out of range *)
  neg_cursor ~allow_partial:true read_font_weight "normal bold";
  (* multiple values *)
  (* not a valid keyword *)
  neg_cursor read_font_weight "extra-bold"

let test_text_transform () =
  check_text_transform "uppercase";
  check_text_transform "lowercase";
  check_text_transform "capitalize";
  check_text_transform "none";
  neg_cursor read_text_transform "invalid-transform";
  neg_cursor ~allow_partial:true read_text_transform "uppercase lowercase";
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
  neg_cursor ~allow_partial:true read_text_decoration_line
    "underline overline underline";
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

let test_text_decoration () =
  check_text_decoration "underline";
  check_text_decoration "line-through";
  check_text_decoration ~expected:"none" "none";
  neg_cursor ~allow_partial:true read_text_decoration "invalid-decoration";
  (* duplicate - per CSS spec, || combinator means each component at most
     once *)
  neg_cursor read_text_decoration "underline line-through underline";
  (* A style or colour is valid without a line: every component is optional. *)
  check_text_decoration "solid";
  check_text_decoration "red"

let test_text_decoration_shorthand () =
  (* Test individual parts. The printer holds every component it parsed;
     dropping one that equals its longhand initial is a node change the
     optimizer makes (pinned in test_declaration). *)
  check_text_decoration_shorthand "underline";
  check_text_decoration_shorthand "underline solid";
  check_text_decoration_shorthand ~expected:"underline solid red"
    "underline solid red";
  check_text_decoration_shorthand ~expected:"underline solid red 2px"
    "underline solid red 2px";
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
  decl_optimizes ~prop:"text-decoration" ~held:"underline wavy blue 3px"
    ~into:"underline wavy #00f 3px" "3px wavy blue underline";
  neg_cursor ~allow_partial:true read_text_decoration_shorthand
    "invalid-decoration";
  neg_cursor read_text_decoration_shorthand "underline underline";
  (* duplicate line *)
  neg_cursor read_text_decoration_shorthand "solid solid";
  (* duplicate style *)
  (* multiple colors *)
  neg_cursor ~allow_partial:true read_text_decoration_shorthand "red blue"

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
  decl_optimizes ~prop:"border" ~held:"2px dashed blue" ~into:"2px dashed#00f"
    "blue 2px dashed";
  neg_cursor ~allow_partial:true read_border_shorthand "1px 2px"

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

let test_transition_shorthand () =
  check_transition_shorthand "all";
  check_transition_shorthand "opacity 1s";
  check_transition_shorthand "opacity 1s ease-in";
  check_transition_shorthand ~expected:"opacity 1s ease-in .5s"
    "opacity 1s ease-in 0.5s";
  check_transition_shorthand "width 2s";
  check_transition_shorthand ~expected:"all .3s linear" "all 0.3s linear";
  neg_cursor ~allow_partial:true read_transition_shorthand "2invalid";
  neg_cursor ~allow_partial:true read_transition_shorthand "-1s";
  (* negative duration. [transition:opacity] (property only, 0s duration) is
     valid - the shorthand components are all optional - so it is not rejected. *)
  (* too many durations *)
  neg_cursor ~allow_partial:true read_transition_shorthand "1s 2s 3s 4s"

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

let test_background_shorthand () =
  check_background_shorthand "red";
  check_background_shorthand "url(image.png)";
  check_background_shorthand "center";
  check_background_shorthand "no-repeat";
  (* pp prints the axis pair it was handed; the fold to the one-keyword spelling
     is an optimize step (test_declaration pins it). *)
  check_background_shorthand "repeat repeat";
  check_background_shorthand ~expected:"url(image.png)red" "red url(image.png)";
  check_background_shorthand ~expected:"url(image.png)center"
    "url(image.png) center";
  check_background_shorthand ~expected:"url(image.png)center no-repeat red"
    "red url(image.png) center no-repeat";
  neg_cursor read_background_shorthand "invalid invalid";
  neg_cursor ~allow_partial:true read_background_shorthand "red blue green";
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
  check_animation_shorthand ~expected:"slide 1s" "1s slide";
  check_animation_shorthand ~expected:"slide 1s" "slide 1s";
  check_animation_shorthand ~expected:"slide 1s ease-in" "slide 1s ease-in";
  check_animation_shorthand ~expected:"slide 1s ease-in .5s infinite"
    "slide 1s ease-in 0.5s infinite";
  check_animation_shorthand ~expected:"slide 1s infinite" "slide 1s infinite";
  check_animation_shorthand ~expected:"slide 1s reverse" "slide 1s reverse";
  check_animation_shorthand ~expected:"slide 1s forwards" "slide 1s forwards";
  check_animation_shorthand ~expected:"slide 1s paused" "slide 1s paused";

  (* Invalid cases *)
  (* invalid time unit *)
  neg_cursor ~allow_partial:true read_animation_shorthand "2invalid";
  (* negative duration *)
  neg_cursor ~allow_partial:true read_animation_shorthand "slide -1s";
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
  neg_cursor ~allow_partial:true read_any_property "font size";
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
  (* Predefined counter styles (CSS Counter Styles 3 sec. 6). *)
  check_list_style_type "decimal-leading-zero";
  check_list_style_type "lower-greek";
  check_list_style_type "lower-latin";
  check_list_style_type "upper-latin";
  check_list_style_type "armenian";
  check_list_style_type "georgian";
  check_list_style_type "cjk-decimal";
  check_list_style_type "hebrew";
  check_list_style_type "katakana";
  check_list_style_type "ethiopic-numeric";
  check_list_style_type "disclosure-open";
  (* CSS Lists 3 section 3.4 accepts custom counter-style names, but the
     <custom-ident> grammar reserves [default]. *)
  neg_cursor read_list_style_type "default";
  (* CSS Lists 3 sec. 6.2: [symbols() = symbols( <symbols-type>? [ <string> |
     <image> ]+ )]. [Cursor.call] did not require the reader to consume its
     whole sub-cursor, so a trailing token the [<symbols-type>? [<string> |
     <image>]+] grammar cannot place was silently dropped instead of
     invalidating the declaration. *)
  check_list_style_type "symbols(cyclic \"a\" \"b\")"
    ~expected:"symbols(cyclic\"a\"\"b\")";
  check_list_style_type "symbols( cyclic \"a\" \"b\" )"
    ~expected:"symbols(cyclic\"a\"\"b\")";
  neg_cursor read_list_style_type "symbols(\"a\" 5)"

let test_list_style_position () =
  check_list_style_position "inside";
  check_list_style_position "outside";
  check_list_style_position "inherit";
  neg_cursor read_list_style_position "middle"

let test_list_style_image () =
  check_list_style_image "none";
  check_list_style_image "inherit";
  check_list_style_image "url(https://example.com/x.png)";
  (* CSS Lists 3 sec. 3.5 gives the property an [<image>], so every image the
     vocabulary reads goes here and only the comma list does not. *)
  check_list_style_image "linear-gradient(red,blue)";
  check_list_style_image "conic-gradient(in hsl longer hue,red,blue)";
  check_list_style_image "image-set(url(a.png)1x)";
  check_list_style_image "cross-fade(url(a.png) 40%,url(b.png))";
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
  (* vertical-align also takes <length-percentage> (CSS2 section 10.8), so a
     length, var(), and a held (non-reducible) calc() are all valid. *)
  check_vertical_align "10px";
  check_vertical_align "var(--v)";
  check_vertical_align "calc(50% + 10px)";
  (* A unitless 0 is the valid zero <length> and stays unitless; any other
     unitless number is not a length. *)
  check_vertical_align "0";
  neg_cursor read_vertical_align "5";
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
  check_font_family ~expected:"Times New Roman" "\"Times New Roman\"";
  check_font_family "Arial";
  (* CSS Fonts 4 sec. 5.1: a [<family-name>] is matched with Default Caseless
     Matching, a caseless string comparison that folds neither a hyphen to a
     space nor one name to another. [open-sans] names a different family from
     [Open Sans] and [ny] a different family from [New York], so the author's
     spelling survives verbatim - re-reading the output must be a fixpoint. *)
  check_font_family ~roundtrip:true "inter";
  check_font_family ~roundtrip:true "open-sans";
  check_font_family ~roundtrip:true "ny";
  check_font_family ~roundtrip:true "sf-pro";
  check_font_family ~roundtrip:true "jetbrains-mono";
  check_font_family ~roundtrip:true "SFMONO-REGULAR";
  (* A [<string>] family name carries no case either; unquoting a single-word
     name keeps its spelling, so the bare form re-reads to the same family. *)
  check_font_family ~roundtrip:true ~expected:"inter" "\"inter\"";
  check_font_family ~roundtrip:true ~expected:"OPEN SANS" "\"OPEN SANS\"";
  check_font_family ~roundtrip:true ~expected:"Open Sans" "\"Open Sans\"";
  (* CSS Syntax 3 sec. 4.3.9 gates when a name may be spelled as an ident at
     all: the first code point must be a name-start code point, or a [-]
     followed by a name-start code point or by a second [-]. A name that fails
     that test has no unquoted spelling - CSS Fonts 4 sec. 2.1.1 leaves only the
     [<string>] arm of [<family-name>] - and emitting it bare makes the browser
     drop the whole declaration. *)
  check_font_family ~roundtrip:true ~expected:"\"2Brand\"" "\"2Brand\"";
  check_font_family ~roundtrip:true ~expected:"\"9\"" "\"9\"";
  check_font_family ~roundtrip:true ~expected:"\"-2x\"" "\"-2x\"";
  check_font_family ~roundtrip:true ~expected:"\"-\"" "\"-\"";
  (* The guard is not a minify choice: the unminified printer picks the same
     spelling. *)
  check_font_family ~minify:false ~roundtrip:true ~expected:"\"2Brand\""
    "\"2Brand\"";
  check_font_family ~minify:false ~roundtrip:true ~expected:"\"-2x\"" "\"-2x\"";
  (* The other side of sec. 4.3.9: a second [-] starts an ident sequence, and
     sec. 4.2 counts [_] as a name-start code point, so these do unquote. *)
  check_font_family ~roundtrip:true ~expected:"--brand" "\"--brand\"";
  check_font_family ~roundtrip:true ~expected:"--" "\"--\"";
  check_font_family ~roundtrip:true ~expected:"-x" "\"-x\"";
  check_font_family ~roundtrip:true ~expected:"_brand" "\"_brand\"";
  check_font_family ~roundtrip:true ~expected:"_" "\"_\"";
  (* Sec. 4.2: [.] is not a name code point, and sec. 4.3.7 reads [\] as the
     start of an escape, so [a\b] would name the family [ab]. Both stay
     quoted. *)
  check_font_family ~roundtrip:true ~expected:"\"Foo.Bar\"" "\"Foo.Bar\"";
  check_font_family ~roundtrip:true ~expected:"\"a\\\\b\"" "\"a\\\\b\"";
  (* Every word of a [<custom-ident>+] sequence is an ident in its own right, so
     the same start rule gates each of them. *)
  check_font_family ~roundtrip:true ~expected:"\"Brand -\"" "\"Brand -\"";
  check_font_family ~roundtrip:true ~expected:"\"3 Rounded\"" "\"3 Rounded\"";
  (* CSS Fonts 4 sec. 2.1 spells [font-family] as [[ <family-name> |
     <generic-family> ]#], so the bare keyword is the generic and only the
     quoted form names a family; CSS Values 4 sec. 4.2 excludes the CSS-wide
     keywords and the reserved [default] from [<custom-ident>]. Unquoting any of
     these changes the meaning rather than the spelling. *)
  check_font_family ~roundtrip:true ~expected:"\"serif\"" "\"serif\"";
  check_font_family ~roundtrip:true ~expected:"\"inherit\"" "\"inherit\"";
  check_font_family ~roundtrip:true ~expected:"\"default\"" "\"default\"";
  (* CSS Fonts 4 sec. 2.1.1 states the exclusion per identifier, not per name:
     "any identifier which could be misinterpreted as a pre-defined keyword in
     the font-family value definition, or the CSS-wide keywords, is not
     allowed". A word of a [<custom-ident>+] sequence is such an identifier, so
     the rule reaches the first, a middle and the last word alike, and a name
     carrying one has no unquoted spelling at all. *)
  check_font_family ~roundtrip:true ~expected:"\"inherit test\""
    "\"inherit test\"";
  check_font_family ~roundtrip:true ~expected:"\"test inherit\""
    "\"test inherit\"";
  check_font_family ~roundtrip:true ~expected:"\"a initial b\""
    "\"a initial b\"";
  check_font_family ~roundtrip:true ~expected:"\"unset Sans\"" "\"unset Sans\"";
  check_font_family ~roundtrip:true ~expected:"\"Brand revert\""
    "\"Brand revert\"";
  check_font_family ~roundtrip:true ~expected:"\"revert-layer x\""
    "\"revert-layer x\"";
  check_font_family ~roundtrip:true ~expected:"\"a revert-layer b\""
    "\"a revert-layer b\"";
  (* CSS Values 4 sec. 4.2 reserves [default] alongside the CSS-wide keywords
     and excludes it from [<custom-ident>] in every position. *)
  check_font_family ~roundtrip:true ~expected:"\"default x\"" "\"default x\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo default Bar\""
    "\"Foo default Bar\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo default\""
    "\"Foo default\"";
  (* Sec. 4.2 excludes the keywords "in all ASCII case permutations". *)
  check_font_family ~roundtrip:true ~expected:"\"INHERIT test\""
    "\"INHERIT test\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo Default\""
    "\"Foo Default\"";
  (* The pre-defined keywords of the [font-family] value definition are the
     [<generic-font-family>] names, and sec. 2.1.1 adds that a UA "must not
     consider these keywords as matching the <font-family-name> type", so a
     sequence word may not be one either. *)
  check_font_family ~roundtrip:true ~expected:"\"Foo serif\"" "\"Foo serif\"";
  check_font_family ~roundtrip:true ~expected:"\"serif Foo\"" "\"serif Foo\"";
  check_font_family ~roundtrip:true ~expected:"\"a sans-serif b\""
    "\"a sans-serif b\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo monospace\""
    "\"Foo monospace\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo cursive\""
    "\"Foo cursive\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo fantasy\""
    "\"Foo fantasy\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo system-ui\""
    "\"Foo system-ui\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo math\"" "\"Foo math\"";
  check_font_family ~roundtrip:true ~expected:"\"Cambria Math\""
    "\"Cambria Math\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-serif\""
    "\"Foo ui-serif\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-sans-serif\""
    "\"Foo ui-sans-serif\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-monospace\""
    "\"Foo ui-monospace\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-rounded\""
    "\"Foo ui-rounded\"";
  (* Sec. 2.1.2 keeps the script-specific generics functional, so bare
     [fangsong] and [emoji] are not pre-defined keywords of the value definition
     and are ordinary [<custom-ident>]s inside a sequence. *)
  check_font_family ~roundtrip:true ~expected:"Foo emoji" "\"Foo emoji\"";
  check_font_family ~roundtrip:true ~expected:"Foo fangsong" "\"Foo fangsong\"";
  check_font_family ~roundtrip:true ~expected:"Noto Color Emoji"
    "\"Noto Color Emoji\"";
  (* The exclusion is on the whole identifier, so a word that merely contains a
     keyword is an ordinary [<custom-ident>] and still unquotes. *)
  check_font_family ~roundtrip:true ~expected:"inherited test"
    "\"inherited test\"";
  check_font_family ~roundtrip:true ~expected:"Foo serifs" "\"Foo serifs\"";
  check_font_family ~roundtrip:true ~expected:"Foo sans" "\"Foo sans\"";
  check_font_family ~roundtrip:true ~expected:"Times New Roman"
    "\"Times New Roman\"";
  let invalid = read_font_family (Cursor.of_string "Arial, inherit") in
  (match invalid with
  | Invalid tokens ->
      Alcotest.(check string)
        "invalid family list source is preserved" "Arial, inherit"
        (Parser.string_of_components tokens)
  | _ -> Alcotest.fail "expected an Invalid font-family node");
  (* Test actual invalid cases *)
  neg_cursor read_font_family "123invalid";
  (* identifier can't start with number *)
  neg_cursor read_font_family "";
  (* Default minify unquotes a multi-word [<family-name>] (CSS Fonts 4 sec.
     2.1.1: an ident sequence is a valid family name and the unquoted form is
     shorter). [enforce_spec] keeps the quotes, matching the CSSOM-canonical
     serialization, so a font stack stored verbatim in a custom property keeps
     its authored quoting. *)
  let stack =
    read_font_family (Cursor.of_string "\"Segoe UI Symbol\",monospace")
  in
  Alcotest.(check string)
    "default minify unquotes a multi-word family name"
    "Segoe UI Symbol,monospace"
    (Css.Pp.to_string ~minify:true pp_font_family stack);
  Alcotest.(check string)
    "enforce_spec keeps a multi-word family name quoted"
    "\"Segoe UI Symbol\",monospace"
    (Css.Pp.to_string ~minify:true ~enforce_spec:true pp_font_family stack)

let test_font_stretch () =
  check_font_stretch "50%";
  check_font_stretch "inherit";
  neg_cursor read_font_stretch "invalid-stretch";
  (* CSS Fonts 4 (ED) sec. 2.3 makes each keyword a percentage, but swapping one
     for the other is a node change, so pp prints the keyword it was handed and
     the optimizer folds (test_declaration pins the fold). *)
  check_font_stretch "normal";
  check_font_stretch "ultra-condensed";
  check_font_stretch "ultra-expanded"

let test_font_variant_numeric () =
  check_font_variant_numeric "normal";
  check_font_variant_numeric "lining-nums";
  check_font_variant_numeric "tabular-nums";
  neg_cursor read_font_variant_numeric "invalid-variant"

let test_font_feature_value () =
  check_font_feature_value "on";
  check_font_feature_value "off";
  check_font_feature_value "2";
  neg_cursor read_font_feature_value "-1";
  neg_cursor read_font_feature_value "1.5"

let test_font_feature_setting () =
  check_font_feature_setting "\"kern\"";
  check_font_feature_setting "\"swsh\" 2";
  check_font_feature_setting ~expected:"\"a\\\"bc\" on" "\"a\\22 bc\" on";
  neg_cursor read_font_feature_setting "\"bad\""

let test_font_feature_settings () =
  check_font_feature_settings "normal";
  check_font_feature_settings "inherit";
  check_font_feature_settings "\"kern\"";
  check_font_feature_settings "\"liga\" 0";
  (* CSS Fonts 4 sec. 6.12 permits every non-negative integer as a feature
     selection index and requires a four-character printable ASCII tag. The tag
     is a CSS string, so its decoded quote must be escaped again by pp. *)
  check_font_feature_settings "\"swsh\" 2";
  check_font_feature_settings ~expected:"\"a\\\"bc\" 3" "\"a\\22 bc\" 3";
  let structured =
    Feature_list
      [
        { tag = "a\"bc"; value = Some (Index 2) };
        { tag = "kern"; value = Some On };
      ]
  in
  check string "structured font feature settings" "\"a\\\"bc\" 2,\"kern\" on"
    (Css.Pp.to_string ~minify:true pp_font_feature_settings structured);
  neg_cursor read_font_feature_settings "\"\\e9 ab\" 1";
  neg_cursor read_font_feature_settings "\"kern\" 1.5";
  neg_cursor read_font_feature_settings "invalid-feature"

let test_font_variation_settings () =
  check_font_variation_settings "normal";
  check_font_variation_settings "inherit";
  check_font_variation_settings "\"wght\" 400";
  (* CSS Fonts 4 sec. 8.2 pairs a printable four-character tag with a number,
     not an integer. Re-serialize the decoded tag through the CSS printer. *)
  check_font_variation_settings "\"wght\" 123.5";
  check_font_variation_settings ~expected:"\"a\\\"bc\" 123.5"
    "\"a\\22 bc\" 123.5";
  let structured =
    Axis_list
      [ { tag = "a\"bc"; value = 123.5 }; { tag = "wght"; value = 650.25 } ]
  in
  check string "structured font variation settings"
    "\"a\\\"bc\" 123.5,\"wght\" 650.25"
    (Css.Pp.to_string ~minify:true pp_font_variation_settings structured);
  neg_cursor read_font_variation_settings "invalid-variation"

let test_font_variation_setting () =
  check_font_variation_setting "\"wght\" 123.5";
  check_font_variation_setting ~expected:"\"a\\\"bc\" 123.5"
    "\"a\\22 bc\" 123.5";
  neg_cursor read_font_variation_setting "\"bad\" 1"

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
  neg_cursor read_timing_function "cubic-bezier()";
  (* CSS Easing 1 sec. 4.2/4.3 give [cubic-bezier()] exactly four [<number>] and
     [steps()] a count and an optional [<step-position>]; a trailing argument is
     invalid (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require either
     reader to consume its whole sub-cursor, so e.g. [steps(4, red)] read only
     the count and answered [steps(4)] instead of invalidating the
     declaration. *)
  check_timing_function "steps(4)";
  check_timing_function "steps(4, jump-start)" ~expected:"steps(4,jump-start)";
  check_timing_function "steps( 4 , jump-start )"
    ~expected:"steps(4,jump-start)";
  check_timing_function "steps(2, jump-none)" ~expected:"steps(2,jump-none)";
  neg_cursor read_timing_function "steps(1.9)";
  neg_cursor read_timing_function "steps(1, jump-none)";
  neg_cursor read_timing_function "steps(4, jump-start, red)";
  neg_cursor read_timing_function "steps(4 red)";
  neg_cursor read_timing_function "cubic-bezier(0.1,0.7,1,0.1,0.5)"

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
  neg_cursor ~allow_partial:true read_transition "2invalid"

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
  check_animation ~expected:"slide-in 1s" "slide-in 1s";
  check_animation ~expected:"my-animation 2s ease-in 1s infinite alternate"
    "my-animation 2s ease-in 1s infinite alternate";
  (* Test invalid animation shorthand according to CSS spec *)
  neg_cursor read_animation "1s 2s 3s";
  (* More than 2 time values *)
  neg_cursor read_animation "-2s";
  (* Negative duration is invalid *)
  neg_cursor ~allow_partial:true read_animation
    "2s -1" (* Negative iteration count is invalid *)

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
  check_background_attachment "scroll,fixed,local";
  neg_cursor read_background_attachment "invalid-attachment"

let test_background_repeat () =
  check_background_repeat "repeat";
  check_background_repeat "space";
  check_background_repeat "round";
  check_background_repeat "no-repeat";
  check_background_repeat "repeat-x";
  check_background_repeat "repeat-y";
  check_background_repeat "inherit";
  (* CSS Backgrounds 3 sec. 2.6: the longhand is a comma-separated layer
     list. *)
  decl_optimizes ~prop:"background-repeat" ~into:"no-repeat,repeat-y,no-repeat"
    "no-repeat,repeat-y,no-repeat";
  decl_optimizes ~prop:"background-repeat" ~into:"repeat-x,repeat-y"
    "repeat-x,repeat-y";
  decl_optimizes ~prop:"background-repeat" ~into:"repeat space,no-repeat"
    "repeat space,no-repeat";
  neg_cursor read_background_repeat "invalid-repeat"

let test_background_size () =
  check_background_size "auto";
  check_background_size "cover";
  check_background_size "contain";
  check_background_size "50px";
  check_background_size "50%";
  check_background_size "inherit";
  (* <bg-size> components are <length-percentage> (CSS Backgrounds 3), so var()
     and calc() are valid; a non-reducible calc() is preserved, including in the
     two-value form. *)
  check_background_size "var(--s)";
  check_background_size "calc(50% + 10px)";
  check_background_size "calc(50% + 10px) auto";
  (* Comma-separated layer list (CSS Backgrounds 3 sec. 2.9). *)
  decl_optimizes ~prop:"background-size" ~into:"cover,contain" "cover,contain";
  decl_optimizes ~prop:"background-size" ~into:"100px 200px,auto"
    "100px 200px,auto";
  neg_cursor read_background_size "invalid-size"

let test_gradient_direction () =
  (* "to <side>" is a <side-or-corner>, a distinct node from the <angle> it
     equals (corners like "to top right" are not fixed angles), so pp holds the
     authored keyword; converting a side to its angle is an optimize
     transform. *)
  check_gradient_direction "to top";
  check_gradient_direction "to right";
  check_gradient_direction "to bottom";
  check_gradient_direction "to left";
  (* optimize+minify converts side keywords to angles, then elides the default
     bottom direction. Stops are already-canonical colours so only the direction
     changes. *)
  let optimizes ~into dir =
    let into =
      if into = "" then "linear-gradient(red,#123456)"
      else "linear-gradient(" ^ into ^ ",red,#123456)"
    in
    decl_optimizes ~prop:"background" ~into
      ("linear-gradient(" ^ dir ^ ",red,#123456)")
  in
  optimizes ~into:"90deg" "to right";
  optimizes ~into:"" "to bottom";
  optimizes ~into:"270deg" "to left";
  (* [to top] is the one side keyword whose angle also disappears: it is the
     default direction turned 180 degrees, so reversing the stops absorbs it. *)
  decl_optimizes ~prop:"background" ~into:"linear-gradient(#123456,red)"
    "linear-gradient(to top,red,#123456)";
  neg_cursor read_gradient_direction "invalid-direction"

(* CSS Images 4 sections 6.1-6.3: the gradient prelude is a linear direction, a
   radial shape/size/position, or a conic angle/position; a typed custom
   property holding a gradient prelude accepts any of the three. *)
let test_gradient_position () =
  check_gradient_position "to right";
  check_gradient_position "45deg";
  check_gradient_position "to right in oklab";
  (* No explicit direction: the interpolation prints without a redundant [to
     bottom], distinct from [to bottom in <interp>]. *)
  check_gradient_position "in oklab";
  check_gradient_position "in oklch shorter hue";
  check_gradient_position "circle at center";
  check_gradient_position "closest-side";
  check_gradient_position "at center";
  check_gradient_position "from 45deg";
  check_gradient_position "from 90deg at left top";
  check_gradient_position "var(--gradient-position)";
  neg_cursor ~allow_partial:true read_gradient_position "invalid-position"

let test_gradient_stop () =
  (* Basic color stops *)
  check_gradient_stop "red";
  check_gradient_stop "blue 50%";
  check_gradient_stop ~expected:"#ff5733 25%" "#ff5733 25%";
  check_gradient_stop ~expected:"rgb(255 0 0) 10px" "rgb(255,0,0) 10px";
  decl_optimizes ~prop:"background-image" ~into:"linear-gradient(#00f 50%,red)"
    "linear-gradient(blue 50%,red)";
  decl_optimizes ~prop:"background-image" ~into:"linear-gradient(red 10px,#00f)"
    "linear-gradient(rgb(255,0,0) 10px,blue)";

  (* Double position stops *)
  check_gradient_stop ~expected:"green 20% 40%" "green 20% 40%";

  (* Hint positions *)
  check_gradient_stop "50%";
  check_gradient_stop "10px";

  (* CSS variables in gradient stops *)
  check_gradient_stop "var(--tw-gradient-from)";
  check_gradient_stop "var(--color-blue-500) 50%";

  (* Complex var with fallback *)
  check_gradient_stop "var(--tw-gradient-from) var(--tw-gradient-from-position)";

  (* Multiple vars in sequence (as used in Tailwind gradients) *)
  check_gradient_stop ~expected:"var(--tw-gradient-position)"
    "var(--tw-gradient-position)";

  (* Nested var with complex fallback *)
  check_gradient_stop
    ~expected:
      "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from) \
       var(--tw-gradient-from-position),var(--tw-gradient-to) \
       var(--tw-gradient-to-position))"
    "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
     var(--tw-gradient-from) var(--tw-gradient-from-position), \
     var(--tw-gradient-to) var(--tw-gradient-to-position))";

  (* Invalid stops *)
  neg_cursor read_gradient_stop "invalid-stop";
  neg_cursor ~allow_partial:true read_gradient_stop "red blue";
  (* Two colors without position *)
  neg_cursor ~allow_partial:true read_gradient_stop
    "50% 25%" (* Two percentages without color *)

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

let test_hue_interpolation_method () =
  check_hue_interpolation_method "shorter hue";
  check_hue_interpolation_method "longer hue";
  check_hue_interpolation_method "increasing hue";
  check_hue_interpolation_method "decreasing hue";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "shorter";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "hue";
  neg_cursor ~allow_partial:true read_hue_interpolation_method "unknown hue"

(* ignore-test: url() escaping spans the printers, not a single property. *)
let test_url_escaping () =
  (* CSS Syntax 4.3.5: a quoted url() must escape the backslash and the
     delimiter quote it wraps, or the re-parse drops or mangles those bytes.
     Both the minify printer (Pp.url) and the pretty printer (pp_quoted_url)
     route through the same escaper. *)
  let render ~minify css =
    match Css.of_string css with
    | Ok { Css.stylesheet; _ } ->
        Css.to_string ~minify stylesheet |> String.trim
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let minify = render ~minify:true in
  let stable css =
    let canon = minify css in
    (* The minified output and the pretty output must both re-parse to the same
       value, i.e. neither printer lost or mangled a byte. *)
    Alcotest.(check string) ("minify idempotent " ^ css) canon (minify canon);
    Alcotest.(check string)
      ("pretty preserves " ^ css)
      canon
      (minify (render ~minify:false css))
  in
  Alcotest.(check string)
    "minify escapes the delimiter quote" {|.a{background:url("ab\"cd")}|}
    (minify {|.a{background:url("ab\"cd")}|});
  Alcotest.(check string)
    "minify escapes the backslash" {|.a{background:url("a\\b")}|}
    (minify {|.a{background:url("a\\b")}|});
  stable {|.a{background:url("ab\"cd")}|};
  stable {|.a{background:url("a\\b")}|};
  stable {|.a{background:url('x\\y')}|}

let test_background_image () =
  check_background_image "none";
  check_background_image "url(image.jpg)";
  (* CSS Images 4 linear-gradient prelude: [ <angle> | to <side-or-corner> ]? ||
     <color-interpolation-method>. A direction, an interpolation method, or both
     in either order are valid. *)
  (* pp holds the authored side keyword and Named blue; side->angle and
     Named->hex are optimize folds. The direction/interpolation pair is stored
     order-independently, so pp emits the canonical direction-first order. *)
  check_background_image ~expected:"linear-gradient(in oklab,red,blue)"
    "linear-gradient(in oklab, red, blue)";
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient(to right, red, blue)";
  check_background_image ~expected:"linear-gradient(to right in oklab,red,blue)"
    "linear-gradient(to right in oklab, red, blue)";
  check_background_image ~expected:"linear-gradient(to right in oklab,red,blue)"
    "linear-gradient(in oklab to right, red, blue)";
  (* optimize+minify folds the side keyword to its angle and Named blue to
     hex. *)
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(in oklab,red,#00f)"
    "linear-gradient(in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg,red,#00f)"
    "linear-gradient(to right, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg in oklab,red,#00f)"
    "linear-gradient(to right in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"linear-gradient(90deg in oklab,red,#00f)"
    "linear-gradient(in oklab to right, red, blue)";
  check_background_image ~minify:false
    ~expected:"linear-gradient(in oklab, red, blue)"
    "linear-gradient(in oklab, red, blue)";
  check_background_image ~minify:false
    ~expected:"linear-gradient(to right in oklab, red, blue)"
    "linear-gradient(in oklab to right, red, blue)";
  check_background_image ~expected:"radial-gradient(in oklab,red,blue)"
    "radial-gradient(in oklab, red, blue)";
  check_background_image
    ~expected:"radial-gradient(in oklab circle at center,red,blue)"
    "radial-gradient(in oklab circle at center, red, blue)";
  check_background_image
    ~expected:"radial-gradient(in oklab circle at center,red,blue)"
    "radial-gradient(circle at center in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab,red,#00f)"
    "radial-gradient(in oklab, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab circle,red,#00f)"
    "radial-gradient(in oklab circle at center, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"radial-gradient(in oklab circle,red,#00f)"
    "radial-gradient(circle at center in oklab, red, blue)";
  check_background_image ~expected:"conic-gradient(in hsl longer hue,red,blue)"
    "conic-gradient(in hsl longer hue, red, blue)";
  check_background_image
    ~expected:"conic-gradient(in hsl longer hue from 45deg at center,red,blue)"
    "conic-gradient(in hsl longer hue from 45deg at center, red, blue)";
  check_background_image
    ~expected:"conic-gradient(in hsl longer hue from 45deg at center,red,blue)"
    "conic-gradient(from 45deg at center in hsl longer hue, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue,red,#00f)"
    "conic-gradient(in hsl longer hue, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue from 45deg at 50%,red,#00f)"
    "conic-gradient(in hsl longer hue from 45deg at center, red, blue)";
  decl_optimizes ~prop:"background-image"
    ~into:"conic-gradient(in hsl longer hue from 45deg at 50%,red,#00f)"
    "conic-gradient(from 45deg at center in hsl longer hue, red, blue)";
  check_background_image ~minify:false
    ~expected:"radial-gradient(in oklab, red, blue)"
    "radial-gradient(in oklab, red, blue)";
  check_background_image ~minify:false
    ~expected:"conic-gradient(in hsl longer hue, red, blue)"
    "conic-gradient(in hsl longer hue, red, blue)";
  (* CSS Images 4 sec. 3.5.6 gives the color-stop-list a comma-separated list of
     stops and hints, so a trailing item outside that grammar is invalid (CSS
     Syntax 3 sec. 8.2). [Cursor.call] did not require [linear-gradient()] /
     [radial-gradient()]'s reader to consume its whole sub-cursor, so the list
     was read up to the first unreadable stop and answered with a truncated
     gradient instead. *)
  check_background_image "linear-gradient(red,blue)";
  check_background_image "radial-gradient(red,blue)";
  check_background_image "repeating-linear-gradient(var(--stops))";
  check_background_image "repeating-radial-gradient(var(--stops))";
  check_background_image "repeating-conic-gradient(var(--stops))";
  check_background_image "-webkit-radial-gradient(var(--stops))";
  check_background_image "-webkit-repeating-radial-gradient(var(--stops))";
  check_background_image "-moz-radial-gradient(var(--stops))";
  check_background_image "-moz-repeating-radial-gradient(var(--stops))";
  check_background_image "-o-radial-gradient(var(--stops))";
  check_background_image "-o-repeating-radial-gradient(var(--stops))";
  neg_cursor read_background_image "linear-gradient(red, blue, bogus-token)";
  neg_cursor read_background_image "radial-gradient(red, blue, bogus-token)";
  (* CSS Images 4 sec. 6.2 gives [image-set()] a comma-separated
     [<image-set-option>]#; same [Cursor.call] gap in the option list. *)
  check_background_image ~expected:"image-set(\"a.png\"1x)"
    "image-set(\"a.png\" 1x)";
  neg_cursor read_background_image "image-set(\"a.png\" 1x, bogus-token)";
  (* [conic-gradient()] (sec. 3.6) reads its stop list the same way. *)
  neg_cursor read_background_image
    "conic-gradient(from 45deg, red, bogus-token)";
  (* Controls: whitespace around the commas and just inside the parens is not
     trailing content, so each call still stands. *)
  check_background_image ~expected:"linear-gradient(to right,red,blue)"
    "linear-gradient( to right , red , blue )";
  check_background_image ~expected:"image-set(\"a.png\"1x,\"b.png\"2x)"
    "image-set( \"a.png\" 1x , \"b.png\" 2x )";
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
  check_radial_gradient_config "circle at center";
  neg_cursor ~allow_partial:true read_radial_gradient_config "invalid-config";
  (* pp holds the authored defaults; the optimizer elides them (CSS Images 4
     section 3.2: ellipse / farthest-corner / center are implied). *)
  decl_optimizes ~prop:"background"
    ~held:"radial-gradient(circle at center,red,#123456)"
    ~into:"radial-gradient(circle,red,#123456)"
    "radial-gradient(circle at center, red, #123456)";
  decl_optimizes ~prop:"background"
    ~held:"radial-gradient(ellipse farthest-corner,red,#123456)"
    ~into:"radial-gradient(red,#123456)"
    "radial-gradient(ellipse farthest-corner, red, #123456)"

let test_conic_gradient_config () =
  check_conic_gradient_config "from 45deg";
  check_conic_gradient_config "at center";
  check_conic_gradient_config "from 90deg at left top";
  neg_cursor read_conic_gradient_config "";
  neg_cursor read_conic_gradient_config "center";
  neg_cursor read_conic_gradient_config "from"

(* CSS Syntax 3 sec. 4.3.3 consumes the [%] into the percentage token, so
   whatever follows starts a fresh token and the separating space carries no
   meaning: it elides under minify, as it already does inside polygon() and
   animation-range. A unit ends in an ident instead ([10px 0] would re-tokenise
   as the single dimension [10px0]), so that space stays. *)
let test_background_position () =
  let assert_complete input =
    let t = Cursor.of_string input in
    ignore (read_background_position t);
    Cursor.ws t;
    if not (Cursor.is_done t) then
      Alcotest.failf "background-position left input after %S" input
  in
  let assert_axis_edge input axis edge =
    let t = Cursor.of_string input in
    match read_background_position t with
    | [ Axis_edge_offset (got_axis, got_edge, _) ] ->
        Alcotest.(check string) input axis got_axis;
        Alcotest.(check string) input edge got_edge
    | _ -> Alcotest.failf "background-position lost edge offset in %S" input
  in
  check_background_position "center";
  check_background_position "left top";
  check_background_position ~roundtrip:true ~expected:"100%0" "right 0";
  check_background_position ~roundtrip:true ~expected:"100%-15.625rem"
    "right -15.625rem";
  check_background_position "right .5rem center";
  check_background_position "left top 10px";
  check_background_position "left top 10%";
  check_background_position "center top 10px";
  check_background_position "center left 10px";
  check_background_position "top 10px left";
  List.iter assert_complete
    [
      "left top 10px";
      "left top 10%";
      "center top 10px";
      "center left 10px";
      "top 10px left";
    ];
  assert_axis_edge "center top 10px" "center" "top";
  assert_axis_edge "center left 10px" "center" "left";
  check_background_position ~roundtrip:true ~expected:"50%25%" "50% 25%";
  check_background_position "inherit";
  neg_cursor ~allow_partial:true read_background_position "invalid-position"

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

let test_offset_anchor () =
  check_offset_anchor "auto";
  check_offset_anchor "center";
  check_offset_anchor "left top";
  check_offset_anchor "left 10px top 20px";
  check_offset_anchor "var(--anchor,left top)";
  check_offset_anchor "inherit";
  neg_cursor read_offset_anchor "normal";
  neg_cursor read_offset_anchor "size"

let test_offset_position () =
  check_offset_position "normal";
  check_offset_position "auto";
  check_offset_position "center";
  check_offset_position "20% 30%" ~expected:"20%30%";
  check_offset_position "left 10px top 20px";
  check_offset_position "var(--position,center)";
  check_offset_position "inherit";
  neg_cursor read_offset_position "none";
  neg_cursor read_offset_position "size"

(* Motion Path 1 (ED) sec. 2.6 gives the [offset] shorthand

   [ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
   <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?

   and says "Omitted values are set to their initial values". The [!] makes the
   leading group required, so a declaration writes at least a position or a
   path; [offset-distance] and [offset-rotate] live inside the path branch and
   are unreachable without one. *)
let offset_sheet css : Css.parse =
  match Css.of_string ~strict:false css with
  | Ok parse -> parse
  | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)

let offset_minified css =
  String.trim (Css.to_string ~minify:true (offset_sheet css).stylesheet)

(* A value the grammar rejects is dropped from the sheet and reported, not
   carried through as opaque text. *)
let offset_rejects value =
  let css = String.concat "" [ ".x{offset:"; value; "}" ] in
  Alcotest.(check string) (css ^ " is dropped") "" (offset_minified css);
  Alcotest.(check bool)
    (css ^ " warns") true
    (match (offset_sheet css).warnings with [] -> false | _ :: _ -> true)

(* [css] survives parse / print / parse unchanged. *)
let offset_roundtrips css =
  let once = offset_minified css in
  Alcotest.(check string) (css ^ " roundtrip") css once;
  Alcotest.(check string)
    (css ^ " roundtrip is stable")
    once (offset_minified once)

let test_offset () =
  (* Each slot on its own: the position branch takes [normal | auto |
     <position>] (sec. 2.3), the path branch [none | <offset-path>] (sec.
     2.1). *)
  check_offset "normal";
  check_offset "auto";
  check_offset "left top";
  check_offset "100px";
  check_offset "none";
  check_offset "path(\"M 0 0 H 1\")";
  (* The path branch carries the distance and the rotation behind it, in the
     grammar's order whichever order the [||] pair was written in. *)
  check_offset "none 50px";
  check_offset "none reverse";
  check_offset ~expected:"none 50px reverse 30deg" "none reverse 30deg 50px";
  (* The anchor sits behind the slash (sec. 2.4). *)
  check_offset "none/50%";
  check_offset "100px/50%";
  (* Every slot at once. *)
  check_offset
    ~expected:"left bottom ray(0rad closest-corner)10px auto 30deg/right bottom"
    "left bottom ray(0rad closest-corner) 10px auto 30deg / right bottom";
  (* A CSS-wide keyword and a whole-value [var()] stand for the shorthand. *)
  check_offset "inherit";
  check_offset "var(--motion,none)";
  neg_cursor read_offset "total nonsense here";
  neg_cursor read_offset "path(\"m 0 0 h 100\") auto reverse"

let test_offset_target () =
  (* The [!] group on its own: a position, a path, or a position then a path. *)
  check_offset_target "normal";
  check_offset_target "left top";
  check_offset_target "none";
  check_offset_target "normal none";
  check_offset_target "100px none";
  check_offset_target "none 50px reverse 30deg";
  (* The group must produce a value, and the rotation needs a path in front of
     it. *)
  neg_cursor read_offset_target "";
  neg_cursor read_offset_target "30deg";
  neg_cursor read_offset_target "reverse"

let offset_canonical_slots () =
  (* Each slot canonicalises the way its longhand does. *)
  decl_optimizes ~prop:"offset" ~held:"none/center" ~into:"none/50%"
    "none / center";
  decl_optimizes ~prop:"offset" ~held:"none/left top" ~into:"none/0 0"
    "none / left top";
  decl_optimizes ~prop:"offset" ~held:"left top none" ~into:"0 0"
    "left top none";
  (* A slot holding its longhand's initial says what leaving it out says, so it
     drops: [normal] (sec. 2.3), [0] (sec. 2.2), [auto] (secs. 2.5, 2.4). *)
  decl_optimizes ~prop:"offset" ~held:"normal none reverse" ~into:"none reverse"
    "normal none reverse";
  decl_optimizes ~prop:"offset" ~held:"none 0" ~into:"none" "none 0";
  decl_optimizes ~prop:"offset" ~held:"path(\"M 0 0 H 1\")auto"
    ~into:"path(\"M 0 0 H 1\")" "path('M 0 0 H 1') auto";
  decl_optimizes ~prop:"offset" ~held:"none/auto" ~into:"none" "none / auto";
  (* [offset: initial] resets the same five longhands a bare [none] path leaves
     at their initials. *)
  decl_optimizes ~prop:"offset" ~held:"initial" ~into:"none" "initial";
  (* The path is the group's initial too, but dropping it is behaviour
     preserving only when no distance and no rotation are left behind it: both
     are reachable only through a path, and a bare [50%] re-reads as the
     position. *)
  decl_optimizes ~prop:"offset" ~held:"50%none" ~into:"50%" "50% none";
  decl_optimizes ~prop:"offset" ~held:"none 50%" ~into:"none 50%" "none 50%";
  (* Dropping every slot would leave an empty value, which is not a declaration.
     The path [none] is the shortest spelling of the whole group, so it is what
     stays behind. *)
  decl_optimizes ~prop:"offset" ~held:"normal" ~into:"none" "normal";
  decl_optimizes ~prop:"offset" ~held:"normal none/auto" ~into:"none"
    "normal none / auto"

let offset_invalid_values () =
  offset_rejects "total nonsense here";
  (* The [!] multiplier: the leading group must produce a value. *)
  offset_rejects "/ center";
  (* The anchor is required once the slash is written. *)
  offset_rejects "none /";
  (* The distance and the rotation need a path in front of them. *)
  offset_rejects "30deg";
  offset_rejects "auto 30deg 90px";
  (* The path may not follow the distance or the rotation. *)
  offset_rejects "100px 0deg path(\"m 0 0 h 100\")";
  (* Neither half of the [||] pair may be filled twice. *)
  offset_rejects "path(\"m 0 0 h 100\") 100px 200px";
  offset_rejects "path(\"m 0 0 h 100\") auto reverse";
  offset_rejects "path(\"m 0 0 h 100\") reverse 100px 30deg";
  offset_rejects "path(\"m 0 0 h 100\") 200% auto 100px";
  (* Nothing follows the group but the anchor. *)
  offset_rejects "path(\"m 0 0 h 100\") bottom";
  (* The anchor is one [<position>]: at most four components. *)
  offset_rejects "none/10px 20px 30deg";
  (* A CSS-wide keyword stands alone (CSS Cascade 5 sec. 7.3). *)
  offset_rejects "initial none";
  offset_rejects "none/inherit"

let offset_slots_roundtrip () =
  offset_roundtrips ".x{offset:auto}";
  offset_roundtrips ".x{offset:none}";
  offset_roundtrips ".x{offset:100px}";
  offset_roundtrips ".x{offset:path(\"M 0 0 H 1\")}";
  offset_roundtrips ".x{offset:none 50px}";
  offset_roundtrips ".x{offset:none reverse}";
  offset_roundtrips ".x{offset:none 50px reverse 30deg}";
  offset_roundtrips ".x{offset:none/50%}";
  offset_roundtrips ".x{offset:100px/50%}";
  offset_roundtrips ".x{offset:0 0 path(\"M 0 0 H 1\")10px reverse 45deg/50%}";
  (* A [var()] the shorthand cannot assign to one slot keeps its authored
     text. *)
  offset_roundtrips ".x{offset:var(--a) var(--b)}"

(* The five longhands keep the behaviour secs. 2.1 to 2.5 give them once the
   shorthand that resets them is modelled. *)
let offset_longhand_guards () =
  offset_roundtrips ".x{offset-path:none}";
  offset_roundtrips ".x{offset-path:path(\"M 0 0 H 1\")}";
  offset_roundtrips ".x{offset-distance:50%}";
  offset_roundtrips ".x{offset-rotate:auto}";
  offset_roundtrips ".x{offset-rotate:reverse 30deg}";
  offset_roundtrips ".x{offset-position:normal}";
  offset_roundtrips ".x{offset-anchor:auto}";
  decl_optimizes ~prop:"offset-anchor" ~held:"center" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-position" ~held:"left top" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-distance" ~held:"0px" ~into:"0" "0px"

(* Two spellings that print alike are one declaration, so the rules carrying
   them factor into a single selector list. *)
let offset_spellings_factor () =
  let css = ".a{offset:none/auto}.b{offset:none 0}.c{offset:normal}" in
  Alcotest.(check string)
    "offset spellings coalesce" ".a,.b,.c{offset:none}"
    (String.trim
       (Css.to_string ~minify:true (Css.optimize (offset_sheet css).stylesheet)))

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
  check_content ~roundtrip:true "\"nav  main\"";
  check_content "none";
  check_content "normal";
  check_content "open-quote";
  check_content "close-quote";
  check_content ~expected:"attr(data-label)" "attr(data-label)";
  check_content ~expected:"attr(data-label string,\"x y\")"
    "attr(data-label string, \"x y\")";
  check_content ~expected:"attr(data-label string,var(--label,\"x y\"))"
    "attr(data-label string, var(--label, \"x y\"))";
  check_content ~minify:false ~expected:"attr(data-label string, \"x y\")"
    "attr(data-label string, \"x y\")";
  check_content ~minify:false
    ~expected:"attr(data-label string, var(--label, \"x y\"))"
    "attr(data-label string, var(--label, \"x y\"))";
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
  check_svg_paint ~expected:"url(#grad)red" "url(#grad) red";
  neg_cursor read_svg_paint "invalid-paint"

let test_direction () =
  check_direction "ltr";
  check_direction "rtl";
  check_direction "inherit";
  neg_cursor read_direction "invalid-direction"

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
  neg_cursor read_stroke_dasharray "red"

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
  (* Per CSS Transforms 1 sec. 4 the keyword [center] is shorthand for [50%] and
     matched-pair shorthand collapses to a single value during optimization. The
     value printer preserves the parsed node. *)
  check_transform_origin "center";
  check_transform_origin "left top";
  (* A single value sets the X origin; Y defaults to center (50%), so it must
     not be duplicated onto the Y axis: [100%] means [100% 50%], not [100%
     100%], and [0] means [0 50%], not [0 0]. Only [50%] coincides with center.
     CSS Transforms 1 section 4; lightningcss and csso keep the single value. *)
  check_transform_origin "100%";
  check_transform_origin "0";
  check_transform_origin "50% 25%";
  check_transform_origin "50% 50% 10px";
  check_transform_origin ~expected:"0% 10px" "left 10px";
  check_transform_origin "left top 10px";
  check_transform_origin "center top 10px";
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

let test_filter_function () =
  List.iter
    (fun name ->
      check_filter_function ~roundtrip:true name;
      check_filter_function ~expected:name (String.uppercase_ascii name))
    [
      "blur";
      "brightness";
      "contrast";
      "grayscale";
      "hue-rotate";
      "invert";
      "opacity";
      "saturate";
      "sepia";
    ];
  neg_cursor read_filter_function "drop-shadow";
  neg_cursor read_filter_function "unknown";
  neg_cursor read_filter_function "blur()"

let test_filter () =
  check_filter "none";
  check_filter "blur(5px)";
  check_filter ~expected:"blur(5px)contrast(1.2)" "blur(5px) contrast(1.2)";
  check_filter ~expected:"hue-rotate(30deg)opacity(.5)"
    "hue-rotate(30deg) opacity(0.5)";
  check_filter ~expected:"drop-shadow(2px 4px 6px red)"
    "drop-shadow(2px 4px 6px red)";
  (* <url> reference to an SVG filter, in both the url-token and quoted forms,
     standalone and mixed with filter functions *)
  check_filter "url(#liquid)";
  check_filter "url(foo.svg#x)";
  check_filter ~expected:"url(#liquid)" "url(\"#liquid\")";
  check_filter ~expected:"url(#blur)blur(2px)" "url(#blur) blur(2px)";
  neg_cursor read_filter "invalid-filter";
  (* CSS Filter Effects 1 sec. 6.1 gives each of these one operand ([<length>],
     [<number>]/[<percentage>], or [<color> && <length>{2,3}] for
     [drop-shadow]); [Cursor.call] did not require the reader to consume its
     whole sub-cursor, so e.g. [blur(2px foo)] read only [2px] and answered
     [blur(2px)] instead of invalidating the whole [filter] value ([filter]'s
     top-level list is space-separated, so the truncated function was simply
     followed by the next one). *)
  List.iter (neg_cursor read_filter)
    [
      "blur(5px 5px)";
      "brightness(1.2 1.2)";
      "contrast(1.2 1.2)";
      "grayscale(.5 .5)";
      "hue-rotate(30deg 30deg)";
      "invert(.5 .5)";
      "opacity(.5 .5)";
      "saturate(1.5 1.5)";
      "sepia(.5 .5)";
      "drop-shadow(2px 4px 6px red foo)";
    ];
  (* Controls: whitespace directly inside the parens still stands. *)
  check_filter "blur( 5px )" ~expected:"blur(5px)";
  check_filter "opacity( .5 )" ~expected:"opacity(.5)";
  check_filter "drop-shadow( 2px 4px 6px red )"
    ~expected:"drop-shadow(2px 4px 6px red)"

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
  (* [inset var(--x)]: the [<length>{2,4} && <color>?] body supplied wholesale
     by one var, which the concrete-offset [Shadow] record cannot hold. *)
  check_shadow "inset var(--shadow)";
  check_shadow "var(--a),inset var(--b)";
  (* Test compact printing - when blur and spread are not provided, should print
     compactly. Cross-form colour canonicalization is an optimize transform. *)
  check_shadow "0 0 #0000" ~expected:"0 0 #0000";
  check_shadow "0 0 rgba(0,0,0,0)" ~expected:"0 0 rgb(0 0 0/0)";
  decl_optimizes ~prop:"box-shadow" ~held:"0 0 rgb(0 0 0/0)" ~into:"0 0 #0000"
    "0 0 rgba(0,0,0,0)";
  check_shadow "inherit";
  neg_cursor read_shadow "invalid-shadow";
  neg_cursor read_shadow "10px";
  (* A [var(--name,)] inset prefix stands in for the optional [inset] keyword,
     so the separator before the offset is load-bearing: if the var resolves to
     [inset] the result must read [inset 0 ...], never [inset0 ...]. The space
     must survive minification. *)
  let inset_var_shadow =
    shadow ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero ~blur:Zero
      ~spread:(Px 1.) ~color:(Values.hex "#000") ()
  in
  Alcotest.(check string)
    "inset-var shadow keeps the separator after the var prefix when minified"
    "var(--tw-ring-inset,) 0 0 0 1px #000"
    (Css.Pp.to_string ~minify:true pp_shadow inset_var_shadow);
  (* A var() colour with an unspecified blur (blur = None) serialises to [12px
     12px var(--c)]: pp does not pad in a [0] the author never wrote. An
     authored zero blur (Some Zero) before a var colour is the opposite case -
     it must keep the [0] (see the optimize test below), because a var() can
     resolve to a length, so dropping it would let [12px 12px var(--c)] re-bind
     the var as the blur. Some Zero and None are not interchangeable here. *)
  let var_color_shadow =
    shadow ~h_offset:(Px 12.) ~v_offset:(Px 12.)
      ~color:(Values.Var (Values.var_ref "c"))
      ()
  in
  Alcotest.(check string)
    "unspecified blur before a var colour is not padded with a 0"
    "12px 12px var(--c)"
    (Css.Pp.to_string ~minify:true pp_shadow var_color_shadow);
  (* [normalize_shadow] drops a zero blur when no spread follows, but not before
     a var() colour: dropping it there changes the value. *)
  let some_zero_var_shadow =
    Css.box_shadow
      (shadow ~h_offset:Zero ~v_offset:(Px 3.) ~blur:Zero
         ~color:(Values.Var (Values.var_ref "c"))
         ())
  in
  let optimized =
    Css.v
      [ Css.rule ~selector:(Css.Selector.class_ "k") [ some_zero_var_shadow ] ]
    |> Css.optimize |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "an authored zero blur before a var colour survives optimization"
    ".k{box-shadow:0 3px 0 var(--c)}" optimized

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
  neg_cursor read_flex "invalid-flex"

let test_font_variant_css21 () =
  check_font_variant_css21 "normal";
  check_font_variant_css21 "small-caps";
  neg_cursor read_font_variant_css21 "oblique"

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
  neg_cursor ~allow_partial:true read_position_try "123"

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
  check_border_image_width "inherit";
  neg_cursor read_border_image_width "blue"

let test_border_image_outset () =
  check_border_image_outset "1";
  check_border_image_outset "10px";
  check_border_image_outset "1 2";
  check_border_image_outset "inherit";
  neg_cursor read_border_image_outset "blue"

let test_list_style () =
  (* All-initial values collapse to the canonical single token [disc], never the
     position initial [outside] (which would change which longhand is set). *)
  (* The printer holds every component it parsed; dropping one that equals its
     longhand initial is a node change the optimizer makes (pinned in
     test_declaration). *)
  check_list_style "disc";
  check_list_style "outside";
  check_list_style "disc outside";
  check_list_style "disc outside none";
  check_list_style "square inside";
  check_list_style "square outside";
  check_list_style "inside";
  (* CSS Lists 3 (ED) sec. 3.6 sends a lone [none] to both the type and the
     image, so the single keyword is the same node the pair spells out. *)
  check_list_style "none";
  check_list_style "url(a.png)";
  check_list_style "square outside url(a.png)";
  check_list_style "inherit";
  check_list_style "initial";
  neg_cursor read_list_style "12px"

let test_list_style_shorthand () =
  check_list_style_shorthand "disc";
  check_list_style_shorthand "disc outside";
  check_list_style_shorthand "square inside";
  check_list_style_shorthand "square";
  check_list_style_shorthand "none";
  neg_cursor read_list_style_shorthand "12px"

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

let test_font () =
  check_font "16px serif";
  check_font "italic 700 16px/1.5 serif";
  check_font "italic bold 16px/1.5 serif";
  check_font "small-caps 12px monospace";
  check_font "inherit";
  check_font "caption";
  neg_cursor read_font "16px";
  (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword cannot be mixed with values. *)
  neg_cursor read_font "initial 16px serif"

let test_font_shorthand () =
  check_font_shorthand "16px serif";
  check_font_shorthand "italic 700 16px/1.5 serif";
  neg_cursor read_font_shorthand "16px"

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

let test_text_indent_value () =
  check_text_indent_value "1em";
  check_text_indent_value ~expected:"1em hanging" "hanging 1em";
  check_text_indent_value ~expected:"2em hanging each-line"
    "each-line 2em hanging";
  check_text_indent_value "inherit";
  check_text_indent_value "var(--indent,1em)";
  (* the <length-percentage> part takes a held (non-reducible) calc(). *)
  check_text_indent_value "calc(50% + 10px)";
  neg_cursor read_text_indent_value "hanging";
  neg_cursor read_text_indent_value "1em hanging hanging";
  neg_cursor read_text_indent_value "1em each-line each-line"

let test_text_transform_case () =
  check_text_transform_case "capitalize";
  check_text_transform_case "uppercase";
  check_text_transform_case "lowercase";
  neg_cursor read_text_transform_case "full-width"

let test_east_asian_feature () =
  check_east_asian_feature "jis78";
  check_east_asian_feature "traditional";
  check_east_asian_feature "ruby";
  neg_cursor read_east_asian_feature "small-caps"

let test_symbols_type () =
  check_symbols_type "cyclic";
  check_symbols_type "numeric";
  check_symbols_type "alphabetic";
  check_symbols_type "symbolic";
  check_symbols_type "fixed";
  neg_cursor read_symbols_type "disc"

let test_list_style_symbol () =
  check_list_style_symbol "\"*\"";
  check_list_style_symbol ~expected:"url(foo.png)" "url(\"foo.png\")";
  neg_cursor read_list_style_symbol "disc"

let test_mask_border_mode () =
  check_mask_border_mode "alpha";
  check_mask_border_mode "luminance";
  neg_cursor read_mask_border_mode "match-source"

let test_clip_geometry_box () =
  check_clip_geometry_box "margin-box";
  check_clip_geometry_box "border-box";
  check_clip_geometry_box "view-box";
  neg_cursor read_clip_geometry_box "closest-side"

let test_clip_path_extent () =
  check_clip_path_extent "closest-side";
  check_clip_path_extent "farthest-side";
  check_clip_path_extent "10px";
  neg_cursor read_clip_path_extent "border-box"

let test_clip_path_fill_rule () =
  check_clip_path_fill_rule "nonzero";
  check_clip_path_fill_rule "evenodd";
  neg_cursor read_clip_path_fill_rule "winding"

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
  check_timeline_shorthand "--main";
  neg_cursor read_timeline_shorthand "main block";
  neg_cursor ~allow_partial:true read_timeline_shorthand "--main z"

let test_view_timeline_shorthand () =
  check_view_timeline_shorthand "--main";
  check_view_timeline_shorthand "--main block";
  check_view_timeline_shorthand "--main 10%";
  check_view_timeline_shorthand "--main x 10% 20%";
  check_view_timeline_shorthand ~expected:"--main x 10%" "--main 10% x";
  check_view_timeline_shorthand "--main 10%,--alt y auto";
  neg_cursor read_view_timeline_shorthand "main 10%";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand "--main x y";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand
    "--main 10% 20% 30%"

let test_view_timeline_shorthand_item () =
  check_view_timeline_shorthand_item "--main";
  check_view_timeline_shorthand_item "--main inline";
  check_view_timeline_shorthand_item "--main auto 10%";
  check_view_timeline_shorthand_item ~expected:"--main x 10%" "--main 10% x";
  neg_cursor read_view_timeline_shorthand_item "main 10%";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand_item "--main x y"

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

let test_columns_value () =
  check_columns_value "auto";
  check_columns_value "2";
  check_columns_value "inherit";
  neg_cursor read_columns_value "invalid-columns"

(* CSS Values 4 (ED) sec. 10.9: a math function that resolves to <number> is
   valid wherever an <integer> is, so every integer position reads a calc(). The
   positions here are tab-size's <number> (CSS Text 4 (ED) sec. 4.4),
   column-count's <integer> and the columns shorthand that carries it (CSS
   Multicol 1 (ED) sec. 3.2 and 3.3), repeat()'s count (CSS Grid 2 (ED) sec.
   7.2.3.1) and the span count of <grid-line> (sec. 8.3). A <track-breadth> is
   not one of them: sec. 7.2.1 admits a <length-percentage> or a <flex> there,
   never a bare <number>. *)
let math_function_at_integer_positions () =
  check_tab_size ~expected:"3" "calc(1 + 2)";
  check_tab_size "4";
  check_tab_size "2px";
  check_column_count ~expected:"3" "calc(1 + 2)";
  check_column_count "3";
  check_columns_value ~expected:"3" "calc(1 + 2)";
  check_columns_value "2";
  check_grid_template ~expected:"repeat(3,1fr)" "repeat(calc(1 + 2),1fr)";
  check_grid_template "repeat(2,calc(1px + 2px))";
  check_grid_line ~expected:"span 3" "span calc(1 + 2)";
  check_grid_line "span 3";
  neg_cursor read_grid_template "calc(1 + 2)"

let test_field_sizing () =
  check_field_sizing "content";
  check_field_sizing "fixed";
  check_field_sizing "inherit";
  neg_cursor read_field_sizing "invalid-field-sizing"

let test_font_size () =
  check_font_size "16px";
  check_font_size "50%";
  let parsed = read_font_size (Cursor.of_string "50%") in
  (match parsed with
  | Pct n -> Alcotest.(check (float 0.)) "public percentage node" 50. n
  | _ -> Alcotest.fail "font-size percentage did not use Pct");
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
  decl_optimizes_to
    ~into:
      "-webkit-mask-clip:border-box,fill-box,no-clip;mask-clip:border-box,fill-box,no-clip"
    "mask-clip:border-box,fill-box,no-clip";
  neg_cursor read_mask_box "invalid-mask-box"

let test_mask_composite () =
  check_mask_composite "add";
  check_mask_composite "subtract";
  check_mask_composite "intersect";
  check_mask_composite "exclude";
  check_mask_composite "inherit";
  decl_optimizes ~prop:"mask-composite" ~into:"add,subtract,intersect"
    "add,subtract,intersect";
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
     [abs(.5)] instead of invalidating the declaration. *)
  check_opacity "abs(.5)";
  check_opacity "sign(.5)";
  check_opacity "abs( .5 )" ~expected:"abs(.5)";
  neg_cursor read_opacity "abs(.5 .5)";
  neg_cursor read_opacity "sign(.5 red)"

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
  check_font_feature_settings ~expected:"\"liga\" off,\"calt\" 1"
    "\"liga\" off, \"calt\" 1";
  check_font_variation_settings ~expected:"\"wght\" 650,\"wdth\" 75"
    "\"wght\" 650, \"wdth\" 75";
  check_timing_function "linear(0, .25 50%, 1)";
  check_timing_function ~expected:"steps(4,jump-none)" "steps(4, jump-none)";
  check_transform "translate(10px, 20%)" ~expected:"translate(10px,20%)";
  check_transform "rotate(1 0 0 45deg)";
  check_transform "scale(1.2 0.8)" ~expected:"scale(1.2 .8)";
  check_transforms ~expected:"translate(10px,20%)rotate(45deg)scale(1.2)"
    "translate(10px,20%) rotate(45deg) scale(1.2)";
  check_container_shorthand "card / inline-size" ~expected:"card/inline-size";
  check_container_shorthand "card / normal" ~expected:"card/normal";
  check_scroll_snap_type "x mandatory";
  check_scroll_snap_type "block proximity";
  check_clip_path "path(\"M 0 0 L 10 10\")";
  check_clip_path "xywh(0 0 100% 100% round 10px)";
  check_clip_path "rect(0px 0px 10px 10px)";
  check_content "counter(page)";
  check_content "counters(section, \".\")";
  neg_cursor read_font_feature_settings "\"kern\" -1";
  neg_cursor read_font_variation_settings "\"wg\" 400";
  neg_cursor read_timing_function "linear()";
  neg_cursor read_timing_function "steps(0, jump-end)";
  neg_cursor read_transform "rotate(1 0 45deg)";
  neg_cursor read_transform "scale(1 2 3 4)";
  neg_cursor ~allow_partial:true read_container_shorthand
    "card / inline-size / size";
  neg_cursor read_scroll_snap_type "mandatory x";
  neg_cursor read_clip_path "xywh(0 0)";
  (* CSS Shapes 1 sec. 3.1 gives [xywh()]/[rect()] a fixed quad plus an optional
     [round <'border-radius'>] tail; when that tail is not present,
     [Cursor.call] did not require the reader to consume its whole sub-cursor,
     so a trailing token past the quad was silently dropped instead of
     invalidating the declaration. *)
  neg_cursor read_clip_path "xywh(0 0 1px 1px foo)";
  neg_cursor read_clip_path "rect(0px 0px 10px 10px foo)";
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
  neg_cursor ~allow_partial:true read_text_wrap "pretty balance";
  neg_cursor ~allow_partial:true read_white_space "normal nowrap pre";
  neg_cursor read_word_break "auto phrase";
  neg_cursor ~allow_partial:true read_overflow_wrap "break-word anywhere";
  neg_cursor read_hyphens "soft";
  neg_cursor read_field_sizing "auto content";
  neg_cursor read_appearance "base button";
  neg_cursor ~allow_partial:true read_user_select "none text"

let spec_mask_clip_property_edges () =
  check_mask_box "view-box";
  check_mask_box "stroke-box";
  check_mask_mode "match-source";
  check_mask_composite "exclude";
  check_webkit_mask_composite "source-over";
  check_webkit_mask_source_type "auto";
  check_clip_path "shape(from 0 0, line to 100% 0, close)";
  check_clip_path ~expected:"polygon(0 0,100%0,100%100%)"
    "polygon(0 0, 100% 0, 100% 100%)";
  neg_cursor read_mask_box "margin-box";
  neg_cursor read_mask_mode "source";
  neg_cursor ~allow_partial:true read_mask_composite "add subtract";
  neg_cursor read_webkit_mask_composite "add";
  neg_cursor read_webkit_mask_source_type "match-source";
  neg_cursor read_clip_path "polygon()"

(* ignore-test: grouped generated-surface vectors. Each row is still a real
   positive/negative spec assertion; the grouping keeps this file navigable. *)
let spec_generated_animation_font_edges () =
  check_animation_composition ~expected:"replace,add" "replace, add";
  check_animation_composition_item "accumulate";
  check_animation_name ~expected:"fade,slide" "fade, slide";
  check_animation_range ~expected:"entry 0%exit 100%" "entry 0% exit 100%";
  check_animation_range "entry";
  (* the <length-percentage> offset takes a held (non-reducible) calc(). *)
  check_animation_range_item "entry calc(50% + 10px)";
  check_animation_range_item "cover 20%";
  check_animation_range_item "entry";
  check_animation_range_name "entry-crossing";
  check_animation_timeline ~expected:"scroll(root)" "scroll(root block)";
  check_font_kerning "normal";
  check_font_language_override "\"TRK\"";
  check_font_optical_sizing "auto";
  check_font_palette "--brand";
  check_font_size_adjust ~expected:"cap-height .5" "cap-height 0.5";
  check_font_size_adjust_metric "ic-height";
  check_font_synthesis "weight style";
  check_font_synthesis_feature "small-caps";
  check_font_synthesis_position "none";
  check_font_synthesis_small_caps "auto";
  check_font_synthesis_style "oblique-only";
  check_font_synthesis_weight "auto";
  check_font_variant "small-caps";
  check_font_variant "common-ligatures small-caps tabular-nums";
  check_font_variant_alternates "swash(fancy)";
  check_font_variant_alternates "styleset(a,b) historical-forms";
  check_white_space_collapse "preserve-breaks";
  check_column_height "10em";
  check_column_wrap "wrap";
  check_webkit_text_stroke "1px red";
  check_font_variant_caps "small-caps";
  check_font_variant_east_asian "jis78 ruby";
  check_east_asian_feature "jis04";
  check_font_variant_emoji "unicode";
  check_font_variant_ligature "common-ligatures";
  check_font_variant_ligatures "common-ligatures contextual";
  check_font_variant_position "super";
  check_glyph_orientation_vertical "90deg";
  check_image_orientation "from-image";
  check_image_rendering "pixelated";
  check_image_resolution "from-image 2dppx snap";
  check_resolution "2dppx";
  neg_cursor ~allow_partial:true read_animation_composition "replace blend";
  neg_cursor read_animation_composition_item "blend";
  neg_cursor read_animation_name "1fade";
  neg_cursor read_animation_range_name "enter";
  neg_cursor read_animation_timeline "scroll(";
  neg_cursor read_font_kerning "kern";
  neg_cursor read_font_language_override "TRK";
  neg_cursor read_font_optical_sizing "manual";
  neg_cursor read_font_size_adjust "-1";
  neg_cursor read_font_size_adjust_metric "x-height";
  neg_cursor read_font_synthesis "weight weight";
  neg_cursor read_font_synthesis_feature "ligatures";
  neg_cursor read_font_synthesis_position "manual";
  neg_cursor read_font_synthesis_small_caps "manual";
  neg_cursor read_font_synthesis_style "italic";
  neg_cursor read_font_synthesis_weight "manual";
  neg_cursor read_font_variant_caps "caps";
  neg_cursor read_font_variant_east_asian "jis78 jis78";
  neg_cursor read_east_asian_feature "jis05";
  neg_cursor ~allow_partial:true read_font_variant_emoji "emoji text";
  neg_cursor read_font_variant_ligature "common";
  neg_cursor read_font_variant_ligatures "common-ligatures common-ligatures";
  neg_cursor ~allow_partial:true read_font_variant_position "sub super";
  neg_cursor read_glyph_orientation_vertical "45deg";
  neg_cursor read_image_orientation "image";
  neg_cursor read_image_rendering "crisp";
  neg_cursor read_image_resolution "snap 2dppx";
  neg_cursor read_resolution "2px"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_box_layout_edges () =
  check_alignment_baseline "text-top";
  check_baseline_shift "10%";
  check_baseline_source "first";
  check_border_image ~expected:"url(border.png)30" "url(border.png) 30";
  check_border_image_outset_item "2px";
  check_border_image_repeat_keyword "round";
  check_border_image_slice "30 fill";
  (* CSS Cascade 5 sec. 7.3 gives the longhand the CSS-wide keywords; the
     offsets the shorthand takes are the same value without them. *)
  check_border_image_slice "initial";
  check_border_image_slice "inherit";
  check_border_image_slice "unset";
  check_border_image_slice "revert";
  check_border_image_slice "revert-layer";
  check_border_image_slice_offsets "30 fill";
  check_border_image_slice_item "30%";
  check_border_image_width_item "auto";
  check_border_spacing "1px 2px";
  check_column_span "all";
  check_contain_intrinsic_longhand "auto 10px";
  check_contain_intrinsic_size "auto 10px 20px";
  check_contain_intrinsic_size_item "auto 10px";
  check_counter_item "section 2";
  check_counter_set "section 2 subsection";
  check_dominant_baseline "text-bottom";
  check_flex_factor "2";
  (* flex-grow/flex-shrink are <number> (CSS Flexbox 1), so var() and calc() are
     valid. parse + lexical pp (no optimize) holds every calc() unfolded - a
     constant calc(1 + 2) and a calc(var(--g) + 1) alike. The constant fold to 3
     is an optimize+minify transform, asserted in test_optimize. *)
  check_flex_factor "var(--g)";
  check_flex_factor "calc(1 + 2)";
  check_flex_factor "calc(var(--g) + 1)";
  check_flex_flow "row wrap";
  check_grid_line_pair ~expected:"1/span 2" "1 / span 2";
  check_grid_template_areas ~expected:"\"a a\"\"b c\"" "\"a a\" \"b c\"";
  check_hyphenate_limit_chars "3 4 5";
  check_initial_letter "2 3";
  check_initial_letter_align "border-box alphabetic";
  check_initial_letter_align_keyword "hanging";
  check_initial_letter_wrap "first";
  check_inline_sizing "stretch";
  check_interpolate_size "allow-keywords";
  check_line_break "anywhere";
  check_line_fit_edge "text alphabetic";
  check_line_fit_edge_keyword "ideographic-ink";
  check_logical_border_color "red blue";
  check_logical_border_style "solid dashed";
  check_logical_border_width "1px 2px";
  decl_optimizes ~prop:"border-inline-color" ~held:"red blue" ~into:"red #00f"
    "red blue";
  check_min_intrinsic_sizing "legacy zero-if-scroll";
  check_min_intrinsic_sizing_keyword "zero-if-extrinsic";
  check_overflow_clip_box "content-box";
  check_overflow_clip_margin "content-box 1px";
  check_shape_image_threshold ".5";
  (* CSS Shapes 1 sec. 6.2 takes an [<opacity-value>], which CSS Color 4 spells
     [<number> | <percentage>], and clamps it to [0,1] at computed-value time,
     so a value outside the range still reads. *)
  check_shape_image_threshold "-1";
  check_shape_image_threshold "2";
  check_value_cursor "shape_image_threshold" read_shape_image_threshold
    pp_shape_image_threshold ~expected:".5" "50%";
  neg_cursor read_shape_image_threshold "2px";
  check_tab_size "4";
  check_zoom "50%";
  check_zoom "normal";
  neg_cursor ~allow_partial:true read_alignment_baseline "baseline middle";
  neg_cursor ~allow_partial:true read_baseline_shift "sub super";
  neg_cursor read_baseline_source "middle";
  neg_cursor read_border_image "fill";
  neg_cursor read_border_image_outset_item "-1";
  neg_cursor read_border_image_repeat_keyword "tile";
  neg_cursor read_border_image_slice "fill";
  neg_cursor read_border_image_slice_item "-1";
  neg_cursor read_border_image_width_item "-1";
  neg_cursor read_border_spacing "-1px";
  neg_cursor read_column_span "auto";
  neg_cursor read_contain_intrinsic_longhand "auto";
  neg_cursor read_contain_intrinsic_size "auto";
  neg_cursor read_contain_intrinsic_size_item "auto";
  neg_cursor read_counter_item "1section";
  neg_cursor ~allow_partial:true read_counter_set "none section";
  neg_cursor read_dominant_baseline "baseline baseline";
  neg_cursor read_flex_factor "-1";
  neg_cursor ~allow_partial:true read_flex_flow "row row";
  neg_cursor read_grid_line_pair "span";
  neg_cursor read_grid_template_areas "\"a .\" \". a\"";
  neg_cursor read_hyphenate_limit_chars "3 4 5 6";
  neg_cursor read_initial_letter ".5";
  neg_cursor read_initial_letter_align "alphabetic alphabetic";
  neg_cursor read_initial_letter_align_keyword "cap-height";
  neg_cursor read_initial_letter_wrap "wrap";
  neg_cursor read_inline_sizing "auto";
  neg_cursor read_interpolate_size "keywords";
  neg_cursor read_line_break "break";
  neg_cursor read_line_fit_edge "text text";
  neg_cursor read_line_fit_edge_keyword "baseline";
  neg_cursor ~allow_partial:true read_logical_border_color "red blue green";
  neg_cursor ~allow_partial:true read_logical_border_style "solid dashed dotted";
  neg_cursor ~allow_partial:true read_logical_border_width "1px 2px 3px";
  neg_cursor read_min_intrinsic_sizing "legacy legacy";
  neg_cursor read_min_intrinsic_sizing_keyword "zero";
  neg_cursor read_overflow_clip_box "margin-box";
  neg_cursor ~allow_partial:true read_overflow_clip_margin "1px 2px";
  (* CSS Shapes 1 sec. 6.2 clamps the threshold at computed-value time, so a
     value outside [0,1] reads; a unit is what it has no place for. *)
  neg_cursor read_shape_image_threshold "2px";
  neg_cursor read_tab_size "-1"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_position_interaction_edges () =
  check_anchor_name ~expected:"--hero,--toast" "--hero, --toast";
  check_caret "red manual block";
  check_caret "red auto";
  check_caret_animation "manual";
  check_caret_shape "underscore";
  check_container_name "main sidebar";
  check_interactivity "inert";
  check_interest_delay "100ms 200ms";
  check_margin_trim "block-start inline-end";
  check_margin_trim_axis "inline";
  check_margin_trim_edge "block-end";
  check_mask "url(mask.png)";
  check_mask_layer "url(mask.png)";
  check_nav "#next current";
  check_nav_scope "root";
  check_object_view_box "inset(10px 20px)";
  check_object_view_box "rect(0px 0px 10px 10px)";
  check_offset_path "ray(45deg sides contain at center)";
  check_offset_rotate "auto 45deg";
  check_offset_rotate_mode "reverse";
  check_overflow_anchor "none";
  check_overlay "auto";
  check_position_anchor "--menu";
  check_position_area "center span-all";
  (* css-anchor-position-1 sec. 3.1.2: a lone keyword behaves as if the second
     were [span-all] when it names one axis, and repeats itself only when it
     names neither. So [top center] is a different area from [top], and Chrome
     151 agrees: it computes them as "center top" and "top", and lays a
     percentage-width box out at 200px wide 50px against 0px wide 600px. Only
     the axis-ambiguous keywords fold. *)
  check_position_area "top center";
  check_position_area "left center";
  check_position_area "block-start center";
  check_position_area "x-start center";
  check_position_area "span-left center";
  check_position_area "center top";
  check_position_area ~expected:"center" "center center";
  check_position_area ~expected:"span-all" "span-all span-all";
  (* css-anchor-position-1 sec. 3.1.2 has two {1,2} branches over the plain
     logical keywords, [start] [end] [span-start] [span-end] [span-all], and
     over their [self-] equivalents. The spec calls every start/end keyword that
     names neither the block nor the inline axis ambiguous, so a lone one
     repeats itself instead of standing for [X span-all]: [start start] folds to
     [start], while [start end] and [end start] name distinct areas and survive
     minification unchanged. *)
  check_position_area "start";
  check_position_area ~expected:"start" "start start";
  check_position_area "start end";
  check_position_area "end start";
  check_position_area "start span-end";
  check_position_area "span-start end";
  check_position_area ~expected:"self-start" "self-start self-start";
  check_position_area "self-start span-self-end";
  check_position_area "span-self-start self-end";
  (* Section 3.1.2 also spells out [self-x-start]/[self-x-end] and
     [self-y-start]/[self-y-end] inside the two physical groups, and gives
     [self-block-start]/[self-inline-start] a branch of their own, each with a
     [span-] form. Unlike [self-start], every one of these names its axis, so
     the ambiguity clause does not reach them: a lone one behaves as if the
     second keyword were [span-all] instead of repeating itself, which leaves a
     repeated one naming a single axis twice, a shape no branch admits. *)
  check_position_area "self-x-start";
  check_position_area "self-x-end";
  check_position_area "self-y-start";
  check_position_area "self-y-end";
  check_position_area "span-self-x-start";
  check_position_area "span-self-x-end";
  check_position_area "span-self-y-start";
  check_position_area "span-self-y-end";
  check_position_area "self-block-start";
  check_position_area "self-block-end";
  check_position_area "self-inline-start";
  check_position_area "self-inline-end";
  check_position_area "span-self-block-start";
  check_position_area "span-self-block-end";
  check_position_area "span-self-inline-start";
  check_position_area "span-self-inline-end";
  (* Two keywords on opposite axes name one row and one column, so they are a
     distinct area from either alone and stay as written. *)
  check_position_area "self-x-start self-y-end";
  check_position_area "span-self-x-end self-y-start";
  check_position_area "self-block-start self-inline-end";
  check_position_area "span-self-block-end self-inline-start";
  (* [span-all] is the ambiguous partner, so it takes the free axis and the pair
     is held exactly as [top span-all] is. *)
  check_position_area "top span-all";
  check_position_area "self-x-start span-all";
  check_position_area "span-all self-block-end";
  (* Naming one axis twice has no branch, in contrast with the ambiguous
     [self-start self-start] above, which folds. *)
  neg_cursor read_position_area "self-x-start self-x-end";
  neg_cursor read_position_area "self-x-start self-x-start";
  neg_cursor read_position_area "self-y-start span-self-y-end";
  neg_cursor read_position_area "self-block-start self-block-end";
  neg_cursor read_position_area "self-block-start self-block-start";
  neg_cursor read_position_area "self-inline-start span-self-inline-end";
  (* Sec. 3.1.2 spells <position-area> as five top-level alternatives: the
     physical [ left | ... ] || [ top | ... ], the logical [ block-start | ... ]
     || [ inline-start | ... ], its [self-] twin, and the two {1,2} groups over
     [ start | ... ] and [ self-start | ... ]. Only [center] and [span-all]
     appear in every group, so any other pair has to come from one alternative.
     Naming two axes is not enough on its own: each pair below names one row and
     one column yet crosses two alternatives, and Chrome 151 rejects every one
     of them, leaving position-area at its [none] initial value. *)
  neg_cursor read_position_area "left block-start";
  neg_cursor read_position_area "block-start left";
  neg_cursor read_position_area "inline-start y-end";
  neg_cursor read_position_area "x-start self-block-start";
  neg_cursor read_position_area "self-x-start self-block-start";
  neg_cursor read_position_area "self-inline-end top";
  neg_cursor read_position_area "block-start self-inline-end";
  neg_cursor read_position_area "span-inline-start span-self-block-end";
  (* The {1,2} groups are alternatives of their own, so a plain [start] pairs
     with nothing outside its own group, not with a physical or logical keyword
     and not with a [self-] one. *)
  neg_cursor read_position_area "start top";
  neg_cursor read_position_area "span-end left";
  neg_cursor read_position_area "start block-end";
  neg_cursor read_position_area "inline-end span-start";
  neg_cursor read_position_area "start self-inline-start";
  neg_cursor read_position_area "self-block-end span-start";
  neg_cursor read_position_area "self-start right";
  neg_cursor read_position_area "span-self-end y-start";
  neg_cursor read_position_area "self-end block-start";
  neg_cursor read_position_area "self-start self-x-end";
  neg_cursor read_position_area "self-start self-block-end";
  neg_cursor read_position_area "span-self-inline-start self-end";
  neg_cursor read_position_area "start self-start";
  neg_cursor read_position_area "span-self-start end";
  (* Within one alternative the two sides of the [||] still combine in either
     order, and [center] and [span-all] pair with anything. *)
  check_position_area "left top";
  check_position_area "x-start self-y-end";
  check_position_area "block-start inline-end";
  check_position_area "span-inline-start block-end";
  check_position_area "self-inline-start self-block-end";
  check_position_area "inline-start center";
  check_position_area "span-all self-inline-end";
  check_position_area "start span-all";
  check_position_area "center self-end";
  check_position_area "top span-right";
  check_position_area "top span-left";
  check_position_area_keyword "span-inline-start";
  check_position_try_fallback "flip-block";
  check_position_try_fallbacks ~expected:"flip-block,--fallback"
    "flip-block, --fallback";
  check_position_try_order "most-inline-size";
  check_position_visibility "anchors-visible no-overflow";
  check_position_visibility_condition "no-overflow";
  check_ray ~expected:"45deg sides contain at center"
    "ray(45deg sides contain at center)";
  check_ray_size "sides";
  check_ruby_align "space-around";
  check_ruby_merge "merge";
  check_ruby_overhang "none";
  check_ruby_position "alternate over";
  check_ruby_position_keyword "inter-character";
  check_scrollbar_color "red blue";
  decl_optimizes ~prop:"scrollbar-color" ~held:"red blue" ~into:"red #00f"
    "red blue";
  check_scrollbar_gutter "stable both-edges";
  check_scrollbar_width "thin";
  neg_cursor ~allow_partial:true read_anchor_name "none --x";
  neg_cursor read_caret "manual manual";
  neg_cursor read_caret_animation "blink";
  neg_cursor read_caret_shape "line";
  neg_cursor ~allow_partial:true read_container_name "none main";
  neg_cursor read_interactivity "enabled";
  neg_cursor ~allow_partial:true read_interest_delay "-1s";
  neg_cursor read_margin_trim "block block";
  neg_cursor read_margin_trim_axis "both";
  neg_cursor read_margin_trim_edge "block";
  neg_cursor read_mask "url(mask.png) url(other.png)";
  neg_cursor read_mask_layer "url(mask.png) url(other.png)";
  neg_cursor read_nav "next current";
  neg_cursor read_nav_scope "document";
  neg_cursor read_object_view_box "xywh(0 0 1px)";
  (* CSS Images 5 sec. 4.1 gives [object-view-box]'s [inset()]/[xywh()]/[rect()]
     a fixed quad (plus an optional [round] tail for the latter two);
     [Cursor.call] did not require any of the three readers to consume their
     whole sub-cursor, so trailing content past the quad (or, for [inset()],
     even a fifth value) was silently dropped instead of invalidating the
     declaration. *)
  neg_cursor read_object_view_box "inset(10px 20px 30px 40px 50px)";
  neg_cursor read_object_view_box "xywh(0 0 1px 1px foo)";
  neg_cursor read_object_view_box "rect(0px 0px 10px 10px foo)";
  neg_cursor read_offset_path "ray()";
  neg_cursor ~allow_partial:true read_offset_rotate "auto reverse";
  neg_cursor read_offset_rotate_mode "left";
  neg_cursor read_overflow_anchor "visible";
  neg_cursor ~allow_partial:true read_overlay "none auto";
  neg_cursor read_position_anchor "menu";
  neg_cursor read_position_area "top top top";
  neg_cursor read_position_area_keyword "middle";
  neg_cursor read_position_try_fallback "flip";
  neg_cursor ~allow_partial:true read_position_try_fallbacks "none flip-block";
  neg_cursor read_position_try_order "most-size";
  neg_cursor ~allow_partial:true read_position_visibility
    "always anchors-visible";
  neg_cursor read_position_visibility_condition "visible";
  neg_cursor read_ray "ray()";
  neg_cursor read_ray_size "closest";
  neg_cursor read_ruby_align "justify";
  neg_cursor read_ruby_merge "merged";
  neg_cursor read_ruby_overhang "all";
  neg_cursor read_ruby_position "over over";
  neg_cursor read_ruby_position_keyword "above";
  neg_cursor read_scrollbar_color "red";
  neg_cursor read_scrollbar_gutter "both-edges";
  neg_cursor read_scrollbar_width "wide"

(* ignore-test: grouped generated-surface vectors. *)
let spec_generated_text_timeline_edges () =
  check_text_box "trim-both text alphabetic";
  check_text_box "cap alphabetic";
  check_text_box "normal";
  check_text_box ~expected:"trim-both cap alphabetic" "cap alphabetic trim-both";
  check_text_box_edge "text ideographic";
  check_text_box_edge_keyword "ideographic-ink";
  check_text_box_trim "trim-both";
  check_text_combine_upright "digits 3";
  check_text_decoration_skip "auto";
  check_text_decoration_skip_box "all";
  check_text_decoration_skip_inset "auto";
  check_text_decoration_skip_self "objects";
  check_text_decoration_skip_space "start";
  check_text_decoration_skip_spaces "start end";
  check_text_emphasis "filled dot red";
  check_text_emphasis_fill "open";
  check_text_emphasis_line "under";
  check_text_emphasis_position "under left";
  check_text_emphasis_shape "sesame";
  check_text_emphasis_side "right";
  check_text_emphasis_skip "spaces punctuation";
  check_text_emphasis_skip_keyword "symbols";
  check_text_emphasis_style "\"*\"";
  check_text_orientation "sideways";
  check_text_spacing_trim "space-first";
  check_text_underline_position "under right";
  check_text_underline_position_keyword "under";
  check_text_wrap_mode "nowrap";
  check_text_wrap_style "stable";
  check_timeline_inset "auto 100%";
  check_timeline_inset_item "100%";
  (* timeline-inset takes [ auto | <length-percentage> ], so a held calc()
     too. *)
  check_timeline_inset_item "calc(50% + 10px)";
  check_timeline_name ~expected:"--main,--alt" "--main, --alt";
  check_timeline_shorthand_item "--main block";
  check_timeline_shorthand_item "--main";
  check_view_transition_class "card active";
  check_view_transition_name "match-element";
  neg_cursor ~allow_partial:true read_text_box "trim-start trim-end";
  neg_cursor read_text_box "cap";
  neg_cursor read_text_box_edge "text text";
  neg_cursor read_text_box_edge_keyword "baseline";
  neg_cursor read_text_box_trim "trim";
  neg_cursor ~allow_partial:true read_text_combine_upright "digits 5";
  neg_cursor read_text_decoration_skip "all";
  neg_cursor read_text_decoration_skip_box "auto";
  neg_cursor read_text_decoration_skip_inset "all";
  neg_cursor read_text_decoration_skip_self "all";
  neg_cursor read_text_decoration_skip_space "middle";
  neg_cursor read_text_decoration_skip_spaces "all start";
  neg_cursor read_text_emphasis "filled open";
  neg_cursor read_text_emphasis_fill "solid";
  neg_cursor read_text_emphasis_line "above";
  neg_cursor read_text_emphasis_position "under over";
  neg_cursor read_text_emphasis_shape "square";
  neg_cursor read_text_emphasis_side "center";
  neg_cursor read_text_emphasis_skip "spaces spaces";
  neg_cursor read_text_emphasis_skip_keyword "letters";
  neg_cursor read_text_emphasis_style "filled open";
  neg_cursor ~allow_partial:true read_text_orientation "upright sideways";
  neg_cursor read_text_spacing_trim "trim";
  neg_cursor ~allow_partial:true read_text_underline_position "left right";
  neg_cursor read_text_underline_position_keyword "below";
  neg_cursor read_text_wrap_mode "pretty";
  neg_cursor read_text_wrap_style "nowrap";
  neg_cursor ~allow_partial:true read_timeline_inset "auto auto auto";
  (* Scroll Animations 1 sec. 3.4.3 gives each item [auto] or a plain
     [<length-percentage>], with no range restriction, so a negative reads. *)
  check_value_cursor "timeline_inset_item" read_timeline_inset_item
    pp_timeline_inset_item "-1px";
  neg_cursor read_timeline_inset_item "1deg";
  neg_cursor ~allow_partial:true read_timeline_name "none --main";
  neg_cursor ~allow_partial:true read_timeline_shorthand_item "--main z";
  neg_cursor ~allow_partial:true read_view_transition_class "none card";
  neg_cursor ~allow_partial:true read_view_transition_name "match-element card"

let enum_readers_accept_leading_ws () =
  List.iter
    (fun check -> check ())
    [
      (fun () -> check_position ~expected:"relative" " relative");
      (fun () -> check_flex_direction ~expected:"row" " row");
      (fun () -> check_align_self ~expected:"center" " center");
      (fun () -> check_text_align ~expected:"start" " start");
      (fun () -> check_text_decoration_style ~expected:"dashed" " dashed");
      (fun () -> check_overflow ~expected:"hidden" " hidden");
      (fun () -> check_visibility ~expected:"hidden" " hidden");
      (fun () -> check_scroll_snap_align ~expected:"start" " start");
      (fun () -> check_scroll_snap_stop ~expected:"always" " always");
      (fun () -> check_scroll_snap_axis ~expected:"inline" " inline");
      (fun () ->
        check_scroll_snap_strictness ~expected:"mandatory" " mandatory");
      (fun () -> check_user_select ~expected:"none" " none");
      (fun () -> check_pointer_events ~expected:"none" " none");
      (fun () -> check_resize ~expected:"both" " both");
      (fun () -> check_box_sizing ~expected:"border-box" " border-box");
      (fun () -> check_object_fit ~expected:"cover" " cover");
      (fun () -> check_content_visibility ~expected:"auto" " auto");
      (fun () -> check_direction ~expected:"rtl" " rtl");
      (fun () -> check_writing_mode ~expected:"vertical-rl" " vertical-rl");
      (fun () -> check_print_color_adjust ~expected:"exact" " exact");
      (fun () -> check_appearance ~expected:"button" " button");
      (fun () -> check_clear ~expected:"both" " both");
      (fun () -> check_float_side ~expected:"left" " left");
    ]

let tests =
  [
    test_case "display" `Quick test_display;
    test_case "position" `Quick test_position;
    test_case "overflow" `Quick test_overflow;
    test_case "zoom" `Quick test_zoom;
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
    test_case "enum readers accept leading whitespace" `Quick
      enum_readers_accept_leading_ws;
    test_case "property names" `Quick test_property_names;
    (* Additional coverage for missing readers *)
    test_case "grid auto-flow component" `Quick test_grid_auto_flow_component;
    test_case "grid auto-flow" `Quick test_grid_auto_flow;
    test_case "grid template" `Quick test_grid_template;
    test_case "grid template areas" `Quick test_grid_template_areas;
    test_case "grid line" `Quick test_grid_line;
    test_case "grid line pair" `Quick test_grid_line_pair;
    test_case "symbols type" `Quick test_symbols_type;
    test_case "list style symbol" `Quick test_list_style_symbol;
    test_case "font variant east asian feature" `Quick test_east_asian_feature;
    test_case "align-items" `Quick test_align_items;
    test_case "justify-content" `Quick test_justify_content;
    test_case "place-items" `Quick test_place_items;
    test_case "place-content" `Quick test_place_content;
    test_case "flex" `Quick test_flex;
    test_case "font-variant-css21" `Quick test_font_variant_css21;
    test_case "column-width" `Quick test_column_width;
    test_case "column-count" `Quick test_column_count;
    test_case "position-try" `Quick test_position_try;
    test_case "border-image-repeat" `Quick test_border_image_repeat;
    test_case "border-image-width" `Quick test_border_image_width;
    test_case "border-image-outset" `Quick test_border_image_outset;
    test_case "list-style" `Quick test_list_style;
    test_case "list-style-shorthand" `Quick test_list_style_shorthand;
    test_case "grid-area" `Quick test_grid_area;
    test_case "font" `Quick test_font;
    test_case "font-shorthand" `Quick test_font_shorthand;
    test_case "transform" `Quick test_transform;
    test_case "transforms" `Quick test_transforms;
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "gradient position" `Quick test_gradient_position;
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
    test_case "url escaping" `Quick test_url_escaping;
    test_case "filter" `Quick test_filter;
    test_case "filter function" `Quick test_filter_function;
    test_case "pp property value" `Quick test_pp_property_value;
    test_case "spec current property grammar edges" `Quick
      spec_property_grammar_edges;
    test_case "spec UI property edges" `Quick spec_ui_property_edges;
    test_case "spec mask and clip property edges" `Quick
      spec_mask_clip_property_edges;
    test_case "spec generated animation/font edges" `Quick
      spec_generated_animation_font_edges;
    test_case "spec generated box/layout edges" `Quick
      spec_generated_box_layout_edges;
    test_case "spec generated position/interaction edges" `Quick
      spec_generated_position_interaction_edges;
    test_case "spec generated text/timeline edges" `Quick
      spec_generated_text_timeline_edges;
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
  check_clip "rect(0px,10px,20px,30px)";
  check_clip "rect(0px 10px 20px 30px)" ~expected:"rect(0px,10px,20px,30px)";
  decl_optimizes ~prop:"clip" ~held:"rect(0px,10px,20px,30px)"
    ~into:"rect(0px,10px,20px,30px)" "rect(0px,10px,20px,30px)";
  neg_cursor read_clip "invalid-clip";
  (* CSS Masking 1 Appendix A gives the deprecated [clip] property's [rect()]
     exactly four [<top>, <right>, <bottom>, <left>]; a fifth value is invalid
     (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require the reader to
     consume its whole sub-cursor, so it stopped after the fourth length and
     answered with a truncated [rect()] instead. *)
  neg_cursor read_clip "rect(0px,10px,20px,30px,40px)"

let test_clip_path () =
  check_clip_path "none";
  check_clip_path "url(clip.svg)";
  (* inset() with 1-4 values like margin/padding shorthand *)
  check_clip_path "inset(50%)";
  (* 1 value: all sides *)
  check_clip_path "inset(10px)";
  check_clip_path ~expected:"inset(10%20%)" "inset(10% 20%)";
  (* 2 values: top/bottom, left/right *)
  check_clip_path ~expected:"inset(10%20%30%)" "inset(10% 20% 30%)";
  (* 3 values: top, left/right, bottom *)
  check_clip_path "inset(0px 10px 20px 30px)";
  (* CSS Values L4 sec. 6: a zero in <length>/<length-percentage> position drops
     its unit under canonical minification; the fold is a node-changing rewrite
     (Length{Px,0} -> Length{None,0}) and so lives in normalize, not pp. The
     held (~held) form stays pp-faithful with the unit; only the canonical
     (~into) form drops it. Same fold applies recursively inside basic shapes
     (inset, polygon, rect, etc.) - all <length-percentage> arg positions, none
     of them inside a math context. *)
  decl_optimizes ~prop:"clip-path" ~held:"inset(0px 10px 20px 30px)"
    ~into:"inset(0 10px 20px 30px)" "inset(0px 10px 20px 30px)";
  (* 4 values *)
  check_clip_path "circle(50px)";
  check_clip_path "ellipse(25px 50px)";
  check_clip_path "polygon(0px 0px,100px 0px,50px 100px)";
  decl_optimizes ~prop:"clip-path" ~held:"polygon(0px 0px,100px 0px,50px 100px)"
    ~into:"polygon(0 0,100px 0,50px 100px)"
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
  neg_cursor ~allow_partial:true read_quotes "invalid-quotes-value"

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

(* CSS Motion Path 1 (ED) sec. 2.1.1 gives [<ray-size> = <radial-extent> |
   sides], and [<radial-extent>] (css-images-3 sec. 3.2.1) is the four extent
   keywords: a length or a pair of radii is a [<radial-size>] branch that
   [ray()] has no syntax for. Every value the type admits prints, and the
   printed text parses back to it. *)
let test_ray_size () =
  let sizes : ray_size list =
    [ Closest_side; Closest_corner; Farthest_side; Farthest_corner; Sides ]
  in
  List.iter
    (fun size ->
      check_ray_size (Css.Pp.to_string ~minify:true pp_ray_size size))
    sizes;
  neg_cursor read_ray_size "10px"

(* A square or curly block is not a grouping operand in the CSS math grammar.
   Canonical whitespace therefore leaves math context when it enters one, just
   as the component-value printer does. *)
let canonical_math_block_context () =
  let components = Cursor.remaining (Cursor.of_string "calc([f() + 1])") in
  let actual =
    components |> canonicalize_math_whitespace_components
    |> Parser.string_of_components
  in
  check string "square block ends math context" "calc([f()+ 1])" actual

let additional_tests =
  [
    test_case "will_change" `Quick test_will_change;
    test_case "clip" `Quick test_clip;
    test_case "clip_path" `Quick test_clip_path;
    test_case "perspective_origin" `Quick test_perspective_origin;
    test_case "quotes" `Quick test_quotes;
    test_case "outline" `Quick test_outline;
    test_case "outline_shorthand" `Quick test_outline_shorthand;
    test_case "ray_size" `Quick test_ray_size;
    test_case "canonical math block context" `Quick canonical_math_block_context;
    test_case "background" `Quick test_background;
    test_case "font_family" `Quick test_font_family;
    test_case "text_shadow" `Quick test_text_shadow;
    test_case "font_weight" `Quick test_font_weight;
    test_case "text_transform" `Quick test_text_transform;
    test_case "text_indent_value" `Quick test_text_indent_value;
    test_case "text_transform_case" `Quick test_text_transform_case;
    test_case "mask_border_mode" `Quick test_mask_border_mode;
    test_case "clip_geometry_box" `Quick test_clip_geometry_box;
    test_case "clip_path_extent" `Quick test_clip_path_extent;
    test_case "clip_path_fill_rule" `Quick test_clip_path_fill_rule;
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
    test_case "font_feature_value" `Quick test_font_feature_value;
    test_case "font_feature_setting" `Quick test_font_feature_setting;
    test_case "font_feature_settings" `Quick test_font_feature_settings;
    test_case "font_variation_setting" `Quick test_font_variation_setting;
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
    test_case "hue_interpolation_method" `Quick test_hue_interpolation_method;
    test_case "conic_gradient_config" `Quick test_conic_gradient_config;
    test_case "radial_shape" `Quick test_radial_shape;
    test_case "radial_size" `Quick test_radial_size;
    test_case "radial_gradient_config" `Quick test_radial_gradient_config;
    test_case "background_image" `Quick test_background_image;
    test_case "background_position" `Quick test_background_position;
    test_case "position_value" `Quick test_position_value;
    test_case "offset_anchor" `Quick test_offset_anchor;
    test_case "offset_position" `Quick test_offset_position;
    test_case "offset" `Quick test_offset;
    test_case "offset_target" `Quick test_offset_target;
    test_case "offset canonical slots" `Quick offset_canonical_slots;
    test_case "offset invalid values" `Quick offset_invalid_values;
    test_case "offset slots roundtrip" `Quick offset_slots_roundtrip;
    test_case "offset longhand guards" `Quick offset_longhand_guards;
    test_case "offset spellings factor" `Quick offset_spellings_factor;
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
    test_case "grid_line_pair" `Quick test_grid_line_pair;
    test_case "grid_flex_math" `Quick test_grid_flex_math;
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
    test_case "axis" `Quick test_timeline_axis;
    test_case "timeline_shorthand" `Quick test_timeline_shorthand;
    test_case "view_timeline_shorthand" `Quick test_view_timeline_shorthand;
    test_case "view_timeline_shorthand_item" `Quick
      test_view_timeline_shorthand_item;
    test_case "caption_side" `Quick test_caption_side;
    test_case "color_scheme" `Quick test_color_scheme;
    test_case "columns_value" `Quick test_columns_value;
    test_case "math_function_at_integer_positions" `Quick
      math_function_at_integer_positions;
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
