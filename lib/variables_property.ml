open Properties
open Declaration
open Variables_intf
open Variables_syntax
open Variables_value
open Variables_text
open Variables_visual
open Variables_motion
open Variables_layout

(* CSS Variables L1 sec. 3: a custom property's value can itself reference other
   custom properties. A [Typed] value embeds real [var] handles, so recurse via
   the kind; a raw [Tokens] value carries refs as [var()] functions in the
   stream, recovered structurally by {!vars_of_token_stream}. *)
let vars_of_kind : type a. a kind -> a -> any_var list =
 fun kind value ->
  match kind with
  | Length -> vars_of_length value
  | Color -> vars_of_color value
  | Rgb -> vars_of_rgb value
  | Number -> vars_of_number_value value
  | Int -> []
  | Float -> []
  | Percentage -> vars_of_percentage value
  | Length_percentage -> vars_of_length_percentage value
  | Number_percentage -> []
  | Opacity -> []
  | Value -> []
  | Duration -> vars_of_duration value
  | Aspect_ratio -> vars_of_aspect_ratio value
  | Border_style -> vars_of_border_style value
  | Outline_style -> []
  | Border -> vars_of_border value
  | Font_weight -> vars_of_font_weight value
  | Font_size -> vars_of_font_size value
  | Line_height -> vars_of_line_height value
  | Font_family -> vars_of_font_family value
  | Font_feature_settings -> vars_of_font_feature_settings value
  | Font_variation_settings -> vars_of_font_variation_settings value
  | Numeric -> vars_of_font_variant_numeric value
  | Font_variant_numeric_token -> []
  | Blend_mode -> vars_of_blend_mode value
  | Scroll_snap_strictness -> vars_of_scroll_snap_strictness value
  | Angle -> vars_of_angle value
  | Rotate -> vars_of_rotate_value value
  | Scale -> vars_of_scale value
  | Shadow -> vars_of_shadow value
  | Content -> vars_of_content value
  | Gradient_stop -> vars_of_gradient_stop value
  | Gradient_direction -> vars_of_gradient_direction value
  | Gradient_position -> vars_of_gradient_position value
  | Radial_shape -> vars_of_radial_shape value
  | Radial_size -> vars_of_radial_size value
  | Position_value -> vars_of_position_value value
  | Animation -> vars_of_animation value
  | Timing_function -> []
  | Transform -> vars_of_transform value
  | Touch_action -> []
  | Transition_property_value -> vars_of_transition_property_value value
  | Background_image -> vars_of_background_image value
  | Z_index -> []
  | Filter -> vars_of_filter value
  | Font_src -> []

(* The structural scans return each name with its leading [--]; [Values.var_ref]
   re-adds it, so strip the prefix before rebuilding the handle. *)
let vars_of_ref_names names : any_var list =
  List.rev_map
    (fun name ->
      let bare = Custom_property_name.strip_prefix name in
      V (Values.var_ref bare))
    names

(* Names referenced via real [var()] functions in an opaque token stream. *)
let vars_of_token_stream (components : Component.t list) : any_var list =
  vars_of_ref_names (var_refs_in_components [] components)

(* Names referenced by a property whose value the reader keeps as raw text. The
   text never went through a typed parser, so a [var()] in it is recoverable
   only by re-tokenising, exactly as for an opaque custom-property stream. *)
let vars_of_value_string (value : string) : any_var list =
  vars_of_ref_names (var_refs_in_value_string value)

let vars_of_custom_property_value : custom_property_value -> any_var list =
  function
  | Typed { kind; value } -> vars_of_kind kind value
  | Tokens components -> vars_of_token_stream components

(* Extract variables from CSS property values using type-specific extraction
   functions *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, value -> vars_of_length_percentage value
  | Height, value -> vars_of_length_percentage value
  | Min_width, value -> vars_of_length_percentage value
  | Min_height, value -> vars_of_length_percentage value
  | Max_width, value -> vars_of_length_percentage value
  | Max_height, value -> vars_of_length_percentage value
  | Padding, values -> vars_of_length_list values
  | Padding_top, value -> vars_of_length value
  | Padding_right, value -> vars_of_length value
  | Padding_bottom, value -> vars_of_length value
  | Padding_left, value -> vars_of_length value
  | Padding_inline, values -> vars_of_length_list values
  | Padding_inline_start, value -> vars_of_length value
  | Padding_inline_end, value -> vars_of_length value
  | Padding_block, values -> vars_of_length_list values
  | Padding_block_start, value -> vars_of_length value
  | Padding_block_end, value -> vars_of_length value
  | Margin, values -> vars_of_length_list values
  | Margin_top, value -> vars_of_length value
  | Margin_right, value -> vars_of_length value
  | Margin_bottom, value -> vars_of_length value
  | Margin_left, value -> vars_of_length value
  | Margin_inline, value -> vars_of_length_list value
  | Margin_inline_start, value -> vars_of_length value
  | Margin_inline_end, value -> vars_of_length value
  | Margin_block, value -> vars_of_length_list value
  | Margin_block_start, value -> vars_of_length value
  | Margin_block_end, value -> vars_of_length value
  | Top, value -> vars_of_length_list value
  | Right, value -> vars_of_length_list value
  | Bottom, value -> vars_of_length_list value
  | Left, value -> vars_of_length_list value
  | Font_size, value -> vars_of_font_size value
  | Letter_spacing, value -> vars_of_length value
  | Line_height, value -> vars_of_line_height value
  | Border_width, value -> vars_of_border_width_list value
  | Border_top_width, value -> vars_of_border_width value
  | Border_right_width, value -> vars_of_border_width value
  | Border_bottom_width, value -> vars_of_border_width value
  | Border_left_width, value -> vars_of_border_width value
  | Border_inline_start_width, value -> vars_of_border_width value
  | Border_inline_end_width, value -> vars_of_border_width value
  | Border_block_start_width, value -> vars_of_border_width value
  | Border_block_end_width, value -> vars_of_border_width value
  | Outline_width, value -> vars_of_border_width value
  | Column_gap, value -> vars_of_length value
  | Row_gap, value -> vars_of_length value
  | Gap, Lengths { row_gap; column_gap } ->
      vars_of_optional_length row_gap @ vars_of_optional_length column_gap
  | Gap, Var v -> [ V v ]
  | Gap, (Inherit | Initial | Unset | Revert | Revert_layer) -> []
  (* Color properties *)
  | Background_color, value -> vars_of_color value
  | Color, value -> vars_of_color value
  | Border_color, value -> List.concat_map vars_of_color value
  | Border_top_color, value -> vars_of_color value
  | Border_right_color, value -> vars_of_color value
  | Border_bottom_color, value -> vars_of_color value
  | Border_left_color, value -> vars_of_color value
  | Border_inline_start_color, value -> vars_of_color value
  | Border_inline_end_color, value -> vars_of_color value
  | Border_block_start_color, value -> vars_of_color value
  | Border_block_end_color, value -> vars_of_color value
  | Border_inline_color, value -> vars_of_logical_border_color value
  | Border_block_color, value -> vars_of_logical_border_color value
  | Border_inline_width, value -> vars_of_logical_border_width value
  | Border_block_width, value -> vars_of_logical_border_width value
  | Border_inline_style, value -> vars_of_logical_border_style value
  | Border_block_style, value -> vars_of_logical_border_style value
  | Border_start_start_radius, value -> vars_of_length_list value
  | Border_start_end_radius, value -> vars_of_length_list value
  | Border_end_start_radius, value -> vars_of_length_list value
  | Border_end_end_radius, value -> vars_of_length_list value
  | Text_decoration_color, value -> vars_of_color value
  | Webkit_text_decoration_color, value -> vars_of_color value
  | Webkit_tap_highlight_color, value -> vars_of_color value
  | Outline_color, value -> vars_of_color value
  (* Border radius *)
  | Border_radius, value -> vars_of_border_radius value
  | Border_top_left_radius, value -> vars_of_length_list value
  | Border_top_right_radius, value -> vars_of_length_list value
  | Border_bottom_left_radius, value -> vars_of_length_list value
  | Border_bottom_right_radius, value -> vars_of_length_list value
  | Border_image, value -> vars_of_border_image value
  (* Outline offset *)
  | Outline_offset, value -> vars_of_length value
  | Flex_basis, value -> vars_of_flex_basis value
  (* Text and font properties *)
  | Text_indent, value -> vars_of_text_indent_value value
  | Text_decoration_thickness, value -> vars_of_length value
  | Word_spacing, value -> vars_of_length value
  (* Other length properties *)
  | Border_spacing, value -> vars_of_border_spacing value
  | Perspective, value -> vars_of_length value
  | Stroke_width, value -> vars_of_stroke_width value
  | Scroll_margin, value -> List.concat_map vars_of_length value
  | Scroll_margin_top, value -> vars_of_length value
  | Scroll_margin_right, value -> vars_of_length value
  | Scroll_margin_bottom, value -> vars_of_length value
  | Scroll_margin_left, value -> vars_of_length value
  | Scroll_padding, value -> List.concat_map vars_of_length value
  | Scroll_padding_top, value -> vars_of_length value
  | Scroll_padding_right, value -> vars_of_length value
  | Scroll_padding_bottom, value -> vars_of_length value
  | Scroll_padding_left, value -> vars_of_length value
  (* Color properties *)
  | Accent_color, value -> vars_of_color value
  | Caret_color, value -> vars_of_color value
  | Stop_color, value -> vars_of_color value
  | Flood_color, value -> vars_of_color value
  | Lighting_color, value -> vars_of_color value
  (* Rotate property *)
  | Rotate, value -> vars_of_rotate_value value
  (* Duration properties *)
  | Transition_duration, value -> vars_of_duration value
  | Transition_delay, value -> vars_of_duration value
  | Animation_duration, value -> vars_of_duration value
  | Animation_delay, value -> vars_of_duration value
  | Animation_name, value -> vars_of_animation_name value
  (* Transform properties *)
  | Transform, value -> vars_of_transform_list value
  | Webkit_transform, value -> vars_of_transform_list value
  | Moz_transform, value -> vars_of_transform_list value
  | Ms_transform, value -> vars_of_transform_list value
  | O_transform, value -> vars_of_transform_list value
  | Translate, value -> vars_of_translate_value value
  (* Border style properties *)
  | Border_style, value -> List.concat_map vars_of_border_style value
  | Border_top_style, value -> vars_of_border_style value
  | Border_right_style, value -> vars_of_border_style value
  | Border_bottom_style, value -> vars_of_border_style value
  | Border_left_style, value -> vars_of_border_style value
  | Border_inline_start_style, value -> vars_of_border_style value
  | Border_inline_end_style, value -> vars_of_border_style value
  | Border_block_start_style, value -> vars_of_border_style value
  | Border_block_end_style, value -> vars_of_border_style value
  (* Font properties *)
  | Font_weight, value -> vars_of_font_weight value
  | Font_family, value -> vars_of_font_family value
  | Font_feature_settings, value -> vars_of_font_feature_settings value
  | Font_size_adjust, value -> vars_of_font_size_adjust value
  | Font_stretch, value -> vars_of_font_stretch value
  | Font_optical_sizing, value -> vars_of_font_optical_sizing value
  | Font_kerning, value -> vars_of_font_kerning value
  | Font_language_override, value -> vars_of_font_language_override value
  | Font_synthesis_style, value -> vars_of_font_synthesis_style value
  | Font_synthesis_weight, value -> vars_of_font_synthesis_weight value
  | Font_synthesis_small_caps, value -> vars_of_synthesis_small_caps value
  | Font_synthesis_position, value -> vars_of_font_synthesis_position value
  | Font_variant_ligatures, value -> vars_of_font_variant_ligatures value
  | Caps, value -> vars_of_font_variant_caps value
  | Font_variant_emoji, value -> vars_of_font_variant_emoji value
  | Font_variation_settings, value -> vars_of_font_variation_settings value
  | Numeric, value -> vars_of_font_variant_numeric value
  | Font_variant_position, value -> vars_of_font_variant_position value
  | East_asian, value -> vars_of_east_asian value
  (* Text properties *)
  | Text_decoration, value -> vars_of_text_decoration value
  | Text_emphasis, value -> vars_of_text_emphasis value
  | Text_emphasis_position, value -> vars_of_text_emphasis_position value
  | Text_emphasis_style, value -> vars_of_text_emphasis_style value
  | Text_orientation, value -> vars_of_text_orientation value
  | Webkit_text_decoration, value -> vars_of_text_decoration value
  | Text_transform, value -> vars_of_text_transform value
  (* Content and visibility *)
  | Content, value -> vars_of_content value
  | Counter_reset, value -> vars_of_counter_set value
  | Counter_increment, value -> vars_of_counter_set value
  | Content_visibility, value -> vars_of_content_visibility value
  (* Blend mode properties *)
  | Mix_blend_mode, value -> vars_of_blend_mode value
  | Background_blend_mode, values -> List.concat_map vars_of_blend_mode values
  (* Filter properties *)
  | Filter, value -> vars_of_filter value
  | Backdrop_filter, value -> vars_of_filter value
  | Webkit_backdrop_filter, value -> vars_of_filter value
  | Webkit_filter, value -> vars_of_filter value
  | Ms_filter, value -> vars_of_filter value
  (* Transition properties *)
  | Transition, values -> List.concat_map vars_of_transition values
  | Webkit_transition, values -> List.concat_map vars_of_transition values
  | O_transition, values -> List.concat_map vars_of_transition values
  (* Animation properties *)
  | Animation, values -> List.concat_map vars_of_animation values
  (* Background properties *)
  | Background, values -> List.concat_map vars_of_background values
  (* Shadow properties *)
  | Box_shadow, value -> vars_of_shadow value
  (* Scale properties *)
  | Scale, value -> vars_of_scale value
  (* Scroll snap properties *)
  | Scroll_snap_type, value -> vars_of_scroll_snap_type value
  (* Timing function properties *)
  | Animation_timing_function, value -> vars_of_timing_function value
  | Transition_timing_function, value -> vars_of_timing_function value
  (* Quotes property *)
  | Quotes, value -> vars_of_quotes value
  (* Aspect ratio *)
  | Aspect_ratio, value -> vars_of_aspect_ratio value
  (* Background image/size *)
  | Background_image, values -> List.concat_map vars_of_background_image values
  | Background_size, value -> vars_of_background_size value
  (* Columns *)
  | Columns, value -> vars_of_columns_value value
  | Column_width, value -> vars_of_column_width value
  | Column_height, value -> vars_of_column_height value
  | Column_wrap, value -> vars_of_column_wrap value
  | Column_count, value -> vars_of_column_count value
  | Column_rule, value -> List.concat_map vars_of_border value
  | Column_rule_color, value -> List.concat_map vars_of_color value
  | Column_rule_width, value -> List.concat_map vars_of_border_width value
  | Column_rule_style, value -> List.concat_map vars_of_border_style value
  | Column_span, value -> vars_of_column_span value
  (* Contain *)
  | Contain, value -> vars_of_contain value
  (* Cursor *)
  | Cursor, value -> vars_of_cursor value
  | Interactivity, value -> vars_of_interactivity value
  | Caret_animation, value -> vars_of_caret_animation value
  | Caret_shape, value -> vars_of_caret_shape value
  | Caret, value -> vars_of_caret value
  | Interest_delay, value -> vars_of_interest_delay value
  | Interest_delay_start, value -> vars_of_interest_delay value
  | Interest_delay_end, value -> vars_of_interest_delay value
  | Nav_up, value -> vars_of_nav value
  | Nav_right, value -> vars_of_nav value
  | Nav_down, value -> vars_of_nav value
  | Nav_left, value -> vars_of_nav value
  (* Grid template *)
  | Grid_auto_columns, value -> vars_of_grid_template value
  | Grid_auto_rows, value -> vars_of_grid_template value
  | Grid, value -> vars_of_grid_template value
  | Grid_template, value -> vars_of_grid_template value
  | Grid_template_columns, value -> vars_of_grid_template value
  | Grid_template_rows, value -> vars_of_grid_template value
  (* Grid line *)
  | Grid_column_end, value -> vars_of_grid_line value
  | Grid_column_start, value -> vars_of_grid_line value
  | Grid_column, value -> vars_of_grid_line_pair value
  | Grid_row, value -> vars_of_grid_line_pair value
  | Grid_row_end, value -> vars_of_grid_line value
  | Grid_row_start, value -> vars_of_grid_line value
  (* List style *)
  | List_style_image, value -> vars_of_list_style_image value
  | List_style_type, value -> vars_of_list_style_type value
  (* Mask image/size *)
  | Border_image_source, value -> vars_of_background_image value
  | Border_image_repeat, value -> vars_of_border_image_repeat value
  | Border_image_width, value -> vars_of_border_image_width value
  | Border_image_outset, value -> vars_of_border_image_outset value
  | Mask_image, value -> vars_of_background_image value
  | Mask_size, value -> vars_of_background_size value
  | Webkit_mask_image, value -> vars_of_background_image value
  | Webkit_mask_size, value -> vars_of_background_size value
  (* Object position *)
  | Object_position, value -> vars_of_position_value value
  (* Outline style *)
  | Outline_style, value -> vars_of_outline_style value
  (* Text shadow *)
  | Text_shadow, value -> vars_of_text_shadow_list value
  (* Transform origin *)
  | Transform_origin, value -> vars_of_transform_origin value
  (* Vertical align *)
  | Vertical_align, value -> vars_of_vertical_align value
  (* Will change *)
  | Will_change, value -> vars_of_will_change value
  (* Opacity (typed math) *)
  | Opacity, value -> vars_of_opacity value
  | Fill_opacity, value -> vars_of_opacity value
  | Stroke_opacity, value -> vars_of_opacity value
  | Stop_opacity, value -> vars_of_opacity value
  | Flood_opacity, value -> vars_of_opacity value
  | Tab_size, value -> vars_of_tab_size value
  | Zoom, value -> vars_of_zoom value
  | Align_content, value -> vars_of_align_content value
  | Align_items, value -> vars_of_align_items value
  | Align_self, value -> vars_of_align_self value
  | Animation_direction, value -> vars_of_animation_direction value
  | Animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Animation_iteration_count, value -> vars_of_animation_iteration_count value
  | Animation_play_state, value -> vars_of_animation_play_state value
  | Animation_composition, value -> (
      match value with Var v -> [ V v ] | _ -> [])
  | Appearance, value -> vars_of_appearance value
  | Backface_visibility, value -> vars_of_backface_visibility value
  | Background_attachment, value -> vars_of_background_attachment value
  | Background_clip, value -> vars_of_background_box value
  | Background_origin, value -> vars_of_background_box value
  | Background_repeat, value -> vars_of_background_repeat value
  | Border, value -> vars_of_border value
  | Border_block, value -> vars_of_border value
  | Border_top, value -> vars_of_border value
  | Border_right, value -> vars_of_border value
  | Border_bottom, value -> vars_of_border value
  | Border_left, value -> vars_of_border value
  | Border_collapse, value -> vars_of_border_collapse value
  | Box_sizing, value -> vars_of_box_sizing value
  | Webkit_box_sizing, value -> vars_of_box_sizing value
  | Moz_box_sizing, value -> vars_of_box_sizing value
  | Box_decoration_break, value -> vars_of_box_decoration_break value
  | Break_after, value -> vars_of_break_value value
  | Break_before, value -> vars_of_break_value value
  | Break_inside, value -> vars_of_break_inside_value value
  | Page_break_after, value -> vars_of_page_break_value value
  | Page_break_before, value -> vars_of_page_break_value value
  | Page_break_inside, value -> vars_of_page_break_inside value
  | Caption_side, value -> vars_of_caption_side value
  | Clear, value -> vars_of_clear value
  | Clip, value -> vars_of_clip value
  | Clip_path, value -> vars_of_clip_path value
  | Color_scheme, value -> vars_of_color_scheme value
  | Container_type, value -> vars_of_container_type value
  | Container, value -> vars_of_container_shorthand value
  | Container_name, value -> vars_of_container_name value
  | Anchor_name, value -> vars_of_anchor_name value
  | Position_anchor, value -> vars_of_position_anchor value
  | Position_try_fallbacks, value -> vars_of_position_try_fallbacks value
  | Position_try_order, value -> vars_of_position_try_order value
  | Position_try, value -> vars_of_position_try value
  | Position_visibility, value -> vars_of_position_visibility value
  | Position_area, value -> vars_of_position_area value
  | Shape_image_threshold, value -> vars_of_shape_image_threshold value
  | Overflow_clip_margin, value -> vars_of_overflow_clip_margin value
  | Overflow_anchor, value -> vars_of_overflow_anchor value
  | Scrollbar_width, value -> vars_of_scrollbar_width value
  | Scrollbar_color, value -> vars_of_scrollbar_color value
  | Scrollbar_gutter, value -> vars_of_scrollbar_gutter value
  | Font_palette, value -> vars_of_font_palette value
  | Font_synthesis, value -> vars_of_font_synthesis value
  | Animation_timeline, value -> vars_of_animation_timeline value
  | Animation_range, value -> vars_of_animation_range value
  | Animation_range_start, value -> vars_of_animation_range_item value
  | Animation_range_end, value -> vars_of_animation_range_item value
  | View_transition_name, value -> vars_of_view_transition_name value
  | View_transition_class, value -> vars_of_view_transition_class value
  | Image_orientation, value -> vars_of_image_orientation value
  | Image_rendering, value -> vars_of_image_rendering value
  | Image_resolution, value -> vars_of_image_resolution value
  | Contain_intrinsic_size, value -> vars_of_contain_intrinsic_size value
  | Contain_intrinsic_width, value -> vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_height, value -> vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_block_size, value ->
      vars_of_contain_intrinsic_longhand value
  | Contain_intrinsic_inline_size, value ->
      vars_of_contain_intrinsic_longhand value
  | Margin_trim, value -> vars_of_margin_trim value
  | Offset_path, value -> vars_of_offset_path value
  | Offset, value -> vars_of_offset value
  | Offset_anchor, value -> vars_of_offset_anchor value
  | Offset_position, value -> vars_of_offset_position value
  | Offset_rotate, value -> vars_of_offset_rotate value
  | All, value -> vars_of_css_wide value
  | Direction, value -> vars_of_direction value
  | Fill_rule, value -> vars_of_fill_rule value
  | Clip_rule, value -> vars_of_fill_rule value
  | Stroke_linecap, value -> vars_of_stroke_linecap value
  | Stroke_linejoin, value -> vars_of_stroke_linejoin value
  | Stroke_miterlimit, value -> vars_of_stroke_miterlimit value
  | Stroke_dashoffset, value -> vars_of_stroke_dashoffset value
  | Stroke_dasharray, value -> vars_of_stroke_dasharray value
  | Paint_order, value -> vars_of_paint_order value
  | Vector_effect, value -> vars_of_vector_effect value
  | Display, value -> vars_of_display value
  | Fill, value -> vars_of_svg_paint value
  | Flex, value -> vars_of_flex value
  | Flex_direction, value -> vars_of_flex_direction value
  | Flex_wrap, value -> vars_of_flex_wrap value
  | Flex_flow, value -> vars_of_flex_flow value
  | Flex_grow, value -> vars_of_flex_factor value
  | Flex_shrink, value -> vars_of_flex_factor value
  | Float, value -> vars_of_float_side value
  | Font_style, value -> vars_of_font_style value
  | Forced_color_adjust, value -> vars_of_forced_color_adjust value
  | Field_sizing, value -> vars_of_field_sizing value
  | Grid_auto_flow, value -> vars_of_grid_auto_flow value
  | Grid_template_areas, value -> vars_of_grid_template_areas value
  | Justify_content, value -> vars_of_justify_content value
  | Justify_items, value -> vars_of_justify_items value
  | Justify_self, value -> vars_of_justify_self value
  | List_style_position, value -> vars_of_list_style_position value
  | Mask_clip, value -> vars_of_mask_box value
  | Mask_composite, value -> vars_of_mask_composite value
  | Mask_type, value -> vars_of_mask_type value
  | Mask, value -> vars_of_mask value
  | Mask_border, value -> vars_of_border_image value
  | Mask_mode, value -> vars_of_mask_mode value
  | Mask_origin, value -> vars_of_mask_box value
  | Mask_repeat, value -> vars_of_background_repeat value
  | Moz_appearance, value -> vars_of_appearance value
  | Moz_orient, value -> vars_of_moz_orient value
  | Moz_osx_font_smoothing, value -> vars_of_osx_font_smoothing value
  | Object_fit, value -> vars_of_object_fit value
  | Object_view_box, value -> vars_of_object_view_box value
  | Order, value -> vars_of_order value
  | Outline, value -> vars_of_outline value
  | Overflow, value -> vars_of_overflow value
  | Overflow_wrap, value -> vars_of_overflow_wrap value
  | Line_break, value -> vars_of_line_break value
  | Overflow_x, value -> vars_of_overflow value
  | Overflow_y, value -> vars_of_overflow value
  | Overflow_block, value -> vars_of_overflow value
  | Overflow_inline, value -> vars_of_overflow value
  | Overscroll_behavior, values ->
      List.concat_map vars_of_overscroll_behavior values
  | Overscroll_behavior_x, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_y, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_block, value -> vars_of_overscroll_behavior value
  | Overscroll_behavior_inline, value -> vars_of_overscroll_behavior value
  | Page_size, value -> vars_of_page_size value
  | Perspective_origin, value -> vars_of_perspective_origin value
  | Place_content, value -> vars_of_place_content value
  | Place_items, value -> vars_of_place_items value
  | Pointer_events, value -> vars_of_pointer_events value
  | Position, value -> vars_of_position value
  | Print_color_adjust, value -> vars_of_print_color_adjust value
  | Resize, value -> vars_of_resize value
  | Scroll_snap_align, value -> vars_of_scroll_snap_align value
  | Scroll_snap_stop, value -> vars_of_scroll_snap_stop value
  | Scroll_behavior, value -> vars_of_scroll_behavior value
  | Scroll_timeline, value -> vars_of_timeline_shorthand value
  | Scroll_timeline_name, value -> vars_of_timeline_name value
  | Scroll_timeline_axis, value -> vars_of_timeline_axis value
  | Stroke, value -> vars_of_svg_paint value
  | Source, _ -> []
  | Table_layout, value -> vars_of_table_layout value
  | Text_align, value -> vars_of_text_align value
  | Text_decoration_line, values ->
      List.concat_map vars_of_text_decoration_line values
  | Text_decoration_skip, value -> vars_of_text_decoration_skip value
  | Text_decoration_skip_self, value -> vars_of_decoration_skip_self value
  | Text_decoration_skip_box, value -> vars_of_decoration_skip_box value
  | Text_decoration_skip_inset, value -> vars_of_decoration_skip_inset value
  | Text_decoration_skip_spaces, value -> vars_of_decoration_skip_spaces value
  | Text_decoration_skip_ink, value -> vars_of_decoration_skip_ink value
  | Text_decoration_style, value -> vars_of_text_decoration_style value
  | Text_overflow, value -> vars_of_text_overflow value
  | Text_size_adjust, value -> vars_of_text_size_adjust value
  | Text_wrap, value -> vars_of_text_wrap value
  | Text_wrap_mode, value -> vars_of_text_wrap_mode value
  | Text_wrap_style, value -> vars_of_text_wrap_style value
  | Text_box_trim, value -> vars_of_text_box_trim value
  | Text_underline_position, value -> vars_of_text_underline_position value
  | Text_box_edge, value -> vars_of_text_box_edge value
  | Text_box, value -> vars_of_text_box value
  | Text_emphasis_skip, value -> vars_of_text_emphasis_skip value
  | Inline_sizing, value -> vars_of_inline_sizing value
  | Line_fit_edge, value -> vars_of_line_fit_edge value
  | Interpolate_size, value -> vars_of_interpolate_size value
  | Min_intrinsic_sizing, value -> vars_of_min_intrinsic_sizing value
  | Ruby_align, value -> vars_of_ruby_align value
  | Ruby_merge, value -> vars_of_ruby_merge value
  | Ruby_overhang, value -> vars_of_ruby_overhang value
  | Ruby_position, value -> vars_of_ruby_position value
  | Glyph_orientation_vertical, value ->
      vars_of_glyph_orientation_vertical value
  | Text_spacing_trim, value -> vars_of_text_spacing_trim value
  | Hyphenate_limit_chars, value -> vars_of_hyphenate_limit_chars value
  | Initial_letter, value -> vars_of_initial_letter value
  | Initial_letter_align, value -> vars_of_initial_letter_align value
  | Initial_letter_wrap, value -> vars_of_initial_letter_wrap value
  | Dominant_baseline, value -> vars_of_dominant_baseline value
  | Hyphens, value -> vars_of_hyphens value
  | Webkit_hyphens, value -> vars_of_hyphens value
  | Isolation, value -> vars_of_isolation value
  | Touch_action, value -> vars_of_touch_action value
  | Transform_box, value -> vars_of_transform_box value
  | Transform_style, value -> vars_of_transform_style value
  | Transition_behavior, value -> vars_of_transition_behavior value
  | Overlay, value -> vars_of_overlay value
  | Unicode_bidi, value -> vars_of_unicode_bidi value
  | User_select, value -> vars_of_user_select value
  | Visibility, value -> vars_of_visibility value
  | Baseline_source, value -> vars_of_baseline_source value
  | Alignment_baseline, value -> vars_of_alignment_baseline value
  | Baseline_shift, value -> vars_of_baseline_shift value
  | View_timeline_name, value -> vars_of_timeline_name value
  | View_timeline_axis, value -> vars_of_timeline_axis value
  | View_timeline_inset, value -> vars_of_timeline_inset value
  | View_timeline, value -> vars_of_view_timeline_shorthand value
  | Timeline_scope, value -> vars_of_timeline_scope value
  | Webkit_appearance, value -> vars_of_webkit_appearance value
  | Webkit_background_clip, value -> vars_of_background_box value
  | Webkit_box_decoration_break, value -> vars_of_box_decoration_break value
  | Webkit_print_color_adjust, value -> vars_of_print_color_adjust value
  | Webkit_box_orient, value -> vars_of_webkit_box_orient value
  | Webkit_font_smoothing, value -> vars_of_webkit_font_smoothing value
  | Webkit_line_clamp, value -> vars_of_webkit_line_clamp value
  | Webkit_mask_clip, value -> vars_of_webkit_mask_box value
  | Webkit_mask_composite, value -> vars_of_webkit_mask_composite value
  | Webkit_mask_origin, value -> vars_of_webkit_mask_box value
  | Webkit_mask_repeat, value -> vars_of_background_repeat value
  | Webkit_mask_source_type, value -> vars_of_mask_source_type value
  | Webkit_text_size_adjust, value -> vars_of_text_size_adjust value
  | Webkit_user_select, value -> vars_of_user_select value
  | Ms_user_select, value -> vars_of_user_select value
  | Webkit_text_fill_color, value -> vars_of_color value
  | Webkit_text_stroke_color, value -> vars_of_color value
  | Webkit_text_stroke_width, value -> vars_of_border_width value
  | Webkit_text_stroke, value ->
      Option.fold ~none:[] ~some:vars_of_border_width value.width
      @ Option.fold ~none:[] ~some:vars_of_color value.color
  | Moz_user_select, value -> vars_of_user_select value
  | White_space, value -> vars_of_white_space value
  | White_space_collapse, value -> vars_of_white_space_collapse value
  | Font_variant_alternates, value -> vars_of_font_variant_alternates value
  | Font_variant, value -> vars_of_font_variant value
  | Word_break, value -> vars_of_word_break value
  | Writing_mode, value -> vars_of_writing_mode value
  | Z_index, value -> vars_of_z_index value
  | Text_combine_upright, value -> vars_of_text_combine_upright value
  | Font, value -> vars_of_font value
  | Grid_area, value -> vars_of_grid_area value
  | List_style, value -> vars_of_list_style value
  | Webkit_animation, value -> List.concat_map vars_of_animation value
  (* Modern vendor-prefixed longhands: same value type as the unprefixed modern
     property, so reuse the same [vars_of_*] extractor. *)
  | Webkit_animation_delay, value -> vars_of_duration value
  | Webkit_animation_duration, value -> vars_of_duration value
  | Webkit_animation_direction, value -> vars_of_animation_direction value
  | Webkit_animation_iteration_count, value ->
      vars_of_animation_iteration_count value
  | Webkit_animation_name, value -> vars_of_animation_name value
  | Webkit_animation_timing_function, value -> vars_of_timing_function value
  | Webkit_animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Webkit_animation_play_state, value -> vars_of_animation_play_state value
  | Moz_animation, value -> List.concat_map vars_of_animation value
  | Moz_animation_delay, value -> vars_of_duration value
  | Moz_animation_duration, value -> vars_of_duration value
  | Moz_animation_direction, value -> vars_of_animation_direction value
  | Moz_animation_iteration_count, value ->
      vars_of_animation_iteration_count value
  | Moz_animation_name, value -> vars_of_animation_name value
  | Moz_animation_timing_function, value -> vars_of_timing_function value
  | Moz_animation_fill_mode, value -> vars_of_animation_fill_mode value
  | Moz_animation_play_state, value -> vars_of_animation_play_state value
  | Webkit_transition_delay, value -> vars_of_duration value
  | Webkit_transition_duration, value -> vars_of_duration value
  | Webkit_transition_property, value -> vars_of_transition_property_list value
  | Webkit_transition_timing_function, value -> vars_of_timing_function value
  | Moz_transition, value -> List.concat_map vars_of_transition value
  | Moz_transition_delay, value -> vars_of_duration value
  | Moz_transition_duration, value -> vars_of_duration value
  | Moz_transition_property, value -> vars_of_transition_property_list value
  | Moz_transition_timing_function, value -> vars_of_timing_function value
  | Webkit_flex_direction, value -> vars_of_flex_direction value
  | Webkit_flex_wrap, value -> vars_of_flex_wrap value
  | Webkit_flex_flow, value -> vars_of_flex_flow value
  | Webkit_justify_content, value -> vars_of_justify_content value
  | Webkit_align_items, value -> vars_of_align_items value
  | Webkit_align_content, value -> vars_of_align_content value
  | Webkit_align_self, value -> vars_of_align_self value
  | Webkit_border_radius, value -> vars_of_border_radius value
  | Webkit_box_shadow, value -> vars_of_shadow value
  | Webkit_background_size, value -> vars_of_background_size value
  | Moz_border_radius, value -> vars_of_border_radius value
  | Moz_box_shadow, value -> vars_of_shadow value
  (* Logical-border shorthand middle tier. *)
  | Border_block_start, value -> vars_of_border value
  | Border_block_end, value -> vars_of_border value
  | Border_inline, value -> vars_of_border value
  | Border_inline_start, value -> vars_of_border value
  | Border_inline_end, value -> vars_of_border value
  (* Logical sizing longhands (CSS Logical 1 sec. 4.1): the same
     <length-percentage> as the physical property each maps to. *)
  | Inline_size, value -> vars_of_length_percentage value
  | Min_inline_size, value -> vars_of_length_percentage value
  | Max_inline_size, value -> vars_of_length_percentage value
  | Block_size, value -> vars_of_length_percentage value
  | Min_block_size, value -> vars_of_length_percentage value
  | Max_block_size, value -> vars_of_length_percentage value
  (* [inset] and its logical longhands hold a length list, like [top]. *)
  | Inset, value -> vars_of_length_list value
  | Inset_inline, value -> vars_of_length_list value
  | Inset_inline_start, value -> vars_of_length_list value
  | Inset_inline_end, value -> vars_of_length_list value
  | Inset_block, value -> vars_of_length_list value
  | Inset_block_start, value -> vars_of_length_list value
  | Inset_block_end, value -> vars_of_length_list value
  (* Logical scroll-margin / scroll-padding, matching the physical arms: the
     two-value forms hold a list, the single-side longhands one length. *)
  | Scroll_margin_inline, value -> vars_of_length_list value
  | Scroll_margin_inline_start, value -> vars_of_length value
  | Scroll_margin_inline_end, value -> vars_of_length value
  | Scroll_margin_block, value -> vars_of_length_list value
  | Scroll_margin_block_start, value -> vars_of_length value
  | Scroll_margin_block_end, value -> vars_of_length value
  | Scroll_padding_inline, value -> vars_of_length_list value
  | Scroll_padding_inline_start, value -> vars_of_length value
  | Scroll_padding_inline_end, value -> vars_of_length value
  | Scroll_padding_block, value -> vars_of_length_list value
  | Scroll_padding_block_start, value -> vars_of_length value
  | Scroll_padding_block_end, value -> vars_of_length value
  (* One position per background / mask layer, as for [object-position]. *)
  | Background_position, value -> List.concat_map vars_of_position_value value
  | Background_position_x, value -> vars_of_background_position_axis value
  | Background_position_y, value -> vars_of_background_position_axis value
  | Webkit_mask_position_x, value -> vars_of_background_position_axis value
  | Webkit_mask_position_y, value -> vars_of_background_position_axis value
  | Mask_position, value -> List.concat_map vars_of_position_value value
  | Webkit_mask_position, value -> List.concat_map vars_of_position_value value
  (* Remaining typed longhands. *)
  | Text_emphasis_color, value -> vars_of_color value
  | Text_underline_offset, value -> vars_of_length value
  | Shape_margin, value -> vars_of_length_percentage value
  | Line_height_step, value -> vars_of_length value
  | Offset_distance, value -> vars_of_length_percentage value
  | Transition_property, value -> vars_of_transition_property_list value
  (* CSS Box Alignment 3 sec. 6.3: a one-value [place-self] sets both, so the
     reader stores the [var()] in each half; the dedup collapses them. *)
  | Place_self, (align, justify) ->
      vars_of_align_self align @ vars_of_justify_self justify
  (* CSS Backgrounds 3 sec. 5.2: [border-image-slice] is numbers, percentages
     and the [fill] keyword, none of which carries a var handle. *)
  | Border_image_slice, _ -> []
  (* [shape-outside] and an unrecognised property are held as raw text and an
     opaque token stream respectively; scan both structurally. *)
  | Shape_outside, value -> vars_of_value_string value
  | Unknown_property _, value -> vars_of_token_stream value
  | Custom_property _, Custom_value { value; _ } ->
      vars_of_custom_property_value value
