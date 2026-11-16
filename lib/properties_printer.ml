(* The typed property-value printer. *)

open Values
open Properties_intf
open Prop_common
open Prop_svg
open Prop_multicol
open Prop_writing
open Prop_align
open Prop_flex
open Prop_grid
open Prop_image
open Prop_background
open Prop_filter
open Prop_mask
open Prop_transform
open Prop_ui
open Prop_layout
open Prop_box
open Prop_font
open Prop_text
open Prop_animation
open Prop_list
open Prop_content
open Properties_custom

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Custom_property _ -> pp pp_custom_property
  | Unknown_property _ ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified_numbers value
        else Parser.string_of_components value
      in
      Pp.string ctx rendered
  | All -> pp pp_css_wide
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp (Pp.list ~sep:Pp.token_sp pp_color)
  | Border_style -> pp (pp_box_shorthand pp_border_style)
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Border_inline_start_style -> pp pp_border_style
  | Border_inline_end_style -> pp pp_border_style
  | Border_block_start_style -> pp pp_border_style
  | Border_block_end_style -> pp pp_border_style
  | Padding -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_block_start -> pp pp_length
  | Padding_block_end -> pp pp_length
  | Margin -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_inline_end -> pp pp_length
  | Margin_inline_start -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_block_start -> pp pp_length
  | Margin_block_end -> pp pp_length
  | Gap -> pp pp_gap
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length_percentage
  | Height -> pp pp_length_percentage
  | Min_width -> pp pp_length_percentage
  | Min_height -> pp pp_length_percentage
  | Max_width -> pp pp_length_percentage
  | Max_height -> pp pp_length_percentage
  | Inline_size -> pp pp_length_percentage
  | Min_inline_size -> pp pp_length_percentage
  | Max_inline_size -> pp pp_length_percentage
  | Block_size -> pp pp_length_percentage
  | Min_block_size -> pp pp_length_percentage
  | Max_block_size -> pp pp_length_percentage
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_weight -> pp pp_font_weight
  | Display -> pp pp_display
  | Position -> pp pp_position
  | Visibility -> pp pp_visibility
  | Baseline_source -> pp pp_baseline_source
  | Alignment_baseline -> pp pp_alignment_baseline
  | Baseline_shift -> pp pp_baseline_shift
  | Align_items -> pp pp_align_items
  | Justify_content -> pp pp_justify_content
  | Justify_items -> pp pp_justify_items
  | Align_self -> pp pp_align_self
  | Border_collapse -> pp pp_border_collapse
  | Table_layout -> pp pp_table_layout
  | Grid_auto_flow -> pp pp_grid_auto_flow
  | Opacity -> pp pp_opacity
  | Fill_opacity -> pp pp_opacity
  | Stroke_opacity -> pp pp_opacity
  | Stop_opacity -> pp pp_opacity
  | Flood_opacity -> pp pp_opacity
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp pp_tab_size
  | Zoom -> pp pp_zoom
  | Webkit_line_clamp -> pp pp_webkit_line_clamp
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Inset -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_inline_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_inline_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_block_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Top -> pp (Pp.list ~sep:Pp.space pp_length)
  | Right -> pp (Pp.list ~sep:Pp.space pp_length)
  | Bottom -> pp (Pp.list ~sep:Pp.space pp_length)
  | Left -> pp (Pp.list ~sep:Pp.space pp_length)
  | Border_width -> pp (Pp.list ~sep:Pp.space pp_border_width)
  | Border_top_width -> pp pp_border_width
  | Border_right_width -> pp pp_border_width
  | Border_bottom_width -> pp pp_border_width
  | Border_left_width -> pp pp_border_width
  | Border_inline_start_width -> pp pp_border_width
  | Border_inline_end_width -> pp pp_border_width
  | Border_block_start_width -> pp pp_border_width
  | Border_block_end_width -> pp pp_border_width
  | Border_image -> pp pp_border_image
  | Border_radius -> pp pp_border_radius
  | Border_top_left_radius -> pp (pp_box_shorthand pp_length)
  | Border_top_right_radius -> pp (pp_box_shorthand pp_length)
  | Border_bottom_left_radius -> pp (pp_box_shorthand pp_length)
  | Border_bottom_right_radius -> pp (pp_box_shorthand pp_length)
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Border_block_start_color -> pp pp_color
  | Border_block_end_color -> pp pp_color
  | Border_inline_color -> pp pp_logical_border_color
  | Border_block_color -> pp pp_logical_border_color
  | Border_inline_width -> pp pp_logical_border_width
  | Border_block_width -> pp pp_logical_border_width
  | Border_inline_style -> pp pp_logical_border_style
  | Border_block_style -> pp pp_logical_border_style
  | Border_start_start_radius -> pp (pp_box_shorthand pp_length)
  | Border_start_end_radius -> pp (pp_box_shorthand pp_length)
  | Border_end_start_radius -> pp (pp_box_shorthand pp_length)
  | Border_end_end_radius -> pp (pp_box_shorthand pp_length)
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Webkit_text_fill_color -> pp pp_color
  | Webkit_text_stroke_color -> pp pp_color
  | Webkit_text_stroke -> pp pp_webkit_text_stroke
  | Webkit_text_stroke_width -> pp pp_border_width
  | Column_rule_color -> pp (Pp.list ~sep:Pp.comma pp_color)
  | Column_rule_width -> pp (Pp.list ~sep:Pp.comma pp_border_width)
  | Column_rule_style -> pp (Pp.list ~sep:Pp.comma pp_border_style)
  | Text_indent -> pp pp_text_indent_value
  | Border_spacing -> pp pp_border_spacing
  | Outline_offset -> pp pp_length
  | Perspective -> pp pp_length
  | Transform -> pp pp_transforms
  | Translate -> pp pp_translate_value
  | Isolation -> pp pp_isolation
  | Break_before -> pp pp_break_value
  | Break_after -> pp pp_break_value
  | Break_inside -> pp pp_break_inside_value
  | Page_break_before -> (
      match if Pp.minified ctx then break_of_page_break value else None with
      | Some aliased -> pp_break_value ctx aliased
      | None -> pp_page_break_value ctx value)
  | Page_break_after -> (
      match if Pp.minified ctx then break_of_page_break value else None with
      | Some aliased -> pp_break_value ctx aliased
      | None -> pp_page_break_value ctx value)
  | Page_break_inside -> (
      match
        if Pp.minified ctx then break_inside_of_page_break value else None
      with
      | Some aliased -> pp_break_inside_value ctx aliased
      | None -> pp_page_break_inside_value ctx value)
  | Page_size -> pp pp_page_size
  | Columns -> pp pp_columns_value
  | Column_width -> pp pp_column_width
  | Column_height -> pp pp_column_height
  | Column_wrap -> pp pp_column_wrap
  | Column_count -> pp pp_column_count
  | Column_rule -> pp (Pp.list ~sep:Pp.comma pp_border)
  | Column_span -> pp pp_column_span
  | Transform_style -> pp pp_transform_style
  | Backface_visibility -> pp pp_backface_visibility
  | Scroll_snap_align -> pp pp_scroll_snap_align
  | Scroll_snap_stop -> pp pp_scroll_snap_stop
  | Scroll_behavior -> pp pp_scroll_behavior
  | Box_sizing -> pp pp_box_sizing
  | Webkit_box_sizing -> pp pp_box_sizing
  | Moz_box_sizing -> pp pp_box_sizing
  | Field_sizing -> pp pp_field_sizing
  | Caption_side -> pp pp_caption_side
  | Resize -> pp pp_resize
  | Object_fit -> pp pp_object_fit
  | Object_view_box -> pp pp_object_view_box
  | Appearance -> pp pp_appearance
  | Color_scheme -> pp pp_color_scheme
  | Print_color_adjust -> pp pp_print_color_adjust
  | Webkit_print_color_adjust -> pp pp_print_color_adjust
  | Box_decoration_break -> pp pp_box_decoration_break
  | Webkit_box_decoration_break -> pp pp_box_decoration_break
  | Flex_grow -> pp pp_flex_factor
  | Flex_shrink -> pp pp_flex_factor
  | Order -> pp pp_order
  | Flex_direction -> pp pp_flex_direction
  | Flex_wrap -> pp pp_flex_wrap
  | Flex_flow -> pp pp_flex_flow
  | Font_style -> pp pp_font_style
  | Text_align -> pp pp_text_align
  | Text_decoration -> pp pp_text_decoration
  | Text_decoration_line -> pp (Pp.list ~sep:Pp.space pp_text_decoration_line)
  | Text_decoration_style -> pp pp_text_decoration_style
  | Text_decoration_skip -> pp pp_text_decoration_skip
  | Text_decoration_skip_self -> pp pp_text_decoration_skip_self
  | Text_decoration_skip_box -> pp pp_text_decoration_skip_box
  | Text_decoration_skip_inset -> pp pp_text_decoration_skip_inset
  | Text_decoration_skip_spaces -> pp pp_text_decoration_skip_spaces
  | Text_emphasis -> pp pp_text_emphasis
  | Text_emphasis_style -> pp pp_text_emphasis_style
  | Text_emphasis_color -> pp pp_color
  | Text_emphasis_position -> pp pp_text_emphasis_position
  | Text_emphasis_skip -> pp pp_text_emphasis_skip
  | Text_orientation -> pp pp_text_orientation
  | Text_transform -> pp pp_text_transform
  | List_style_type -> pp pp_list_style_type
  | List_style_position -> pp pp_list_style_position
  | List_style_image -> pp pp_list_style_image
  | Overflow -> pp pp_overflow
  | Overflow_x -> pp pp_overflow
  | Overflow_y -> pp pp_overflow
  | Overflow_block -> pp pp_overflow
  | Overflow_inline -> pp pp_overflow
  | Vertical_align -> pp pp_vertical_align
  | Text_overflow -> pp pp_text_overflow
  | Text_wrap -> pp pp_text_wrap
  | Word_break -> pp pp_word_break
  | Overflow_wrap -> pp pp_overflow_wrap
  | Line_break -> pp pp_line_break
  | Hyphens -> pp pp_hyphens
  | Webkit_hyphens -> pp pp_hyphens
  | Font_stretch -> pp pp_font_stretch
  | Font_optical_sizing -> pp pp_font_optical_sizing
  | Font_kerning -> pp pp_font_kerning
  | Font_language_override -> pp pp_font_language_override
  | Font_synthesis_style -> pp pp_font_synthesis_style
  | Font_synthesis_weight -> pp pp_font_synthesis_weight
  | Font_synthesis_small_caps -> pp pp_font_synthesis_small_caps
  | Font_synthesis_position -> pp pp_font_synthesis_position
  | Font_variant_ligatures -> pp pp_font_variant_ligatures
  | Caps -> pp pp_font_variant_caps
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_position -> pp pp_font_variant_position
  | East_asian -> pp pp_font_variant_east_asian
  | Webkit_font_smoothing -> pp pp_webkit_font_smoothing
  | Scroll_snap_type -> pp pp_scroll_snap_type
  | Container_type -> pp pp_container_type
  | Container -> pp pp_container_shorthand
  | White_space -> pp pp_white_space
  | White_space_collapse -> pp pp_white_space_collapse
  | Font_variant_alternates -> pp pp_font_variant_alternates
  | Font_variant -> pp pp_font_variant
  | Grid_template_columns -> pp pp_grid_template
  | Grid_template_rows -> pp pp_grid_template
  | Grid_template_areas -> pp pp_grid_template_areas
  | Grid_template -> pp pp_grid_template
  | Grid -> pp pp_grid_template
  | Grid_area -> pp pp_grid_area
  | Grid_auto_columns -> pp pp_grid_template
  | Grid_auto_rows -> pp pp_grid_template
  | Flex -> pp pp_flex
  | Flex_basis -> pp pp_flex_basis
  | Align_content -> pp pp_align_content
  | Justify_self -> pp pp_justify_self
  | Place_content -> pp pp_place_content
  | Place_items -> pp pp_place_items
  | Place_self ->
      pp (fun ctx (a, j) ->
          pp_align_self ctx a;
          (* Tailwind's minifier quirk: outputs single value for most cases, but
             expands stretch to two values *)
          let needs_second_value =
            match (a, j) with
            | Stretch, Stretch -> false
            | Auto, Auto -> false
            | Normal, Normal -> false
            | Baseline, Baseline -> false
            | First_baseline, First_baseline -> false
            | Last_baseline, Last_baseline -> false
            | Center, Center -> false
            | Start, Start -> false
            | End, End -> false
            | Self_start, Self_start -> false
            | Self_end, Self_end -> false
            | Flex_start, Flex_start -> false
            | Flex_end, Flex_end -> false
            | Safe_center, Safe_center -> false
            | Safe_start, Safe_start -> false
            | Safe_end, Safe_end -> false
            | Safe_flex_start, Safe_flex_start -> false
            | Safe_flex_end, Safe_flex_end -> false
            | Unsafe_center, Unsafe_center -> false
            | Unsafe_start, Unsafe_start -> false
            | Unsafe_end, Unsafe_end -> false
            | Inherit, Inherit -> false
            | Initial, Initial -> false
            | Unset, Unset -> false
            | Revert, Revert -> false
            | Revert_layer, Revert_layer -> false
            | _ -> true (* Different values always need both *)
          in
          if needs_second_value then (
            Pp.space ctx ();
            pp_justify_self ctx j))
  | Grid_column -> pp pp_grid_line_pair
  | Grid_row -> pp pp_grid_line_pair
  | Grid_column_start -> pp pp_grid_line
  | Grid_column_end -> pp pp_grid_line
  | Grid_row_start -> pp pp_grid_line
  | Grid_row_end -> pp pp_grid_line
  | Text_underline_offset -> pp pp_length
  | Background_position -> pp pp_background_position
  | Background_position_x -> pp pp_background_position_axis
  | Background_position_y -> pp pp_background_position_axis
  | Webkit_mask_position_x -> pp pp_background_position_axis
  | Webkit_mask_position_y -> pp pp_background_position_axis
  | Background_repeat -> pp pp_background_repeat
  | Background_size -> pp pp_background_size
  | Moz_osx_font_smoothing -> pp pp_moz_osx_font_smoothing
  | Backdrop_filter -> pp pp_filter
  | Webkit_backdrop_filter -> pp pp_filter
  | Webkit_mask_image -> pp pp_background_image
  | Webkit_mask_composite -> pp pp_webkit_mask_composite
  | Webkit_mask_source_type -> pp pp_webkit_mask_source_type
  | Webkit_mask_size -> pp pp_background_size
  | Webkit_mask_position -> pp pp_background_position
  | Webkit_mask_repeat -> pp pp_background_repeat
  | Webkit_mask_clip -> pp pp_webkit_mask_box
  | Webkit_mask_origin -> pp pp_webkit_mask_box
  | Border_image_source -> pp pp_background_image
  | Border_image_slice -> pp pp_border_image_slice
  | Border_image_repeat -> pp pp_border_image_repeat
  | Border_image_width -> pp pp_border_image_width
  | Border_image_outset -> pp pp_border_image_outset
  | Mask_image -> pp pp_background_image
  | Mask_composite -> pp pp_mask_composite
  | Mask_mode -> pp pp_mask_mode
  | Mask_size -> pp pp_background_size
  | Mask_position -> pp pp_background_position
  | Mask_repeat -> pp pp_background_repeat
  | Mask_clip -> pp pp_mask_box
  | Mask_origin -> pp pp_mask_box
  | Mask_type -> pp pp_mask_type
  | Mask -> pp pp_mask
  | Container_name -> pp pp_container_name
  | Anchor_name -> pp pp_anchor_name
  | Position_anchor -> pp pp_position_anchor
  | Position_try_fallbacks -> pp pp_position_try_fallbacks
  | Position_try_order -> pp pp_position_try_order
  | Position_try -> pp pp_position_try
  | Position_visibility -> pp pp_position_visibility
  | Position_area -> pp pp_position_area
  | Shape_outside -> pp Pp.string
  | Shape_margin -> pp (pp_length_percentage ~always:true)
  | Shape_image_threshold -> pp pp_shape_image_threshold
  | Overflow_clip_margin -> pp pp_overflow_clip_margin
  | Overflow_anchor -> pp pp_overflow_anchor
  | Scrollbar_width -> pp pp_scrollbar_width
  | Scrollbar_color -> pp pp_scrollbar_color
  | Scrollbar_gutter -> pp pp_scrollbar_gutter
  | Line_height_step -> pp (pp_length ~always:true)
  | Font_palette -> pp pp_font_palette
  | Font_synthesis -> pp pp_font_synthesis
  | Text_wrap_mode -> pp pp_text_wrap_mode
  | Text_wrap_style -> pp pp_text_wrap_style
  | Text_box_trim -> pp pp_text_box_trim
  | Text_underline_position -> pp pp_text_underline_position
  | Text_box_edge -> pp pp_text_box_edge
  | Text_box -> pp pp_text_box
  | Inline_sizing -> pp pp_inline_sizing
  | Line_fit_edge -> pp pp_line_fit_edge
  | Interpolate_size -> pp pp_interpolate_size
  | Min_intrinsic_sizing -> pp pp_min_intrinsic_sizing
  | Ruby_align -> pp pp_ruby_align
  | Ruby_merge -> pp pp_ruby_merge
  | Ruby_overhang -> pp pp_ruby_overhang
  | Ruby_position -> pp pp_ruby_position
  | Glyph_orientation_vertical -> pp pp_glyph_orientation_vertical
  | Animation_timeline -> pp pp_animation_timeline
  | Animation_range -> pp pp_animation_range
  | Animation_range_start -> pp pp_animation_range_item
  | Animation_range_end -> pp pp_animation_range_item
  | Scroll_timeline -> pp pp_timeline_shorthand
  | Scroll_timeline_name -> pp pp_timeline_name
  | Scroll_timeline_axis -> pp pp_timeline_axis
  | View_transition_name -> pp pp_view_transition_name
  | View_transition_class -> pp pp_view_transition_class
  | Image_orientation -> pp pp_image_orientation
  | Image_rendering -> pp pp_image_rendering
  | Image_resolution -> pp pp_image_resolution
  | Contain_intrinsic_size -> pp pp_contain_intrinsic_size
  | Contain_intrinsic_width -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_height -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_block_size -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_inline_size -> pp pp_contain_intrinsic_longhand
  | Margin_trim -> pp pp_margin_trim
  | Offset_path -> pp pp_offset_path
  | Offset -> pp pp_offset
  | Offset_anchor -> pp pp_offset_anchor
  | Offset_position -> pp pp_offset_position
  | Offset_distance -> pp (pp_length_percentage ~always:true)
  | Offset_rotate -> pp pp_offset_rotate
  | Font_size_adjust -> pp pp_font_size_adjust
  | Font_variant_emoji -> pp pp_font_variant_emoji
  | Text_spacing_trim -> pp pp_text_spacing_trim
  | Hyphenate_limit_chars -> pp pp_hyphenate_limit_chars
  | Initial_letter -> pp pp_initial_letter
  | Initial_letter_align -> pp pp_initial_letter_align
  | Initial_letter_wrap -> pp pp_initial_letter_wrap
  | Dominant_baseline -> pp pp_dominant_baseline
  | View_timeline_name -> pp pp_timeline_name
  | View_timeline_axis -> pp pp_timeline_axis
  | View_timeline_inset -> pp pp_timeline_inset
  | View_timeline -> pp pp_view_timeline_shorthand
  | Timeline_scope -> pp pp_timeline_scope
  | Perspective_origin -> pp pp_perspective_origin
  | Object_position -> pp pp_position_value
  | Rotate -> pp pp_rotate_value
  | Transition_duration -> pp pp_duration
  | Transition_timing_function -> pp pp_timing_function
  | Transition_delay -> pp pp_duration
  | Transition_property -> pp pp_transition_property
  | Transition_behavior -> pp pp_transition_behavior
  | Overlay -> pp pp_overlay
  | Will_change -> pp pp_will_change
  | Contain -> pp pp_contain
  | Word_spacing -> pp pp_length
  | Background_attachment -> pp pp_background_attachment
  | Border_top -> pp pp_border
  | Border_right -> pp pp_border
  | Border_bottom -> pp pp_border
  | Border_left -> pp pp_border
  | Transform_origin -> pp pp_transform_origin
  | Transform_box -> pp pp_transform_box
  | Text_shadow -> pp (Pp.list ~sep:Pp.comma pp_text_shadow)
  | Clip_path -> pp pp_clip_path
  | Mask_border -> pp pp_border_image
  | Content_visibility -> pp pp_content_visibility
  | Filter -> pp pp_filter
  | Background_image -> pp (Pp.list ~sep:Pp.comma pp_background_image)
  | Background_origin -> pp pp_background_box
  | Background_clip -> pp pp_background_box
  | Webkit_background_clip -> pp pp_background_box
  | Animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Aspect_ratio -> pp pp_aspect_ratio
  | Content -> pp pp_content
  | Counter_reset -> pp pp_counter_set
  | Counter_increment -> pp pp_counter_set
  | Quotes -> pp pp_quotes
  | Box_shadow -> pp pp_shadow
  | Fill -> pp pp_svg_paint
  | Stroke -> pp pp_svg_paint
  | Stroke_width -> pp pp_stroke_width
  | Transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Scale -> pp pp_scale
  | Outline -> pp pp_outline
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_border_width
  | Outline_color -> pp pp_color
  | Forced_color_adjust -> pp pp_forced_color_adjust
  | Clip -> pp pp_clip
  | Clear -> pp pp_clear
  | Float -> pp pp_float_side
  | Border -> pp pp_border
  | Border_block -> pp pp_border
  | Border_block_start -> pp pp_border
  | Border_block_end -> pp pp_border
  | Border_inline -> pp pp_border
  | Border_inline_start -> pp pp_border
  | Border_inline_end -> pp pp_border
  | Background -> pp (Pp.list ~sep:Pp.comma pp_background)
  | Text_decoration_thickness -> pp pp_length
  | Text_size_adjust -> pp pp_text_size_adjust
  | Touch_action -> pp pp_touch_action
  | Direction -> pp pp_direction
  | Fill_rule -> pp pp_fill_rule
  | Clip_rule -> pp pp_fill_rule
  | Stroke_linecap -> pp pp_stroke_linecap
  | Stroke_linejoin -> pp pp_stroke_linejoin
  | Stroke_miterlimit -> pp pp_stroke_miterlimit
  | Stroke_dashoffset -> pp pp_stroke_dashoffset
  | Stroke_dasharray -> pp pp_stroke_dasharray
  | Paint_order -> pp pp_paint_order
  | Vector_effect -> pp pp_vector_effect
  | Unicode_bidi -> pp pp_unicode_bidi
  | Writing_mode -> pp pp_writing_mode
  | Text_combine_upright -> pp pp_text_combine_upright
  | Text_decoration_skip_ink -> pp pp_text_decoration_skip_ink
  | Animation_name -> pp pp_animation_name
  | Animation_duration -> pp pp_duration
  | Animation_timing_function -> pp pp_timing_function
  | Animation_delay -> pp pp_duration
  | Animation_iteration_count -> pp pp_animation_iteration_count
  | Animation_direction -> pp pp_animation_direction
  | Animation_fill_mode -> pp pp_animation_fill_mode
  | Animation_play_state -> pp pp_animation_play_state
  | Animation_composition -> pp pp_animation_composition
  | Background_blend_mode -> pp (Pp.list ~sep:Pp.comma pp_blend_mode)
  | Scroll_margin -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_top -> pp pp_length
  | Scroll_margin_right -> pp pp_length
  | Scroll_margin_bottom -> pp pp_length
  | Scroll_margin_left -> pp pp_length
  | Scroll_margin_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_inline_start -> pp pp_length
  | Scroll_margin_inline_end -> pp pp_length
  | Scroll_margin_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_block_start -> pp pp_length
  | Scroll_margin_block_end -> pp pp_length
  | Scroll_padding -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_top -> pp pp_length
  | Scroll_padding_right -> pp pp_length
  | Scroll_padding_bottom -> pp pp_length
  | Scroll_padding_left -> pp pp_length
  | Scroll_padding_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_inline_start -> pp pp_length
  | Scroll_padding_inline_end -> pp pp_length
  | Scroll_padding_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_block_start -> pp pp_length
  | Scroll_padding_block_end -> pp pp_length
  | Overscroll_behavior -> pp (Pp.list ~sep:Pp.space pp_overscroll_behavior)
  | Overscroll_behavior_x -> pp pp_overscroll_behavior
  | Overscroll_behavior_y -> pp pp_overscroll_behavior
  | Overscroll_behavior_block -> pp pp_overscroll_behavior
  | Overscroll_behavior_inline -> pp pp_overscroll_behavior
  | Accent_color -> pp pp_color
  | Caret_color -> pp pp_color
  | Stop_color -> pp pp_color
  | Flood_color -> pp pp_color
  | Lighting_color -> pp pp_color
  | List_style -> pp pp_list_style
  | Font -> pp pp_font
  | Source -> pp pp_font_src
  | Webkit_appearance -> pp pp_webkit_appearance
  | Letter_spacing -> pp pp_length
  | Cursor -> pp pp_cursor
  | Interactivity -> pp pp_interactivity
  | Caret_animation -> pp pp_caret_animation
  | Caret_shape -> pp pp_caret_shape
  | Caret -> pp pp_caret
  | Interest_delay -> pp pp_interest_delay
  | Interest_delay_start -> pp pp_interest_delay
  | Interest_delay_end -> pp pp_interest_delay
  | Nav_up -> pp pp_nav
  | Nav_right -> pp pp_nav
  | Nav_down -> pp pp_nav
  | Nav_left -> pp pp_nav
  | Pointer_events -> pp pp_pointer_events
  | User_select -> pp pp_user_select
  | Webkit_user_select -> pp pp_user_select
  | Ms_user_select -> pp pp_user_select
  | Moz_user_select -> pp pp_user_select
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Webkit_text_decoration -> pp pp_text_decoration
  | Webkit_text_size_adjust -> pp pp_text_size_adjust
  | Webkit_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Moz_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Ms_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | O_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Webkit_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Webkit_transition_delay -> pp pp_duration
  | Webkit_transition_duration -> pp pp_duration
  | Webkit_transition_property -> pp pp_transition_property
  | Webkit_transition_timing_function -> pp pp_timing_function
  | Webkit_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Webkit_animation_delay -> pp pp_duration
  | Webkit_animation_duration -> pp pp_duration
  | Webkit_animation_direction -> pp pp_animation_direction
  | Webkit_animation_iteration_count -> pp pp_animation_iteration_count
  | Webkit_animation_name -> pp pp_animation_name
  | Webkit_animation_timing_function -> pp pp_timing_function
  | Webkit_animation_fill_mode -> pp pp_animation_fill_mode
  | Webkit_animation_play_state -> pp pp_animation_play_state
  | Webkit_flex_direction -> pp pp_flex_direction
  | Webkit_flex_wrap -> pp pp_flex_wrap
  | Webkit_flex_flow -> pp pp_flex_flow
  | Webkit_justify_content -> pp pp_justify_content
  | Webkit_align_items -> pp pp_align_items
  | Webkit_align_content -> pp pp_align_content
  | Webkit_align_self -> pp pp_align_self
  | Webkit_border_radius -> pp pp_border_radius
  | Webkit_box_shadow -> pp pp_shadow
  | Webkit_background_size -> pp pp_background_size
  | Webkit_filter -> pp pp_filter
  | Moz_appearance -> pp pp_appearance
  | Moz_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Moz_animation_delay -> pp pp_duration
  | Moz_animation_duration -> pp pp_duration
  | Moz_animation_direction -> pp pp_animation_direction
  | Moz_animation_iteration_count -> pp pp_animation_iteration_count
  | Moz_animation_name -> pp pp_animation_name
  | Moz_animation_timing_function -> pp pp_timing_function
  | Moz_animation_fill_mode -> pp pp_animation_fill_mode
  | Moz_animation_play_state -> pp pp_animation_play_state
  | Moz_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Moz_transition_delay -> pp pp_duration
  | Moz_transition_duration -> pp pp_duration
  | Moz_transition_property -> pp pp_transition_property
  | Moz_transition_timing_function -> pp pp_timing_function
  | Moz_border_radius -> pp pp_border_radius
  | Moz_box_shadow -> pp pp_shadow
  | Moz_orient -> pp pp_moz_orient
  | Ms_filter -> pp pp_filter
  | O_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Font_family -> pp pp_font_family
