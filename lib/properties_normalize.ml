(* Typed property-value normalisation, including the minify swap of [initial]
   for a shorter initial value. *)

open Common
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

(* CSS Values 4 sec. 4.1.1: the [initial] keyword resolves to the property's
   spec-defined initial value at computed time. Under [--minify] swap the
   keyword for that value when its serialization is shorter (or the same length
   but a more canonical spelling that cleancss / csso emit). *)
(* Detect the [<css-wide-keyword>] keyword sequences that the box-shorthand
   expander leaves behind: [margin: initial] is read as the singleton
   [[Initial]], then [try_merge_box_shorthand] fans it out to
   [[Initial; Initial; Initial; Initial]] before we reach the printer. *)
let box_is_all_initial : length list -> bool = function
  | [ Initial ] | [ Initial; Initial; Initial; Initial ] -> true
  | _ -> false

let canonical_initial_for_minify : type a. a property -> a -> a =
 fun prop value ->
  match (prop, value) with
  (* CSS2 sec. 9.9.1 gives [z-index] the initial value [auto]. *)
  | Z_index, Initial -> Auto
  (* Motion Path 1 secs. 2.3-2.4: the initial values are [normal] and [auto]. *)
  | Offset_anchor, Initial -> Auto
  | Offset_position, Initial -> Normal
  (* Sec. 2.6 resets all five longhands, so [initial] and a bare [none] path
     leave the same computed values behind. *)
  | Offset, Initial ->
      Shorthand
        {
          target =
            With_path
              {
                position = None;
                path = (None : offset_path);
                distance = None;
                rotate = None;
              };
          anchor = None;
        }
  (* CSS Color 4 sec. 3.3 gives [opacity] the initial value [1]. *)
  | Opacity, Initial -> Opacity_number 1.
  (* CSS Box 4 secs. 3.1 and 4.1 give the margin and padding longhands the
     initial value [0]. *)
  | Margin, vs when box_is_all_initial vs -> [ Px 0. ]
  | Padding, vs when box_is_all_initial vs -> [ Px 0. ]
  | Margin_top, Initial -> Px 0.
  | Margin_right, Initial -> Px 0.
  | Margin_bottom, Initial -> Px 0.
  | Margin_left, Initial -> Px 0.
  | Padding_top, Initial -> Px 0.
  | Padding_right, Initial -> Px 0.
  | Padding_bottom, Initial -> Px 0.
  | Padding_left, Initial -> Px 0.
  (* CSS Sizing 3 secs. 3.1.1-3.1.2 gives [width] / [height] and their physical
     minimum-size properties the initial value [auto]. This is level-sensitive:
     CSS2 sec. 10.4 instead gives [min-width] / [min-height] the initial value
     [0]. *)
  | Width, Length Initial -> Length Auto
  | Height, Length Initial -> Length Auto
  | Min_width, Length Initial -> Length Auto
  | Min_height, Length Initial -> Length Auto
  (* The logical twins fold to [auto] too. Two stale tables say [0] and neither
     governs: CSS2 sec. 10.4 predates CSS Sizing 3, and CSS Logical 1 sec. 4.1's
     own [Initial: 0] line copies that superseded CSS2 value while its [Value:
     <'min-width'>] line and sec. 4 ("paired properties share a computed value")
     both bind these to min-width. [auto] is the automatic minimum size, not
     zero, so folding to [0] would let a flex item shrink below its content. *)
  | Min_inline_size, Length Initial -> Length Auto
  | Min_block_size, Length Initial -> Length Auto
  (* Keep this fallback exhaustive: a new property must make this match fail to
     compile until its initial-value fold has been considered. *)
  | Custom_property _, value -> value
  | Unknown_property _, value -> value
  | All, value -> value
  | ( ( Background_color | Color | Text_decoration_color | Text_emphasis_color
      | Border_top_color | Border_right_color | Border_bottom_color
      | Border_left_color | Border_inline_start_color | Border_inline_end_color
      | Border_block_start_color | Border_block_end_color | Outline_color
      | Webkit_tap_highlight_color | Webkit_text_decoration_color
      | Webkit_text_fill_color | Webkit_text_stroke_color | Column_rule_color
      | Webkit_text_stroke | Stop_color | Flood_color | Lighting_color
      | Accent_color | Caret_color ),
      value ) ->
      value
  | Border_color, value -> value
  | ( ( Border_style | Border_top_style | Border_right_style
      | Border_bottom_style | Border_left_style | Border_inline_start_style
      | Border_inline_end_style | Border_block_start_style
      | Border_block_end_style | Border_inline_style | Border_block_style
      | Column_rule_style ),
      value ) ->
      value
  | ( ( Padding | Padding_inline | Padding_block | Margin | Margin_inline
      | Margin_block | Inset | Inset_inline | Inset_inline_start
      | Inset_inline_end | Inset_block | Inset_block_start | Inset_block_end
      | Top | Right | Bottom | Left | Scroll_margin | Scroll_margin_inline
      | Scroll_margin_block | Scroll_padding | Scroll_padding_inline
      | Scroll_padding_block ),
      value ) ->
      value
  | ( ( Padding_left | Padding_right | Padding_bottom | Padding_top
      | Padding_inline_start | Padding_inline_end | Padding_block_start
      | Padding_block_end | Margin_inline_end | Margin_inline_start
      | Margin_left | Margin_right | Margin_top | Margin_bottom
      | Margin_block_start | Margin_block_end | Column_gap | Row_gap
      | Text_underline_offset | Letter_spacing | Border_top_left_radius
      | Border_top_right_radius | Border_bottom_left_radius
      | Border_bottom_right_radius | Border_start_start_radius
      | Border_start_end_radius | Border_end_start_radius
      | Border_end_end_radius | Outline_offset | Line_height_step | Perspective
      | Word_spacing | Text_decoration_thickness | Scroll_margin_top
      | Scroll_margin_right | Scroll_margin_bottom | Scroll_margin_left
      | Scroll_margin_inline_start | Scroll_margin_inline_end
      | Scroll_margin_block_start | Scroll_margin_block_end | Scroll_padding_top
      | Scroll_padding_right | Scroll_padding_bottom | Scroll_padding_left
      | Scroll_padding_inline_start | Scroll_padding_inline_end
      | Scroll_padding_block_start | Scroll_padding_block_end ),
      value ) ->
      value
  | Gap, value -> value
  | ( ( Width | Height | Min_width | Min_height | Max_width | Max_height
      | Inline_size | Min_inline_size | Max_inline_size | Block_size
      | Min_block_size | Max_block_size | Shape_margin | Offset_distance ),
      value ) ->
      value
  | Font_size, value -> value
  | Line_height, value -> value
  | Font_weight, value -> value
  | Font_style, value -> value
  | Text_align, value -> value
  | (Text_decoration | Webkit_text_decoration), value -> value
  | Text_decoration_line, value -> value
  | Text_decoration_style, value -> value
  | Text_decoration_skip, value -> value
  | Text_decoration_skip_self, value -> value
  | Text_decoration_skip_box, value -> value
  | Text_decoration_skip_inset, value -> value
  | Text_decoration_skip_spaces, value -> value
  | Text_emphasis, value -> value
  | Text_emphasis_style, value -> value
  | Text_emphasis_position, value -> value
  | Text_emphasis_skip, value -> value
  | Text_orientation, value -> value
  | Text_transform, value -> value
  | List_style_type, value -> value
  | List_style_position, value -> value
  | List_style_image, value -> value
  | Display, value -> value
  | Position, value -> value
  | Visibility, value -> value
  | Baseline_source, value -> value
  | Alignment_baseline, value -> value
  | Baseline_shift, value -> value
  | (Flex_direction | Webkit_flex_direction), value -> value
  | (Flex_wrap | Webkit_flex_wrap), value -> value
  | (Flex_flow | Webkit_flex_flow), value -> value
  | Flex, value -> value
  | (Flex_grow | Flex_shrink), value -> value
  | Flex_basis, value -> value
  | Order, value -> value
  | (Align_items | Webkit_align_items), value -> value
  | (Justify_content | Webkit_justify_content), value -> value
  | Justify_items, value -> value
  | Justify_self, value -> value
  | (Align_content | Webkit_align_content), value -> value
  | (Align_self | Webkit_align_self), value -> value
  | Place_content, value -> value
  | Place_items, value -> value
  | Place_self, value -> value
  | ( ( Grid_template_columns | Grid_template_rows | Grid_template | Grid
      | Grid_auto_columns | Grid_auto_rows ),
      value ) ->
      value
  | Grid_template_areas, value -> value
  | Grid_area, value -> value
  | Grid_auto_flow, value -> value
  | (Grid_column | Grid_row), value -> value
  | (Grid_column_start | Grid_column_end | Grid_row_start | Grid_row_end), value
    ->
      value
  | Border_width, value -> value
  | ( ( Border_top_width | Border_right_width | Border_bottom_width
      | Border_left_width | Border_inline_start_width | Border_inline_end_width
      | Border_block_start_width | Border_block_end_width | Outline_width
      | Column_rule_width | Webkit_text_stroke_width ),
      value ) ->
      value
  | (Border_inline_width | Border_block_width), value -> value
  | (Border_image | Mask_border), value -> value
  | (Border_image_source | Webkit_mask_image | Mask_image), value -> value
  | Border_image_slice, value -> value
  | Border_image_repeat, value -> value
  | Border_image_width, value -> value
  | Border_image_outset, value -> value
  | (Border_radius | Webkit_border_radius | Moz_border_radius), value -> value
  | (Border_inline_color | Border_block_color), value -> value
  | ( (Opacity | Fill_opacity | Stroke_opacity | Stop_opacity | Flood_opacity),
      value ) ->
      value
  | Mix_blend_mode, value -> value
  | ( (Transform | Webkit_transform | Moz_transform | Ms_transform | O_transform),
      value ) ->
      value
  | Translate, value -> value
  | Cursor, value -> value
  | Interactivity, value -> value
  | Caret_animation, value -> value
  | Caret_shape, value -> value
  | Caret, value -> value
  | (Interest_delay | Interest_delay_start | Interest_delay_end), value -> value
  | (Nav_up | Nav_right | Nav_down | Nav_left), value -> value
  | Table_layout, value -> value
  | Border_collapse, value -> value
  | Border_spacing, value -> value
  | (User_select | Webkit_user_select | Moz_user_select | Ms_user_select), value
    ->
      value
  | Pointer_events, value -> value
  | ( (Overflow | Overflow_x | Overflow_y | Overflow_block | Overflow_inline),
      value ) ->
      value
  | Z_index, value -> value
  | Outline, value -> value
  | Outline_style, value -> value
  | Forced_color_adjust, value -> value
  | Scroll_snap_type, value -> value
  | White_space, value -> value
  | White_space_collapse, value -> value
  | Font_variant_alternates, value -> value
  | Font_variant, value -> value
  | ( ( Border | Border_block | Border_block_start | Border_block_end
      | Border_inline | Border_inline_start | Border_inline_end | Border_top
      | Border_right | Border_bottom | Border_left ),
      value ) ->
      value
  | Column_rule, value -> value
  | Background, value -> value
  | Tab_size, value -> value
  | Zoom, value -> value
  | (Webkit_text_size_adjust | Text_size_adjust), value -> value
  | Font_feature_settings, value -> value
  | Font_variation_settings, value -> value
  | Text_indent, value -> value
  | List_style, value -> value
  | Font, value -> value
  | Source, value -> value
  | Webkit_appearance, value -> value
  | (Webkit_transition | Moz_transition | O_transition | Transition), value ->
      value
  | ( ( Webkit_transition_delay | Webkit_transition_duration
      | Webkit_animation_delay | Webkit_animation_duration | Moz_animation_delay
      | Moz_animation_duration | Moz_transition_delay | Moz_transition_duration
      | Transition_duration | Transition_delay | Animation_duration
      | Animation_delay ),
      value ) ->
      value
  | ( ( Webkit_transition_property | Moz_transition_property
      | Transition_property ),
      value ) ->
      value
  | ( ( Webkit_transition_timing_function | Webkit_animation_timing_function
      | Moz_animation_timing_function | Moz_transition_timing_function
      | Transition_timing_function | Animation_timing_function ),
      value ) ->
      value
  | (Webkit_animation | Moz_animation | Animation), value -> value
  | ( ( Webkit_animation_direction | Moz_animation_direction
      | Animation_direction ),
      value ) ->
      value
  | ( ( Webkit_animation_iteration_count | Moz_animation_iteration_count
      | Animation_iteration_count ),
      value ) ->
      value
  | (Webkit_animation_name | Moz_animation_name | Animation_name), value ->
      value
  | ( ( Webkit_animation_fill_mode | Moz_animation_fill_mode
      | Animation_fill_mode ),
      value ) ->
      value
  | ( ( Webkit_animation_play_state | Moz_animation_play_state
      | Animation_play_state ),
      value ) ->
      value
  | (Webkit_box_sizing | Moz_box_sizing | Box_sizing), value -> value
  | (Webkit_box_shadow | Moz_box_shadow | Box_shadow), value -> value
  | ( (Webkit_background_size | Background_size | Webkit_mask_size | Mask_size),
      value ) ->
      value
  | ( ( Webkit_filter | Ms_filter | Filter | Backdrop_filter
      | Webkit_backdrop_filter ),
      value ) ->
      value
  | (Moz_appearance | Appearance), value -> value
  | Container_type, value -> value
  | Container_name, value -> value
  | Container, value -> value
  | Anchor_name, value -> value
  | Position_anchor, value -> value
  | Position_try_fallbacks, value -> value
  | Position_try_order, value -> value
  | Position_try, value -> value
  | Position_visibility, value -> value
  | Position_area, value -> value
  | Shape_outside, value -> value
  | Shape_image_threshold, value -> value
  | Overflow_clip_margin, value -> value
  | Overflow_anchor, value -> value
  | Scrollbar_width, value -> value
  | Scrollbar_color, value -> value
  | Scrollbar_gutter, value -> value
  | Font_palette, value -> value
  | Font_synthesis, value -> value
  | Text_wrap_mode, value -> value
  | Text_wrap_style, value -> value
  | Text_box_trim, value -> value
  | Text_underline_position, value -> value
  | Text_box_edge, value -> value
  | Text_box, value -> value
  | Inline_sizing, value -> value
  | Line_fit_edge, value -> value
  | Interpolate_size, value -> value
  | Min_intrinsic_sizing, value -> value
  | Ruby_align, value -> value
  | Ruby_merge, value -> value
  | Ruby_overhang, value -> value
  | Ruby_position, value -> value
  | Glyph_orientation_vertical, value -> value
  | Text_combine_upright, value -> value
  | Animation_timeline, value -> value
  | Animation_range, value -> value
  | (Animation_range_start | Animation_range_end), value -> value
  | Scroll_timeline, value -> value
  | View_timeline, value -> value
  | (Scroll_timeline_name | View_timeline_name | Timeline_scope), value -> value
  | (Scroll_timeline_axis | View_timeline_axis), value -> value
  | View_transition_name, value -> value
  | View_transition_class, value -> value
  | Image_orientation, value -> value
  | Image_rendering, value -> value
  | Image_resolution, value -> value
  | Contain_intrinsic_size, value -> value
  | ( ( Contain_intrinsic_width | Contain_intrinsic_height
      | Contain_intrinsic_block_size | Contain_intrinsic_inline_size ),
      value ) ->
      value
  | Margin_trim, value -> value
  | Offset_path, value -> value
  | Offset, value -> value
  | Offset_anchor, value -> value
  | Offset_position, value -> value
  | Offset_rotate, value -> value
  | Font_size_adjust, value -> value
  | Font_variant_emoji, value -> value
  | Text_spacing_trim, value -> value
  | Hyphenate_limit_chars, value -> value
  | Initial_letter, value -> value
  | Initial_letter_align, value -> value
  | Initial_letter_wrap, value -> value
  | Dominant_baseline, value -> value
  | View_timeline_inset, value -> value
  | Perspective_origin, value -> value
  | Transform_style, value -> value
  | Backface_visibility, value -> value
  | Object_position, value -> value
  | Rotate, value -> value
  | Transition_behavior, value -> value
  | Overlay, value -> value
  | Will_change, value -> value
  | Contain, value -> value
  | Isolation, value -> value
  | (Break_before | Break_after), value -> value
  | Break_inside, value -> value
  | (Page_break_before | Page_break_after), value -> value
  | Page_break_inside, value -> value
  | Page_size, value -> value
  | Columns, value -> value
  | Column_width, value -> value
  | Column_height, value -> value
  | Column_wrap, value -> value
  | Column_count, value -> value
  | Column_span, value -> value
  | Background_attachment, value -> value
  | Transform_origin, value -> value
  | Transform_box, value -> value
  | Text_shadow, value -> value
  | Clip_path, value -> value
  | Mask, value -> value
  | Content_visibility, value -> value
  | Background_image, value -> value
  | (Background_origin | Background_clip | Webkit_background_clip), value ->
      value
  | Aspect_ratio, value -> value
  | Vertical_align, value -> value
  | Font_family, value -> value
  | (Background_position | Webkit_mask_position | Mask_position), value -> value
  | (Background_position_x | Background_position_y), value -> value
  | (Webkit_mask_position_x | Webkit_mask_position_y), value -> value
  | (Background_repeat | Webkit_mask_repeat | Mask_repeat), value -> value
  | Webkit_font_smoothing, value -> value
  | Moz_osx_font_smoothing, value -> value
  | Webkit_line_clamp, value -> value
  | Webkit_box_orient, value -> value
  | Moz_orient, value -> value
  | Text_overflow, value -> value
  | Text_wrap, value -> value
  | Word_break, value -> value
  | Overflow_wrap, value -> value
  | Line_break, value -> value
  | (Hyphens | Webkit_hyphens), value -> value
  | Font_stretch, value -> value
  | Font_optical_sizing, value -> value
  | Font_kerning, value -> value
  | Font_language_override, value -> value
  | Font_synthesis_style, value -> value
  | Font_synthesis_weight, value -> value
  | Font_synthesis_small_caps, value -> value
  | Font_synthesis_position, value -> value
  | Font_variant_ligatures, value -> value
  | Caps, value -> value
  | Numeric, value -> value
  | Font_variant_position, value -> value
  | East_asian, value -> value
  | Webkit_mask_composite, value -> value
  | Webkit_mask_source_type, value -> value
  | (Webkit_mask_clip | Webkit_mask_origin | Mask_clip | Mask_origin), value ->
      value
  | Mask_composite, value -> value
  | Mask_mode, value -> value
  | Mask_type, value -> value
  | Scroll_snap_align, value -> value
  | Scroll_snap_stop, value -> value
  | Scroll_behavior, value -> value
  | Field_sizing, value -> value
  | Caption_side, value -> value
  | Resize, value -> value
  | Object_fit, value -> value
  | Object_view_box, value -> value
  | Color_scheme, value -> value
  | (Print_color_adjust | Webkit_print_color_adjust), value -> value
  | (Box_decoration_break | Webkit_box_decoration_break), value -> value
  | Content, value -> value
  | (Counter_reset | Counter_increment), value -> value
  | Quotes, value -> value
  | Touch_action, value -> value
  | Clip, value -> value
  | Clear, value -> value
  | Float, value -> value
  | Scale, value -> value
  | (Fill | Stroke), value -> value
  | Stroke_width, value -> value
  | (Fill_rule | Clip_rule), value -> value
  | Stroke_linecap, value -> value
  | Stroke_linejoin, value -> value
  | Stroke_miterlimit, value -> value
  | Stroke_dashoffset, value -> value
  | Stroke_dasharray, value -> value
  | Paint_order, value -> value
  | Vector_effect, value -> value
  | Direction, value -> value
  | Unicode_bidi, value -> value
  | Writing_mode, value -> value
  | Text_decoration_skip_ink, value -> value
  | Animation_composition, value -> value
  | Background_blend_mode, value -> value
  | Overscroll_behavior, value -> value
  | ( ( Overscroll_behavior_x | Overscroll_behavior_y
      | Overscroll_behavior_block | Overscroll_behavior_inline ),
      value ) ->
      value

let normalize_property_value : type a.
    ?lossless:bool ->
    ?exact_srgb:bool ->
    ?resolve_missing:bool ->
    ?ctx:Values.calc_ctx ->
    a property ->
    a ->
    a =
 fun ?(lossless = false) ?(exact_srgb = false) ?(resolve_missing = false)
     ?(ctx = Values.default_calc_ctx) property value ->
  let normalize_color =
    Values.normalize_color ~lossless ~exact_srgb ~resolve_missing
  in
  (* [initial] -> shortest spec-equivalent (e.g. min-width:initial -> auto) is a
     semantic rewrite, so it belongs here, not in pp. *)
  let value = canonical_initial_for_minify property value in
  match property with
  | Transform -> map_preserve (normalize_transform ~ctx) value
  | Webkit_transform -> map_preserve (normalize_transform ~ctx) value
  | Webkit_border_radius -> normalize_border_radius value
  | Moz_border_radius -> normalize_border_radius value
  | Webkit_box_shadow -> normalize_shadow ~lossless value
  | Moz_box_shadow -> normalize_shadow ~lossless value
  | Rotate -> normalize_rotate ~ctx value
  | Scale -> normalize_scale value
  | Translate -> normalize_translate_value value
  | Transform_origin -> normalize_transform_origin value
  | Offset_path -> normalize_offset_path value
  | Offset -> normalize_offset ~ctx value
  | Offset_anchor -> normalize_offset_anchor value
  | Offset_position -> normalize_offset_position value
  | Offset_rotate -> normalize_offset_rotate value
  | Font_style -> normalize_font_style value
  | Width -> Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Height -> Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_width ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_height ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_width ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_height ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Shape_margin ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Offset_distance -> Values.normalize_length_percentage ~ctx value
  | Border_radius -> normalize_border_radius value
  | Background_image ->
      map_preserve (normalize_background_image ~lossless) value
  | Mask_image -> normalize_background_image ~lossless value
  | Webkit_mask_image -> normalize_background_image ~lossless value
  | Border_image_source -> normalize_background_image ~lossless value
  | Background -> map_preserve (normalize_background ~lossless) value
  | Background_repeat -> normalize_background_repeat value
  | Mask_border -> normalize_mask_border value
  | Border_spacing -> normalize_border_spacing value
  | Mask -> normalize_mask ~lossless value
  | Clip_path -> normalize_clip_path value
  | Object_view_box -> normalize_object_view_box value
  | Object_position -> normalize_position_value value
  | Perspective_origin -> normalize_position_value value
  | Background_position -> map_preserve normalize_position_value value
  | Background_position_x -> normalize_background_position_axis value
  | Background_position_y -> normalize_background_position_axis value
  | Webkit_mask_position_x -> normalize_background_position_axis value
  | Webkit_mask_position_y -> normalize_background_position_axis value
  | Mask_position -> map_preserve normalize_position_value value
  | Webkit_mask_position -> map_preserve normalize_position_value value
  | Text_indent -> normalize_text_indent value
  | Animation_range -> normalize_animation_range value
  | Shape_image_threshold -> normalize_shape_image_threshold value
  | Tab_size -> normalize_tab_size ~ctx value
  | Hyphenate_limit_chars -> normalize_hyphenate_limit_chars ~ctx value
  | Animation_iteration_count -> normalize_animation_iteration_count ~ctx value
  | Webkit_animation_iteration_count ->
      normalize_animation_iteration_count ~ctx value
  | Moz_animation_iteration_count ->
      normalize_animation_iteration_count ~ctx value
  | Scroll_timeline -> normalize_timeline_shorthand value
  | View_timeline -> normalize_view_timeline_shorthand value
  | View_timeline_inset -> normalize_timeline_inset value
  | Baseline_shift -> normalize_baseline_shift value
  | Background_color -> normalize_color value
  | Color -> normalize_color value
  | Border_color ->
      normalize_box_shorthand ~is_substitution:is_color_substitution
        normalize_color value
  | Border_style ->
      normalize_box_shorthand ~is_substitution:is_border_style_substitution
        Fun.id value
  | Border_top_color -> normalize_color value
  | Border_right_color -> normalize_color value
  | Border_bottom_color -> normalize_color value
  | Border_left_color -> normalize_color value
  | Border_inline_start_color -> normalize_color value
  | Border_inline_end_color -> normalize_color value
  | Border_block_start_color -> normalize_color value
  | Border_block_end_color -> normalize_color value
  | Border_inline_color -> normalize_logical_border_color ~lossless value
  | Border_block_color -> normalize_logical_border_color ~lossless value
  | Text_decoration_color -> normalize_color value
  | Webkit_text_decoration_color -> normalize_color value
  | Webkit_text_fill_color -> normalize_color value
  | Webkit_text_stroke_color -> normalize_color value
  | Webkit_text_stroke_width -> normalize_border_width value
  | Column_rule_color -> List.map normalize_color value
  | Column_rule_width -> List.map normalize_border_width value
  | Webkit_tap_highlight_color -> normalize_color value
  | Text_emphasis_color -> normalize_color value
  | Outline_color -> normalize_color value
  | Accent_color -> normalize_color value
  | Caret_color -> normalize_color value
  | Stop_color -> normalize_color value
  | Flood_color -> normalize_color value
  | Lighting_color -> normalize_color value
  | Border -> normalize_border ~lossless value
  | Border_block -> normalize_border ~lossless value
  | Border_block_start -> normalize_border ~lossless value
  | Border_block_end -> normalize_border ~lossless value
  | Border_inline -> normalize_border ~lossless value
  | Border_inline_start -> normalize_border ~lossless value
  | Border_inline_end -> normalize_border ~lossless value
  | Border_top -> normalize_border ~lossless value
  | Border_right -> normalize_border ~lossless value
  | Border_bottom -> normalize_border ~lossless value
  | Border_left -> normalize_border ~lossless value
  | Column_rule -> List.map (normalize_border ~lossless) value
  | Outline -> normalize_outline ~lossless value
  | Box_shadow -> normalize_shadow ~lossless value
  | Text_shadow -> map_preserve (normalize_text_shadow ~lossless) value
  | Text_decoration -> normalize_text_decoration ~lossless value
  | Webkit_text_decoration -> normalize_text_decoration ~lossless value
  | Text_emphasis -> normalize_text_emphasis ~lossless value
  | Flex_flow -> normalize_flex_flow value
  | Caret -> normalize_caret ~lossless value
  | Interest_delay -> normalize_interest_delay value
  | Interest_delay_start -> normalize_interest_delay value
  | Interest_delay_end -> normalize_interest_delay value
  | Fill -> normalize_svg_paint ~lossless value
  | Stroke -> normalize_svg_paint ~lossless value
  | Scrollbar_color -> normalize_scrollbar_color ~lossless value
  | Filter -> normalize_filter ~lossless value
  | Webkit_filter -> normalize_filter ~lossless value
  | Ms_filter -> normalize_filter ~lossless value
  | Backdrop_filter -> normalize_filter ~lossless value
  | Webkit_backdrop_filter -> normalize_filter ~lossless value
  | Flex_grow -> normalize_flex_factor value
  | Stroke_miterlimit -> normalize_stroke_miterlimit value
  | Stroke_dashoffset -> normalize_stroke_dashoffset ~ctx value
  | Stroke_dasharray -> normalize_stroke_dasharray ~ctx value
  | Paint_order -> normalize_paint_order value
  | Vector_effect -> normalize_vector_effect value
  | Flex_shrink -> normalize_flex_factor value
  | Flex_basis -> normalize_flex_basis ~ctx value
  | Flex -> normalize_flex ~ctx value
  | Grid_template_columns -> normalize_grid_template value
  | Grid_template_rows -> normalize_grid_template value
  | Grid_template -> normalize_grid_template value
  | Grid -> normalize_grid_template value
  | Grid_auto_columns -> normalize_grid_template value
  | Grid_auto_rows -> normalize_grid_template value
  | Grid_auto_flow -> normalize_grid_auto_flow value
  | Aspect_ratio -> normalize_aspect_ratio value
  | Gap -> normalize_gap value
  | Font_size -> normalize_font_size value
  | Font_weight -> normalize_font_weight value
  | Webkit_line_clamp -> normalize_webkit_line_clamp value
  | Font_family -> normalize_font_family value
  | Font_stretch -> normalize_font_stretch value
  | Font_size_adjust -> normalize_font_size_adjust value
  | Initial_letter -> normalize_initial_letter value
  | Text_size_adjust -> normalize_text_size_adjust value
  | Webkit_text_size_adjust -> normalize_text_size_adjust value
  | Font -> normalize_font value
  | Display -> normalize_display value
  | Overflow -> normalize_overflow value
  | Transition -> map_preserve normalize_transition value
  | Webkit_transition -> map_preserve normalize_transition value
  | Moz_transition -> map_preserve normalize_transition value
  | O_transition -> map_preserve normalize_transition value
  | List_style -> normalize_list_style value
  | Transition_timing_function -> normalize_timing_function value
  | Animation -> map_preserve (normalize_animation ~ctx) value
  | Webkit_animation -> map_preserve (normalize_animation ~ctx) value
  | Moz_animation -> map_preserve (normalize_animation ~ctx) value
  | Animation_timing_function -> normalize_timing_function value
  | Padding_left -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_right -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_bottom -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_top -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_inline_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Padding_inline_end -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_block_start -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_block_end -> Values.normalize_length ~non_negative:true ~ctx value
  | Margin_inline_end -> Values.normalize_length ~ctx value
  | Margin_inline_start -> Values.normalize_length ~ctx value
  | Margin_left -> Values.normalize_length ~ctx value
  | Margin_right -> Values.normalize_length ~ctx value
  | Margin_top -> Values.normalize_length ~ctx value
  | Margin_bottom -> Values.normalize_length ~ctx value
  | Margin_block_start -> Values.normalize_length ~ctx value
  | Margin_block_end -> Values.normalize_length ~ctx value
  | Column_gap -> Values.normalize_length ~non_negative:true ~ctx value
  | Row_gap -> Values.normalize_length ~non_negative:true ~ctx value
  | Text_underline_offset -> Values.normalize_length ~ctx value
  | Letter_spacing -> Values.normalize_length ~ctx value
  | Border_top_left_radius -> normalize_length_box ~non_negative:true ~ctx value
  | Border_top_right_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_bottom_left_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_bottom_right_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_start_start_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_start_end_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_end_start_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_end_end_radius -> normalize_length_box ~non_negative:true ~ctx value
  | Outline_width -> normalize_border_width value
  | Outline_offset -> Values.normalize_length ~ctx value
  | Line_height_step -> Values.normalize_length ~non_negative:true ~ctx value
  | Perspective -> Values.normalize_length ~non_negative:true ~ctx value
  | Word_spacing -> Values.normalize_length ~ctx value
  | Text_decoration_thickness -> Values.normalize_length ~ctx value
  | Stroke_width -> normalize_stroke_width ~ctx value
  | Scroll_margin_top -> Values.normalize_length ~ctx value
  | Scroll_margin_right -> Values.normalize_length ~ctx value
  | Scroll_margin_bottom -> Values.normalize_length ~ctx value
  | Scroll_margin_left -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_start -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_end -> Values.normalize_length ~ctx value
  | Scroll_margin_block_start -> Values.normalize_length ~ctx value
  | Scroll_margin_block_end -> Values.normalize_length ~ctx value
  | Scroll_padding_top -> Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_right ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_bottom ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_left -> Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_inline_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_inline_end ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_block_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_block_end ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Padding -> normalize_length_box ~non_negative:true ~ctx value
  | Padding_inline -> normalize_length_box ~non_negative:true ~ctx value
  | Padding_block -> normalize_length_box ~non_negative:true ~ctx value
  | Margin -> normalize_length_box ~ctx value
  | Margin_inline -> normalize_length_box ~ctx value
  | Margin_block -> normalize_length_box ~ctx value
  | Inset -> normalize_length_box ~ctx value
  | Inset_inline -> normalize_length_box ~ctx value
  | Inset_inline_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_end -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block -> normalize_length_box ~ctx value
  | Inset_block_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_end -> map_preserve (Values.normalize_length ~ctx) value
  | Top -> map_preserve (Values.normalize_length ~ctx) value
  | Right -> map_preserve (Values.normalize_length ~ctx) value
  | Bottom -> map_preserve (Values.normalize_length ~ctx) value
  | Left -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin -> normalize_length_box ~ctx value
  | Scroll_margin_inline -> normalize_length_box ~ctx value
  | Scroll_margin_block -> normalize_length_box ~ctx value
  | Scroll_padding -> normalize_length_box ~ctx value
  | Scroll_padding_inline -> normalize_length_box ~ctx value
  | Scroll_padding_block -> normalize_length_box ~ctx value
  | Custom_property _ -> (
      match value with
      | Custom_value ({ value = Tokens components; _ } as r) ->
          let components' =
            components
            |> canonicalize_custom_shadow_components ~lossless
            |> canonicalize_custom_colors_components ~lossless ~resolve_missing
            |> canonicalize_math_whitespace_components
          in
          if components' == components then value
          else Custom_value { r with value = Tokens components' }
      | Custom_value _ -> value)
  | Opacity -> normalize_opacity value
  | Fill_opacity -> normalize_opacity value
  | Stroke_opacity -> normalize_opacity value
  | Stop_opacity -> normalize_opacity value
  | Flood_opacity -> normalize_opacity value
  | Line_height -> normalize_line_height ~lossless value
  | Vertical_align -> normalize_vertical_align value
  | Border_image -> normalize_border_image value
  | Columns -> normalize_columns_value value
  | Content -> normalize_content value
  | Column_width -> normalize_column_width value
  | Page_size -> normalize_page_size value
  | Initial_letter_wrap -> normalize_initial_letter_wrap value
  | Background_size -> normalize_background_size value
  | Webkit_background_size -> normalize_background_size value
  | Mask_size -> normalize_background_size value
  | Webkit_mask_size -> normalize_background_size value
  | Column_count -> normalize_column_count value
  | Border_width ->
      normalize_box_shorthand ~is_substitution:is_border_width_substitution
        normalize_border_width value
  | Border_top_width -> normalize_border_width value
  | Border_right_width -> normalize_border_width value
  | Border_bottom_width -> normalize_border_width value
  | Border_left_width -> normalize_border_width value
  | Border_inline_start_width -> normalize_border_width value
  | Border_inline_end_width -> normalize_border_width value
  | Border_block_start_width -> normalize_border_width value
  | Border_block_end_width -> normalize_border_width value
  | Border_inline_width -> normalize_logical_border_width value
  | Border_block_width -> normalize_logical_border_width value
  | Border_inline_style -> normalize_logical_border_style value
  | Border_block_style -> normalize_logical_border_style value
  | Overflow_clip_margin -> normalize_overflow_clip_margin value
  | Contain_intrinsic_size -> normalize_contain_intrinsic_size value
  | Contain_intrinsic_width -> normalize_contain_intrinsic_longhand value
  | Contain_intrinsic_height -> normalize_contain_intrinsic_longhand value
  | Contain_intrinsic_inline_size -> normalize_contain_intrinsic_longhand value
  | Contain_intrinsic_block_size -> normalize_contain_intrinsic_longhand value
  | Transition_duration -> Values.normalize_duration ~ctx value
  | Transition_delay -> Values.normalize_duration ~ctx value
  | Animation_duration -> Values.normalize_duration ~ctx value
  | Animation_delay -> Values.normalize_duration ~ctx value
  | Webkit_transition_duration -> Values.normalize_duration ~ctx value
  | Webkit_transition_delay -> Values.normalize_duration ~ctx value
  | Webkit_animation_duration -> Values.normalize_duration ~ctx value
  | Webkit_animation_delay -> Values.normalize_duration ~ctx value
  | Moz_transition_duration -> Values.normalize_duration ~ctx value
  | Moz_transition_delay -> Values.normalize_duration ~ctx value
  | Moz_animation_duration -> Values.normalize_duration ~ctx value
  | Moz_animation_delay -> Values.normalize_duration ~ctx value
  | _ -> value
