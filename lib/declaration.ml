(** CSS declaration types and parser. *)

include Declaration_intf
open Common
open Properties
open Values
include Declaration_core
include Declaration_value
include Declaration_reader

(* Pretty printer for declarations *)
let rec pp : declaration Pp.t =
 fun ctx -> function
  | Declaration
      {
        property = Custom_property name;
        value = Custom_value { value; layer; _ };
        important;
        _;
      } ->
      pp_property ctx (Custom_property name);
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx
        (Custom_property name, Custom_value { value; layer; meta = None });
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Declaration { property; value; important; _ } ->
      (* Under minify [pp_property] gives a [page-break-*] property the
         [break-*] property it aliases, which CSS Fragmentation 3 sec. 3.4
         defines by a value mapping table. A value the table does not name has
         no alias spelling, so the name follows the value back to the legacy
         property rather than writing it under one that cannot express it. *)
      if minified_name_carries property value then pp_property ctx property
      else Pp.string ctx (canonical_prop_name property);
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (property, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Theme_guarded { decl; _ } ->
      (* Theme guards are resolved by the transform layer; if one survives to
         print time, emit the wrapped declaration. *)
      pp ctx decl

(* A declaration feature query hands the authored declaration to another parser.
   Minify its separators, but keep opaque numeric token spellings: the spelling
   itself is the compatibility question. *)
(* The value half of {!pp_opaque}, for a caller writing the property name
   itself: an [\@supports] feature keeps the name the author spelled, which the
   typed property behind it cannot reproduce. *)
let rec pp_opaque_value ?(verbatim = true) : declaration Pp.t =
 fun ctx decl ->
  let pp_components components important =
    Pp.string ctx
      (if verbatim && not (Pp.minified ctx) then
         Parser.to_string_verbatim components
       else Parser.to_string_minified components);
    if important then
      Pp.string ctx (if ctx.minify then "!important" else " !important")
  in
  match decl with
  | Declaration { property = Unknown_property _; value; important; _ } ->
      pp_components value important
  | Declaration
      {
        property = Custom_property _;
        value = Custom_value { value = Tokens components; _ };
        important;
        _;
      } ->
      pp_components components important
  | Declaration { property; value; important; _ } ->
      pp_property_value ctx (property, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Theme_guarded { decl; _ } -> pp_opaque_value ~verbatim ctx decl

let rec pp_opaque : declaration Pp.t =
 fun ctx decl ->
  (* Every caller of this is an [@supports] condition, and CSS Conditional Rules
     3 sec. 6.1 answers a declaration feature by running that exact declaration
     through the rendering browser's parser. The text is the question, so it is
     written back as read rather than respelled. *)
  let pp_components property components important =
    pp_property ctx property;
    Pp.char ctx ':';
    Pp.space_if_pretty ctx ();
    Pp.string ctx
      (if Pp.minified ctx then Parser.to_string_minified components
       else Parser.to_string_verbatim components);
    if important then
      Pp.string ctx (if ctx.minify then "!important" else " !important")
  in
  match decl with
  | Declaration
      { property = Unknown_property _ as property; value; important; _ } ->
      pp_components property value important
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value { value = Tokens components; _ };
        important;
        _;
      } ->
      pp_components property components important
  | Declaration _ -> pp ctx decl
  | Theme_guarded { decl; _ } -> pp_opaque ctx decl

(* Convert a declaration to its string representation *)
let to_string ?(minify = false) (decl : t) =
  let buf = Buffer.create 32 in
  let ctx = Pp.ctx ~minify buf in
  pp ctx decl;
  Buffer.contents buf

(* Single-to-list property helpers *)
let require_nonempty name = function [] -> invalid_arg name | values -> values
let background_image value = v Background_image [ value ]
let text_shadow value = v Text_shadow [ value ]

let text_shadows values =
  v Text_shadow (require_nonempty "text_shadows: empty list" values)

let transition value = v Transition [ value ]

let transitions values =
  v Transition (require_nonempty "transitions: empty list" values)

let animation value = v Animation [ value ]
let box_shadow value = v Box_shadow value

let box_shadows values =
  v Box_shadow (List (require_nonempty "box_shadows: empty list" values))

(* Special helpers *)
let z_index_auto = v Z_index Auto

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = (Tokens tokens : font_variant_numeric)

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background bg = v Background [ bg ]
let background_color c = v Background_color c
let color c = v Color c
let border_color c = v Border_color [ c ]
let border_style bs = v Border_style [ bs ]
let border_top_style bs = v Border_top_style bs
let border_right_style bs = v Border_right_style bs
let border_bottom_style bs = v Border_bottom_style bs
let border_left_style bs = v Border_left_style bs
let text_decoration td = v Text_decoration td
let font_style fs = v Font_style fs
let list_style_type lst = v List_style_type lst
let list_style_position ls = v List_style_position ls
let list_style_image is = v List_style_image is
let padding values = v Padding (require_nonempty "padding: empty list" values)
let padding_left len = v Padding_left len
let padding_right len = v Padding_right len
let padding_bottom len = v Padding_bottom len
let padding_top len = v Padding_top len
let margin values = v Margin (require_nonempty "margin: empty list" values)
let margin_left len = v Margin_left len
let margin_right len = v Margin_right len
let margin_top len = v Margin_top len
let margin_bottom len = v Margin_bottom len

(* Remove deprecated string-based versions *)
let gap len = v Gap len
let column_gap len = v Column_gap len
let row_gap len = v Row_gap len

(* Grid functions *)
let grid_template_areas value = v Grid_template_areas value
let grid_template template = v Grid_template template
let grid_auto_columns size = v Grid_auto_columns size
let grid_auto_rows size = v Grid_auto_rows size
let grid_row_start value = v Grid_row_start value
let grid_row_end value = v Grid_row_end value
let grid_column_start value = v Grid_column_start value
let grid_column_end value = v Grid_column_end value
let grid_row (start, end_) = v Grid_row (Lines (start, end_))
let grid_column (start, end_) = v Grid_column (Lines (start, end_))
let grid_area value = v Grid_area value
let width len = v Width (Length len)
let height len = v Height (Length len)

(* Remove deprecated string-based versions *)
let min_width len = v Min_width (Length len)
let min_height len = v Min_height (Length len)
let max_width len = v Max_width (Length len)
let max_height len = v Max_height (Length len)
let inline_size len = v Inline_size (Length len)
let min_inline_size len = v Min_inline_size (Length len)
let max_inline_size len = v Max_inline_size (Length len)
let block_size len = v Block_size (Length len)
let min_block_size len = v Min_block_size (Length len)
let max_block_size len = v Max_block_size (Length len)
let font_size len = v Font_size (Length len)
let font_size_kw fs = v Font_size fs
let line_height len = v Line_height len
let font_weight w = v Font_weight w
let text_align a = v Text_align a
let text_decoration_style value = v Text_decoration_style value
let text_decoration_line value = v Text_decoration_line [ value ]
let text_underline_offset value = v Text_underline_offset value
let text_decoration_skip value = v Text_decoration_skip value
let text_decoration_skip_self value = v Text_decoration_skip_self value
let text_decoration_skip_box value = v Text_decoration_skip_box value
let text_decoration_skip_inset value = v Text_decoration_skip_inset value
let text_decoration_skip_spaces value = v Text_decoration_skip_spaces value
let text_emphasis value = v Text_emphasis value
let text_emphasis_style value = v Text_emphasis_style value
let text_emphasis_color value = v Text_emphasis_color value
let text_emphasis_position value = v Text_emphasis_position value
let text_emphasis_skip value = v Text_emphasis_skip value
let text_orientation value = v Text_orientation value
let text_transform value = v Text_transform value
let letter_spacing len = v Letter_spacing len
let white_space value = v White_space value
let display d = v Display d
let position p = v Position p
let visibility p = v Visibility p
let inset values = v Inset (require_nonempty "inset: empty list" values)

let inset_inline values =
  v Inset_inline (require_nonempty "inset_inline: empty list" values)

let inset_inline_start len = v Inset_inline_start [ len ]
let inset_inline_end len = v Inset_inline_end [ len ]

let inset_block values =
  v Inset_block (require_nonempty "inset_block: empty list" values)

let inset_block_start len = v Inset_block_start [ len ]
let inset_block_end len = v Inset_block_end [ len ]
let top len = v Top [ len ]
let right len = v Right [ len ]
let bottom len = v Bottom [ len ]
let left len = v Left [ len ]
let opacity value = v Opacity value

(* Remove deprecated string-based versions *)
let flex_direction d = v Flex_direction d
let flex value = v Flex value
let flex_grow value = v Flex_grow (Number value : flex_factor)
let flex_shrink value = v Flex_shrink (Number value : flex_factor)
let flex_basis value = v Flex_basis value
let flex_wrap value = v Flex_wrap value
let flex_flow value = v Flex_flow value
let order value = v Order value
let align_items a = v Align_items a
let align_content a = v Align_content a
let align_self a = v Align_self a
let justify_content a = v Justify_content a
let justify_items a = v Justify_items a
let justify_self a = v Justify_self a
let place_content value = v Place_content value
let place_items value = v Place_items value
let place_self value = v Place_self value
let border_width len = v Border_width [ len ]
let border_radius len = v Border_radius len
let border_top_left_radius len = v Border_top_left_radius [ len ]
let border_top_right_radius len = v Border_top_right_radius [ len ]
let border_bottom_left_radius len = v Border_bottom_left_radius [ len ]
let border_bottom_right_radius len = v Border_bottom_right_radius [ len ]
let fill value = v Fill value
let stroke value = v Stroke value
let stroke_width value = v Stroke_width value
let fill_rule value = v Fill_rule value
let clip_rule value = v Clip_rule value
let fill_opacity value = v Fill_opacity value
let stroke_opacity value = v Stroke_opacity value
let stroke_linecap value = v Stroke_linecap value
let stroke_linejoin value = v Stroke_linejoin value
let stroke_miterlimit value = v Stroke_miterlimit value
let stroke_dashoffset value = v Stroke_dashoffset value
let stroke_dasharray value = v Stroke_dasharray value
let paint_order value = v Paint_order value
let vector_effect value = v Vector_effect value
let stop_color value = v Stop_color value
let stop_opacity value = v Stop_opacity value
let flood_color value = v Flood_color value
let flood_opacity value = v Flood_opacity value
let lighting_color value = v Lighting_color value
let dominant_baseline value = v Dominant_baseline value
let alignment_baseline value = v Alignment_baseline value
let baseline_shift value = v Baseline_shift value
let baseline_source value = v Baseline_source value
let anchor_name value = v Anchor_name value
let position_anchor value = v Position_anchor value
let position_area value = v Position_area value
let position_try_fallbacks value = v Position_try_fallbacks value
let position_try_order value = v Position_try_order value
let position_try value = v Position_try value
let position_visibility value = v Position_visibility value
let view_transition_name value = v View_transition_name value
let view_transition_class value = v View_transition_class value
let all value = v All value
let offset_path value = v Offset_path value
let offset_distance value = v Offset_distance value
let offset_rotate value = v Offset_rotate value
let offset_anchor value = v Offset_anchor value
let offset_position value = v Offset_position value
let offset value = v Offset value
let column_width value = v Column_width value
let column_count value = v Column_count value
let column_height value = v Column_height value
let column_wrap value = v Column_wrap value
let column_rule_width value = v Column_rule_width value
let column_rule_style value = v Column_rule_style value
let column_rule_color value = v Column_rule_color value
let border_image value = v Border_image value
let border_image_source value = v Border_image_source value
let border_image_slice value = v Border_image_slice value
let border_image_width value = v Border_image_width value
let border_image_outset value = v Border_image_outset value
let border_image_repeat value = v Border_image_repeat value
let mask_border value = v Mask_border value
let contain_intrinsic_size value = v Contain_intrinsic_size value
let contain_intrinsic_width value = v Contain_intrinsic_width value
let contain_intrinsic_height value = v Contain_intrinsic_height value
let contain_intrinsic_block_size value = v Contain_intrinsic_block_size value
let contain_intrinsic_inline_size value = v Contain_intrinsic_inline_size value
let animation_timeline value = v Animation_timeline value
let animation_range value = v Animation_range value
let animation_range_start value = v Animation_range_start value
let animation_range_end value = v Animation_range_end value
let scroll_timeline value = v Scroll_timeline value
let scroll_timeline_name value = v Scroll_timeline_name value
let scroll_timeline_axis value = v Scroll_timeline_axis value
let view_timeline value = v View_timeline value
let view_timeline_name value = v View_timeline_name value
let view_timeline_axis value = v View_timeline_axis value
let view_timeline_inset value = v View_timeline_inset value
let timeline_scope value = v Timeline_scope value
let shape_image_threshold value = v Shape_image_threshold value
let shape_margin value = v Shape_margin value
let shape_outside value = v Shape_outside value
let overflow_clip_margin value = v Overflow_clip_margin value
let overflow_anchor value = v Overflow_anchor value
let overflow_block value = v Overflow_block value
let overflow_inline value = v Overflow_inline value
let overscroll_behavior_block value = v Overscroll_behavior_block value
let overscroll_behavior_inline value = v Overscroll_behavior_inline value
let image_orientation value = v Image_orientation value
let margin_trim value = v Margin_trim value
let overlay value = v Overlay value
let animation_composition value = v Animation_composition value
let background_position_x value = v Background_position_x value
let background_position_y value = v Background_position_y value
let webkit_mask_position_x value = v Webkit_mask_position_x value
let webkit_mask_position_y value = v Webkit_mask_position_y value
let moz_orient value = v Moz_orient value
let grid value = v Grid value
let webkit_text_stroke value = v Webkit_text_stroke value
let page_size value = v Page_size value
let white_space_collapse value = v White_space_collapse value
let line_height_step value = v Line_height_step value
let font_palette value = v Font_palette value
let font_synthesis value = v Font_synthesis value
let font_size_adjust value = v Font_size_adjust value
let font_variant_emoji value = v Font_variant_emoji value
let font_variant_alternates value = v Font_variant_alternates value
let font_variant value = v Font_variant value
let text_wrap_style value = v Text_wrap_style value
let text_box_trim value = v Text_box_trim value
let text_box value = v Text_box value
let text_spacing_trim value = v Text_spacing_trim value
let hyphenate_limit_chars value = v Hyphenate_limit_chars value
let initial_letter value = v Initial_letter value
let initial_letter_align value = v Initial_letter_align value
let initial_letter_wrap value = v Initial_letter_wrap value
let border_block_start value = v Border_block_start value
let border_block_end value = v Border_block_end value
let border_inline value = v Border_inline value
let border_inline_start value = v Border_inline_start value
let border_inline_end value = v Border_inline_end value
let moz_user_select value = v Moz_user_select value
let ms_user_select value = v Ms_user_select value
let webkit_text_fill_color value = v Webkit_text_fill_color value
let webkit_text_stroke_width value = v Webkit_text_stroke_width value
let webkit_text_stroke_color value = v Webkit_text_stroke_color value
let webkit_transform value = v Webkit_transform value
let moz_transform value = v Moz_transform value
let ms_transform value = v Ms_transform value
let o_transform value = v O_transform value
let webkit_transition value = v Webkit_transition value
let webkit_transition_delay value = v Webkit_transition_delay value
let webkit_transition_duration value = v Webkit_transition_duration value
let webkit_transition_property value = v Webkit_transition_property value

let webkit_transition_timing_function value =
  v Webkit_transition_timing_function value

let webkit_animation value = v Webkit_animation value
let webkit_animation_delay value = v Webkit_animation_delay value
let webkit_animation_duration value = v Webkit_animation_duration value
let webkit_animation_direction value = v Webkit_animation_direction value

let webkit_animation_iteration_count value =
  v Webkit_animation_iteration_count value

let webkit_animation_name value = v Webkit_animation_name value

let webkit_animation_timing_function value =
  v Webkit_animation_timing_function value

let webkit_animation_fill_mode value = v Webkit_animation_fill_mode value
let webkit_animation_play_state value = v Webkit_animation_play_state value
let webkit_flex_direction value = v Webkit_flex_direction value
let webkit_flex_wrap value = v Webkit_flex_wrap value
let webkit_flex_flow value = v Webkit_flex_flow value
let webkit_justify_content value = v Webkit_justify_content value
let webkit_align_items value = v Webkit_align_items value
let webkit_align_content value = v Webkit_align_content value
let webkit_align_self value = v Webkit_align_self value
let webkit_border_radius value = v Webkit_border_radius value
let webkit_box_sizing value = v Webkit_box_sizing value
let moz_box_sizing value = v Moz_box_sizing value
let webkit_box_shadow value = v Webkit_box_shadow value
let webkit_background_size value = v Webkit_background_size value
let webkit_filter value = v Webkit_filter value
let moz_appearance value = v Moz_appearance value
let moz_animation value = v Moz_animation value
let moz_animation_delay value = v Moz_animation_delay value
let moz_animation_duration value = v Moz_animation_duration value
let moz_animation_direction value = v Moz_animation_direction value
let moz_animation_iteration_count value = v Moz_animation_iteration_count value
let moz_animation_name value = v Moz_animation_name value
let moz_animation_timing_function value = v Moz_animation_timing_function value
let moz_animation_fill_mode value = v Moz_animation_fill_mode value
let moz_animation_play_state value = v Moz_animation_play_state value
let moz_transition value = v Moz_transition value
let moz_transition_delay value = v Moz_transition_delay value
let moz_transition_duration value = v Moz_transition_duration value
let moz_transition_property value = v Moz_transition_property value

let moz_transition_timing_function value =
  v Moz_transition_timing_function value

let moz_border_radius value = v Moz_border_radius value
let moz_box_shadow value = v Moz_box_shadow value
let ms_filter value = v Ms_filter value
let o_transition value = v O_transition value
let outline_style o = v Outline_style o
let outline_width len = v Outline_width len
let outline_color c = v Outline_color c
let forced_color_adjust c = v Forced_color_adjust c
let table_layout value = v Table_layout value
let border_spacing lens = v Border_spacing lens
let overflow o = v Overflow o
let object_fit value = v Object_fit value
let object_view_box value = v Object_view_box value
let clip value = v Clip value
let clear value = v Clear value
let float value = v Float value
let interactivity value = v Interactivity value
let caret_animation value = v Caret_animation value
let caret_shape value = v Caret_shape value
let caret value = v Caret value
let interest_delay value = v Interest_delay value
let interest_delay_start value = v Interest_delay_start value
let interest_delay_end value = v Interest_delay_end value
let nav_up value = v Nav_up value
let nav_right value = v Nav_right value
let nav_down value = v Nav_down value
let nav_left value = v Nav_left value
let touch_action value = v Touch_action value
let direction value = v Direction value
let unicode_bidi value = v Unicode_bidi value
let writing_mode value = v Writing_mode value
let text_combine_upright value = v Text_combine_upright value
let text_decoration_skip_ink value = v Text_decoration_skip_ink value
let animation_name value = v Animation_name value
let animation_duration value = v Animation_duration value
let animation_timing_function value = v Animation_timing_function value
let animation_delay value = v Animation_delay value
let animation_iteration_count value = v Animation_iteration_count value
let animation_direction value = v Animation_direction value
let animation_fill_mode value = v Animation_fill_mode value
let animation_play_state value = v Animation_play_state value
let background_blend_mode value = v Background_blend_mode [ value ]

let scroll_margin values =
  v Scroll_margin (require_nonempty "scroll_margin: empty list" values)

let scroll_margin_top value = v Scroll_margin_top value
let scroll_margin_right value = v Scroll_margin_right value
let scroll_margin_bottom value = v Scroll_margin_bottom value
let scroll_margin_left value = v Scroll_margin_left value

let scroll_margin_inline values =
  v Scroll_margin_inline
    (require_nonempty "scroll_margin_inline: empty list" values)

let scroll_margin_inline_start value = v Scroll_margin_inline_start value
let scroll_margin_inline_end value = v Scroll_margin_inline_end value

let scroll_margin_block values =
  v Scroll_margin_block
    (require_nonempty "scroll_margin_block: empty list" values)

let scroll_margin_block_start value = v Scroll_margin_block_start value
let scroll_margin_block_end value = v Scroll_margin_block_end value

let scroll_padding values =
  v Scroll_padding (require_nonempty "scroll_padding: empty list" values)

let scroll_padding_top value = v Scroll_padding_top value
let scroll_padding_right value = v Scroll_padding_right value
let scroll_padding_bottom value = v Scroll_padding_bottom value
let scroll_padding_left value = v Scroll_padding_left value

let scroll_padding_inline values =
  v Scroll_padding_inline
    (require_nonempty "scroll_padding_inline: empty list" values)

let scroll_padding_inline_start value = v Scroll_padding_inline_start value
let scroll_padding_inline_end value = v Scroll_padding_inline_end value

let scroll_padding_block values =
  v Scroll_padding_block
    (require_nonempty "scroll_padding_block: empty list" values)

let scroll_padding_block_start value = v Scroll_padding_block_start value
let scroll_padding_block_end value = v Scroll_padding_block_end value

let overscroll_behavior values =
  v Overscroll_behavior
    (require_nonempty "overscroll_behavior: empty list" values)

let overscroll_behavior_x value = v Overscroll_behavior_x value
let overscroll_behavior_y value = v Overscroll_behavior_y value
let accent_color value = v Accent_color value
let caret_color value = v Caret_color value
let text_decoration_color value = v Text_decoration_color value
let text_decoration_thickness value = v Text_decoration_thickness value
let text_size_adjust value = v Text_size_adjust value
let aspect_ratio a = v Aspect_ratio a
let filter value = v Filter value

let filter_var_empty name : filter =
  Var
    {
      name;
      fallback = Empty;
      default = None;
      layer = None;
      meta = None;
      runtime = false;
    }

let background_image_var_none name : background_image =
  Var
    {
      name;
      fallback = None;
      default = None;
      layer = None;
      meta = None;
      runtime = false;
    }

let word_spacing value = v Word_spacing value
let quotes value = v Quotes value

let border ?width ?style ?color () =
  let border_value : border =
    match (width, style, color) with
    | None, None, None -> None
    | _ -> Shorthand { width; style; color }
  in
  v Border border_value

let border_block value = v Border_block value
let tab_size value = v Tab_size (Number (Num (float_of_int value)) : tab_size)
let tab_size_value value = v Tab_size (value : tab_size)
let scrollbar_width value = v Scrollbar_width value
let scrollbar_color value = v Scrollbar_color value
let scrollbar_gutter value = v Scrollbar_gutter value
let zoom value = v Zoom value
let webkit_text_size_adjust value = v Webkit_text_size_adjust value
let font_feature_settings value = v Font_feature_settings value
let font_variation_settings value = v Font_variation_settings value
let webkit_tap_highlight_color value = v Webkit_tap_highlight_color value
let webkit_text_decoration value = v Webkit_text_decoration value
let webkit_text_decoration_color value = v Webkit_text_decoration_color value
let text_indent value = v Text_indent value
let border_collapse value = v Border_collapse value
let list_style value = v List_style value
let font value = v Font value
let webkit_appearance value = v Webkit_appearance value
let transform_style value = v Transform_style value
let backface_visibility value = v Backface_visibility value
let object_position value = v Object_position value
let transition_duration value = v Transition_duration value
let transition_timing_function value = v Transition_timing_function value
let transition_delay value = v Transition_delay value
let transition_behavior value = v Transition_behavior value
let transition_property value = v Transition_property value

(* Additional v constructors to match the interface *)
let mix_blend_mode value = v Mix_blend_mode value
let grid_template_columns value = v Grid_template_columns value
let grid_template_rows value = v Grid_template_rows value
let grid_auto_flow value = v Grid_auto_flow value
let pointer_events value = v Pointer_events value
let z_index value = v Z_index value
let appearance value = v Appearance value
let overflow_x value = v Overflow_x value
let overflow_y value = v Overflow_y value
let resize value = v Resize value
let vertical_align value = v Vertical_align value
let box_sizing value = v Box_sizing value
let field_sizing value = v Field_sizing value
let caption_side value = v Caption_side value
let font_family value = v Font_family value
let print_color_adjust value = v Print_color_adjust value
let webkit_print_color_adjust value = v Webkit_print_color_adjust value
let box_decoration_break value = v Box_decoration_break value
let webkit_box_decoration_break value = v Webkit_box_decoration_break value
let background_origin value = v Background_origin value
let background_clip value = v Background_clip value
let webkit_background_clip value = v Webkit_background_clip value

let font_families fonts =
  v Font_family (List (require_nonempty "font_families: empty list" fonts))

let background_attachment value = v Background_attachment value
let border_top value = v Border_top value
let border_right value = v Border_right value
let border_bottom value = v Border_bottom value
let border_left value = v Border_left value
let transform_origin value = v Transform_origin value
let transform_box value = v Transform_box value
let clip_path value = v Clip_path value
let mask value = v Mask value
let webkit_mask_image value = v Webkit_mask_image value
let mask_image value = v Mask_image value
let webkit_mask_composite value = v Webkit_mask_composite value
let mask_composite value = v Mask_composite value
let webkit_mask_source_type value = v Webkit_mask_source_type value
let mask_mode value = v Mask_mode value
let mask_type value = v Mask_type value
let webkit_mask_size value = v Webkit_mask_size value
let mask_size value = v Mask_size value

let webkit_mask_position values =
  v Webkit_mask_position
    (require_nonempty "webkit_mask_position: empty list" values)

let mask_position values =
  v Mask_position (require_nonempty "mask_position: empty list" values)

let webkit_mask_repeat value = v Webkit_mask_repeat value
let mask_repeat value = v Mask_repeat value
let webkit_mask_clip value = v Webkit_mask_clip value
let mask_clip value = v Mask_clip value
let webkit_mask_origin value = v Webkit_mask_origin value
let mask_origin value = v Mask_origin value
let content_visibility value = v Content_visibility value
let moz_osx_font_smoothing value = v Moz_osx_font_smoothing value
let webkit_line_clamp value = v Webkit_line_clamp value
let webkit_box_orient value = v Webkit_box_orient value
let text_overflow value = v Text_overflow value
let text_wrap value = v Text_wrap value
let text_wrap_mode value = v Text_wrap_mode value
let text_underline_position value = v Text_underline_position value
let text_box_edge value = v Text_box_edge value
let inline_sizing value = v Inline_sizing value
let line_fit_edge value = v Line_fit_edge value
let interpolate_size value = v Interpolate_size value
let min_intrinsic_sizing value = v Min_intrinsic_sizing value
let ruby_align value = v Ruby_align value
let ruby_merge value = v Ruby_merge value
let ruby_overhang value = v Ruby_overhang value
let ruby_position value = v Ruby_position value
let glyph_orientation_vertical value = v Glyph_orientation_vertical value
let word_break value = v Word_break value
let overflow_wrap value = v Overflow_wrap value
let line_break value = v Line_break value
let hyphens value = v Hyphens value
let webkit_hyphens value = v Webkit_hyphens value
let font_stretch value = v Font_stretch value
let font_optical_sizing value = v Font_optical_sizing value
let font_kerning value = v Font_kerning value
let font_language_override value = v Font_language_override value
let font_synthesis_style value = v Font_synthesis_style value
let font_synthesis_weight value = v Font_synthesis_weight value
let font_synthesis_small_caps value = v Font_synthesis_small_caps value
let font_synthesis_position value = v Font_synthesis_position value
let font_variant_ligatures value = v Font_variant_ligatures value
let font_variant_caps value = v Caps value
let font_variant_numeric value = v Numeric value
let font_variant_position value = v Font_variant_position value
let font_variant_east_asian value = v East_asian value
let backdrop_filter value = v Backdrop_filter value
let webkit_backdrop_filter value = v Webkit_backdrop_filter value

let background_position values =
  v Background_position
    (require_nonempty "background_position: empty list" values)

let background_repeat value = v Background_repeat value
let background_size value = v Background_size value
let content value = v Content value
let counter_reset value = v Counter_reset value
let counter_increment value = v Counter_increment value
let border_left_width value = v Border_left_width value
let border_inline_start_width value = v Border_inline_start_width value
let border_inline_end_width value = v Border_inline_end_width value
let border_block_start_width value = v Border_block_start_width value
let border_block_end_width value = v Border_block_end_width value
let border_bottom_width value = v Border_bottom_width value
let border_top_width value = v Border_top_width value
let border_right_width value = v Border_right_width value
let border_top_color value = v Border_top_color value
let border_right_color value = v Border_right_color value
let border_bottom_color value = v Border_bottom_color value
let border_left_color value = v Border_left_color value
let border_inline_start_color value = v Border_inline_start_color value
let border_block_start_color value = v Border_block_start_color value
let border_block_end_color value = v Border_block_end_color value
let border_inline_end_color value = v Border_inline_end_color value
let border_inline_color value = v Border_inline_color value
let border_block_color value = v Border_block_color value
let border_inline_width value = v Border_inline_width value
let border_block_width value = v Border_block_width value
let border_inline_style value = v Border_inline_style value
let border_block_style value = v Border_block_style value
let border_inline_start_style value = v Border_inline_start_style value
let border_inline_end_style value = v Border_inline_end_style value
let border_block_start_style value = v Border_block_start_style value
let border_block_end_style value = v Border_block_end_style value
let border_start_start_radius value = v Border_start_start_radius [ value ]
let border_start_end_radius value = v Border_start_end_radius [ value ]
let border_end_start_radius value = v Border_end_start_radius [ value ]
let border_end_end_radius value = v Border_end_end_radius [ value ]
let webkit_font_smoothing value = v Webkit_font_smoothing value
let cursor value = v Cursor value
let user_select value = v User_select value
let webkit_user_select value = v Webkit_user_select value
let container_type value = v Container_type value

let container_name_of_string value : container_name =
  let names = String.split_on_char ' ' value |> List.filter (( <> ) "") in
  match names with [ "none" ] -> None | _ -> Names names

let container_name value = v Container_name (container_name_of_string value)

let container ?type_ name =
  v Container
    (Shorthand { name = container_name_of_string name; ctype = type_ })

let transform value = v Transform [ value ]

let transforms values =
  v Transform (require_nonempty "transforms: empty list" values)

let rotate (value : Properties_intf.rotate_value) = v Rotate value
let scale (value : Properties_intf.scale) = v Scale value
let translate (value : Properties_intf.translate_value) = v Translate value
let perspective value = v Perspective value
let perspective_origin value = v Perspective_origin value

let padding_inline values =
  v Padding_inline (require_nonempty "padding_inline: empty list" values)

let padding_inline_start value = v Padding_inline_start value
let padding_inline_end value = v Padding_inline_end value

let padding_block values =
  v Padding_block (require_nonempty "padding_block: empty list" values)

let padding_block_start value = v Padding_block_start value
let padding_block_end value = v Padding_block_end value
let margin_inline value = v Margin_inline [ value ]
let margin_inline_start value = v Margin_inline_start value
let margin_inline_end value = v Margin_inline_end value
let margin_block value = v Margin_block [ value ]
let margin_block_start value = v Margin_block_start value
let margin_block_end value = v Margin_block_end value
let will_change value = v Will_change value
let contain value = v Contain value
let isolation value = v Isolation value
let break_before value = v Break_before value
let break_after value = v Break_after value
let break_inside value = v Break_inside value
let page_break_before value = v Page_break_before value
let page_break_after value = v Page_break_after value
let page_break_inside value = v Page_break_inside value
let columns value = v Columns value
let column_rule value = v Column_rule value
let column_span value = v Column_span value
let outline value = v Outline value
let outline_offset len = v Outline_offset len
let scroll_snap_type value = v Scroll_snap_type value
let scroll_snap_align value = v Scroll_snap_align value
let scroll_snap_stop value = v Scroll_snap_stop value
let scroll_behavior value = v Scroll_behavior value
let color_scheme value = v Color_scheme value
let image_rendering value = v Image_rendering value
let image_resolution value = v Image_resolution value

(* Alignment constructor helpers (declarations) *)
