(** Property type shared between properties.ml and properties.mli.

    {2 Keyword constructors}

    A CSS keyword carries its own name as a constructor in every value type
    whose grammar admits it: [None] for [none], [Auto] for [auto], [Normal] for
    [normal]. A new value type follows the same rule, so the name stays shared
    rather than prefixed with the property it belongs to.

    The expected type selects among them. An argument to a property setter
    ([Css.display None]), an annotated binding ([let d : Css.display = None]),
    an annotated parameter and a [match] on a scrutinee that already has a type
    all resolve without warning 41. A binding with no expected type
    ([let d = None]) and a [function] whose argument type comes from its own
    branches do not; annotate the binding or the parameter there. *)

open Values
include Properties_layout_intf
include Properties_text_intf
include Properties_border_intf
include Properties_font_intf
include Properties_animation_intf
include Properties_background_intf
include Properties_ui_intf
include Properties_content_intf
include Properties_scroll_intf
include Properties_svg_intf
include Properties_other_intf

type _ kind =
  | Length : length kind
  | Color : color kind
  | Rgb : rgb kind
  | Number : number kind
  | Int : int kind
  | Float : float kind
  | Percentage : percentage kind
  | Length_percentage : length_percentage kind
  | Number_percentage : number_percentage kind
  | Opacity : opacity kind
  | Value : custom_value kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Outline_style : outline_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Font_size : font_size kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Rotate : rotate_value kind
  | Scale : scale kind
  | Shadow : shadow kind
  | Content : content kind
  | Gradient_stop : gradient_stop kind
  | Gradient_direction : gradient_direction kind
  | Gradient_position : gradient_position kind
  | Radial_shape : radial_shape kind
  | Radial_size : radial_size kind
  | Position_value : position_value kind
  | Animation : animation kind
  | Timing_function : timing_function kind
  | Transform : transform kind
  | Touch_action : touch_action kind
  | Transition_property_value : transition_property_value kind
  | Background_image : background_image kind
  | Z_index : z_index kind
  | Filter : filter kind
  | Font_src : Font_face.src kind

type custom_property =
  | Custom_value : {
      value : custom_property_value;
      layer : string option;
      meta : meta option;
    }
      -> custom_property

and custom_property_value =
  | Typed : { kind : 'a kind; value : 'a } -> custom_property_value
  | Tokens of custom_value

(** [all] shorthand value (CSS Cascade 5 sec. 3.2). The [all] property only
    accepts CSS-wide keywords - no other syntax is valid. *)
type css_wide =
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of css_wide var

(* Property type definition *)
type 'a property =
  | Custom_property : string -> custom_property property
  | Unknown_property : string -> custom_value property
  | All : css_wide property
  | Background_color : color property
  | Color : color property
  | Border_color : color list property
  | Border_style : border_style list property
  | Border_top_style : border_style property
  | Border_right_style : border_style property
  | Border_bottom_style : border_style property
  | Border_left_style : border_style property
  | Border_inline_start_style : border_style property
  | Border_inline_end_style : border_style property
  | Border_block_start_style : border_style property
  | Border_block_end_style : border_style property
  | Padding : length list property
  | Padding_left : length property
  | Padding_right : length property
  | Padding_bottom : length property
  | Padding_top : length property
  | Padding_inline : length list property
  | Padding_inline_start : length property
  | Padding_inline_end : length property
  | Padding_block : length list property
  | Padding_block_start : length property
  | Padding_block_end : length property
  | Margin : length list property
  | Margin_inline_end : length property
  | Margin_inline_start : length property
  | Margin_left : length property
  | Margin_right : length property
  | Margin_top : length property
  | Margin_bottom : length property
  | Margin_inline : length list property
  | Margin_block : length list property
  | Margin_block_start : length property
  | Margin_block_end : length property
  | Gap : gap property
  | Column_gap : length property
  | Row_gap : length property
  | Width : length_percentage property
  | Height : length_percentage property
  | Min_width : length_percentage property
  | Min_height : length_percentage property
  | Max_width : length_percentage property
  | Max_height : length_percentage property
  | Inline_size : length_percentage property
  | Min_inline_size : length_percentage property
  | Max_inline_size : length_percentage property
  | Block_size : length_percentage property
  | Min_block_size : length_percentage property
  | Max_block_size : length_percentage property
  | Font_size : font_size property
  | Line_height : line_height property
  | Font_weight : font_weight property
  | Font_style : font_style property
  | Text_align : text_align property
  | Text_decoration : text_decoration property
  | Text_decoration_line : text_decoration_line list property
  | Text_decoration_style : text_decoration_style property
  | Text_decoration_color : color property
  | Text_underline_offset : length property
  | Text_decoration_skip : text_decoration_skip property
  | Text_decoration_skip_self : text_decoration_skip_self property
  | Text_decoration_skip_box : text_decoration_skip_box property
  | Text_decoration_skip_inset : text_decoration_skip_inset property
  | Text_decoration_skip_spaces : text_decoration_skip_spaces property
  | Text_emphasis : text_emphasis property
  | Text_emphasis_style : text_emphasis_style property
  | Text_emphasis_color : color property
  | Text_emphasis_position : text_emphasis_position property
  | Text_emphasis_skip : text_emphasis_skip property
  | Text_orientation : text_orientation property
  | Text_transform : text_transform property
  | Letter_spacing : length property
  | List_style_type : list_style_type property
  | List_style_position : list_style_position property
  | List_style_image : list_style_image property
  | Display : display property
  | Position : position property
  | Visibility : visibility property
  | Baseline_source : baseline_source property
  | Alignment_baseline : alignment_baseline property
  | Baseline_shift : baseline_shift property
  | Flex_direction : flex_direction property
  | Flex_wrap : flex_wrap property
  | Flex_flow : flex_flow property
  | Flex : flex property
  | Flex_grow : flex_factor property
  | Flex_shrink : flex_factor property
  | Flex_basis : flex_basis property
  | Order : order property
  | Align_items : align_items property
  | Justify_content : justify_content property
  | Justify_items : justify_items property
  | Justify_self : justify_self property
  | Align_content : align_content property
  | Align_self : align_self property
  | Place_content : place_content property
  | Place_items : place_items property
  | Place_self : (align_self * justify_self) property
  | Grid_template_columns : grid_template property
  | Grid_template_rows : grid_template property
  | Grid_template_areas : grid_template_areas property
  | Grid_template : grid_template property
  | Grid : grid_template property
      (** CSS Grid 1 sec. 7.8 [grid] shorthand. Cascade treats it as a free-form
          [grid_template] for now: the simple cases (track-list, grid-template
          syntax with area strings) round-trip through the same AST as
          [grid-template], and inputs that exercise the auto-flow branches fall
          back to the raw [Template] preservation arm. *)
  | Grid_area : grid_area property
  | Grid_auto_flow : grid_auto_flow property
  | Grid_auto_columns : grid_template property
  | Grid_auto_rows : grid_template property
  | Grid_column : grid_line_pair property
  | Grid_row : grid_line_pair property
  | Grid_column_start : grid_line property
  | Grid_column_end : grid_line property
  | Grid_row_start : grid_line property
  | Grid_row_end : grid_line property
  | Border_width : border_width list property
  | Border_top_width : border_width property
  | Border_right_width : border_width property
  | Border_bottom_width : border_width property
  | Border_left_width : border_width property
  | Border_inline_start_width : border_width property
  | Border_inline_end_width : border_width property
  | Border_block_start_width : border_width property
  | Border_block_end_width : border_width property
  | Border_inline_width : logical_border_width property
  | Border_block_width : logical_border_width property
  | Border_image : border_image property
  | Border_image_source : background_image property
  | Border_image_slice : border_image_slice property
  | Border_image_repeat : border_image_repeat property
  | Border_image_width : border_image_width property
  | Border_image_outset : border_image_outset property
  | Border_radius : border_radius property
  | Border_top_left_radius : length list property
  | Border_top_right_radius : length list property
  | Border_bottom_left_radius : length list property
  | Border_bottom_right_radius : length list property
  | Border_top_color : color property
  | Border_right_color : color property
  | Border_bottom_color : color property
  | Border_left_color : color property
  | Border_inline_start_color : color property
  | Border_inline_end_color : color property
  | Border_block_start_color : color property
  | Border_block_end_color : color property
  | Border_inline_color : logical_border_color property
  | Border_block_color : logical_border_color property
  | Border_inline_style : logical_border_style property
  | Border_block_style : logical_border_style property
  | Border_start_start_radius : length list property
  | Border_start_end_radius : length list property
  | Border_end_start_radius : length list property
  | Border_end_end_radius : length list property
  | Opacity : opacity property
  | Fill_opacity : opacity property
  | Stroke_opacity : opacity property
  | Stop_opacity : opacity property
  | Flood_opacity : opacity property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Translate : translate_value property
  | Cursor : cursor property
  | Interactivity : interactivity property
  | Caret_animation : caret_animation property
  | Caret_shape : caret_shape property
  | Caret : caret property
  | Interest_delay : interest_delay property
  | Interest_delay_start : interest_delay property
  | Interest_delay_end : interest_delay property
  | Nav_up : nav property
  | Nav_right : nav property
  | Nav_down : nav property
  | Nav_left : nav property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : border_spacing property
  | User_select : user_select property
  | Pointer_events : pointer_events property
  | Overflow : overflow property
  | Inset : length list property
  | Inset_inline : length list property
  | Inset_inline_start : length list property
  | Inset_inline_end : length list property
  | Inset_block : length list property
  | Inset_block_start : length list property
  | Inset_block_end : length list property
  | Top : length list property
  | Right : length list property
  | Bottom : length list property
  | Left : length list property
  | Z_index : z_index property
  | Outline : outline property
  | Outline_style : outline_style property
  | Outline_width : border_width property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | White_space_collapse : white_space_collapse property
  | Border : border property
  | Border_block : border property
  | Border_block_start : border property
  | Border_block_end : border property
  | Border_inline : border property
  | Border_inline_start : border property
  | Border_inline_end : border property
  | Background : background list property
  | Tab_size : tab_size property
  | Zoom : zoom property
  | Webkit_text_size_adjust : text_size_adjust property
  | Font_feature_settings : font_feature_settings property
  | Font_variation_settings : font_variation_settings property
  | Webkit_tap_highlight_color : color property
  | Webkit_user_select : user_select property
  | Moz_user_select : user_select property
  | Ms_user_select : user_select property
  | Webkit_text_decoration : text_decoration property
  | Webkit_text_decoration_color : color property
  | Webkit_text_fill_color : color property
  | Webkit_text_stroke : webkit_text_stroke property
  | Webkit_text_stroke_width : border_width property
  | Webkit_text_stroke_color : color property
  | Text_indent : text_indent_value property
  | List_style : list_style property
  | Font : font property
  | Source : Font_face.src property
  | Webkit_appearance : webkit_appearance property
  | Webkit_transform : transform list property
  | Moz_transform : transform list property
  | Ms_transform : transform list property
  | O_transform : transform list property
  | Webkit_transition : transition list property
  | Webkit_transition_delay : duration property
  | Webkit_transition_duration : duration property
  | Webkit_transition_property : transition_property property
  | Webkit_transition_timing_function : timing_function property
  | Webkit_animation : animation list property
  | Webkit_animation_delay : duration property
  | Webkit_animation_duration : duration property
  | Webkit_animation_direction : animation_direction property
  | Webkit_animation_iteration_count : animation_iteration_count property
  | Webkit_animation_name : animation_name property
  | Webkit_animation_timing_function : timing_function property
  | Webkit_animation_fill_mode : animation_fill_mode property
  | Webkit_animation_play_state : animation_play_state property
  | Webkit_flex_direction : flex_direction property
  | Webkit_flex_wrap : flex_wrap property
  | Webkit_flex_flow : flex_flow property
  | Webkit_justify_content : justify_content property
  | Webkit_align_items : align_items property
  | Webkit_align_content : align_content property
  | Webkit_align_self : align_self property
  | Webkit_border_radius : border_radius property
  | Webkit_box_sizing : box_sizing property
  | Moz_box_sizing : box_sizing property
  | Webkit_box_shadow : shadow property
  | Webkit_background_size : background_size property
  | Webkit_filter : filter property
  | Moz_appearance : appearance property
  | Moz_animation : animation list property
  | Moz_animation_delay : duration property
  | Moz_animation_duration : duration property
  | Moz_animation_direction : animation_direction property
  | Moz_animation_iteration_count : animation_iteration_count property
  | Moz_animation_name : animation_name property
  | Moz_animation_timing_function : timing_function property
  | Moz_animation_fill_mode : animation_fill_mode property
  | Moz_animation_play_state : animation_play_state property
  | Moz_transition : transition list property
  | Moz_transition_delay : duration property
  | Moz_transition_duration : duration property
  | Moz_transition_property : transition_property property
  | Moz_transition_timing_function : timing_function property
  | Moz_border_radius : border_radius property
  | Moz_box_shadow : shadow property
  | Ms_filter : filter property
  | O_transition : transition list property
  | Container_type : container_type property
  | Container_name : container_name property
  | Container : container_shorthand property
  | Anchor_name : anchor_name property
  | Position_anchor : position_anchor property
  | Position_try_fallbacks : position_try_fallbacks property
  | Position_try_order : position_try_order property
  | Position_try : position_try property
  | Position_visibility : position_visibility property
  | Position_area : position_area property
  | Shape_outside : string property
  | Shape_margin : length_percentage property
  | Shape_image_threshold : shape_image_threshold property
  | Overflow_clip_margin : overflow_clip_margin property
  | Overflow_anchor : overflow_anchor property
  | Scrollbar_width : scrollbar_width property
  | Scrollbar_color : scrollbar_color property
  | Scrollbar_gutter : scrollbar_gutter property
  | Line_height_step : length property
  | Font_palette : font_palette property
  | Font_synthesis : font_synthesis property
  | Text_wrap_mode : text_wrap_mode property
  | Text_wrap_style : text_wrap_style property
  | Text_box_trim : text_box_trim property
  | Text_underline_position : text_underline_position property
  | Text_box_edge : text_box_edge property
  | Text_box : text_box property
  | Inline_sizing : inline_sizing property
  | Line_fit_edge : line_fit_edge property
  | Interpolate_size : interpolate_size property
  | Min_intrinsic_sizing : min_intrinsic_sizing property
  | Ruby_align : ruby_align property
  | Ruby_merge : ruby_merge property
  | Ruby_overhang : ruby_overhang property
  | Ruby_position : ruby_position property
  | Glyph_orientation_vertical : glyph_orientation_vertical property
  | Text_combine_upright : text_combine_upright property
  | Animation_timeline : animation_timeline property
  | Animation_range : animation_range property
  | Animation_range_start : animation_range_item property
  | Animation_range_end : animation_range_item property
  | Scroll_timeline : timeline_shorthand property
  | Scroll_timeline_name : timeline_name property
  | Scroll_timeline_axis : timeline_axis property
  | View_transition_name : view_transition_name property
  | View_transition_class : view_transition_class property
  | Image_orientation : image_orientation property
  | Image_rendering : image_rendering property
  | Image_resolution : image_resolution property
  | Contain_intrinsic_size : contain_intrinsic_size property
  | Contain_intrinsic_width : contain_intrinsic_longhand property
  | Contain_intrinsic_height : contain_intrinsic_longhand property
  | Contain_intrinsic_block_size : contain_intrinsic_longhand property
  | Contain_intrinsic_inline_size : contain_intrinsic_longhand property
  | Margin_trim : margin_trim property
  | Offset_path : offset_path property
  | Offset_distance : length_percentage property
  | Offset_rotate : offset_rotate property
  | Font_size_adjust : font_size_adjust property
  | Font_variant_emoji : font_variant_emoji property
  | Text_spacing_trim : text_spacing_trim property
  | Hyphenate_limit_chars : hyphenate_limit_chars property
  | Initial_letter : initial_letter property
  | Initial_letter_align : initial_letter_align property
  | Initial_letter_wrap : initial_letter_wrap property
  | Dominant_baseline : dominant_baseline property
  | View_timeline_name : timeline_name property
  | View_timeline_axis : timeline_axis property
  | View_timeline_inset : timeline_inset property
  | View_timeline : view_timeline_shorthand property
  | Timeline_scope : timeline_scope property
  | Perspective : length property
  | Perspective_origin : perspective_origin property
  | Transform_style : transform_style property
  | Backface_visibility : backface_visibility property
  | Object_position : position_value property
  | Rotate : rotate_value property
  | Transition_duration : duration property
  | Transition_timing_function : timing_function property
  | Transition_delay : duration property
  | Transition_property : transition_property property
  | Transition_behavior : transition_behavior property
  | Overlay : overlay property
  | Will_change : will_change property
  | Contain : contain property
  | Isolation : isolation property
  | Break_before : break_value property
  | Break_after : break_value property
  | Break_inside : break_inside_value property
  | Page_break_before : page_break_value property
  | Page_break_after : page_break_value property
  | Page_break_inside : page_break_inside_value property
  | Page_size : page_size property
  | Columns : columns_value property
  | Column_width : column_width property
  | Column_height : column_height property
  | Column_wrap : column_wrap property
  | Column_count : column_count property
  | Column_rule : border list property
      (** CSS Gaps 1 sec. 4 gives the three longhands below a comma-separated
          list, one entry per gap decoration line. *)
  | Column_rule_width : border_width list property
  | Column_rule_style : border_style list property
  | Column_rule_color : color list property
  | Column_span : column_span property
  | Word_spacing : length property
  | Background_attachment : background_attachment property
  | Border_top : border property
  | Border_right : border property
  | Border_bottom : border property
  | Border_left : border property
  | Transform_origin : transform_origin property
  | Transform_box : transform_box property
  | Text_shadow : text_shadow list property
  | Clip_path : clip_path property
  | Mask : mask property
  | Mask_border : border_image property
  | Content_visibility : content_visibility property
  | Filter : filter property
  | Background_image : background_image list property
  | Background_origin : background_box property
  | Background_clip : background_box property
  | Webkit_background_clip : background_box property
  | Animation : animation list property
  | Aspect_ratio : aspect_ratio property
  | Overflow_x : overflow property
  | Overflow_y : overflow property
  | Overflow_block : overflow property
  | Overflow_inline : overflow property
  | Vertical_align : vertical_align property
  | Font_family : font_family property
  | Background_position : background_position property
  | Background_position_x : background_position_axis property
  | Background_position_y : background_position_axis property
  | Webkit_mask_position_x : background_position_axis property
  | Webkit_mask_position_y : background_position_axis property
  | Background_repeat : background_repeat property
  | Background_size : background_size property
  | Webkit_font_smoothing : webkit_font_smoothing property
  | Moz_osx_font_smoothing : moz_osx_font_smoothing property
  | Webkit_line_clamp : webkit_line_clamp property
  | Webkit_box_orient : webkit_box_orient property
  | Moz_orient : moz_orient property
  | Text_overflow : text_overflow property
  | Text_wrap : text_wrap property
  | Word_break : word_break property
  | Overflow_wrap : overflow_wrap property
  | Line_break : line_break property
  | Hyphens : hyphens property
  | Webkit_hyphens : hyphens property
  | Font_stretch : font_stretch property
  | Font_optical_sizing : font_optical_sizing property
  | Font_kerning : font_kerning property
  | Font_language_override : font_language_override property
  | Font_synthesis_style : font_synthesis_style property
  | Font_synthesis_weight : font_synthesis_weight property
  | Font_synthesis_small_caps : font_synthesis_small_caps property
  | Font_synthesis_position : font_synthesis_position property
  | Font_variant_ligatures : font_variant_ligatures property
  | Caps : font_variant_caps property
  | Numeric : font_variant_numeric property
  | Font_variant_position : font_variant_position property
  | Font_variant_alternates : font_variant_alternates property
  | Font_variant : font_variant property
  | East_asian : font_variant_east_asian property
  | Backdrop_filter : filter property
  | Webkit_backdrop_filter : filter property
  | Webkit_mask_image : background_image property
  | Webkit_mask_composite : webkit_mask_composite property
  | Webkit_mask_source_type : webkit_mask_source_type property
  | Webkit_mask_size : background_size property
  | Webkit_mask_position : background_position property
  | Webkit_mask_repeat : background_repeat property
  | Webkit_mask_clip : webkit_mask_box property
  | Webkit_mask_origin : webkit_mask_box property
  | Mask_image : background_image property
  | Mask_composite : mask_composite property
  | Mask_mode : mask_mode property
  | Mask_size : background_size property
  | Mask_position : background_position property
  | Mask_repeat : background_repeat property
  | Mask_clip : mask_box property
  | Mask_origin : mask_box property
  | Mask_type : mask_type property
  | Scroll_snap_align : scroll_snap_align property
  | Scroll_snap_stop : scroll_snap_stop property
  | Scroll_behavior : scroll_behavior property
  | Box_sizing : box_sizing property
  | Field_sizing : field_sizing property
  | Caption_side : caption_side property
  | Resize : resize property
  | Object_fit : object_fit property
  | Object_view_box : object_view_box property
  | Appearance : appearance property
  | Color_scheme : color_scheme property
  | Print_color_adjust : print_color_adjust property
  | Webkit_print_color_adjust : print_color_adjust property
  | Box_decoration_break : box_decoration_break property
  | Webkit_box_decoration_break : box_decoration_break property
  | Content : content property
  | Counter_reset : counter_set property
  | Counter_increment : counter_set property
  | Quotes : quotes property
  | Text_decoration_thickness : length property
  | Text_size_adjust : text_size_adjust property
  | Touch_action : touch_action property
  | Clip : clip property
  | Clear : clear property
  | Float : float_side property
  | Scale : scale property
  | Transition : transition list property
  | Box_shadow : shadow property
  | Fill : svg_paint property
  | Stroke : svg_paint property
  | Stroke_width : stroke_width property
  | Fill_rule : fill_rule property
  | Clip_rule : fill_rule property
  | Stroke_linecap : stroke_linecap property
  | Stroke_linejoin : stroke_linejoin property
  | Stroke_miterlimit : stroke_miterlimit property
  | Stroke_dashoffset : stroke_dashoffset property
  | Stroke_dasharray : stroke_dasharray property
  | Paint_order : paint_order property
  | Vector_effect : vector_effect property
  | Stop_color : color property
  | Flood_color : color property
  | Lighting_color : color property
  | Direction : direction property
  | Unicode_bidi : unicode_bidi property
  | Writing_mode : writing_mode property
  | Text_decoration_skip_ink : text_decoration_skip_ink property
  | Animation_name : animation_name property
  | Animation_duration : duration property
  | Animation_timing_function : timing_function property
  | Animation_delay : duration property
  | Animation_iteration_count : animation_iteration_count property
  | Animation_direction : animation_direction property
  | Animation_fill_mode : animation_fill_mode property
  | Animation_play_state : animation_play_state property
  | Animation_composition : animation_composition property
  | Background_blend_mode : blend_mode list property
  | Scroll_margin : length list property
  | Scroll_margin_top : length property
  | Scroll_margin_right : length property
  | Scroll_margin_bottom : length property
  | Scroll_margin_left : length property
  | Scroll_margin_inline : length list property
  | Scroll_margin_inline_start : length property
  | Scroll_margin_inline_end : length property
  | Scroll_margin_block : length list property
  | Scroll_margin_block_start : length property
  | Scroll_margin_block_end : length property
  | Scroll_padding : length list property
  | Scroll_padding_top : length property
  | Scroll_padding_right : length property
  | Scroll_padding_bottom : length property
  | Scroll_padding_left : length property
  | Scroll_padding_inline : length list property
  | Scroll_padding_inline_start : length property
  | Scroll_padding_inline_end : length property
  | Scroll_padding_block : length list property
  | Scroll_padding_block_start : length property
  | Scroll_padding_block_end : length property
  | Overscroll_behavior : overscroll_behavior list property
  | Overscroll_behavior_x : overscroll_behavior property
  | Overscroll_behavior_y : overscroll_behavior property
  | Overscroll_behavior_block : overscroll_behavior property
  | Overscroll_behavior_inline : overscroll_behavior property
  | Accent_color : color property
  | Caret_color : color property
  | Offset_anchor : offset_anchor property
  | Offset_position : offset_position property
  | Offset : offset property

type any_property = Prop : 'a property -> any_property

type _ property_value_kind =
  | Length : length property_value_kind
  | Lengths : length list property_value_kind
  | Length_percentage : length_percentage property_value_kind
  | Border_width : border_width property_value_kind
  | Border_widths : border_width list property_value_kind
  | Opacity : opacity property_value_kind
  | Rotate : rotate_value property_value_kind
  | Duration : duration property_value_kind
  | Number_percentage : number_percentage property_value_kind
  | Font_size : font_size property_value_kind
  | Display : display property_value_kind
  | Position : position property_value_kind
  | Visibility : visibility property_value_kind
  | Clear : clear property_value_kind
  | Float : float_side property_value_kind
  | Scale : scale property_value_kind
  | Translate : translate_value property_value_kind
  | Transform : transform list property_value_kind
  | Animation : animation list property_value_kind
  | Transition : transition list property_value_kind
  | Filter : filter property_value_kind
  | Shadow : shadow property_value_kind
  | Border_radius : border_radius property_value_kind
  | Color : color property_value_kind
  | Colors : color list property_value_kind
  | Animation_name : animation_name property_value_kind
  | Background : background list property_value_kind
  | Background_image : background_image property_value_kind
  | Background_images : background_image list property_value_kind
  | Font_src : Font_face.src property_value_kind
  | Font_family : font_family property_value_kind
  | Stroke_width : stroke_width property_value_kind

let equal_overflow (a : overflow) b = a = b
let equal_grid_line (a : grid_line) b = a = b
let equal_text_emphasis_skip_keyword (a : text_emphasis_skip_keyword) b = a = b

let equal_min_intrinsic_sizing_keyword (a : min_intrinsic_sizing_keyword) b =
  a = b

let equal_initial_letter_align_keyword (a : initial_letter_align_keyword) b =
  a = b

let equal_ruby_position_keyword (a : ruby_position_keyword) b = a = b
let equal_background_shorthand (a : background_shorthand) b = a = b
let equal_background_box (a : background_box) b = a = b
let equal_mask_layer (a : mask_layer) b = a = b
let equal_position_area_keyword (a : position_area_keyword) b = a = b

let equal_position_visibility_condition (a : position_visibility_condition) b =
  a = b

let equal_animation_range_name (a : animation_range_name) b = a = b
let equal_container_shorthand (a : container_shorthand) b = a = b
let equal_paint_order_keyword (a : paint_order_keyword) b = a = b
let equal_paint_order (a : paint_order) b = a = b
let equal_border_width (a : border_width) b = a = b
let equal_overscroll_behavior (a : overscroll_behavior) b = a = b

let equal_contain_intrinsic_size_item (a : contain_intrinsic_size_item) b =
  a = b

let equal_border_style (a : border_style) b = a = b
