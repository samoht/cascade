(** CSS properties: types and helpers. *)

open Values
include module type of Properties_intf

val pp_property : 'a property Pp.t
(** [pp_property] is the pretty-printer for property names. *)

val read_property : Reader.t -> any_property
(** [read_property t] reads one property and returns an existential wrapper. *)

val pp_property_value : ('a property * 'a) Pp.t
(** [pp_property_value] is the pretty-printer for a property and its typed
    value. *)

val pp_shadow : shadow Pp.t
(** [pp_shadow] is the pretty-printer for [shadow] values. *)

val read_shadow : Reader.t -> shadow
(** [read_shadow t] parses a [shadow] value from [t]. *)

(* Background and animation helpers moved from Css *)

val url : string -> background_image
(** [url path] builds a [background_image] URL value. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] builds a linear-gradient background image. *)

val radial_gradient : gradient_stop list -> background_image
(** [radial_gradient stops] builds a radial-gradient background image. *)

val color_stop : Values.color -> gradient_stop
(** [color_stop c] is a gradient stop with just a color. *)

val color_position : Values.color -> Values.length -> gradient_stop
(** [color_position c pos] is a gradient stop with color and position. *)

val animation_spec :
  ?name:string ->
  ?duration:Values.duration ->
  ?timing_function:timing_function ->
  ?delay:Values.duration ->
  ?iteration_count:animation_iteration_count ->
  ?direction:animation_direction ->
  ?fill_mode:animation_fill_mode ->
  ?play_state:animation_play_state ->
  unit ->
  animation
(** [animation_spec ?name ?duration ?timing_function ?delay ?iteration_count
     ?direction ?fill_mode ?play_state ()] constructs an [animation] record. *)

(** Pretty-printers and readers for property value types. *)

val pp_border_style : border_style Pp.t
(** [pp_border_style] is the pretty-printer for [border_style]. *)

val read_border_style : Reader.t -> border_style
(** [read_border_style t] is the [border_style] parsed from [t]. *)

val pp_border_width : border_width Pp.t
(** [pp_border_width] is the pretty-printer for [border_width]. *)

val read_border_width : Reader.t -> border_width
(** [read_border_width t] is the [border_width] parsed from [t]. *)

val pp_border : border Pp.t
(** [pp_border] is the pretty-printer for [border]. *)

val read_border : Reader.t -> border
(** [read_border t] is the [border] shorthand parsed from [t]. *)

val pp_line_height : line_height Pp.t
(** [pp_line_height] is the pretty-printer for [line_height]. *)

val read_line_height : Reader.t -> line_height
(** [read_line_height t] is the [line_height] parsed from [t]. *)

val pp_font_weight : font_weight Pp.t
(** [pp_font_weight] is the pretty-printer for [font_weight]. *)

val read_font_weight : Reader.t -> font_weight
(** [read_font_weight t] is the [font_weight] parsed from [t]. *)

val pp_display : display Pp.t
(** [pp_display] is the pretty-printer for [display]. *)

val read_display : Reader.t -> display
(** [read_display t] is the [display] parsed from [t]. *)

val pp_position : position Pp.t
(** [pp_position] is the pretty-printer for [position]. *)

val read_position : Reader.t -> position
(** [read_position t] is the [position] parsed from [t]. *)

val pp_visibility : visibility Pp.t
(** [pp_visibility] is the pretty-printer for [visibility]. *)

val read_visibility : Reader.t -> visibility
(** [read_visibility t] is the [visibility] parsed from [t]. *)

val pp_z_index : z_index Pp.t
(** [pp_z_index] is the pretty-printer for [z_index]. *)

val read_z_index : Reader.t -> z_index
(** [read_z_index t] is the [z_index] parsed from [t]. *)

val pp_overflow : overflow Pp.t
(** [pp_overflow] is the pretty-printer for [overflow]. *)

val read_overflow : Reader.t -> overflow
(** [read_overflow t] is the [overflow] parsed from [t]. *)

val pp_flex_direction : flex_direction Pp.t
(** [pp_flex_direction] is the pretty-printer for [flex_direction]. *)

val read_flex_direction : Reader.t -> flex_direction
(** [read_flex_direction t] is the [flex_direction] parsed from [t]. *)

val pp_flex_wrap : flex_wrap Pp.t
(** [pp_flex_wrap] is the pretty-printer for [flex_wrap]. *)

val read_flex_wrap : Reader.t -> flex_wrap
(** [read_flex_wrap t] is the [flex_wrap] parsed from [t]. *)

val pp_align : align Pp.t
(** [pp_align] is the pretty-printer for [align]. *)

val read_align : Reader.t -> align
(** [read_align t] is the [align] parsed from [t]. *)

val pp_align_items : align_items Pp.t
(** [pp_align_items] is the pretty-printer for [align_items]. *)

val read_align_items : Reader.t -> align_items
(** [read_align_items t] is the [align_items] parsed from [t]. *)

val pp_align_self : align_self Pp.t
(** [pp_align_self] is the pretty-printer for [align_self]. *)

val read_align_self : Reader.t -> align_self
(** [read_align_self t] is the [align_self] parsed from [t]. *)

val pp_justify_content : justify_content Pp.t
(** [pp_justify_content] is the pretty-printer for [justify_content]. *)

val read_justify_content : Reader.t -> justify_content
(** [read_justify_content t] is the [justify_content] parsed from [t]. *)

val pp_justify : justify Pp.t
(** [pp_justify] is the pretty-printer for [justify]. *)

val read_justify : Reader.t -> justify
(** [read_justify t] is the [justify] parsed from [t]. *)

val pp_flex : flex Pp.t
(** [pp_flex] is the pretty-printer for [flex]. *)

val read_flex : Reader.t -> flex
(** [read_flex t] is the [flex] parsed from [t]. *)

val pp_place_content : place_content Pp.t
(** [pp_place_content] is the pretty-printer for [place_content]. *)

val read_place_content : Reader.t -> place_content
(** [read_place_content t] is the [place_content] parsed from [t]. *)

val pp_place_items : place_items Pp.t
(** [pp_place_items] is the pretty-printer for [place_items]. *)

val read_place_items : Reader.t -> place_items
(** [read_place_items t] is the [place_items] parsed from [t]. *)

val pp_grid_auto_flow : grid_auto_flow Pp.t
(** [pp_grid_auto_flow] is the pretty-printer for [grid_auto_flow]. *)

val read_grid_auto_flow : Reader.t -> grid_auto_flow
(** [read_grid_auto_flow t] is the [grid_auto_flow] parsed from [t]. *)

val pp_grid_track_size : grid_track_size Pp.t
(** [pp_grid_track_size] is the pretty-printer for [grid_track_size]. *)

val read_grid_track_size : Reader.t -> grid_track_size
(** [read_grid_track_size t] is the [grid_track_size] parsed from [t]. *)

val pp_grid_template : grid_template Pp.t
(** [pp_grid_template] is the pretty-printer for [grid_template]. *)

val read_grid_template : Reader.t -> grid_template
(** [read_grid_template t] is the [grid_template] parsed from [t]. *)

val pp_grid_line : grid_line Pp.t
(** [pp_grid_line] is the pretty-printer for [grid_line]. *)

val read_grid_line : Reader.t -> grid_line
(** [read_grid_line t] is the [grid_line] parsed from [t]. *)

val pp_aspect_ratio : aspect_ratio Pp.t
(** [pp_aspect_ratio] is the pretty-printer for [aspect_ratio]. *)

val read_aspect_ratio : Reader.t -> aspect_ratio
(** [read_aspect_ratio t] is the [aspect_ratio] parsed from [t]. *)

val pp_font_style : font_style Pp.t
(** [pp_font_style] is the pretty-printer for [font_style]. *)

val read_font_style : Reader.t -> font_style
(** [read_font_style t] is the [font_style] parsed from [t]. *)

val pp_text_align : text_align Pp.t
(** [pp_text_align] is the pretty-printer for [text_align]. *)

val read_text_align : Reader.t -> text_align
(** [read_text_align t] is the [text_align] parsed from [t]. *)

val pp_text_decoration : text_decoration Pp.t
(** [pp_text_decoration] is the pretty-printer for [text_decoration]. *)

val read_text_decoration : Reader.t -> text_decoration
(** [read_text_decoration t] is the [text_decoration] parsed from [t]. *)

val pp_text_decoration_style : text_decoration_style Pp.t
(** [pp_text_decoration_style] is the pretty-printer for
    [text_decoration_style]. *)

val read_text_decoration_style : Reader.t -> text_decoration_style
(** [read_text_decoration_style t] is the [text_decoration_style] parsed from
    [t]. *)

val pp_text_transform : text_transform Pp.t
(** [pp_text_transform] is the pretty-printer for [text_transform]. *)

val read_text_transform : Reader.t -> text_transform
(** [read_text_transform t] is the [text_transform] parsed from [t]. *)

val pp_text_overflow : text_overflow Pp.t
(** [pp_text_overflow] is the pretty-printer for [text_overflow]. *)

val read_text_overflow : Reader.t -> text_overflow
(** [read_text_overflow t] is the [text_overflow] parsed from [t]. *)

val pp_text_wrap : text_wrap Pp.t
(** [pp_text_wrap] is the pretty-printer for [text_wrap]. *)

val read_text_wrap : Reader.t -> text_wrap
(** [read_text_wrap t] is the [text_wrap] parsed from [t]. *)

val pp_white_space : white_space Pp.t
(** [pp_white_space] is the pretty-printer for [white_space]. *)

val read_white_space : Reader.t -> white_space
(** [read_white_space t] is the [white_space] parsed from [t]. *)

val pp_word_break : word_break Pp.t
(** [pp_word_break] is the pretty-printer for [word_break]. *)

val read_word_break : Reader.t -> word_break
(** [read_word_break t] is the [word_break] parsed from [t]. *)

val pp_overflow_wrap : overflow_wrap Pp.t
(** [pp_overflow_wrap] is the pretty-printer for [overflow_wrap]. *)

val read_overflow_wrap : Reader.t -> overflow_wrap
(** [read_overflow_wrap t] is the [overflow_wrap] parsed from [t]. *)

val pp_hyphens : hyphens Pp.t
(** [pp_hyphens] is the pretty-printer for [hyphens]. *)

val read_hyphens : Reader.t -> hyphens
(** [read_hyphens t] is the [hyphens] parsed from [t]. *)

val pp_list_style_type : list_style_type Pp.t
(** [pp_list_style_type] is the pretty-printer for [list_style_type]. *)

val read_list_style_type : Reader.t -> list_style_type
(** [read_list_style_type t] is the [list_style_type] parsed from [t]. *)

val pp_list_style_position : list_style_position Pp.t
(** [pp_list_style_position] is the pretty-printer for [list_style_position]. *)

val read_list_style_position : Reader.t -> list_style_position
(** [read_list_style_position t] is the [list_style_position] parsed from [t].
*)

val pp_list_style_image : list_style_image Pp.t
(** [pp_list_style_image] is the pretty-printer for [list_style_image]. *)

val read_list_style_image : Reader.t -> list_style_image
(** [read_list_style_image t] is the [list_style_image] parsed from [t]. *)

val pp_table_layout : table_layout Pp.t
(** [pp_table_layout] is the pretty-printer for [table_layout]. *)

val read_table_layout : Reader.t -> table_layout
(** [read_table_layout t] is the [table_layout] parsed from [t]. *)

val pp_vertical_align : vertical_align Pp.t
(** [pp_vertical_align] is the pretty-printer for [vertical_align]. *)

val read_vertical_align : Reader.t -> vertical_align
(** [read_vertical_align t] is the [vertical_align] parsed from [t]. *)

val pp_border_collapse : border_collapse Pp.t
(** [pp_border_collapse] is the pretty-printer for [border_collapse]. *)

val read_border_collapse : Reader.t -> border_collapse
(** [read_border_collapse t] is the [border_collapse] parsed from [t]. *)

val pp_outline_style : outline_style Pp.t
(** [pp_outline_style] is the pretty-printer for [outline_style]. *)

val read_outline_style : Reader.t -> outline_style
(** [read_outline_style t] is the [outline_style] parsed from [t]. *)

val pp_font_family : font_family Pp.t
(** [pp_font_family] is the pretty-printer for [font_family]. *)

val read_font_family : Reader.t -> font_family
(** [read_font_family t] is the [font_family] parsed from [t]. *)

val pp_font_stretch : font_stretch Pp.t
(** [pp_font_stretch] is the pretty-printer for [font_stretch]. *)

val read_font_stretch : Reader.t -> font_stretch
(** [read_font_stretch t] is the [font_stretch] parsed from [t]. *)

val pp_font_variant_numeric_token : font_variant_numeric_token Pp.t
(** [pp_font_variant_numeric_token] is the pretty-printer for
    [font_variant_numeric_token]. *)

val read_font_variant_numeric_token : Reader.t -> font_variant_numeric_token
(** [read_font_variant_numeric_token t] is the [font_variant_numeric_token]
    parsed from [t]. *)

val pp_font_variant_numeric : font_variant_numeric Pp.t
(** [pp_font_variant_numeric] is the pretty-printer for [font_variant_numeric].
*)

val read_font_variant_numeric : Reader.t -> font_variant_numeric
(** [read_font_variant_numeric t] is the [font_variant_numeric] parsed from [t].
*)

val pp_font_feature_settings : font_feature_settings Pp.t
(** [pp_font_feature_settings] is the pretty-printer for
    [font_feature_settings]. *)

val read_font_feature_settings : Reader.t -> font_feature_settings
(** [read_font_feature_settings t] is the [font_feature_settings] parsed from
    [t]. *)

val pp_font_variation_settings : font_variation_settings Pp.t
(** [pp_font_variation_settings] is the pretty-printer for
    [font_variation_settings]. *)

val read_font_variation_settings : Reader.t -> font_variation_settings
(** [read_font_variation_settings t] is the [font_variation_settings] parsed
    from [t]. *)

val pp_transform : transform Pp.t
(** [pp_transform] is the pretty-printer for [transform]. *)

val read_transform : Reader.t -> transform
(** [read_transform t] is the [transform] parsed from [t]. *)

val read_transform_origin : Reader.t -> transform_origin
(** [read_transform_origin t] is the [transform_origin] parsed from [t]. *)

val pp_transform_style : transform_style Pp.t
(** [pp_transform_style] is the pretty-printer for [transform_style]. *)

val read_transform_style : Reader.t -> transform_style
(** [read_transform_style t] is the [transform_style] parsed from [t]. *)

val pp_backface_visibility : backface_visibility Pp.t
(** [pp_backface_visibility] is the pretty-printer for [backface_visibility]. *)

val read_backface_visibility : Reader.t -> backface_visibility
(** [read_backface_visibility t] is the [backface_visibility] parsed from [t].
*)

val pp_scale : scale Pp.t
(** [pp_scale] is the pretty-printer for [scale]. *)

val read_scale : Reader.t -> scale
(** [read_scale t] is the [scale] parsed from [t]. *)

val pp_timing_function : timing_function Pp.t
(** [pp_timing_function] is the pretty-printer for [timing_function]. *)

val read_timing_function : Reader.t -> timing_function
(** [read_timing_function t] is the [timing_function] parsed from [t]. *)

val pp_transition_property : transition_property Pp.t
(** [pp_transition_property] is the pretty-printer for [transition_property]. *)

val read_transition_property : Reader.t -> transition_property
(** [read_transition_property t] is the [transition_property] parsed from [t].
*)

val pp_transition : transition Pp.t
(** [pp_transition] is the pretty-printer for [transition]. *)

val read_transition : Reader.t -> transition
(** [read_transition t] is the [transition] parsed from [t]. *)

val read_transitions : Reader.t -> transition list
(** [read_transitions t] parses a comma-separated list of [transition]s. *)

val pp_animation_direction : animation_direction Pp.t
(** [pp_animation_direction] is the pretty-printer for [animation_direction]. *)

val read_animation_direction : Reader.t -> animation_direction
(** [read_animation_direction t] is the [animation_direction] parsed from [t].
*)

val pp_animation_fill_mode : animation_fill_mode Pp.t
(** [pp_animation_fill_mode] is the pretty-printer for [animation_fill_mode]. *)

val read_animation_fill_mode : Reader.t -> animation_fill_mode
(** [read_animation_fill_mode t] is the [animation_fill_mode] parsed from [t].
*)

val pp_animation_iteration_count : animation_iteration_count Pp.t
(** [pp_animation_iteration_count] is the pretty-printer for
    [animation_iteration_count]. *)

val read_animation_iteration_count : Reader.t -> animation_iteration_count
(** [read_animation_iteration_count t] is the [animation_iteration_count] parsed
    from [t]. *)

val pp_animation_play_state : animation_play_state Pp.t
(** [pp_animation_play_state] is the pretty-printer for [animation_play_state].
*)

val read_animation_play_state : Reader.t -> animation_play_state
(** [read_animation_play_state t] is the [animation_play_state] parsed from [t].
*)

val pp_animation : animation Pp.t
(** [pp_animation] is the pretty-printer for [animation]. *)

val read_animation : Reader.t -> animation
(** [read_animation t] is the [animation] parsed from [t]. *)

val read_animations : Reader.t -> animation list
(** [read_animations t] parses a comma-separated list of [animation]s. *)

val pp_blend_mode : blend_mode Pp.t
(** [pp_blend_mode] is the pretty-printer for [blend_mode]. *)

val read_blend_mode : Reader.t -> blend_mode
(** [read_blend_mode t] is the [blend_mode] parsed from [t]. *)

val read_blend_modes : Reader.t -> blend_mode list
(** [read_blend_modes t] parses a comma-separated list of [blend_mode] values.
*)

val pp_position_2d : position_2d Pp.t
(** [pp_position_2d] pretty-prints a 2D position. Special case: [Center, Center]
    prints as "center". *)

val pp_transform_origin : transform_origin Pp.t
(** [pp_transform_origin] pretty-prints a transform-origin value. *)

val pos_left : position_2d
(** [pos_left] position helper for [XY (Left, Center)]. *)

val pos_right : position_2d
(** [pos_right] position helper for [XY (Right, Center)]. *)

val pos_top : position_2d
(** [pos_top] position helper for [XY (Center, Top)]. *)

val pos_bottom : position_2d
(** [pos_bottom] position helper for [XY (Center, Bottom)]. *)

val origin : position_component -> position_component -> transform_origin
(** [origin x y] transform-origin helper for 2D positions. *)

val origin3d :
  position_component -> position_component -> length -> transform_origin
(** [origin3d x y z] transform-origin helper for 3D positions. *)

val pp_text_shadow : text_shadow Pp.t
(** [pp_text_shadow] is the pretty-printer for [text_shadow]. *)

val read_text_shadow : Reader.t -> text_shadow
(** [read_text_shadow t] is the [text_shadow] parsed from [t]. *)

val read_text_shadows : Reader.t -> text_shadow list
(** [read_text_shadows t] parses a comma-separated list of [text_shadow]s. *)

val pp_filter : filter Pp.t
(** [pp_filter] is the pretty-printer for [filter]. *)

val read_filter_item : Reader.t -> filter
(** [read_filter_item t] reads a single filter function. *)

val read_filter : Reader.t -> filter
(** [read_filter t] is the [filter] parsed from [t]. *)

val pp_background_attachment : background_attachment Pp.t
(** [pp_background_attachment] is the pretty-printer for
    [background_attachment]. *)

val read_background_attachment : Reader.t -> background_attachment
(** [read_background_attachment t] is the [background_attachment] parsed from
    [t]. *)

val pp_background_repeat : background_repeat Pp.t
(** [pp_background_repeat] is the pretty-printer for [background_repeat]. *)

val read_background_repeat : Reader.t -> background_repeat
(** [read_background_repeat t] is the [background_repeat] parsed from [t]. *)

val pp_background_size : background_size Pp.t
(** [pp_background_size] is the pretty-printer for [background_size]. *)

val read_background_size : Reader.t -> background_size
(** [read_background_size t] is the [background_size] parsed from [t]. *)

val pp_gradient_direction : gradient_direction Pp.t
(** [pp_gradient_direction] is the pretty-printer for [gradient_direction]. *)

val read_gradient_direction : Reader.t -> gradient_direction
(** [read_gradient_direction t] is the [gradient_direction] parsed from [t]. *)

val pp_gradient_stop : gradient_stop Pp.t
(** [pp_gradient_stop] is the pretty-printer for [gradient_stop]. *)

val read_gradient_stop : Reader.t -> gradient_stop
(** [read_gradient_stop t] is the [gradient_stop] parsed from [t]. *)

val pp_background_image : background_image Pp.t
(** [pp_background_image] is the pretty-printer for [background_image]. *)

val read_background_image : Reader.t -> background_image
(** [read_background_image t] is the [background_image] parsed from [t]. *)

val read_background_images : Reader.t -> background_image list
(** [read_background_images t] parses a comma-separated list of
    [background_image]s. *)

val read_background_box : Reader.t -> background_box
(** [read_background_box t] parses a background-clip or background-origin value.
*)

val pp_background_box : background_box Pp.t
(** [pp_background_box] pretty-prints a background-clip or background-origin
    value. *)

val read_background : Reader.t -> background
(** [read_background t] parses a background shorthand property. *)

val read_backgrounds : Reader.t -> background list
(** [read_backgrounds t] parses a comma-separated list of background shorthand
    properties. *)

val read_gap : Reader.t -> gap
(** [read_gap t] parses a gap shorthand property (one or two length values). *)

val pp_background : background Pp.t
(** [pp_background] pretty-prints a background value. *)

val pp_gap : gap Pp.t
(** [pp_gap] pretty-prints a gap shorthand value. *)

val pp_cursor : cursor Pp.t
(** [pp_cursor] is the pretty-printer for [cursor]. *)

val read_cursor : Reader.t -> cursor
(** [read_cursor t] is the [cursor] parsed from [t]. *)

val pp_user_select : user_select Pp.t
(** [pp_user_select] is the pretty-printer for [user_select]. *)

val read_user_select : Reader.t -> user_select
(** [read_user_select t] is the [user_select] parsed from [t]. *)

val pp_pointer_events : pointer_events Pp.t
(** [pp_pointer_events] is the pretty-printer for [pointer_events]. *)

val read_pointer_events : Reader.t -> pointer_events
(** [read_pointer_events t] is the [pointer_events] parsed from [t]. *)

val pp_touch_action : touch_action Pp.t
(** [pp_touch_action] is the pretty-printer for [touch_action]. *)

val read_touch_action : Reader.t -> touch_action
(** [read_touch_action t] is the [touch_action] parsed from [t]. *)

val pp_resize : resize Pp.t
(** [pp_resize] is the pretty-printer for [resize]. *)

val read_resize : Reader.t -> resize
(** [read_resize t] is the [resize] parsed from [t]. *)

val pp_box_sizing : box_sizing Pp.t
(** [pp_box_sizing] is the pretty-printer for [box_sizing]. *)

val read_box_sizing : Reader.t -> box_sizing
(** [read_box_sizing t] is the [box_sizing] parsed from [t]. *)

val pp_object_fit : object_fit Pp.t
(** [pp_object_fit] is the pretty-printer for [object_fit]. *)

val read_object_fit : Reader.t -> object_fit
(** [read_object_fit t] is the [object_fit] parsed from [t]. *)

val read_position_2d : Reader.t -> position_2d
(** [read_position_2d t] is the [position_2d] parsed from [t]. *)

val pp_content : content Pp.t
(** [pp_content] is the pretty-printer for [content]. *)

val read_content : Reader.t -> content
(** [read_content t] is the [content] parsed from [t]. *)

val pp_content_visibility : content_visibility Pp.t
(** [pp_content_visibility] is the pretty-printer for [content_visibility]. *)

val read_content_visibility : Reader.t -> content_visibility
(** [read_content_visibility t] is the [content_visibility] parsed from [t]. *)

val pp_container_type : container_type Pp.t
(** [pp_container_type] is the pretty-printer for [container_type]. *)

val read_container_type : Reader.t -> container_type
(** [read_container_type t] is the [container_type] parsed from [t]. *)

val pp_contain : contain Pp.t
(** [pp_contain] is the pretty-printer for [contain]. *)

val read_contain : Reader.t -> contain
(** [read_contain t] is the [contain] parsed from [t]. *)

val pp_isolation : isolation Pp.t
(** [pp_isolation] is the pretty-printer for [isolation]. *)

val read_isolation : Reader.t -> isolation
(** [read_isolation t] is the [isolation] parsed from [t]. *)

val pp_scroll_behavior : scroll_behavior Pp.t
(** [pp_scroll_behavior] is the pretty-printer for [scroll_behavior]. *)

val read_scroll_behavior : Reader.t -> scroll_behavior
(** [read_scroll_behavior t] is the [scroll_behavior] parsed from [t]. *)

val pp_scroll_snap_align : scroll_snap_align Pp.t
(** [pp_scroll_snap_align] is the pretty-printer for [scroll_snap_align]. *)

val read_scroll_snap_align : Reader.t -> scroll_snap_align
(** [read_scroll_snap_align t] is the [scroll_snap_align] parsed from [t]. *)

val pp_scroll_snap_stop : scroll_snap_stop Pp.t
(** [pp_scroll_snap_stop] is the pretty-printer for [scroll_snap_stop]. *)

val read_scroll_snap_stop : Reader.t -> scroll_snap_stop
(** [read_scroll_snap_stop t] is the [scroll_snap_stop] parsed from [t]. *)

val pp_scroll_snap_axis : scroll_snap_axis Pp.t
(** [pp_scroll_snap_axis] is the pretty-printer for [scroll_snap_axis]. *)

val read_scroll_snap_axis : Reader.t -> scroll_snap_axis
(** [read_scroll_snap_axis t] is the [scroll_snap_axis] parsed from [t]. *)

val pp_scroll_snap_strictness : scroll_snap_strictness Pp.t
(** [pp_scroll_snap_strictness] is the pretty-printer for
    [scroll_snap_strictness]. *)

val read_scroll_snap_strictness : Reader.t -> scroll_snap_strictness
(** [read_scroll_snap_strictness t] is the [scroll_snap_strictness] parsed from
    [t]. *)

val pp_scroll_snap_type : scroll_snap_type Pp.t
(** [pp_scroll_snap_type] is the pretty-printer for [scroll_snap_type]. *)

val read_scroll_snap_type : Reader.t -> scroll_snap_type
(** [read_scroll_snap_type t] is the [scroll_snap_type] parsed from [t]. *)

val pp_overscroll_behavior : overscroll_behavior Pp.t
(** [pp_overscroll_behavior] is the pretty-printer for [overscroll_behavior]. *)

val read_overscroll_behavior : Reader.t -> overscroll_behavior
(** [read_overscroll_behavior t] is the [overscroll_behavior] parsed from [t].
*)

val pp_svg_paint : svg_paint Pp.t
(** [pp_svg_paint] is the pretty-printer for [svg_paint]. *)

val read_svg_paint : Reader.t -> svg_paint
(** [read_svg_paint t] is the [svg_paint] parsed from [t]. *)

val pp_direction : direction Pp.t
(** [pp_direction] is the pretty-printer for [direction]. *)

val read_direction : Reader.t -> direction
(** [read_direction t] is the [direction] parsed from [t]. *)

val pp_unicode_bidi : unicode_bidi Pp.t
(** [pp_unicode_bidi] is the pretty-printer for [unicode_bidi]. *)

val read_unicode_bidi : Reader.t -> unicode_bidi
(** [read_unicode_bidi t] is the [unicode_bidi] parsed from [t]. *)

val pp_writing_mode : writing_mode Pp.t
(** [pp_writing_mode] is the pretty-printer for [writing_mode]. *)

val read_writing_mode : Reader.t -> writing_mode
(** [read_writing_mode t] is the [writing_mode] parsed from [t]. *)

val pp_webkit_appearance : webkit_appearance Pp.t
(** [pp_webkit_appearance] is the pretty-printer for [webkit_appearance]. *)

val read_webkit_appearance : Reader.t -> webkit_appearance
(** [read_webkit_appearance t] is the [webkit_appearance] parsed from [t]. *)

val pp_webkit_font_smoothing : webkit_font_smoothing Pp.t
(** [pp_webkit_font_smoothing] is the pretty-printer for
    [webkit_font_smoothing]. *)

val read_webkit_font_smoothing : Reader.t -> webkit_font_smoothing
(** [read_webkit_font_smoothing t] is the [webkit_font_smoothing] parsed from
    [t]. *)

val pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t
(** [pp_moz_osx_font_smoothing] is the pretty-printer for
    [moz_osx_font_smoothing]. *)

val read_moz_osx_font_smoothing : Reader.t -> moz_osx_font_smoothing
(** [read_moz_osx_font_smoothing t] is the [moz_osx_font_smoothing] parsed from
    [t]. *)

val pp_webkit_box_orient : webkit_box_orient Pp.t
(** [pp_webkit_box_orient] is the pretty-printer for [webkit_box_orient]. *)

val read_webkit_box_orient : Reader.t -> webkit_box_orient
(** [read_webkit_box_orient t] is the [webkit_box_orient] parsed from [t]. *)

val read_text_size_adjust : Reader.t -> text_size_adjust
(** [read_text_size_adjust t] is the text size adjust value parsed from [t]. *)

val pp_forced_color_adjust : forced_color_adjust Pp.t
(** [pp_forced_color_adjust] is the pretty-printer for [forced_color_adjust]. *)

val read_forced_color_adjust : Reader.t -> forced_color_adjust
(** [read_forced_color_adjust t] is the [forced_color_adjust] parsed from [t].
*)

val pp_appearance : appearance Pp.t
(** [pp_appearance] is the pretty-printer for [appearance]. *)

val read_appearance : Reader.t -> appearance
(** [read_appearance t] is the [appearance] parsed from [t]. *)

val pp_clear : clear Pp.t
(** [pp_clear] is the pretty-printer for [clear]. *)

val read_clear : Reader.t -> clear
(** [read_clear t] is the [clear] parsed from [t]. *)

val pp_float_side : float_side Pp.t
(** [pp_float_side] is the pretty-printer for [float_side]. *)

val read_float_side : Reader.t -> float_side
(** [read_float_side t] is the [float_side] parsed from [t]. *)

val pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t
(** [pp_text_decoration_skip_ink] is the pretty-printer for
    [text_decoration_skip_ink]. *)

val read_text_decoration_skip_ink : Reader.t -> text_decoration_skip_ink
(** [read_text_decoration_skip_ink t] is the [text_decoration_skip_ink] parsed
    from [t]. *)

(** {2 Helper functions for property types} *)

val shadow :
  ?inset:bool ->
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [shadow ?inset ?h_offset ?v_offset ?blur ?spread ?color ()] is a shadow
    value with optional parameters. Defaults: inset=false, h_offset=0px,
    v_offset=0px, blur=0px, spread=0px, color=Rgb(0,0,0). *)

val inset_ring_shadow :
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [inset_ring_shadow ~h_offset ~v_offset ~blur ~spread ~color] is an inset
    shadow with the given parameters. *)

val background :
  ?color:color ->
  ?image:background_image ->
  ?position:position_2d ->
  ?size:background_size ->
  ?repeat:background_repeat ->
  ?attachment:background_attachment ->
  ?clip:background_box ->
  ?origin:background_box ->
  unit ->
  background
(** [background ?color ?image ?position ?size ?repeat ?attachment ?clip ?origin
     ()] constructs a background value with optional components. *)
