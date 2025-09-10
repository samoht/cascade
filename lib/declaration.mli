(** CSS declarations and parser. *)

include module type of Declaration_intf

val pp_property : 'a Properties.property Pp.t
(** [pp_property] is the pretty-printer for CSS property names. *)

val pp_declaration : declaration Pp.t
(** [pp_declaration] is the pretty-printer for declarations. *)

val pp_value : ('a kind * 'a) Pp.t
(** [pp_value] is the pretty-printer for typed values. *)

val declaration_meta : declaration -> Values.meta option
(** [declaration_meta d] is the metadata of [d], if any. *)

val important : declaration -> declaration
(** [important d] is [d] marked as [!important]. *)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a raw custom property declaration. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer d] is the layer of [d], if any. *)

val is_custom_property : string -> bool
(** [is_custom_property name] is [true] if [name] is a CSS custom property name.
*)

val read_property_name : Reader.t -> string
(** [read_property_name t] is the property name read from [t]. *)

val read_property_value : Reader.t -> string
(** [read_property_value t] is the value read from [t] (until ';' or '\}'). *)

val read_declaration : Reader.t -> declaration option
(** [read_declaration t] is one typed declaration, or [None] when no more valid
    declarations. Performs full property name and value validation per CSS spec.
*)

val read_declarations : Reader.t -> declaration list
(** [read_declarations t] is all typed declarations in an unbraced block. *)

val read_block : Reader.t -> declaration list
(** [read_block t] is the typed declarations parsed from a braced block. *)

(** {2 Type-driven helper functions} *)

val declaration : ?important:bool -> 'a Properties.property -> 'a -> declaration
(** [declaration ?important property value] creates a typed declaration. *)

val custom_declaration :
  ?important:bool ->
  ?layer:string ->
  ?meta:Values.meta ->
  string ->
  'a kind ->
  'a ->
  declaration
(** [custom_declaration ?important ?layer ?meta name kind value] creates a
    custom property declaration. *)

val is_important : declaration -> bool
(** [is_important decl] returns true if the declaration has !important. *)

val property_name : declaration -> string
(** [property_name decl] returns the property name as a string. *)

val string_of_value : ?minify:bool -> declaration -> string
(** [string_of_value ?minify decl] returns the value as a string. *)

(** Single-to-list property helpers. These construct typed declarations for
    properties that accept comma-separated lists, while keeping a simple
    single-value API. *)

open Values
open Properties

val background_image : background_image -> declaration
val text_shadow : text_shadow -> declaration
val transition : transition -> declaration
val transitions : transition list -> declaration
val animation : animation -> declaration
val box_shadow : box_shadow -> declaration
val box_shadow_list : box_shadow list -> declaration

(** Declaration constructors *)

val z_index_auto : declaration

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric

val font_variant_numeric_composed :
  ?ordinal:font_variant_numeric_token ->
  ?slashed_zero:font_variant_numeric_token ->
  ?numeric_figure:font_variant_numeric_token ->
  ?numeric_spacing:font_variant_numeric_token ->
  ?numeric_fraction:font_variant_numeric_token ->
  unit ->
  font_variant_numeric

val background_color : color -> declaration
val color : color -> declaration
val border_color : color -> declaration
val border_style : border_style -> declaration
val border_top_style : border_style -> declaration
val border_right_style : border_style -> declaration
val border_bottom_style : border_style -> declaration
val border_left_style : border_style -> declaration
val text_decoration : text_decoration -> declaration
val font_style : font_style -> declaration
val list_style_type : list_style_type -> declaration
val list_style_position : list_style_position -> declaration
val list_style_image : list_style_image -> declaration
val padding : length -> declaration
val padding_left : length -> declaration
val padding_right : length -> declaration
val padding_bottom : length -> declaration
val padding_top : length -> declaration
val margin : length -> declaration
val margin_left : length -> declaration
val margin_right : length -> declaration
val margin_top : length -> declaration
val margin_bottom : length -> declaration
val gap : length -> declaration
val column_gap : length -> declaration
val row_gap : length -> declaration
val grid_template_areas : string -> declaration
val grid_template : grid_template -> declaration
val grid_auto_columns : grid_template -> declaration
val grid_auto_rows : grid_template -> declaration
val grid_row_start : grid_line -> declaration
val grid_row_end : grid_line -> declaration
val grid_column_start : grid_line -> declaration
val grid_column_end : grid_line -> declaration
val grid_row : grid_line * grid_line -> declaration
val grid_column : grid_line * grid_line -> declaration
val grid_area : string -> declaration
val width : length -> declaration
val height : length -> declaration
val min_width : length -> declaration
val min_height : length -> declaration
val max_width : length -> declaration
val max_height : length -> declaration
val font_size : length -> declaration
val line_height : line_height -> declaration
val font_weight : font_weight -> declaration
val text_align : text_align -> declaration
val text_decoration_style : text_decoration_style -> declaration
val text_underline_offset : string -> declaration
val text_transform : text_transform -> declaration
val letter_spacing : length -> declaration
val white_space : white_space -> declaration
val display : display -> declaration
val position : position -> declaration
val visibility : visibility -> declaration
val top : length -> declaration
val right : length -> declaration
val bottom : length -> declaration
val left : length -> declaration
val opacity : float -> declaration
val flex_direction : flex_direction -> declaration
val flex : flex -> declaration
val flex_grow : float -> declaration
val flex_shrink : float -> declaration
val flex_basis : length -> declaration
val flex_wrap : flex_wrap -> declaration
val order : int -> declaration
val align_items : align_items -> declaration
val align_content : align -> declaration
val align_self : align_self -> declaration
val justify_content : justify_content -> declaration
val justify_items : justify -> declaration
val justify_self : justify -> declaration
val place_content : place_content -> declaration
val place_items : place_items -> declaration
val place_self : align_self -> declaration
val border_width : border_width -> declaration
val border_radius : length -> declaration
val fill : svg_paint -> declaration
val stroke : svg_paint -> declaration
val stroke_width : length -> declaration
val outline_style : outline_style -> declaration
val outline_width : length -> declaration
val outline_color : color -> declaration
val forced_color_adjust : forced_color_adjust -> declaration
val table_layout : table_layout -> declaration
val border_spacing : length -> declaration
val overflow : overflow -> declaration
val object_fit : object_fit -> declaration
val clip : string -> declaration
val clear : clear -> declaration
val float : float_side -> declaration
val touch_action : touch_action -> declaration
val direction : direction -> declaration
val unicode_bidi : unicode_bidi -> declaration
val writing_mode : writing_mode -> declaration
val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
val animation_name : string -> declaration
val animation_duration : duration -> declaration
val animation_timing_function : timing_function -> declaration
val animation_delay : duration -> declaration
val animation_iteration_count : animation_iteration_count -> declaration
val animation_direction : animation_direction -> declaration
val animation_fill_mode : animation_fill_mode -> declaration
val animation_play_state : animation_play_state -> declaration
val background_blend_mode : blend_mode -> declaration
val scroll_margin : length -> declaration
val scroll_margin_top : length -> declaration
val scroll_margin_right : length -> declaration
val scroll_margin_bottom : length -> declaration
val scroll_margin_left : length -> declaration
val scroll_padding : length -> declaration
val scroll_padding_top : length -> declaration
val scroll_padding_right : length -> declaration
val scroll_padding_bottom : length -> declaration
val scroll_padding_left : length -> declaration
val overscroll_behavior : overscroll_behavior -> declaration
val overscroll_behavior_x : overscroll_behavior -> declaration
val overscroll_behavior_y : overscroll_behavior -> declaration
val accent_color : color -> declaration
val caret_color : color -> declaration
val text_decoration_color : color -> declaration
val text_decoration_thickness : length -> declaration
val text_size_adjust : string -> declaration
val aspect_ratio : aspect_ratio -> declaration
val filter : filter -> declaration
val mix_blend_mode : blend_mode -> declaration
val grid_template_columns : grid_template -> declaration
val grid_template_rows : grid_template -> declaration
val grid_auto_flow : grid_auto_flow -> declaration
val pointer_events : pointer_events -> declaration
val z_index : z_index -> declaration
val appearance : appearance -> declaration
val overflow_x : overflow -> declaration
val overflow_y : overflow -> declaration
val resize : resize -> declaration
val vertical_align : vertical_align -> declaration
val box_sizing : box_sizing -> declaration
val font_family : font_family list -> declaration
val word_spacing : length -> declaration
val background_attachment : background_attachment -> declaration
val border_top : string -> declaration
val border_right : string -> declaration
val border_bottom : string -> declaration
val border_left : string -> declaration
val object_position : position_2d -> declaration
val transform_origin : transform_origin -> declaration
val clip_path : string -> declaration
val mask : string -> declaration
val content_visibility : content_visibility -> declaration
val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
val webkit_line_clamp : int -> declaration
val webkit_box_orient : webkit_box_orient -> declaration
val text_overflow : text_overflow -> declaration
val text_wrap : text_wrap -> declaration
val word_break : word_break -> declaration
val overflow_wrap : overflow_wrap -> declaration
val hyphens : hyphens -> declaration
val webkit_hyphens : hyphens -> declaration
val font_stretch : font_stretch -> declaration
val font_variant_numeric : font_variant_numeric -> declaration
val backdrop_filter : filter -> declaration
val background_position : position_2d list -> declaration
val background_repeat : background_repeat -> declaration
val background_size : background_size -> declaration
val content : content -> declaration
val border_left_width : border_width -> declaration
val border_inline_start_width : border_width -> declaration
val border_inline_end_width : border_width -> declaration
val border_bottom_width : border_width -> declaration
val border_top_width : border_width -> declaration
val border_right_width : border_width -> declaration
val border_top_color : color -> declaration
val border_right_color : color -> declaration
val border_bottom_color : color -> declaration
val border_left_color : color -> declaration
val border_inline_start_color : color -> declaration
val border_inline_end_color : color -> declaration
val quotes : string -> declaration

val border :
  ?width:Properties.border_width ->
  ?style:Properties.border_style ->
  ?color:Values.color ->
  unit ->
  declaration

val tab_size : int -> declaration
val webkit_text_size_adjust : text_size_adjust -> declaration
val font_feature_settings : font_feature_settings -> declaration
val font_variation_settings : font_variation_settings -> declaration
val webkit_tap_highlight_color : color -> declaration
val webkit_text_decoration : text_decoration -> declaration
val webkit_text_decoration_color : color -> declaration
val text_indent : length -> declaration
val border_collapse : border_collapse -> declaration
val list_style : string -> declaration
val font : string -> declaration
val webkit_appearance : webkit_appearance -> declaration
val webkit_font_smoothing : webkit_font_smoothing -> declaration
val cursor : cursor -> declaration
val user_select : user_select -> declaration
val container_type : container_type -> declaration
val container_name : string -> declaration
val transform : transform list -> declaration
val rotate : angle -> declaration
val scale : scale -> declaration
val perspective : length -> declaration
val perspective_origin : string -> declaration
val transform_style : transform_style -> declaration
val backface_visibility : backface_visibility -> declaration
val transition_duration : duration -> declaration
val transition_timing_function : timing_function -> declaration
val transition_delay : duration -> declaration
val will_change : string -> declaration
val contain : contain -> declaration
val isolation : isolation -> declaration
val padding_inline : length -> declaration
val padding_inline_start : length -> declaration
val padding_inline_end : length -> declaration
val padding_block : length -> declaration
val margin_inline : length -> declaration
val margin_block : length -> declaration
val margin_inline_end : length -> declaration
val outline : string -> declaration
val outline_offset : length -> declaration
val scroll_snap_type : scroll_snap_type -> declaration
val scroll_snap_align : scroll_snap_align -> declaration
val scroll_snap_stop : scroll_snap_stop -> declaration
val scroll_behavior : scroll_behavior -> declaration
