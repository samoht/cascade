(** CSS generation utilities with abstract types *)

(** {1 Types} *)

type var =
  | Color of string * int option (* color name and optional shade *)
  | Spacing of int (* spacing value *)
  | Font of string (* font family *)
  | Text_size of string (* text size *)
  | Font_weight of string (* font weight *)
  | Radius of string (* border radius *)
  | Transition (* transition timing *)
  | Custom of string * string (* custom variable name and value *)
      (** CSS variable requirements *)

type property_name
(** Abstract type for CSS property names *)

type property
(** Abstract type for CSS properties *)

type rule
(** Abstract type for CSS rules *)

type rule_or_nested
(** Abstract type for rules or nested content *)

type media_query
(** Abstract type for media queries *)

type container_query
(** Abstract type for container queries *)

type starting_style
(** Abstract type for [@starting-style] rules *)

type supports_query
(** Abstract type for [@supports] rules *)

type layer =
  | Properties
  | Theme
  | Base
  | Components
  | Utilities  (** CSS layer types for Tailwind v4 *)

type layered_rules
(** Abstract type for rules within a layer *)

type t
(** Abstract type for CSS stylesheets *)

val pp : t -> string
(** [pp  stylesheet] pretty-prints a CSS stylesheet. *)

type at_property
(** Abstract type for [@property] rules *)

(** {1 Property Constructors} *)

val background_color : string -> property
(** [background_color value] sets the CSS background-color property to value. *)

val color : string -> property
(** [color value] sets the CSS color property to value. *)

val border_color : string -> property
(** [border_color value] sets the CSS border-color property to value. *)

val border_style : string -> property
(** [border_style value] sets the CSS border-style property to value. *)

val padding : string -> property
(** [padding value] sets the CSS padding property to value. *)

val padding_left : string -> property
(** [padding_left value] sets the CSS padding-left property to value. *)

val padding_right : string -> property
(** [padding_right value] sets the CSS padding-right property to value. *)

val padding_bottom : string -> property
(** [padding_bottom value] sets the CSS padding-bottom property to value. *)

val padding_top : string -> property
(** [padding_top value] sets the CSS padding-top property to value. *)

val padding_inline : string -> property
(** [padding_inline value] sets the CSS padding-inline property to value. *)

val padding_block : string -> property
(** [padding_block value] sets the CSS padding-block property to value. *)

val margin : string -> property
(** [margin value] sets the CSS margin property to value. *)

val margin_left : string -> property
(** [margin_left value] sets the CSS margin-left property to value. *)

val margin_right : string -> property
(** [margin_right value] sets the CSS margin-right property to value. *)

val margin_top : string -> property
(** [margin_top value] sets the CSS margin-top property to value. *)

val margin_bottom : string -> property
(** [margin_bottom value] sets the CSS margin-bottom property to value. *)

val margin_inline : string -> property
(** [margin_inline value] sets the CSS margin-inline property to value. *)

val margin_block : string -> property
(** [margin_block value] sets the CSS margin-block property to value. *)

val gap : string -> property
(** [gap value] sets the CSS gap property to value. *)

val column_gap : string -> property
(** [column_gap value] sets the CSS column-gap property to value. *)

val row_gap : string -> property
(** [row_gap value] sets the CSS row-gap property to value. *)

val width : string -> property
(** [width value] sets the CSS width property to value. *)

val height : string -> property
(** [height value] sets the CSS height property to value. *)

val min_width : string -> property
(** [min_width value] sets the CSS min-width property to value. *)

val min_height : string -> property
(** [min_height value] sets the CSS min-height property to value. *)

val max_width : string -> property
(** [max_width value] sets the CSS max-width property to value. *)

val max_height : string -> property
(** [max_height value] sets the CSS max-height property to value. *)

val font_size : string -> property
(** [font_size value] sets the CSS font-size property to value. *)

val line_height : string -> property
(** [line_height value] sets the CSS line-height property to value. *)

val font_weight : string -> property
(** [font_weight value] sets the CSS font-weight property to value. *)

val font_style : string -> property
(** [font_style value] sets the CSS font-style property to value. *)

val text_align : string -> property
(** [text_align value] sets the CSS text-align property to value. *)

val text_decoration : string -> property
(** [text_decoration value] sets the CSS text-decoration property to value. *)

val text_decoration_style : string -> property
(** [text_decoration_style value] sets the CSS text-decoration-style property to
    value. *)

val text_underline_offset : string -> property
(** [text_underline_offset value] sets the CSS text-underline-offset property to
    value. *)

val text_transform : string -> property
(** [text_transform value] sets the CSS text-transform property to value. *)

val letter_spacing : string -> property
(** [letter_spacing value] sets the CSS letter-spacing property to value. *)

val white_space : string -> property
(** [white_space value] sets the CSS white-space property to value. *)

val display : string -> property
(** [display value] sets the CSS display property to value. *)

val position : string -> property
(** [position value] sets the CSS position property to value. *)

val flex_direction : string -> property
(** [flex_direction value] sets the CSS flex-direction property to value. *)

val flex_wrap : string -> property
(** [flex_wrap value] sets the CSS flex-wrap property to value. *)

val flex : string -> property
(** [flex value] sets the CSS flex property to value. *)

val flex_grow : string -> property
(** [flex_grow value] sets the CSS flex-grow property to value. *)

val flex_shrink : string -> property
(** [flex_shrink value] sets the CSS flex-shrink property to value. *)

val align_items : string -> property
(** [align_items value] sets the CSS align-items property to value. *)

val justify_content : string -> property
(** [justify_content value] sets the CSS justify-content property to value. *)

val align_content : string -> property
(** [align_content value] sets the CSS align-content property to value. *)

val align_self : string -> property
(** [align_self value] sets the CSS align-self property to value. *)

val justify_self : string -> property
(** [justify_self value] sets the CSS justify-self property to value. *)

val place_content : string -> property
(** [place_content value] sets the CSS place-content property to value. *)

val place_items : string -> property
(** [place_items value] sets the CSS place-items property to value. *)

val place_self : string -> property
(** [place_self value] sets the CSS place-self property to value. *)

val grid_template_columns : string -> property
(** [grid_template_columns value] sets the CSS grid-template-columns property to
    value. *)

val grid_template_rows : string -> property
(** [grid_template_rows value] sets the CSS grid-template-rows property to
    value. *)

val border_width : string -> property
(** [border_width value] sets the CSS border-width property to value. *)

val border_radius : string -> property
(** [border_radius value] sets the CSS border-radius property to value. *)

val box_shadow : string -> property
(** [box_shadow value] sets the CSS box-shadow property to value. *)

val opacity : string -> property
(** [opacity value] sets the CSS opacity property to value. *)

val transition : string -> property
(** [transition value] sets the CSS transition property to value. *)

val transform : string -> property
(** [transform value] sets the CSS transform property to value. *)

val cursor : string -> property
(** [cursor value] sets the CSS cursor property to value. *)

val table_layout : string -> property
(** [table_layout value] sets the CSS table-layout property to value. *)

val border_collapse : string -> property
(** [border_collapse value] sets the CSS border-collapse property to value. *)

val border_spacing : string -> property
(** [border_spacing value] sets the CSS border-spacing property to value. *)

val user_select : string -> property
(** [user_select value] sets the CSS user-select property to value. *)

val pointer_events : string -> property
(** [pointer_events value] sets the CSS pointer-events property to value. *)

val overflow : string -> property
(** [overflow value] sets the CSS overflow property to value. *)

val object_fit : string -> property
(** [object_fit value] sets the CSS object-fit property to value. *)

val top : string -> property
(** [top value] sets the CSS top property to value. *)

val right : string -> property
(** [right value] sets the CSS right property to value. *)

val bottom : string -> property
(** [bottom value] sets the CSS bottom property to value. *)

val left : string -> property
(** [left value] sets the CSS left property to value. *)

val z_index : string -> property
(** [z_index value] sets the CSS z-index property to value. *)

val border_top_width : string -> property
(** [border_top_width value] sets the CSS border-top-width property to value. *)

val border_right_width : string -> property
(** [border_right_width value] sets the CSS border-right-width property to
    value. *)

val border_bottom_width : string -> property
(** [border_bottom_width value] sets the CSS border-bottom-width property to
    value. *)

val border_left_width : string -> property
(** [border_left_width value] sets the CSS border-left-width property to value.
*)

val outline : string -> property
(** [outline value] sets the CSS outline property to value. *)

val outline_offset : string -> property
(** [outline_offset value] sets the CSS outline-offset property to value. *)

val clip : string -> property
(** [clip value] sets the CSS clip property to value. *)

val filter : string -> property
(** [filter value] sets the CSS filter property to value. *)

val background_image : string -> property
(** [background_image value] sets the CSS background-image property to value. *)

val animation : string -> property
(** [animation value] sets the CSS animation property to value. *)

val appearance : string -> property
(** [appearance value] sets the CSS appearance property to value. *)

val overflow_x : string -> property
(** [overflow_x value] sets the CSS overflow-x property to value. *)

val overflow_y : string -> property
(** [overflow_y value] sets the CSS overflow-y property to value. *)

val resize : string -> property
(** [resize value] sets the CSS resize property to value. *)

val vertical_align : string -> property
(** [vertical_align value] sets the CSS vertical-align property to value. *)

val box_sizing : string -> property
(** [box_sizing value] sets the CSS box-sizing property to value. *)

val font_family : string -> property
(** [font_family value] sets the CSS font-family property to value. *)

val background_position : string -> property
(** [background_position value] sets the CSS background-position property to
    value. *)

val background_repeat : string -> property
(** [background_repeat value] sets the CSS background-repeat property to value.
*)

val background_size : string -> property
(** [background_size value] sets the CSS background-size property to value. *)

val webkit_font_smoothing : string -> property
(** [webkit_font_smoothing value] sets the CSS -webkit-font-smoothing property
    to value. *)

val moz_osx_font_smoothing : string -> property
(** [moz_osx_font_smoothing value] sets the CSS -moz-osx-font-smoothing property
    to value. *)

val webkit_line_clamp : string -> property
(** [webkit_line_clamp value] sets the CSS -webkit-line-clamp property to value.
*)

val backdrop_filter : string -> property
(** [backdrop_filter value] sets the CSS backdrop-filter property to value. *)

val scroll_snap_type : string -> property
(** [scroll_snap_type value] sets the CSS scroll-snap-type property to value. *)

val scroll_snap_align : string -> property
(** [scroll_snap_align value] sets the CSS scroll-snap-align property to value.
*)

val scroll_snap_stop : string -> property
(** [scroll_snap_stop value] sets the CSS scroll-snap-stop property to value. *)

val scroll_behavior : string -> property
(** [scroll_behavior value] sets the CSS scroll-behavior property to value. *)

val property : string -> string -> property
(** [property name value] creates a custom CSS property. *)

val rule : selector:string -> property list -> rule
(** [rule ~selector properties] creates a CSS rule with a selector and
    properties. *)

val selector : rule -> string
(** [selector rule] returns the selector of a CSS rule. *)

val properties : rule -> property list
(** [properties rule] returns the properties of a CSS rule. *)

val rule_to_nested : rule -> rule_or_nested
(** [rule_to_nested rule] converts a rule to a rule_or_nested. *)

val supports_to_nested : supports_query -> rule_or_nested
(** [supports_to_nested supports] converts a supports query to a rule_or_nested.
*)

val media : condition:string -> rule list -> media_query
(** [media ~condition rules] creates a media query. *)

val supports : condition:string -> rule list -> supports_query
(** [supports ~condition rules] creates a [@supports] query. *)

val supports_nested :
  condition:string -> rule list -> supports_query list -> supports_query
(** [supports_nested ~condition rules nested_queries] creates a [@supports]
    query with nested [@supports] queries. *)

val container :
  ?name:string option -> condition:string -> rule list -> container_query
(** [container ?name ~condition rules] creates a container query. *)

val at_property :
  name:string ->
  syntax:string ->
  initial_value:string ->
  ?inherits:bool ->
  unit ->
  at_property
(** [at_property ~name ~syntax ~initial_value ?inherits ()] creates a
    [@property] rule for custom properties. *)

val layered_rules :
  layer:layer ->
  ?media_queries:media_query list ->
  ?container_queries:container_query list ->
  ?supports_queries:supports_query list ->
  rule_or_nested list ->
  layered_rules
(** [layered_rules ~layer ?media_queries ?container_queries ?supports_queries
     rules] creates rules within a specific CSS layer with optional nested
    at-rules. *)

val stylesheet :
  ?layers:layered_rules list ->
  ?media_queries:media_query list ->
  ?container_queries:container_query list ->
  ?starting_styles:starting_style list ->
  ?supports_queries:supports_query list ->
  ?at_properties:at_property list ->
  rule list ->
  t
(** [stylesheet ?layers ?media_queries ?container_queries ?starting_styles
     ?supports_queries ?at_properties rules] creates a stylesheet with optional
    layers and at-rules. *)

(** {1 Rendering} *)

val to_string : ?minify:bool -> ?preserve_order:bool -> t -> string
(** [to_string ?minify ?preserve_order stylesheet] renders a complete stylesheet
    to CSS. If [preserve_order] is true, rules are kept in their original order
    without merging. *)

val property_name_to_string : property_name -> string
(** [property_name_to_string prop] converts a property name to its CSS string
    representation. *)

val property_value : property -> string
(** [property_value prop] extracts the value from a property. *)

val property_name : property -> property_name
(** [property_name prop] extracts the property name from a property. *)

val is_custom_property : property -> bool
(** [is_custom_property prop] returns true if the property is a CSS custom
    property (starts with --). *)

(** {1 Utilities} *)

val all_vars : property list -> string list
(** [all_vars properties] extracts all CSS variable names referenced in property
    values, returning them sorted and deduplicated. *)

val deduplicate_properties : property list -> property list
(** [deduplicate_properties properties] removes duplicate properties, keeping
    the last occurrence. *)

val properties_to_inline_style : property list -> string
(** [properties_to_inline_style properties] converts a list of properties to an
    inline style string. *)

(** {1 Version Information} *)

val version : string
(** The version string of the library. *)

val header : string
(** CSS comment header with version information. *)
