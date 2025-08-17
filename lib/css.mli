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

type property
(** Abstract type for CSS property names *)

type declaration
(** Abstract type for CSS declarations (property-value pairs) *)

type rule
(** Abstract type for CSS rules *)

type nested_rule
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

val pp : ?minify:bool -> t -> string
(** [pp stylesheet] is [to_string]. We don't use Format to have an efficient
    js_of_ocaml bundle. *)

type at_property
(** Abstract type for [@property] rules *)

(** {1 Declaration Constructors} *)

val background_color : string -> declaration
(** [background_color value] sets the CSS background-color property to value. *)

val color : string -> declaration
(** [color value] sets the CSS color property to value. *)

val border_color : string -> declaration
(** [border_color value] sets the CSS border-color property to value. *)

val border_style : string -> declaration
(** [border_style value] sets the CSS border-style property to value. *)

val padding : string -> declaration
(** [padding value] sets the CSS padding property to value. *)

val padding_left : string -> declaration
(** [padding_left value] sets the CSS padding-left property to value. *)

val padding_right : string -> declaration
(** [padding_right value] sets the CSS padding-right property to value. *)

val padding_bottom : string -> declaration
(** [padding_bottom value] sets the CSS padding-bottom property to value. *)

val padding_top : string -> declaration
(** [padding_top value] sets the CSS padding-top property to value. *)

val padding_inline : string -> declaration
(** [padding_inline value] sets the CSS padding-inline property to value. *)

val padding_block : string -> declaration
(** [padding_block value] sets the CSS padding-block property to value. *)

val margin : string -> declaration
(** [margin value] sets the CSS margin property to value. *)

val margin_left : string -> declaration
(** [margin_left value] sets the CSS margin-left property to value. *)

val margin_right : string -> declaration
(** [margin_right value] sets the CSS margin-right property to value. *)

val margin_top : string -> declaration
(** [margin_top value] sets the CSS margin-top property to value. *)

val margin_bottom : string -> declaration
(** [margin_bottom value] sets the CSS margin-bottom property to value. *)

val margin_inline : string -> declaration
(** [margin_inline value] sets the CSS margin-inline property to value. *)

val margin_block : string -> declaration
(** [margin_block value] sets the CSS margin-block property to value. *)

val gap : string -> declaration
(** [gap value] sets the CSS gap property to value. *)

val column_gap : string -> declaration
(** [column_gap value] sets the CSS column-gap property to value. *)

val row_gap : string -> declaration
(** [row_gap value] sets the CSS row-gap property to value. *)

val width : string -> declaration
(** [width value] sets the CSS width property to value. *)

val height : string -> declaration
(** [height value] sets the CSS height property to value. *)

val min_width : string -> declaration
(** [min_width value] sets the CSS min-width property to value. *)

val min_height : string -> declaration
(** [min_height value] sets the CSS min-height property to value. *)

val max_width : string -> declaration
(** [max_width value] sets the CSS max-width property to value. *)

val max_height : string -> declaration
(** [max_height value] sets the CSS max-height property to value. *)

val font_size : string -> declaration
(** [font_size value] sets the CSS font-size property to value. *)

val line_height : string -> declaration
(** [line_height value] sets the CSS line-height property to value. *)

val font_weight : string -> declaration
(** [font_weight value] sets the CSS font-weight property to value. *)

val font_style : string -> declaration
(** [font_style value] sets the CSS font-style property to value. *)

val text_align : string -> declaration
(** [text_align value] sets the CSS text-align property to value. *)

val text_decoration : string -> declaration
(** [text_decoration value] sets the CSS text-decoration property to value. *)

val text_decoration_style : string -> declaration
(** [text_decoration_style value] sets the CSS text-decoration-style property to
    value. *)

val text_underline_offset : string -> declaration
(** [text_underline_offset value] sets the CSS text-underline-offset property to
    value. *)

val text_transform : string -> declaration
(** [text_transform value] sets the CSS text-transform property to value. *)

val letter_spacing : string -> declaration
(** [letter_spacing value] sets the CSS letter-spacing property to value. *)

val white_space : string -> declaration
(** [white_space value] sets the CSS white-space property to value. *)

val display : string -> declaration
(** [display value] sets the CSS display property to value. *)

val position : string -> declaration
(** [position value] sets the CSS position property to value. *)

val flex_direction : string -> declaration
(** [flex_direction value] sets the CSS flex-direction property to value. *)

val flex_wrap : string -> declaration
(** [flex_wrap value] sets the CSS flex-wrap property to value. *)

val flex : string -> declaration
(** [flex value] sets the CSS flex property to value. *)

val flex_grow : string -> declaration
(** [flex_grow value] sets the CSS flex-grow property to value. *)

val flex_shrink : string -> declaration
(** [flex_shrink value] sets the CSS flex-shrink property to value. *)

val align_items : string -> declaration
(** [align_items value] sets the CSS align-items property to value. *)

val justify_content : string -> declaration
(** [justify_content value] sets the CSS justify-content property to value. *)

val align_content : string -> declaration
(** [align_content value] sets the CSS align-content property to value. *)

val align_self : string -> declaration
(** [align_self value] sets the CSS align-self property to value. *)

val justify_self : string -> declaration
(** [justify_self value] sets the CSS justify-self property to value. *)

val place_content : string -> declaration
(** [place_content value] sets the CSS place-content property to value. *)

val place_items : string -> declaration
(** [place_items value] sets the CSS place-items property to value. *)

val place_self : string -> declaration
(** [place_self value] sets the CSS place-self property to value. *)

val grid_template_columns : string -> declaration
(** [grid_template_columns value] sets the CSS grid-template-columns property to
    value. *)

val grid_template_rows : string -> declaration
(** [grid_template_rows value] sets the CSS grid-template-rows property to
    value. *)

val border_width : string -> declaration
(** [border_width value] sets the CSS border-width property to value. *)

val border_radius : string -> declaration
(** [border_radius value] sets the CSS border-radius property to value. *)

val box_shadow : string -> declaration
(** [box_shadow value] sets the CSS box-shadow property to value. *)

val opacity : string -> declaration
(** [opacity value] sets the CSS opacity property to value. *)

val transition : string -> declaration
(** [transition value] sets the CSS transition property to value. *)

val transform : string -> declaration
(** [transform value] sets the CSS transform property to value. *)

val cursor : string -> declaration
(** [cursor value] sets the CSS cursor property to value. *)

val table_layout : string -> declaration
(** [table_layout value] sets the CSS table-layout property to value. *)

val border_collapse : string -> declaration
(** [border_collapse value] sets the CSS border-collapse property to value. *)

val border_spacing : string -> declaration
(** [border_spacing value] sets the CSS border-spacing property to value. *)

val user_select : string -> declaration
(** [user_select value] sets the CSS user-select property to value. *)

val pointer_events : string -> declaration
(** [pointer_events value] sets the CSS pointer-events property to value. *)

val overflow : string -> declaration
(** [overflow value] sets the CSS overflow property to value. *)

val object_fit : string -> declaration
(** [object_fit value] sets the CSS object-fit property to value. *)

val top : string -> declaration
(** [top value] sets the CSS top property to value. *)

val right : string -> declaration
(** [right value] sets the CSS right property to value. *)

val bottom : string -> declaration
(** [bottom value] sets the CSS bottom property to value. *)

val left : string -> declaration
(** [left value] sets the CSS left property to value. *)

val z_index : string -> declaration
(** [z_index value] sets the CSS z-index property to value. *)

val border_top_width : string -> declaration
(** [border_top_width value] sets the CSS border-top-width property to value. *)

val border_right_width : string -> declaration
(** [border_right_width value] sets the CSS border-right-width property to
    value. *)

val border_bottom_width : string -> declaration
(** [border_bottom_width value] sets the CSS border-bottom-width property to
    value. *)

val border_left_width : string -> declaration
(** [border_left_width value] sets the CSS border-left-width property to value.
*)

val outline : string -> declaration
(** [outline value] sets the CSS outline property to value. *)

val outline_offset : string -> declaration
(** [outline_offset value] sets the CSS outline-offset property to value. *)

val clip : string -> declaration
(** [clip value] sets the CSS clip property to value. *)

val filter : string -> declaration
(** [filter value] sets the CSS filter property to value. *)

val background_image : string -> declaration
(** [background_image value] sets the CSS background-image property to value. *)

val animation : string -> declaration
(** [animation value] sets the CSS animation property to value. *)

val appearance : string -> declaration
(** [appearance value] sets the CSS appearance property to value. *)

val overflow_x : string -> declaration
(** [overflow_x value] sets the CSS overflow-x property to value. *)

val overflow_y : string -> declaration
(** [overflow_y value] sets the CSS overflow-y property to value. *)

val resize : string -> declaration
(** [resize value] sets the CSS resize property to value. *)

val vertical_align : string -> declaration
(** [vertical_align value] sets the CSS vertical-align property to value. *)

val box_sizing : string -> declaration
(** [box_sizing value] sets the CSS box-sizing property to value. *)

val font_family : string -> declaration
(** [font_family value] sets the CSS font-family property to value. *)

val background_position : string -> declaration
(** [background_position value] sets the CSS background-position property to
    value. *)

val background_repeat : string -> declaration
(** [background_repeat value] sets the CSS background-repeat property to value.
*)

val background_size : string -> declaration
(** [background_size value] sets the CSS background-size property to value. *)

val webkit_font_smoothing : string -> declaration
(** [webkit_font_smoothing value] sets the CSS -webkit-font-smoothing property
    to value. *)

val moz_osx_font_smoothing : string -> declaration
(** [moz_osx_font_smoothing value] sets the CSS -moz-osx-font-smoothing property
    to value. *)

val webkit_line_clamp : string -> declaration
(** [webkit_line_clamp value] sets the CSS -webkit-line-clamp property to value.
*)

val backdrop_filter : string -> declaration
(** [backdrop_filter value] sets the CSS backdrop-filter property to value. *)

val scroll_snap_type : string -> declaration
(** [scroll_snap_type value] sets the CSS scroll-snap-type property to value. *)

val scroll_snap_align : string -> declaration
(** [scroll_snap_align value] sets the CSS scroll-snap-align property to value.
*)

val scroll_snap_stop : string -> declaration
(** [scroll_snap_stop value] sets the CSS scroll-snap-stop property to value. *)

val scroll_behavior : string -> declaration
(** [scroll_behavior value] sets the CSS scroll-behavior property to value. *)

val declaration : string -> string -> declaration
(** [declaration name value] creates a custom CSS declaration. *)

val rule : selector:string -> declaration list -> rule
(** [rule ~selector declarations] creates a CSS rule with a selector and
    declarations. *)

val selector : rule -> string
(** [selector rule] returns the selector of a CSS rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the declarations of a CSS rule. *)

val rule_to_nested : rule -> nested_rule
(** [rule_to_nested rule] converts a rule to a nested_rule. *)

val supports_to_nested : supports_query -> nested_rule
(** [supports_to_nested supports] converts a supports query to a nested_rule. *)

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
  nested_rule list ->
  layered_rules
(** [layered_rules ~layer ?media_queries ?container_queries ?supports_queries
     rules] creates rules within a specific CSS layer with optional nested
    at-rules. *)

type sheet_item =
  | Rule of rule
  | Media of media_query
  | Container of container_query
  | Starting_style of starting_style
  | Supports of supports_query
  | At_property of at_property
  | Layer of layered_rules  (** Items that can be added to a stylesheet *)

val empty : t
(** [empty] is an empty stylesheet. *)

val concat : t list -> t
(** [concat stylesheets] concatenates multiple stylesheets into one. Rules are
    combined in order, with later stylesheets taking precedence. *)

val stylesheet : sheet_item list -> t
(** [stylesheet items] creates a stylesheet from a list of items. *)

(** {1 Rendering} *)

val to_string : ?minify:bool -> t -> string
(** [to_string ?minify stylesheet] renders a complete stylesheet to CSS. If
    [minify] is [true], the output will be minified (no unnecessary whitespace).
    Default is [false]. *)

val string_of_property : property -> string
(** [string_of_property prop] converts a property to its CSS string
    representation. *)

val declaration_value : declaration -> string
(** [declaration_value decl] extracts the value from a declaration. *)

val declaration_property : declaration -> property
(** [declaration_property decl] extracts the property from a declaration. *)

val is_custom_property : declaration -> bool
(** [is_custom_property decl] returns true if the declaration is a CSS custom
    property (starts with --). *)

(** {1 Utilities} *)

val all_vars : declaration list -> string list
(** [all_vars declarations] extracts all CSS variable names referenced in
    declaration values, returning them sorted and deduplicated. *)

val deduplicate_declarations : declaration list -> declaration list
(** [deduplicate_declarations declarations] removes duplicate declarations,
    keeping the last occurrence. *)

val inline_style_of_declarations : declaration list -> string
(** [inline_style_of_declarations declarations] converts a list of declarations
    to an inline style string. *)

(** {1 Version Information} *)

val version : string
(** The version string of the library. *)

val header : string
(** CSS comment header with version information. *)
