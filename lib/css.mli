(** Type-safe CSS generation library.

    This library provides OCaml types to represent CSS declarations, rules, and
    stylesheets, preventing common errors through a structured, typed approach
    instead of raw strings.

    {b Core Concepts:}
    - A {!type:declaration} is a single property-value pair (e.g., [color: red])
    - A {!type:rule} combines a selector with a list of declarations
    - A {!type:t} (stylesheet) is a collection of rules and other top-level
      items
    - Typed values like {!type:length} and {!type:color} ensure valid CSS

    {b Example:}
    {[
      open Css

      let my_stylesheet =
        stylesheet
          [
            Rule
              (rule ~selector:".button"
                 [
                   background_color (Hex "3b82f6");
                   color (Hex "ffffff");
                   padding (Rem 0.5);
                   border_radius "0.375rem";
                   font_weight Bold;
                   display Inline_block;
                 ]);
            Media
              (media ~condition:"(min-width: 768px)"
                 [ rule ~selector:".button" [ padding (Rem 1.0) ] ]);
          ]

      let () = print_endline (to_string my_stylesheet)
    ]}

    {b CSS Custom Properties:}
    {[
      (* Define CSS variables *)
      let theme_vars =
        rule ~selector:":root"
          [
            custom_property "--primary-color" "#3b82f6";
            custom_property "--text-color" "#1f2937";
            custom_property "--spacing" "1rem";
          ]

      (* Use CSS variables *)
      let component =
        rule ~selector:".card"
          [
            background_color (Var "primary-color");
            color (Var "text-color");
            padding (Var "spacing");
          ]
    ]}

    @see <https://www.w3.org/Style/CSS/specs.en.html> W3C CSS Specifications
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS>
      MDN CSS Documentation *)

(** {1 Types} *)

(** CSS variable requirements. *)

type 'a var = {
  name : string;  (** Variable name (without --) *)
  fallback : 'a var_fallback option;  (** Optional fallback value *)
}
(** CSS variable reference *)

and 'a var_fallback =
  | Var of 'a var  (** Another variable as fallback *)
  | Value of 'a  (** Direct value as fallback *)

(** CSS calc operations. *)
type calc_op = Add | Sub | Mult | Div

(** CSS calc values. *)
type 'a calc =
  | Var of 'a calc var (* CSS variable *)
  | Val of 'a
  | Expr of 'a calc * calc_op * 'a calc

(** CSS length values. *)
type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Ch of float (* Character units *)
  | Lh of float (* Line height units *)
  | Num of float (* Unitless numbers, e.g., line-height multipliers *)
  | Auto
  | Zero
  | Inherit
  | Fit_content (* fit-content keyword *)
  | Max_content (* max-content keyword *)
  | Min_content (* min-content keyword *)
  | Var of length var (* CSS variable reference *)
  | Calc of length calc (* Calculated expressions *)

(** Builder functions for calc() expressions. *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  (** [add left right] creates [left + right] *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub left right] creates [left - right] *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul left right] creates [left * right] *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div left right] creates [left / right] *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [(+)] is {!add} *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [(-)] is {!sub} *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [( * )] is {!mul} *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [(/)] is {!div} *)

  val length : length -> length calc
  (** [length len] lifts a length value into calc *)

  val var : string -> 'a calc
  (** [var name] creates a variable reference for calc expressions. Example:
      [var "spacing"] *)

  val float : float -> length calc
  (** [float f] creates a numeric value for calc expressions *)

  val px : int -> length calc
  (** [px n] creates a pixel value for calc expressions *)

  val rem : float -> length calc
  (** [rem f] creates a rem value for calc expressions *)

  val em : float -> length calc
  (** [em f] creates an em value for calc expressions *)

  val pct : float -> length calc
  (** [pct f] creates a percentage value for calc expressions *)
end

(** CSS color spaces for color-mix() *)
type color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

(** CSS color values. *)
type color =
  | Hex of string
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }  (** OKLCH color space *)
  | Var of string
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space;  (** Color space for mixing *)
      color1 : color;  (** First color *)
      percent1 : int option;  (** Optional percentage for first color *)
      color2 : color;  (** Second color *)
      percent2 : int option;  (** Optional percentage for second color *)
    }

(** CSS display values. *)
type display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | None
  | Table
  | Table_row
  | Table_cell
  | List_item

(** CSS position values. *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS font weight values. *)
type font_weight =
  | Weight of int
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Var of font_weight var (* CSS variable reference *)

(** CSS text align values. *)
type text_align = Left | Right | Center | Justify | Start | End | Inherit

(** CSS overflow values. *)
type overflow = Visible | Hidden | Scroll | Auto | Clip

(** CSS flex direction values. *)
type flex_direction = Row | Row_reverse | Column | Column_reverse

(** CSS align/justify values. *)
type align =
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Start
  | End
  | Baseline
  | Auto

(** CSS text decoration values. *)
type text_decoration =
  | None
  | Underline
  | Overline
  | Line_through
  | Inherit
  | Underline_dotted (* underline dotted *)

(** CSS font style values. *)
type font_style = Font_normal | Italic | Oblique | Font_inherit

(** CSS list style type values. *)
type list_style_type =
  | None
  | Disc
  | Circle
  | Square
  | Decimal
  | Lower_alpha
  | Upper_alpha
  | Lower_roman
  | Upper_roman

(** CSS border style values. *)
type border_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Hidden
  | Var of border_style var (* CSS variable reference *)

(** CSS cursor values. *)
type cursor =
  | Auto
  | Default
  | Pointer
  | Wait
  | Text
  | Move
  | Help
  | Not_allowed
  | None
  | Context_menu
  | Progress
  | Cell
  | Crosshair
  | Vertical_text
  | Alias
  | Copy
  | No_drop
  | Grab
  | Grabbing
  | All_scroll
  | Col_resize
  | Row_resize
  | N_resize
  | E_resize
  | S_resize
  | W_resize
  | Ne_resize
  | Nw_resize
  | Se_resize
  | Sw_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Zoom_in
  | Zoom_out

(** CSS user-select values. *)
type user_select = None | Auto | Text | All | Contain

(** CSS flex wrap values. *)
type flex_wrap = Nowrap | Wrap | Wrap_reverse

(** CSS text transform values. *)
type text_transform =
  | None
  | Uppercase
  | Lowercase
  | Capitalize
  | Full_width
  | Full_size_kana
  | Inherit

(** CSS box sizing values. *)
type box_sizing = Border_box | Content_box | Inherit

(** CSS white space values. *)
type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Inherit

(** CSS table layout values. *)
type table_layout = Auto | Fixed | Inherit

(** CSS resize values. *)
type resize_value =
  | None
  | Both
  | Horizontal
  | Vertical
  | Block
  | Inline
  | Inherit

(** CSS object fit values. *)
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit

(** CSS appearance values. *)
type appearance_value = None | Auto | Button | Textfield | Menulist | Inherit

(** CSS vertical-align values. *)
type vertical_align_value =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Length of length
  | Percentage of float
  | Inherit

(** CSS border-collapse values. *)
type border_collapse_value = Collapse | Separate | Inherit

(** CSS pointer-events values. *)
type pointer_events_value =
  | Auto
  | None
  | Visible_painted
  | Visible_fill
  | Visible_stroke
  | Visible
  | Painted
  | Fill
  | Stroke
  | All
  | Inherit

(** CSS flex shorthand values. *)
type flex_value =
  | Initial (* 0 1 auto *)
  | Auto (* 1 1 auto *)
  | None (* 0 0 auto *)
  | Grow of float (* grow 1 0% *)
  | Basis of length (* 1 1 basis *)
  | Grow_shrink of float * float (* grow shrink 0% *)
  | Full of float * float * length (* grow shrink basis *)

(** CSS duration values. *)
type duration =
  | Ms of int
  (* milliseconds *)
  | S of float (* seconds *)

(** CSS timing function values. *)
type timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * [ `Start | `End ]
  | Cubic_bezier of float * float * float * float

(** CSS transition property values. *)
type transition_property = All | None | Property of string

(** CSS transition values. *)
type transition_value =
  | Simple of transition_property * duration
  | With_timing of transition_property * duration * timing_function
  | With_delay of transition_property * duration * timing_function * duration
  | Multiple of transition_value list

(** CSS grid track sizing. *)
type grid_track_size =
  | Fr of float (* Fraction units *)
  | Min_max of length * grid_track_size (* minmax(min, max) *)
  | Grid_auto (* auto *)
  | Max_content (* max-content *)
  | Min_content (* min-content *)
  | Fit_content of length (* fit-content(limit) *)
  | Grid_length of length (* Any length value *)

(** CSS grid template values. *)
type grid_template =
  | Tracks of grid_track_size list (* List of track sizes *)
  | Repeat of int * grid_track_size (* repeat(count, size) *)
  | Repeat_auto_fill of grid_track_size (* repeat(auto-fill, size) *)
  | Repeat_auto_fit of grid_track_size (* repeat(auto-fit, size) *)
  | Grid_none (* none *)
  | Grid_inherit (* inherit *)

(** CSS transform angle values. *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Angle_var of { var_name : string; fallback : float option }
(* CSS variable with optional fallback *)

(** CSS transform scale values. *)
type scale_value =
  | Scale_num of float
  | Scale_var of { var_name : string; fallback : float option }
(* CSS variable with optional fallback *)

(** CSS transform values. *)
type transform_value =
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate of length * length
  | Translate_var of { var_name : string; fallback : string option }
    (* var name and optional fallback *)
  | Translate3d of length * length * length
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate of angle
  | Rotate_var of { var_name : string; fallback : string option }
    (* var name and optional fallback *)
  | Rotate3d of float * float * float * angle
  | Scale_x of scale_value
  | Scale_y of scale_value
  | Scale_z of scale_value
  | Scale of scale_value
  | Scale2 of scale_value * scale_value
  | Scale3d of scale_value * scale_value * scale_value
  | Skew_x of angle
  | Skew_y of angle
  | Skew_var of { var_name : string; fallback : string option }
    (* var name and optional fallback *)
  | Skew of angle * angle
  | Matrix of float * float * float * float * float * float
  | Matrix3d of
      float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
  | Perspective of length
  | Transform_none

type property
(** Abstract type for CSS property names. *)

type declaration
(** Abstract type for CSS declarations (property-value pairs). *)

val important : declaration -> declaration
(** [important decl] marks a declaration as !important. *)

type rule
(** Abstract type for CSS rules. *)

type nested_rule
(** Abstract type for rules or nested content. *)

type media_query
(** Abstract type for media queries. *)

type container_query
(** Abstract type for container queries. *)

type starting_style
(** Abstract type for [@starting-style] rules. *)

type supports_query
(** Abstract type for [@supports] rules. *)

type layer =
  | Properties
  | Theme
  | Base
  | Components
  | Utilities  (** CSS layer types for Tailwind v4. *)

type layered_rules
(** Abstract type for rules within a layer. *)

type t
(** Abstract type for CSS stylesheets. *)

val pp : ?minify:bool -> t -> string
(** [pp stylesheet] is [to_string]. We don't use Format to have an efficient
    js_of_ocaml bundle. *)

type at_property
(** Abstract type for [@property] rules *)

(** CSS text-decoration-style values. *)
type text_decoration_style_value =
  | Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  | Inherit

(** CSS webkit-font-smoothing values. *)
type webkit_font_smoothing_value =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS -moz-osx-font-smoothing values. *)
type moz_osx_font_smoothing_value = Auto | Grayscale | Inherit

(** CSS transform-style values. *)
type transform_style_value = Flat | Preserve_3d | Inherit

(** CSS backface-visibility values. *)
type backface_visibility_value = Visible | Hidden | Inherit

(** CSS scroll-behavior values. *)
type scroll_behavior_value = Auto | Smooth | Inherit

(** CSS scroll-snap-type values. *)
type scroll_snap_type_value =
  | None
  | X_mandatory
  | Y_mandatory
  | Block_mandatory
  | Inline_mandatory
  | Both_mandatory
  | X_proximity
  | Y_proximity
  | Block_proximity
  | Inline_proximity
  | Both_proximity
  | X_var of string (* For "x var(--tw-scroll-snap-strictness)" *)
  | Y_var of string (* For "y var(--tw-scroll-snap-strictness)" *)
  | Both_var of string (* For "both var(--tw-scroll-snap-strictness)" *)
  | Inherit

(** CSS scroll-snap-align values. *)
type scroll_snap_align_value = None | Start | End | Center | Inherit

(** CSS scroll-snap-stop values. *)
type scroll_snap_stop_value = Normal | Always | Inherit

(** CSS isolation values. *)
type isolation_value = Auto | Isolate | Inherit

(** CSS background-repeat values. *)
type background_repeat_value =
  | Repeat
  | Repeat_x
  | Repeat_y
  | No_repeat
  | Space
  | Round
  | Inherit

(** CSS container-type values. *)
type container_type_value = Normal | Size | Inline_size | Inherit

(** {1 Declaration Constructors} *)

(** {2 Modular Property Groups} *)

(** Flex layout properties. *)
module Flex : sig
  val direction : flex_direction -> declaration
  (** [direction dir] sets the flex-direction property. *)

  val wrap : flex_wrap -> declaration
  (** [wrap w] sets the flex-wrap property. *)

  val flex : flex_value -> declaration
  (** [flex value] sets the flex shorthand property. *)

  val grow : float -> declaration
  (** [grow n] sets the flex-grow property. *)

  val shrink : float -> declaration
  (** [shrink n] sets the flex-shrink property. *)

  val basis : length -> declaration
  (** [basis len] sets the flex-basis property. *)

  val order : int -> declaration
  (** [order n] sets the order property. *)

  val align_items :
    [ `Flex_start | `Flex_end | `Center | `Baseline | `Stretch ] -> declaration
  (** [align_items value] sets the align-items property. *)

  val align_self :
    [ `Auto | `Flex_start | `Flex_end | `Center | `Baseline | `Stretch ] ->
    declaration
  (** [align_self value] sets the align-self property. *)

  val justify_content :
    [ `Flex_start
    | `Flex_end
    | `Center
    | `Space_between
    | `Space_around
    | `Space_evenly ] ->
    declaration
  (** [justify_content value] sets the justify-content property. *)

  val gap : length -> declaration
  (** [gap len] sets the gap property for flex containers. *)
end

(** Grid layout properties. *)
module Grid : sig
  val template_columns : grid_template -> declaration
  (** [template_columns template] sets the grid-template-columns property. *)

  val template_rows : grid_template -> declaration
  (** [template_rows template] sets the grid-template-rows property. *)

  val column_gap : length -> declaration
  (** [column_gap len] sets the column-gap property. *)

  val row_gap : length -> declaration
  (** [row_gap len] sets the row-gap property. *)

  val gap : length -> declaration
  (** [gap len] sets the gap property for grid containers. *)

  val auto_flow : [ `Row | `Column | `Row_dense | `Column_dense ] -> declaration
  (** [auto_flow flow] sets the grid-auto-flow property. *)

  val auto_columns : grid_track_size -> declaration
  (** [auto_columns size] sets the grid-auto-columns property. *)

  val auto_rows : grid_track_size -> declaration
  (** [auto_rows size] sets the grid-auto-rows property. *)

  val column : string -> declaration
  (** [column value] sets the grid-column shorthand property. *)

  val row : string -> declaration
  (** [row value] sets the grid-row shorthand property. *)
end

(** Border properties. *)
module Border : sig
  val width : length -> declaration
  (** [width len] sets the border-width property. *)

  val style : border_style -> declaration
  (** [style s] sets the border-style property. *)

  val color : color -> declaration
  (** [color c] sets the border-color property. *)

  val radius : length -> declaration
  (** [radius len] sets the border-radius property. *)

  val top_width : length -> declaration
  (** [top_width len] sets the border-top-width property. *)

  val right_width : length -> declaration
  (** [right_width len] sets the border-right-width property. *)

  val bottom_width : length -> declaration
  (** [bottom_width len] sets the border-bottom-width property. *)

  val left_width : length -> declaration
  (** [left_width len] sets the border-left-width property. *)

  val top_color : color -> declaration
  (** [top_color c] sets the border-top-color property. *)

  val right_color : color -> declaration
  (** [right_color c] sets the border-right-color property. *)

  val bottom_color : color -> declaration
  (** [bottom_color c] sets the border-bottom-color property. *)

  val left_color : color -> declaration
  (** [left_color c] sets the border-left-color property. *)
end

(** {2 CSS Custom Properties (Variables)} *)

val var : ?fallback:'a var_fallback -> string -> 'a var
(** [var ?fallback name] creates a CSS variable reference. Example:
    - [var "spacing"] creates [var(--spacing)]
    - [var ~fallback:(Var (var "default")) "custom"] creates
      [var(--custom, var(--default))]
    - [var ~fallback:(Value (Px 10)) "spacing"] creates [var(--spacing, 10px)].
*)

val custom_property : string -> string -> declaration
(** [custom_property name value] creates a CSS custom property declaration. The
    name must start with "--". Example:
    [custom_property "--color-primary" "#3b82f6"]
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/--*>
      MDN: CSS Custom Properties. *)

(** {2 Typed Constructors} *)

val background_color : color -> declaration
(** [background_color color] sets the CSS background-color property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-color>
      MDN: background-color. *)

val background_image : string -> declaration
(** [background_image value] sets the CSS background-image property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-image>
      MDN: background-image. *)

val color : color -> declaration
(** [color c] sets the CSS color property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/color> MDN: color. *)

val padding : length -> declaration
(** [padding len] sets the CSS padding property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding>
      MDN: padding. *)

val padding_left : length -> declaration
(** [padding_left len] sets the CSS padding-left property. *)

val padding_right : length -> declaration
(** [padding_right len] sets the CSS padding-right property. *)

val padding_bottom : length -> declaration
(** [padding_bottom len] sets the CSS padding-bottom property. *)

val padding_top : length -> declaration
(** [padding_top len] sets the CSS padding-top property. *)

val margin : length -> declaration
(** [margin len] sets the CSS margin property. *)

val margin_left : length -> declaration
(** [margin_left len] sets the CSS margin-left property. *)

val margin_right : length -> declaration
(** [margin_right len] sets the CSS margin-right property. *)

val margin_top : length -> declaration
(** [margin_top len] sets the CSS margin-top property. *)

val margin_bottom : length -> declaration
(** [margin_bottom len] sets the CSS margin-bottom property. *)

val width : length -> declaration
(** [width len] sets the CSS width property. *)

val height : length -> declaration
(** [height len] sets the CSS height property. *)

val display : display -> declaration
(** [display d] sets the CSS display property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/display>
      MDN: display. *)

val position : position -> declaration
(** [position p] sets the CSS position property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/position>
      MDN: position. *)

val top : length -> declaration
(** [top value] sets the CSS top property. *)

val right : length -> declaration
(** [right value] sets the CSS right property. *)

val bottom : length -> declaration
(** [bottom value] sets the CSS bottom property. *)

val left : length -> declaration
(** [left value] sets the CSS left property. *)

val font_weight : font_weight -> declaration
(** [font_weight w] sets the CSS font-weight property. *)

val text_align : text_align -> declaration
(** [text_align a] sets the CSS text-align property. *)

val overflow : overflow -> declaration
(** [overflow o] sets the CSS overflow property. *)

val overflow_x : overflow -> declaration
(** [overflow_x o] sets the CSS overflow-x property. *)

val overflow_y : overflow -> declaration
(** [overflow_y o] sets the CSS overflow-y property. *)

val opacity : float -> declaration
(** [opacity value] sets the CSS opacity property. *)

val resize : resize_value -> declaration
(** [resize value] sets the CSS resize property. *)

val align_items : align -> declaration
(** [align_items a] sets the CSS align-items property. *)

val align_content : align -> declaration
(** [align_content a] sets the CSS align-content property. *)

val align_self : align -> declaration
(** [align_self a] sets the CSS align-self property. *)

val justify_content : align -> declaration
(** [justify_content a] sets the CSS justify-content property. *)

val justify_self : align -> declaration
(** [justify_self a] sets the CSS justify-self property. *)

val place_content : string -> declaration
(** [place_content value] sets the CSS place-content property. *)

val place_items : string -> declaration
(** [place_items value] sets the CSS place-items property. *)

val place_self : string -> declaration
(** [place_self value] sets the CSS place-self property. *)

val gap : length -> declaration
(** [gap len] sets the CSS gap property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/gap> MDN: gap. *)

val column_gap : length -> declaration
(** [column_gap len] sets the CSS column-gap property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap>
      MDN: column-gap. *)

val row_gap : length -> declaration
(** [row_gap len] sets the CSS row-gap property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap>
      MDN: row-gap. *)

val min_width : length -> declaration
(** [min_width len] sets the CSS min-width property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/min-width>
      MDN: min-width. *)

val min_height : length -> declaration
(** [min_height len] sets the CSS min-height property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/min-height>
      MDN: min-height. *)

val max_width : length -> declaration
(** [max_width len] sets the CSS max-width property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/max-width>
      MDN: max-width. *)

val max_height : length -> declaration
(** [max_height len] sets the CSS max-height property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/max-height>
      MDN: max-height. *)

val font_size : length -> declaration
(** [font_size len] sets the CSS font-size property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-size>
      MDN: font-size. *)

val line_height : length -> declaration
(** [line_height len] sets the CSS line-height property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/line-height>
      MDN: line-height. *)

val border_radius : length -> declaration
(** [border_radius len] sets the CSS border-radius property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius>
      MDN: border-radius. *)

val border_width : length -> declaration
(** [border_width len] sets the CSS border-width property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-width>
      MDN: border-width. *)

val border_color : color -> declaration
(** [border_color c] sets the CSS border-color property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-color>
      MDN: border-color. *)

val border_style : border_style -> declaration
(** [border_style s] sets the CSS border-style property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-style>
      MDN: border-style. *)

val text_decoration : text_decoration -> declaration
(** [text_decoration td] sets the CSS text-decoration property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration>
      MDN: text-decoration. *)

val text_transform : text_transform -> declaration
(** [text_transform value] sets the CSS text-transform property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform>
      MDN: text-transform. *)

val text_decoration_style : text_decoration_style_value -> declaration
(** [text_decoration_style value] sets the CSS text-decoration-style property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style>
      MDN: text-decoration-style. *)

val text_underline_offset : string -> declaration
(** [text_underline_offset value] sets the CSS text-underline-offset property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset>
      MDN: text-underline-offset. *)

val font_style : font_style -> declaration
(** [font_style fs] sets the CSS font-style property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-style>
      MDN: font-style. *)

val list_style_type : list_style_type -> declaration
(** [list_style_type lst] sets the CSS list-style-type property.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type>
      MDN: list-style-type. *)

(** {2 Additional Properties} *)

val content : string -> declaration
(** [content value] sets the CSS content property. *)

val border_left_width : length -> declaration
(** [border_left_width len] sets the CSS border-left-width property. *)

val border_bottom_width : length -> declaration
(** [border_bottom_width len] sets the CSS border-bottom-width property. *)

val border_top_width : length -> declaration
(** [border_top_width len] sets the CSS border-top-width property. *)

val border_right_width : length -> declaration
(** [border_right_width len] sets the CSS border-right-width property. *)

val border_left_color : color -> declaration
(** [border_left_color c] sets the CSS border-left-color property. *)

val border_bottom_color : color -> declaration
(** [border_bottom_color c] sets the CSS border-bottom-color property. *)

val transition : transition_value -> declaration
(** [transition value] sets the CSS transition property. *)

val quotes : string -> declaration
(** [quotes value] sets the CSS quotes property. *)

type font_family_value =
  (* Generic CSS font families *)
  | Sans_serif
  | Serif
  | Monospace
  | Cursive
  | Fantasy
  | System_ui
  | Ui_sans_serif
  | Ui_serif
  | Ui_monospace
  | Ui_rounded
  | Emoji
  | Math
  | Fangsong
  (* Popular web fonts *)
  | Inter
  | Roboto
  | Open_sans
  | Lato
  | Montserrat
  | Poppins
  | Source_sans_pro
  | Raleway
  | Oswald
  | Noto_sans
  | Ubuntu
  | Playfair_display
  | Merriweather
  | Lora
  | PT_sans
  | PT_serif
  | Nunito
  | Nunito_sans
  | Work_sans
  | Rubik
  | Fira_sans
  | Fira_code
  | JetBrains_mono
  | IBM_plex_sans
  | IBM_plex_serif
  | IBM_plex_mono
  | Source_code_pro
  | Space_mono
  | DM_sans
  | DM_serif_display
  | Bebas_neue
  | Barlow
  | Mulish
  | Josefin_sans
  (* Platform-specific fonts *)
  | Helvetica
  | Helvetica_neue
  | Arial
  | Verdana
  | Tahoma
  | Trebuchet_ms
  | Times_new_roman
  | Georgia
  | Garamond
  | Courier_new
  | Courier
  | Lucida_console
  | SF_pro
  | SF_pro_display
  | SF_pro_text
  | SF_mono
  | NY
  | Segoe_ui
  | Segoe_ui_emoji
  | Segoe_ui_symbol
  | Apple_color_emoji
  | Noto_color_emoji
  | Android_emoji
  | Twemoji_mozilla
  (* Developer fonts *)
  | Menlo
  | Monaco
  | Consolas
  | Liberation_mono
  | SFMono_regular
  | Cascadia_code
  | Cascadia_mono
  | Victor_mono
  | Inconsolata
  | Hack
  (* CSS keywords *)
  | Inherit
  | Initial
  | Unset
  (* CSS variables *)
  | Var of { name : string; fallback : font_family_value list option }

val font_family : font_family_value list -> declaration
(** [font_family value] sets the CSS font-family property. *)

val table_layout : table_layout -> declaration
(** [table_layout value] sets the CSS table-layout property. *)

val vertical_align : vertical_align_value -> declaration
(** [vertical_align value] sets the CSS vertical-align property. *)

val box_sizing : box_sizing -> declaration
(** [box_sizing value] sets the CSS box-sizing property. *)

val box_shadow : string -> declaration
(** [box_shadow value] sets the CSS box-shadow property. *)

val appearance : appearance_value -> declaration
(** [appearance value] sets the CSS appearance property. *)

(** {2 Additional CSS Properties} *)

val border : string -> declaration
(** [border value] sets the CSS border shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border> MDN: border.
*)

val tab_size : string -> declaration
(** [tab_size value] sets the CSS tab-size property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size>
      MDN: tab-size. *)

val webkit_text_size_adjust : string -> declaration
(** [webkit_text_size_adjust value] sets the -webkit-text-size-adjust property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust>
      MDN: text-size-adjust. *)

val font_feature_settings : string -> declaration
(** [font_feature_settings value] sets the CSS font-feature-settings property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings>
      MDN: font-feature-settings. *)

val font_variation_settings : string -> declaration
(** [font_variation_settings value] sets the CSS font-variation-settings
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings>
      MDN: font-variation-settings. *)

val webkit_tap_highlight_color : string -> declaration
(** [webkit_tap_highlight_color value] sets the -webkit-tap-highlight-color
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-tap-highlight-color>
      MDN: -webkit-tap-highlight-color. *)

val webkit_text_decoration : string -> declaration
(** [webkit_text_decoration value] sets the -webkit-text-decoration property. *)

val text_indent : length -> declaration
(** [text_indent len] sets the CSS text-indent property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent>
      MDN: text-indent. *)

val border_collapse : border_collapse_value -> declaration
(** [border_collapse value] sets the CSS border-collapse property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse>
      MDN: border-collapse. *)

val border_spacing : length -> declaration
(** [border_spacing len] sets the CSS border-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing>
      MDN: border-spacing. *)

val list_style : string -> declaration
(** [list_style value] sets the CSS list-style shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/list-style>
      MDN: list-style. *)

val font : string -> declaration
(** [font value] sets the CSS font shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/font> MDN: font. *)

val letter_spacing : length -> declaration
(** [letter_spacing len] sets the CSS letter-spacing property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing>
      MDN: letter-spacing. *)

val webkit_appearance : string -> declaration
(** [webkit_appearance value] sets the -webkit-appearance property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/appearance>
      MDN: appearance. *)

val webkit_font_smoothing : webkit_font_smoothing_value -> declaration
(** [webkit_font_smoothing value] sets the -webkit-font-smoothing property. *)

val moz_osx_font_smoothing : moz_osx_font_smoothing_value -> declaration
(** [moz_osx_font_smoothing value] sets the -moz-osx-font-smoothing property. *)

val webkit_line_clamp : string -> declaration
(** [webkit_line_clamp value] sets the -webkit-line-clamp property. *)

val cursor : cursor -> declaration
(** [cursor c] sets the CSS cursor property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/cursor> MDN: cursor.
*)

val user_select : user_select -> declaration
(** [user_select u] sets the CSS user-select property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/user-select>
      MDN: user-select. *)

val container_type : container_type_value -> declaration
(** [container_type value] sets the CSS container-type property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/container-type>
      MDN: container-type. *)

val container_name : string -> declaration
(** [container_name value] sets the CSS container-name property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/container-name>
      MDN: container-name. *)

val perspective : length -> declaration
(** [perspective len] sets the CSS perspective property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/perspective>
      MDN: perspective. *)

val perspective_origin : string -> declaration
(** [perspective_origin value] sets the CSS perspective-origin property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin>
      MDN: perspective-origin. *)

val transform_style : transform_style_value -> declaration
(** [transform_style value] sets the CSS transform-style property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style>
      MDN: transform-style. *)

val backface_visibility : backface_visibility_value -> declaration
(** [backface_visibility value] sets the CSS backface-visibility property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility>
      MDN: backface-visibility. *)

val object_position : string -> declaration
(** [object_position value] sets the CSS object-position property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/object-position>
      MDN: object-position. *)

val object_fit : object_fit -> declaration
(** [object_fit value] sets the CSS object-fit property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit>
      MDN: object-fit. *)

val rotate : angle -> declaration
(** [rotate a] sets the CSS rotate property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/rotate> MDN: rotate.
*)

val transform : transform_value list -> declaration
(** [transform values] sets the CSS transform property with a list of
    transformations.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transform>
      MDN: transform. *)

val transition_duration : duration -> declaration
(** [transition_duration value] sets the CSS transition-duration property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration>
      MDN: transition-duration. *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function value] sets the CSS transition-timing-function
    property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function>
      MDN: transition-timing-function. *)

val will_change : string -> declaration
(** [will_change value] sets the CSS will-change property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/will-change>
      MDN: will-change. *)

val contain : string -> declaration
(** [contain value] sets the CSS contain property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/contain>
      MDN: contain. *)

val isolation : isolation_value -> declaration
(** [isolation value] sets the CSS isolation property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/isolation>
      MDN: isolation. *)

val padding_inline_start : length -> declaration
(** [padding_inline_start value] sets the CSS padding-inline-start property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start>
      MDN: padding-inline-start. *)

val padding_inline : length -> declaration
(** [padding_inline value] sets the CSS padding-inline shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline>
      MDN: padding-inline. *)

val padding_block : length -> declaration
(** [padding_block value] sets the CSS padding-block shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block>
      MDN: padding-block. *)

val margin_inline : length -> declaration
(** [margin_inline len] sets the CSS margin-inline property with a length value.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline>
      MDN: margin-inline. *)

val margin_block : length -> declaration
(** [margin_block len] sets the CSS margin-block property with a length value.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block>
      MDN: margin-block. *)

val margin_inline_end : length -> declaration
(** [margin_inline_end len] sets the CSS margin-inline-end property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end>
      MDN: margin-inline-end. *)

val outline : string -> declaration
(** [outline value] sets the CSS outline shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline>
      MDN: outline. *)

val outline_offset : length -> declaration
(** [outline_offset len] sets the CSS outline-offset property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset>
      MDN: outline-offset. *)

val white_space : white_space -> declaration
(** [white_space value] sets the CSS white-space property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/white-space>
      MDN: white-space. *)

val clip : string -> declaration
(** [clip value] sets the CSS clip property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/clip> MDN: clip. *)

val scroll_snap_type : scroll_snap_type_value -> declaration
(** [scroll_snap_type value] sets the CSS scroll-snap-type property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-type>
      MDN: scroll-snap-type. *)

val scroll_snap_align : scroll_snap_align_value -> declaration
(** [scroll_snap_align value] sets the CSS scroll-snap-align property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-align>
      MDN: scroll-snap-align. *)

val scroll_snap_stop : scroll_snap_stop_value -> declaration
(** [scroll_snap_stop value] sets the CSS scroll-snap-stop property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-stop>
      MDN: scroll-snap-stop. *)

val scroll_behavior : scroll_behavior_value -> declaration
(** [scroll_behavior value] sets the CSS scroll-behavior property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior>
      MDN: scroll-behavior. *)

val filter : string -> declaration
(** [filter value] sets the CSS filter property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/filter> MDN: filter.
*)

val backdrop_filter : string -> declaration
(** [backdrop_filter value] sets the CSS backdrop-filter property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/backdrop-filter>
      MDN: backdrop-filter. *)

val background_position : string -> declaration
(** [background_position value] sets the CSS background-position property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-position>
      MDN: background-position. *)

val background_repeat : background_repeat_value -> declaration
(** [background_repeat value] sets the CSS background-repeat property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat>
      MDN: background-repeat. *)

val background_size : string -> declaration
(** [background_size value] sets the CSS background-size property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/background-size>
      MDN: background-size. *)

val animation : string -> declaration
(** [animation value] sets the CSS animation shorthand property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/animation>
      MDN: animation. *)

val pointer_events : pointer_events_value -> declaration
(** [pointer_events value] sets the CSS pointer-events property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events>
      MDN: pointer-events. *)

val z_index : int -> declaration
(** [z_index value] sets the CSS z-index property.
    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/z-index>
      MDN: z-index. *)

val rule : selector:string -> declaration list -> rule
(** [rule ~selector declarations] creates a CSS rule with the given selector and
    list of declarations. This is the primary way to create CSS rules. *)

val selector : rule -> string
(** [selector rule] returns the selector string of a CSS rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the list of declarations in a CSS rule. *)

val rule_to_nested : rule -> nested_rule
(** [rule_to_nested rule] converts a rule to a nested_rule for use in layered or
    nested contexts. *)

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
  | Layer of layered_rules  (** Items that can be added to a stylesheet. *)

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
