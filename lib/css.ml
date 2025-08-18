(** CSS generation utilities *)

(* Simple string formatting utilities *)
let str ?(sep = "") segments = String.concat sep segments
let lines segments = str ~sep:"\n" segments

(** CSS variable types *)
type var =
  | Color of string * int option (* color name and optional shade *)
  | Spacing of int (* spacing value *)
  | Font of string (* font family *)
  | Text_size of string (* text size *)
  | Font_weight of string (* font weight *)
  | Radius of string (* border radius *)
  | Transition (* transition timing *)
  | Custom of string * string (* custom variable name and value *)

(** CSS length values *)
type calc_op = Add | Sub | Mult | Div

type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Ch of float (* Character units *)
  | Num of float (* Unitless numbers, e.g., line-height multipliers *)
  | Auto
  | Zero
  | Inherit
  | Calc of calc_value (* Calculated expressions *)

and calc_value =
  | Length of length
  | Var of var (* CSS variable *)
  | CalcNum of float
  | Expr of calc_value * calc_op * calc_value

(** CSS color values *)
type color =
  | Hex of string
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Var of string
  | Current
  | Transparent
  | Inherit

(** CSS display values *)
type display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | Display_none
  | Table
  | Table_row
  | Table_cell

(** CSS position values *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS font weight values *)
type font_weight = Weight of int | Normal | Bold | Bolder | Lighter | Inherit

(** CSS text align values *)
type text_align =
  | Left
  | Right
  | Center
  | Justify
  | Start
  | End
  | Text_align_inherit

(** CSS overflow values *)
type overflow = Visible | Hidden | Scroll | Auto | Clip

(** CSS flex direction values *)
type flex_direction = Row | Row_reverse | Column | Column_reverse

(** CSS align/justify values *)
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

(** CSS text decoration values *)
type text_decoration =
  | Text_decoration_none
  | Underline
  | Overline
  | Line_through
  | Text_decoration_inherit

(** CSS font style values *)
type font_style = Font_normal | Italic | Oblique | Font_inherit

(** CSS list style type values *)
type list_style_type =
  | List_none
  | Disc
  | Circle
  | Square
  | Decimal
  | Lower_alpha
  | Upper_alpha
  | Lower_roman
  | Upper_roman

(** CSS border style values *)
type border_style =
  | Border_none
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset

(** CSS cursor values *)
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

(** CSS user-select values *)
type user_select = None | Auto | Text | All | Contain

(** CSS grid track sizing *)
type grid_track_size =
  | Fr of float
  | MinMax of length * grid_track_size
  | GridAuto
  | MaxContent
  | MinContent
  | FitContent of length
  | GridLength of length

(** CSS grid template values *)
type grid_template =
  | Tracks of grid_track_size list
  | Repeat of int * grid_track_size
  | RepeatAutoFill of grid_track_size
  | RepeatAutoFit of grid_track_size
  | GridNone
  | GridInherit

(** CSS transform angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | AngleVar of { var_name : string; fallback : float option }

(** CSS transform scale values *)
type scale_value =
  | ScaleNum of float
  | ScaleVar of { var_name : string; fallback : float option }

(** CSS transform values *)
type transform_value =
  | TranslateX of length
  | TranslateY of length
  | TranslateZ of length
  | Translate of length * length
  | TranslateVar of { var_name : string; fallback : string option }
  | Translate3d of length * length * length
  | RotateX of angle
  | RotateY of angle
  | RotateZ of angle
  | Rotate of angle
  | RotateVar of { var_name : string; fallback : string option }
  | Rotate3d of float * float * float * angle
  | ScaleX of scale_value
  | ScaleY of scale_value
  | ScaleZ of scale_value
  | Scale of scale_value
  | Scale2 of scale_value * scale_value
  | Scale3d of scale_value * scale_value * scale_value
  | SkewX of angle
  | SkewY of angle
  | SkewVar of { var_name : string; fallback : string option }
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

(* Convert typed values to strings *)
let rec string_of_length = function
  | Px n -> string_of_int n ^ "px"
  | Rem f -> string_of_float f ^ "rem"
  | Em f -> string_of_float f ^ "em"
  | Pct f -> string_of_float f ^ "%"
  | Vw f -> string_of_float f ^ "vw"
  | Vh f -> string_of_float f ^ "vh"
  | Ch f -> string_of_float f ^ "ch"
  | Num f -> string_of_float f
  | Auto -> "auto"
  | Zero -> "0"
  | Inherit -> "inherit"
  | Calc cv -> "calc(" ^ string_of_calc_value cv ^ ")"

and string_of_calc_value = function
  | Length l -> string_of_length l
  | Var v -> "var(--" ^ string_of_var v ^ ")"
  | CalcNum f -> string_of_float f
  | Expr (left, op, right) ->
      let op_str =
        match op with
        | Add -> " + "
        | Sub -> " - "
        | Mult -> " * "
        | Div -> " / "
      in
      string_of_calc_value left ^ op_str ^ string_of_calc_value right

and string_of_var = function
  | Color (name, Some shade) -> "color-" ^ name ^ "-" ^ string_of_int shade
  | Color (name, None) -> "color-" ^ name
  | Spacing n -> "spacing-" ^ string_of_int n
  | Font family -> "font-" ^ family
  | Text_size size -> "text-" ^ size
  | Font_weight weight -> "font-weight-" ^ weight
  | Radius r -> "radius-" ^ r
  | Transition -> "transition"
  | Custom (name, _) -> name

let string_of_color = function
  | Hex s -> str [ "#"; s ]
  | Rgb { r; g; b } ->
      str
        [
          "rgb(";
          string_of_int r;
          ", ";
          string_of_int g;
          ", ";
          string_of_int b;
          ")";
        ]
  | Rgba { r; g; b; a } ->
      str
        [
          "rgba(";
          string_of_int r;
          ", ";
          string_of_int g;
          ", ";
          string_of_int b;
          ", ";
          string_of_float a;
          ")";
        ]
  | Var v -> str [ "var(--"; v; ")" ]
  | Current -> "currentColor"
  | Transparent -> "transparent"
  | Inherit -> "inherit"

let string_of_display = function
  | Block -> "block"
  | Inline -> "inline"
  | Inline_block -> "inline-block"
  | Flex -> "flex"
  | Inline_flex -> "inline-flex"
  | Grid -> "grid"
  | Inline_grid -> "inline-grid"
  | Display_none -> "none"
  | Table -> "table"
  | Table_row -> "table-row"
  | Table_cell -> "table-cell"

let string_of_position = function
  | Static -> "static"
  | Relative -> "relative"
  | Absolute -> "absolute"
  | Fixed -> "fixed"
  | Sticky -> "sticky"

let string_of_font_weight = function
  | Weight n -> string_of_int n
  | Normal -> "normal"
  | Bold -> "bold"
  | Bolder -> "bolder"
  | Lighter -> "lighter"
  | Inherit -> "inherit"

let string_of_text_align = function
  | Left -> "left"
  | Right -> "right"
  | Center -> "center"
  | Justify -> "justify"
  | Start -> "start"
  | End -> "end"
  | Text_align_inherit -> "inherit"

let string_of_overflow = function
  | Visible -> "visible"
  | Hidden -> "hidden"
  | Scroll -> "scroll"
  | Auto -> "auto"
  | Clip -> "clip"

let string_of_flex_direction = function
  | Row -> "row"
  | Row_reverse -> "row-reverse"
  | Column -> "column"
  | Column_reverse -> "column-reverse"

let string_of_align = function
  | Flex_start -> "flex-start"
  | Flex_end -> "flex-end"
  | Center -> "center"
  | Space_between -> "space-between"
  | Space_around -> "space-around"
  | Space_evenly -> "space-evenly"
  | Stretch -> "stretch"
  | Start -> "start"
  | End -> "end"
  | Baseline -> "baseline"

let string_of_text_decoration = function
  | Text_decoration_none -> "none"
  | Underline -> "underline"
  | Overline -> "overline"
  | Line_through -> "line-through"
  | Text_decoration_inherit -> "inherit"

let string_of_font_style = function
  | Font_normal -> "normal"
  | Italic -> "italic"
  | Oblique -> "oblique"
  | Font_inherit -> "inherit"

let string_of_list_style_type = function
  | List_none -> "none"
  | Disc -> "disc"
  | Circle -> "circle"
  | Square -> "square"
  | Decimal -> "decimal"
  | Lower_alpha -> "lower-alpha"
  | Upper_alpha -> "upper-alpha"
  | Lower_roman -> "lower-roman"
  | Upper_roman -> "upper-roman"

let string_of_border_style = function
  | Border_none -> "none"
  | Solid -> "solid"
  | Dashed -> "dashed"
  | Dotted -> "dotted"
  | Double -> "double"
  | Groove -> "groove"
  | Ridge -> "ridge"
  | Inset -> "inset"
  | Outset -> "outset"

let string_of_cursor : cursor -> string = function
  | Auto -> "auto"
  | Default -> "default"
  | Pointer -> "pointer"
  | Wait -> "wait"
  | Text -> "text"
  | Move -> "move"
  | Help -> "help"
  | Not_allowed -> "not-allowed"
  | None -> "none"
  | Context_menu -> "context-menu"
  | Progress -> "progress"
  | Cell -> "cell"
  | Crosshair -> "crosshair"
  | Vertical_text -> "vertical-text"
  | Alias -> "alias"
  | Copy -> "copy"
  | No_drop -> "no-drop"
  | Grab -> "grab"
  | Grabbing -> "grabbing"
  | All_scroll -> "all-scroll"
  | Col_resize -> "col-resize"
  | Row_resize -> "row-resize"
  | N_resize -> "n-resize"
  | E_resize -> "e-resize"
  | S_resize -> "s-resize"
  | W_resize -> "w-resize"
  | Ne_resize -> "ne-resize"
  | Nw_resize -> "nw-resize"
  | Se_resize -> "se-resize"
  | Sw_resize -> "sw-resize"
  | Ew_resize -> "ew-resize"
  | Ns_resize -> "ns-resize"
  | Nesw_resize -> "nesw-resize"
  | Nwse_resize -> "nwse-resize"
  | Zoom_in -> "zoom-in"
  | Zoom_out -> "zoom-out"

let string_of_user_select : user_select -> string = function
  | None -> "none"
  | Auto -> "auto"
  | Text -> "text"
  | All -> "all"
  | Contain -> "contain"

let rec string_of_grid_track_size = function
  | Fr f -> string_of_float f ^ "fr"
  | MinMax (min, max) ->
      "minmax(" ^ string_of_length min ^ ", "
      ^ string_of_grid_track_size max
      ^ ")"
  | GridAuto -> "auto"
  | MaxContent -> "max-content"
  | MinContent -> "min-content"
  | FitContent l -> "fit-content(" ^ string_of_length l ^ ")"
  | GridLength l -> string_of_length l

let string_of_grid_template = function
  | Tracks sizes -> String.concat " " (List.map string_of_grid_track_size sizes)
  | Repeat (count, size) ->
      "repeat(" ^ string_of_int count ^ ", "
      ^ string_of_grid_track_size size
      ^ ")"
  | RepeatAutoFill size ->
      "repeat(auto-fill, " ^ string_of_grid_track_size size ^ ")"
  | RepeatAutoFit size ->
      "repeat(auto-fit, " ^ string_of_grid_track_size size ^ ")"
  | GridNone -> "none"
  | GridInherit -> "inherit"

let string_of_angle = function
  | Deg f -> string_of_float f ^ "deg"
  | Rad f -> string_of_float f ^ "rad"
  | Turn f -> string_of_float f ^ "turn"
  | Grad f -> string_of_float f ^ "grad"
  | AngleVar { var_name; fallback } -> (
      match fallback with
      | None -> "var(--" ^ var_name ^ ")"
      | Some f -> "var(--" ^ var_name ^ ", " ^ string_of_float f ^ ")")

let string_of_scale_value = function
  | ScaleNum f -> string_of_float f
  | ScaleVar { var_name; fallback } -> (
      match fallback with
      | None -> "var(--" ^ var_name ^ ")"
      | Some f -> "var(--" ^ var_name ^ ", " ^ string_of_float f ^ ")")

let string_of_transform_value = function
  | TranslateX l -> "translateX(" ^ string_of_length l ^ ")"
  | TranslateY l -> "translateY(" ^ string_of_length l ^ ")"
  | TranslateZ l -> "translateZ(" ^ string_of_length l ^ ")"
  | Translate (x, y) ->
      "translate(" ^ string_of_length x ^ ", " ^ string_of_length y ^ ")"
  | TranslateVar { var_name; fallback } -> (
      match fallback with
      | None -> "translate(var(--" ^ var_name ^ "))"
      | Some fb -> "translate(var(--" ^ var_name ^ ", " ^ fb ^ "))")
  | Translate3d (x, y, z) ->
      "translate3d(" ^ string_of_length x ^ ", " ^ string_of_length y ^ ", "
      ^ string_of_length z ^ ")"
  | RotateX a -> "rotateX(" ^ string_of_angle a ^ ")"
  | RotateY a -> "rotateY(" ^ string_of_angle a ^ ")"
  | RotateZ a -> "rotateZ(" ^ string_of_angle a ^ ")"
  | Rotate a -> "rotate(" ^ string_of_angle a ^ ")"
  | RotateVar { var_name; fallback } -> (
      match fallback with
      | None -> "rotate(var(--" ^ var_name ^ "))"
      | Some fb -> "rotate(var(--" ^ var_name ^ ", " ^ fb ^ "))")
  | Rotate3d (x, y, z, angle) ->
      "rotate3d(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ", "
      ^ string_of_float z ^ ", " ^ string_of_angle angle ^ ")"
  | ScaleX s -> "scaleX(" ^ string_of_scale_value s ^ ")"
  | ScaleY s -> "scaleY(" ^ string_of_scale_value s ^ ")"
  | ScaleZ s -> "scaleZ(" ^ string_of_scale_value s ^ ")"
  | Scale s -> "scale(" ^ string_of_scale_value s ^ ")"
  | Scale2 (x, y) ->
      "scale(" ^ string_of_scale_value x ^ ", " ^ string_of_scale_value y ^ ")"
  | Scale3d (x, y, z) ->
      "scale3d(" ^ string_of_scale_value x ^ ", " ^ string_of_scale_value y
      ^ ", " ^ string_of_scale_value z ^ ")"
  | SkewX a -> "skewX(" ^ string_of_angle a ^ ")"
  | SkewY a -> "skewY(" ^ string_of_angle a ^ ")"
  | SkewVar { var_name; fallback } -> (
      match fallback with
      | None -> "skew(var(--" ^ var_name ^ "))"
      | Some fb -> "skew(var(--" ^ var_name ^ ", " ^ fb ^ "))")
  | Skew (x, y) -> "skew(" ^ string_of_angle x ^ ", " ^ string_of_angle y ^ ")"
  | Matrix (a, b, c, d, e, f) ->
      "matrix("
      ^ String.concat ", "
          [
            string_of_float a;
            string_of_float b;
            string_of_float c;
            string_of_float d;
            string_of_float e;
            string_of_float f;
          ]
      ^ ")"
  | Matrix3d
      (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16) ->
      "matrix3d("
      ^ String.concat ", "
          [
            string_of_float m1;
            string_of_float m2;
            string_of_float m3;
            string_of_float m4;
            string_of_float m5;
            string_of_float m6;
            string_of_float m7;
            string_of_float m8;
            string_of_float m9;
            string_of_float m10;
            string_of_float m11;
            string_of_float m12;
            string_of_float m13;
            string_of_float m14;
            string_of_float m15;
            string_of_float m16;
          ]
      ^ ")"
  | Perspective l -> "perspective(" ^ string_of_length l ^ ")"
  | Transform_none -> "none"

type property =
  | Background_color
  | Color
  | Border_color
  | Border_style
  | Padding
  | Padding_left
  | Padding_right
  | Padding_bottom
  | Padding_top
  | Padding_inline
  | Padding_inline_start
  | Padding_block
  | Margin
  | Margin_inline_end
  | Margin_left
  | Margin_right
  | Margin_top
  | Margin_bottom
  | Margin_inline
  | Margin_block
  | Gap
  | Column_gap
  | Row_gap
  | Width
  | Height
  | Min_width
  | Min_height
  | Max_width
  | Max_height
  | Font_size
  | Line_height
  | Font_weight
  | Font_style
  | Text_align
  | Text_decoration
  | Text_decoration_style
  | Text_underline_offset
  | Text_transform
  | Letter_spacing
  | List_style_type
  | Display
  | Position
  | Flex_direction
  | Flex_wrap
  | Flex
  | Flex_grow
  | Flex_shrink
  | Align_items
  | Justify_content
  | Align_content
  | Align_self
  | Justify_self
  | Place_content
  | Place_items
  | Place_self
  | Grid_template_columns
  | Grid_template_rows
  | Border_width
  | Border_radius
  | Box_shadow
  | Opacity
  | Transition
  | Transform
  | Cursor
  | Table_layout
  | Border_collapse
  | Border_spacing
  | User_select
  | Pointer_events
  | Overflow
  | Top
  | Right
  | Bottom
  | Left
  | Z_index
  | Border_top_width
  | Border_right_width
  | Border_bottom_width
  | Border_left_width
  | Outline
  | Outline_offset
  | Scroll_snap_type
  | Clip
  | White_space
  | Border
  | Tab_size
  | Webkit_text_size_adjust
  | Font_feature_settings
  | Font_variation_settings
  | Webkit_tap_highlight_color
  | Webkit_text_decoration
  | Text_indent
  | List_style
  | Font
  | Webkit_appearance
  | Container_type
  | Container_name
  | Perspective
  | Perspective_origin
  | Transform_style
  | Backface_visibility
  | Object_position
  | Object_fit
  | Rotate
  | Transition_duration
  | Transition_timing_function
  | Will_change
  | Contain
  | Isolation
  | Filter
  | Background_image
  | Animation
  | Appearance
  | Overflow_x
  | Overflow_y
  | Resize
  | Vertical_align
  | Box_sizing
  | Font_family
  | Background_position
  | Background_repeat
  | Background_size
  | Webkit_font_smoothing
  | Moz_osx_font_smoothing
  | Webkit_line_clamp
  | Backdrop_filter
  | Scroll_snap_align
  | Scroll_snap_stop
  | Scroll_behavior
  | Custom of string  (** CSS property names as a variant type *)

type declaration = property * string
(** A CSS property as (name, value) pair *)

(* Typed property constructors *)
let background_color c = (Background_color, string_of_color c)
let color c = (Color, string_of_color c)
let border_color c = (Border_color, string_of_color c)
let border_style bs = (Border_style, string_of_border_style bs)
let text_decoration td = (Text_decoration, string_of_text_decoration td)
let font_style fs = (Font_style, string_of_font_style fs)
let list_style_type lst = (List_style_type, string_of_list_style_type lst)
let padding len = (Padding, string_of_length len)
let padding_left len = (Padding_left, string_of_length len)
let padding_right len = (Padding_right, string_of_length len)
let padding_bottom len = (Padding_bottom, string_of_length len)
let padding_top len = (Padding_top, string_of_length len)

(* Remove deprecated string-based versions *)
let margin len = (Margin, string_of_length len)
let margin_left len = (Margin_left, string_of_length len)
let margin_right len = (Margin_right, string_of_length len)
let margin_top len = (Margin_top, string_of_length len)
let margin_bottom len = (Margin_bottom, string_of_length len)

(* Remove deprecated string-based versions *)
let gap len = (Gap, string_of_length len)
let column_gap len = (Column_gap, string_of_length len)
let row_gap len = (Row_gap, string_of_length len)
let width len = (Width, string_of_length len)
let height len = (Height, string_of_length len)

(* Remove deprecated string-based versions *)
let min_width len = (Min_width, string_of_length len)
let min_height len = (Min_height, string_of_length len)
let max_width len = (Max_width, string_of_length len)
let max_height len = (Max_height, string_of_length len)
let font_size len = (Font_size, string_of_length len)
let line_height len = (Line_height, string_of_length len)
let font_weight w = (Font_weight, string_of_font_weight w)
let text_align a = (Text_align, string_of_text_align a)
let text_decoration_style value = (Text_decoration_style, value)
let text_underline_offset value = (Text_underline_offset, value)
let text_transform value = (Text_transform, value)
let letter_spacing value = (Letter_spacing, value)
let white_space value = (White_space, value)
let display d = (Display, string_of_display d)
let position p = (Position, string_of_position p)
let top value = (Top, value)
let right value = (Right, value)
let bottom value = (Bottom, value)
let left value = (Left, value)
let opacity value = (Opacity, value)

(* Remove deprecated string-based versions *)
let flex_direction d = (Flex_direction, string_of_flex_direction d)
let flex value = (Flex, value)
let flex_grow value = (Flex_grow, value)
let flex_shrink value = (Flex_shrink, value)
let flex_wrap value = (Flex_wrap, value)
let align_items a = (Align_items, string_of_align a)
let align_content a = (Align_content, string_of_align a)
let align_self a = (Align_self, string_of_align a)
let justify_content a = (Justify_content, string_of_align a)
let justify_self a = (Justify_self, string_of_align a)
let place_content value = (Place_content, value)
let place_items value = (Place_items, value)
let place_self value = (Place_self, value)
let border_width len = (Border_width, string_of_length len)
let border_radius len = (Border_radius, string_of_length len)
let box_shadow value = (Box_shadow, value)
let table_layout value = (Table_layout, value)
let border_spacing value = (Border_spacing, value)
let overflow o = (Overflow, string_of_overflow o)
let object_fit value = (Object_fit, value)
let clip value = (Clip, value)
let filter value = (Filter, value)
let background_image value = (Background_image, value)
let animation value = (Animation, value)

let grid_template_columns value =
  (Grid_template_columns, string_of_grid_template value)

let grid_template_rows value =
  (Grid_template_rows, string_of_grid_template value)

let pointer_events value = (Pointer_events, value)
let z_index value = (Z_index, value)
let appearance value = (Appearance, value)
let overflow_x o = (Overflow_x, string_of_overflow o)
let overflow_y o = (Overflow_y, string_of_overflow o)
let resize value = (Resize, value)
let vertical_align value = (Vertical_align, value)
let box_sizing value = (Box_sizing, value)
let font_family value = (Font_family, value)
let moz_osx_font_smoothing value = (Moz_osx_font_smoothing, value)
let webkit_line_clamp value = (Webkit_line_clamp, value)
let backdrop_filter value = (Backdrop_filter, value)
let background_position value = (Background_position, value)
let background_repeat value = (Background_repeat, value)
let background_size value = (Background_size, value)
let unsafe_declaration name value = (Custom name, value)

(* CSS Custom Properties *)
let custom_property name value =
  if not (String.starts_with ~prefix:"--" name) then
    invalid_arg
      (str [ "custom_property: name must start with '--', got: "; name ])
  else (Custom name, value)

(* Additional property constructors *)
let content value = (Custom "content", value)
let border_left_width len = (Border_left_width, string_of_length len)
let border_bottom_width len = (Border_bottom_width, string_of_length len)
let border_top_width len = (Border_top_width, string_of_length len)
let border_right_width len = (Border_right_width, string_of_length len)
let border_left_color c = (Custom "border-left-color", string_of_color c)
let border_bottom_color c = (Custom "border-bottom-color", string_of_color c)
let transition value = (Transition, value)
let quotes value = (Custom "quotes", value)

(* Internal declaration function for tw.ml *)
let declaration = unsafe_declaration

(* New property constructors for tw.ml *)
let border value = (Border, value)
let tab_size value = (Tab_size, value)
let webkit_text_size_adjust value = (Webkit_text_size_adjust, value)
let font_feature_settings value = (Font_feature_settings, value)
let font_variation_settings value = (Font_variation_settings, value)
let webkit_tap_highlight_color value = (Webkit_tap_highlight_color, value)
let webkit_text_decoration value = (Webkit_text_decoration, value)
let text_indent value = (Text_indent, value)
let border_collapse value = (Border_collapse, value)
let list_style value = (List_style, value)
let font value = (Font, value)
let webkit_appearance value = (Webkit_appearance, value)
let webkit_font_smoothing value = (Webkit_font_smoothing, value)
let cursor c = (Cursor, string_of_cursor c)
let user_select u = (User_select, string_of_user_select u)
let container_type value = (Container_type, value)
let container_name value = (Container_name, value)
let perspective value = (Perspective, value)
let perspective_origin value = (Perspective_origin, value)
let transform_style value = (Transform_style, value)
let backface_visibility value = (Backface_visibility, value)
let object_position value = (Object_position, value)
let rotate value = (Rotate, value)

let transform values =
  let str = String.concat " " (List.map string_of_transform_value values) in
  (Transform, str)

let transition_duration value = (Transition_duration, value)
let transition_timing_function value = (Transition_timing_function, value)
let will_change value = (Will_change, value)
let contain value = (Contain, value)
let isolation value = (Isolation, value)
let padding_inline value = (Padding_inline, value)
let padding_inline_start value = (Padding_inline_start, value)
let padding_block value = (Padding_block, value)
let margin_inline value = (Margin_inline, value)
let margin_block value = (Margin_block, value)
let margin_inline_end value = (Margin_inline_end, value)
let outline value = (Outline, value)
let outline_offset value = (Outline_offset, value)
let scroll_snap_type value = (Scroll_snap_type, value)
let scroll_snap_align value = (Scroll_snap_align, value)
let scroll_snap_stop value = (Scroll_snap_stop, value)
let scroll_behavior value = (Scroll_behavior, value)

type rule = { selector : string; declarations : declaration list }
type media_query = { media_condition : string; media_rules : rule list }

type container_query = {
  container_name : string option;
  container_condition : string;
  container_rules : rule list;
}

type starting_style = { starting_rules : rule list }

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_query list

and supports_query = {
  supports_condition : string;
  supports_content : supports_content;
}

type nested_rule = Rule of rule | Supports of supports_query

type at_property = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string;
}

type layer = Properties | Theme | Base | Components | Utilities

type layered_rules = {
  layer : layer;
  rules : nested_rule list;
  media_queries : media_query list;
  container_queries : container_query list;
  supports_queries : supports_query list;
}

type t = {
  layers : layered_rules list;
  rules : rule list;
  media_queries : media_query list;
  container_queries : container_query list;
  starting_styles : starting_style list;
  supports_queries : supports_query list;
  at_properties : at_property list;
}

type sheet_item =
  | Rule of rule
  | Media of media_query
  | Container of container_query
  | Starting_style of starting_style
  | Supports of supports_query
  | At_property of at_property
  | Layer of layered_rules

(** {1 Creation} *)

let declaration_value (_, value) = value
let declaration_property (prop_name, _) = prop_name

let is_custom_property (prop_name, _) =
  match prop_name with
  | Custom name -> String.starts_with ~prefix:"--" name
  | _ -> false

let string_of_property = function
  | Background_color -> "background-color"
  | Color -> "color"
  | Border_color -> "border-color"
  | Border_style -> "border-style"
  | Padding -> "padding"
  | Padding_left -> "padding-left"
  | Padding_right -> "padding-right"
  | Padding_bottom -> "padding-bottom"
  | Padding_top -> "padding-top"
  | Padding_inline -> "padding-inline"
  | Padding_inline_start -> "padding-inline-start"
  | Padding_block -> "padding-block"
  | Margin -> "margin"
  | Margin_inline_end -> "margin-inline-end"
  | Margin_left -> "margin-left"
  | Margin_right -> "margin-right"
  | Margin_top -> "margin-top"
  | Margin_bottom -> "margin-bottom"
  | Margin_inline -> "margin-inline"
  | Margin_block -> "margin-block"
  | Gap -> "gap"
  | Column_gap -> "column-gap"
  | Row_gap -> "row-gap"
  | Width -> "width"
  | Height -> "height"
  | Min_width -> "min-width"
  | Min_height -> "min-height"
  | Max_width -> "max-width"
  | Max_height -> "max-height"
  | Font_size -> "font-size"
  | Line_height -> "line-height"
  | Font_weight -> "font-weight"
  | Font_style -> "font-style"
  | Text_align -> "text-align"
  | Text_decoration -> "text-decoration"
  | Text_decoration_style -> "text-decoration-style"
  | Text_underline_offset -> "text-underline-offset"
  | Text_transform -> "text-transform"
  | Letter_spacing -> "letter-spacing"
  | List_style_type -> "list-style-type"
  | Display -> "display"
  | Position -> "position"
  | Flex_direction -> "flex-direction"
  | Flex_wrap -> "flex-wrap"
  | Flex -> "flex"
  | Flex_grow -> "flex-grow"
  | Flex_shrink -> "flex-shrink"
  | Align_items -> "align-items"
  | Justify_content -> "justify-content"
  | Align_content -> "align-content"
  | Align_self -> "align-self"
  | Justify_self -> "justify-self"
  | Place_content -> "place-content"
  | Place_items -> "place-items"
  | Place_self -> "place-self"
  | Grid_template_columns -> "grid-template-columns"
  | Grid_template_rows -> "grid-template-rows"
  | Border_width -> "border-width"
  | Border_radius -> "border-radius"
  | Box_shadow -> "box-shadow"
  | Opacity -> "opacity"
  | Transition -> "transition"
  | Transform -> "transform"
  | Cursor -> "cursor"
  | Table_layout -> "table-layout"
  | Border_collapse -> "border-collapse"
  | Border_spacing -> "border-spacing"
  | User_select -> "user-select"
  | Pointer_events -> "pointer-events"
  | Overflow -> "overflow"
  | Top -> "top"
  | Right -> "right"
  | Bottom -> "bottom"
  | Left -> "left"
  | Z_index -> "z-index"
  | Border_top_width -> "border-top-width"
  | Border_right_width -> "border-right-width"
  | Border_bottom_width -> "border-bottom-width"
  | Border_left_width -> "border-left-width"
  | Outline -> "outline"
  | Outline_offset -> "outline-offset"
  | Scroll_snap_type -> "scroll-snap-type"
  | Clip -> "clip"
  | White_space -> "white-space"
  | Border -> "border"
  | Tab_size -> "tab-size"
  | Webkit_text_size_adjust -> "-webkit-text-size-adjust"
  | Font_feature_settings -> "font-feature-settings"
  | Font_variation_settings -> "font-variation-settings"
  | Webkit_tap_highlight_color -> "-webkit-tap-highlight-color"
  | Webkit_text_decoration -> "-webkit-text-decoration"
  | Text_indent -> "text-indent"
  | List_style -> "list-style"
  | Font -> "font"
  | Webkit_appearance -> "-webkit-appearance"
  | Container_type -> "container-type"
  | Container_name -> "container-name"
  | Perspective -> "perspective"
  | Perspective_origin -> "perspective-origin"
  | Transform_style -> "transform-style"
  | Backface_visibility -> "backface-visibility"
  | Object_position -> "object-position"
  | Object_fit -> "object-fit"
  | Rotate -> "rotate"
  | Transition_duration -> "transition-duration"
  | Transition_timing_function -> "transition-timing-function"
  | Will_change -> "will-change"
  | Contain -> "contain"
  | Isolation -> "isolation"
  | Filter -> "filter"
  | Background_image -> "background-image"
  | Animation -> "animation"
  | Appearance -> "appearance"
  | Overflow_x -> "overflow-x"
  | Overflow_y -> "overflow-y"
  | Resize -> "resize"
  | Vertical_align -> "vertical-align"
  | Box_sizing -> "box-sizing"
  | Font_family -> "font-family"
  | Background_position -> "background-position"
  | Background_repeat -> "background-repeat"
  | Background_size -> "background-size"
  | Webkit_font_smoothing -> "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> "-webkit-line-clamp"
  | Backdrop_filter -> "backdrop-filter"
  | Scroll_snap_align -> "scroll-snap-align"
  | Scroll_snap_stop -> "scroll-snap-stop"
  | Scroll_behavior -> "scroll-behavior"
  | Custom s -> s

let rule ~selector declarations = { selector; declarations }
let selector rule = rule.selector
let declarations rule = rule.declarations

let media ~condition rules =
  { media_condition = condition; media_rules = rules }

let supports ~condition rules =
  { supports_condition = condition; supports_content = Support_rules rules }

let supports_nested ~condition rules nested_queries =
  {
    supports_condition = condition;
    supports_content = Support_nested (rules, nested_queries);
  }

let container ?(name = Option.none) ~condition rules =
  {
    container_name = name;
    container_condition = condition;
    container_rules = rules;
  }

let at_property ~name ~syntax ~initial_value ?(inherits = false) () =
  { name; syntax; inherits; initial_value }

let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layered_rules ~layer ?(media_queries = []) ?(container_queries = [])
    ?(supports_queries = []) rules =
  { layer; rules; media_queries; container_queries; supports_queries }

let empty =
  {
    layers = [];
    rules = [];
    media_queries = [];
    container_queries = [];
    starting_styles = [];
    supports_queries = [];
    at_properties = [];
  }

let concat stylesheets =
  List.fold_left
    (fun acc sheet ->
      {
        layers = acc.layers @ sheet.layers;
        rules = acc.rules @ sheet.rules;
        media_queries = acc.media_queries @ sheet.media_queries;
        container_queries = acc.container_queries @ sheet.container_queries;
        starting_styles = acc.starting_styles @ sheet.starting_styles;
        supports_queries = acc.supports_queries @ sheet.supports_queries;
        at_properties = acc.at_properties @ sheet.at_properties;
      })
    empty stylesheets

let stylesheet items =
  List.fold_left
    (fun acc item ->
      match item with
      | Rule r -> { acc with rules = acc.rules @ [ r ] }
      | Media m -> { acc with media_queries = acc.media_queries @ [ m ] }
      | Container c ->
          { acc with container_queries = acc.container_queries @ [ c ] }
      | Starting_style s ->
          { acc with starting_styles = acc.starting_styles @ [ s ] }
      | Supports s ->
          { acc with supports_queries = acc.supports_queries @ [ s ] }
      | At_property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] })
    empty items

(** {1 Utilities} *)

(** Extract all CSS variables referenced in properties (for theme layer) *)
let all_vars properties =
  let extract_var_name var_content =
    match String.index var_content ',' with
    | exception Not_found -> String.trim var_content
    | comma -> String.trim (String.sub var_content 0 comma)
  in

  let process_var_at_position value var_pos acc =
    if var_pos + 4 > String.length value || String.sub value var_pos 4 <> "var("
    then Option.none
    else
      let var_start = var_pos + 4 in
      match String.index_from value var_start ')' with
      | exception Not_found -> Option.none
      | end_paren ->
          let var_content =
            String.sub value var_start (end_paren - var_start)
          in
          let var_name = extract_var_name var_content in
          let new_acc =
            if String.length var_name > 2 && String.sub var_name 0 2 = "--" then
              var_name :: acc
            else acc
          in
          Option.some (new_acc, end_paren + 1)
  in

  let rec extract_vars_from_value value acc pos =
    if pos >= String.length value then acc
    else
      try
        let var_pos = String.index_from value pos 'v' in
        match process_var_at_position value var_pos acc with
        | Some (new_acc, next_pos) ->
            extract_vars_from_value value new_acc next_pos
        | None -> extract_vars_from_value value acc (var_pos + 1)
      with Not_found -> acc
  in
  List.concat_map
    (fun (_, value) -> extract_vars_from_value value [] 0)
    properties
  |> List.sort_uniq String.compare

let deduplicate_declarations props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  List.fold_right
    (fun (prop_name, value) acc ->
      if Hashtbl.mem seen prop_name then acc
      else (
        Hashtbl.add seen prop_name ();
        (prop_name, value) :: acc))
    props []

let inline_style_of_declarations props =
  props
  |> List.map (fun (prop_name, value) ->
         str [ string_of_property prop_name; ": "; value ])
  |> String.concat "; "

let merge_rules rules =
  (* Group rules by selector while preserving first occurrence order *)
  let rec merge_helper acc seen = function
    | [] -> List.rev acc
    | rule :: rest ->
        if List.mem rule.selector seen then
          (* Selector already seen, merge properties into existing rule *)
          let acc' =
            List.map
              (fun r ->
                if r.selector = rule.selector then
                  { r with declarations = r.declarations @ rule.declarations }
                else r)
              acc
          in
          merge_helper acc' seen rest
        else
          (* New selector, add to acc and mark as seen *)
          merge_helper
            ({
               rule with
               declarations = deduplicate_declarations rule.declarations;
             }
            :: acc)
            (rule.selector :: seen) rest
  in
  merge_helper [] [] rules
  |> List.map (fun r ->
         { r with declarations = deduplicate_declarations r.declarations })

(* Merge rules with identical properties into combined selectors *)
let merge_by_properties rules =
  (* Create a hash of properties for comparison *)
  let properties_hash props =
    props
    |> List.map (fun (name, value) ->
           str [ string_of_property name; ":"; value ])
    |> List.sort String.compare |> String.concat ";"
  in

  (* Group rules by their properties *)
  let groups = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      let hash = properties_hash rule.declarations in
      let existing = try Hashtbl.find groups hash with Not_found -> [] in
      Hashtbl.replace groups hash (rule :: existing))
    rules;

  (* Create merged rules *)
  Hashtbl.fold
    (fun _hash rules_with_same_props acc ->
      match rules_with_same_props with
      | [] -> acc
      | [ rule ] -> rule :: acc
      | multiple ->
          let selectors =
            multiple
            |> List.map (fun r -> r.selector)
            |> List.sort String.compare |> String.concat ","
          in
          let declarations = (List.hd multiple).declarations in
          { selector = selectors; declarations } :: acc)
    groups []
  |> List.sort (fun a b -> String.compare a.selector b.selector)

let minify_selector s =
  (* Remove unnecessary whitespace in selectors *)
  s
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '>'; Re.rep Re.space ]))
       ~by:">"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '+'; Re.rep Re.space ]))
       ~by:"+"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char '~'; Re.rep Re.space ]))
       ~by:"~"
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ','; Re.rep Re.space ]))
       ~by:","
  |> Re.replace_string
       (Re.compile (Re.seq [ Re.rep Re.space; Re.char ':'; Re.rep Re.space ]))
       ~by:":"
  |> String.trim

let minify_value v =
  let v = String.trim v in
  (* Special case: plain "0" or "1" should stay as is *)
  if v = "0" || v = "1" then v
  else
    let v =
      (* Remove spaces after commas in specific contexts *)
      (* Keep spaces in: color functions like rgb(), hsl(), color-mix() *)
      if
        Re.execp
          (Re.compile
             (Re.str "\\b(rgb|rgba|hsl|hsla|color-mix|oklab|oklch)\\s*\\("))
          v
      then v (* Don't remove spaces in color functions *)
      else Re.replace_string (Re.compile (Re.str ", ")) ~by:"," v
    in
    let v =
      (* Remove spaces in calc expressions *)
      if String.contains v '(' && String.contains v ')' then
        Re.replace_string (Re.compile (Re.str " * ")) ~by:"*" v
        |> Re.replace_string (Re.compile (Re.str " + ")) ~by:"+"
        |> Re.replace_string (Re.compile (Re.str " - ")) ~by:"-"
        |> Re.replace_string (Re.compile (Re.str " / ")) ~by:"/"
      else v
    in
    (* Remove leading 0 from decimals in oklch values and measurements *)
    (* Handle oklch values specially - replace " 0." with " ." *)
    let v =
      if String.contains v 'o' && String.contains v 'k' then
        Re.replace_string (Re.compile (Re.str " 0.")) ~by:" ." v
      else v
    in
    (* Remove leading 0 from decimals but preserve units - e.g., "0.5rem" ->
       ".5rem" *)
    let decimal_re =
      Re.compile
        (Re.seq
           [
             Re.bos;
             Re.str "0.";
             Re.group (Re.rep1 Re.digit);
             Re.group (Re.rep Re.any);
           ])
    in
    match Re.exec_opt decimal_re v with
    | Some m -> str [ "."; Re.Group.get m 1; Re.Group.get m 2 ]
    | None -> v

(** {1 Rendering} *)

let render_minified_rule rule =
  let selector = minify_selector rule.selector in
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ string_of_property prop_name; ":"; minify_value value ])
  in
  str [ selector; "{"; str ~sep:";" props; "}" ]

let render_formatted_rule rule =
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ "  "; string_of_property prop_name; ": "; value; ";" ])
  in
  lines [ str [ rule.selector; " {" ]; lines props; "}" ]

let render_formatted_media_rule rule =
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ "    "; string_of_property prop_name; ": "; value; ";" ])
  in
  str [ "  "; rule.selector; " {\n"; lines props; "\n  }" ]

let layer_to_string = function
  | Properties -> "properties"
  | Theme -> "theme"
  | Base -> "base"
  | Components -> "components"
  | Utilities -> "utilities"

let rec render_supports_content ~minify content =
  match content with
  | Support_rules rules ->
      if minify then
        rules |> merge_rules |> merge_by_properties
        |> List.map render_minified_rule
        |> String.concat ""
      else rules |> List.map render_formatted_media_rule |> String.concat "\n"
  | Support_nested (rules, nested_queries) ->
      let rules_str =
        if minify then
          rules |> merge_rules |> merge_by_properties
          |> List.map render_minified_rule
          |> String.concat ""
        else rules |> List.map render_formatted_media_rule |> String.concat "\n"
      in
      let nested_str =
        nested_queries
        |> List.map (fun nsq ->
               let content_str =
                 render_supports_content ~minify nsq.supports_content
               in
               if minify then
                 str
                   [
                     "@supports "; nsq.supports_condition; "{"; content_str; "}";
                   ]
               else
                 str
                   [
                     "@supports ";
                     nsq.supports_condition;
                     " {\n";
                     content_str;
                     "\n}";
                   ])
        |> String.concat (if minify then "" else "\n")
      in
      if minify then str [ rules_str; nested_str ]
      else str [ rules_str; "\n"; nested_str ]

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  str
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

(* Configuration for stylesheet rendering *)
type config = { minify : bool }

(* TODO: Complete migration to structured CSS representation Intermediate
   representation for CSS output type css_block = | RuleBlock of { selector:
   string; properties: (declaration_property * string) list } | MediaBlock of {
   condition: string; blocks: css_block list } | ContainerBlock of { name:
   string option; condition: string; blocks: css_block list } | SupportsBlock of
   { condition: string; blocks: css_block list } | LayerBlock of { name: string;
   blocks: css_block list } | StartingStyleBlock of { blocks: css_block list } |
   PropertyBlock of { name: string; syntax: string; inherits: bool;
   initial_value: string } | Raw of string

   let rules_to_blocks rules = List.map (fun r -> RuleBlock { selector =
   r.selector; properties = r.declarations }) rules

   let rec format_block ~config block = match block with | RuleBlock { selector;
   properties } -> let sel = if config.minify then minify_selector selector else
   selector in let props = properties |> List.map (fun (name, value) -> let
   prop_name = string_of_property name in let val_str = if config.minify then
   minify_value value else value in str [prop_name; ":"; val_str]) in if
   config.minify then str [sel; "{"; str ~sep:";" props; "}"] else let
   prop_lines = props |> List.map (fun p -> " " ^ p ^ ";") |> String.concat "\n"
   in str [sel; " {\n"; prop_lines; "\n}"]

   | MediaBlock { condition; blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@media "; condition; "{"; content; "}"] else
   let indented = blocks |> List.map (format_block ~config) |> List.map (fun s
   -> " " ^ String.concat "\n " (String.split_on_char '\n' s)) |> String.concat
   "\n" in str ["@media "; condition; " {\n"; indented; "\n}"]

   | ContainerBlock { name; condition; blocks } -> let name_part = match name
   with None -> "" | Some n -> n ^ " " in let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@container "; name_part; condition; "{";
   content; "}"] else let indented = blocks |> List.map (format_block ~config)
   |> List.map (fun s -> " " ^ String.concat "\n " (String.split_on_char '\n'
   s)) |> String.concat "\n" in str ["@container "; name_part; condition; "
   {\n"; indented; "\n}"]

   | SupportsBlock { condition; blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@supports "; condition; "{"; content; "}"]
   else let indented = blocks |> List.map (format_block ~config) |> List.map
   (fun s -> " " ^ String.concat "\n " (String.split_on_char '\n' s)) |>
   String.concat "\n" in str ["@supports "; condition; " {\n"; indented; "\n}"]

   | LayerBlock { name; blocks } -> if blocks = [] then str ["@layer "; name;
   ";"] else let content = blocks |> List.map (format_block ~config) |>
   String.concat (if config.minify then "" else "\n") in if config.minify then
   str ["@layer "; name; "{"; content; "}"] else let indented = blocks |>
   List.map (format_block ~config) |> List.map (fun s -> " " ^ String.concat "\n
   " (String.split_on_char '\n' s)) |> String.concat "\n" in str ["@layer ";
   name; " {\n"; indented; "\n}"]

   | StartingStyleBlock { blocks } -> let content = blocks |> List.map
   (format_block ~config) |> String.concat (if config.minify then "" else "\n")
   in if config.minify then str ["@starting-style{"; content; "}"] else let
   indented = blocks |> List.map (format_block ~config) |> List.map (fun s -> "
   " ^ String.concat "\n " (String.split_on_char '\n' s)) |> String.concat "\n"
   in str ["@starting-style {\n"; indented; "\n}"]

   | PropertyBlock { name; syntax; inherits; initial_value } -> if config.minify
   then str ["@property "; name; "{syntax:\""; syntax; "\";inherits:"; (if
   inherits then "true" else "false"); ";initial-value:"; initial_value; "}"]
   else str ["@property "; name; " {\n syntax: \""; syntax; "\";\n inherits: ";
   (if inherits then "true" else "false"); ";\n initial-value: "; initial_value;
   ";\n}"]

   | Raw s -> s *)

(* Helper: Render rules string for a layer *)
let render_layer_rules ~config rules =
  let render_nested_rule : nested_rule -> string = function
    | Rule r ->
        if config.minify then render_minified_rule r
        else render_formatted_rule r
    | Supports sq ->
        let sq_content =
          render_supports_content ~minify:config.minify sq.supports_content
        in
        if config.minify then
          str [ "@supports "; sq.supports_condition; "{"; sq_content; "}" ]
        else
          str [ "@supports "; sq.supports_condition; " {\n"; sq_content; "\n}" ]
  in

  (* Render rules in order without merging *)
  rules
  |> List.map render_nested_rule
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render media queries *)
let render_layer_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         let content =
           if config.minify then
             mq.media_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             mq.media_rules
             |> List.map render_formatted_media_rule
             |> String.concat "\n"
         in
         if config.minify then
           str [ "@media "; mq.media_condition; "{"; content; "}" ]
         else str [ "@media "; mq.media_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render container queries *)
let render_layer_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with None -> "" | Some name -> name ^ " "
         in
         let content =
           if config.minify then
             cq.container_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             cq.container_rules
             |> List.map render_formatted_media_rule
             |> String.concat "\n"
         in
         if config.minify then
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               "{";
               content;
               "}";
             ]
         else
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               " {\n";
               content;
               "\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render supports queries *)
let render_layer_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify sq.supports_content
         in
         if config.minify then
           str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           str [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Check if layer is empty *)
let is_layer_empty (lr : layered_rules) =
  lr.rules = [] && lr.media_queries = [] && lr.container_queries = []
  && lr.supports_queries = []

(* Helper: Render non-layered elements *)
let render_stylesheet_rules ~config rules =
  if config.minify then
    rules |> merge_rules |> merge_by_properties
    |> List.map render_minified_rule
    |> String.concat ""
  else rules |> List.map render_formatted_rule |> String.concat "\n"

let render_stylesheet_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         let content =
           if config.minify then
             mq.media_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             mq.media_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then
           str [ "@media "; mq.media_condition; "{"; content; "}" ]
         else str [ "@media "; mq.media_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with None -> "" | Some name -> name ^ " "
         in
         let content =
           if config.minify then
             cq.container_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             cq.container_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               "{";
               content;
               "}";
             ]
         else
           str
             [
               "@container ";
               name_part;
               cq.container_condition;
               " {\n";
               content;
               "\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify sq.supports_content
         in
         if config.minify then
           str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           str [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_starting_styles ~config starting_styles =
  starting_styles
  |> List.map (fun ss ->
         let content =
           if config.minify then
             ss.starting_rules |> merge_rules |> merge_by_properties
             |> List.map render_minified_rule
             |> String.concat ""
           else
             ss.starting_rules
             |> List.map render_formatted_rule
             |> String.concat "\n"
         in
         if config.minify then str [ "@starting-style{"; content; "}" ]
         else str [ "@starting-style {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_at_properties ~config at_properties =
  at_properties
  |> List.map (fun at ->
         if config.minify then
           let initial_value_part =
             if at.initial_value = "" then ""
             else str [ ";initial-value:"; at.initial_value ]
           in
           str
             [
               "@property ";
               at.name;
               "{syntax:\"";
               at.syntax;
               "\";inherits:";
               (if at.inherits then "true" else "false");
               initial_value_part;
               "}";
             ]
         else
           let initial_value_part =
             if at.initial_value = "" then ""
             else str [ ";\n  initial-value: "; at.initial_value ]
           in
           str
             [
               "@property ";
               at.name;
               " {\n  syntax: \"";
               at.syntax;
               "\";\n  inherits: ";
               (if at.inherits then "true" else "false");
               initial_value_part;
               ";\n}";
             ])
  |> String.concat (if config.minify then "" else "\n")

(* Helper functions for to_string *)
let render_layer ~config layer_rules =
  let layer_name = layer_to_string layer_rules.layer in
  let all_parts =
    [
      render_layer_rules ~config layer_rules.rules;
      render_layer_media ~config layer_rules.media_queries;
      render_layer_containers ~config layer_rules.container_queries;
      render_layer_supports ~config layer_rules.supports_queries;
    ]
    |> List.filter (fun s -> s <> "")
  in
  if all_parts = [] then str [ "@layer "; layer_name; ";" ]
  else
    let content =
      String.concat (if config.minify then "" else "\n") all_parts
    in
    if config.minify then str [ "@layer "; layer_name; "{"; content; "}" ]
    else str [ "@layer "; layer_name; " {\n"; content; "\n}" ]

let prepare_layer_strings ~config stylesheet =
  let has_empty_components_and_utilities =
    List.exists
      (fun lr -> lr.layer = Components && is_layer_empty lr)
      stylesheet.layers
    && List.exists
         (fun lr -> lr.layer = Utilities && is_layer_empty lr)
         stylesheet.layers
  in
  let layer_strings =
    if has_empty_components_and_utilities && config.minify then
      stylesheet.layers
      |> List.filter (fun lr ->
             not
               ((lr.layer = Components || lr.layer = Utilities)
               && is_layer_empty lr))
      |> List.map (render_layer ~config)
    else stylesheet.layers |> List.map (render_layer ~config)
  in
  let empty_layers_decl =
    if has_empty_components_and_utilities && config.minify then
      [ "@layer components,utilities;" ]
    else []
  in
  (layer_strings, empty_layers_decl)

let render_optional_section render_fn items =
  let rendered = render_fn items in
  if rendered = "" then [] else [ rendered ]

let to_string ?(minify = false) stylesheet =
  let config = { minify } in
  (* Add tw library header *)
  let header_str =
    if List.length stylesheet.layers > 0 then str [ header; "\n" ] else ""
  in

  (* Prepare layer strings *)
  let layer_strings, empty_layers_decl =
    prepare_layer_strings ~config stylesheet
  in

  (* Render all optional sections *)
  let rule_strings =
    render_optional_section (render_stylesheet_rules ~config) stylesheet.rules
  in
  let at_property_strings =
    render_optional_section
      (render_at_properties ~config)
      stylesheet.at_properties
  in
  let starting_style_strings =
    render_optional_section
      (render_starting_styles ~config)
      stylesheet.starting_styles
  in
  let container_strings =
    render_optional_section
      (render_stylesheet_containers ~config)
      stylesheet.container_queries
  in
  let supports_strings =
    render_optional_section
      (render_stylesheet_supports ~config)
      stylesheet.supports_queries
  in
  let media_strings =
    render_optional_section
      (render_stylesheet_media ~config)
      stylesheet.media_queries
  in

  (* Combine all parts *)
  let all_parts =
    [ header_str; "" ] (* layer_declarations is always empty for Tailwind v4 *)
    @ layer_strings @ empty_layers_decl @ rule_strings @ starting_style_strings
    @ container_strings @ supports_strings @ media_strings @ at_property_strings
  in

  if config.minify then String.concat "" all_parts
  else String.concat "\n" (List.filter (fun s -> s <> "") all_parts)

let pp = to_string
