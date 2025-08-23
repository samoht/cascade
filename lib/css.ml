(** CSS generation utilities *)

(* Simple string formatting utilities *)
let str ?(sep = "") segments = String.concat sep segments
let lines segments = str ~sep:"\n" segments

type 'a var = { name : string; fallback : 'a var_fallback option }
(** CSS variable reference *)

and 'a var_fallback = Var of 'a var | Value of 'a

let var ?fallback name = { name; fallback }

(** CSS length values *)
type calc_op = Add | Sub | Mult | Div

type 'a calc =
  | Var of 'a calc var (* CSS variable *)
  | Val of 'a
  | Expr of 'a calc * calc_op * 'a calc

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

(** CSS color values *)
type color =
  | Hex of { hash : bool; value : string } (* hash indicates if # was present *)
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }
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

(** CSS display values *)
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

(** CSS position values *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS font weight values *)
type font_weight =
  | Weight of int
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Var of font_weight var (* CSS variable reference *)

(** CSS text align values *)
type text_align = Left | Right | Center | Justify | Start | End | Inherit

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
  | Auto

(** CSS text decoration values *)
type text_decoration =
  | None
  | Underline
  | Overline
  | Line_through
  | Inherit
  | Underline_dotted (* underline dotted *)

(** CSS font style values *)
type font_style = Font_normal | Italic | Oblique | Font_inherit

(** CSS list style type values *)
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

(** CSS border style values *)
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

(** CSS flex wrap values *)
type flex_wrap = Nowrap | Wrap | Wrap_reverse

(** CSS text transform values *)
type text_transform =
  | None
  | Uppercase
  | Lowercase
  | Capitalize
  | Full_width
  | Full_size_kana
  | Inherit

(** CSS box sizing values *)
type box_sizing = Border_box | Content_box | Inherit

(** CSS white space values *)
type white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Inherit

(** CSS table layout values *)
type table_layout = Auto | Fixed | Inherit

(** CSS resize values *)
type resize_value =
  | None
  | Both
  | Horizontal
  | Vertical
  | Block
  | Inline
  | Inherit

(** CSS object fit values *)
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit

(** CSS appearance values *)
type appearance_value = None | Auto | Button | Textfield | Menulist | Inherit

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

type border_collapse_value = Collapse | Separate | Inherit

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

(** CSS text-decoration-style values *)
type text_decoration_style_value =
  | Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  | Inherit

(** CSS webkit-font-smoothing values *)
type webkit_font_smoothing_value =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS -moz-osx-font-smoothing values *)
type moz_osx_font_smoothing_value = Auto | Grayscale | Inherit

(** CSS transform-style values *)
type transform_style_value = Flat | Preserve_3d | Inherit

(** CSS backface-visibility values *)
type backface_visibility_value = Visible | Hidden | Inherit

(** CSS scroll-behavior values *)
type scroll_behavior_value = Auto | Smooth | Inherit

(** CSS scroll-snap-type values *)
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
  | X_var of string
  | Y_var of string
  | Both_var of string
  | Inherit

(** CSS scroll-snap-align values *)
type scroll_snap_align_value = None | Start | End | Center | Inherit

(** CSS scroll-snap-stop values *)
type scroll_snap_stop_value = Normal | Always | Inherit

(** CSS isolation values *)
type isolation_value = Auto | Isolate | Inherit

(** CSS background-repeat values *)
type background_repeat_value =
  | Repeat
  | Repeat_x
  | Repeat_y
  | No_repeat
  | Space
  | Round
  | Inherit

(** CSS container-type values *)
type container_type_value = Normal | Size | Inline_size | Inherit

(** CSS flex shorthand values *)
type flex_value =
  | Initial (* 0 1 auto *)
  | Auto (* 1 1 auto *)
  | None (* 0 0 auto *)
  | Grow of float (* grow 1 0% *)
  | Basis of length (* 1 1 basis *)
  | Grow_shrink of float * float (* grow shrink 0% *)
  | Full of float * float * length (* grow shrink basis *)

(** CSS duration values *)
type duration =
  | Ms of int
  (* milliseconds *)
  | S of float (* seconds *)

(** CSS timing function values *)
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

(** CSS transition property values *)
type transition_property = All | None | Property of string

(** CSS transition values *)
type transition_value =
  | Simple of transition_property * duration
  | With_timing of transition_property * duration * timing_function
  | With_delay of transition_property * duration * timing_function * duration
  | Multiple of transition_value list

(** CSS grid track sizing *)
type grid_track_size =
  | Fr of float
  | Min_max of length * grid_track_size
  | Grid_auto
  | Max_content
  | Min_content
  | Fit_content of length
  | Grid_length of length

(** CSS grid template values *)
type grid_template =
  | Tracks of grid_track_size list
  | Repeat of int * grid_track_size
  | Repeat_auto_fill of grid_track_size
  | Repeat_auto_fit of grid_track_size
  | Grid_none
  | Grid_inherit

(** CSS transform angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Angle_var of { var_name : string; fallback : float option }

(** CSS transform scale values *)
type scale_value =
  | Scale_num of float
  | Scale_var of { var_name : string; fallback : float option }

(** CSS transform values *)
type transform_value =
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate of length * length
  | Translate_var of { var_name : string; fallback : string option }
  | Translate3d of length * length * length
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate of angle
  | Rotate_var of { var_name : string; fallback : string option }
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

let pp_float f =
  (* For whole numbers, use integer formatting *)
  if f = floor f then string_of_int (int_of_float f)
  else
    let s = string_of_float f in
    (* Remove trailing dot if present *)
    let s =
      if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
      else s
    in
    (* Remove leading zero for decimals between -1 and 1 *)
    if String.starts_with ~prefix:"-0." s then
      "-" ^ String.sub s 2 (String.length s - 2)
    else if String.starts_with ~prefix:"0." s then
      String.sub s 1 (String.length s - 1)
    else s

(* Convert typed values to strings *)
let rec string_of_var : type a. (a -> string) -> a var -> string =
 fun value_to_string v ->
  let base = str [ "var(--"; v.name ] in
  match v.fallback with
  | None -> str [ base; ")" ]
  | Some (Var fallback) ->
      str [ base; ", "; string_of_var value_to_string fallback; ")" ]
  | Some (Value value) -> str [ base; ", "; value_to_string value; ")" ]

and string_of_length = function
  | Px n ->
      (* Special case for max float value used in rounded-full *)
      if n = max_int then "3.40282e38px" else str [ string_of_int n; "px" ]
  | Rem f -> str [ pp_float f; "rem" ]
  | Em f -> str [ pp_float f; "em" ]
  | Pct f -> str [ pp_float f; "%" ]
  | Vw f -> str [ pp_float f; "vw" ]
  | Vh f -> str [ pp_float f; "vh" ]
  | Ch f -> str [ pp_float f; "ch" ]
  | Lh f -> str [ pp_float f; "lh" ]
  | Num f -> pp_float f
  | Auto -> "auto"
  | Zero -> "0"
  | Inherit -> "inherit"
  | Fit_content -> "fit-content"
  | Max_content -> "max-content"
  | Min_content -> "min-content"
  | Var v -> string_of_var string_of_length v
  | Calc cv -> str [ "calc("; string_of_calc string_of_length cv; ")" ]

and string_of_calc : ('a -> string) -> 'a calc -> string =
 fun string_of_val calc ->
  match calc with
  | Val v -> string_of_val v
  | Var v -> string_of_var (string_of_calc string_of_val) v
  | Expr (left, op, right) ->
      let op_str =
        match op with
        | Add -> " + "
        | Sub -> " - "
        | Mult -> " * "
        | Div -> " / "
      in
      str
        [
          string_of_calc string_of_val left;
          op_str;
          string_of_calc string_of_val right;
        ]

(* Calc module for building calc() expressions *)
module Calc = struct
  let add left right = Expr (left, Add, right)
  let sub left right = Expr (left, Sub, right)
  let mul left right = Expr (left, Mult, right)
  let div left right = Expr (left, Div, right)

  (* Operators *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  (* Value constructors *)
  let length len = Val len
  let var name = (Var (var name) : 'a calc)
  let float f = Val (Num f)
  let px n = Val (Px n)
  let rem f = Val (Rem f)
  let em f = Val (Em f)
  let pct f = Val (Pct f)
end

let rec string_of_color_in_mix = function
  | Current -> "currentcolor" (* lowercase in color-mix *)
  | c -> string_of_color c

and string_of_color = function
  | Hex { hash; value } ->
      (* If hash was originally present, include it; for Tailwind arbitrary
         values without hash, omit it *)
      if hash then str [ "#"; value ]
      else if
        (* Also normalize by removing ONE leading zero from hex if starts with
           "00" *)
        String.length value = 6 && String.sub value 0 2 = "00"
      then String.sub value 1 5 (* 00ff00 -> 0ff00 *)
      else value
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
          pp_float a;
          ")";
        ]
  | Oklch { l; c; h } ->
      str
        [
          "oklch("; pp_float (l /. 100.0); " "; pp_float c; " "; pp_float h; ")";
        ]
  | Var v -> str [ "var(--"; v; ")" ]
  | Current -> "currentColor"
  | Transparent -> "transparent"
  | Inherit -> "inherit"
  | Mix { in_space; color1; percent1; color2; percent2 } ->
      let space_str = string_of_color_space in_space in
      let p1_str =
        match percent1 with Some p -> " " ^ string_of_int p ^ "%" | None -> ""
      in
      let p2_str =
        match percent2 with Some p -> " " ^ string_of_int p ^ "%" | None -> ""
      in
      str
        [
          "color-mix(in ";
          space_str;
          ",";
          string_of_color_in_mix color1;
          p1_str;
          ",";
          string_of_color_in_mix color2;
          p2_str;
          ")";
        ]

and string_of_color_space = function
  | Srgb -> "srgb"
  | Srgb_linear -> "srgb-linear"
  | Display_p3 -> "display-p3"
  | A98_rgb -> "a98-rgb"
  | Prophoto_rgb -> "prophoto-rgb"
  | Rec2020 -> "rec2020"
  | Lab -> "lab"
  | Oklab -> "oklab"
  | Xyz -> "xyz"
  | Xyz_d50 -> "xyz-d50"
  | Xyz_d65 -> "xyz-d65"
  | Lch -> "lch"
  | Oklch -> "oklch"
  | Hsl -> "hsl"
  | Hwb -> "hwb"

let string_of_display : display -> string = function
  | Block -> "block"
  | Inline -> "inline"
  | Inline_block -> "inline-block"
  | Flex -> "flex"
  | Inline_flex -> "inline-flex"
  | Grid -> "grid"
  | Inline_grid -> "inline-grid"
  | None -> "none"
  | Table -> "table"
  | Table_row -> "table-row"
  | Table_cell -> "table-cell"
  | List_item -> "list-item"

let string_of_position = function
  | Static -> "static"
  | Relative -> "relative"
  | Absolute -> "absolute"
  | Fixed -> "fixed"
  | Sticky -> "sticky"

let rec string_of_font_weight = function
  | Weight n -> string_of_int n
  | Normal -> "normal"
  | Bold -> "bold"
  | Bolder -> "bolder"
  | Lighter -> "lighter"
  | Inherit -> "inherit"
  | Var v -> string_of_var string_of_font_weight v

let string_of_text_align = function
  | Left -> "left"
  | Right -> "right"
  | Center -> "center"
  | Justify -> "justify"
  | Start -> "start"
  | End -> "end"
  | Inherit -> "inherit"

let string_of_overflow : overflow -> string = function
  | Visible -> "visible"
  | Hidden -> "hidden"
  | Scroll -> "scroll"
  | (Auto : overflow) -> "auto"
  | Clip -> "clip"

let string_of_flex_direction = function
  | Row -> "row"
  | Row_reverse -> "row-reverse"
  | Column -> "column"
  | Column_reverse -> "column-reverse"

let string_of_flex_wrap : flex_wrap -> string = function
  | Nowrap -> "nowrap"
  | Wrap -> "wrap"
  | Wrap_reverse -> "wrap-reverse"

let string_of_text_transform : text_transform -> string = function
  | None -> "none"
  | Uppercase -> "uppercase"
  | Lowercase -> "lowercase"
  | Capitalize -> "capitalize"
  | Full_width -> "full-width"
  | Full_size_kana -> "full-size-kana"
  | Inherit -> "inherit"

let string_of_box_sizing : box_sizing -> string = function
  | Border_box -> "border-box"
  | Content_box -> "content-box"
  | Inherit -> "inherit"

let string_of_white_space : white_space -> string = function
  | Normal -> "normal"
  | Nowrap -> "nowrap"
  | Pre -> "pre"
  | Pre_wrap -> "pre-wrap"
  | Pre_line -> "pre-line"
  | Break_spaces -> "break-spaces"
  | Inherit -> "inherit"

let string_of_table_layout : table_layout -> string = function
  | Auto -> "auto"
  | Fixed -> "fixed"
  | Inherit -> "inherit"

let string_of_resize : resize_value -> string = function
  | None -> "none"
  | Both -> "both"
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"
  | Block -> "block"
  | Inline -> "inline"
  | Inherit -> "inherit"

let string_of_object_fit : object_fit -> string = function
  | Fill -> "fill"
  | Contain -> "contain"
  | Cover -> "cover"
  | None -> "none"
  | Scale_down -> "scale-down"
  | Inherit -> "inherit"

let string_of_appearance : appearance_value -> string = function
  | (None : appearance_value) -> "none"
  | (Auto : appearance_value) -> "auto"
  | Button -> "button"
  | Textfield -> "textfield"
  | Menulist -> "menulist"
  | (Inherit : appearance_value) -> "inherit"

let string_of_vertical_align : vertical_align_value -> string = function
  | Baseline -> "baseline"
  | Top -> "top"
  | Middle -> "middle"
  | Bottom -> "bottom"
  | Text_top -> "text-top"
  | Text_bottom -> "text-bottom"
  | Sub -> "sub"
  | Super -> "super"
  | Length l -> string_of_length l
  | Percentage p -> pp_float p ^ "%"
  | (Inherit : vertical_align_value) -> "inherit"

let string_of_border_collapse : border_collapse_value -> string = function
  | Collapse -> "collapse"
  | Separate -> "separate"
  | (Inherit : border_collapse_value) -> "inherit"

let string_of_pointer_events : pointer_events_value -> string = function
  | (Auto : pointer_events_value) -> "auto"
  | (None : pointer_events_value) -> "none"
  | Visible_painted -> "visiblePainted"
  | Visible_fill -> "visibleFill"
  | Visible_stroke -> "visibleStroke"
  | Visible -> "visible"
  | Painted -> "painted"
  | Fill -> "fill"
  | Stroke -> "stroke"
  | All -> "all"
  | (Inherit : pointer_events_value) -> "inherit"

let string_of_text_decoration_style : text_decoration_style_value -> string =
  function
  | Solid -> "solid"
  | Double -> "double"
  | Dotted -> "dotted"
  | Dashed -> "dashed"
  | Wavy -> "wavy"
  | (Inherit : text_decoration_style_value) -> "inherit"

let string_of_webkit_font_smoothing : webkit_font_smoothing_value -> string =
  function
  | (Auto : webkit_font_smoothing_value) -> "auto"
  | (None : webkit_font_smoothing_value) -> "none"
  | Antialiased -> "antialiased"
  | Subpixel_antialiased -> "subpixel-antialiased"
  | (Inherit : webkit_font_smoothing_value) -> "inherit"

let string_of_moz_font_smoothing : moz_osx_font_smoothing_value -> string =
  function
  | (Auto : moz_osx_font_smoothing_value) -> "auto"
  | Grayscale -> "grayscale"
  | (Inherit : moz_osx_font_smoothing_value) -> "inherit"

let string_of_transform_style : transform_style_value -> string = function
  | Flat -> "flat"
  | Preserve_3d -> "preserve-3d"
  | (Inherit : transform_style_value) -> "inherit"

let string_of_backface_visibility : backface_visibility_value -> string =
  function
  | (Visible : backface_visibility_value) -> "visible"
  | Hidden -> "hidden"
  | (Inherit : backface_visibility_value) -> "inherit"

let string_of_scroll_behavior : scroll_behavior_value -> string = function
  | (Auto : scroll_behavior_value) -> "auto"
  | Smooth -> "smooth"
  | (Inherit : scroll_behavior_value) -> "inherit"

let string_of_scroll_snap_type : scroll_snap_type_value -> string = function
  | (None : scroll_snap_type_value) -> "none"
  | X_mandatory -> "x mandatory"
  | Y_mandatory -> "y mandatory"
  | Block_mandatory -> "block mandatory"
  | Inline_mandatory -> "inline mandatory"
  | Both_mandatory -> "both mandatory"
  | X_proximity -> "x proximity"
  | Y_proximity -> "y proximity"
  | Block_proximity -> "block proximity"
  | Inline_proximity -> "inline proximity"
  | Both_proximity -> "both proximity"
  | X_var var -> str [ "x "; var ]
  | Y_var var -> str [ "y "; var ]
  | Both_var var -> str [ "both "; var ]
  | (Inherit : scroll_snap_type_value) -> "inherit"

let string_of_scroll_snap_align : scroll_snap_align_value -> string = function
  | (None : scroll_snap_align_value) -> "none"
  | Start -> "start"
  | End -> "end"
  | (Center : scroll_snap_align_value) -> "center"
  | (Inherit : scroll_snap_align_value) -> "inherit"

let string_of_scroll_snap_stop : scroll_snap_stop_value -> string = function
  | Normal -> "normal"
  | Always -> "always"
  | (Inherit : scroll_snap_stop_value) -> "inherit"

let string_of_isolation : isolation_value -> string = function
  | (Auto : isolation_value) -> "auto"
  | Isolate -> "isolate"
  | (Inherit : isolation_value) -> "inherit"

let string_of_background_repeat : background_repeat_value -> string = function
  | Repeat -> "repeat"
  | Repeat_x -> "repeat-x"
  | Repeat_y -> "repeat-y"
  | No_repeat -> "no-repeat"
  | Space -> "space"
  | Round -> "round"
  | (Inherit : background_repeat_value) -> "inherit"

let string_of_container_type : container_type_value -> string = function
  | Normal -> "normal"
  | Size -> "size"
  | Inline_size -> "inline-size"
  | (Inherit : container_type_value) -> "inherit"

let string_of_flex_value = function
  | Initial -> "0 1 auto"
  | Auto -> "1 1 auto"
  | None -> "0 0 auto"
  | Grow f -> pp_float f (* Simplified flex shorthand for single grow value *)
  | Basis len -> str [ "1 1 "; string_of_length len ]
  | Grow_shrink (g, s) -> str [ pp_float g; " "; pp_float s; " 0%" ]
  | Full (g, s, b) ->
      str [ pp_float g; " "; pp_float s; " "; string_of_length b ]

let string_of_duration = function
  | Ms n -> str [ string_of_int n; "ms" ]
  | S f -> str [ pp_float f; "s" ]

let string_of_timing_function = function
  | Ease -> "ease"
  | Linear -> "linear"
  | Ease_in -> "ease-in"
  | Ease_out -> "ease-out"
  | Ease_in_out -> "ease-in-out"
  | Step_start -> "step-start"
  | Step_end -> "step-end"
  | Steps (n, dir) ->
      let dir_str = match dir with `Start -> "start" | `End -> "end" in
      str [ "steps("; string_of_int n; ", "; dir_str; ")" ]
  | Cubic_bezier (a, b, c, d) ->
      str
        [
          "cubic-bezier(";
          pp_float a;
          ", ";
          pp_float b;
          ", ";
          pp_float c;
          ", ";
          pp_float d;
          ")";
        ]

let string_of_transition_property : transition_property -> string = function
  | All -> "all"
  | None -> "none"
  | Property p -> p

let rec string_of_transition_value = function
  | Simple (prop, dur) ->
      str [ string_of_transition_property prop; " "; string_of_duration dur ]
  | With_timing (prop, dur, timing) ->
      str
        [
          string_of_transition_property prop;
          " ";
          string_of_duration dur;
          " ";
          string_of_timing_function timing;
        ]
  | With_delay (prop, dur, timing, delay) ->
      str
        [
          string_of_transition_property prop;
          " ";
          string_of_duration dur;
          " ";
          string_of_timing_function timing;
          " ";
          string_of_duration delay;
        ]
  | Multiple transitions ->
      String.concat ", " (List.map string_of_transition_value transitions)

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
  | Auto -> "auto"

let string_of_text_decoration : text_decoration -> string = function
  | None -> "none"
  | Underline -> "underline"
  | Overline -> "overline"
  | Line_through -> "line-through"
  | Inherit -> "inherit"
  | Underline_dotted -> "underline dotted"

let string_of_font_style = function
  | Font_normal -> "normal"
  | Italic -> "italic"
  | Oblique -> "oblique"
  | Font_inherit -> "inherit"

let string_of_list_style_type : list_style_type -> string = function
  | None -> "none"
  | Disc -> "disc"
  | Circle -> "circle"
  | Square -> "square"
  | Decimal -> "decimal"
  | Lower_alpha -> "lower-alpha"
  | Upper_alpha -> "upper-alpha"
  | Lower_roman -> "lower-roman"
  | Upper_roman -> "upper-roman"

let rec string_of_border_style : border_style -> string = function
  | None -> "none"
  | Solid -> "solid"
  | Dashed -> "dashed"
  | Dotted -> "dotted"
  | Double -> "double"
  | Groove -> "groove"
  | Ridge -> "ridge"
  | Inset -> "inset"
  | Outset -> "outset"
  | Hidden -> "hidden"
  | Var v -> string_of_var string_of_border_style v

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
  | Fr f -> str [ pp_float f; "fr" ]
  | Min_max (min, max) ->
      str
        [
          "minmax(";
          string_of_length min;
          ", ";
          string_of_grid_track_size max;
          ")";
        ]
  | Grid_auto -> "auto"
  | Max_content -> "max-content"
  | Min_content -> "min-content"
  | Fit_content l -> str [ "fit-content("; string_of_length l; ")" ]
  | Grid_length l -> string_of_length l

let string_of_grid_template = function
  | Tracks sizes -> String.concat " " (List.map string_of_grid_track_size sizes)
  | Repeat (count, size) ->
      str
        [
          "repeat(";
          string_of_int count;
          ", ";
          string_of_grid_track_size size;
          ")";
        ]
  | Repeat_auto_fill size ->
      str [ "repeat(auto-fill, "; string_of_grid_track_size size; ")" ]
  | Repeat_auto_fit size ->
      str [ "repeat(auto-fit, "; string_of_grid_track_size size; ")" ]
  | Grid_none -> "none"
  | Grid_inherit -> "inherit"

let string_of_angle = function
  | Deg f -> str [ pp_float f; "deg" ]
  | Rad f -> str [ pp_float f; "rad" ]
  | Turn f -> str [ pp_float f; "turn" ]
  | Grad f -> str [ pp_float f; "grad" ]
  | Angle_var { var_name; fallback } -> (
      match fallback with
      | None -> str [ "var(--"; var_name; ")" ]
      | Some f -> str [ "var(--"; var_name; ", "; pp_float f; ")" ])

let string_of_scale_value = function
  | Scale_num f -> pp_float f
  | Scale_var { var_name; fallback } -> (
      match fallback with
      | None -> str [ "var(--"; var_name; ")" ]
      | Some f -> str [ "var(--"; var_name; ", "; pp_float f; ")" ])

(* Helper functions for transform values *)
(* Helper functions for transform string generation *)
let transform_func name args = str ((name :: "(" :: args) @ [ ")" ])

let transform_var_func name var_name (fallback : string option) =
  match fallback with
  | None -> str [ name; "(var(--"; var_name; "))" ]
  | Some fb -> str [ name; "(var(--"; var_name; ", "; fb; "))" ]

let string_of_transform_value = function
  (* Translate transforms *)
  | Translate_x l -> transform_func "translateX" [ string_of_length l ]
  | Translate_y l -> transform_func "translateY" [ string_of_length l ]
  | Translate_z l -> transform_func "translateZ" [ string_of_length l ]
  | Translate (x, y) ->
      transform_func "translate"
        [ string_of_length x; ", "; string_of_length y ]
  | Translate_var { var_name; fallback } ->
      transform_var_func "translate" var_name fallback
  | Translate3d (x, y, z) ->
      transform_func "translate3d"
        [
          string_of_length x; ", "; string_of_length y; ", "; string_of_length z;
        ]
  (* Rotate transforms *)
  | Rotate_x a -> transform_func "rotateX" [ string_of_angle a ]
  | Rotate_y a -> transform_func "rotateY" [ string_of_angle a ]
  | Rotate_z a -> transform_func "rotateZ" [ string_of_angle a ]
  | Rotate a -> transform_func "rotate" [ string_of_angle a ]
  | Rotate_var { var_name; fallback } ->
      transform_var_func "rotate" var_name fallback
  | Rotate3d (x, y, z, angle) ->
      transform_func "rotate3d"
        [
          pp_float x;
          ", ";
          pp_float y;
          ", ";
          pp_float z;
          ", ";
          string_of_angle angle;
        ]
  (* Scale transforms *)
  | Scale_x s -> transform_func "scaleX" [ string_of_scale_value s ]
  | Scale_y s -> transform_func "scaleY" [ string_of_scale_value s ]
  | Scale_z s -> transform_func "scaleZ" [ string_of_scale_value s ]
  | Scale s -> transform_func "scale" [ string_of_scale_value s ]
  | Scale2 (x, y) ->
      transform_func "scale"
        [ string_of_scale_value x; ", "; string_of_scale_value y ]
  | Scale3d (x, y, z) ->
      transform_func "scale3d"
        [
          string_of_scale_value x;
          ", ";
          string_of_scale_value y;
          ", ";
          string_of_scale_value z;
        ]
  (* Skew transforms *)
  | Skew_x a -> transform_func "skewX" [ string_of_angle a ]
  | Skew_y a -> transform_func "skewY" [ string_of_angle a ]
  | Skew_var { var_name; fallback } ->
      transform_var_func "skew" var_name fallback
  | Skew (x, y) ->
      transform_func "skew" [ string_of_angle x; ", "; string_of_angle y ]
  (* Matrix transforms *)
  | Matrix (a, b, c, d, e, f) ->
      let values =
        [
          pp_float a; pp_float b; pp_float c; pp_float d; pp_float e; pp_float f;
        ]
      in
      str [ "matrix("; String.concat ", " values; ")" ]
  | Matrix3d
      (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16) ->
      let values =
        [
          pp_float m1;
          pp_float m2;
          pp_float m3;
          pp_float m4;
          pp_float m5;
          pp_float m6;
          pp_float m7;
          pp_float m8;
          pp_float m9;
          pp_float m10;
          pp_float m11;
          pp_float m12;
          pp_float m13;
          pp_float m14;
          pp_float m15;
          pp_float m16;
        ]
      in
      str [ "matrix3d("; String.concat ", " values; ")" ]
  (* Other transforms *)
  | Perspective l -> str [ "perspective("; string_of_length l; ")" ]
  | Transform_none -> "none"

type property =
  | Background_color
  | Color
  | Border_color
  | Border_style
  | Border_top_style
  | Border_right_style
  | Border_bottom_style
  | Border_left_style
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
  | Flex_basis
  | Order
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
  | Grid_auto_flow
  | Grid_auto_columns
  | Grid_auto_rows
  | Grid_column
  | Grid_row
  | Border_width
  | Border_top_width
  | Border_right_width
  | Border_bottom_width
  | Border_left_width
  | Border_radius
  | Border_top_color
  | Border_right_color
  | Border_bottom_color
  | Border_left_color
  | Box_shadow
  | Opacity
  | Transition
  | Transform
  | Scale
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
  | Rotate
  | Transition_duration
  | Transition_timing_function
  | Will_change
  | Contain
  | Isolation
  | Filter
  | Background_image
  | Animation
  | Overflow_x
  | Overflow_y
  | Vertical_align
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
  | Box_sizing
  | Resize
  | Object_fit
  | Appearance
  | Content
  | Quotes
  | Custom of string (* For arbitrary CSS properties - use sparingly! *)

type declaration = property * string
(** A CSS property as (name, value) pair *)

(* Helper to mark a declaration as important *)
let important (prop, value) = (prop, value ^ "!important")

(* Typed property constructors *)
let background_color c = (Background_color, string_of_color c)
let color c = (Color, string_of_color c)
let border_color c = (Border_color, string_of_color c)
let border_style bs = (Border_style, string_of_border_style bs)
let border_top_style bs = (Border_top_style, string_of_border_style bs)
let border_right_style bs = (Border_right_style, string_of_border_style bs)
let border_bottom_style bs = (Border_bottom_style, string_of_border_style bs)
let border_left_style bs = (Border_left_style, string_of_border_style bs)
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

let text_decoration_style value =
  (Text_decoration_style, string_of_text_decoration_style value)

let text_underline_offset value = (Text_underline_offset, value)
let text_transform value = (Text_transform, string_of_text_transform value)
let letter_spacing len = (Letter_spacing, string_of_length len)
let white_space value = (White_space, string_of_white_space value)
let display d = (Display, string_of_display d)
let position p = (Position, string_of_position p)
let top len = (Top, string_of_length len)
let right len = (Right, string_of_length len)
let bottom len = (Bottom, string_of_length len)
let left len = (Left, string_of_length len)
let opacity value = (Opacity, pp_float value)

(* Remove deprecated string-based versions *)
let flex_direction d = (Flex_direction, string_of_flex_direction d)
let flex value = (Flex, string_of_flex_value value)
let flex_grow value = (Flex_grow, pp_float value)
let flex_shrink value = (Flex_shrink, pp_float value)
let flex_wrap value = (Flex_wrap, string_of_flex_wrap value)
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
let table_layout value = (Table_layout, string_of_table_layout value)
let border_spacing len = (Border_spacing, string_of_length len)
let overflow o = (Overflow, string_of_overflow o)
let object_fit value = (Object_fit, string_of_object_fit value)
let clip value = (Clip, value)
let filter value = (Filter, value)
let background_image value = (Background_image, value)
let animation value = (Animation, value)

let grid_template_columns value =
  (Grid_template_columns, string_of_grid_template value)

let grid_template_rows value =
  (Grid_template_rows, string_of_grid_template value)

let pointer_events value = (Pointer_events, string_of_pointer_events value)
let z_index value = (Z_index, string_of_int value)
let appearance value = (Appearance, string_of_appearance value)
let overflow_x o = (Overflow_x, string_of_overflow o)
let overflow_y o = (Overflow_y, string_of_overflow o)
let resize value = (Resize, string_of_resize value)
let vertical_align value = (Vertical_align, string_of_vertical_align value)
let box_sizing value = (Box_sizing, string_of_box_sizing value)

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

let rec string_of_font_family_value = function
  (* Generic CSS font families *)
  | Sans_serif -> "sans-serif"
  | Serif -> "serif"
  | Monospace -> "monospace"
  | Cursive -> "cursive"
  | Fantasy -> "fantasy"
  | System_ui -> "system-ui"
  | Ui_sans_serif -> "ui-sans-serif"
  | Ui_serif -> "ui-serif"
  | Ui_monospace -> "ui-monospace"
  | Ui_rounded -> "ui-rounded"
  | Emoji -> "emoji"
  | Math -> "math"
  | Fangsong -> "fangsong"
  (* Popular web fonts *)
  | Inter -> "Inter"
  | Roboto -> "Roboto"
  | Open_sans -> "\"Open Sans\""
  | Lato -> "Lato"
  | Montserrat -> "Montserrat"
  | Poppins -> "Poppins"
  | Source_sans_pro -> "\"Source Sans Pro\""
  | Raleway -> "Raleway"
  | Oswald -> "Oswald"
  | Noto_sans -> "\"Noto Sans\""
  | Ubuntu -> "Ubuntu"
  | Playfair_display -> "\"Playfair Display\""
  | Merriweather -> "Merriweather"
  | Lora -> "Lora"
  | PT_sans -> "\"PT Sans\""
  | PT_serif -> "\"PT Serif\""
  | Nunito -> "Nunito"
  | Nunito_sans -> "\"Nunito Sans\""
  | Work_sans -> "\"Work Sans\""
  | Rubik -> "Rubik"
  | Fira_sans -> "\"Fira Sans\""
  | Fira_code -> "\"Fira Code\""
  | JetBrains_mono -> "\"JetBrains Mono\""
  | IBM_plex_sans -> "\"IBM Plex Sans\""
  | IBM_plex_serif -> "\"IBM Plex Serif\""
  | IBM_plex_mono -> "\"IBM Plex Mono\""
  | Source_code_pro -> "\"Source Code Pro\""
  | Space_mono -> "\"Space Mono\""
  | DM_sans -> "\"DM Sans\""
  | DM_serif_display -> "\"DM Serif Display\""
  | Bebas_neue -> "\"Bebas Neue\""
  | Barlow -> "Barlow"
  | Mulish -> "Mulish"
  | Josefin_sans -> "\"Josefin Sans\""
  (* Platform-specific fonts *)
  | Helvetica -> "Helvetica"
  | Helvetica_neue -> "\"Helvetica Neue\""
  | Arial -> "Arial"
  | Verdana -> "Verdana"
  | Tahoma -> "Tahoma"
  | Trebuchet_ms -> "\"Trebuchet MS\""
  | Times_new_roman -> "\"Times New Roman\""
  | Georgia -> "Georgia"
  | Garamond -> "Garamond"
  | Courier_new -> "\"Courier New\""
  | Courier -> "Courier"
  | Lucida_console -> "\"Lucida Console\""
  | SF_pro -> "\"SF Pro\""
  | SF_pro_display -> "\"SF Pro Display\""
  | SF_pro_text -> "\"SF Pro Text\""
  | SF_mono -> "\"SF Mono\""
  | NY -> "\"New York\""
  | Segoe_ui -> "\"Segoe UI\""
  | Segoe_ui_emoji -> "\"Segoe UI Emoji\""
  | Segoe_ui_symbol -> "\"Segoe UI Symbol\""
  | Apple_color_emoji -> "\"Apple Color Emoji\""
  | Noto_color_emoji -> "\"Noto Color Emoji\""
  | Android_emoji -> "\"Android Emoji\""
  | Twemoji_mozilla -> "\"Twemoji Mozilla\""
  (* Developer fonts *)
  | Menlo -> "Menlo"
  | Monaco -> "Monaco"
  | Consolas -> "Consolas"
  | Liberation_mono -> "\"Liberation Mono\""
  | SFMono_regular -> "SFMono-Regular"
  | Cascadia_code -> "\"Cascadia Code\""
  | Cascadia_mono -> "\"Cascadia Mono\""
  | Victor_mono -> "\"Victor Mono\""
  | Inconsolata -> "Inconsolata"
  | Hack -> "Hack"
  (* CSS keywords *)
  | Inherit -> "inherit"
  | Initial -> "initial"
  | Unset -> "unset"
  | Var { name; fallback } -> (
      match fallback with
      | None -> str [ "var(--"; name; ")" ]
      | Some fonts ->
          str
            [
              "var(--";
              name;
              ",";
              String.concat "," (List.map string_of_font_family_value fonts);
              ")";
            ])

let font_family fonts =
  (Font_family, String.concat ", " (List.map string_of_font_family_value fonts))

let moz_osx_font_smoothing value =
  (Moz_osx_font_smoothing, string_of_moz_font_smoothing value)

let webkit_line_clamp value = (Webkit_line_clamp, value)
let backdrop_filter value = (Backdrop_filter, value)
let background_position value = (Background_position, value)

let background_repeat value =
  (Background_repeat, string_of_background_repeat value)

let background_size value = (Background_size, value)

(* CSS Custom Properties *)
let custom_property name value : declaration =
  if not (String.starts_with ~prefix:"--" name) then
    invalid_arg
      (str [ "custom_property: name must start with '--', got: "; name ])
  else ((Custom name : property), value)

(* Additional property constructors *)
let content value = (Content, value)
let border_left_width len = (Border_left_width, string_of_length len)
let border_bottom_width len = (Border_bottom_width, string_of_length len)
let border_top_width len = (Border_top_width, string_of_length len)
let border_right_width len = (Border_right_width, string_of_length len)
let border_left_color c = (Border_left_color, string_of_color c)
let border_bottom_color c = (Border_bottom_color, string_of_color c)
let transition value = (Transition, string_of_transition_value value)
let quotes value = (Quotes, value)

(* New property constructors for tw.ml *)
let border value = (Border, value)
let tab_size value = (Tab_size, value)
let webkit_text_size_adjust value = (Webkit_text_size_adjust, value)
let font_feature_settings value = (Font_feature_settings, value)
let font_variation_settings value = (Font_variation_settings, value)
let webkit_tap_highlight_color value = (Webkit_tap_highlight_color, value)
let webkit_text_decoration value = (Webkit_text_decoration, value)
let text_indent len = (Text_indent, string_of_length len)
let border_collapse value = (Border_collapse, string_of_border_collapse value)
let list_style value = (List_style, value)
let font value = (Font, value)
let webkit_appearance value = (Webkit_appearance, value)

let webkit_font_smoothing value =
  (Webkit_font_smoothing, string_of_webkit_font_smoothing value)

let cursor c = (Cursor, string_of_cursor c)
let user_select u = (User_select, string_of_user_select u)
let container_type value = (Container_type, string_of_container_type value)
let container_name value = (Container_name, value)
let perspective len = (Perspective, string_of_length len)
let perspective_origin value = (Perspective_origin, value)
let transform_style value = (Transform_style, string_of_transform_style value)

let backface_visibility value =
  (Backface_visibility, string_of_backface_visibility value)

let object_position value = (Object_position, value)
let rotate a = (Rotate, string_of_angle a)

let transform values =
  let str = String.concat " " (List.map string_of_transform_value values) in
  (Transform, str)

let scale value = (Scale, value)
let transition_duration value = (Transition_duration, string_of_duration value)

let transition_timing_function value =
  (Transition_timing_function, string_of_timing_function value)

let will_change value = (Will_change, value)
let contain value = (Contain, value)
let isolation value = (Isolation, string_of_isolation value)
let padding_inline len = (Padding_inline, string_of_length len)
let padding_inline_start len = (Padding_inline_start, string_of_length len)
let padding_block len = (Padding_block, string_of_length len)
let margin_inline len = (Margin_inline, string_of_length len)
let margin_block len = (Margin_block, string_of_length len)
let margin_inline_end len = (Margin_inline_end, string_of_length len)
let outline value = (Outline, value)
let outline_offset len = (Outline_offset, string_of_length len)
let scroll_snap_type value = (Scroll_snap_type, string_of_scroll_snap_type value)

let scroll_snap_align value =
  (Scroll_snap_align, string_of_scroll_snap_align value)

let scroll_snap_stop value = (Scroll_snap_stop, string_of_scroll_snap_stop value)
let scroll_behavior value = (Scroll_behavior, string_of_scroll_behavior value)

(** Flex module *)
module Flex = struct
  let direction dir = flex_direction dir
  let wrap w = flex_wrap w
  let flex v = flex v
  let grow n = flex_grow n
  let shrink n = flex_shrink n
  let basis len = (Flex_basis, string_of_length len)
  let order n = (Order, string_of_int n)

  let align_items v =
    let str =
      match v with
      | `Flex_start -> "flex-start"
      | `Flex_end -> "flex-end"
      | `Center -> "center"
      | `Baseline -> "baseline"
      | `Stretch -> "stretch"
    in
    (Align_items, str)

  let align_self v =
    let str =
      match v with
      | `Auto -> "auto"
      | `Flex_start -> "flex-start"
      | `Flex_end -> "flex-end"
      | `Center -> "center"
      | `Baseline -> "baseline"
      | `Stretch -> "stretch"
    in
    (Align_self, str)

  let justify_content v =
    let str =
      match v with
      | `Flex_start -> "flex-start"
      | `Flex_end -> "flex-end"
      | `Center -> "center"
      | `Space_between -> "space-between"
      | `Space_around -> "space-around"
      | `Space_evenly -> "space-evenly"
    in
    (Justify_content, str)

  let gap len = (Gap, string_of_length len)
end

(** Grid module *)
module Grid = struct
  let template_columns template = grid_template_columns template
  let template_rows template = grid_template_rows template
  let column_gap len = (Column_gap, string_of_length len)
  let row_gap len = (Row_gap, string_of_length len)
  let gap len = (Gap, string_of_length len)

  let auto_flow flow =
    let str =
      match flow with
      | `Row -> "row"
      | `Column -> "column"
      | `Row_dense -> "row dense"
      | `Column_dense -> "column dense"
    in
    (Grid_auto_flow, str)

  let auto_columns size = (Grid_auto_columns, string_of_grid_track_size size)
  let auto_rows size = (Grid_auto_rows, string_of_grid_track_size size)
  let column value = (Grid_column, value)
  let row value = (Grid_row, value)
end

(** Border module *)
module Border = struct
  let width len = border_width len
  let style s = border_style s
  let color c = border_color c
  let radius len = border_radius len
  let top_width len = (Border_top_width, string_of_length len)
  let right_width len = (Border_right_width, string_of_length len)
  let bottom_width len = (Border_bottom_width, string_of_length len)
  let left_width len = (Border_left_width, string_of_length len)
  let top_color c = (Border_top_color, string_of_color c)
  let right_color c = (Border_right_color, string_of_color c)
  let bottom_color c = (Border_bottom_color, string_of_color c)
  let left_color c = (Border_left_color, string_of_color c)
end

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

let is_custom_property ((prop_name, _) : declaration) =
  match prop_name with
  | Custom name -> String.starts_with ~prefix:"--" name
  | _ -> false

let string_of_property = function
  | Background_color -> "background-color"
  | Color -> "color"
  | Border_color -> "border-color"
  | Border_style -> "border-style"
  | Border_top_style -> "border-top-style"
  | Border_right_style -> "border-right-style"
  | Border_bottom_style -> "border-bottom-style"
  | Border_left_style -> "border-left-style"
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
  | Flex_basis -> "flex-basis"
  | Order -> "order"
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
  | Grid_auto_flow -> "grid-auto-flow"
  | Grid_auto_columns -> "grid-auto-columns"
  | Grid_auto_rows -> "grid-auto-rows"
  | Grid_column -> "grid-column"
  | Grid_row -> "grid-row"
  | Border_width -> "border-width"
  | Border_top_width -> "border-top-width"
  | Border_right_width -> "border-right-width"
  | Border_bottom_width -> "border-bottom-width"
  | Border_left_width -> "border-left-width"
  | Border_radius -> "border-radius"
  | Border_top_color -> "border-top-color"
  | Border_right_color -> "border-right-color"
  | Border_bottom_color -> "border-bottom-color"
  | Border_left_color -> "border-left-color"
  | Box_shadow -> "box-shadow"
  | Opacity -> "opacity"
  | Transition -> "transition"
  | Transform -> "transform"
  | Scale -> "scale"
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
  | Rotate -> "rotate"
  | Transition_duration -> "transition-duration"
  | Transition_timing_function -> "transition-timing-function"
  | Will_change -> "will-change"
  | Contain -> "contain"
  | Isolation -> "isolation"
  | Filter -> "filter"
  | Background_image -> "background-image"
  | Animation -> "animation"
  | Overflow_x -> "overflow-x"
  | Overflow_y -> "overflow-y"
  | Vertical_align -> "vertical-align"
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
  | Box_sizing -> "box-sizing"
  | Resize -> "resize"
  | Object_fit -> "object-fit"
  | Appearance -> "appearance"
  | Content -> "content"
  | Quotes -> "quotes"
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
           let value =
             (* Convert transparent to #0000 for background-color in minified
                output *)
             if prop_name = Background_color && value = "transparent" then
               "#0000"
             else minify_value value
           in
           str [ string_of_property prop_name; ":"; value ])
  in
  str [ selector; "{"; str ~sep:";" props; "}" ]

let render_formatted_rule ?(indent = "") rule =
  let props =
    rule.declarations
    |> List.map (fun (prop_name, value) ->
           str [ indent; "  "; string_of_property prop_name; ": "; value; ";" ])
  in
  lines
    [ str [ indent; rule.selector; " {" ]; lines props; str [ indent; "}" ] ]

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
      else
        rules
        |> List.map (render_formatted_rule ~indent:"    ")
        |> String.concat "\n"
  | Support_nested (rules, nested_queries) ->
      let rules_str =
        if minify then
          rules |> merge_rules |> merge_by_properties
          |> List.map render_minified_rule
          |> String.concat ""
        else
          rules
          |> List.map (render_formatted_rule ~indent:"    ")
          |> String.concat "\n"
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

(* Generic at-rule rendering function *)
let render_at_rules ~config ~at_rule ~condition ~name_part ~rules ~indent =
  let content =
    if config.minify then
      rules |> merge_rules |> merge_by_properties
      |> List.map render_minified_rule
      |> String.concat ""
    else rules |> List.map (render_formatted_rule ~indent) |> String.concat "\n"
  in
  if config.minify then
    str [ "@"; at_rule; " "; name_part; condition; "{"; content; "}" ]
  else str [ "@"; at_rule; " "; name_part; condition; " {\n"; content; "\n}" ]

(* Helper: Render media queries *)
let render_layer_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         render_at_rules ~config ~at_rule:"media" ~condition:mq.media_condition
           ~name_part:"" ~rules:mq.media_rules ~indent:"  ")
  |> String.concat (if config.minify then "" else "\n")

(* Helper: Render container queries *)
let render_layer_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with
           | None -> ""
           | Some name -> str [ name; " " ]
         in
         render_at_rules ~config ~at_rule:"container"
           ~condition:cq.container_condition ~name_part
           ~rules:cq.container_rules ~indent:"  ")
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
         render_at_rules ~config ~at_rule:"media" ~condition:mq.media_condition
           ~name_part:"" ~rules:mq.media_rules ~indent:"")
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with
           | None -> ""
           | Some name -> str [ name; " " ]
         in
         render_at_rules ~config ~at_rule:"container"
           ~condition:cq.container_condition ~name_part
           ~rules:cq.container_rules ~indent:"")
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

let render_stylesheet_sections ~config stylesheet =
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
  ( rule_strings,
    at_property_strings,
    starting_style_strings,
    container_strings,
    supports_strings,
    media_strings )

let to_string ?(minify = false) stylesheet =
  let config = { minify } in
  let header_str =
    if List.length stylesheet.layers > 0 then str [ header; "\n" ] else ""
  in
  let layer_strings, empty_layers_decl =
    prepare_layer_strings ~config stylesheet
  in
  let ( rule_strings,
        at_property_strings,
        starting_style_strings,
        container_strings,
        supports_strings,
        media_strings ) =
    render_stylesheet_sections ~config stylesheet
  in
  let all_parts =
    [ header_str; "" ] @ layer_strings @ empty_layers_decl @ rule_strings
    @ starting_style_strings @ container_strings @ supports_strings
    @ media_strings @ at_property_strings
  in
  if config.minify then String.concat "" all_parts
  else String.concat "\n" (List.filter (fun s -> s <> "") all_parts)

let pp = to_string
