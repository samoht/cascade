(** CSS generation utilities *)

(* Simple string formatting utilities *)
module Pp = struct
  let str ?(sep = "") = function
    | [ x ] -> x
    | segments -> String.concat sep segments

  let lines segments = str ~sep:"\n" segments

  (* Float formatting from Pp *)
  let float f =
    (* Handle special float values *)
    if f = infinity then "3.40282e38"
    else if f = neg_infinity then "-3.40282e38"
    else if f <> f then "NaN"
      (* NaN check: NaN != NaN *)
      (* For whole numbers, use integer formatting *)
    else if f = floor f then string_of_int (int_of_float f)
    else
      let s = string_of_float f in
      (* Remove trailing dot if present *)
      let s =
        if String.ends_with ~suffix:"." s then
          String.sub s 0 (String.length s - 1)
        else s
      in
      (* Remove leading zero for decimals between -1 and 1 *)
      if String.starts_with ~prefix:"-0." s then
        str [ "-"; String.sub s 2 (String.length s - 2) ]
      else if String.starts_with ~prefix:"0." s then
        String.sub s 1 (String.length s - 1)
      else s

  let float_n decimals f =
    let multiplier = Float.pow 10.0 (float_of_int decimals) in
    let rounded = Float.round (f *. multiplier) /. multiplier in
    if Float.is_integer rounded then string_of_int (int_of_float rounded)
    else
      let whole = int_of_float (Float.floor rounded) in
      let frac_value = (rounded -. Float.floor rounded) *. multiplier in
      let frac_int = int_of_float (Float.round frac_value) in
      if frac_int = 0 then string_of_int whole
      else
        let frac_str = string_of_int frac_int in
        let padded_frac =
          let len = String.length frac_str in
          if len < decimals then
            str [ String.make (decimals - len) '0'; frac_str ]
          else frac_str
        in
        let rec remove_trailing_zeros s =
          let len = String.length s in
          if len > 0 && s.[len - 1] = '0' then
            remove_trailing_zeros (String.sub s 0 (len - 1))
          else s
        in
        let trimmed_frac = remove_trailing_zeros padded_frac in
        str [ string_of_int whole; "."; trimmed_frac ]
end

type meta = ..

type 'a var = {
  name : string;
  fallback : 'a option;
  default : 'a option;
  layer : string option;
  meta : meta option;
}

let var_name v = v.name
let var_layer v = v.layer

type any_var = V : 'a var -> any_var
type mode = Variables | Inline

let var_ref ?fallback ?default ?layer ?meta name =
  { name; fallback; default; layer; meta }

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

(** CSS length values *)
type calc_op = Add | Sub | Mult | Div

type 'a calc =
  | Var of 'a var (* CSS variable *)
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
  | Var of color var (* Use typed var instead of string *)
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
  | Webkit_box

(** CSS position values *)
type position = Static | Relative | Absolute | Fixed | Sticky

(** CSS visibility values *)
type visibility = Visible | Hidden | Collapse

(** CSS z-index values *)
type z_index = Auto | Index of int

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
type font_style = Normal | Italic | Oblique | Inherit

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

(** CSS list-style-position values *)
type list_style_position = Inside | Outside | Inherit

(** CSS list-style-image values *)
type list_style_image = None_img | Url of string | Inherit

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

(** CSS outline style values *)
type outline_style =
  | None
  | Auto
  | Dotted
  | Dashed
  | Solid
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Inherit

type forced_color_adjust = Auto | None | Inherit

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

type align_items = Flex_start | Flex_end | Center | Baseline | Stretch

type justify_content =
  | Flex_start
  | Flex_end
  | Center
  | Space_between
  | Space_around
  | Space_evenly

type align_self = Auto | Flex_start | Flex_end | Center | Baseline | Stretch
type border_collapse = Collapse | Separate | Inherit
type grid_auto_flow = Row | Column | Row_dense | Column_dense
type isolation = Auto | Isolate | Inherit
type clear = None | Left | Right | Both
type transform_style = Flat | Preserve_3d | Inherit
type backface_visibility = Visible | Hidden | Inherit
type resize = None | Both | Horizontal | Vertical | Block | Inline | Inherit
type object_fit = Fill | Contain | Cover | None | Scale_down | Inherit
type box_sizing = Border_box | Content_box | Inherit
type scroll_behavior = Auto | Smooth | Inherit
type scroll_snap_stop = Normal | Always | Inherit
type scroll_snap_strictness = Mandatory | Proximity
type scroll_snap_align = None | Start | End | Center | Inherit
type scroll_snap_axis = X | Y | Block | Inline | Both
type webkit_box_orient = Horizontal | Vertical | Inherit

type scroll_snap_type =
  | None
  | Axis of scroll_snap_axis * scroll_snap_strictness option
  | Inherit

type touch_action =
  | Auto
  | None
  | Pan_x
  | Pan_y
  | Manipulation
  | Pan_left
  | Pan_right
  | Pan_up
  | Pan_down

(** SVG paint values *)
type svg_paint = None | Current_color | Color of color

(** CSS text transform values *)
type text_transform =
  | None
  | Uppercase
  | Lowercase
  | Capitalize
  | Full_width
  | Full_size_kana
  | Inherit

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

(** CSS appearance values *)
type appearance = None | Auto | Button | Textfield | Menulist | Inherit

type vertical_align =
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

type text_overflow = Clip | Ellipsis | String of string | Inherit
type text_wrap = Wrap | No_wrap | Balance | Pretty | Inherit
type word_break = Normal | Break_all | Keep_all | Break_word | Inherit
type overflow_wrap = Normal_wrap | Anywhere | Break_word_wrap | Inherit
type hyphens = None_h | Manual | Auto | Inherit

type font_stretch =
  | Ultra_condensed
  | Extra_condensed
  | Condensed
  | Semi_condensed
  | Normal
  | Semi_expanded
  | Expanded
  | Extra_expanded
  | Ultra_expanded
  | Percentage of float
  | Inherit

type font_variant_numeric_token =
  | Ordinal
  | Slashed_zero
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Normal_numeric
  | Empty

type font_variant_numeric =
  | Tokens of font_variant_numeric_token list
  | Var of font_variant_numeric_token var
  | Composed of {
      ordinal : font_variant_numeric option;
      slashed_zero : font_variant_numeric option;
      numeric_figure : font_variant_numeric option;
      numeric_spacing : font_variant_numeric option;
      numeric_fraction : font_variant_numeric option;
    }

type pointer_events =
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
type text_decoration_style = Solid | Double | Dotted | Dashed | Wavy | Inherit

(** CSS webkit-font-smoothing values *)
type webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit

(** CSS -moz-osx-font-smoothing values *)
type moz_osx_font_smoothing = Auto | Grayscale | Inherit

(** CSS transform-style values *)

(** CSS backface-visibility values *)

(** CSS scroll-behavior values *)

(** CSS scroll-snap-type values *)

(** CSS scroll-snap-align values *)

(** CSS scroll-snap-stop values *)

(** CSS isolation values *)

(** CSS background-repeat values *)
type background_repeat =
  | Repeat
  | Repeat_x
  | Repeat_y
  | No_repeat
  | Space
  | Round
  | Inherit

(** CSS container-type values *)
type container_type = Normal | Size | Inline_size | Inherit

(** CSS aspect-ratio values *)
type aspect_ratio = Auto | Ratio of int * int | Number of float | Inherit

(** CSS justify-self values *)
type justify_self =
  | Auto
  | Start
  | End
  | Center
  | Stretch
  | Flex_start
  | Flex_end

(** CSS place-content values *)
type place_content =
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly

(** CSS flex shorthand values *)
type flex =
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
  | Var of duration var (* CSS variable reference *)

(** CSS blend mode values *)
type blend_mode =
  | Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | Color_dodge
  | Color_burn
  | Hard_light
  | Soft_light
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity
  | Inherit

(** CSS angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var (* CSS variable reference *)

type font_family =
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
  | Times
  | Georgia
  | Cambria
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
  | Var of font_family list var

type font_feature_settings =
  | Normal
  | Feature_string of string
  | Inherit
  | Var of font_feature_settings var

type font_variation_settings =
  | Normal
  | Variation_string of string
  | Inherit
  | Var of font_variation_settings var

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
type transition =
  | Simple of transition_property * duration
  | With_timing of transition_property * duration * timing_function
  | With_delay of transition_property * duration * timing_function * duration
  | Multiple of transition list

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
  | None
  | Inherit

(** CSS grid line values *)
type grid_line =
  | Line_number of int (* 1, 2, 3, ... or -1, -2, ... *)
  | Line_name of string (* "header-start", "main-end", etc. *)
  | Span of int (* span 2, span 3, etc. *)
  | Auto (* auto *)

(** CSS transform scale values *)
type scale = Num of float | Var of scale var

(** CSS transform values *)
type transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate3d of float * float * float * angle
  | Scale of scale * scale option
  | Scale_x of scale
  | Scale_y of scale
  | Scale_z of scale
  | Scale3d of scale * scale * scale
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
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
  | Var of transform list var
  | None

type shadow = {
  inset : bool;
  h_offset : length;
  v_offset : length;
  blur : length;
  spread : length;
  color : color;
}

type box_shadow = Shadow of shadow | Shadows of shadow list | None
type float_side = None | Left | Right

(** Value kind GADT for typed custom properties *)
type _ kind =
  | Length : length kind
  | Color : color kind
  | Int : int kind
  | Float : float kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Font_weight : font_weight kind
  | Font_family : font_family list kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Font_variant_numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Scale : scale kind
  | String : string kind

(* Convert CSS variable to string *)
let rec string_of_var : type a. ?mode:mode -> (a -> string) -> a var -> string =
 fun ?(mode = Variables) value_to_string v ->
  let var () =
    let base = Pp.str [ "var(--"; v.name ] in
    match v.fallback with
    | None -> Pp.str [ base; ")" ]
    | Some value -> Pp.str [ base; ", "; value_to_string value; ")" ]
  in
  match mode with
  | Variables -> var ()
  | Inline -> (
      match v.default with
      | Some value -> value_to_string value
      | None -> var ())

and string_of_length ?(mode = Variables) = function
  | Px n -> Pp.str [ string_of_int n; "px" ]
  | Rem f -> Pp.str [ Pp.float f; "rem" ]
  | Em f -> Pp.str [ Pp.float f; "em" ]
  | Pct f -> Pp.str [ Pp.float f; "%" ]
  | Vw f -> Pp.str [ Pp.float f; "vw" ]
  | Vh f -> Pp.str [ Pp.float f; "vh" ]
  | Ch f -> Pp.str [ Pp.float f; "ch" ]
  | Lh f -> Pp.str [ Pp.float f; "lh" ]
  | Num f -> Pp.float f
  | Auto -> "auto"
  | Zero -> "0"
  | Inherit -> "inherit"
  | Fit_content -> "fit-content"
  | Max_content -> "max-content"
  | Min_content -> "min-content"
  | Var v -> string_of_var ~mode (string_of_length ~mode) v
  | Calc cv -> (
      (* Optimize calc(infinity * 1px) to 3.40282e38px for minification *)
      match cv with
      | Expr (Val (Num f), Mult, Val (Px 1)) when f = infinity -> "3.40282e38px"
      | _ ->
          Pp.str
            [ "calc("; string_of_calc ~mode (string_of_length ~mode) cv; ")" ])

and string_of_calc : ?mode:mode -> ('a -> string) -> 'a calc -> string =
 fun ?(mode = Variables) string_of_val calc ->
  match calc with
  | Val v -> string_of_val v
  | Var v -> string_of_var ~mode string_of_val v
  | Expr (left, op, right) ->
      let op_str =
        match op with
        | Add -> " + "
        | Sub -> " - "
        | Mult -> " * "
        | Div -> " / "
      in
      Pp.str
        [
          string_of_calc ~mode string_of_val left;
          op_str;
          string_of_calc ~mode string_of_val right;
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

  let var : ?default:'a -> ?fallback:'a -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : length calc = Val (Num f)
  let infinity : length calc = Val (Num infinity)
  let px n = Val (Px n)
  let rem f = Val (Rem f)
  let em f = Val (Em f)
  let pct f = Val (Pct f)
end

let rec string_of_color_in_mix = function
  | Current -> "currentcolor" (* lowercase in color-mix *)
  | c -> string_of_color c

and string_of_color ?(mode = Variables) = function
  | Hex { hash; value } ->
      (* If hash was originally present, include it; for Tailwind arbitrary
         values without hash, omit it *)
      if hash then Pp.str [ "#"; value ]
      else if
        (* Also normalize by removing ONE leading zero from hex if starts with
           "00" *)
        String.length value = 6 && String.sub value 0 2 = "00"
      then String.sub value 1 5 (* 00ff00 -> 0ff00 *)
      else value
  | Rgb { r; g; b } ->
      Pp.str
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
      Pp.str
        [
          "rgba(";
          string_of_int r;
          ", ";
          string_of_int g;
          ", ";
          string_of_int b;
          ", ";
          Pp.float a;
          ")";
        ]
  | Oklch { l; c; h } ->
      let l_str = Pp.float_n 1 l in
      let c_str = Pp.float_n 3 c in
      let h_str = Pp.float_n 3 h in
      Pp.str [ "oklch("; l_str; "% "; c_str; " "; h_str; ")" ]
  | Var v -> string_of_var ~mode (string_of_color ~mode) v
  | Current -> "currentColor"
  | Transparent -> "transparent"
  | Inherit -> "inherit"
  | Mix { in_space; color1; percent1; color2; percent2 } ->
      let space_str = string_of_color_space in_space in
      let p1_str =
        match percent1 with
        | Some p -> Pp.str [ " "; string_of_int p; "%" ]
        | None -> ""
      in
      let p2_str =
        match percent2 with
        | Some p -> Pp.str [ " "; string_of_int p; "%" ]
        | None -> ""
      in
      Pp.str
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

let string_of_svg_paint ?(mode = Variables) (paint : svg_paint) =
  match paint with
  | None -> "none"
  | Current_color -> "currentColor"
  | Color c -> string_of_color ~mode c

let string_of_shadow ?(mode = Variables)
    { inset; h_offset; v_offset; blur; spread; color } =
  let h = string_of_length ~mode h_offset in
  let v = string_of_length ~mode v_offset in
  let b = string_of_length ~mode blur in
  let s = string_of_length ~mode spread in
  let c = string_of_color ~mode color in
  if inset then Pp.str [ "inset "; h; " "; v; " "; b; " "; s; " "; c ]
  else Pp.str [ h; " "; v; " "; b; " "; s; " "; c ]

let string_of_box_shadow ?(mode = Variables) : box_shadow -> string = function
  | None -> "none"
  | Shadow shadow -> string_of_shadow ~mode shadow
  | Shadows shadows ->
      shadows |> List.map (string_of_shadow ~mode) |> String.concat ", "

let string_of_blend_mode : blend_mode -> string = function
  | Normal -> "normal"
  | Multiply -> "multiply"
  | Screen -> "screen"
  | Overlay -> "overlay"
  | Darken -> "darken"
  | Lighten -> "lighten"
  | Color_dodge -> "color-dodge"
  | Color_burn -> "color-burn"
  | Hard_light -> "hard-light"
  | Soft_light -> "soft-light"
  | Difference -> "difference"
  | Exclusion -> "exclusion"
  | Hue -> "hue"
  | Saturation -> "saturation"
  | Color -> "color"
  | Luminosity -> "luminosity"
  | Inherit -> "inherit"

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
  | Webkit_box -> "-webkit-box"

let string_of_position = function
  | Static -> "static"
  | Relative -> "relative"
  | Absolute -> "absolute"
  | Fixed -> "fixed"
  | Sticky -> "sticky"

let string_of_visibility : visibility -> string = function
  | Visible -> "visible"
  | Hidden -> "hidden"
  | Collapse -> "collapse"

let string_of_z_index : z_index -> string = function
  | Auto -> "auto"
  | Index n -> string_of_int n

let rec string_of_font_weight = function
  | Weight n -> string_of_int n
  | Normal -> "normal"
  | Bold -> "bold"
  | Bolder -> "bolder"
  | Lighter -> "lighter"
  | Inherit -> "inherit"
  | Var v -> string_of_var string_of_font_weight v

let string_of_text_align : text_align -> string = function
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

let string_of_flex_direction : flex_direction -> string = function
  | Row -> "row"
  | Row_reverse -> "row-reverse"
  | Column -> "column"
  | Column_reverse -> "column-reverse"

let string_of_flex_wrap : flex_wrap -> string = function
  | Nowrap -> "nowrap"
  | Wrap -> "wrap"
  | Wrap_reverse -> "wrap-reverse"

let string_of_align_items : align_items -> string = function
  | Flex_start -> "flex-start"
  | Flex_end -> "flex-end"
  | Center -> "center"
  | Baseline -> "baseline"
  | Stretch -> "stretch"

let string_of_justify_content : justify_content -> string = function
  | Flex_start -> "flex-start"
  | Flex_end -> "flex-end"
  | Center -> "center"
  | Space_between -> "space-between"
  | Space_around -> "space-around"
  | Space_evenly -> "space-evenly"

let string_of_align_self : align_self -> string = function
  | Auto -> "auto"
  | Flex_start -> "flex-start"
  | Flex_end -> "flex-end"
  | Center -> "center"
  | Baseline -> "baseline"
  | Stretch -> "stretch"

let string_of_border_collapse : border_collapse -> string = function
  | Collapse -> "collapse"
  | Separate -> "separate"
  | Inherit -> "inherit"

let string_of_grid_auto_flow : grid_auto_flow -> string = function
  | Row -> "row"
  | Column -> "column"
  | Row_dense -> "row dense"
  | Column_dense -> "column dense"

let string_of_isolation : isolation -> string = function
  | Auto -> "auto"
  | Isolate -> "isolate"
  | Inherit -> "inherit"

let string_of_clear : clear -> string = function
  | None -> "none"
  | Left -> "left"
  | Right -> "right"
  | Both -> "both"

let string_of_float_side : float_side -> string = function
  | None -> "none"
  | Left -> "left"
  | Right -> "right"

let string_of_transform_style : transform_style -> string = function
  | Flat -> "flat"
  | Preserve_3d -> "preserve-3d"
  | Inherit -> "inherit"

let string_of_backface_visibility : backface_visibility -> string = function
  | Visible -> "visible"
  | Hidden -> "hidden"
  | Inherit -> "inherit"

let string_of_appearance : appearance -> string = function
  | None -> "none"
  | Auto -> "auto"
  | Button -> "button"
  | Textfield -> "textfield"
  | Menulist -> "menulist"
  | Inherit -> "inherit"

let string_of_resize : resize -> string = function
  | None -> "none"
  | Both -> "both"
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"
  | Block -> "block"
  | Inline -> "inline"
  | Inherit -> "inherit"

let string_of_webkit_box_orient : webkit_box_orient -> string = function
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"
  | Inherit -> "inherit"

let string_of_object_fit : object_fit -> string = function
  | Fill -> "fill"
  | Contain -> "contain"
  | Cover -> "cover"
  | None -> "none"
  | Scale_down -> "scale-down"
  | Inherit -> "inherit"

let string_of_box_sizing : box_sizing -> string = function
  | Border_box -> "border-box"
  | Content_box -> "content-box"
  | Inherit -> "inherit"

let string_of_scroll_behavior : scroll_behavior -> string = function
  | Auto -> "auto"
  | Smooth -> "smooth"
  | Inherit -> "inherit"

let string_of_scroll_snap_stop : scroll_snap_stop -> string = function
  | Normal -> "normal"
  | Always -> "always"
  | Inherit -> "inherit"

let string_of_scroll_snap_strictness : scroll_snap_strictness -> string =
  function
  | Mandatory -> "mandatory"
  | Proximity -> "proximity"

let string_of_scroll_snap_align : scroll_snap_align -> string = function
  | None -> "none"
  | Start -> "start"
  | End -> "end"
  | Center -> "center"
  | Inherit -> "inherit"

let string_of_scroll_snap_axis : scroll_snap_axis -> string = function
  | X -> "x"
  | Y -> "y"
  | Block -> "block"
  | Inline -> "inline"
  | Both -> "both"

let string_of_scroll_snap_type : scroll_snap_type -> string = function
  | None -> "none"
  | Axis (axis, strictness) -> (
      let axis_str = string_of_scroll_snap_axis axis in
      match strictness with
      | None -> axis_str
      | Some s -> Pp.str [ axis_str; " "; string_of_scroll_snap_strictness s ])
  | Inherit -> "inherit"

let string_of_touch_action : touch_action -> string = function
  | Auto -> "auto"
  | None -> "none"
  | Pan_x -> "pan-x"
  | Pan_y -> "pan-y"
  | Manipulation -> "manipulation"
  | Pan_left -> "pan-left"
  | Pan_right -> "pan-right"
  | Pan_up -> "pan-up"
  | Pan_down -> "pan-down"

let string_of_text_transform : text_transform -> string = function
  | None -> "none"
  | Uppercase -> "uppercase"
  | Lowercase -> "lowercase"
  | Capitalize -> "capitalize"
  | Full_width -> "full-width"
  | Full_size_kana -> "full-size-kana"
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

let string_of_vertical_align : vertical_align -> string = function
  | Baseline -> "baseline"
  | Top -> "top"
  | Middle -> "middle"
  | Bottom -> "bottom"
  | Text_top -> "text-top"
  | Text_bottom -> "text-bottom"
  | Sub -> "sub"
  | Super -> "super"
  | Length l -> string_of_length l
  | Percentage p -> Pp.str [ Pp.float p; "%" ]
  | Inherit -> "inherit"

let string_of_pointer_events : pointer_events -> string = function
  | Auto -> "auto"
  | None -> "none"
  | Visible_painted -> "visiblePainted"
  | Visible_fill -> "visibleFill"
  | Visible_stroke -> "visibleStroke"
  | Visible -> "visible"
  | Painted -> "painted"
  | Fill -> "fill"
  | Stroke -> "stroke"
  | All -> "all"
  | Inherit -> "inherit"

let string_of_text_decoration_style : text_decoration_style -> string = function
  | Solid -> "solid"
  | Double -> "double"
  | Dotted -> "dotted"
  | Dashed -> "dashed"
  | Wavy -> "wavy"
  | Inherit -> "inherit"

let string_of_webkit_font_smoothing : webkit_font_smoothing -> string = function
  | Auto -> "auto"
  | None -> "none"
  | Antialiased -> "antialiased"
  | Subpixel_antialiased -> "subpixel-antialiased"
  | Inherit -> "inherit"

let string_of_moz_font_smoothing : moz_osx_font_smoothing -> string = function
  | Auto -> "auto"
  | Grayscale -> "grayscale"
  | Inherit -> "inherit"

let string_of_background_repeat : background_repeat -> string = function
  | Repeat -> "repeat"
  | Repeat_x -> "repeat-x"
  | Repeat_y -> "repeat-y"
  | No_repeat -> "no-repeat"
  | Space -> "space"
  | Round -> "round"
  | Inherit -> "inherit"

let string_of_container_type : container_type -> string = function
  | Normal -> "normal"
  | Size -> "size"
  | Inline_size -> "inline-size"
  | Inherit -> "inherit"

let string_of_flex : flex -> string = function
  | Initial -> "0 1 auto"
  | Auto -> "1 1 auto"
  | None -> "0 0 auto"
  | Grow f -> Pp.float f
  | Basis len -> Pp.str [ "1 1 "; string_of_length len ]
  | Grow_shrink (g, s) -> Pp.str [ Pp.float g; " "; Pp.float s; " 0%" ]
  | Full (g, s, b) ->
      Pp.str [ Pp.float g; " "; Pp.float s; " "; string_of_length b ]

let rec string_of_duration = function
  | Ms n -> Pp.str [ string_of_int n; "ms" ]
  | S f -> Pp.str [ Pp.float f; "s" ]
  | Var v -> string_of_var string_of_duration v

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
      Pp.str [ "steps("; string_of_int n; ", "; dir_str; ")" ]
  | Cubic_bezier (a, b, c, d) ->
      Pp.str
        [
          "cubic-bezier(";
          Pp.float a;
          ", ";
          Pp.float b;
          ", ";
          Pp.float c;
          ", ";
          Pp.float d;
          ")";
        ]

let string_of_transition_property : transition_property -> string = function
  | All -> "all"
  | None -> "none"
  | Property p -> p

let rec string_of_transition = function
  | Simple (prop, dur) ->
      Pp.str [ string_of_transition_property prop; " "; string_of_duration dur ]
  | With_timing (prop, dur, timing) ->
      Pp.str
        [
          string_of_transition_property prop;
          " ";
          string_of_duration dur;
          " ";
          string_of_timing_function timing;
        ]
  | With_delay (prop, dur, timing, delay) ->
      Pp.str
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
      String.concat ", " (List.map string_of_transition transitions)

let string_of_align : align -> string = function
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

let string_of_justify_self : justify_self -> string = function
  | Auto -> "auto"
  | Start -> "start"
  | End -> "end"
  | Center -> "center"
  | Stretch -> "stretch"
  | Flex_start -> "flex-start"
  | Flex_end -> "flex-end"

let string_of_aspect_ratio : aspect_ratio -> string = function
  | Auto -> "auto"
  | Ratio (w, h) -> Pp.str [ string_of_int w; "/"; string_of_int h ]
  | Number f -> Pp.float f
  | Inherit -> "inherit"

let string_of_place_content : place_content -> string = function
  | Start -> "start"
  | End -> "end"
  | Center -> "center"
  | Stretch -> "stretch"
  | Space_between -> "space-between"
  | Space_around -> "space-around"
  | Space_evenly -> "space-evenly"

let string_of_text_decoration : text_decoration -> string = function
  | None -> "none"
  | Underline -> "underline"
  | Overline -> "overline"
  | Line_through -> "line-through"
  | Inherit -> "inherit"
  | Underline_dotted -> "underline dotted"

let string_of_font_style : font_style -> string = function
  | Normal -> "normal"
  | Italic -> "italic"
  | Oblique -> "oblique"
  | Inherit -> "inherit"

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

let string_of_list_style_position = function
  | Inside -> "inside"
  | Outside -> "outside"
  | Inherit -> "inherit"

let string_of_list_style_image = function
  | None_img -> "none"
  | Url u -> Pp.str [ "url("; u; ")" ]
  | Inherit -> "inherit"

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

let string_of_outline_style : outline_style -> string = function
  | None -> "none"
  | Auto -> "auto"
  | Dotted -> "dotted"
  | Dashed -> "dashed"
  | Solid -> "solid"
  | Double -> "double"
  | Groove -> "groove"
  | Ridge -> "ridge"
  | Inset -> "inset"
  | Outset -> "outset"
  | Inherit -> "inherit"

let string_of_forced_color_adjust : forced_color_adjust -> string = function
  | Auto -> "auto"
  | None -> "none"
  | Inherit -> "inherit"

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
  | Fr f -> Pp.str [ Pp.float f; "fr" ]
  | Min_max (min, max) ->
      Pp.str
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
  | Fit_content l -> Pp.str [ "fit-content("; string_of_length l; ")" ]
  | Grid_length l -> string_of_length l

let string_of_grid_template = function
  | Tracks sizes -> String.concat " " (List.map string_of_grid_track_size sizes)
  | Repeat (count, size) ->
      Pp.str
        [
          "repeat(";
          string_of_int count;
          ", ";
          string_of_grid_track_size size;
          ")";
        ]
  | Repeat_auto_fill size ->
      Pp.str [ "repeat(auto-fill, "; string_of_grid_track_size size; ")" ]
  | Repeat_auto_fit size ->
      Pp.str [ "repeat(auto-fit, "; string_of_grid_track_size size; ")" ]
  | None -> "none"
  | Inherit -> "inherit"

let string_of_grid_line = function
  | Line_number n -> string_of_int n
  | Line_name name -> name
  | Span n -> Pp.str [ "span "; string_of_int n ]
  | Auto -> "auto"

let rec string_of_angle = function
  | Deg f -> Pp.str [ Pp.float f; "deg" ]
  | Rad f -> Pp.str [ Pp.float f; "rad" ]
  | Turn f -> Pp.str [ Pp.float f; "turn" ]
  | Grad f -> Pp.str [ Pp.float f; "grad" ]
  | Var v -> string_of_var string_of_angle v

let rec string_of_scale = function
  | Num f -> Pp.float f
  | Var v -> string_of_var string_of_scale v

let rec string_of_font_feature_settings : font_feature_settings -> string =
  function
  | Normal -> "normal"
  | Feature_string s -> s
  | Inherit -> "inherit"
  | Var v -> string_of_var string_of_font_feature_settings v

let rec string_of_font_variation_settings : font_variation_settings -> string =
  function
  | Normal -> "normal"
  | Variation_string s -> s
  | Inherit -> "inherit"
  | Var v -> string_of_var string_of_font_variation_settings v

let transform_func name args = Pp.str ((name :: "(" :: args) @ [ ")" ])

let rec string_of_transform = function
  (* Translate transforms *)
  | Translate (x, None) -> transform_func "translate" [ string_of_length x ]
  | Translate (x, Some y) ->
      transform_func "translate"
        [ string_of_length x; ", "; string_of_length y ]
  | Translate_x l -> transform_func "translateX" [ string_of_length l ]
  | Translate_y l -> transform_func "translateY" [ string_of_length l ]
  | Translate_z l -> transform_func "translateZ" [ string_of_length l ]
  | Translate3d (x, y, z) ->
      transform_func "translate3d"
        [
          string_of_length x; ", "; string_of_length y; ", "; string_of_length z;
        ]
  (* Rotate transforms *)
  | Rotate a -> transform_func "rotate" [ string_of_angle a ]
  | Rotate_x a -> transform_func "rotateX" [ string_of_angle a ]
  | Rotate_y a -> transform_func "rotateY" [ string_of_angle a ]
  | Rotate_z a -> transform_func "rotateZ" [ string_of_angle a ]
  | Rotate3d (x, y, z, angle) ->
      transform_func "rotate3d"
        [
          Pp.float x;
          ", ";
          Pp.float y;
          ", ";
          Pp.float z;
          ", ";
          string_of_angle angle;
        ]
  (* Scale transforms *)
  | Scale (x, None) -> transform_func "scale" [ string_of_scale x ]
  | Scale (x, Some y) ->
      transform_func "scale" [ string_of_scale x; ", "; string_of_scale y ]
  | Scale_x s -> transform_func "scaleX" [ string_of_scale s ]
  | Scale_y s -> transform_func "scaleY" [ string_of_scale s ]
  | Scale_z s -> transform_func "scaleZ" [ string_of_scale s ]
  | Scale3d (x, y, z) ->
      transform_func "scale3d"
        [ string_of_scale x; ", "; string_of_scale y; ", "; string_of_scale z ]
  (* Skew transforms *)
  | Skew (x, None) -> transform_func "skewX" [ string_of_angle x ]
  | Skew (x, Some y) ->
      transform_func "skew" [ string_of_angle x; ", "; string_of_angle y ]
  | Skew_x a -> transform_func "skewX" [ string_of_angle a ]
  | Skew_y a -> transform_func "skewY" [ string_of_angle a ]
  (* Matrix transforms *)
  | Matrix (a, b, c, d, e, f) ->
      let values =
        [
          Pp.float a; Pp.float b; Pp.float c; Pp.float d; Pp.float e; Pp.float f;
        ]
      in
      Pp.str [ "matrix("; String.concat ", " values; ")" ]
  | Matrix3d
      (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16) ->
      let values =
        [
          Pp.float m1;
          Pp.float m2;
          Pp.float m3;
          Pp.float m4;
          Pp.float m5;
          Pp.float m6;
          Pp.float m7;
          Pp.float m8;
          Pp.float m9;
          Pp.float m10;
          Pp.float m11;
          Pp.float m12;
          Pp.float m13;
          Pp.float m14;
          Pp.float m15;
          Pp.float m16;
        ]
      in
      Pp.str [ "matrix3d("; String.concat ", " values; ")" ]
  (* Other transforms *)
  | Perspective l -> Pp.str [ "perspective("; string_of_length l; ")" ]
  | Var v ->
      let string_of_transforms ts =
        Pp.str ~sep:" " (List.map string_of_transform ts)
      in
      string_of_var string_of_transforms v
  | None -> "none"

let string_of_font_variant_numeric_token = function
  | Ordinal -> "ordinal"
  | Slashed_zero -> "slashed-zero"
  | Lining_nums -> "lining-nums"
  | Oldstyle_nums -> "oldstyle-nums"
  | Proportional_nums -> "proportional-nums"
  | Tabular_nums -> "tabular-nums"
  | Diagonal_fractions -> "diagonal-fractions"
  | Stacked_fractions -> "stacked-fractions"
  | Normal_numeric -> "normal"
  | Empty -> ""

let rec string_of_font_variant_numeric mode : font_variant_numeric -> string =
  function
  | Tokens tokens ->
      String.concat " " (List.map string_of_font_variant_numeric_token tokens)
  | Var v -> string_of_var ~mode string_of_font_variant_numeric_token v
  | Composed
      {
        ordinal;
        slashed_zero;
        numeric_figure;
        numeric_spacing;
        numeric_fraction;
      } ->
      let values =
        List.map
          (function
            | Some o -> string_of_font_variant_numeric mode o | None -> "")
          [
            ordinal;
            slashed_zero;
            numeric_figure;
            numeric_spacing;
            numeric_fraction;
          ]
      in
      String.concat "" values

type 'a property =
  | Background_color : color property
  | Color : color property
  | Border_color : color property
  | Border_style : border_style property
  | Border_top_style : border_style property
  | Border_right_style : border_style property
  | Border_bottom_style : border_style property
  | Border_left_style : border_style property
  | Padding : length property
  | Padding_left : length property
  | Padding_right : length property
  | Padding_bottom : length property
  | Padding_top : length property
  | Padding_inline : length property
  | Padding_inline_start : length property
  | Padding_block : length property
  | Margin : length property
  | Margin_inline_end : length property
  | Margin_left : length property
  | Margin_right : length property
  | Margin_top : length property
  | Margin_bottom : length property
  | Margin_inline : length property
  | Margin_block : length property
  | Gap : length property
  | Column_gap : length property
  | Row_gap : length property
  | Width : length property
  | Height : length property
  | Min_width : length property
  | Min_height : length property
  | Max_width : length property
  | Max_height : length property
  | Font_size : length property
  | Line_height : length property
  | Font_weight : font_weight property
  | Font_style : font_style property
  | Text_align : text_align property
  | Text_decoration : text_decoration property
  | Text_decoration_style : text_decoration_style property
  | Text_decoration_color : color property
  | Text_underline_offset : string property
  | Text_transform : text_transform property
  | Letter_spacing : length property
  | List_style_type : list_style_type property
  | List_style_position : list_style_position property
  | List_style_image : list_style_image property
  | Display : display property
  | Position : position property
  | Visibility : visibility property
  | Flex_direction : flex_direction property
  | Flex_wrap : flex_wrap property
  | Flex : string property
  | Flex_grow : float property
  | Flex_shrink : float property
  | Flex_basis : string property
  | Order : int property
  | Align_items : align_items property
  | Justify_content : justify_content property
  | Justify_items : justify_self property
  | Justify_self : justify_self property
  | Align_content : align property
  | Align_self : align_self property
  | Place_content : place_content property
  | Place_items : place_content property
  | Place_self : string property
  | Grid_template_columns : grid_template property
  | Grid_template_rows : grid_template property
  | Grid_auto_flow : grid_auto_flow property
  | Grid_auto_columns : grid_track_size property
  | Grid_auto_rows : grid_track_size property
  | Grid_column : string property
  | Grid_row : string property
  | Grid_column_start : grid_line property
  | Grid_column_end : grid_line property
  | Grid_row_start : grid_line property
  | Grid_row_end : grid_line property
  | Border_width : length property
  | Border_top_width : length property
  | Border_right_width : length property
  | Border_bottom_width : length property
  | Border_left_width : length property
  | Border_radius : length property
  | Border_top_color : color property
  | Border_right_color : color property
  | Border_bottom_color : color property
  | Border_left_color : color property
  | Opacity : float property
  | Mix_blend_mode : blend_mode property
  | Transform : transform list property
  | Cursor : cursor property
  | Table_layout : table_layout property
  | Border_collapse : border_collapse property
  | Border_spacing : length property
  | User_select : user_select property
  | Pointer_events : pointer_events property
  | Overflow : overflow property
  | Top : length property
  | Right : length property
  | Bottom : length property
  | Left : length property
  | Z_index : z_index property
  | Outline : string property
  | Outline_style : outline_style property
  | Outline_width : length property
  | Outline_color : color property
  | Outline_offset : length property
  | Forced_color_adjust : forced_color_adjust property
  | Scroll_snap_type : scroll_snap_type property
  | White_space : white_space property
  | Border : string property
  | Tab_size : int property
  | Webkit_text_size_adjust : string property
  | Font_feature_settings : font_feature_settings property
  | Font_variation_settings : font_variation_settings property
  | Webkit_tap_highlight_color : color property
  | Webkit_text_decoration : text_decoration property
  | Webkit_text_decoration_color : color property
  | Text_indent : length property
  | List_style : string property
  | Font : string property
  | Webkit_appearance : appearance property
  | Container_type : container_type property
  | Container_name : string property
  | Perspective : length property
  | Perspective_origin : string property
  | Transform_style : transform_style property
  | Backface_visibility : backface_visibility property
  | Object_position : string property
  | Rotate : angle property
  | Transition_duration : duration property
  | Transition_timing_function : timing_function property
  | Transition_delay : duration property
  | Will_change : string property
  | Contain : string property
  | Isolation : isolation property
  | Filter : string property
  | Background_image : string property
  | Animation : string property
  | Aspect_ratio : aspect_ratio property
  | Overflow_x : overflow property
  | Overflow_y : overflow property
  | Vertical_align : vertical_align property
  | Font_family : string property
  | Background_position : string property
  | Background_repeat : background_repeat property
  | Background_size : string property
  | Webkit_font_smoothing : webkit_font_smoothing property
  | Moz_osx_font_smoothing : string property
  | Webkit_line_clamp : int property
  | Webkit_box_orient : webkit_box_orient property
  | Text_overflow : text_overflow property
  | Text_wrap : text_wrap property
  | Word_break : word_break property
  | Overflow_wrap : overflow_wrap property
  | Hyphens : hyphens property
  | Webkit_hyphens : hyphens property
  | Font_stretch : font_stretch property
  | Font_variant_numeric : font_variant_numeric property
  | Backdrop_filter : string property
  | Scroll_snap_align : scroll_snap_align property
  | Scroll_snap_stop : scroll_snap_stop property
  | Scroll_behavior : scroll_behavior property
  | Box_sizing : box_sizing property
  | Resize : resize property
  | Object_fit : object_fit property
  | Appearance : appearance property
  | Content : string property
  | Quotes : string property
  | Text_decoration_thickness : string property
  | Text_size_adjust : string property
  | Touch_action : touch_action property
  | Clip : string property
  | Clear : clear property
  | Float : float_side property
  | Scale : string property
  | Transition : transition property
  | Box_shadow : box_shadow property
  | Fill : svg_paint property
  | Stroke : svg_paint property
  | Stroke_width : length property

type declaration =
  | Declaration : 'a property * 'a -> declaration
      (** A CSS property-value pair with typed value using existential type *)
  | Custom_declaration : {
      name : string;
      kind : 'a kind;
      value : 'a;
      layer : string option;
      meta : meta option;
    }
      -> declaration
      (** Custom property with dynamic name, typed value via value kind, and
          optional layer info *)
  | Important_declaration : 'a property * 'a -> declaration
      (** A CSS property-value pair marked as !important *)

(* Extract metadata from a declaration *)
let declaration_meta : declaration -> meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None
  | Important_declaration _ -> None

(* Helper to mark a declaration as important - needs special handling for
   GADT *)
let important = function
  | Declaration (prop, value) -> Important_declaration (prop, value)
  | Custom_declaration { name; kind; value; layer; meta } ->
      Custom_declaration { name; kind; value; layer; meta }
      (* Custom properties remain as-is; we don't attach !important here *)
  | Important_declaration (prop, value) -> Important_declaration (prop, value)
(* Already important *)

(* Helper for raw custom properties - primarily for internal use *)
let custom_property ?layer name value =
  Custom_declaration { name; kind = String; value; layer; meta = None }

(* Access the layer associated with a custom declaration, if any *)
let custom_declaration_layer = function
  | Custom_declaration { layer; _ } -> layer
  | Declaration _ -> None
  | Important_declaration _ -> None

(* Convert property value to string based on its type *)
let string_of_property_value : type a. ?mode:mode -> a property -> a -> string =
 fun ?(mode = Variables) prop value ->
  match prop with
  | Background_color -> string_of_color ~mode value
  | Color -> string_of_color ~mode value
  | Border_color -> string_of_color ~mode value
  | Border_style -> string_of_border_style value
  | Border_top_style -> string_of_border_style value
  | Border_right_style -> string_of_border_style value
  | Border_bottom_style -> string_of_border_style value
  | Border_left_style -> string_of_border_style value
  | Padding -> string_of_length ~mode value
  | Padding_left -> string_of_length ~mode value
  | Padding_right -> string_of_length ~mode value
  | Padding_bottom -> string_of_length ~mode value
  | Padding_top -> string_of_length ~mode value
  | Padding_inline -> string_of_length ~mode value
  | Padding_inline_start -> string_of_length ~mode value
  | Padding_block -> string_of_length ~mode value
  | Margin -> string_of_length ~mode value
  | Margin_inline_end -> string_of_length ~mode value
  | Margin_left -> string_of_length ~mode value
  | Margin_right -> string_of_length ~mode value
  | Margin_top -> string_of_length ~mode value
  | Margin_bottom -> string_of_length ~mode value
  | Margin_inline -> string_of_length ~mode value
  | Margin_block -> string_of_length ~mode value
  | Gap -> string_of_length ~mode value
  | Column_gap -> string_of_length ~mode value
  | Row_gap -> string_of_length ~mode value
  | Width -> string_of_length ~mode value
  | Height -> string_of_length ~mode value
  | Min_width -> string_of_length ~mode value
  | Min_height -> string_of_length ~mode value
  | Max_width -> string_of_length ~mode value
  | Max_height -> string_of_length ~mode value
  | Font_size -> string_of_length ~mode value
  | Line_height -> string_of_length ~mode value
  | Font_weight -> string_of_font_weight value
  | Display -> string_of_display value
  | Position -> string_of_position value
  | Visibility -> string_of_visibility value
  | Align_items -> string_of_align_items value
  | Justify_content -> string_of_justify_content value
  | Justify_items -> string_of_justify_self value
  | Align_self -> string_of_align_self value
  | Border_collapse -> string_of_border_collapse value
  | Table_layout -> string_of_table_layout value
  | Grid_auto_flow -> string_of_grid_auto_flow value
  | Opacity -> Pp.float value
  | Mix_blend_mode -> string_of_blend_mode value
  | Z_index -> string_of_z_index value
  | Tab_size -> string_of_int value
  | Webkit_line_clamp -> string_of_int value
  | Webkit_box_orient -> string_of_webkit_box_orient value
  | Top -> string_of_length ~mode value
  | Right -> string_of_length ~mode value
  | Bottom -> string_of_length ~mode value
  | Left -> string_of_length ~mode value
  | Border_width -> string_of_length ~mode value
  | Border_top_width -> string_of_length ~mode value
  | Border_right_width -> string_of_length ~mode value
  | Border_bottom_width -> string_of_length ~mode value
  | Border_left_width -> string_of_length ~mode value
  | Border_radius -> string_of_length ~mode value
  | Border_top_color -> string_of_color ~mode value
  | Border_right_color -> string_of_color ~mode value
  | Border_bottom_color -> string_of_color ~mode value
  | Border_left_color -> string_of_color ~mode value
  | Text_decoration_color -> string_of_color ~mode value
  | Webkit_text_decoration_color -> string_of_color ~mode value
  | Webkit_tap_highlight_color -> string_of_color ~mode value
  | Text_indent -> string_of_length ~mode value
  | Border_spacing -> string_of_length ~mode value
  | Outline_offset -> string_of_length ~mode value
  | Perspective -> string_of_length ~mode value
  | Transform -> List.map string_of_transform value |> String.concat " "
  | Isolation -> string_of_isolation value
  | Transform_style -> string_of_transform_style value
  | Backface_visibility -> string_of_backface_visibility value
  | Scroll_snap_align -> string_of_scroll_snap_align value
  | Scroll_snap_stop -> string_of_scroll_snap_stop value
  | Scroll_behavior -> string_of_scroll_behavior value
  | Box_sizing -> string_of_box_sizing value
  | Resize -> string_of_resize value
  | Object_fit -> string_of_object_fit value
  | Appearance -> string_of_appearance value
  | Flex_grow -> Pp.float value
  | Flex_shrink -> Pp.float value
  | Order -> string_of_int value
  | Flex_direction -> string_of_flex_direction value
  | Flex_wrap -> string_of_flex_wrap value
  | Font_style -> string_of_font_style value
  | Text_align -> string_of_text_align value
  | Text_decoration -> string_of_text_decoration value
  | Text_decoration_style -> string_of_text_decoration_style value
  | Text_transform -> string_of_text_transform value
  | List_style_type -> string_of_list_style_type value
  | List_style_position -> string_of_list_style_position value
  | List_style_image -> string_of_list_style_image value
  | Overflow -> string_of_overflow value
  | Overflow_x -> string_of_overflow value
  | Overflow_y -> string_of_overflow value
  | Vertical_align -> string_of_vertical_align value
  | Text_overflow -> (
      match value with
      | Clip -> "clip"
      | Ellipsis -> "ellipsis"
      | String s -> s
      | Inherit -> "inherit")
  | Text_wrap -> (
      match value with
      | Wrap -> "wrap"
      | No_wrap -> "nowrap"
      | Balance -> "balance"
      | Pretty -> "pretty"
      | Inherit -> "inherit")
  | Word_break -> (
      match value with
      | Normal -> "normal"
      | Break_all -> "break-all"
      | Keep_all -> "keep-all"
      | Break_word -> "break-word"
      | Inherit -> "inherit")
  | Overflow_wrap -> (
      match value with
      | Normal_wrap -> "normal"
      | Anywhere -> "anywhere"
      | Break_word_wrap -> "break-word"
      | Inherit -> "inherit")
  | Hyphens -> (
      match value with
      | None_h -> "none"
      | Manual -> "manual"
      | Auto -> "auto"
      | Inherit -> "inherit")
  | Webkit_hyphens -> (
      match value with
      | None_h -> "none"
      | Manual -> "manual"
      | Auto -> "auto"
      | Inherit -> "inherit")
  | Font_stretch -> (
      match value with
      | Ultra_condensed -> "50%"
      | Extra_condensed -> "62.5%"
      | Condensed -> "75%"
      | Semi_condensed -> "87.5%"
      | Normal -> "100%"
      | Semi_expanded -> "112.5%"
      | Expanded -> "125%"
      | Extra_expanded -> "150%"
      | Ultra_expanded -> "200%"
      | Percentage p -> Pp.str [ string_of_float p; "%" ]
      | Inherit -> "inherit")
  | Font_variant_numeric -> string_of_font_variant_numeric mode value
  | Webkit_font_smoothing -> string_of_webkit_font_smoothing value
  | Scroll_snap_type -> string_of_scroll_snap_type value
  | Container_type -> string_of_container_type value
  | White_space -> string_of_white_space value
  | Grid_template_columns -> string_of_grid_template value
  | Grid_template_rows -> string_of_grid_template value
  | Grid_auto_columns -> string_of_grid_track_size value
  | Grid_auto_rows -> string_of_grid_track_size value
  (* String properties *)
  | Flex -> value
  | Flex_basis -> value
  | Align_content -> string_of_align value
  | Justify_self -> string_of_justify_self value
  | Place_content -> string_of_place_content value
  | Place_items -> string_of_place_content value
  | Place_self -> value
  | Grid_column -> value
  | Grid_row -> value
  | Grid_column_start -> string_of_grid_line value
  | Grid_column_end -> string_of_grid_line value
  | Grid_row_start -> string_of_grid_line value
  | Grid_row_end -> string_of_grid_line value
  | Text_underline_offset -> value
  | Font_family -> value
  | Background_position -> value
  | Background_repeat -> string_of_background_repeat value
  | Background_size -> value
  | Moz_osx_font_smoothing -> value
  | Backdrop_filter -> value
  | Container_name -> value
  | Perspective_origin -> value
  | Object_position -> value
  | Rotate -> string_of_angle value
  | Transition_duration -> string_of_duration value
  | Transition_timing_function -> string_of_timing_function value
  | Transition_delay -> string_of_duration value
  | Will_change -> value
  | Contain -> value
  | Filter -> value
  | Background_image -> value
  | Animation -> value
  | Aspect_ratio -> string_of_aspect_ratio value
  | Content -> value
  | Quotes -> value
  | Box_shadow -> string_of_box_shadow ~mode value
  | Fill -> string_of_svg_paint ~mode value
  | Stroke -> string_of_svg_paint ~mode value
  | Stroke_width -> string_of_length ~mode value
  | Transition -> string_of_transition value
  | Scale -> value
  | Outline -> value
  | Outline_style -> string_of_outline_style value
  | Outline_width -> string_of_length ~mode value
  | Outline_color -> string_of_color ~mode value
  | Forced_color_adjust -> string_of_forced_color_adjust value
  | Clip -> value
  | Clear -> string_of_clear value
  | Float -> string_of_float_side value
  | Border -> value
  | Text_decoration_thickness -> value
  | Text_size_adjust -> value
  | Touch_action -> string_of_touch_action value
  | List_style -> value
  | Font -> value
  | Webkit_appearance -> string_of_appearance value
  | Letter_spacing -> string_of_length ~mode value
  | Cursor -> string_of_cursor value
  | Pointer_events -> string_of_pointer_events value
  | User_select -> string_of_user_select value
  | Font_feature_settings -> string_of_font_feature_settings value
  | Font_variation_settings -> string_of_font_variation_settings value
  | Webkit_text_decoration -> string_of_text_decoration value
  | Webkit_text_size_adjust -> value

(* String conversion for value kinds - reuses existing printers *)
let rec string_of_font_family = function
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
  | Times -> "Times"
  | Georgia -> "Georgia"
  | Cambria -> "Cambria"
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
  | Var { name; fallback; _ } -> (
      match fallback with
      | None -> Pp.str [ "var(--"; name; ")" ]
      | Some fonts ->
          Pp.str
            [
              "var(--";
              name;
              ",";
              String.concat "," (List.map string_of_font_family fonts);
              ")";
            ])

let string_of_value : type a. ?mode:mode -> a kind -> a -> string =
 fun ?(mode = Variables) kind value ->
  match kind with
  | Length -> string_of_length ~mode value
  | Color -> string_of_color ~mode value
  | Int -> string_of_int value
  | Float -> Pp.float value
  | Duration -> string_of_duration value
  | Aspect_ratio -> string_of_aspect_ratio value
  | Border_style -> string_of_border_style value
  | Font_weight -> string_of_font_weight value
  | Font_family -> Pp.str ~sep:", " (List.map string_of_font_family value)
  | Font_feature_settings -> string_of_font_feature_settings value
  | Font_variation_settings -> string_of_font_variation_settings value
  | Font_variant_numeric -> string_of_font_variant_numeric mode value
  | Font_variant_numeric_token -> string_of_font_variant_numeric_token value
  | Blend_mode -> string_of_blend_mode value
  | Scroll_snap_strictness -> string_of_scroll_snap_strictness value
  | Angle -> string_of_angle value
  | Scale -> string_of_scale value
  | String -> value

(* Typed variable setters *)
let var : type a.
    ?fallback:a ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?fallback ?layer ?meta name kind value ->
  let declaration =
    Custom_declaration
      { name = Pp.str [ "--"; name ]; kind; value; layer; meta }
  in
  let var_handle = { name; fallback; default = Some value; layer; meta } in
  (declaration, var_handle)

(* Property constructors with typed values *)
let background_color c = Declaration (Background_color, c)
let color c = Declaration (Color, c)
let border_color c = Declaration (Border_color, c)
let border_style bs = Declaration (Border_style, bs)
let border_top_style bs = Declaration (Border_top_style, bs)
let border_right_style bs = Declaration (Border_right_style, bs)
let border_bottom_style bs = Declaration (Border_bottom_style, bs)
let border_left_style bs = Declaration (Border_left_style, bs)
let text_decoration td = Declaration (Text_decoration, td)
let font_style fs = Declaration (Font_style, fs)
let list_style_type lst = Declaration (List_style_type, lst)
let list_style_position v = Declaration (List_style_position, v)
let list_style_image v = Declaration (List_style_image, v)
let padding len = Declaration (Padding, len)
let padding_left len = Declaration (Padding_left, len)
let padding_right len = Declaration (Padding_right, len)
let padding_bottom len = Declaration (Padding_bottom, len)
let padding_top len = Declaration (Padding_top, len)

(* Remove deprecated string-based versions *)
let margin len = Declaration (Margin, len)
let margin_left len = Declaration (Margin_left, len)
let margin_right len = Declaration (Margin_right, len)
let margin_top len = Declaration (Margin_top, len)
let margin_bottom len = Declaration (Margin_bottom, len)

(* Remove deprecated string-based versions *)
let gap len = Declaration (Gap, len)
let column_gap len = Declaration (Column_gap, len)
let row_gap len = Declaration (Row_gap, len)
let width len = Declaration (Width, len)
let height len = Declaration (Height, len)

(* Remove deprecated string-based versions *)
let min_width len = Declaration (Min_width, len)
let min_height len = Declaration (Min_height, len)
let max_width len = Declaration (Max_width, len)
let max_height len = Declaration (Max_height, len)
let font_size len = Declaration (Font_size, len)
let line_height len = Declaration (Line_height, len)
let font_weight w = Declaration (Font_weight, w)
let text_align a = Declaration (Text_align, a)
let text_decoration_style value = Declaration (Text_decoration_style, value)
let text_underline_offset value = Declaration (Text_underline_offset, value)
let text_transform value = Declaration (Text_transform, value)
let letter_spacing len = Declaration (Letter_spacing, len)
let white_space value = Declaration (White_space, value)
let display d = Declaration (Display, d)
let position p = Declaration (Position, p)
let visibility v = Declaration (Visibility, v)
let top len = Declaration (Top, len)
let right len = Declaration (Right, len)
let bottom len = Declaration (Bottom, len)
let left len = Declaration (Left, len)
let opacity value = Declaration (Opacity, value)

(* Remove deprecated string-based versions *)
let flex_direction d = Declaration (Flex_direction, d)
let flex value = Declaration (Flex, value)
let flex_grow value = Declaration (Flex_grow, value)
let flex_shrink value = Declaration (Flex_shrink, value)
let flex_wrap value = Declaration (Flex_wrap, value)
let align_items a = Declaration (Align_items, a)
let align_content a = Declaration (Align_content, a)
let align_self a = Declaration (Align_self, a)
let justify_content a = Declaration (Justify_content, a)
let justify_self a = Declaration (Justify_self, a)
let place_content value = Declaration (Place_content, value)
let place_items value = Declaration (Place_items, value)
let place_self value = Declaration (Place_self, value)

(* Typed place-* helpers *)
type place =
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly
  | Inherit

(* Not currently used let string_of_place = function | Start -> "start" | End ->
   "end" | Center -> "center" | Stretch -> "stretch" | Space_between ->
   "space-between" | Space_around -> "space-around" | Space_evenly ->
   "space-evenly" | Inherit -> "inherit" *)

let place_items_v v = Declaration (Place_items, v)

let place_self_v = function
  | `Auto -> Declaration (Place_self, "auto")
  | `Start -> Declaration (Place_self, "start")
  | `End -> Declaration (Place_self, "end")
  | `Center -> Declaration (Place_self, "center")
  | `Stretch -> Declaration (Place_self, "stretch")

let border_width len = Declaration (Border_width, len)
let border_radius len = Declaration (Border_radius, len)
let box_shadow value = Declaration (Box_shadow, value)
let fill value = Declaration (Fill, value)
let stroke value = Declaration (Stroke, value)
let stroke_width value = Declaration (Stroke_width, value)
let outline_style v = Declaration (Outline_style, v)
let outline_width len = Declaration (Outline_width, len)
let outline_color c = Declaration (Outline_color, c)
let forced_color_adjust v = Declaration (Forced_color_adjust, v)
let table_layout value = Declaration (Table_layout, value)
let border_spacing len = Declaration (Border_spacing, len)
let overflow o = Declaration (Overflow, o)
let object_fit value = Declaration (Object_fit, value)
let clip value = Declaration (Clip, value)
let clear value = Declaration (Clear, value)
let float value = Declaration (Float, value)
let touch_action value = Declaration (Touch_action, value)
let text_decoration_color value = Declaration (Text_decoration_color, value)

let text_decoration_thickness value =
  Declaration (Text_decoration_thickness, value)

let text_size_adjust value = Declaration (Text_size_adjust, value)
let aspect_ratio v = Declaration (Aspect_ratio, v)
let filter value = Declaration (Filter, value)

(* Gradient direction values *)
type gradient_direction =
  | To_top
  | To_top_right
  | To_right
  | To_bottom_right
  | To_bottom
  | To_bottom_left
  | To_left
  | To_top_left
  | Angle of angle

(* Gradient stop values *)
type gradient_stop =
  | Color_stop of color
  | Color_position of color * length
  | Var of color var
  | Computed_stops of
      string (* For complex computed values like --tw-gradient-stops *)

(* Background image values *)
type background_image =
  | Url of string
  | Linear_gradient of gradient_direction * gradient_stop list
  | Radial_gradient of gradient_stop list
  | None

let string_of_gradient_direction = function
  | To_top -> "to top"
  | To_top_right -> "to top right"
  | To_right -> "to right"
  | To_bottom_right -> "to bottom right"
  | To_bottom -> "to bottom"
  | To_bottom_left -> "to bottom left"
  | To_left -> "to left"
  | To_top_left -> "to top left"
  | Angle a -> string_of_angle a

let string_of_gradient_stop = function
  | Color_stop c -> string_of_color c
  | Color_position (c, len) ->
      Pp.str [ string_of_color c; " "; string_of_length len ]
  | Var v -> Pp.str [ "var(--"; v.name; ")" ]
  | Computed_stops s -> s

let string_of_background_image = function
  | Url url -> Pp.str [ "url(\""; url; "\")" ]
  | Linear_gradient (dir, stops) -> (
      let stop_strings = List.map string_of_gradient_stop stops in
      match stop_strings with
      | [] ->
          Pp.str [ "linear-gradient("; string_of_gradient_direction dir; ")" ]
      | _ ->
          Pp.str
            [
              "linear-gradient(";
              string_of_gradient_direction dir;
              ", ";
              Pp.str ~sep:", " stop_strings;
              ")";
            ])
  | Radial_gradient stops -> (
      let stop_strings = List.map string_of_gradient_stop stops in
      match stop_strings with
      | [] -> "radial-gradient()"
      | _ -> Pp.str [ "radial-gradient("; Pp.str ~sep:", " stop_strings; ")" ])
  | None -> "none"

let background_image value =
  Declaration (Background_image, string_of_background_image value)

(* Helper functions for background images *)
let url path = Url path
let linear_gradient dir stops = Linear_gradient (dir, stops)
let radial_gradient stops = Radial_gradient stops

(* Helper functions for gradient stops *)
let color_stop c = Color_stop c
let color_position c pos = Color_position (c, pos)
let animation value = Declaration (Animation, value)

(* Blend modes *)

let mix_blend_mode v = Declaration (Mix_blend_mode, v)
let grid_template_columns value = Declaration (Grid_template_columns, value)
let grid_template_rows value = Declaration (Grid_template_rows, value)
let pointer_events value = Declaration (Pointer_events, value)
let z_index value = Declaration (Z_index, Index value)
let z_index_auto = Declaration (Z_index, Auto)
let appearance value = Declaration (Appearance, value)
let overflow_x o = Declaration (Overflow_x, o)
let overflow_y o = Declaration (Overflow_y, o)
let resize value = Declaration (Resize, value)
let vertical_align value = Declaration (Vertical_align, value)
let box_sizing value = Declaration (Box_sizing, value)

let font_family fonts =
  Declaration
    (Font_family, String.concat ", " (List.map string_of_font_family fonts))

let moz_osx_font_smoothing value =
  Declaration (Moz_osx_font_smoothing, string_of_moz_font_smoothing value)

let webkit_line_clamp value = Declaration (Webkit_line_clamp, value)
let webkit_box_orient value = Declaration (Webkit_box_orient, value)
let text_overflow v = Declaration (Text_overflow, v)
let text_wrap v = Declaration (Text_wrap, v)
let word_break v = Declaration (Word_break, v)
let overflow_wrap v = Declaration (Overflow_wrap, v)
let hyphens v = Declaration (Hyphens, v)
let webkit_hyphens v = Declaration (Webkit_hyphens, v)
let font_stretch v = Declaration (Font_stretch, v)
let font_variant_numeric v = Declaration (Font_variant_numeric, v)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

let backdrop_filter value = Declaration (Backdrop_filter, value)
let background_position value = Declaration (Background_position, value)
let background_repeat value = Declaration (Background_repeat, value)
let background_size value = Declaration (Background_size, value)

(* Additional property constructors *)
let content value = Declaration (Content, value)
let border_left_width len = Declaration (Border_left_width, len)
let border_bottom_width len = Declaration (Border_bottom_width, len)
let border_top_width len = Declaration (Border_top_width, len)
let border_right_width len = Declaration (Border_right_width, len)
let border_left_color c = Declaration (Border_left_color, c)
let border_bottom_color c = Declaration (Border_bottom_color, c)
let transition value = Declaration (Transition, value)
let quotes value = Declaration (Quotes, value)

(* New property constructors for tw.ml *)
let border value = Declaration (Border, value)
let tab_size value = Declaration (Tab_size, value)
let webkit_text_size_adjust value = Declaration (Webkit_text_size_adjust, value)
let font_feature_settings value = Declaration (Font_feature_settings, value)
let font_variation_settings value = Declaration (Font_variation_settings, value)

let webkit_tap_highlight_color value =
  Declaration (Webkit_tap_highlight_color, value)

let webkit_text_decoration value = Declaration (Webkit_text_decoration, value)

let webkit_text_decoration_color value =
  Declaration (Webkit_text_decoration_color, value)

let text_indent len = Declaration (Text_indent, len)
let border_collapse value = Declaration (Border_collapse, value)
let list_style value = Declaration (List_style, value)
let font value = Declaration (Font, value)
let webkit_appearance value = Declaration (Webkit_appearance, value)
let webkit_font_smoothing value = Declaration (Webkit_font_smoothing, value)
let cursor c = Declaration (Cursor, c)
let user_select u = Declaration (User_select, u)
let container_type value = Declaration (Container_type, value)
let container_name value = Declaration (Container_name, value)
let perspective len = Declaration (Perspective, len)
let perspective_origin value = Declaration (Perspective_origin, value)
let transform_style value = Declaration (Transform_style, value)
let backface_visibility value = Declaration (Backface_visibility, value)
let object_position value = Declaration (Object_position, value)
let rotate a = Declaration (Rotate, a)
let transform values = Declaration (Transform, values)
let scale value = Declaration (Scale, value)
let transition_duration value = Declaration (Transition_duration, value)

let transition_timing_function value =
  Declaration (Transition_timing_function, value)

let transition_delay value = Declaration (Transition_delay, value)
let will_change value = Declaration (Will_change, value)
let contain value = Declaration (Contain, value)
let isolation value = Declaration (Isolation, value)
let padding_inline len = Declaration (Padding_inline, len)
let padding_inline_start len = Declaration (Padding_inline_start, len)
let padding_block len = Declaration (Padding_block, len)
let margin_inline len = Declaration (Margin_inline, len)
let margin_block len = Declaration (Margin_block, len)
let margin_inline_end len = Declaration (Margin_inline_end, len)
let outline value = Declaration (Outline, value)
let outline_offset len = Declaration (Outline_offset, len)
let scroll_snap_type value = Declaration (Scroll_snap_type, value)
let scroll_snap_align value = Declaration (Scroll_snap_align, value)
let scroll_snap_stop value = Declaration (Scroll_snap_stop, value)
let scroll_behavior value = Declaration (Scroll_behavior, value)

(** Flex module *)
module Flex = struct
  let direction dir = Declaration (Flex_direction, dir)
  let wrap w = Declaration (Flex_wrap, w)
  let flex v = Declaration (Flex, string_of_flex v)
  let grow n = Declaration (Flex_grow, n)
  let shrink n = Declaration (Flex_shrink, n)
  let basis len = Declaration (Flex_basis, string_of_length len)
  let order n = Declaration (Order, n)
  let align_items v = Declaration (Align_items, v)
  let align_self v = Declaration (Align_self, v)
  let justify_content v = Declaration (Justify_content, v)
  let justify_items v = Declaration (Justify_items, v)
  let justify_self v = Declaration (Justify_self, v)
  let gap len = Declaration (Gap, len)
end

(** Grid module *)
module Grid = struct
  let template_columns template = Declaration (Grid_template_columns, template)
  let template_rows template = Declaration (Grid_template_rows, template)
  let column_gap len = Declaration (Column_gap, len)
  let row_gap len = Declaration (Row_gap, len)
  let gap len = Declaration (Gap, len)

  let auto_flow flow =
    let value =
      match flow with
      | `Row -> Row
      | `Column -> Column
      | `Row_dense -> Row_dense
      | `Column_dense -> Column_dense
    in
    Declaration (Grid_auto_flow, value)

  let auto_columns size = Declaration (Grid_auto_columns, size)
  let auto_rows size = Declaration (Grid_auto_rows, size)
  let column value = Declaration (Grid_column, value)
  let row value = Declaration (Grid_row, value)
  let column_start value = Declaration (Grid_column_start, value)
  let column_end value = Declaration (Grid_column_end, value)
  let row_start value = Declaration (Grid_row_start, value)
  let row_end value = Declaration (Grid_row_end, value)
end

(** Border module *)
module Border = struct
  let width len = border_width len
  let style s = border_style s
  let color c = border_color c
  let radius len = border_radius len
  let top_width len = Declaration (Border_top_width, len)
  let right_width len = Declaration (Border_right_width, len)
  let bottom_width len = Declaration (Border_bottom_width, len)
  let left_width len = Declaration (Border_left_width, len)
  let top_color c = Declaration (Border_top_color, c)
  let right_color c = Declaration (Border_right_color, c)
  let bottom_color c = Declaration (Border_bottom_color, c)
  let left_color c = Declaration (Border_left_color, c)
end

type rule = { selector : string; declarations : declaration list }
type media_rule = { media_condition : string; media_rules : rule list }

type container_rule = {
  container_name : string option;
  container_condition : string;
  container_rules : rule list;
}

type starting_style_rule = { starting_rules : rule list }

type supports_content =
  | Support_rules of rule list
  | Support_nested of rule list * supports_rule list

and supports_rule = {
  supports_condition : string;
  supports_content : supports_content;
}

type nested_rule = Rule of rule | Supports of supports_rule

type property_rule = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string;
}

type layer_rule = {
  layer : string;
  rules : nested_rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  supports_queries : supports_rule list;
}

type t = {
  layers : layer_rule list;
  rules : rule list;
  media_queries : media_rule list;
  container_queries : container_rule list;
  starting_styles : starting_style_rule list;
  supports_queries : supports_rule list;
  at_properties : property_rule list;
}

type sheet_item =
  | Rule of rule
  | Media of media_rule
  | Supports of supports_rule
  | Container of container_rule
  | Layer of layer_rule
  | Property of property_rule
  | Starting_style of starting_style_rule

(** {1 Creation} *)

let string_of_property : type a. a property -> string = function
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
  | Text_decoration_color -> "text-decoration-color"
  | Text_decoration_thickness -> "text-decoration-thickness"
  | Text_underline_offset -> "text-underline-offset"
  | Text_transform -> "text-transform"
  | Letter_spacing -> "letter-spacing"
  | List_style_type -> "list-style-type"
  | List_style_position -> "list-style-position"
  | List_style_image -> "list-style-image"
  | Display -> "display"
  | Position -> "position"
  | Visibility -> "visibility"
  | Flex_direction -> "flex-direction"
  | Flex_wrap -> "flex-wrap"
  | Flex -> "flex"
  | Flex_grow -> "flex-grow"
  | Flex_shrink -> "flex-shrink"
  | Flex_basis -> "flex-basis"
  | Order -> "order"
  | Align_items -> "align-items"
  | Justify_content -> "justify-content"
  | Justify_items -> "justify-items"
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
  | Grid_column_start -> "grid-column-start"
  | Grid_column_end -> "grid-column-end"
  | Grid_row_start -> "grid-row-start"
  | Grid_row_end -> "grid-row-end"
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
  | Fill -> "fill"
  | Stroke -> "stroke"
  | Stroke_width -> "stroke-width"
  | Opacity -> "opacity"
  | Mix_blend_mode -> "mix-blend-mode"
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
  | Outline_style -> "outline-style"
  | Outline_width -> "outline-width"
  | Outline_color -> "outline-color"
  | Outline_offset -> "outline-offset"
  | Forced_color_adjust -> "forced-color-adjust"
  | Scroll_snap_type -> "scroll-snap-type"
  | Clip -> "clip"
  | Clear -> "clear"
  | Float -> "float"
  | White_space -> "white-space"
  | Border -> "border"
  | Tab_size -> "tab-size"
  | Webkit_text_size_adjust -> "-webkit-text-size-adjust"
  | Font_feature_settings -> "font-feature-settings"
  | Font_variation_settings -> "font-variation-settings"
  | Webkit_tap_highlight_color -> "-webkit-tap-highlight-color"
  | Webkit_text_decoration -> "-webkit-text-decoration"
  | Webkit_text_decoration_color -> "-webkit-text-decoration-color"
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
  | Transition_delay -> "transition-delay"
  | Will_change -> "will-change"
  | Contain -> "contain"
  | Isolation -> "isolation"
  | Filter -> "filter"
  | Background_image -> "background-image"
  | Animation -> "animation"
  | Aspect_ratio -> "aspect-ratio"
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
  | Webkit_box_orient -> "-webkit-box-orient"
  | Text_overflow -> "text-overflow"
  | Text_wrap -> "text-wrap"
  | Word_break -> "word-break"
  | Overflow_wrap -> "overflow-wrap"
  | Hyphens -> "hyphens"
  | Webkit_hyphens -> "-webkit-hyphens"
  | Font_stretch -> "font-stretch"
  | Font_variant_numeric -> "font-variant-numeric"
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
  | _ -> "unknown" (* Custom properties removed *)

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

let property ~name ~syntax ~initial_value ?(inherits = false) () =
  { name; syntax; inherits; initial_value }

let property_rule_name r = r.name
let property_rule_initial r = r.initial_value
let default_decl_of_property_rule r = custom_property r.name r.initial_value
let rule_to_nested rule : nested_rule = Rule rule
let supports_to_nested supports : nested_rule = Supports supports

let layer ~name ?(media = []) ?(container = []) ?(supports = []) rules =
  {
    layer = name;
    rules;
    media_queries = media;
    container_queries = container;
    supports_queries = supports;
  }

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
      | Property a -> { acc with at_properties = acc.at_properties @ [ a ] }
      | Layer l -> { acc with layers = acc.layers @ [ l ] })
    empty items

(** {1 Utilities} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right

(* Extract variables from any property value *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  (* Length properties *)
  | Width, Var v -> [ V v ]
  | Width, Calc calc -> vars_of_calc calc
  | Height, Var v -> [ V v ]
  | Height, Calc calc -> vars_of_calc calc
  | Min_width, Var v -> [ V v ]
  | Min_width, Calc calc -> vars_of_calc calc
  | Min_height, Var v -> [ V v ]
  | Min_height, Calc calc -> vars_of_calc calc
  | Max_width, Var v -> [ V v ]
  | Max_width, Calc calc -> vars_of_calc calc
  | Max_height, Var v -> [ V v ]
  | Max_height, Calc calc -> vars_of_calc calc
  | Padding, Var v -> [ V v ]
  | Padding, Calc calc -> vars_of_calc calc
  | Padding_top, Var v -> [ V v ]
  | Padding_top, Calc calc -> vars_of_calc calc
  | Padding_right, Var v -> [ V v ]
  | Padding_right, Calc calc -> vars_of_calc calc
  | Padding_bottom, Var v -> [ V v ]
  | Padding_bottom, Calc calc -> vars_of_calc calc
  | Padding_left, Var v -> [ V v ]
  | Padding_left, Calc calc -> vars_of_calc calc
  | Padding_inline, Var v -> [ V v ]
  | Padding_inline, Calc calc -> vars_of_calc calc
  | Padding_inline_start, Var v -> [ V v ]
  | Padding_inline_start, Calc calc -> vars_of_calc calc
  | Padding_block, Var v -> [ V v ]
  | Padding_block, Calc calc -> vars_of_calc calc
  | Margin, Var v -> [ V v ]
  | Margin, Calc calc -> vars_of_calc calc
  | Margin_top, Var v -> [ V v ]
  | Margin_top, Calc calc -> vars_of_calc calc
  | Margin_right, Var v -> [ V v ]
  | Margin_right, Calc calc -> vars_of_calc calc
  | Margin_bottom, Var v -> [ V v ]
  | Margin_bottom, Calc calc -> vars_of_calc calc
  | Margin_left, Var v -> [ V v ]
  | Margin_left, Calc calc -> vars_of_calc calc
  | Margin_inline, Var v -> [ V v ]
  | Margin_inline, Calc calc -> vars_of_calc calc
  | Margin_block, Var v -> [ V v ]
  | Margin_block, Calc calc -> vars_of_calc calc
  | Top, Var v -> [ V v ]
  | Top, Calc calc -> vars_of_calc calc
  | Right, Var v -> [ V v ]
  | Right, Calc calc -> vars_of_calc calc
  | Bottom, Var v -> [ V v ]
  | Bottom, Calc calc -> vars_of_calc calc
  | Left, Var v -> [ V v ]
  | Left, Calc calc -> vars_of_calc calc
  | Font_size, Var v -> [ V v ]
  | Font_size, Calc calc -> vars_of_calc calc
  | Letter_spacing, Var v -> [ V v ]
  | Letter_spacing, Calc calc -> vars_of_calc calc
  | Line_height, Var v -> [ V v ]
  | Line_height, Calc calc -> vars_of_calc calc
  | Border_width, Var v -> [ V v ]
  | Border_width, Calc calc -> vars_of_calc calc
  | Border_top_width, Var v -> [ V v ]
  | Border_top_width, Calc calc -> vars_of_calc calc
  | Border_right_width, Var v -> [ V v ]
  | Border_right_width, Calc calc -> vars_of_calc calc
  | Border_bottom_width, Var v -> [ V v ]
  | Border_bottom_width, Calc calc -> vars_of_calc calc
  | Border_left_width, Var v -> [ V v ]
  | Border_left_width, Calc calc -> vars_of_calc calc
  | Outline_width, Var v -> [ V v ]
  | Outline_width, Calc calc -> vars_of_calc calc
  | Column_gap, Var v -> [ V v ]
  | Column_gap, Calc calc -> vars_of_calc calc
  | Row_gap, Var v -> [ V v ]
  | Row_gap, Calc calc -> vars_of_calc calc
  | Gap, Var v -> [ V v ]
  | Gap, Calc calc -> vars_of_calc calc
  (* Color properties *)
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Outline_color, Var v -> [ V v ]
  (* Border radius *)
  | Border_radius, Var v -> [ V v ]
  | Border_radius, Calc calc -> vars_of_calc calc
  (* Outline offset *)
  | Outline_offset, Var v -> [ V v ]
  | Outline_offset, Calc calc -> vars_of_calc calc
  (* Other properties don't support Var *)
  (* All other cases *)
  | _ -> []

let rec vars_of_value : type a. a kind -> a -> any_var list =
 fun kind value ->
  match (kind, value) with
  | Length, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Duration, Var v -> [ V v ]
  | Blend_mode, _ -> [] (* blend_mode doesn't have Var constructor *)
  | Scroll_snap_strictness, _ ->
      [] (* scroll_snap_strictness doesn't have Var constructor *)
  | Angle, Var v -> [ V v ] (* angle can have variable references *)
  | Angle, _ -> [] (* other angle values don't have variables *)
  | Length, Calc calc -> vars_of_calc calc
  | Color, Mix _ -> [] (* Could extend to extract from color mix *)
  | Int, _ -> []
  | Float, _ -> []
  | Aspect_ratio, _ -> []
  | Border_style, _ -> []
  | Font_weight, _ -> []
  | String, _ -> [] (* String values don't have typed variables *)
  | Font_variant_numeric, Var v -> [ V v ]
  | ( Font_variant_numeric,
      Composed
        {
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        } ) ->
      vars_of_values_opt
        [
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        ]
  | Font_variant_numeric, _ -> []
  | Font_variant_numeric_token, _ -> []
  | _ -> []

and vars_of_values_opt values =
  let collect_vars (opt_fv : _ option) =
    match opt_fv with
    | None -> []
    | Some fv -> vars_of_value Font_variant_numeric fv
  in
  List.concat_map collect_vars values

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** Extract all CSS variables referenced in properties (for theme layer) *)
let vars_of_declarations properties =
  List.concat_map
    (function
      | Declaration (prop, value) -> vars_of_property prop value
      | Important_declaration (prop, value) -> vars_of_property prop value
      | Custom_declaration { kind; value; _ } -> vars_of_value kind value)
    properties
  |> List.sort_uniq compare_vars_by_name

(* Helper function to check if a property should allow duplicates Some webkit
   properties need to be duplicated for browser compatibility. This is a
   workaround for older WebKit/Safari versions that had bugs with certain CSS
   properties.

   See: https://bugs.webkit.org/show_bug.cgi?id=101180 and:
   https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color#browser_compatibility

   Modern CSS libraries include these duplicates for maximum compatibility. *)

(* Duplicate known buggy properties for browser compatibility *)
let duplicate_buggy_properties decls =
  List.concat_map
    (fun decl ->
      match decl with
      | Declaration (Webkit_text_decoration, Inherit)
      | Important_declaration (Webkit_text_decoration, Inherit) ->
          [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration (Webkit_text_decoration_color, _)
      | Important_declaration (Webkit_text_decoration_color, _) ->
          [ decl; decl ] (* Always duplicate webkit-text-decoration-color *)
      | _ -> [ decl ])
    decls

let deduplicate_declarations props =
  (* Keep last occurrence of each property while preserving order *)
  let seen = Hashtbl.create 16 in
  let deduped =
    List.fold_right
      (fun decl acc ->
        let prop_name =
          match decl with
          | Declaration (prop, _) -> string_of_property prop
          | Important_declaration (prop, _) -> string_of_property prop
          | Custom_declaration { name; _ } -> name
        in
        if Hashtbl.mem seen prop_name then acc
        else (
          Hashtbl.add seen prop_name ();
          decl :: acc))
      props []
  in
  (* Apply buggy property duplication after deduplication *)
  duplicate_buggy_properties deduped

(* Get the name of a variable *)
let any_var_name (V v) = Pp.str [ "--"; v.name ]

(* Extract variables from a typed value - needs to handle each property type *)
let extract_vars_from_prop_value : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Webkit_text_decoration_color, Var v -> [ V v ]
  | Webkit_tap_highlight_color, Var v -> [ V v ]
  | Padding, Var v -> [ V v ]
  | Padding_left, Var v -> [ V v ]
  | Padding_right, Var v -> [ V v ]
  | Padding_top, Var v -> [ V v ]
  | Padding_bottom, Var v -> [ V v ]
  | Margin, Var v -> [ V v ]
  | Margin_left, Var v -> [ V v ]
  | Margin_right, Var v -> [ V v ]
  | Margin_top, Var v -> [ V v ]
  | Margin_bottom, Var v -> [ V v ]
  | Gap, Var v -> [ V v ]
  | Column_gap, Var v -> [ V v ]
  | Row_gap, Var v -> [ V v ]
  | Width, Var v -> [ V v ]
  | Height, Var v -> [ V v ]
  | Min_width, Var v -> [ V v ]
  | Min_height, Var v -> [ V v ]
  | Max_width, Var v -> [ V v ]
  | Max_height, Var v -> [ V v ]
  | Font_size, Var v -> [ V v ]
  | Line_height, Var v -> [ V v ]
  | Letter_spacing, Var v -> [ V v ]
  | Top, Var v -> [ V v ]
  | Right, Var v -> [ V v ]
  | Bottom, Var v -> [ V v ]
  | Left, Var v -> [ V v ]
  | Border_radius, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Outline_offset, Var v -> [ V v ]
  | _ -> [] (* No variables in this value *)

let extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
  | Declaration (prop, value) -> extract_vars_from_prop_value prop value
  | Important_declaration (prop, value) ->
      extract_vars_from_prop_value prop value

(* Analyze declarations to find all variable references *)
let analyze_declarations (decls : declaration list) : any_var list =
  List.concat_map extract_vars_from_declaration decls

(* Extract only custom property declarations (variable definitions) *)
let extract_custom_declarations (decls : declaration list) : declaration list =
  List.filter (function Custom_declaration _ -> true | _ -> false) decls

(* Extract the variable name from a custom declaration *)
let custom_declaration_name (decl : declaration) : string option =
  match decl with Custom_declaration { name; _ } -> Some name | _ -> None

(* Helper to format a property-value pair *)
let format_property_value ~mode ~sep prop value =
  Pp.str
    [ string_of_property prop; sep; string_of_property_value ~mode prop value ]

let inline_style_of_declarations ?(mode : mode = Inline) props =
  props
  |> List.map (function
       | Declaration (prop, value) ->
           format_property_value ~mode ~sep:": " prop value
       | Important_declaration (prop, value) ->
           Pp.str
             [
               string_of_property prop;
               ": ";
               string_of_property_value ~mode prop value;
               " !important";
             ]
       | Custom_declaration { name; kind; value; _ } ->
           Pp.str [ name; ": "; string_of_value kind value ])
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
          merge_helper (rule :: acc) (rule.selector :: seen) rest
  in
  merge_helper [] [] rules
  |> List.map (fun r ->
         { r with declarations = deduplicate_declarations r.declarations })

(* Merge rules with identical properties into combined selectors *)
let merge_by_properties rules =
  (* Create a hash of properties for comparison *)
  let properties_hash props =
    props
    |> List.map (function
         | Declaration (prop, value) ->
             format_property_value ~mode:Inline ~sep:":" prop value
         | Important_declaration (prop, value) ->
             Pp.str
               [
                 string_of_property prop;
                 ":";
                 string_of_property_value ~mode:Inline prop value;
                 "!important";
               ]
         | Custom_declaration { name; kind; value; _ } ->
             Pp.str [ name; ":"; string_of_value kind value ])
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
    | Some m -> Pp.str [ "."; Re.Group.get m 1; Re.Group.get m 2 ]
    | None -> v

(** {1 Rendering} *)

let render_minified_rule ~mode rule =
  let selector = minify_selector rule.selector in
  let props =
    rule.declarations
    |> List.map (function
         | Declaration (prop, value) ->
             let prop_name = string_of_property prop in
             let value_str = string_of_property_value ~mode prop value in
             let final_value =
               (* Convert transparent to #0000 for background-color in minified
                  output *)
               match prop with
               | Background_color when value_str = "transparent" -> "#0000"
               | _ -> minify_value value_str
             in
             Pp.str [ prop_name; ":"; final_value ]
         | Important_declaration (prop, value) ->
             let prop_name = string_of_property prop in
             let value_str = string_of_property_value ~mode prop value in
             let final_value =
               (* Convert transparent to #0000 for background-color in minified
                  output *)
               match prop with
               | Background_color when value_str = "transparent" -> "#0000"
               | _ -> minify_value value_str
             in
             Pp.str [ prop_name; ":"; final_value; "!important" ]
         | Custom_declaration { name; kind; value; _ } ->
             let value_str = string_of_value kind value in
             Pp.str [ name; ":"; minify_value value_str ])
  in
  Pp.str [ selector; "{"; Pp.str ~sep:";" props; "}" ]

let render_formatted_rule ~mode ?(indent = "") rule =
  let props =
    rule.declarations
    |> List.map (function
         | Declaration (prop, value) ->
             Pp.str
               [
                 indent;
                 "  ";
                 string_of_property prop;
                 ": ";
                 string_of_property_value ~mode prop value;
                 ";";
               ]
         | Important_declaration (prop, value) ->
             Pp.str
               [
                 indent;
                 "  ";
                 string_of_property prop;
                 ": ";
                 string_of_property_value ~mode prop value;
                 " !important;";
               ]
         | Custom_declaration { name; kind; value; _ } ->
             Pp.str
               [
                 indent; "  "; name; ": "; string_of_value ~mode kind value; ";";
               ])
  in
  Pp.lines
    [
      Pp.str [ indent; rule.selector; " {" ];
      Pp.lines props;
      Pp.str [ indent; "}" ];
    ]

let rec render_supports_content ~minify ~mode content =
  match content with
  | Support_rules rules ->
      if minify then
        rules |> merge_rules |> merge_by_properties
        |> List.map (render_minified_rule ~mode)
        |> String.concat ""
      else
        rules
        |> List.map (render_formatted_rule ~mode ~indent:"    ")
        |> String.concat "\n"
  | Support_nested (rules, nested_queries) ->
      let rules_str =
        if minify then
          rules |> merge_rules |> merge_by_properties
          |> List.map (render_minified_rule ~mode)
          |> String.concat ""
        else
          rules
          |> List.map (render_formatted_rule ~mode ~indent:"    ")
          |> String.concat "\n"
      in
      let nested_str =
        nested_queries
        |> List.map (fun nsq ->
               let content_str =
                 render_supports_content ~minify ~mode nsq.supports_content
               in
               if minify then
                 Pp.str
                   [
                     "@supports "; nsq.supports_condition; "{"; content_str; "}";
                   ]
               else
                 Pp.str
                   [
                     "@supports ";
                     nsq.supports_condition;
                     " {\n";
                     content_str;
                     "\n}";
                   ])
        |> String.concat (if minify then "" else "\n")
      in
      if minify then Pp.str [ rules_str; nested_str ]
      else Pp.str [ rules_str; "\n"; nested_str ]

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  Pp.str
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

(* Configuration for stylesheet rendering *)
type config = { minify : bool; mode : mode }

let render_layer_rules ~config rules =
  let render_nested_rule : nested_rule -> string = function
    | Rule r ->
        let r =
          { r with declarations = deduplicate_declarations r.declarations }
        in
        if config.minify then render_minified_rule ~mode:config.mode r
        else render_formatted_rule ~mode:config.mode r
    | Supports sq ->
        let sq_content =
          render_supports_content ~minify:config.minify ~mode:config.mode
            sq.supports_content
        in
        if config.minify then
          Pp.str [ "@supports "; sq.supports_condition; "{"; sq_content; "}" ]
        else
          Pp.str
            [ "@supports "; sq.supports_condition; " {\n"; sq_content; "\n}" ]
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
      |> List.map (render_minified_rule ~mode:config.mode)
      |> String.concat ""
    else
      rules
      |> List.map (render_formatted_rule ~mode:config.mode ~indent)
      |> String.concat "\n"
  in
  if config.minify then
    Pp.str [ "@"; at_rule; " "; name_part; condition; "{"; content; "}" ]
  else
    Pp.str [ "@"; at_rule; " "; name_part; condition; " {\n"; content; "\n}" ]

let render_layer_media ~config media_queries =
  media_queries
  |> List.map (fun mq ->
         render_at_rules ~config ~at_rule:"media" ~condition:mq.media_condition
           ~name_part:"" ~rules:mq.media_rules ~indent:"  ")
  |> String.concat (if config.minify then "" else "\n")

let render_layer_containers ~config container_queries =
  container_queries
  |> List.map (fun cq ->
         let name_part =
           match cq.container_name with
           | None -> ""
           | Some name -> Pp.str [ name; " " ]
         in
         render_at_rules ~config ~at_rule:"container"
           ~condition:cq.container_condition ~name_part
           ~rules:cq.container_rules ~indent:"  ")
  |> String.concat (if config.minify then "" else "\n")

let render_layer_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify ~mode:config.mode
             sq.supports_content
         in
         if config.minify then
           Pp.str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           Pp.str
             [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_rules ~config rules =
  if config.minify then
    rules |> merge_rules |> merge_by_properties
    |> List.map (render_minified_rule ~mode:config.mode)
    |> String.concat ""
  else
    rules
    |> List.map (render_formatted_rule ~mode:config.mode)
    |> String.concat "\n"

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
           | Some name -> Pp.str [ name; " " ]
         in
         render_at_rules ~config ~at_rule:"container"
           ~condition:cq.container_condition ~name_part
           ~rules:cq.container_rules ~indent:"")
  |> String.concat (if config.minify then "" else "\n")

let render_stylesheet_supports ~config supports_queries =
  supports_queries
  |> List.map (fun sq ->
         let content =
           render_supports_content ~minify:config.minify ~mode:config.mode
             sq.supports_content
         in
         if config.minify then
           Pp.str [ "@supports "; sq.supports_condition; "{"; content; "}" ]
         else
           Pp.str
             [ "@supports "; sq.supports_condition; " {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_starting_styles ~config starting_styles =
  starting_styles
  |> List.map (fun ss ->
         let content =
           if config.minify then
             ss.starting_rules |> merge_rules |> merge_by_properties
             |> List.map (render_minified_rule ~mode:config.mode)
             |> String.concat ""
           else
             ss.starting_rules
             |> List.map (render_formatted_rule ~mode:config.mode)
             |> String.concat "\n"
         in
         if config.minify then Pp.str [ "@starting-style{"; content; "}" ]
         else Pp.str [ "@starting-style {\n"; content; "\n}" ])
  |> String.concat (if config.minify then "" else "\n")

let render_at_properties ~config at_properties =
  at_properties
  |> List.map (fun at ->
         if config.minify then
           let initial_value_part =
             if at.initial_value = "" || at.initial_value = "initial" then ""
             else Pp.str [ ";initial-value:"; at.initial_value ]
           in
           Pp.str
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
             if at.initial_value = "" || at.initial_value = "initial" then ""
             else Pp.str [ ";\n  initial-value: "; at.initial_value ]
           in
           Pp.str
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
  let layer_name = layer_rules.layer in
  let all_parts =
    [
      render_layer_rules ~config layer_rules.rules;
      render_layer_media ~config layer_rules.media_queries;
      render_layer_containers ~config layer_rules.container_queries;
      render_layer_supports ~config layer_rules.supports_queries;
    ]
    |> List.filter (fun s -> s <> "")
  in
  if all_parts = [] then Pp.str [ "@layer "; layer_name; ";" ]
  else
    let content =
      String.concat (if config.minify then "" else "\n") all_parts
    in
    if config.minify then Pp.str [ "@layer "; layer_name; "{"; content; "}" ]
    else Pp.str [ "@layer "; layer_name; " {\n"; content; "\n}" ]

let is_layer_empty (layer : layer_rule) =
  layer.rules = [] && layer.media_queries = []
  && layer.container_queries = []
  && layer.supports_queries = []

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

let merge_empty_layers ~config layers =
  (* Merge consecutive empty layers into single @layer declarations *)
  if not config.minify then List.map (render_layer ~config) layers
  else
    let rec process_layers acc current_empty_group = function
      | [] ->
          (* Finish any remaining empty group *)
          if current_empty_group = [] then List.rev acc
          else
            let merged =
              Pp.str
                [
                  "@layer ";
                  String.concat "," (List.rev current_empty_group);
                  ";";
                ]
            in
            List.rev (merged :: acc)
      | layer :: rest ->
          if is_layer_empty layer then
            (* Collect empty layer names *)
            process_layers acc (layer.layer :: current_empty_group) rest
          else
            (* Non-empty layer: finish any empty group and add this layer *)
            let acc' =
              if current_empty_group = [] then acc
              else
                let merged =
                  Pp.str
                    [
                      "@layer ";
                      String.concat "," (List.rev current_empty_group);
                      ";";
                    ]
                in
                merged :: acc
            in
            process_layers (render_layer ~config layer :: acc') [] rest
    in
    process_layers [] [] layers

let to_string ?(minify = false) ?(mode = Variables) stylesheet =
  let config = { minify; mode } in
  let header_str =
    if List.length stylesheet.layers > 0 then Pp.str [ header; "\n" ] else ""
  in
  let layer_strings = merge_empty_layers ~config stylesheet.layers in
  let ( rule_strings,
        at_property_strings,
        starting_style_strings,
        container_strings,
        supports_strings,
        media_strings ) =
    render_stylesheet_sections ~config stylesheet
  in
  let all_parts =
    [ header_str; "" ] @ layer_strings @ rule_strings @ starting_style_strings
    @ container_strings @ supports_strings @ media_strings @ at_property_strings
  in
  if config.minify then String.concat "" all_parts
  else String.concat "\n" (List.filter (fun s -> s <> "") all_parts)

(** Extract all CSS variables from different input types *)

let vars_of_rules rules =
  List.concat_map (fun rule -> vars_of_declarations rule.declarations) rules

let vars_of_media_queries media_queries =
  List.concat_map (fun mq -> vars_of_rules mq.media_rules) media_queries

let vars_of_container_queries container_queries =
  List.concat_map (fun cq -> vars_of_rules cq.container_rules) container_queries

let vars_of_stylesheet stylesheet =
  vars_of_rules stylesheet.rules
  @ vars_of_media_queries stylesheet.media_queries
  @ vars_of_container_queries stylesheet.container_queries

let pp ?minify ?mode stylesheet = to_string ?minify ?mode stylesheet
