(** CSS Values & Units - Core types and parsing/printing functions *)

(** {1 Core Types} *)

type meta = ..
(** Extensible meta type for CSS variables *)

type 'a var = {
  name : string;
  fallback : 'a option;
  default : 'a option;
  layer : string option;
  meta : meta option;
}
(** CSS variable reference with optional fallback and default values *)

val var_ref :
  ?fallback:'a -> ?default:'a -> ?layer:string -> ?meta:meta -> string -> 'a var
(** Create a CSS variable reference *)

(** Calc expression operators *)
type calc_op = Add | Sub | Mult | Div

(** CSS calc() expressions *)
type 'a calc =
  | Var of 'a var
  | Val of 'a
  | Num of float
  | Expr of 'a calc * calc_op * 'a calc

(** {1 Value Types} *)

(** CSS length values *)
type length =
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float
  | Lh of float
  | Num of float
  | Auto
  | Zero
  | Inherit
  | Fit_content
  | Max_content
  | Min_content
  | From_font
  | Var of length var
  | Calc of length calc

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

(** CSS named colors *)
type color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua
  | Alice_blue
  | Antique_white
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Blanched_almond
  | Blue_violet
  | Brown
  | Burlywood
  | Cadet_blue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflower_blue
  | Cornsilk
  | Crimson
  | Dark_blue
  | Dark_cyan
  | Dark_goldenrod
  | Dark_gray
  | Dark_green
  | Dark_grey
  | Dark_khaki
  | Dark_magenta
  | Dark_olive_green
  | Dark_orange
  | Dark_orchid
  | Dark_red
  | Dark_salmon
  | Dark_sea_green
  | Dark_slate_blue
  | Dark_slate_gray
  | Dark_slate_grey
  | Dark_turquoise
  | Dark_violet
  | Deep_pink
  | Deep_sky_blue
  | Dim_gray
  | Dim_grey
  | Dodger_blue
  | Firebrick
  | Floral_white
  | Forest_green
  | Gainsboro
  | Ghost_white
  | Gold
  | Goldenrod
  | Green_yellow
  | Honeydew
  | Hot_pink
  | Indian_red
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavender_blush
  | Lawn_green
  | Lemon_chiffon
  | Light_blue
  | Light_coral
  | Light_cyan
  | Light_goldenrod_yellow
  | Light_gray
  | Light_green
  | Light_grey
  | Light_pink
  | Light_salmon
  | Light_sea_green
  | Light_sky_blue
  | Light_slate_gray
  | Light_slate_grey
  | Light_steel_blue
  | Light_yellow
  | Lime_green
  | Linen
  | Medium_aquamarine
  | Medium_blue
  | Medium_orchid
  | Medium_purple
  | Medium_sea_green
  | Medium_slate_blue
  | Medium_spring_green
  | Medium_turquoise
  | Medium_violet_red
  | Midnight_blue
  | Mint_cream
  | Misty_rose
  | Moccasin
  | Navajo_white
  | Old_lace
  | Olive_drab
  | Orange_red
  | Orchid
  | Pale_goldenrod
  | Pale_green
  | Pale_turquoise
  | Pale_violet_red
  | Papaya_whip
  | Peach_puff
  | Peru
  | Plum
  | Powder_blue
  | Rebecca_purple
  | Rosy_brown
  | Royal_blue
  | Saddle_brown
  | Salmon
  | Sandy_brown
  | Sea_green
  | Sea_shell
  | Sienna
  | Sky_blue
  | Slate_blue
  | Slate_gray
  | Slate_grey
  | Snow
  | Spring_green
  | Steel_blue
  | Tan
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White_smoke
  | Yellow_green

(** CSS channel values (for RGB) *)
type channel =
  | Int of int (* 0–255, legacy/comma syntax *)
  | Num of float (* 0–255, modern/space syntax *)
  | Pct of float (* 0%–100% *)
  | Var of channel var

(** CSS angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var

(** CSS alpha values (for HSL/HWB/etc) *)
type alpha =
  | None
  | Num of float (* Number value (0-1) *)
  | Pct of float (* Percentage value (0%-100%) *)
  | Var of alpha var

(** CSS hue values (for HSL/HWB) *)
type hue =
  | Unitless of float (* Unitless number, defaults to degrees *)
  | Angle of angle (* Explicit angle unit *)
  | Var of hue var

(** CSS color component values *)
type component =
  | Number of float
  | Pct of float
  | Angle of hue (* for color(lch ...) / color(lab ...) syntaxes *)
  | Var of component var
  | Calc of component calc

(** CSS percentage values *)
type percentage =
  | Pct of float (* 0%–100% as a % token *)
  | Var of percentage var
  | Calc of percentage calc (* calc(...) that resolves to a % *)

(** CSS hue interpolation options *)
type hue_interpolation = Shorter | Longer | Increasing | Decreasing | Default

(** CSS color values *)
type color =
  | Hex of { hash : bool; value : string }
  | Rgb of { r : channel; g : channel; b : channel }
  | Rgba of { r : channel; g : channel; b : channel; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Oklch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Oklab of { l : percentage; a : float; b : float; alpha : alpha }
  | Lch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Named of color_name
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space option; (* None => default per spec *)
      hue : hue_interpolation;
      color1 : color;
      percent1 : percentage option;
      color2 : color;
      percent2 : percentage option;
    }

(** CSS duration values *)
type duration = Ms of float | S of float | Var of duration var

(** CSS number values (unitless) *)
type number = Float of float | Int of int | Pct of float | Var of number var

(** {1 Constructor Functions} *)

val hex : string -> color
(** [hex s] creates a hex color value. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates an RGB color. *)

val rgba : int -> int -> int -> float -> color
(** [rgba r g b a] creates an RGBA color with alpha. *)

val hsl : float -> float -> float -> color
(** [hsl h s l] creates an HSL color. *)

val hsla : float -> float -> float -> float -> color
(** [hsla h s l a] creates an HSLA color with alpha. *)

val hwb : float -> float -> float -> color
(** [hwb h w b] creates an HWB color. *)

val hwba : float -> float -> float -> float -> color
(** [hwba h w b a] creates an HWB color with alpha. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] creates an OKLCH color. *)

val oklcha : float -> float -> float -> float -> color
(** [oklcha l c h a] creates an OKLCH color with alpha. *)

val oklab : float -> float -> float -> color
(** [oklab l a b] creates an OKLAB color. *)

val oklaba : float -> float -> float -> float -> color
(** [oklaba l a b alpha] creates an OKLAB color with alpha. *)

val lch : float -> float -> float -> color
(** [lch l c h] creates an LCH color. *)

val lcha : float -> float -> float -> float -> color
(** [lcha l c h a] creates an LCH color with alpha. *)

val color_name : color_name -> color
(** [color_name n] creates a named color. *)

val current_color : color
(** [current_color] is the CSS currentcolor value. *)

val transparent : color
(** [transparent] is the CSS transparent value. *)

val color_mix :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  ?percent1:int ->
  ?percent2:int ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 color1 color2] creates a
    color-mix() value. *)

(** {1 Pretty-printing Functions} *)

val pp_length : length Pp.t
(** [pp_length] is the pretty printer for {!length} values. *)

val pp_color : color Pp.t
(** [pp_color] is the pretty printer for {!color} values. *)

val pp_angle : angle Pp.t
(** [pp_angle] is the pretty printer for {!angle} values. *)

val pp_duration : duration Pp.t
(** [pp_duration] is the pretty printer for {!duration} values. *)

val pp_number : number Pp.t
(** [pp_number] is the pretty printer for {!number} values. *)

val pp_percentage : float Pp.t
(** [pp_percentage] is the pretty printer for percentage values. *)

val pp_calc : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc pp_value] is the pretty printer for calc expressions. *)

val pp_color_name : color_name Pp.t
(** [pp_color_name] is the pretty printer for {!color_name} values. *)

val pp_color_space : color_space Pp.t
(** [pp_color_space] is the pretty printer for {!color_space} values. *)

(** {2 Helper Functions} *)

val pp_fun : string -> 'a Pp.t -> Pp.ctx -> 'a -> unit
(** [pp_fun name pp_args] formats function calls as [name(args)]. *)

val pp_fun' : string -> 'a Pp.t -> Pp.ctx -> 'a list -> unit
(** [pp_fun' name pp_item] formats function calls with comma-separated list
    arguments. *)

val pp_var : 'a Pp.t -> 'a var Pp.t
(** [pp_var pp_value] is the pretty printer for CSS variables. *)

(** {1 Calc Module} *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  val sub : 'a calc -> 'a calc -> 'a calc
  val mul : 'a calc -> 'a calc -> 'a calc
  val div : 'a calc -> 'a calc -> 'a calc
  val ( + ) : 'a calc -> 'a calc -> 'a calc
  val ( - ) : 'a calc -> 'a calc -> 'a calc
  val ( * ) : 'a calc -> 'a calc -> 'a calc
  val ( / ) : 'a calc -> 'a calc -> 'a calc
  val length : 'a -> 'a calc
  val var : ?default:'a -> ?fallback:'a -> string -> 'a calc
  val float : float -> length calc
  val infinity : length calc
  val px : float -> length calc
  val rem : float -> length calc
  val em : float -> length calc
  val pct : float -> length calc
end

(** {1 Parsing Functions} *)

val read_length : Reader.t -> length
(** [read_length t] reads a CSS length or keyword value. *)

val read_color : Reader.t -> color
(** [read_color t] reads a CSS color value (hex, rgb()/rgba(), keyword). *)

val read_angle : Reader.t -> angle
(** [read_angle t] reads a CSS angle value. *)

val read_duration : Reader.t -> duration
(** [read_duration t] reads a CSS duration value. *)

val read_number : Reader.t -> number
(** [read_number t] reads a CSS number (int or float). *)

val read_percentage : Reader.t -> float
(** [read_percentage t] reads a percentage value and returns the numeric part.
*)

val read_calc : (Reader.t -> 'a) -> Reader.t -> 'a calc
(** [read_calc t] reads a calc() expression or a value promotable to calc. *)
