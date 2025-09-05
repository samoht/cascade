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
type 'a calc = Var of 'a var | Val of 'a | Expr of 'a calc * calc_op * 'a calc

(** {1 Value Types} *)

(** CSS length values *)
type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
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

(** CSS color values *)
type color =
  | Hex of { hash : bool; value : string }
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }
  | Named of color_name
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space;
      color1 : color;
      percent1 : int option;
      color2 : color;
      percent2 : int option;
    }

(** CSS angle values *)
type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var

(** CSS duration values *)
type duration = Ms of int | S of float | Var of duration var

(** CSS number values (unitless) *)
type number = Float of float | Int of int | Pct of float | Var of number var

(** {1 Constructor Functions} *)

val hex : string -> color
(** [hex s] creates a hex color value. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates an RGB color. *)

val rgba : int -> int -> int -> float -> color
(** [rgba r g b a] creates an RGBA color with alpha. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] creates an OKLCH color. *)

val color_name : color_name -> color
(** [color_name n] creates a named color. *)

val current_color : color
(** [current_color] is the CSS currentcolor value. *)

val transparent : color
(** [transparent] is the CSS transparent value. *)

val color_mix :
  ?in_space:color_space ->
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
  val px : int -> length calc
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

val read_calc : Reader.t -> length calc
(** [read_calc t] reads a calc() expression or a value promotable to calc. *)
