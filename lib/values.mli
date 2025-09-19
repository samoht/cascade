(** CSS values and units: core types, printers, and parsers. *)

(** {1 Core Types} *)

include module type of Values_intf
(** Shared value/unit types exposed by both implementation and interface. *)

val var_ref :
  ?fallback:'a fallback ->
  ?default:'a ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a var
(** [var_ref ?fallback ?default ?layer ?meta name] creates a CSS variable
    reference to [--name]. *)

val var_ref_empty :
  ?default:'a -> ?layer:string -> ?meta:meta -> string -> 'a var
(** [var_ref_empty ?default ?layer ?meta name] creates a CSS variable reference
    with an empty fallback, i.e. [var(--name,)]. *)

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

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] pretty-prints {!length} values. When [always] is true, units are always included even for zero values (required for @property initial-value). *)

val pp_color : color Pp.t
(** [pp_color] pretty-prints {!color} values. *)

val pp_angle : angle Pp.t
(** [pp_angle] pretty-prints {!angle} values. *)

val pp_duration : duration Pp.t
(** [pp_duration] pretty-prints {!duration} values. *)

val pp_number : number Pp.t
(** [pp_number] pretty-prints {!number} values. *)

val pp_percentage : percentage Pp.t
(** [pp_percentage] pretty-prints {!percentage} values. *)

val pp_length_percentage : length_percentage Pp.t
(** [pp_length_percentage] pretty-prints {!length_percentage} values. *)

val read_length_percentage : Reader.t -> length_percentage
(** [read_length_percentage] reads a {!length_percentage} value. *)

val pp_calc : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc pp] pretty-prints [calc] expressions using [pp] for leaf values. *)

val pp_color_name : color_name Pp.t
(** [pp_color_name] pretty-prints {!color_name} values. *)

val read_color_name : Reader.t -> color_name
(** [read_color_name] reads a {!color_name} value. *)

val pp_color_space : color_space Pp.t
(** [pp_color_space] pretty-prints {!color_space} values. *)

val read_color_space : Reader.t -> color_space
(** [read_color_space] reads a {!color_space} value. *)

(** {2 Helper Functions} *)

val pp_var : 'a Pp.t -> 'a var Pp.t
(** [pp_var pp] pretty-prints CSS variables using [pp] for the payload. *)

val read_var : (Reader.t -> 'a) -> Reader.t -> 'a var
(** [read_var read t] parses a CSS variable with [var(...)] syntax using [read]
    for the payload. Expects to be positioned at [var(] and parses the full
    expression. *)

val read_var_after_ident : (Reader.t -> 'a) -> Reader.t -> 'a var
(** [read_var_after_ident read t] parses a CSS variable after the [var]
    identifier has been consumed. Used in [enum_or_calls ~calls]. *)

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
  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  val float : float -> length calc
  val infinity : length calc
  val px : float -> length calc
  val rem : float -> length calc
  val em : float -> length calc
  val pct : float -> length calc
end

(** {1 Parsing Functions} *)

val read_length :
  ?allow_negative:bool -> ?with_keywords:bool -> Reader.t -> length
(** [read_length t] parses a CSS length. *)

val read_non_negative_length : ?with_keywords:bool -> Reader.t -> length
(** [read_non_negative_length reader] parses a length value that must be
    non-negative. Used for padding properties which cannot have negative values
    per CSS specification. *)

val read_padding_shorthand : Reader.t -> length list
(** [read_padding_shorthand reader] parses a padding shorthand property
    accepting 1-4 space-separated non-negative length values according to CSS
    specification. *)

val read_margin_shorthand : Reader.t -> length list
(** [read_margin_shorthand reader] parses a margin shorthand property accepting
    1-4 space-separated length values according to CSS specification. *)

val read_color : Reader.t -> color
(** [read_color t] parses a CSS color (hex, rgb/rgba, keywords, etc.). *)

val pp_hue : hue Pp.t
(** [pp_hue] pretty-prints {!hue} values. *)

val read_hue : Reader.t -> hue
(** [read_hue t] parses a CSS hue value. *)

val pp_alpha : alpha Pp.t
(** [pp_alpha] pretty-prints {!alpha} values. *)

val read_alpha : Reader.t -> alpha
(** [read_alpha t] parses a CSS alpha value. *)

val pp_meta : meta Pp.t
(** [pp_meta] pretty-prints {!meta} values. *)

val read_meta : Reader.t -> meta
(** [read_meta t] parses metadata. *)

val pp_hue_interpolation : hue_interpolation Pp.t
(** [pp_hue_interpolation] pretty-prints {!hue_interpolation} values. *)

val read_hue_interpolation : Reader.t -> hue_interpolation
(** [read_hue_interpolation t] parses a hue interpolation method. *)

val pp_calc_op : calc_op Pp.t
(** [pp_calc_op] pretty-prints {!calc_op} values. *)

val read_calc_op : Reader.t -> calc_op
(** [read_calc_op t] parses a calc operation. *)

val pp_component : component Pp.t
(** [pp_component] pretty-prints {!component} values. *)

val read_component : Reader.t -> component
(** [read_component t] parses a component value. *)

val pp_channel : channel Pp.t
(** [pp_channel] pretty-prints {!channel} values. *)

val read_channel : Reader.t -> channel
(** [read_channel t] parses a channel value. *)

val read_angle : Reader.t -> angle
(** [read_angle t] parses a CSS angle. *)

val read_duration : Reader.t -> duration
(** [read_duration t] parses a CSS duration. *)

val read_time : Reader.t -> duration
(** [read_time t] parses a CSS time value (can be negative). *)

val read_dimension : Reader.t -> float * string
(** [read_dimension t] parses a dimension (number + unit) returning
    [(value, unit)]. *)

val read_number : Reader.t -> number
(** [read_number t] parses a CSS number (int/float). *)

val read_percentage : Reader.t -> percentage
(** [read_percentage t] parses a CSS percentage. *)

val read_calc : (Reader.t -> 'a) -> Reader.t -> 'a calc
(** [read_calc read t] parses a [calc(...)] expression or a promotable value. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without --). *)

val var_layer : 'a var -> string option
(** [var_layer v] is [v]'s optional layer name. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is [v]'s optional metadata. *)
