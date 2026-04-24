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

(** {1 Constructor Functions} *)

val hex : string -> color
(** [hex s] creates a hex color value. *)

val rgb : ?alpha:float -> int -> int -> int -> color
(** [rgb ?alpha r g b] creates an RGB color with optional alpha. *)

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

val oklaba_none_zeros : float -> float -> float -> float -> color
(** [oklaba_none_zeros l a b alpha] is like [oklaba] but uses [none] for zero
    a/b components. *)

val lch : float -> float -> float -> color
(** [lch l c h] creates an LCH color. *)

val lcha : float -> float -> float -> float -> color
(** [lcha l c h a] creates an LCH color with alpha. *)

val color_name : color_name -> color
(** [color_name n] creates a named color. *)

val minify_color : color -> color
(** [minify_color c] converts named colors to hex when the hex form is shorter
    or equal length, matching Lightning CSS behavior. *)

val current_color : color
(** [current_color] is the CSS currentcolor value. *)

val transparent : color
(** [transparent] is the CSS transparent value. *)

val color_mix :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  ?percent1:float ->
  ?percent2:float ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 color1 color2] creates a
    color-mix() value. *)

val color_mix_var_percent :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  color ->
  color ->
  color
(** [color_mix_var_percent ?in_space ?hue ~var_name c1 c2] is like [color_mix]
    but uses a CSS var reference for the first percentage. *)

val color_mix_var_pct_fallback :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  fallback:percentage fallback ->
  color ->
  color ->
  color
(** [color_mix_var_pct_fallback ?in_space ?hue ~var_name ~fallback c1 c2] is
    like [color_mix_var_percent] but with an explicit fallback on the percentage
    variable. Used for named opacity modifiers where the fallback is either a
    concrete number ([Fallback (Num 0.5)]) or a theme variable reference
    ([Var_fallback "custom-opacity"]). *)

(** {1 Pretty-printing Functions} *)

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] pretty-prints {!length} values. When [always] is true,
    units are always included even for zero values (required for CSS [@property]
    initial-value). *)

val pp_color : color Pp.t
(** [pp_color] pretty-prints {!color} values. *)

val pp_color_in_mix : color Pp.t
(** [pp_color_in_mix] pretty-prints color values for use inside color-mix
    functions, using lowercase [currentcolor]. *)

val pp_angle : angle Pp.t
(** [pp_angle] pretty-prints {!angle} values. *)

val pp_duration : duration Pp.t
(** [pp_duration] pretty-prints {!duration} values. *)

val pp_number : number Pp.t
(** [pp_number] pretty-prints {!number} values. *)

val pp_transition_behavior : transition_behavior Pp.t
(** [pp_transition_behavior] pretty-prints {!transition_behavior} values. *)

val pp_percentage : ?always:bool -> percentage Pp.t
(** [pp_percentage ?always] pretty-prints {!percentage} values. When [always] is
    true, always includes the % unit even for 0. *)

val pp_length_percentage : ?always:bool -> length_percentage Pp.t
(** [pp_length_percentage ?always] pretty-prints {!length_percentage} values.
    When [always] is true, always includes units even for 0. *)

val pp_number_percentage : ?always:bool -> number_percentage Pp.t
(** [pp_number_percentage ?always] pretty-prints {!number_percentage} values.
    When [always] is true, always includes units even for 0. *)

val pp_calc : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc pp] pretty-prints [calc] expressions using [pp] for leaf values. *)

val pp_color_name : color_name Pp.t
(** [pp_color_name] pretty-prints {!color_name} values. *)

val read_color_name : Cursor.t -> color_name
(** [read_color_name] reads a {!color_name} value. *)

val pp_color_space : color_space Pp.t
(** [pp_color_space] pretty-prints {!color_space} values. *)

val read_color_space : Cursor.t -> color_space
(** [read_color_space] reads a {!color_space} value. *)

(** {2 Helper Functions} *)

val pp_var : 'a Pp.t -> 'a var Pp.t
(** [pp_var pp] pretty-prints CSS variables using [pp] for the payload. *)

val read_var : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var read t] parses a CSS variable with [var(...)] syntax using [read]
    for the payload. Expects to be positioned at [var(] and parses the full
    expression. *)

val read_var_after_ident : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var_after_ident read t] is a compatibility alias for {!read_var}.
    Component cursors receive [var(...)] as a single function component, so [t]
    should be positioned at [var(]. *)

(** {1 Calc Module} *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  (** [add a b] is the calc addition [a + b]. *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub a b] is the calc subtraction [a - b]. *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul a b] is the calc multiplication [a * b]. *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div a b] is the calc division [a / b]. *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [a + b] is the calc addition. *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [a - b] is the calc subtraction. *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [a * b] is the calc multiplication. *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [a / b] is the calc division. *)

  val length : 'a -> 'a calc
  (** [length v] wraps a value as a calc leaf. *)

  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  (** [var ?default ?fallback name] is a CSS variable reference in calc. *)

  val float : float -> length calc
  (** [float f] is a numeric literal in calc. *)

  val infinity : length calc
  (** [infinity] is the CSS infinity value in calc. *)

  val px : float -> length calc
  (** [px f] is a pixel length in calc. *)

  val rem : float -> length calc
  (** [rem f] is a rem length in calc. *)

  val em : float -> length calc
  (** [em f] is an em length in calc. *)

  val pct : float -> length calc
  (** [pct f] is a percentage value in calc. *)

  val nested : 'a calc -> 'a calc
  (** [nested inner] wraps [inner] in an explicit nested [calc()] call. *)

  val parens : 'a calc -> 'a calc
  (** [parens inner] wraps [inner] in parentheses only. *)
end

(** {1 Parsing Functions} *)

val read_length :
  ?allow_negative:bool -> ?with_keywords:bool -> Cursor.t -> length
(** [read_length t] parses a CSS length. *)

val read_non_negative_length : ?with_keywords:bool -> Cursor.t -> length
(** [read_non_negative_length reader] parses a length value that must be
    non-negative. Used for padding properties which cannot have negative values
    per CSS specification. *)

val read_padding_shorthand : Cursor.t -> length list
(** [read_padding_shorthand reader] parses a padding shorthand property
    accepting 1-4 space-separated non-negative length values according to CSS
    specification. *)

val read_margin_shorthand : Cursor.t -> length list
(** [read_margin_shorthand reader] parses a margin shorthand property accepting
    1-4 space-separated length values according to CSS specification. *)

val read_color : Cursor.t -> color
(** [read_color t] parses a CSS color (hex, rgb/rgba, keywords, etc.). *)

val pp_hue : hue Pp.t
(** [pp_hue] pretty-prints {!hue} values. *)

val read_hue : Cursor.t -> hue
(** [read_hue t] parses a CSS hue value. *)

val pp_alpha : alpha Pp.t
(** [pp_alpha] pretty-prints {!alpha} values. *)

val read_alpha : Cursor.t -> alpha
(** [read_alpha t] parses a CSS alpha value. *)

val pp_hue_interpolation : hue_interpolation Pp.t
(** [pp_hue_interpolation] pretty-prints {!hue_interpolation} values. *)

val read_hue_interpolation : Cursor.t -> hue_interpolation
(** [read_hue_interpolation t] parses a hue interpolation method. *)

val pp_calc_op : calc_op Pp.t
(** [pp_calc_op] pretty-prints {!calc_op} values. *)

val read_calc_op : Cursor.t -> calc_op
(** [read_calc_op t] parses a calc operation. *)

val pp_component : component Pp.t
(** [pp_component] pretty-prints {!component} values. *)

val read_component : Cursor.t -> component
(** [read_component t] parses a component value. *)

val pp_channel : channel Pp.t
(** [pp_channel] pretty-prints {!channel} values. *)

val read_channel : Cursor.t -> channel
(** [read_channel t] parses a channel value. *)

val pp_rgb : rgb Pp.t
(** [pp_rgb] pretty-prints {!rgb} values. *)

val read_rgb : Cursor.t -> rgb
(** [read_rgb t] parses an RGB value. *)

val read_angle : Cursor.t -> angle
(** [read_angle t] parses a CSS angle. *)

val read_duration : Cursor.t -> duration
(** [read_duration t] parses a CSS duration. *)

val read_time : Cursor.t -> duration
(** [read_time t] parses a CSS time value (can be negative). *)

val read_number : Cursor.t -> number
(** [read_number t] parses a CSS number (int/float). *)

val read_transition_behavior : Cursor.t -> transition_behavior
(** [read_transition_behavior t] parses a CSS transition-behavior value. *)

val read_percentage : Cursor.t -> percentage
(** [read_percentage t] parses a CSS percentage. *)

val read_length_percentage : Cursor.t -> length_percentage
(** [read_length_percentage t] parses a CSS length or percentage. *)

val read_number_percentage : Cursor.t -> number_percentage
(** [read_number_percentage t] parses a CSS number or percentage. *)

val read_calc : (Cursor.t -> 'a) -> Cursor.t -> 'a calc
(** [read_calc read t] parses a [calc(...)] expression or a promotable value. *)

val eval_numeric_calc : 'a calc -> float option
(** [eval_numeric_calc calc] tries to evaluate a calc expression containing only
    numbers to a float. Returns [None] if the expression contains variables or
    non-numeric values. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without --). *)

val var_layer : 'a var -> string option
(** [var_layer v] is [v]'s optional layer name. *)

val with_fallback : 'a var -> 'a -> 'a var
(** [with_fallback var_ref fallback_value] creates a new variable reference with
    the same variable name but a different fallback value. This is useful when
    you need to reference a variable from another module with a specific
    fallback, without creating a declaration. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is [v]'s optional metadata. *)

val pp_system_color : system_color Pp.t
(** [pp_system_color] is the pretty-printer for [system_color]. *)

val read_system_color : Cursor.t -> system_color
(** [read_system_color t] is the [system_color] parsed from [t]. *)
