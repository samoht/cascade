(** Named and system colour keywords, and the colour constructors. *)

open Values_intf

val color_name_is_shortest : name:string -> hex:string -> bool
(** [color_name_is_shortest ~name ~hex] is [true] when the colour name [name] is
    no longer than the [#hex] spelling of the same colour. *)

val color_name_hex : color_name -> string * string
(** [color_name_hex c] is [(name, hex)], the canonical spelling of [c] and its
    hex value without [#]. *)

val color_name_of_string : string -> color_name option
(** [color_name_of_string s] is the named colour spelled by the lower-case [s].
*)

val color_name_of_hex : string -> color_name option
(** [color_name_of_hex hex] is the named colour whose name is the shortest
    spelling of [hex], if any. *)

val read_color_keyword_of_string : string -> color option
(** [read_color_keyword_of_string s] is the color keyword named by the
    lower-case ident [s]: a named color, {!val-transparent}, [currentcolor],
    [auto], [inherit], or a system color. *)

val fold_custom_value_ident : string -> string
(** [fold_custom_value_ident s] is the canonical lower-case spelling of [s] when
    [s] names a color keyword other than a system color, or one of the idents
    {!Parser.fold_value_ident} folds; other idents keep their source case. *)

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

val oklch_none_hue : float -> float -> color
(** [oklch_none_hue l c] creates an OKLCH color whose hue is [none]. *)

val oklab : float -> float -> float -> color
(** [oklab l a b] creates an OKLAB color. *)

val oklaba : float -> float -> float -> float -> color
(** [oklaba l a b alpha] creates an OKLAB color with alpha. *)

val oklaba_none_zeros : float -> float -> float -> float -> color
(** [oklaba_none_zeros l a b alpha] is like {!val-oklaba} but uses [none] for
    zero a/b components. *)

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
(** [color_mix_var_percent ?in_space ?hue ~var_name c1 c2] is like
    {!val-color_mix} but uses a CSS var reference for the first percentage. *)

val color_mix_var_pct_fallback :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  fallback:percentage fallback ->
  color ->
  color ->
  color
(** [color_mix_var_pct_fallback ?in_space ?hue ~var_name ~fallback c1 c2] is
    like {!val-color_mix_var_percent} but with an explicit fallback on the
    percentage variable. *)
