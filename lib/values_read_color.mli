(** Colour readers, with the static folds of relative colours. *)

open Values_intf

val read_alpha : Cursor.t -> alpha
(** [read_alpha t] parses a CSS alpha value. *)

val read_channel : Cursor.t -> channel
(** [read_channel t] parses a channel value. *)

val read_rgb : Cursor.t -> rgb
(** [read_rgb t] parses an RGB value. *)

val read_color_space : Cursor.t -> color_space
(** [read_color_space] reads a {!color_space} value. *)

val normalize_hue : float -> float
(** [normalize_hue d] is the hue [d] in degrees reduced to [\[0, 360)]. *)

val read_hue : Cursor.t -> hue
(** [read_hue t] parses a CSS hue value. *)

val conversion_error_of_color : color -> float
(** [conversion_error_of_color c] is the conversion error a fold takes from the
    space [c] is written in. *)

val read_color : Cursor.t -> color
(** [read_color t] parses a CSS color (hex, rgb/rgba, keywords, etc.). *)

val color_has_specified_hue : color -> bool
(** [color_has_specified_hue color] is [true] when [color] contains the obsolete
    [specified hue] interpolation keyword. *)

val color_is_color_4 : color -> bool
(** [color_is_color_4 c] is [true] when [c] uses a CSS Color 4 / 5 construct
    ([lab], {!val-lch}, {!val-oklab}, {!val-oklch}, {!val-hwb}, [color()],
    [color-mix()], [light-dark()], [contrast-color()], or [from <origin>]
    relative forms). *)

val color_uses_feature_where : (string -> bool) -> color -> bool
(** [color_uses_feature_where lacks c] is [true] when [c] uses a CSS Color 4 / 5
    construct whose web-features key [lacks] answers [true] for, so a caller
    holding a target contract can ask whether a browser in it fails to parse
    [c]. *)

val read_system_color : Cursor.t -> system_color
(** [read_system_color t] is the [system_color] parsed from [t]. *)

val read_color_name : Cursor.t -> color_name
(** [read_color_name] reads a {!type-color_name} value: every name
    {!pp_color_name} prints, matched case-insensitively, and read back as the
    constructor that prints it, so [grey] is [Grey] and not the [Gray] that
    prints [gray]. *)

val read_hue_interpolation : Cursor.t -> hue_interpolation
(** [read_hue_interpolation t] parses a hue interpolation method. *)
