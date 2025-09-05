(** CSS Custom Properties (CSS Variables) parsing using Reader API. *)

val is_custom_property : string -> bool
(** [is_custom_property name] is [true] if [name] starts with "--". *)

val read_custom_property_name : Css.Reader.t -> string
(** [read_custom_property_name t] reads a custom property name including the
    leading "--". *)

val read_var : Css.Reader.t -> string * string option
(** [read_var t] reads a [var(--name[, fallback])]. Returns the variable name
    and optional fallback as raw strings. *)

val read_custom_property_value : Css.Reader.t -> string
(** [read_custom_property_value t] reads a custom property value as a raw
    string, preserving nested delimiters and strings. *)
