(** Custom-property values: typed re-reading, colour and math folding, token
    canonicalisation and printing. *)

open Values
open Properties_intf

val read_custom_property_value :
  ?font_family:bool -> Cursor.t -> custom_property_value
(** [read_custom_property_value t] returns the remaining component values as a
    raw token stream. Typed promotion is deferred to the [@property] registry
    pass. *)

val is_color_function : string -> bool
(** [is_color_function name] is true for a function whose own syntax fixes it as
    a colour: the CSS Color 4 numeric notations, [color()] and [color-mix()],
    and [light-dark()]. A gradient is not one: its type is fixed by the property
    it lands in, not by the function. *)

val is_math_function : string -> bool
(** [is_math_function name] is true for a CSS Values 4 sec. 10 math function:
    [calc()], the comparison functions ([min()], [max()], [clamp()]), the
    stepped-value ([round()], [mod()], [rem()]), trigonometric ([sin()] through
    [atan2()]), exponential ([pow()], [sqrt()], [hypot()], [log()], [exp()]) and
    sign-related ([abs()], [sign()]) families. [name] is matched case
    insensitively, as CSS function names are. *)

val canonicalize_custom_shadow_components :
  lossless:bool -> Values.invalid_value -> Values.invalid_value
(** [canonicalize_custom_shadow_components ~lossless components] is [components]
    with the colour of a complete shadow canonicalised, the authored shadow
    shape otherwise kept. *)

val canonicalize_custom_colors_components :
  lossless:bool -> ?resolve_missing:bool -> Component.t list -> Component.t list
(** [canonicalize_custom_colors_components ~lossless components] is [components]
    with every colour it holds folded to its canonical form. *)

val try_read_custom_color : custom_value -> custom_property_value option
(** [try_read_custom_color tokens] parses [tokens] as a [<color>], returning a
    typed payload when the stream matches. *)

val try_read_custom_length : custom_value -> custom_property_value option
(** [try_read_custom_length tokens] parses [tokens] as a [<length>]. *)

val try_read_custom_length_percentage :
  custom_value -> custom_property_value option
(** [try_read_custom_length_percentage tokens] parses [tokens] as a
    [<length-percentage>]. *)

val try_read_custom_number : custom_value -> custom_property_value option
(** [try_read_custom_number tokens] parses [tokens] as a [<number>]. *)

val try_read_custom_percentage : custom_value -> custom_property_value option
(** [try_read_custom_percentage tokens] parses [tokens] as a [<percentage>]. *)

val try_read_custom_angle : custom_value -> custom_property_value option
(** [try_read_custom_angle tokens] parses [tokens] as an [<angle>]. *)

val try_read_custom_time : custom_value -> custom_property_value option
(** [try_read_custom_time tokens] parses [tokens] as a [<time>]. *)

val pp_value : ('a kind * 'a) Pp.t
(** [pp_value] pretty-prints a typed custom property value. *)

val string_of_kind_value : 'a kind -> 'a -> string
(** [string_of_kind_value kind value] serializes a typed CSS value for a custom
    property initial value. Values that cannot be represented as a concrete
    initial value serialize to ["initial"]. *)

val pp_custom_property_value : custom_property_value Pp.t
(** [pp_custom_property_value] pretty-prints a parsed custom-property payload.
*)

val components_of_custom_property_value :
  custom_property_value -> Component.t list
(** [components_of_custom_property_value value] returns the component stream
    represented by a custom-property payload. Typed values are serialized with
    the normal minified typed printer before being parsed back to components. *)

val pp_custom_property : custom_property Pp.t
(** [pp_custom_property] prints a custom property's value. *)

val canonicalize_math_whitespace_components : custom_value -> custom_value
(** [canonicalize_math_whitespace_components components] drops the whitespace of
    [components] that CSS reads as nothing: around the [*] and [/] of CSS Values
    4 (ED) sec. 10.8 arithmetic, and after a function or block whose closing
    bracket already separates it from what follows. The whitespace sec. 10.8
    requires around a math [+] or [-], and the whitespace next to a [var()],
    [env()] or [attr()] whose substitution would otherwise merge with its
    neighbour, both stay. *)

val canonicalize_time_components : custom_value -> custom_value
(** [canonicalize_time_components components] spells every [<time>] token of
    [components] the way a minified typed [<time>] is spelled, the shorter of
    its [s] and [ms] forms, which CSS Values 4 sec. 7.2 makes one quantity.
    Equivalence-only normalisation for structural diffing; every other token
    passes through unchanged. *)

val canonicalize_angle_components : custom_value -> custom_value
(** [canonicalize_angle_components components] spells every [<angle>] token of
    [components] as the degrees it names, a [rad], [turn] or [grad] rounded to
    the six-significant-figure budget a computed value takes, which CSS Values 4
    sec. 6.1 makes one dimension. Equivalence-only normalisation for structural
    diffing; every other token passes through unchanged. *)

val normalize_custom_property_value :
  ?lossless:bool ->
  ?ctx:Values.calc_ctx ->
  custom_property_value ->
  custom_property_value
(** [normalize_custom_property_value v] canonicalises the typed value of a
    registered custom property (currently the [<color>] syntax) so a promoted
    custom-property value folds the same way a typed colour property would. *)
