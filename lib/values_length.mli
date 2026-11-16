(** Lengths and length-percentages: units, [calc()] reduction and printing. *)

open Values_intf

type length_unit
(** The unit of a [<length>] dimension. *)

val length_is_zero : length -> bool
(** [length_is_zero l] is [true] when [l] is a literal zero in any length unit
    ([0], [0px], [0%], etc.). *)

val calc_length_unit : length -> (string * float) option
(** [calc_length_unit l] is [Some (unit, value)] when [l] is a dimension, with
    the unit lower-cased. *)

val length_from_calc_unit : string -> float -> length
(** [length_from_calc_unit unit value] is the length {!val-calc_length_unit}
    reads back as [(unit, value)]; a zero pixel length is [Zero]. *)

val value_of_var_resolution : Pp.ctx -> 'b -> 'a var -> 'a option
(** [value_of_var_resolution ctx parse_default v] is the value an inline [ctx]
    substitutes for [v]. *)

val absolute_unit_px_ratio : string -> float option
(** [absolute_unit_px_ratio unit] is [Some ratio] when [unit] is one of the
    absolute units CSS Values 4 sec. 6.2 puts on the [px] scale, with [ratio]
    the number of [px] in one [unit]. *)

val length_percentage_is_vendor_prefixed : length_percentage -> bool
(** [length_percentage_is_vendor_prefixed v] is [true] when [v] is a legacy
    vendor-prefixed intrinsic sizing keyword ([-webkit-max-content] etc.). *)

val length_has_runtime_subst : length -> bool
(** [length_has_runtime_subst l] is [true] when [l] is [var()] / [env()] /
    [attr()] / an anchor query, or a [calc()] containing one. *)

val eval_length_calc : ?ctx:Values_calc.calc_ctx -> length calc -> length calc
(** [eval_length_calc c] reduces [c] with the typed length rules of CSS Values 4
    sec. 10.10.1: a unitless factor scales a length or a percentage, and a
    division folds only when it is exact. *)

val unit_of_string : string -> length_unit option
(** [unit_of_string s] is the length unit spelled by the lower-case [s]. *)

val is_length_unit : string -> bool
(** [is_length_unit unit] is whether [unit] names a CSS length, whatever its
    case: the absolute units, the font- and viewport-relative ones, and the
    container-relative ones. *)

val length_of_unit : length_unit -> float -> length
(** [length_of_unit unit n] is the length [n] in [unit]. *)

val canonical_dimension : length -> length
(** [canonical_dimension l] folds a length written as a spelling of a value the
    constructors already hold back onto the constructor: [1.0px], [+1px], [01px]
    and [1e3px] all become [Px]. *)

val linear_length_calc : length calc -> length calc
(** [linear_length_calc c] merges the same-unit terms of the length sum [c] in
    the order CSS Values 4 sec. 10.13 serialises them. *)

val resolve_lp_calc_vars :
  Pp.ctx -> length_percentage calc -> length_percentage calc
(** [resolve_lp_calc_vars ctx c] substitutes the [var()] leaves an inline [ctx]
    resolves in [c]. *)

val eval_lp_calc :
  ?ctx:Values_calc.calc_ctx -> length_percentage calc -> length_percentage calc
(** [eval_lp_calc ?ctx c] reduces the length-percentage [calc()] [c] with the
    typed rules of CSS Values 4 sec. 10.10.1. *)

val linear_lp_calc : length_percentage calc -> length_percentage calc
(** [linear_lp_calc c] is {!linear_length_calc} on a length-percentage sum. *)

val round_length_step : string -> float -> float -> float
(** [round_length_step] is {!Values_calc.round_to_step}. *)

val try_reduce_typed_min_max :
  length list -> (float -> float -> float) -> length option
(** [try_reduce_typed_min_max xs reduce] reduces a [min()] or [max()] over the
    lengths [xs] with [reduce] when they share a unit. *)

val px_values : length list -> float list option
(** [px_values ls] is the [px] coefficients of [ls] when every length is in
    [px]. *)

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] pretty-prints {!length} values. *)
