(** Angles, numbers, percentages and durations: canonicalisation and printing.
*)

open Values_intf

val angle_degrees_opt : angle -> float option
(** [angle_degrees_opt a] converts concrete angle units to degrees. *)

val pp_angle : angle Pp.t
(** [pp_angle] pretty-prints {!angle} values. *)

val pp_percentage : ?always:bool -> percentage Pp.t
(** [pp_percentage ?always] pretty-prints {!percentage} values. *)

val normalize_length :
  ?strip:bool ->
  ?non_negative:bool ->
  ?ctx:Values_calc.calc_ctx ->
  length ->
  length
(** [normalize_length ?strip l] evaluates the static CSS math functions on a
    [<length>] (min / max / clamp reduce to one dimension on shared units; round
    / mod / rem / hypot / abs fold on {!Calc.px}; calc folds through the
    simplifier), recursing into nested calls; a non-static operand keeps the
    call. *)

val normalize_length_percentage :
  ?strip:bool ->
  ?non_negative:bool ->
  ?ctx:Values_calc.calc_ctx ->
  length_percentage ->
  length_percentage
(** [normalize_length_percentage lp] folds the numeric parts of a [calc()],
    keeping any [var()]: [calc(var(--x) + 1px + 2px)] becomes
    [calc(var(--x) + 3px)], and [calc(1px + 2px)] becomes [3px]. *)

val normalize_number :
  ?ctx:Values_calc.calc_ctx -> ?non_negative:bool -> number -> number
(** [normalize_number n] evaluates the static CSS math functions on a [<number>]
    ([hypot(3, 4)] becomes [5], [calc(1 + 2)] becomes [3]), recursing into
    nested calls; an operand with a [var()] keeps the call. *)

val pct_scale :
  ?exact:bool -> calc_op -> percentage -> float -> percentage option
(** [pct_scale ?exact op p n] scales the percentage [p] by [n]. *)

val pct_is_zero : percentage -> bool
(** [pct_is_zero p] is [true] when [p] is a literal zero. *)

val normalize_percentage : ?ctx:Values_calc.calc_ctx -> percentage -> percentage
(** [normalize_percentage p] folds the value-independent parts of a
    [<percentage>] [calc()] ([calc(1 / 2 * 100%)] becomes [calc(.5*100%)]),
    keeping any [var()]. *)

val normalize_duration :
  ?ctx:Values_calc.calc_ctx ->
  ?canonicalize_ms:bool ->
  ?non_negative:bool ->
  duration ->
  duration
(** [normalize_duration d] folds the value-independent parts of a [<time>]
    [calc()] ([calc(var(--d) * 1)] becomes [calc(var(--d))]), keeping any
    [var()]. *)

val normalize_angle : ?ctx:Values_calc.calc_ctx -> angle -> angle
(** [normalize_angle a] folds the static angle math functions (round / mod / rem
    on [deg] operands) and converts to the shortest of the
    losslessly-interconvertible units (deg / turn / grad); [rad] (irrational via
    pi) stays as-is. *)

val pp_length_percentage : ?always:bool -> length_percentage Pp.t
(** [pp_length_percentage ?always] pretty-prints {!length_percentage} values. *)

val pp_number_percentage : number_percentage Pp.t
(** [pp_number_percentage] pretty-prints {!number_percentage} values. *)

val normalize_number_percentage :
  ?ctx:Values_calc.calc_ctx -> number_percentage -> number_percentage
(** [normalize_number_percentage np] picks the shorter spelling for a typed
    [<number-percentage>] leaf where percentage and number are spec-equivalent
    (100% = 1, e.g. transform [scale()], the [scale] property, and the [filter]
    [brightness()]/[contrast()]/... functions; CSS Transforms 2 secs. 5 and
    12.1-12.2 and Filter Effects 1 sec. 6.1). *)

val pp_duration : duration Pp.t
(** [pp_duration] pretty-prints {!duration} values. *)

val pp_duration_preserve_ms : duration Pp.t
(** [pp_duration_preserve_ms] pretty-prints {!duration} values without
    shortening milliseconds to seconds. *)

val pp_number : number Pp.t
(** [pp_number] pretty-prints {!number} values. *)

val pp_transition_behavior : transition_behavior Pp.t
(** [pp_transition_behavior] pretty-prints {!transition_behavior} values. *)
