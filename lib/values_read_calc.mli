(** Readers for [calc()], the math functions and their result types. *)

open Values_intf

val is_element_relative_unit : string -> bool
(** [is_element_relative_unit u] is whether [u] is one of CSS Values 4 sec.
    5.1.1's font-relative or sec. 5.1.4's container-relative length units, the
    ones that resolve against something an element is given. *)

val normalize_signed_zero : float -> string -> float * string
(** [normalize_signed_zero n repr] collapses any zero to the canonical [0]: when
    [n] is zero it returns [(0., "0")], dropping a signed or over-precise
    authored spelling so [-0px] / [+0px] / [0.0px] all read (and serialise) with
    a [0] repr. *)

val read_length_unit :
  ?allow_negative:bool -> ?length_only:bool -> Cursor.t -> length
(** [read_length_unit ?allow_negative ?length_only t] parses a length dimension.
*)

val read_length_keyword : ?sizing:bool -> Cursor.t -> length
(** [read_length_keyword ?sizing t] parses a length keyword, with the sizing
    keywords when [sizing]. *)

val read_round_strategy : Cursor.t -> string
(** [read_round_strategy t] parses the optional [round()] strategy, [nearest] by
    default. *)

val read_round_call :
  (string -> 'a -> 'a -> 'b) -> (Cursor.t -> 'a) -> Cursor.t -> 'b
(** [read_round_call make read t] parses a [round()] call over [read] and builds
    it with [make]. *)

val read_binary_call :
  string -> ('a -> 'a -> 'b) -> (Cursor.t -> 'a) -> Cursor.t -> 'b
(** [read_binary_call name make read t] parses the two-argument math call [name]
    over [read] and builds it with [make]. *)

val read_calc_expr : (Cursor.t -> 'a) -> Cursor.t -> 'a calc
(** [read_calc_expr read t] parses a calc expression body -- the contents of a
    [calc(...)] form without the surrounding [calc(] and [)]. *)

val read_calc_numeric_function : Cursor.t -> 'a calc
(** [read_calc_numeric_function t] parses a numeric math function call as a
    [calc()] operand. *)

val read_math_arg : Cursor.t -> math_arg
(** [read_math_arg t] parses a numeric math argument. *)

val read_math_call_arg : string -> Cursor.t -> math_arg
(** [read_math_call_arg name t] parses the one-argument math call [name]. *)

val read_num_expr : Cursor.t -> float
(** [read_num_expr t] parses a number-typed calc expression and folds it to a
    float. *)

val validate_calc_type :
  Cursor.t ->
  [< `Number | `Number_or_percentage | `Number_or_value | `Percentage | `Value ] ->
  'a calc ->
  unit
(** [validate_calc_type t result_type calc] raises unless [calc] infers to
    [result_type], the check CSS Values 4 sec. 10.9 makes on a calculation's
    type. *)

val typed_math_function_names : string list
(** [typed_math_function_names] is the math functions a [<length>] slot takes as
    readily as a [<number>] one. *)

val looking_at_math_function : Cursor.t -> bool
(** [looking_at_math_function t] is [true] on a call to a math function other
    than [calc()]. *)

val math_function_calls : 'a -> (string * 'a) list
(** [math_function_calls read] pairs every math-function name other than
    [calc()] with [read], for the [~calls] of a {!Cursor.enum_or_calls} whose
    [calc()] entry already reads one. *)

val typed_math_function_calls : 'a -> (string * 'a) list
(** [typed_math_function_calls read] is {!math_function_calls} restricted to the
    functions that answer the type of their arguments, for a slot that takes a
    [<length>] or another dimension rather than a [<number>]. *)

val math_function_calls_beside_comparisons : 'a -> (string * 'a) list
(** [math_function_calls_beside_comparisons read] is {!math_function_calls}
    without [min()], [max()] and [clamp()], for a slot reading those three
    itself. *)

val percentage_math_function_calls :
  pct:(Cursor.t -> float -> 'a) ->
  (Cursor.t -> 'a) ->
  (string * (Cursor.t -> 'a)) list
(** [percentage_math_function_calls ~pct read] is {!math_function_calls} for a
    slot spelling a [<percentage>] and no [<number>] beside it. *)

val read_folded_percentage_math : Cursor.t -> float
(** [read_folded_percentage_math t] reads a math function at a slot spelling a
    [<percentage>] and answers the percentage it resolves to. *)

val looking_at_percentage_math : Cursor.t -> bool
(** [looking_at_percentage_math t] is [true] on a [calc()] or on any other math
    function, the two spellings {!read_folded_percentage_math} reads. *)

val read_calc :
  ?result_type:
    [ `Number | `Number_or_percentage | `Number_or_value | `Percentage | `Value ] ->
  (Cursor.t -> 'a) ->
  Cursor.t ->
  'a calc
(** [read_calc ~result_type read t] parses a [calc(...)] expression or a
    promotable value and checks that its statically knowable result type matches
    the property's numeric grammar. *)

val read_integer_calc : string -> Cursor.t -> [ `Calc of 'a calc | `Int of int ]
(** [read_integer_calc name t] parses the math function at an [<integer>]
    position, which CSS Values 4 sec. 10.9 accepts wherever a literal integer
    is. *)

val calc_integer_value : 'a calc -> int option
(** [calc_integer_value calc] is the integer a math function stands for at an
    [<integer>] slot: CSS Values 4 sec. 10.12 rounds it to the nearest integer,
    a tie going toward positive infinity. *)

val read_integer : string -> Cursor.t -> int
(** [read_integer name t] parses an [<integer>] position whose value type
    carries no calc node: a literal integer, or a [calc()] folding to one. *)

val read_numeric_expression : Cursor.t -> float
(** [read_numeric_expression t] parses and evaluates a numeric math expression,
    including top-level math functions such as [min()], [max()], and [clamp()].
*)

val read_number_percentage_expression : Cursor.t -> float
(** [read_number_percentage_expression t] is {!read_numeric_expression} at a
    [<number> | <percentage>] slot, where a percentage resolves against the
    number it denotes. *)

val read_anchor_size_length : Cursor.t -> length
(** [read_anchor_size_length t] parses the arguments of [anchor-size()]. *)

val read_anchor_name_side : Cursor.t -> string -> string option * string
(** [read_anchor_name_side t first] parses the anchor name and side of
    [anchor()], [first] being the ident already read. *)

val read_attr_type_hint : Cursor.t -> attr_type option
(** [read_attr_type_hint t] parses the optional type hint of a length [attr()].
*)

val read_calc_op : Cursor.t -> calc_op
(** [read_calc_op t] parses a calc operation. *)

val read_component : Cursor.t -> component
(** [read_component t] parses a component value. *)
