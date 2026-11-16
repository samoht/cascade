(** Math functions and [calc()] trees: static evaluation, the typed reduction of
    CSS Values 4 sec. 10.10.1, and printing. *)

open Values_intf

(** What a static math function reduces to: a plain coefficient, or one that
    carries a unit. *)
type math_result = Scalar of float | United of float * string

type calc_ctx = { var_is_single_valued : string -> bool; budget : bool }
(** Context threaded through the calc folds for stylesheet-dependent rewrites.
    [var_is_single_valued n] reports whether [--n] is registered with a
    single-component [@property] syntax, so its [var()] substitutes exactly one
    calc term and a redundant [calc(var(--n))] nested inside another [calc()]
    may be unwrapped. [budget] folds a non-terminating quotient to the
    six-significant-figure computed-value budget, as a number's fold already
    does, so [calc(1/3 * 100%)] is [33.3333%]; without it the quotient keeps the
    [calc()] the author wrote, which is what {!default_calc_ctx} and the
    minifier do. The canonical comparison spends the budget, since the two
    spellings render alike. *)

val pp_calc_op : calc_op Pp.t
(** [pp_calc_op] pretty-prints {!calc_op} values. *)

val pp_math_const : Pp.ctx -> math_const -> unit
(** [pp_math_const] pretty-prints a math constant. *)

val read_math_const : Cursor.t -> math_const
(** [read_math_const t] parses a math constant. *)

val round_to_step : string -> float -> float -> float
(** [round_to_step strategy value step] is CSS Values 4 sec. 10.7.3's [round()]:
    the multiple of [step] the strategy names, and for the default [nearest] the
    closest one with a tie going toward positive infinity. *)

val mod_value : float -> float -> float
(** [mod_value a b] is [a] modulo [b] with the sign of [b], or [a] when [b] is
    zero. *)

val pp_math_arg : math_arg Pp.t
(** [pp_math_arg] pretty-prints a numeric math argument. *)

val pp_math_fn : Pp.ctx -> math_fn -> unit
(** [pp_math_fn] pretty-prints a numeric math function call. *)

val eval_math_fn : math_fn -> float option
(** [eval_math_fn fn] evaluates a CSS Values 4 (ED) sec. 9.1 numeric math
    function call to a float. *)

val math_arg_result : math_arg -> math_result option
(** [math_arg_result a] is what the static math argument [a] reduces to. *)

val math_fn_result : math_fn -> math_result option
(** [math_fn_result fn] is what the static math function call [fn] reduces to.
*)

val fold_united_nan : math_result option -> math_result option
(** [fold_united_nan r] drops a NaN result that carries a unit, which has no
    spelling. *)

val typed_math_fn : (string -> float -> 'a option) -> math_fn -> 'a calc
(** [typed_math_fn of_unit fn] reduces [fn] inside a typed [calc()], rebuilding
    a leaf through [of_unit] or keeping the call. *)

val math_fn_contains_var : math_fn -> bool
(** [math_fn_contains_var fn] is [true] when an argument of [fn] holds a
    [var()]. *)

val calc_contains_var : 'a calc -> bool
(** [calc_contains_var c] is [true] when [c] holds a [var()]. *)

val pp_calc_contents : 'a Pp.t -> 'a calc Pp.t
(** [pp_calc_contents pp] pretty-prints the body of a [calc()] using [pp] for
    leaves. *)

val round_computed : float -> float
(** [round_computed f] is [f] at the six significant figures of a serialised
    computed value. *)

val exact_div : float -> float -> float option
(** [exact_div a b] is [a / b] when the quotient is exact. *)

val scale_leaf :
  exact:bool -> calc_op -> float -> (float -> 'a) -> float -> 'a option
(** [scale_leaf ~exact op n rebuild v] scales the leaf coefficient [v] by [n],
    rebuilding the leaf through [rebuild]. *)

val combine_leaf : calc_op -> float -> float -> float option
(** [combine_leaf op x y] adds or subtracts two same-unit coefficients. *)

val calc_identity :
  zero:'a calc ->
  is_zero:('a -> bool) ->
  'a calc ->
  calc_op ->
  'a calc ->
  'a calc option
(** [calc_identity ~zero ~is_zero l op r] applies the value-independent CSS
    Values 4 sec. 10.10.1 identities to one [Expr (l, op, r)] node, returning
    the folded operand when one applies. *)

val default_calc_ctx : calc_ctx
(** [default_calc_ctx] knows of no single-valued variables, so every
    context-dependent calc rewrite is a no-op. *)

val eval_calc : ?ctx:calc_ctx -> 'a calc -> 'a calc
(** [eval_calc calc] applies CSS Values 4 sec. 10.10.1 structural
    simplification: folds [Expr (Num _, op, Num _)] subtrees into a single [Num]
    and unwraps trivial [Nested] / [Parens] around leaves. *)

val eval_typed_calc :
  math_fn:(math_fn -> 'a calc) ->
  scale:(exact:bool -> calc_op -> 'a -> float -> 'a option) ->
  combine:(calc_op -> 'a -> 'a -> 'a option) ->
  round:('a -> 'a) ->
  is_zero:('a -> bool) ->
  zero:'a calc ->
  ctx:calc_ctx ->
  'a calc ->
  'a calc
(** [eval_typed_calc ~math_fn ~scale ~combine ~round ~is_zero ~zero ~ctx c] is
    the typed [calc()] reduction of CSS Values 4 sec. 10.10.1, with the leaf
    operations of one type. *)

val negative_length : length -> bool
(** [negative_length l] is [true] when [l] is a negative dimension. *)

val pp_calc_with :
  ?unwrap_num:bool ->
  ?unwrap:('a -> bool) ->
  ?pp_unwrapped:'a Pp.t ->
  'a Pp.t ->
  'a calc Pp.t
(** [pp_calc_with ?unwrap_num ?unwrap ?pp_unwrapped pp] is {!pp_calc} on its own
    arguments. *)

val pp_calc :
  ?unwrap_num:bool ->
  ?unwrap:('a -> bool) ->
  ?pp_unwrapped:'a Pp.t ->
  'a Pp.t ->
  Pp.ctx ->
  'a calc ->
  unit
(** [pp_calc ?unwrap_num ?unwrap ?pp_unwrapped pp] pretty-prints [calc]
    expressions using [pp] for leaf values, and [pp_unwrapped] for the one leaf
    that comes out of the call, which is no longer an operand and takes the
    spelling a leaf on its own takes; it defaults to [pp]. *)

val pp_unit : ?always:'a -> Pp.ctx -> float -> string -> unit
(** [pp_unit ctx f suffix] prints the dimension [f] followed by its unit
    [suffix]. *)

val numeric_calc_result : 'a calc -> math_result option
(** [numeric_calc_result c] is what the static [calc()] [c] reduces to, with the
    unit [hypot()] and [abs()] carry. *)

val eval_numeric_calc : 'a calc -> float option
(** [eval_numeric_calc calc] tries to evaluate a calc expression containing only
    numbers to a float. *)

val map_calc : ('a -> 'b) -> 'a calc -> 'b calc
(** [map_calc f calc] rewrites every [Val] leaf via [f], preserving the calc
    structure (operators, [Nested], [Parens], [Var] fallbacks). *)

val map_calc_opt : ('a -> 'b option) -> 'a calc -> 'b calc option
(** [map_calc_opt f calc] is {!map_calc} for a partial [f]: the result is [None]
    as soon as one [Val] leaf has no image, and a [Var] / [Sibling_index] /
    [Sibling_count] node fails the same way since it carries no leaf to retype.
*)

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

  val length : length -> length calc
  (** [length v] wraps a value as a calc leaf. *)

  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  (** [var ?default ?fallback name] is a CSS variable reference in calc. *)

  val float : float -> 'a calc
  (** [float f] is a dimensionless [<number>] literal in calc. Polymorphic per
      CSS Values 4 sec. 10: a [<number>] multiplied by a [<length>] yields a
      [<length>], by an [<angle>] yields an [<angle>], etc., so the same literal
      flows into any dimension. *)

  val infinity : 'a calc
  (** [infinity] is the CSS [infinity] math constant in calc. Polymorphic for
      the same reason as {!float}: a math constant is a [<number>]. *)

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
