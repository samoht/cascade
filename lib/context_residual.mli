(** Residual evaluation: resolving [var()] references and folding [calc()]
    against a context, keeping whatever does not resolve as residual syntax.
    Every per-type simplifier is an instance of {!Var_residual} or
    {!Calc_residual}. *)

open Context_intf

val read_custom_value :
  'a Properties.kind -> (Cursor.t -> 'a) -> Declaration.declaration -> 'a option
(** [read_custom_value kind read decl] is the value of custom-property [decl] as
    a [kind]: its typed value when typed as [kind], else its tokens read with
    [read]. [None] when [decl] is not a custom property or does not read in
    full. *)

val read_custom_components :
  (Cursor.t -> 'a) -> Declaration.declaration -> 'a option
(** [read_custom_components read decl] is the tokens of custom-property [decl]
    read in full with [read]. *)

val map_var_fallback : ('a -> 'b) -> 'a Values.fallback -> 'b Values.fallback
(** [map_var_fallback f fb] applies [f] to the typed value of [fb], if any. *)

val combine_numeric_values :
  to_number:('a -> float option) ->
  of_number:(float -> 'b) ->
  'a ->
  Values.calc_op ->
  'a ->
  'b option
(** [combine_numeric_values ~to_number ~of_number a op b] is [a op b] for an
    addition or subtraction of two numeric values, rounded as printed. *)

val combine_numeric_value_num :
  to_number:('a -> float option) ->
  of_number:(float -> 'b) ->
  'a ->
  Values.calc_op ->
  float ->
  'b option
(** [combine_numeric_value_num ~to_number ~of_number v op n] is [v op n] for a
    multiplication or a non-zero division, rounded as printed. *)

val normalize_numeric_value :
  to_number:('a -> float option) -> of_number:(float -> 'a) -> 'a -> 'a
(** [normalize_numeric_value ~to_number ~of_number v] is [v] rounded as printed
    when numeric, [v] otherwise. *)

(** [var()] resolution over a value type whose only residual form is a
    reference. *)
module Var_residual : sig
  type 'a simplifier = authored:bool -> visited:string list -> 'a -> 'a
  (** A simplifier over ['a]; [visited] lists the names being resolved, to stop
      cycles. *)

  type 'a ops = {
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a simplifier;
  }
  (** How to see a value of type ['a] as a reference, read one from a custom
      property, and simplify a value that is not a reference. *)

  val simplify :
    ?layer_order:string list -> ?layer:string -> t -> 'a ops -> 'a -> 'a
  (** [simplify ctx ops v] is [v] with every reference [ctx] resolves replaced
      by its value; unresolved ones stay as references. *)
end

(** [var()] resolution and [calc()] folding over a value type with a [calc()]
    form. *)
module Calc_residual : sig
  type 'a simplifier = visited:string list -> 'a -> 'a
  (** A simplifier over ['a]. *)

  type 'a calc_simplifier =
    visited:string list -> 'a Values.calc -> 'a Values.calc
  (** A simplifier over the [calc()] trees of ['a]. *)

  type 'a ops = {
    of_unitless_number : float -> 'a option;
    zero : 'a Values.calc;
        (** The type's canonical unitless zero, returned by [x * 0] when neither
            operand carries a unit. *)
    is_zero : 'a -> bool;
        (** Recognises a literal zero value, so [0 * x] collapses to zero even
            when [x] is an unresolved [var()]. Conservative [false] is always
            safe (keeps the multiplication). *)
    combine_values : 'a -> Values.calc_op -> 'a -> 'a option;
    combine_value_num : 'a -> Values.calc_op -> float -> 'a option;
    normalize_value : 'a -> 'a;
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    as_calc : 'a -> 'a Values.calc option;
    of_calc : 'a Values.calc -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a calc_simplifier -> 'a simplifier;
  }
  (** The arithmetic, reference and [calc()] views of a value type. *)

  val contains_var : 'a Values.calc -> bool
  (** [contains_var c] is whether [c] holds a [var()] reference. *)

  val simplify :
    ?resolve_fallback:('a -> bool) ->
    ?layer_order:string list ->
    ?layer:string ->
    t ->
    'a ops ->
    'a ->
    'a
  (** [simplify ctx ops v] is [v] with the references [ctx] resolves replaced
      and its [calc()] trees folded as far as they go. [resolve_fallback]
      (default always) says which fallbacks may stand for an unresolved
      reference. *)
end
