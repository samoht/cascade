(** Structured [\@supports] conditions for type-safe feature query construction.
*)

(** Supports condition type. Provides type safety and consistent formatting. *)
type t =
  | Property of string * string  (** [(property: value)] feature test *)
  | Selector of Selector.t  (** [selector(sel)] selector test *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS [\@supports] string. *)

val pp : t Pp.t
(** [pp ctx cond] prints the condition with context-aware spacing. *)

val of_string : string -> t
(** [of_string s] parses a [\@supports] condition string into a structured type.
    Fails if the condition cannot be parsed. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)
