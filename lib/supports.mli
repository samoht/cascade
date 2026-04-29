(** Structured [\@supports] conditions for type-safe feature query construction.
*)

(** Supports condition type. Provides type safety and consistent formatting. *)

type property_name

type declaration_feature =
  | Declaration of Declaration.t
  | Empty of property_name
  | Vendor_flag_enabled

type t =
  | Property of declaration_feature  (** [(property: value)] feature test *)
  | Func of string * Component.t list
      (** [name(args)] function test: [selector()], [font-format()],
          [font-tech()], [var()], etc. *)
  | Not of t  (** [not (condition)] negation *)
  | And of t * t  (** [(cond1) and (cond2)] conjunction *)
  | Or of t * t  (** [(cond1) or (cond2)] disjunction *)

val property : string -> string -> t
(** [property name value] parses [name: value] as a structured supports
    declaration feature. *)

val func : string -> string -> t
(** [func name args] parses [args] as CSS component values for a supports
    function feature. *)

val to_string : t -> string
(** [to_string cond] renders the condition as a CSS [\@supports] string. *)

val pp : t Pp.t
(** [pp ctx cond] prints the condition with context-aware spacing. *)

val pp_declaration_feature : declaration_feature Pp.t
(** [pp_declaration_feature ctx feature] prints a supports declaration feature
    without the surrounding condition parentheses. *)

val of_string : ?allow_unwrapped_decl:bool -> string -> t
(** [of_string s] parses a [\@supports] condition string into a structured type.
    Fails if the condition cannot be parsed. *)

val compare : t -> t -> int
(** [compare a b] compares conditions for sorting. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality. *)
