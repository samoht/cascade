(** Whether declarations can be lifted out of rules into a shared one.

    Factoring moves declarations a group of rules has in common into one new
    rule and leaves each member what remains. The predicates below say when that
    move changes what an element computes, reading the declarations alone;
    declarations compare by their minified text. *)

open Stylesheet

val unique_decls : Declaration.declaration list -> Declaration.declaration list
(** [unique_decls ds] is [ds] with each declaration kept at its first
    occurrence. *)

val common_exact_decls : rule list -> Declaration.declaration list
(** [common_exact_decls rules] is the declarations of the first rule that every
    other rule also holds, once each, in the first rule's order. *)

val remove_common :
  Declaration.declaration list ->
  Declaration.declaration list ->
  Declaration.declaration list
(** [remove_common common ds] is [ds] without the declarations of [common]. *)

val append_unique_decls :
  Declaration.declaration list ->
  Declaration.declaration list ->
  Declaration.declaration list
(** [append_unique_decls base extras] is [base] followed by the declarations of
    [extras] it does not hold yet. *)

val decl_mem : Declaration.declaration -> Declaration.declaration list -> bool
(** [decl_mem d ds] is whether [ds] holds [d]. *)

val lifts_over_leftover : Commute.fact list -> rule -> bool
(** [lifts_over_leftover group r] is whether lifting the declarations whose
    facts are [group] out of [r] moves one of them above an earlier declaration
    [r] keeps that it conflicts with. *)

val reorders_group : Declaration.declaration list -> rule -> bool
(** [reorders_group group r] is whether the order of [group] reverses two
    conflicting declarations of [r]. *)

val declarations_conflict_with_group_facts :
  Commute.fact list -> Declaration.declaration list -> bool
(** [declarations_conflict_with_group_facts group ds] is whether a declaration
    of [ds] conflicts with one of [group]. *)

val declarations_cross_order :
  Declaration.declaration list -> Declaration.declaration list -> bool
(** [declarations_cross_order grouped external] is whether a declaration of
    [grouped] and one of [external] conflict across a shorthand and a longhand
    of different properties. *)
