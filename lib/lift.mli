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

val exact_leftovers : Declaration.declaration list -> rule list -> rule list
(** [exact_leftovers common rules] is what each of [rules] keeps once [common]
    is lifted out of it, dropping a rule left empty. *)

val decl_mem : Declaration.declaration -> Declaration.declaration list -> bool
(** [decl_mem d ds] is whether [ds] holds [d]. *)

val unsafe_lift_in_any_member :
  Declaration.declaration list -> rule list -> bool
(** [unsafe_lift_in_any_member group rules] is whether lifting [group] out of
    some rule of [rules] moves a declaration of it above an earlier leftover of
    that rule it conflicts with. *)

val unsafe_group_order : Declaration.declaration list -> rule list -> bool
(** [unsafe_group_order group rules] is whether the order of [group] reverses
    two conflicting declarations of some rule of [rules]. *)

val declarations_conflict_with_group_facts :
  Commute.fact list -> Declaration.declaration list -> bool
(** [declarations_conflict_with_group_facts group ds] is whether a declaration
    of [ds] conflicts with one of [group]. *)

val unsafe_cross_member_leftover :
  Declaration.declaration list -> rule list -> bool
(** [unsafe_cross_member_leftover group rules] is whether a rule of [rules]
    keeps a leftover conflicting with [group] while its selector may match an
    element another rule of [rules] matches. *)

val unsafe_lift_over_prior_member :
  conflict:('id -> 'id -> bool) ->
  Declaration.declaration list ->
  ('id * rule) list ->
  bool
(** [unsafe_lift_over_prior_member ~conflict group members] is whether some
    member is ordered after an earlier one ([conflict]) whose leftover conflicts
    with [group], so lifting [group] above that leftover would change the
    cascade. *)

val declarations_cross_order :
  Declaration.declaration list -> Declaration.declaration list -> bool
(** [declarations_cross_order grouped external] is whether a declaration of
    [grouped] and one of [external] conflict across a shorthand and a longhand
    of different properties. *)
