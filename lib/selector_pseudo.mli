(** Pseudo-class and pseudo-element classification: the identifier tables the
    reader and printer share, and the CSS Selectors 4 rules on what may follow a
    pseudo-element. *)

open Selector_intf

val scrollbar_state_ident : scrollbar_state -> string
(** [scrollbar_state_ident s] is the pseudo-class name of [s]. *)

val scrollbar_part_ident : scrollbar_part -> string
(** [scrollbar_part_ident p] is the pseudo-element name of [p]. *)

val is_pseudo_element : t -> bool
(** [is_pseudo_element s] is whether [s] is a pseudo-element. *)

val any : (t -> bool) -> t -> bool
(** [any p s] is whether [p] holds for [s] or any selector nested in it. *)

val has_pseudo_element : t -> bool
(** [has_pseudo_element s] is [any is_pseudo_element s]. *)

val has_unknown_pseudo_class : t -> bool
(** [has_unknown_pseudo_class s] is whether [s] holds a pseudo-class cascade
    does not model. *)

val matches_nothing : t -> bool
(** [matches_nothing s] is whether [s] holds an empty forgiving [:is()] or
    [:where()] that makes it match no element. *)

val is_pe_action : t -> bool
(** [is_pe_action s] is whether [s] is a pseudo-class, the only simple selector
    allowed after a pseudo-element (sec. 3.6.3). *)

val pseudo_element_allows : t -> t -> bool
(** [pseudo_element_allows pe pc] is whether the pseudo-class [pc] may follow
    the pseudo-element [pe]. *)

val bars_following_combinator : t -> bool
(** [bars_following_combinator s] is whether a compound [s] holds a
    pseudo-element that no combinator may follow (sec. 3.6.5). *)

val has_combinator_after_pseudo_element : t -> bool
(** [has_combinator_after_pseudo_element s] is whether [s] puts a combinator
    after a pseudo-element that bars one. *)

(** Why a compound may not carry a component after its pseudo-element. *)
type compound_refusal =
  | Sub_pseudo_element  (** sec. 3.6.4. *)
  | Simple_after_pseudo_element  (** sec. 3.6.3, a non-pseudo-class. *)
  | Pseudo_class_after_pseudo_element
      (** sec. 3.6.3, a pseudo-class the pseudo-element refuses. *)

val compound_refusal : seen:t list -> t -> compound_refusal option
(** [compound_refusal ~seen s] is why [s] may not follow [seen], the compound's
    earlier components most recent first, or [None] when it may. *)

val has_refused_simple_in_compound : t -> bool
(** [has_refused_simple_in_compound s] is whether a compound of [s] carries a
    component {!compound_refusal} refuses. *)

val pseudo_class_all_idents : unit -> (string * t) list
(** [pseudo_class_all_idents ()] is every name read after a single colon. *)

val pseudo_element_unknown_idents : (string * t) list Lazy.t
(** [pseudo_element_unknown_idents] is every name read after a double colon. *)
