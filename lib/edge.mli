(** Selector/property write edges. *)

type packed_property =
  | Packed : 'a Properties.property -> packed_property
      (** Existential wrapper for typed property tags. *)

type t = {
  summary : Selector_summary.t;
  property : packed_property;
  important : bool;
}
(** A selector-summary/property write edge. *)

val selectors : Selector.t -> Selector.t list
(** [selectors sel] returns the selector-list branches of [sel], or [sel] alone.
*)

val of_rule : Stylesheet.rule -> t list
(** [of_rule rule] enumerates property writes in [rule]. *)

val pp : t Pp.t
(** Pretty-printer for debugging. *)
