(** Shared CSS property grammar vectors used by deterministic tests and fuzzers.
*)

type row = {
  property : string;
  positives : string list;
  negatives : string list;
}

val rows : row list
(** Spec-derived positive and negative vectors for every modeled property. *)

val property_names : string list
(** [property_names] is the unique modeled property names covered by rows. *)
