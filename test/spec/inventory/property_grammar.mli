(** Shared CSS property grammar vectors used by deterministic tests and fuzzers.
*)

type row = {
  property : string;
  positives : string list;
  negatives : string list;
}

val rows : row list
(** Spec-derived positive and negative vectors for every modeled property.

    A negative is invalid as a complete value. It may still be a valid
    component: in a [&&] or [||] grammar, [hanging] is no [text-indent] on its
    own and a perfectly good one after a length. Generators must not assume that
    appending a negative to a positive yields an invalid declaration. *)

val property_names : string list
(** [property_names] is the unique modeled property names covered by rows. *)
