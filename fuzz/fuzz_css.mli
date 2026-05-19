(** Fuzz tests for top-level CSS parsing, mapping, and serialization. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the CSS fuzz cases. *)
