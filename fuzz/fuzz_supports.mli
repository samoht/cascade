(** Fuzz tests for CSS supports query parsing and serialization. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the supports-query fuzz cases. *)
