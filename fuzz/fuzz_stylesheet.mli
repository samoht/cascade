(** Fuzz tests for stylesheet parsing, recovery, and serialization. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the stylesheet fuzz cases. *)
