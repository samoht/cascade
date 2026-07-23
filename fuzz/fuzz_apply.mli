(** Fuzz tests for projecting a stylesheet onto an element tree. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the [Cascade.Apply] fuzz cases. *)
