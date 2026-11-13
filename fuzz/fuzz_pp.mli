(** Fuzz tests for [Pp]'s number printer. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the number-printer fuzz cases (the rounding rule
    [Pp.string_of_float] states). *)
