(** CSS tests: parse recovery, bad strings, keyword and identifier case, unknown
    at-rules. *)

val suite : string * unit Alcotest.test_case list
(** [suite] declares the test cases for this module. *)
