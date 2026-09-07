(** Test suite module. *)

val suite : string * unit Alcotest.test_case list
(** [suite] pins what {!Cascade.Support} answers from the generated web-features
    table. *)
