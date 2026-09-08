(** Test suite module. *)

val suite : string * unit Alcotest.test_case list
(** [suite] pins the compatibility-prefix pairs through the emission, which is
    where the relation is observable: the module itself is an optimizer
    implementation detail. *)
