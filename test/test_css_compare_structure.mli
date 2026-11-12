(** Css_compare tests: canonical equality across rule order, nesting, layers,
    at-rule conditions and selector lists. *)

val suite : string * unit Alcotest.test_case list
(** [suite] declares the test cases for this module. *)
