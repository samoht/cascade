(** Css_compare tests: canonical and semantic equality of values: colours,
    numbers, units, whitespace and custom property tokens. *)

val suite : string * unit Alcotest.test_case list
(** [suite] declares the test cases for this module. *)
