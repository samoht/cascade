(** The report a browser comparison writes for a reader, checked over reports
    built by hand. *)

val suite : string * unit Alcotest.test_case list
(** Tests of the report a browser comparison writes for a reader. *)
