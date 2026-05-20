(** Fuzz tests for [Color_space]. *)

val suite : string * Alcobar.test_case list
(** [suite] declares the colour-space fuzz cases (roundtrip identities, hue
    interpolation range). *)
