(** Values shared by the Css_compare test suites. *)

val color_mix_transparent : string
(** [color_mix_transparent] is a [color-mix()] towards [transparent]. *)

val color_mix_transparent_hex : string
(** [color_mix_transparent_hex] is {!color_mix_transparent} with [transparent]
    spelt [#0000]. *)

val equal :
  ?mode:Cascade_diff.Css_compare.mode ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?prune_unused_custom_props:bool ->
  string ->
  string ->
  bool
(** [equal] is {!Cascade_diff.Css_compare.equal}. *)
