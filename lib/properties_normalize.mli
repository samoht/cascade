(** Typed property-value normalisation, including the minify swap of [initial]
    for a shorter initial value. *)

open Properties_intf

val normalize_property_value :
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?ctx:Values.calc_ctx ->
  'a property ->
  'a ->
  'a
(** [normalize_property_value ?lossless ?enforce_spec prop value] applies
    semantic (equivalence) canonicalisation to [value] so the optimizer holds a
    canonical AST and the pretty-printer stays a pure serialiser. Identity for
    properties whose folds have not yet migrated out of [pp]. [lossless]
    disables bounded colour and numeric approximation while keeping exact
    canonicalisation. *)
