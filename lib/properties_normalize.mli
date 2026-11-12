(** Typed property-value normalisation, including the minify swap of [initial]
    for a shorter initial value. *)

open Properties_intf

val normalize_property_value :
  ?lossless:bool ->
  ?exact_srgb:bool ->
  ?resolve_missing:bool ->
  ?ctx:Values.calc_ctx ->
  'a property ->
  'a ->
  'a
(** [normalize_property_value ?lossless prop value] applies semantic
    (equivalence) canonicalisation to [value] so the optimizer holds a canonical
    AST and the pretty-printer stays a pure serialiser. Identity for properties
    whose folds have not yet migrated out of [pp]. [lossless] disables bounded
    colour and numeric approximation while keeping exact canonicalisation.

    [exact_srgb] and [resolve_missing] are {!Values.normalize_color}'s flags of
    the same names, applied to the properties whose whole value is a colour.
    Both are for the canonical diff projection only. A colour a value nests - a
    gradient stop, a shadow colour - normalises without either, so
    [resolve_missing] never reaches a colour the value interpolates. *)
