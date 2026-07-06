(** Declaration deduplication and shorthand composition. *)

val duplicate_buggy_properties :
  Declaration.declaration list -> Declaration.declaration list
(** Duplicate compatibility-sensitive properties. *)

val is_intentionally_duplicated : Declaration.declaration -> bool
(** Whether a declaration may deliberately coexist with same-property writes. *)

val declaration_covers :
  Declaration.declaration -> Declaration.declaration -> bool
(** [declaration_covers newer older] is the shorthand/property coverage
    relation. *)

val declarations_overlap :
  Declaration.declaration -> Declaration.declaration -> bool
(** [declarations_overlap a b] is [true] when [a] and [b] may write at least one
    common cascade slot after shorthand expansion. Unlike
    {!val-declaration_covers}, this relation is symmetric: [border-top] and
    [border-color] overlap because both write [border-top-color]. *)

type overlap_key
(** Precomputed declaration-overlap key used by the rule graph. *)

val overlap_key_equal : overlap_key -> overlap_key -> bool
(** [overlap_key_equal a b] is structural equality for overlap keys. *)

val overlap_key_hash : overlap_key -> int
(** [overlap_key_hash key] is consistent with {!val-overlap_key_equal}. *)

val broad_overlap_key : overlap_key
(** Key for broad reset-like declarations, such as [all], that may overlap any
    other non-exempt declaration. *)

val overlap_keys_intersect : overlap_key list -> overlap_key list -> bool
(** [overlap_keys_intersect a b] is [true] when two precomputed declaration
    footprints may touch a common cascade slot. *)

val declaration_overlap_keys : Declaration.declaration -> overlap_key list
(** [declaration_overlap_keys d] is the conservative set of longhand footprint
    keys touched by [d], for indexing potential declaration overlaps.
    {!val-broad_overlap_key} means [d] is broad and must be checked against
    every non-exempt property. *)

val declarations_overlap_with_keys :
  Declaration.declaration ->
  overlap_key list ->
  Declaration.declaration ->
  overlap_key list ->
  bool
(** [declarations_overlap_with_keys a a_keys b b_keys] is
    {!val-declarations_overlap} using precomputed declaration footprints. *)

val same_property : Declaration.declaration -> Declaration.declaration -> bool
(** Same CSS property. *)

val same_value : Declaration.declaration -> Declaration.declaration -> bool
(** Same declaration value ignoring importance. *)

val same_minified_declaration :
  Declaration.declaration -> Declaration.declaration -> bool
(** Same canonical minified declaration. *)

val is_all_declaration : Declaration.declaration -> bool
(** Whether a declaration is the [all] shorthand. *)

val merge_box_shorthand_longhands :
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** Fold later box longhands into preceding box shorthands. *)

val merge_overflow_longhands :
  (int * Declaration.declaration) list -> (int * Declaration.declaration) list
(** Fold [overflow-x]/[overflow-y] into [overflow]. *)

val compose_shorthands :
  ctx:Ctx.t ->
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** Compose supported longhand runs into shorthand declarations. *)

val deduplicate_declarations_with :
  ctx:Ctx.t ->
  ?merge_box:bool ->
  Declaration.declaration list ->
  Declaration.declaration list
(** Deduplicate and optionally compose declarations under [ctx]. *)

val deduplicate_declarations :
  ?scope:Ctx.scope ->
  Declaration.declaration list ->
  Declaration.declaration list
(** Deduplicate declarations under an optional scope. *)
