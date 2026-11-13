(** Which cascade slots a declaration writes: the shorthand-to-longhand coverage
    relation and the overlap footprints built on it. *)

val is_intentionally_duplicated : Declaration.declaration -> bool
(** [is_intentionally_duplicated d] is [true] when [d] may deliberately coexist
    with same-property writes. *)

val covers_longhand : 'a Properties.property -> 'b Properties.property -> bool
(** [covers_longhand sh lh] is [true] when setting [sh] writes [lh], directly or
    through an intermediate shorthand. *)

val unwrap_theme_guard : Declaration.declaration -> Declaration.declaration
(** [unwrap_theme_guard d] is [d] with every theme guard removed. *)

val all_preserved_reorder_declaration : Declaration.declaration -> bool
(** [all_preserved_reorder_declaration d] is [true] when [d] writes [direction]
    or [unicode-bidi], which keep their position after an [all]. *)

val declaration_covers :
  Declaration.declaration -> Declaration.declaration -> bool
(** [declaration_covers newer older] is the shorthand/property coverage
    relation. *)

val declarations_overlap :
  Declaration.declaration -> Declaration.declaration -> bool
(** [declarations_overlap a b] is [true] when [a] and [b] may write a common
    cascade slot after shorthand expansion. *)

type overlap_key
(** Precomputed declaration-overlap key. *)

val overlap_key_equal : overlap_key -> overlap_key -> bool
(** [overlap_key_equal a b] is structural equality for overlap keys. *)

val overlap_key_hash : overlap_key -> int
(** [overlap_key_hash key] is consistent with {!val-overlap_key_equal}. *)

val overlap_key_compare : overlap_key -> overlap_key -> int
(** [overlap_key_compare a b] is a total order on overlap keys. *)

val broad_overlap_key : overlap_key
(** Key for a declaration that may overlap any other non-exempt declaration. *)

val overlap_keys_broad : overlap_key list -> bool
(** [overlap_keys_broad keys] is [true] when the sorted [keys] hold
    {!val-broad_overlap_key}. *)

val overlap_keys_intersect : overlap_key list -> overlap_key list -> bool
(** [overlap_keys_intersect a b] is [true] when two sorted key lists may touch a
    common cascade slot: they share a key, or either holds
    {!val-broad_overlap_key}. Both lists are sorted by
    {!val-overlap_key_compare}, as {!val-declaration_overlap_keys} returns them.
*)

val declaration_overlap_keys : Declaration.declaration -> overlap_key list
(** [declaration_overlap_keys d] is the conservative set of longhand footprint
    keys touched by [d], sorted by {!val-overlap_key_compare} without repeats.
*)

val declarations_overlap_with_keys :
  Declaration.declaration ->
  overlap_key list ->
  Declaration.declaration ->
  overlap_key list ->
  bool
(** [declarations_overlap_with_keys a a_keys b b_keys] is
    {!val-declarations_overlap} using precomputed footprints. *)

val declarations_conflict_with_keys :
  Declaration.declaration ->
  overlap_key list ->
  Declaration.declaration ->
  overlap_key list ->
  bool
(** [declarations_conflict_with_keys a a_keys b b_keys] is [true] when source
    order decides between [a] and [b] on an element both apply to: they are of
    one importance, they are not the same declaration, and they overlap by
    {!val-declarations_overlap_with_keys}. *)

val declaration_is_broad : Declaration.declaration -> bool
(** [declaration_is_broad d] is [true] when [d] is the [all] shorthand or a
    property outside the footprint model. *)

val custom_property_name : Declaration.declaration -> string option
(** [custom_property_name d] is the custom property [d] writes, read through a
    theme guard. *)
