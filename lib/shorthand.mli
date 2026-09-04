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
    [border-color] overlap because both write [border-top-color]. A
    flow-relative property overlaps every physical property of its family, since
    the writing mode of the elements the stylesheet will match decides which
    physical side it resolves to. *)

type overlap_key
(** Precomputed declaration-overlap key used by the rule graph. *)

val overlap_key_equal : overlap_key -> overlap_key -> bool
(** [overlap_key_equal a b] is structural equality for overlap keys. *)

val overlap_key_hash : overlap_key -> int
(** [overlap_key_hash key] is consistent with {!val-overlap_key_equal}. *)

val overlap_key_compare : overlap_key -> overlap_key -> int
(** [overlap_key_compare a b] is a total order on overlap keys, consistent with
    {!val-overlap_key_equal}. Sorting a footprint by it lets a caller merge-walk
    two footprints instead of scanning one per element of the other. *)

val broad_overlap_key : overlap_key
(** Key for a declaration that may overlap any other non-exempt declaration: a
    reset such as [all], or a property name the footprint model cannot place. *)

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

val declaration_is_broad : Declaration.declaration -> bool
(** [declaration_is_broad d] is [true] when [d] may write a cascade slot any
    other declaration writes, so comparing footprints cannot tell the two apart:
    the [all] shorthand, and a property outside the model, whose name does not
    spell out what it expands to. *)

val custom_property_name : Declaration.declaration -> string option
(** [custom_property_name d] is the custom property [d] writes, read through a
    theme guard. Such a declaration overlaps the declarations writing that same
    name and nothing else, so a caller indexing footprints files it under the
    name rather than under {!val-declaration_overlap_keys}. *)

val same_property : Declaration.declaration -> Declaration.declaration -> bool
(** Same CSS property. *)

val same_value : Declaration.declaration -> Declaration.declaration -> bool
(** Same declaration value ignoring importance. *)

val declarations_commute :
  Declaration.declaration list -> Declaration.declaration list -> bool
(** [declarations_commute a b] is [true] when running [a] before [b] and [b]
    before [a] compute the same value for every property on every element: no
    pair across the two writes a common cascade slot at the same importance with
    a different value. Selectors are not read, so two runs that could never meet
    on one element still count as constrained when their properties clash. *)

val drop_redundant_decoration_color_aliases :
  Declaration.declaration list -> Declaration.declaration list
(** Drop an identical WebKit [text-decoration-color] compatibility alias when
    its unprefixed twin is present. Differing values or importance, and a
    prefixed-only declaration, are kept. *)

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

type held
(** Shorthand slots a set of declarations leaves holding something other than
    the slot initial. Composing a run that leaves a slot unwritten resets it, so
    a run only contracts when nothing that reaches the same element holds it.
    Composition reads one rule, so holders in the rest of a rule run reach it
    only through this summary. Carries the [transition] slots; a family that
    contracts a partial run adds its own. *)

val held_none : held
(** Nothing held. *)

val held_add : held -> Declaration.declaration list -> held
(** [held_add held decls] extends [held] with the slots [decls] hold. *)

val compose_shorthands :
  ?held:held ->
  ctx:Ctx.t ->
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** Compose supported longhand runs into shorthand declarations. [held] is what
    the rest of the rule run holds; slots the given declarations hold themselves
    are judged in place and drop out of it. *)

val deduplicate_declarations_with :
  ?held:held ->
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
