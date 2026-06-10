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
