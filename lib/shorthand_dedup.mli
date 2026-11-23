(** Declaration deduplication: which earlier declaration a later one makes dead,
    the vendor-prefixed and legacy fallbacks that survive, the vendor aliases
    dropped once the unprefixed twin is widely available, and the longhands a
    preceding shorthand already sets. *)

val same_value : Declaration.declaration -> Declaration.declaration -> bool
(** [same_value a b] is value equality ignoring importance. *)

val declarations_commute :
  Declaration.declaration list -> Declaration.declaration list -> bool
(** [declarations_commute a b] is [true] when no pair across [a] and [b] writes
    a common cascade slot at the same importance with a different value. *)

val is_all_declaration : Declaration.declaration -> bool
(** [is_all_declaration d] is [true] when [d] is the [all] shorthand. *)

val deduplicate_step :
  floors:Support.targets ->
  judged:bool ->
  ('a * Declaration.declaration) list ->
  'a * Declaration.declaration ->
  ('a * Declaration.declaration) list
(** [deduplicate_step ~floors ~judged kept d] adds [d] to the reversed list
    [kept], dropping the declarations of [kept] that [d] makes dead, or drops
    [d] when an important declaration of [kept] already covers it. A declaration
    [d] covers is dead only when every browser at or above [floors] that reads
    it also reads [d] ({!Declaration.fallback_for}). A run that does not judge
    for browsers ([judged] false) also keeps one before a CSS Color 4 colour or
    a runtime substitution. *)

val read_by_no_target : Declaration.declaration -> bool
(** [read_by_no_target decl] is whether no browser a {!Support.targets} names
    reads [decl]'s property: one with the [-ms-], [-o-] or [-khtml-] prefix,
    [font-smooth] or [font-smoothing]. *)

val drop_redundant_decoration_color_aliases :
  Declaration.declaration list -> Declaration.declaration list
(** [drop_redundant_decoration_color_aliases decls] drops an identical WebKit
    [text-decoration-color] alias when its unprefixed twin is present. *)

val as_aliased_property : Declaration.declaration -> Declaration.declaration
(** [as_aliased_property d] is [d] under the property it is a legacy name alias
    of, or [d] itself. See {!Shorthand.as_aliased_property}. *)

val drop_vendor_aliases :
  ctx:Ctx.t ->
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** [drop_vendor_aliases ~ctx decls] drops a vendor-prefixed declaration whose
    unprefixed twin carries the same value and importance and is widely
    available. Under {!Ctx.enforce_spec} every declaration is kept. *)

val implied_longhands :
  enforce_spec:bool ->
  Declaration.declaration ->
  Declaration.declaration list option
(** [implied_longhands ~enforce_spec sh] is every longhand the shorthand [sh]
    writes, each with the value [sh] assigns it, the initial value for a slot
    [sh] omits and reset-only ones included, at the importance of [sh]. It reads
    [border], a single-layer [background], [flex] and a single [transition]:
    [border:1px solid] gives [border-width:1px], [border-style:solid],
    [border-color:currentcolor] and [border-image:none]. The basis [flex:1]
    omits is [0%], what Chrome computes, and [0] under [enforce_spec], what CSS
    Flexible Box 1 (ED) sec. 7.1 specifies. It is [None] for any other shorthand
    and wherever a value cannot be proven: an omitted slot of a shorthand
    holding a [var()]. *)

val declarations_conflict_with_keys :
  Declaration.declaration ->
  Shorthand_coverage.overlap_key list ->
  Declaration.declaration ->
  Shorthand_coverage.overlap_key list ->
  bool
(** [declarations_conflict_with_keys a a_keys b b_keys] is
    {!Shorthand_coverage.declarations_conflict_with_keys} less the pairs where
    one covers the other and sets every slot the other writes to the value the
    other writes there. *)

val drop_longhands_after_covering_shorthand :
  enforce_spec:bool ->
  Declaration.declaration list ->
  Declaration.declaration list
(** [drop_longhands_after_covering_shorthand ~enforce_spec decls] drops a
    longhand that writes the value a preceding covering shorthand of the same
    importance already assigns it, an omitted slot read as {!implied_longhands}
    reads it. *)
