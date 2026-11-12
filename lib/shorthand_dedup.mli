(** Declaration deduplication: which earlier declaration a later one makes dead,
    the vendor-prefixed and legacy fallbacks that survive, the vendor aliases
    dropped once the unprefixed twin is widely available, and the longhands a
    preceding shorthand already sets. *)

val same_property : Declaration.declaration -> Declaration.declaration -> bool
(** [same_property a b] is [true] when [a] and [b] write the same property. *)

val same_value : Declaration.declaration -> Declaration.declaration -> bool
(** [same_value a b] is value equality ignoring importance. *)

val declarations_commute :
  Declaration.declaration list -> Declaration.declaration list -> bool
(** [declarations_commute a b] is [true] when no pair across [a] and [b] writes
    a common cascade slot at the same importance with a different value. *)

val is_all_declaration : Declaration.declaration -> bool
(** [is_all_declaration d] is [true] when [d] is the [all] shorthand. *)

val deduplicate_step :
  ?targets:Support.targets ->
  ('a * Declaration.declaration) list ->
  'a * Declaration.declaration ->
  ('a * Declaration.declaration) list
(** [deduplicate_step ?targets kept d] adds [d] to the reversed list [kept],
    dropping the declarations of [kept] that [d] makes dead under [targets], or
    drops [d] when an important declaration of [kept] already covers it. *)

val drop_redundant_decoration_color_aliases :
  Declaration.declaration list -> Declaration.declaration list
(** [drop_redundant_decoration_color_aliases decls] drops an identical WebKit
    [text-decoration-color] alias when its unprefixed twin is present. *)

val drop_vendor_aliases :
  ctx:Ctx.t ->
  (int * Declaration.declaration) list ->
  (int * Declaration.declaration) list
(** [drop_vendor_aliases ~ctx decls] drops a vendor-prefixed declaration whose
    unprefixed twin carries the same value and importance and is widely
    available. Under {!Ctx.enforce_spec} every declaration is kept. *)

val drop_longhands_after_covering_shorthand :
  Declaration.declaration list -> Declaration.declaration list
(** [drop_longhands_after_covering_shorthand decls] drops a longhand that writes
    the value a preceding covering shorthand of the same importance already
    assigns it. *)
