(** The WebKit compatibility fallbacks: which standard property has a prefixed
    spelling, what the prefixed declaration for a value is, and whether a set of
    targets still needs one.

    This sits apart from {!Optimize} because two passes have to agree about it.
    The prefix synthesis reads it to write a declaration, and the rule factoring
    reads it to refuse a rewrite that would separate the two halves of a pair it
    wrote. A second table spelling the same relation is how they came to
    disagree. *)

(** The families with a prefixed spelling. Sealed: a property that grows one
    stops every site that decides about the set from compiling. *)
type t =
  | User_select_fallback
  | Backdrop_filter_fallback
  | Hyphens_fallback
  | Text_decoration_color_fallback
  | Mask_fallback
  | Mask_image_fallback
  | Mask_position_fallback
  | Mask_size_fallback
  | Mask_repeat_fallback
  | Mask_clip_fallback
  | Mask_origin_fallback

val pp : t Pp.t
(** [pp] writes the family's standard property name, which is what names it in a
    log line or a test failure. *)

val kind_of : Declaration.declaration -> t option
(** [kind_of decl] is the family [decl] heads, or [None] where its property has
    no prefixed spelling. Asked of the standard spelling. *)

val is_prefixed : t -> Declaration.declaration -> bool
(** [is_prefixed kind decl] is whether [decl] is the prefixed spelling of
    [kind], whatever value it carries. *)

val of_declaration :
  Support.targets -> Declaration.declaration -> Declaration.declaration option
(** [of_declaration targets decl] is the prefixed declaration [decl] needs for
    [targets], or [None] where the targets read the standard spelling, the value
    has no prefixed spelling, or [decl] heads no family. *)

val holds_a_prefixed_spelling : Declaration.declaration list -> bool
(** [holds_a_prefixed_spelling decls] is whether any of [decls] is spelled with
    a [-webkit-] prefix. Every prefixed half of a pair is, so a list this
    refuses holds no pair: it is the cheap filter in front of {!is_pair}, not
    the relation itself. *)

val is_pair : Declaration.declaration -> Declaration.declaration -> bool
(** [is_pair a b] is whether the two are the two halves of one fallback pair,
    carrying the same value and importance, in either order. It takes no
    targets: by the time a caller asks, the synthesis has already decided the
    targets needed the pair, and separating the halves is what breaks it. *)
