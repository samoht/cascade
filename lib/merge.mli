(** Rule merge helpers. *)

val pseudo : Selector.t -> Selector.t option
(** Extract a selector's final pseudo-element, if any. *)

val vendor : Selector.t -> bool
(** Whether a selector contains a vendor-specific pseudo-element. *)

val key : Selector.t -> Selector.t list
(** Canonical key for same-target selector comparison. *)

val selector_list : Selector.t list -> Selector.t
(** Build a flat selector list, preserving a singleton selector. *)

val pair : Stylesheet.rule -> Stylesheet.rule -> Stylesheet.rule
(** Merge two adjacent same-selector rules without reordering declarations. *)

val adjacent : Stylesheet.rule list -> Stylesheet.rule list
(** Merge adjacent rules with the same selector. *)

val identical :
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** Combine cascade-safe rules with identical declaration blocks. The walker
    closes its current group as soon as one intermediate rule writes a group
    property on a selector overlapping the group's head. *)

val identical_global :
  ?extend_lists:bool ->
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** [identical_global ?extend_lists ~same rules] is the body-keyed global
    analogue of {!identical}: it buckets every eligible rule by its body, then
    greedily absorbs every later occurrence into the earliest as long as the
    gap is cascade-safe against the actual candidate's selector (not the
    head's). Each absorption is sound iff every intermediate rule outside the
    merge group that writes one of the body's properties has a selector that
    does not overlap the union of already-accepted member selectors and the
    candidate's selector.

    When [extend_lists] is [true] (default [false]), {!Selector.List} rules
    become eligible too, so the pass can extend an existing
    [.a,.b\{body\}] rule with later [.c\{body\}] rules. Each commit is locally
    sound but interacts greedily with downstream factoring; the optimizer
    runs both settings A/B and emits whichever serializes shorter. *)

val declarations_equal :
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Declaration.declaration list ->
  Declaration.declaration list ->
  bool
(** Compare declaration lists with physical-identity fast path. *)

val compatible : Selector.t -> Selector.t -> bool
(** Whether two selector-list branches can safely share one selector list. *)
