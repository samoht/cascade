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
(** Combine cascade-safe rules with identical declaration blocks. *)

val declarations_equal :
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Declaration.declaration list ->
  Declaration.declaration list ->
  bool
(** Compare declaration lists with physical-identity fast path. *)

val compatible : Selector.t -> Selector.t -> bool
(** Whether two selector-list branches can safely share one selector list. *)
