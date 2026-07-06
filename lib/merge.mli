(** Rule merge helpers. *)

val pseudo : Selector.t -> Selector.t option
(** Extract a selector's final pseudo-element, if any. *)

val vendor : Selector.t -> bool
(** Whether a selector contains a vendor-specific pseudo-element. *)

val key : Selector.t -> Selector.t list
(** Canonical key for same-target selector comparison. *)

val selector_list : Selector.t list -> Selector.t
(** Build a flat selector list, preserving a singleton selector. *)

val declarations_equal :
  same:(Declaration.declaration -> Declaration.declaration -> bool) ->
  Declaration.declaration list ->
  Declaration.declaration list ->
  bool
(** Compare declaration lists with physical-identity fast path. *)

val compatible : Selector.t -> Selector.t -> bool
(** Whether two selector-list branches can safely share one selector list. *)
