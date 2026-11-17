(** Selector-list branches. *)

val selectors : Selector.t -> Selector.t list
(** [selectors sel] returns the selector-list branches of [sel], or [sel] alone.
*)
