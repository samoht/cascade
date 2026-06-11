(** Suffix coverage table for same-selector shadowing. *)

type t
(** Properties written later, keyed by selector. *)

val v : unit -> t
(** [v ()] creates an empty coverage table. *)

val covered : t -> Selector.t -> Declaration.t -> bool
(** [covered t selector decl] is [true] when [decl]'s property is definitely
    shadowed by a later declaration for [selector] at the same or stronger
    importance. *)

val add : t -> Selector.t -> Declaration.t -> unit
(** [add t selector decl] records that [decl]'s property is written later for
    [selector]. *)
