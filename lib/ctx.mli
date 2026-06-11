(** Optimizer context. *)

type scope = [ `Fragment | `Stylesheet ]
(** Surrounding CSS context assumed by optimizations. *)

type t
(** Shared optimizer context. *)

val fragment : t
(** Default fragment context. *)

val of_scope : ?lossless:bool -> scope option -> t
(** Build a context from an optional scope. *)

val v : ?lossless:bool -> ?registered:(string -> bool) -> scope -> t
(** Build a context explicitly. *)

val scope : t -> scope
(** Scope assumed by optimizations. *)

val registered : t -> string -> bool
(** Whether a custom property name is registered. *)

val lossless : t -> bool
(** Whether lossless value optimization is enabled. *)

val pp : t Pp.t
(** Pretty-printer for debugging. *)
