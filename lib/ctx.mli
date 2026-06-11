(** Optimizer context. *)

type scope = [ `Fragment | `Stylesheet ]
(** Surrounding CSS context assumed by optimizations. *)

type t
(** Shared optimizer context. *)

val fragment : t
(** Default fragment context. *)

val of_scope :
  ?lossless:bool -> ?aggressive:bool -> ?extend_lists:bool -> scope option -> t
(** Build a context from an optional scope. *)

val v :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?extend_lists:bool ->
  ?registered:(string -> bool) ->
  scope ->
  t
(** Build a context explicitly. *)

val scope : t -> scope
(** Scope assumed by optimizations. *)

val registered : t -> string -> bool
(** Whether a custom property name is registered. *)

val lossless : t -> bool
(** Whether lossless value optimization is enabled. *)

val aggressive : t -> bool
(** Whether expensive optimization passes (notably the global factoring
    fixpoint) run regardless of the preflight's byte-gain estimate. *)

val extend_lists : t -> bool
(** Whether the body-keyed across-gap combine ({!Merge.identical_global}) is
    allowed to absorb candidates into an existing {!Selector.List} rule or treat
    such a rule as a multi-subselector candidate. Off by default; the A/B
    optimizer in {!Optimize.stylesheet} flips it for one of two runs and emits
    whichever stylesheet serializes shorter. *)

val pp : t Pp.t
(** Pretty-printer for debugging. *)
