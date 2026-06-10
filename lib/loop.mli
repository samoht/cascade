(** Best-first rewrite loop over a mutable pool. *)

type action
(** A rewrite action scored for one anchor node. *)

type score = Pool.node -> action option
(** [score node] returns the current best action rooted at [node], if any. *)

type t
(** A queued rewrite loop. *)

val action :
  replacement:Stylesheet.rule list ->
  consumed:Pool.node list ->
  saving:int ->
  action
(** [action ~replacement ~consumed ~saving] rewrites an anchor by inserting
    [replacement] before it, then removing the anchor and [consumed] nodes. *)

val v : Pool.t -> score -> t
(** [v pool score] builds a loop over the live nodes of [pool]. *)

val run : ?on_apply:(int -> unit) -> t -> int
(** [run t] drains the queue, applying still-live positive actions and
    re-scoring stale anchors. [on_apply saving] is called for every applied
    action. The result is the number of applied actions. *)
