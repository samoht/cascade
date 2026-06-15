(** Best-first rewrite loop over a mutable rule pool: the incremental engine the
    minifier uses to merge rules.

    {1 Design: incremental, priority-driven, order-preserving}

    Rules are merged greedily, largest byte-saving first, while keeping cascade
    order intact. Two structures cooperate with this loop:

    - {!Pool} holds the rules in cascade order (via {!Order_maintenance}, so "is
      A before B?" is O(1)) with merge classes in a union-find (so a handle to
      an original rule still resolves to its merged representative);
    - a priority-search queue is the {b frontier} -- the next merge to try,
      keyed by saving, with O(log n) removal of a candidate a prior merge
      invalidated.

    Applying a merge re-scores only the handful of anchors it touched and pushes
    them back on the frontier; the loop never re-scans the whole stylesheet.

    {1 The simpler alternative}

    A {b batch fixpoint} -- re-scan every rule each pass until a pass changes
    nothing -- needs none of these structures and is materially less code. Its
    cost is throughput: each pass is O(rules) and, on a chain that merges
    pairwise, it tends towards quadratic overall. This loop spends the extra
    machinery to buy that throughput back.

    {1 Scaling}

    Draining a chain of [n] adjacent merges is O(n log n): each doubling of [n]
    costs ~2.2x work, well under the 4x a quadratic shows. The "scales
    sub-quadratically" case in [test/test_loop.ml] measures it on allocation (a
    deterministic proxy for work) and asserts the ratio. *)

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
