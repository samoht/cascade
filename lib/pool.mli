(** A mutable pool of CSS rules in cascade order, supporting near-constant
    merging and O(1) precedence queries.

    This is the working representation for the incremental rule merger (after
    Hague, Lin & Hong, "CSS Minification via Constraint Solving", TOPLAS 2019):

    - {b order} is kept by {!Order_maintenance}, so "does rule A come before
      rule B?" stays O(1) as rules are inserted and removed;
    - {b merge classes} are kept by a small in-tree union-find (path-compressed,
      union-by-rank, in [pool.ml]), so combining two rules is near-constant and
      a handle to an original rule still resolves to its current merged
      representative.

    {!Loop} drives the greedy, priority-ordered merging over this pool; its
    interface documents that design and the simpler batch-fixpoint alternative.

    A {!node} is a stable handle to one live rule. Combining or removing a node
    invalidates it; the surviving node keeps the merged rule. *)

type t
(** A mutable pool of rules. *)

type node
(** A stable handle to one rule in the pool. *)

val of_rules : Stylesheet.rule list -> t
(** [of_rules rs] builds a pool holding [rs] in order. *)

val to_rules : t -> Stylesheet.rule list
(** [to_rules t] returns the live rules in cascade order. *)

val nodes : t -> node list
(** [nodes t] are the live node handles in cascade order. *)

val rule : node -> Stylesheet.rule
(** [rule n] is [n]'s current rule (resolving through any merges). *)

val is_live : node -> bool
(** [is_live n] is [false] once [n] has been removed (e.g. merged away). *)

val id : node -> int
(** [id n] is a stable integer identity, unique in the pool and invariant under
    edits elsewhere, suitable for keying a priority queue of nodes. *)

val length : t -> int
(** [length t] is the number of live rules. *)

val before : node -> node -> bool
(** [before a b] is [true] when [a] precedes [b] in cascade order. O(1). *)

val next : node -> node option
(** [next n] is the rule immediately after [n] in cascade order. *)

val prev : node -> node option
(** [prev n] is the rule immediately before [n] in cascade order. *)

val set : node -> Stylesheet.rule -> unit
(** [set n r] replaces [n]'s rule in place (e.g. a rule whose declarations
    shrank after factoring). *)

val combine :
  t ->
  node ->
  node ->
  (Stylesheet.rule -> Stylesheet.rule -> Stylesheet.rule) ->
  node
(** [combine t a b f] merges [b] into [a]: the union-find classes are unioned,
    the surviving rule is [f (rule a) (rule b)] kept at [a]'s position, and [b]
    is removed from the order. Returns the surviving node ([a]'s identity). [a]
    must precede [b]. *)

val insert_after : t -> node -> Stylesheet.rule -> node
(** [insert_after t n r] inserts a fresh rule [r] just after [n] (e.g. a shared
    factored rule) and returns its handle. *)

val insert_before : t -> node -> Stylesheet.rule -> node
(** [insert_before t n r] inserts a fresh rule [r] just before [n] (e.g. a
    shared factored rule placed ahead of the run it was hoisted from). *)

val remove : t -> node -> unit
(** [remove t n] deletes [n] from the pool. *)
