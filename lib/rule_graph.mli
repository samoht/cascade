(** Cascade-dependency graph over a run of flat style rules.

    Models the cascade as the CSS-graph dependency of Hague, Lin and Hong ("CSS
    Minification via Constraint Solving", TOPLAS 2019): two rules are
    order-constrained ("conflict") only when source order can break the cascade
    tie - some element can match equal-specificity selector branches {e and}
    both rules write a shared property at equal importance
    (shorthand/longhand-aware). Disjoint selectors/properties, differing
    importance, or strictly-ordered specificity means their relative order does
    not affect the cascade within this fixed origin/layer/scope context.

    The graph holds the rules as numbered nodes plus this pairwise relation. It
    is the order-independent substrate the factor/merge engine reasons over, so
    that rule {e ordering} can be a single canonical projection applied at the
    end rather than a fact the optimization depends on. *)

type t
(** A run of rules with their pairwise cascade-dependency relation. *)

(** Stable node identifier within one graph generation. *)
module Node_id : sig
  type t = private int

  val compare : t -> t -> int
  (** Total order over node ids, suitable for maps, sets, and deterministic
      candidate ordering. *)

  val of_int_exn : int -> t
  (** [of_int_exn i] converts a non-negative raw node number into a typed id.
      Raises [Invalid_argument] for negative inputs. *)

  val to_int : t -> int
  (** [to_int id] returns the raw node number for diagnostics and tests. *)
end

type node_id = Node_id.t

(** Why one node must precede another in the graph. [Cascade_conflict] is a real
    order-sensitive cascade dependency. [Shared_branch_pin] is a conservative
    structural pin used to keep produced selector residuals contiguous. *)
type edge_reason = Cascade_conflict | Shared_branch_pin

(** Why a graph rewrite was rejected. *)
type rewrite_error =
  | Empty_consume
  | Empty_produce
  | Invalid_node of node_id
  | Duplicate_node of node_id
  | Stale_node of node_id
  | Ambiguous_external_order of { produced : node_id; external_ : node_id }
  | New_external_conflict of { produced : node_id; external_ : node_id }
  | Ambiguous_produced_order of { left : node_id; right : node_id }
  | New_produced_conflict of { left : node_id; right : node_id }
  | Cycle

val of_rules :
  ?parent:Selector.t -> ?closed_world:bool -> Stylesheet.rule list -> t
(** [of_rules ?parent rules] builds the graph over [rules] (node [i] is the
    [i]th rule). [parent], when set, is the enclosing nesting selector: each
    rule's relative selector is expanded against it so overlap is computed on
    the effective (parent-qualified) selector rather than the raw nested form.
    [closed_world] (default [false]) makes distinct selectors assumed not to
    match a common element, so they never cascade-conflict on selector grounds.
*)

val node_count : t -> int
(** Number of nodes. *)

val node_rule : t -> node_id -> Stylesheet.rule
(** [node_rule t i] is the rule at node [i]. *)

val node_size : t -> node_id -> int
(** [node_size t i] is the cached minified byte size of node [i]'s rule. *)

val node_origin : t -> node_id -> int
(** [node_origin t i] is source-position metadata for local scheduling. Authored
    nodes use their original position. Produced grouped nodes use the earliest
    matching consumed selector branch, while single-source residuals keep their
    original consumed node's position. The canonical projection uses this value
    as its stable tie-break key, so unconstrained nodes keep first-appearance
    source order. *)

val is_live : t -> node_id -> bool
(** [is_live t i] is whether node [i] is still present (not consumed by a
    rewrite). *)

val generation : t -> int
(** [generation t] increments after every successful rewrite. Candidate queues
    can use it to discard stale work. *)

val live_nodes : t -> node_id list
(** [live_nodes t] is the live node ids in creation order. *)

val declaration_body_key : t -> node_id -> int list
(** [declaration_body_key t i] is a hash key for bucketing nodes with identical
    declaration bodies. Hash collisions must be checked by callers. *)

val conflict : t -> node_id -> node_id -> bool
(** [conflict t i j] is [true] when nodes [i] and [j] are order-constrained:
    equal-specificity selector branches may match a common element and the rules
    write a shared equal-importance property (or share a selector branch).
    Symmetric. *)

val order_constrained : t -> node_id -> node_id -> bool
(** [order_constrained] is a clearer alias for {!val-conflict}. *)

val precedes : t -> node_id -> node_id -> bool
(** [precedes t i j] is [true] when the graph has a direct edge requiring node
    [i] to be emitted before node [j]. This is for local candidate ordering; use
    {!val-canonical_order} when a full output projection is needed. *)

val canonical_order_by : t -> (node_id -> int) -> node_id array
(** [canonical_order_by t rank] is the live node indices in a linear extension
    of the dependency DAG, choosing among the nodes currently free to come next
    the one with the smallest [rank] (then node id). {!val-canonical_order}
    passes source position for a minimal-disruption order; the canonical
    comparison passes a content-derived rank so two source orderings of
    cascade-independent rules converge to one form. *)

val canonical_order : t -> node_id array
(** [canonical_order t] is the live node indices in a canonical linear extension
    of the dependency DAG (an edge from the earlier to the later node of every
    conflicting pair). Among the nodes currently free to come next, Kahn's
    algorithm takes the smallest first-appearance source key, then the node id.
    Every constrained pair keeps its required order; unconstrained pairs are
    projected deterministically without inventing a lexicographic CSS order. *)

val canonical_linearization : t -> node_id array
(** [canonical_linearization] is a clearer alias for {!val-canonical_order}. *)

val canonicalize : t -> t
(** [canonicalize t] rebuilds [t] from its canonical rule projection, preserving
    the nesting parent. *)

val to_rules : t -> Stylesheet.rule list
(** [to_rules t] is the live rules in {!val-canonical_order} order. *)

val to_canonical_rules : t -> Stylesheet.rule list
(** [to_canonical_rules] is a clearer alias for {!val-to_rules}. *)

val rewrite :
  t ->
  consume:node_id list ->
  produce:Stylesheet.rule list ->
  (t, rewrite_error) result
(** [rewrite] is {!val-try_rewrite} with rejection diagnostics. *)

val try_rewrite :
  t -> consume:node_id list -> produce:Stylesheet.rule list -> t option
(** [try_rewrite t ~consume ~produce] replaces the live nodes [consume] with new
    nodes for [produce], as one transaction. Produced nodes inherit each
    consumed node's orientation toward any external node they still conflict
    with, so the partial order is preserved; produced nodes are mutually ordered
    by their position in [produce]. Returns [None] if a consumed node is already
    dead/stale, if [produce] is empty, or if the result would have a cycle (the
    rewrite would not preserve the cascade). On success {!val-generation} is
    bumped so candidates captured against the old graph can be rejected as
    stale. *)
