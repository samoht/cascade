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

(** Why a graph rewrite was rejected. A produced rule is ordered against a rule
    it conflicts with as the sources it draws on that conflict with that rule
    are, by where they print: an ambiguous order has them on both sides of it,
    and a new conflict has none. Neither reads a rule a rewrite consumed. *)
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
  ?parent:Selector.t ->
  ?closed_world:bool ->
  ?pin_shared_branches:bool ->
  ?exclusive:(int -> int -> bool) ->
  Stylesheet.rule list ->
  t
(** [of_rules ?parent rules] builds the graph over [rules] (node [i] is the
    [i]th rule). [parent], when set, is the enclosing nesting selector: each
    rule's relative selector is expanded against it so overlap is computed on
    the effective (parent-qualified) selector rather than the raw nested form.
    [closed_world] (default [false]) makes distinct selectors assumed not to
    match a common element, so they never cascade-conflict on selector grounds.
    [pin_shared_branches] (default [true]) adds conservative structural edges
    between rules sharing a selector branch. Optimizer rewrites need those edges
    to keep produced residuals contiguous; a consumer projecting rules that have
    already been expanded may disable them. [exclusive i j], when given, asserts
    that rules [i] and [j] never apply to one element together, as two rules
    under a condition and its negation do, so no cascade conflict is recorded
    between them; it does not reach the nodes a rewrite produces. *)

val node_count : t -> int
(** Number of nodes. *)

val node_rule : t -> node_id -> Stylesheet.rule
(** [node_rule t i] is the rule at node [i]. *)

val node_size : t -> node_id -> int
(** [node_size t i] is the cached minified byte size of node [i]'s rule. *)

val node_selector_size : t -> node_id -> int
(** [node_selector_size t i] is the cached minified byte size of node [i]'s
    selector. *)

val node_branches : t -> node_id -> string list
(** [node_branches t i] is the {!selector_branch_key} of each comma branch of
    node [i]'s selector, sorted: two nodes with one list have one selector up to
    branch order. *)

val selector_branch_key : Selector.t -> string
(** [selector_branch_key s] is the minified text of [s] canonicalized, the key
    under which a branch is pinned and shared. *)

val node_origin : t -> node_id -> int
(** [node_origin t i] is the first source appearance of node [i], the key
    {!canonical_order} breaks ties on: an authored node's position, and for a
    node a rewrite produced the earliest origin of the consumed nodes it shares
    a selector branch with (of all of them when it shares none). *)

val compare_position : t -> node_id -> node_id -> int
(** [compare_position t i j] orders nodes [i] and [j] as {!canonical_order}
    prints them: negative when [i] comes first. It answers in constant time for
    every node of [t], consumed ones included, which keep the place they had.
    The order is the lineage's, kept by its newest graph: an older graph reads
    it restricted to its own nodes, a linear extension of its edges too. *)

val node_slot : t -> node_id -> int
(** [node_slot t i] is the source position where node [i] prints: the largest
    {!node_origin} of the nodes up to it in {!compare_position}, consumed ones
    included. It never decreases along the order, is never below
    [node_origin t i], and is [i]'s own position for a graph {!of_rules} built.
*)

val moved : t -> node_id list
(** [moved t] is the live nodes, besides the nodes it produced, whose place in
    {!compare_position} or whose {!node_slot} the rewrite that made [t] changed:
    the nodes that had to wait for a produced node, and the nodes whose slot
    that changed. Empty for a graph {!of_rules} built. *)

val generation : t -> int
(** [generation t] increments after every successful rewrite. Candidate queues
    can use it to discard stale work. *)

val live_nodes : t -> node_id list
(** [live_nodes t] is the live node ids in creation order. *)

val exists_live_node : t -> (node_id -> bool) -> bool
(** [exists_live_node t f] is whether [f] holds of some live node, asked in
    creation order and stopping at the first that says yes. This is
    [List.exists f (live_nodes t)] without the list: the candidate search runs
    it once per proposed group over the whole graph, and answers no at almost
    every node, so the list was the scan's whole cost. *)

val exists_live_between : t -> node_id -> node_id -> (node_id -> bool) -> bool
(** [exists_live_between t lo hi f] is whether [f] holds of some live node
    printed strictly between [lo] and [hi], asked in printed order and stopping
    at the first that says yes. It walks those nodes alone, consumed ones
    included, so a test of what stands between two rules costs their distance
    rather than the size of the graph. *)

val declaration_body_key : t -> node_id -> int list
(** [declaration_body_key t i] is a hash key for bucketing nodes with identical
    declaration bodies. Hash collisions must be checked by callers. *)

val conflict : t -> node_id -> node_id -> bool
(** [conflict t i j] is [true] when nodes [i] and [j] are order-constrained:
    equal-specificity selector branches may match a common element and the rules
    write a shared equal-importance property, or the graph enables structural
    pins and they share a selector branch. Symmetric. *)

val rules_conflict :
  ?parent:Selector.t -> Stylesheet.rule -> Stylesheet.rule -> bool
(** [rules_conflict ?parent a b] is {!val-conflict} between the two nodes of
    [of_rules ?parent ~pin_shared_branches:false [a; b]], decided without
    building the graph: [true] when an element can match selector branches of
    [a] and [b] of equal specificity and the two write a common slot at equal
    importance, so that element computes something different once the two swap
    source order. Symmetric.

    [false] is the side a caller acts on: it says no element can tell the two
    rules apart, so a pass may reorder them. Each half leans towards [true] when
    it cannot decide: {!Selector_summary.may_overlap} answers [true] unless it
    can prove the two subjects disjoint, and a shorthand counts as writing every
    longhand it expands to. The answer holds within one origin, layer and scope
    context, where specificity and then order of appearance rank the
    declarations (CSS Cascade 5 sec. 6.1), so the selectors have to be the ones
    the cascade weighs: a nested rule's selector expanded against its ancestors,
    as [parent] does for one level. *)

type facts
(** What {!val-rules_conflict} reads off one rule. *)

val facts : ?parent:Selector.t -> Stylesheet.rule -> facts
(** [facts ?parent r] is what {!val-rules_conflict} reads off [r], read once for
    a caller that asks about [r] against many rules. *)

val facts_conflict : facts -> facts -> bool
(** [facts_conflict (facts ?parent a) (facts ?parent b)] is
    [rules_conflict ?parent a b]. *)

type facts_index
(** Rules read by {!val-facts}, each at a position, indexed by what
    {!val-facts_conflict} needs two rules to share. *)

val facts_index : unit -> facts_index
(** [facts_index ()] is an empty index. *)

val index_facts : facts_index -> at:int -> facts -> unit
(** [index_facts index ~at f] adds [f] to [index] at position [at], which is no
    less than any position added before. *)

val conflicts_after : facts_index -> after:int -> facts -> bool
(** [conflicts_after index ~after f] is [true] when some [g] added to [index] at
    a position greater than [after] has [facts_conflict f g]. It tests only the
    rules after [after] sharing a specificity and an overlap key with [f]. *)

val selectors_conflict : Selector.t -> Selector.t -> bool
(** [selectors_conflict a b] is the selector half of {!val-rules_conflict}: some
    branch of [a] and some branch of [b] have equal specificity and may match
    one element. *)

type branches
(** The comma branches of a selector, each with what {!selectors_conflict} reads
    off it. *)

val branches : Selector.t -> branches
(** [branches s] is what {!selectors_conflict} reads off [s]. *)

val node_selector_conflict : t -> branches -> node_id -> bool
(** [node_selector_conflict t (branches a) i] is
    [selectors_conflict a (node_rule t i).selector], read off what [t] holds for
    node [i] rather than off its selector again. *)

val precedes : t -> node_id -> node_id -> bool
(** [precedes t i j] is [true] when the graph requires node [i] to be emitted
    before node [j], directly or through a compact transitive chain. This is for
    local candidate ordering; use {!val-canonical_order} when a full output
    projection is needed. *)

val precedes_afresh : t -> node_id -> node_id -> node_id list option
(** [precedes_afresh t i j] is the live rules a graph built afresh from the live
    rules of [t], in the order they print, puts between [i] and [j] to require
    [i] before [j], when it does: [i] prints first, and each rule on the way
    conflicts with the one before. It implies {!precedes}; the converse fails
    where only a rule a rewrite consumed orders the two. The answer holds on
    every graph rewritten from [t] while the rules it names stay live. *)

val successors : t -> node_id -> node_id list
(** [successors t i] is the nodes [i] has a direct edge to. A dense run is
    stored as a transitive chain rather than one edge per constrained pair, so a
    node that must precede another is not always a direct successor of it;
    {!val-precedes} is the reachability question. *)

val reachability_expansions : t -> int
(** [reachability_expansions t] is how many nodes [t] and the graphs it was
    rewritten from have expanded to answer {!val-precedes}. Each node's
    reachable set is settled once and reused, so on one graph this stays bounded
    by the node count however many pairs a caller asks about. A graph
    {!val-rewrite} returns inherits the sets and the count: the rewrite expands
    the nodes it produces and the settled nodes that reach one, and no other. *)

val canonical_order_by : t -> (node_id -> int) -> node_id array
(** [canonical_order_by t rank] is the live node indices in a linear extension
    of the dependency DAG, choosing among the nodes currently free to come next
    the one with the smallest [rank] (then node id). {!val-canonical_order}
    passes source position for a minimal-disruption order; the canonical
    comparison passes a content-derived rank so two source orderings of
    cascade-independent rules converge to one form. *)

type run
(** The order of one run of rules a canonical projection settles, kept while
    some of its rules fold together. Unlike {!t}, a consumed node leaves no
    relay behind: once it is gone, two rules only it ordered are free. *)

val run :
  ?parent:Selector.t ->
  ?exclusive:(node_id -> node_id -> bool) ->
  folds:(node_id -> bool) ->
  Stylesheet.rule list ->
  run
(** [run ?parent ?exclusive ~folds rules] is the run of [rules], node [i] the
    [i]th rule standing at position [i]. Until a fold, {!run_order} orders it as
    {!canonical_order_by} orders
    [of_rules ?parent ~pin_shared_branches:false ?exclusive rules]. [folds i]
    says whether node [i] may later be consumed by {!fold}; the fewer that may,
    the fewer edges the run holds. [exclusive i j] asserts that nodes [i] and
    [j] never apply to one element together, so no cascade conflict is recorded
    between them, and it speaks for the nodes {!fold} produces too. *)

val run_order : run -> (node_id -> int) -> node_id array
(** [run_order r rank] is the live nodes of [r] in a linear extension of their
    conflicts, choosing among the nodes free to come next the one with the
    smallest [rank]. That order becomes where the nodes stand. [rank] must be
    injective on the live nodes. *)

val run_conflict : run -> node_id -> node_id -> bool
(** [run_conflict r i j] is {!rules_conflict} between the rules of nodes [i] and
    [j], whatever [exclusive] says. *)

val fold : run -> node_id list -> Stylesheet.rule -> at:node_id -> node_id
(** [fold r consumed rule ~at] replaces the live nodes [consumed], each one
    [run]'s [folds] allowed or one an earlier fold produced, by a new node for
    [rule] standing where [at] stood, and is that node. [at] is one of
    [consumed]. *)

val canonical_order : t -> node_id array
(** [canonical_order t] is the live node indices in a canonical linear extension
    of the dependency DAG (a path from the earlier to the later node of every
    conflicting pair). Among the nodes currently free to come next, Kahn's
    algorithm takes the smallest {!node_origin}, then the node id, consumed
    nodes taking their turn as relays. Every constrained pair keeps its required
    order; unconstrained pairs are projected deterministically without inventing
    a lexicographic CSS order.

    The lineage keeps that order as rewrites land rather than running Kahn again
    ({!compare_position} reads it), so this walks it in time linear in the
    nodes. A node a rewrite produces prints after its last predecessor, before
    the first node free then whose origin is later than its own; the nodes it
    must precede that printed before that predecessor wait for it and come after
    it, interleaved by origin with the nodes that follow. *)

val rewrite :
  t ->
  consume:node_id list ->
  produce:Stylesheet.rule list ->
  (t, rewrite_error) result
(** [rewrite] is {!val-try_rewrite} with rejection diagnostics. *)

val refuses : t -> consume:node_id list -> produce:Stylesheet.rule list -> bool
(** [refuses t ~consume ~produce] is [true] when {!rewrite} [~consume ~produce]
    refuses for another reason than a cycle: a node is not live, or a produced
    rule conflicts with a rule its sources print on both sides of, or with none
    of them. That reads the live rules alone and where they print, so a graph
    built afresh from the rules [t] prints, in that order, refuses it too. A
    rewrite it does not refuse may still close a cycle. *)

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
    stale.

    The result shares [t]'s storage, so the attempt costs what it touches rather
    than the size of the graph, accepted or not. [t] still answers every
    question about itself afterwards, but it can no longer be rewritten: a
    lineage of graphs is rewritten from its newest member only.

    Raises [Invalid_argument] if a rewrite of [t] or of a graph rewritten from
    it was already accepted. *)
