(** The order a {!Rule_graph} prints its nodes in, kept as rewrites add nodes.

    The order is Kahn's algorithm over every node, consumed ones included,
    taking among the nodes free to come next the one with the smallest
    [(origin, id)]: a topological order of the edges with the first source
    appearance as the tie-break. A consumed node stays in the graph as a relay
    with its edges and its origin, so consuming one changes nothing; only an
    added node, which enters with its edges, moves anything. {!add} updates the
    order for one added node in place, at the cost of the nodes between its last
    predecessor and where it lands and of the nodes it displaces, rather than
    running Kahn over the whole graph again. *)

type 'edge nodes = {
  order : Order_maintenance.t;
      (** every node in the order, the added one not yet *)
  origin : int array;  (** each node's first source appearance *)
  slot : int array;
      (** where each node prints on the source scale: the largest origin up to
          it in {!field-order}, which {!add} keeps *)
  succ : (int * 'edge) list array;
      (** each node's edges; one to a node not in {!field-order} is skipped *)
  death : int array;
  generation : int;
      (** a node is printed while its {!field-death} is past [generation] *)
}
(** What the order reads of a graph. Node numbers index the arrays. *)

val add : 'edge nodes -> preds:int list -> moved:(int -> unit) -> int -> unit
(** [add nodes ~preds ~moved p] places node [p], whose origin is set and whose
    predecessors in {!field-order} are [preds], where Kahn's algorithm takes it,
    moves the nodes that must now wait for it, and updates {!field-slot}.
    [moved] is called on every printed node, other than [p], that changed places
    or slot. *)
