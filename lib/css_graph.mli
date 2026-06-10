(** Bipartite CSS-graph view of a rule list.

    Selectors (rows) on one side, declaration occurrences (cols) on the other;
    an edge [(row, col)] records that rule [row] contains declaration [col]. The
    {b weight} of a col is the byte cost of emitting that declaration once.
    Restructuring the stylesheet to share declarations across rules is, in this
    view, a {b weighted biclique-cover} problem (Hague-Lin-Hong, TOPLAS 2019):
    replace each biclique [(R, D)] in the graph with a single rule whose
    selector is the union of [R]'s and whose declarations are [D], dropping
    edges [{r} x D] from every [r in R].

    The greedy heuristic here picks biclique candidates by exact declaration
    match, scores each by
    [(|R| - 1) * sum(weight(d) for d in D) - selector overhead], and applies the
    strictly-positive-saving ones non-overlappingly. It is intentionally
    conservative on cascade dependencies -- selector overlaps, specificity,
    importance, layer/origin/scope -- and skips any biclique whose factoring
    would change a property's resolved value for some matching element. *)

type t
(** Bipartite graph backed by sorted integer adjacency arrays. *)

val build : Stylesheet.rule list -> t
(** [build rules] builds the bipartite graph. Identical declarations
    (structurally equal, indexed by [Declaration.hash]) share a single column so
    a property/value pair appearing in K rules contributes K edges to one
    column. *)

val n_rules : t -> int
(** [n_rules g] is the number of row vertices. *)

val n_decls : t -> int
(** [n_decls g] is the number of distinct declaration columns. *)

val n_edges : t -> int
(** [n_edges g] is the total edge count (sum over rules of declaration count).
*)

(** {1 Greedy biclique cover} *)

type factoring = {
  rules : int array;
      (** Row indices, ascending, that the factored rule consumes. *)
  decls : int array;
      (** Column indices, ascending, that the factored rule shares. *)
  saving : int;
      (** Strict byte saving over emitting the original rules separately, in
          minified form. *)
}
(** A single biclique selected by the greedy cover. *)

val greedy_cover : t -> factoring list
(** [greedy_cover g] returns the non-overlapping bicliques the heuristic
    selects, in ascending row order. Each factoring saves strictly positive
    bytes and does not consume a rule shared with another returned factoring.
    The cover is computed without consulting the cascade dependency edges -- it
    is suitable as an upper bound on the savings any cascade-safe rewrite could
    achieve, useful for benchmarking the scan-based optimiser against a
    graph-theoretic ceiling. *)
