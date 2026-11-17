(** Local DAG rewrite candidates and byte scoring. *)

(** The family that produced a candidate. Used for deterministic scheduling,
    debugging, and tests. *)
type kind =
  | Identical_body
  | Same_selector
  | Exact_shared_declarations
  | Selector_branch_inline
  | Default_factoring

val equal_kind : kind -> kind -> bool
(** [equal_kind a b] tests rewrite families for equality. *)

type candidate = {
  generation : int;
  kind : kind;
  consume : Rule_graph.node_id list;
  produce : Stylesheet.rule list;
  saving : int;
}
(** A local rewrite proposal. {!field-saving} is computed after the produced
    rules are normalized/finalized, so the greedy scheduler ranks candidates by
    the bytes they would actually commit. *)

type size_cache
(** Minified sizes of produced rules, for scoring many candidates. *)

val size_cache : unit -> size_cache
(** [size_cache ()] is an empty cache. A produced rule's size does not depend on
    the graph, so one cache serves every candidate of a scheduler run, across
    its commits. *)

val v :
  ?size_cache:size_cache ->
  kind:kind ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  consume:Rule_graph.node_id list ->
  produce:Stylesheet.rule list ->
  candidate option
(** [v] finalizes {!field-produce}, compares its minified size to
    {!field-consume}, and returns a positive-saving candidate. *)
