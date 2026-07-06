(** Local DAG rewrite candidates and byte scoring. *)

type kind =
  | Identical_body
  | Same_selector
  | Exact_shared_declarations
  | Selector_branch_inline
  | Default_factoring
      (** The family that produced a candidate. Used for deterministic
          scheduling, debugging, and tests. *)

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
(** Transaction-local minified-size cache for scoring many candidates against
    the same graph. *)

val size_cache : Rule_graph.t -> size_cache
(** [size_cache graph] creates an empty cache for candidate scoring over
    [graph]. The cache is intentionally short-lived: create one per candidate
    enumeration pass. *)

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
