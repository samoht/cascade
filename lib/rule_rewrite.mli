(** Local DAG rewrite candidates and their scoring. *)

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
    what they would actually commit: bytes under the raw objective; under the
    transfer objective, in thousandths of a byte, the estimated compressed size
    of the consumed rules, read in the order they print, less that of the
    produced rules. *)

val v :
  objective:Ctx.objective ->
  kind:kind ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  consume:Rule_graph.node_id list ->
  produce:Stylesheet.rule list ->
  candidate option
(** [v ~objective ~kind ~finalize g ~consume ~produce] finalizes [produce],
    compares its size to [consume]'s as [objective] measures them, and is the
    candidate when it saves something. *)

val of_finalized :
  objective:Ctx.objective ->
  kind:kind ->
  Rule_graph.t ->
  consume:Rule_graph.node_id list ->
  produce:Stylesheet.rule list ->
  size:int ->
  candidate option
(** [of_finalized ~objective ~kind g ~consume ~produce ~size] is the candidate
    {!v} makes of rules it finalized into [produce], none of them empty, whose
    minified size is [size]. *)

val bytes : Ctx.objective -> int -> int
(** [bytes objective saving] is [saving], a {!field-saving} under [objective],
    in bytes. *)
