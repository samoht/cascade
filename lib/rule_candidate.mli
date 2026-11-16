(** Fast local rewrite candidate generation for {!Rule_graph}. *)

type memo
(** What {!enumerate} worked out from the rules of a subset alone, for the
    graphs one rewrite lineage passes through. *)

val memo : unit -> memo
(** [memo ()] holds nothing yet. *)

val enumerate :
  ?memo:memo ->
  ?touching:Rule_graph.node_id list ->
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  Rule_rewrite.candidate list
(** [enumerate ?touching ~ctx ~finalize graph] returns bounded positive-saving
    rewrite candidates. When [touching] is supplied, only buckets that mention
    at least one of those nodes are expanded; this is the incremental
    scheduler's neighborhood refresh after a commit. The list is intentionally
    local and heuristic; every candidate still commits through
    {!Rule_graph.rewrite}.

    [memo] (empty by default) is read and extended. Given the one an earlier
    call filled on an ancestor of [graph], with the same [ctx] and [finalize],
    the call returns the same candidates as with an empty one, and a subset
    whose rules it already judged costs only the questions that read the rest of
    [graph]: which rules are live, and which precede which. Node ids name the
    same rules on every graph {!Rule_graph.rewrite} derives from one another,
    and other rules on any other graph. *)

val nested_merge_is_safe : stats:Stats.t -> Stylesheet.rule list -> bool
(** [nested_merge_is_safe ~stats rules] is whether a run of rules on one
    selector, given in source order, can merge into the first without changing
    what any element computes. CSS Nesting 1 sec. 3.4 puts a declaration written
    after a nested rule behind it, so merging moves each later rule's
    declarations ahead of every nested block an earlier rule carries; the answer
    is [false] as soon as one of those pairs writes a common cascade slot at the
    same weight with a different value. Selectors are not read, so a pair that
    could never meet on one element still counts.

    Each declaration block put to the nested bodies written before it is counted
    in [stats] as one [nested_merge_probes], so a run of [n] rules adds at most
    [n - 1]. *)
