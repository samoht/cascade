(** Fast local rewrite candidate generation for {!Rule_graph}. *)

val enumerate :
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
    {!Rule_graph.rewrite}. *)

val nested_merge_is_safe : Stylesheet.rule list -> bool
(** [nested_merge_is_safe rules] is whether a run of rules on one selector,
    given in source order, can merge into the first without changing what any
    element computes. CSS Nesting 1 sec. 3.4 puts a declaration written after a
    nested rule behind it, so merging moves each later rule's declarations ahead
    of every nested block an earlier rule carries; the answer is [false] as soon
    as one of those pairs writes a common cascade slot at the same weight with a
    different value. Selectors are not read, so a pair that could never meet on
    one element still counts. *)
