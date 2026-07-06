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
