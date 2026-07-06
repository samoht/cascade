(** Incremental greedy scheduler for graph rewrites.

    This is the SatCSS-style greedy loop over {!Rule_graph}: candidates are
    ranked by byte saving, committed through {!Rule_graph.rewrite}, and
    refreshed only around the nodes affected by the last commit. *)

val run :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  Rule_graph.t
(** [run ~ctx ~finalize graph] repeatedly applies the best legal graph rewrite
    until no positive-saving candidate remains. [finalize] is used to score and
    normalize every produced rule before the candidate is admitted. *)
