(** Incremental greedy scheduler for graph rewrites.

    This is the SatCSS-style greedy loop over {!Rule_graph}: candidates are
    ranked by byte saving and committed through {!Rule_graph.rewrite}, and
    {!Rule_candidate} works out the candidates a commit brings about from what
    it changed. *)

type outcome = {
  graph : Rule_graph.t;  (** the graph the last accepted rewrite returned *)
  settled : bool;
      (** every candidate {!Rule_candidate} offers on {!field-graph}, under the
          bounds the run started with, was tried there and refused *)
}

val run :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  outcome
(** [run ~ctx ~finalize graph] repeatedly applies the best legal graph rewrite
    until no positive-saving candidate remains, or until it has applied
    [max 256 (16 * n)] for the [n] nodes of [graph], which leaves the outcome
    unsettled. [finalize] is used to score and normalize every produced rule
    before the candidate is admitted. *)
