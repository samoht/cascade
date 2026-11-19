(** Rewrite candidates for {!Rule_graph}, kept up to date across rewrites.

    The candidates are bounded and local: groups of rules sharing a body, a
    selector, a declaration or a property, and receivers for a long selector
    branch. Every candidate still commits through {!Rule_graph.rewrite}. *)

type t
(** The candidates of one graph, carried to each graph {!rewrite} reaches. *)

type offer
(** A candidate and the key it is queued under. *)

val key : offer -> int
(** [key o] is a key no other offer of the same {!t} ever carries. *)

val candidate : offer -> Rule_rewrite.candidate
(** [candidate o] is the candidate [o] offers. *)

val start :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  t
(** [start ~ctx ~finalize graph] files the rules of [graph]; the first
    {!refresh} offers its candidates. [finalize] scores and normalizes every
    produced rule. [graph] pins rules that share a selector branch, as
    {!Rule_graph.of_rules} does by default. *)

val rewrite : t -> Rule_graph.t -> consume:Rule_graph.node_id list -> unit
(** [rewrite t graph ~consume] refiles the rules [Rule_graph.rewrite] consumed
    and produced to return [graph] from the graph [t] last saw. The candidates
    themselves wait for {!refresh}. *)

val refresh : t -> offer list
(** [refresh t] works out the candidates the rewrites since the last refresh
    brought about, and is the offers it made, in the order it made them.
    Afterwards {!candidates} [t] is {!enumerate}[ graph] for the last graph
    [graph]. The cost follows what the rewrites changed: the keys their rules
    are filed under, and the candidates that read a span or an order of the
    printed rules they touched, moved or reslotted. *)

val offers : t -> offer list
(** [offers t] is every offer [t] makes as of the last {!refresh}, in the order
    they were made. *)

val afresh : t -> bool
(** [afresh t] is [true] when a graph built afresh from the rules the graph [t]
    last refreshed on prints, in that order, would accept no candidate [t] has
    not offered. Such a graph searches the same subsets, but it orders two rules
    only through live rules that conflict, so a default group can keep an entry
    [t] drops. The candidates that brings about are asked {!Rule_graph.refuses}.
    It reads the graph and leaves the offers of [t] as they are. *)

val is_live : t -> Rule_graph.node_id -> bool
(** [is_live t id] is [true] if the rule [id] is live in the graph [t] last
    refreshed on. *)

val candidates : t -> Rule_rewrite.candidate list
(** [candidates t] is the candidates of [offers t]. *)

val enumerate :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  Rule_rewrite.candidate list
(** [enumerate ~ctx ~finalize graph] is the candidates of [graph], worked out
    from nothing. *)
