(** Incremental, semantics-preserving rule merging on a {!Rule_pool}.

    The eventual replacement for the optimizer's iterate-to-fixpoint factoring,
    following the greedy strategy of Hague, Lin & Hong ("CSS Minification via
    Constraint Solving", TOPLAS 2019): keep the rules in a pool (union-find +
    order maintenance) and apply merges directly, so each pass is local rather
    than a whole-list rescan.

    This is being built one merge kind at a time; for now it exposes the
    simplest one. *)

val combine_identical : Stylesheet.rule list -> Stylesheet.rule list
(** [combine_identical rs] merges each run of consecutive rules that share an
    identical declaration block into one rule whose selector is the union of
    theirs, in a single left-to-right sweep. Adjacent rules with the same block
    are always cascade-safe to merge (an element matching several gets the same
    declarations either way). *)
