(** Incremental, semantics-preserving rule merging on a {!Pool}.

    The eventual replacement for the optimizer's iterate-to-fixpoint factoring,
    following the greedy strategy of Hague, Lin & Hong ("CSS Minification via
    Constraint Solving", TOPLAS 2019): keep the rules in a pool (union-find +
    order maintenance) and apply merges directly, so each pass is local rather
    than a whole-list rescan.

    It exposes two merge kinds: {!combine_identical} unions adjacent rules that
    share a declaration block, and {!factor_common} hoists a declaration shared
    by a run of rules into a single rule ahead of them. *)

val combine_identical : Stylesheet.rule list -> Stylesheet.rule list
(** [combine_identical rs] merges each run of consecutive rules that share an
    identical declaration block into one rule whose selector is the union of
    theirs, in a single left-to-right sweep. Adjacent rules with the same block
    are always cascade-safe to merge (an element matching several gets the same
    declarations either way). *)

val factor_common : Stylesheet.rule list -> Stylesheet.rule list
(** [factor_common rs] greedily hoists a declaration shared by a maximal run of
    consecutive rules into one shared rule placed ahead of the run, keeping the
    per-rule leftovers, whenever doing so shrinks the output. Restricted to the
    provably-safe case: the hoisted declaration's property must be unique within
    each rule of the run, so hoisting cannot reorder any property. *)
