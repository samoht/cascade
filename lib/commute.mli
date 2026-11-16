(** Whether declaration blocks commute: replayed in either order, they leave
    every element computing the same thing. *)

type fact
(** A declaration with the cascade slots it writes, read once. *)

val fact : Declaration.declaration -> fact
(** [fact d] reads the slots [d] writes. *)

val facts : Declaration.declaration list -> fact list
(** [facts ds] is [List.map fact ds]. *)

val decl : fact -> Declaration.declaration
(** [decl f] is the declaration [f] was read off. *)

val conflict : fact -> fact -> bool
(** [conflict a b] is whether the two declarations write a common cascade slot
    at the same weight with values that do not minify the same, so their order
    is observable. Symmetric. *)

val merge_order :
  (Rule_graph.node_id * Stylesheet.rule) list ->
  (Rule_graph.node_id * Stylesheet.rule) list
(** [merge_order rules] is the order a merge of same-selector [rules], given in
    source order, replays their blocks in: every pair whose declarations or
    nested blocks conflict keeps its source order, and the rest are ordered by
    their declarations' property, importance and hash, then by node. *)

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
