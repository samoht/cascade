(** The canonical form of a run of style rules, read as the trace of its
    declarations.

    A run of plain style rules and conditional blocks holding only such rules
    reads as a sequence of entries, one per declaration, each under the selector
    and the block conditions of its rule. Two entries depend on each other when
    swapping them where they stand side by side could change a value some
    element computes: they write a common slot differently, at one importance,
    on an element both can match at one specificity. Entries that do not depend
    on each other may swap, so every sequence reached that way is one trace, the
    partial order of the dependencies (Mazurkiewicz traces). The canonical form
    is one linear extension of that order, chosen from the order and the
    entries' text alone, so two runs with one order print one form whatever
    order their folds or their source took. Where a run's rules begin and end is
    not read: only the selector and the conditions each entry sits under. *)

val settle :
  ctx:Ctx.t ->
  ?parent:Selector.t ->
  ?drops:bool ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** [settle ~ctx ?parent ?drops run] is the canonical form of [run], a sequence
    of style rules without nested rules and of [@media], [@supports] and
    [@container] blocks holding only such statements. [parent] is the enclosing
    nesting selector the rules' selectors are relative to. [drops] (default
    [true]) is [false] for a run another run reads again, the body of a block
    that is an element of it: whether an entry is a repeat is decided by the run
    that sees every entry.

    An entry that a later entry of its selector and conditions overrides on
    every element it applies to is dropped, as the declaration dedup under [ctx]
    decides for two writes of one property, and so is an entry that changes
    nothing because the order lets it stand right after an entry with the same
    text that applies wherever it does. Of the slots an entry writes, the ones a
    later entry of its selector and conditions rewrites in every engine are not
    read when it is ordered against another selector, and two entries of
    different selectors are not ordered when a later entry of either one's
    selector and conditions repeats the other.

    Each selector's entries print as one rule wherever the order lets them stand
    together, in text order as their own order allows, and consecutive rules
    under one block prelude print in one block. *)
