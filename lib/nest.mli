(** CSS nesting rewrites. *)

val contains : Selector.t -> bool
(** [contains selector] is [true] when [selector] contains [&]. *)

val substitute : ?leftmost:bool -> parent:Selector.t -> Selector.t -> Selector.t
(** Replace every [&] in a selector with [parent]. *)

val combine : Selector.t -> Selector.t -> Selector.t
(** Combine a parent selector with a nested child selector. *)

val keep_readable_branches : Selector.t -> Selector.t option
(** [keep_readable_branches sel] keeps the branches of [sel] that a reader
    accepts, and is [None] when none does. Nesting composes a parent and a child
    that are each valid into a selector putting a combinator after a
    pseudo-element, which CSS Selectors 4 sec. 3.6.5 makes invalid and no engine
    matches. *)

val drop_dead_nested : Stylesheet.rule -> Stylesheet.rule
(** [drop_dead_nested rule] drops from [rule]'s body every nested rule whose
    selector, composed with the parent's, {!keep_readable_branches} rejects, and
    every branch of one that keeps others. A dropped rule takes its own body
    with it, and a conditional block is walked under the same parent, so the
    body keeps exactly what flattening it would keep. Physically unchanged when
    nothing is dead. *)

val merge_lone : Stylesheet.rule -> Stylesheet.rule
(** Merge a pure wrapper rule with its sole nested rule when safe. A merge
    {!keep_readable_branches} rejects outright leaves the wrapper empty, for the
    empty-rule pass to drop. *)

val hoist_declaration_runs : Stylesheet.rule -> Stylesheet.rule
(** [hoist_declaration_runs rule] moves each declaration written after a nested
    statement (CSS Nesting 1 sec. 3.4) back into the rule's own run, as far as
    it commutes with everything nested above it. A declaration whose property a
    crossed statement also writes stays where the author put it, so the rule
    computes the same values while the rest rejoins the declaration passes.
    Physically unchanged when nothing moves. *)

val leaves :
  ?parent:Selector.t -> Stylesheet.statement list -> Stylesheet.rule list option
(** [leaves ?parent stmts] is every declaration [stmts] write, at any depth, as
    the style rules that write them in source order, each under the selector the
    cascade weighs it by and with no nested statement. [parent] is the selector
    of the style rule whose body [stmts] is: a nested rule's selector is
    composed with it (CSS Nesting 1 sec. 3) and a declarations run (sec. 3.4) is
    a rule with that selector. A conditional group's rules are read as though
    its condition held. [None] when some declaration has no selector to weigh it
    by: a declarations run with no [parent], or a rule under [@scope], whose [&]
    is the scoping root. *)

val rules : Stylesheet.rule list -> Stylesheet.rule list
(** Synthesize nested rules from adjacent flat selector chains. *)

val statements : Stylesheet.statement list -> Stylesheet.statement list
(** Apply {!rules} to each adjacent run of rule statements. *)
