(** CSS nesting rewrites. *)

val contains : Selector.t -> bool
(** [contains selector] is [true] when [selector] contains [&]. *)

val substitute : ?leftmost:bool -> parent:Selector.t -> Selector.t -> Selector.t
(** Replace every [&] in a selector with [parent]. *)

val combine : Selector.t -> Selector.t -> Selector.t
(** Combine a parent selector with a nested child selector. *)

val merge_lone : Stylesheet.rule -> Stylesheet.rule
(** Merge a pure wrapper rule with its sole nested rule when safe. *)

val hoist_declaration_runs : Stylesheet.rule -> Stylesheet.rule
(** [hoist_declaration_runs rule] moves each declaration written after a nested
    statement (CSS Nesting 1 sec. 3.4) back into the rule's own run, as far as
    it commutes with everything nested above it. A declaration whose property a
    crossed statement also writes stays where the author put it, so the rule
    computes the same values while the rest rejoins the declaration passes.
    Physically unchanged when nothing moves. *)

val rules : Stylesheet.rule list -> Stylesheet.rule list
(** Synthesize nested rules from adjacent flat selector chains. *)

val statements : Stylesheet.statement list -> Stylesheet.statement list
(** Apply {!rules} to each adjacent run of rule statements. *)
