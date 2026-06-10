(** CSS nesting rewrites. *)

val contains : Selector.t -> bool
(** [contains selector] is [true] when [selector] contains [&]. *)

val substitute : parent:Selector.t -> Selector.t -> Selector.t
(** Replace every [&] in a selector with [parent]. *)

val combine : Selector.t -> Selector.t -> Selector.t
(** Combine a parent selector with a nested child selector. *)

val merge_lone : Stylesheet.rule -> Stylesheet.rule
(** Merge a pure wrapper rule with its sole nested rule when safe. *)

val rules : Stylesheet.rule list -> Stylesheet.rule list
(** Synthesize nested rules from adjacent flat selector chains. *)

val statements : Stylesheet.statement list -> Stylesheet.statement list
(** Apply {!rules} to each adjacent run of rule statements. *)
