(** Rule-local optimization passes. *)

val single : ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Optimize one rule without descending into nested statements. *)

val declaration_run :
  ctx:Ctx.t -> Declaration.declaration list -> Declaration.declaration list
(** Declaration cleanup for a body that has no rule of its own: a nested
    declarations run or a bare [Declarations] block. *)

val finalize :
  ?held:Shorthand.held ->
  ?canonicalize_selector:bool ->
  ctx:Ctx.t ->
  Stylesheet.rule ->
  Stylesheet.rule
(** Final declaration cleanup before a rule leaves the fixpoint. [held] is what
    the rest of the rule run holds, which shorthand composition needs and the
    rule alone cannot show. *)

val drop_shadowed : Stylesheet.rule list -> Stylesheet.rule list
(** Drop same-selector shadowed rules and declarations. *)

val merge_adjacent_identical :
  ctx:Ctx.t -> Stylesheet.rule list -> Stylesheet.rule list
(** Merge each maximal run of adjacent rules whose declaration bodies are
    identical into one selector-list rule. Cascade-safe for any DOM: the same
    declarations apply either way. Runs with the always-on local rewrites,
    independent of the gated global factoring. *)
