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

val drop_shadowed :
  floors:Support.targets -> Stylesheet.rule list -> Stylesheet.rule list
(** [drop_shadowed ~floors rules] drops the declarations, and the rules left
    with none, that a later rule of the same selector writes over. A later write
    shadows only where every browser at or above [floors] reads it. *)

val normalize :
  ctx:Ctx.t ->
  held:Shorthand.held ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** [normalize ~ctx ~held rules] runs the rule-local passes over a rule run:
    {!single} on each rule, {!drop_shadowed} over the run and {!finalize} on
    each rule, with [held] summarising the run. *)

val merge_adjacent_identical :
  ctx:Ctx.t -> Stylesheet.rule list -> Stylesheet.rule list
(** Merge each maximal run of adjacent rules whose declaration bodies are
    identical into one selector-list rule. Cascade-safe for any DOM: the same
    declarations apply either way. Runs with the always-on local rewrites,
    independent of the gated global factoring. *)
