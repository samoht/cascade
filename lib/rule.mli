(** Rule-local optimization passes. *)

val single : ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Optimize one rule without descending into nested statements. *)

val finalize :
  ?canonicalize_selector:bool -> ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Final declaration cleanup before a rule leaves the fixpoint. *)

val drop_shadowed : Stylesheet.rule list -> Stylesheet.rule list
(** Drop same-selector shadowed rules and declarations. *)

val merge_adjacent_identical :
  ctx:Ctx.t -> Stylesheet.rule list -> Stylesheet.rule list
(** Merge each maximal run of adjacent rules whose declaration bodies are
    identical into one selector-list rule. Cascade-safe for any DOM: the same
    declarations apply either way. Runs with the always-on local rewrites,
    independent of the gated global factoring. *)
