(** Rule-local optimization passes. *)

val single : ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Optimize one rule without descending into nested statements. *)

val finalize :
  ?canonicalize_selector:bool -> ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Final declaration cleanup before a rule leaves the fixpoint. *)

val merge : Stylesheet.rule list -> Stylesheet.rule list
(** Merge adjacent rules with identical selectors. *)

val identical : Stylesheet.rule list -> Stylesheet.rule list
(** Combine consecutive rules with identical declarations. *)

val drop_shadowed : Stylesheet.rule list -> Stylesheet.rule list
(** Drop same-selector shadowed rules and declarations. *)
