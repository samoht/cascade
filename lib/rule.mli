(** Rule-local optimization passes. *)

val single : ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Optimize one rule without descending into nested statements. *)

val finalize :
  ?canonicalize_selector:bool -> ctx:Ctx.t -> Stylesheet.rule -> Stylesheet.rule
(** Final declaration cleanup before a rule leaves the fixpoint. *)

val drop_shadowed : Stylesheet.rule list -> Stylesheet.rule list
(** Drop same-selector shadowed rules and declarations. *)
