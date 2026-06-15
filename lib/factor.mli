(** Cascade-aware rule factoring. *)

val common : Stylesheet.rule list -> Stylesheet.rule list
(** Factor common declaration groups from adjacent and indexed lookahead runs.
*)

val anchor : Stylesheet.rule list -> Stylesheet.rule list
(** Factor declaration gaps with the global anchor scheduler. *)

val run :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** Run the incremental factor fixpoint. *)
