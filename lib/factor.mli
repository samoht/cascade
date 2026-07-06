(** Cascade-aware rule factoring. *)

type cache
(** Per-optimization memo for flat rule runs. A cache is scoped to one
    finalizer/context pair; do not share it across independent optimizer calls.
*)

val cache : unit -> cache
(** [cache ()] creates an empty memo table for one optimizer pipeline run. *)

val run :
  ?cache:cache ->
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** Run the DAG-backed factor fixpoint. *)
