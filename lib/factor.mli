(** Cascade-aware rule factoring. *)

type cache
(** Per-optimization memo for flat rule runs. A cache is scoped to one
    finalizer/context pair; do not share it across independent optimizer calls.
*)

val cache : unit -> cache
(** [cache ()] creates an empty memo table for one optimizer pipeline run. *)

val run :
  ?cache:cache ->
  ?settle:(Stylesheet.rule list -> Stylesheet.rule list) ->
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Stylesheet.rule list ->
  Stylesheet.rule list
(** Run the DAG-backed factor fixpoint. [settle] (default identity) applies
    cheap postprocessing to both sides of the transfer-size gate, so the gate
    compares the actual alternatives returned to its caller. *)
