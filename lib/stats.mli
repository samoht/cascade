(** Optimizer profiling counters, scoped to one run.

    A recorder belongs to the optimizer run that created it and is reachable
    only through that run's {!Ctx.t}, so an optimization started while another
    is in progress counts its own work. Readers take a {!snapshot}: a value, not
    a view, so a later run cannot rewrite what an earlier one reported. *)

type t
(** One run's recorder. *)

val v : ?profile:bool -> unit -> t
(** [v ()] is a recorder for one optimizer run. [profile] (default [false])
    turns on exact size accounting: the byte columns cost a full serialization
    of the rule list on either side of every fixpoint iteration, so they are
    collected only when asked for. *)

val profile : t -> bool
(** Whether exact size accounting is enabled for this run. *)

type iteration_stat = {
  fixpoint : int;
  iteration : int;
  local_iteration : int;
  before_rules : int;
  after_rules : int;
  before_bytes : int;
  after_bytes : int;
  bytes_saved : int;
  active_passes : int;
  changed_passes : int;
  elapsed : float;
}
(** One global factoring fixpoint iteration. *)

type counters = {
  iterations : int;  (** [factor_rules_to_fixpoint] iterations *)
  factor_fixpoints_run : int;
      (** global factoring fixpoints attempted after the preflight *)
  factor_fixpoints_skipped : int;
      (** global factoring fixpoints skipped by the incremental preflight *)
  factor_preflight_gain : int;
      (** total raw-byte gain estimated by the global factoring preflight *)
  factor_bytes_saved : int;
      (** total committed byte savings reported by global factoring passes *)
  factor_transfer_reverts : int;
      (** factoring results discarded because the estimated DEFLATE size grew *)
}
(** Counters over one run. *)

type snapshot = {
  counters : counters;
  iteration_stats : iteration_stat list;  (** newest first *)
}
(** What a run recorded, as of the moment it was taken. *)

val snapshot : t -> snapshot
(** [snapshot t] is what [t] has recorded so far. *)

(** {1 Recording}

    Called by the factoring passes; a caller holding a recorder can add to a run
    it does not own, which only skews that run's own report. *)

val start_fixpoint : t -> int
(** [start_fixpoint t] counts one factoring fixpoint and returns its number. *)

val skip_fixpoint : t -> unit
(** Count one fixpoint the preflight declined to run. *)

val revert_fixpoint : t -> unit
(** Count one factoring result the transfer gate threw away. *)

val add_preflight_gain : t -> int -> unit
(** Add the preflight's raw-byte gain estimate for one segment. *)

val record_iteration :
  t ->
  fixpoint:int ->
  local_iteration:int ->
  before_rules:int ->
  before_bytes:int ->
  after_rules:int ->
  after_bytes:int ->
  bytes_saved:int ->
  active_passes:int ->
  changed_passes:int ->
  elapsed:float ->
  unit
(** Record one factor-fixpoint iteration. *)

val add_saving : t -> int -> unit
(** Add committed factoring savings for the current iteration. *)

val reset_saving : t -> unit
(** Reset committed factoring savings for the current iteration. *)

val saving : t -> int
(** Committed factoring savings for the current iteration. *)
