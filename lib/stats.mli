(** Optimizer profiling counters. *)

type pass_stat = {
  mutable time : float;  (** accumulated wall-clock, seconds *)
  mutable calls : int;  (** times the pass ran across all fixpoint iterations *)
  mutable changes : int;  (** times the pass returned a structurally new list *)
  mutable rules_in : int;  (** total input rule count summed across calls *)
  mutable rules_out : int;  (** total output rule count summed across calls *)
}
(** One pass's contribution to a single optimizer run. *)

val pass_times : (string, pass_stat) Hashtbl.t
(** Per-pass stats for the factor fixpoint. *)

val pass : string -> pass_stat
(** [pass name] returns the mutable stats bucket for [name]. *)

val set_profile : bool -> unit
(** Enable or disable exact diagnostic size collection. *)

val profile : unit -> bool
(** Whether exact diagnostic size collection is enabled. *)

val add_saving : int -> unit
(** Add committed factoring savings for the current iteration. *)

val reset_saving : unit -> unit
(** Reset committed factoring savings for the current iteration. *)

val saving : unit -> int
(** Committed factoring savings for the current iteration. *)

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

val iteration_stats : unit -> iteration_stat list
(** Per-iteration stats, newest first. *)

type counters = {
  mutable iterations : int;  (** [factor_rules_to_fixpoint] iterations *)
  mutable factor_fixpoints_run : int;
      (** global factoring fixpoints attempted after the preflight *)
  mutable marginal_stops : int;
      (** fixpoints stopped because consecutive iterations had low byte gain *)
  mutable factor_fixpoints_skipped : int;
      (** global factoring fixpoints skipped by the incremental preflight *)
  mutable factor_preflight_gain : int;
      (** total raw-byte gain estimated by the global factoring preflight *)
  mutable factor_bytes_saved : int;
      (** total committed byte savings reported by global factoring passes *)
  mutable factor_transfer_reverts : int;
      (** factoring results discarded because the estimated DEFLATE size grew *)
}
(** Global counters across the last optimizer run. *)

val counters : counters
(** The global mutable counters. *)

val record_iteration :
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

val reset : unit -> unit
(** Reset all counters and pass timings. *)
