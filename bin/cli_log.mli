(** Logging and diagnostic verbosity for the cascade CLI.

    [-q]/[--quiet] keeps errors only, [-v]/[--verbose] raises the level (once
    for info, twice for debug). The optimizer emits its per-pass step tracing on
    the [cascade.optimize] source at debug level, so [-vv] surfaces it. *)

(** How much of what a parse recovered from is reported. *)
type verbosity =
  | Quiet  (** [-q]: no warning, only errors. *)
  | Normal
      (** The default: a warning that repeats one already printed for the same
          file is counted under it rather than printed again. *)
  | Verbose  (** [-v] and up: every warning, each in full. *)

val term : verbosity Cmdliner.Term.t
(** [term] installs the [Logs_fmt] reporter, sets the global level from the
    [-q]/[-v] flags, and is the verbosity they select. Compose it into a
    command's term so logging is configured before the command runs. *)
