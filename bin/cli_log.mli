(** Logging setup for the cascade CLI.

    Mirrors the standard [Logs]/[Logs_fmt] verbosity wiring: [-v]/[--verbose]
    raises the level (once for info, twice for debug), [-q]/[--quiet] drops it
    to errors only. The optimizer emits its per-pass step tracing on the
    [cascade.optimize] source at debug level, so [-vv] surfaces it. *)

val term : unit Cmdliner.Term.t
(** [term] is a cmdliner term that, when evaluated, installs the [Logs_fmt]
    reporter and sets the global level from [-q]/[-v] flags. Compose it into a
    command's term so logging is configured before the command runs. *)
