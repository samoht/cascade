(** Closed-world [var()] inlining for the cascade CLI. Thin wrapper around
    {!Cascade.Css.inline_vars}; CLI-specific keep-list handling lives here. *)

open Cascade

let run (verbosity : Cli_log.verbosity) ~keep_vars stylesheet =
  Css.inline_vars ?keep_vars:(Some keep_vars)
    ~warn:(fun name ->
      match verbosity with
      | Quiet -> ()
      | Normal | Verbose ->
          Cli_io.warning
            "%s takes different values in different scopes; kept live (cannot \
             inline safely)"
            name)
    stylesheet
