(** Closed-world [var()] inlining for the cascade CLI. Thin wrapper around
    {!Cascade.Css.inline_vars}; CLI-specific keep-list handling lives here. *)

open Cascade

let run ~keep_vars stylesheet =
  Css.inline_vars ?keep_vars:(Some keep_vars) stylesheet
