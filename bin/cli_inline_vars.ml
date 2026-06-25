(** Closed-world [var()] inlining for the cascade CLI. Thin wrapper around
    {!Cascade.Css.inline_vars}; CLI-specific keep-list handling lives here. *)

open Cascade

let run ~keep_vars stylesheet =
  Css.inline_vars ?keep_vars:(Some keep_vars)
    ~warn:(fun name ->
      Fmt.epr
        "Warning: %s is redefined in a different scope; kept live (cannot \
         inline safely)@."
        name)
    stylesheet
