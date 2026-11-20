(** CSS nesting flattening. *)

val block : Stylesheet.statement list -> Stylesheet.statement list
(** Flatten nested rules in a statement block. *)

val rules_with_nested_declarations :
  Stylesheet.statement list -> Stylesheet.statement list
(** [rules_with_nested_declarations block] flattens, as {!block} does, each
    style rule of [block] whose body holds a nested declarations rule (CSS
    Nesting 1 sec. 3.4 and 5) at any depth, and keeps every other rule as
    written. Releases that parse nesting but predate nested declarations rules
    read such a body differently. *)
