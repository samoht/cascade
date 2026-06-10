(** CSS nesting flattening. *)

val block : Stylesheet.statement list -> Stylesheet.statement list
(** Flatten nested rules in a statement block. *)
