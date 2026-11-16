(** Minified CSS byte-size estimates used by optimizer cost models. *)

val decls : Declaration.declaration list -> int
(** Minified size of a declaration list without separator bytes. *)

val rule : Stylesheet.rule -> int
(** Minified size of one rule. *)

val rules : Stylesheet.rule list -> int
(** Minified size of a rule list. *)
