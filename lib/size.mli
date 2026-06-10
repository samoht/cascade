(** Minified CSS byte-size estimates used by optimizer cost models. *)

val decls : Declaration.declaration list -> int
(** Minified size of a declaration list without separator bytes. *)

val rule : Stylesheet.rule -> int
(** Minified size of one rule. *)

val rules : Stylesheet.rule list -> int
(** Minified size of a rule list. *)

val decl_list : int -> int -> int
(** [decl_list decl_bytes decl_count] adds declaration separator bytes. *)

val rule_from_parts : int -> int -> int -> int
(** [rule_from_parts selector_bytes decl_bytes decl_count] estimates a rule. *)
