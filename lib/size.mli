(** Minified CSS byte-size estimates used by optimizer cost models. *)

type print
(** The flags a run prints with. A declaration's minified spelling depends on
    them, so a cost model measures under the run's own. *)

val default : print
(** [default] is the print with [lossless] and [enforce_spec] unset. *)

val print : lossless:bool -> enforce_spec:bool -> print
(** [print ~lossless ~enforce_spec] is the print with those flags. *)

val decl : print -> Declaration.declaration -> int
(** [decl p d] is the length of [d]'s minified print under [p]. Under {!default}
    it is {!Declaration.size}. *)

val decls : print -> Declaration.declaration list -> int
(** [decls p ds] is the minified size of [ds] under [p], without separator
    bytes. *)

val selector : Selector.t -> int
(** Minified size of a selector. *)

val body : print -> Declaration.declaration list -> int
(** [body p ds] is the minified size of a rule body under [p]: the declarations,
    their separators and the braces. *)

val rule : print -> Stylesheet.rule -> int
(** [rule p r] is the minified size of [r] under [p]. *)

val rules : print -> Stylesheet.rule list -> int
(** [rules p rs] is the minified size of [rs] under [p]. *)
