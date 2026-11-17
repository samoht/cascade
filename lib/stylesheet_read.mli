(** The statement reader: dispatch on the at-keyword, the grouping at-rules,
    style rules and their nesting bodies, and a whole stylesheet with the
    placement rules of its prelude. *)

open Stylesheet_intf

val follows_conditional : statement -> bool
(** [follows_conditional stmt] holds when an [@else] may follow [stmt], that is
    when [stmt] is an [@when] or an [@else]. *)

val read_layer_name : Cursor.t -> layer_name
(** [read_layer_name r] reads a [<layer-name>], refusing a CSS-wide keyword. *)

val at_rule_keyword : string -> string
(** [at_rule_keyword name] is the at-rule name [name] as {!val-read_statement}
    dispatches on it: folded to lower case, but for a [charset] in another case,
    which names no rule. *)

val read_statement : Cursor.t -> statement
(** [read_statement r] reads one top-level statement. *)

val read_block : Cursor.t -> block
(** [read_block r] reads the statements of a block. *)

val read_rule : ?nested:bool -> Cursor.t -> rule
(** [read_rule ?nested r] reads a style rule; with [~nested:true] its prelude is
    a relative selector list. *)

(** {1 Prelude placement} *)

type prelude
(** How far a sheet has read into the [@charset], [@import] and [@namespace]
    that open it. *)

val prelude_start : prelude
(** [prelude_start] is the place before the first statement of a sheet. *)

val misplaced : prelude -> statement -> string option
(** [misplaced prelude stmt] is the placement rule [stmt] breaks when it comes
    at [prelude], [None] when it may stand there. A misplaced statement is
    invalid and a browser ignores it. *)

val prelude_next : prelude -> statement -> prelude
(** [prelude_next prelude stmt] is the place after [stmt] read at [prelude]. *)

val read : Cursor.t -> stylesheet
(** [read r] reads a whole stylesheet, raising on the first invalid statement.
*)
