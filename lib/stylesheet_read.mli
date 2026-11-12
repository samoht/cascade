(** The statement reader: dispatch on the at-keyword, the grouping at-rules,
    style rules and their nesting bodies, and a whole stylesheet with the
    placement rules of its prelude. *)

open Stylesheet_intf

val follows_conditional : statement -> bool
(** [follows_conditional stmt] holds when an [@else] may follow [stmt], that is
    when [stmt] is an [@when] or an [@else]. *)

val read_layer_name : Cursor.t -> layer_name
(** [read_layer_name r] reads a [<layer-name>], refusing a CSS-wide keyword. *)

val read_statement : Cursor.t -> statement
(** [read_statement r] reads one top-level statement. *)

val read_block : Cursor.t -> block
(** [read_block r] reads the statements of a block. *)

val read_rule : ?nested:bool -> Cursor.t -> rule
(** [read_rule ?nested r] reads a style rule; with [~nested:true] its prelude is
    a relative selector list. *)

val read : Cursor.t -> stylesheet
(** [read r] reads a whole stylesheet, raising on the first invalid statement.
*)
