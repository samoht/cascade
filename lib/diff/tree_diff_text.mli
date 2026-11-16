(** How statements and declarations read in the tree diff report and in the keys
    its comparison matches on. *)

open Cascade

val add_strings : Buffer.t -> string list -> unit
(** [add_strings buf ls] appends each string of [ls] to [buf] in order. *)

val decl_with_important : Css.declaration -> string -> string
(** [decl_with_important decl value] is [value] followed by [" !important"] when
    [decl] is important, [value] otherwise. *)

val decl_shown_value : Css.declaration -> string
(** [decl_shown_value decl] is the value of [decl] as the report shows it: the
    author's own spelling, with its [!important] flag. *)

val statement_text_split : Css.statement -> string * string
(** [statement_text_split stmt] is [(head, body)], the minified text of [stmt]
    cut at its block. [body] is [""] when [stmt] has no block. *)

val describe_statement : Css.statement -> string option
(** [describe_statement stmt] is the name the report gives [stmt]: its selector,
    its at-rule and condition, or the head it prints to. *)
