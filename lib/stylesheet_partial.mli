(** Typed validation of the rules section 5.3 recovery hands over, one rule at a
    time, so a rule the reader refuses becomes a warning instead of failing the
    sheet. *)

open Stylesheet_intf

type reading
(** The state of a typed read that takes the rules one at a time. *)

val reading : ?source:Error.Source.t -> ?meta:Loc.meta_level -> unit -> reading
(** [reading ?source ?meta ()] is a read that has seen no rule yet. *)

val read_next : reading -> Component.rule -> statement option
(** [read_next t rule] validates [rule], the next rule of the sheet, to a
    statement, or is [None] when it is dropped with a warning. *)

val reading_warnings : reading -> Error.t list
(** [reading_warnings t] is every warning [t] has recorded, in source order. *)

val read_stylesheet_of_rules :
  ?source:Error.Source.t ->
  ?meta:Loc.meta_level ->
  Component.rule list ->
  stylesheet * Error.t list
(** [read_stylesheet_of_rules ?source ?meta rules] validates each of [rules] to
    a statement, dropping a refused one with a warning. *)
