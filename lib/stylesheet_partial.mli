(** Typed validation of the rules section 5.3 recovery hands over, one rule at a
    time, so a rule the reader refuses becomes a warning instead of failing the
    sheet. *)

open Stylesheet_intf

val rule_loc : Component.rule -> Loc.t
(** [rule_loc rule] is the source range of [rule]. *)

val read_stylesheet_of_rules :
  ?source:string ->
  ?meta:Loc.meta_level ->
  Component.rule list ->
  stylesheet * Error.t list
(** [read_stylesheet_of_rules ?source ?meta rules] validates each of [rules] to
    a statement, dropping a refused one with a warning. *)
