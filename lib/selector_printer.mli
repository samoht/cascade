(** Selector printing: escaping, attribute quoting and the minified spellings
    CSS Selectors 4 makes equivalent. *)

open Selector_intf

val pp_attr_flag : attr_flag option Pp.t
(** [pp_attr_flag] prints an optional attribute flag. *)

val attr_value_needs_quoting : string -> bool
(** [attr_value_needs_quoting value] is whether [value] is not a CSS ident and
    so must stay quoted. *)

val pp_attribute_match : attribute_match Pp.t
(** [pp_attribute_match] prints an attribute matcher. *)

val pp_aria_attr : aria_attr Pp.t
(** [pp_aria_attr] prints an ARIA attribute name. *)

val pp_attr_name : attr_name Pp.t
(** [pp_attr_name] prints an attribute name. *)

val pp_nth : nth Pp.t
(** [pp_nth] prints an An+B expression, in its shorter spelling when minified.
*)

val pp_combinator : combinator Pp.t
(** [pp_combinator] prints a combinator. *)

val pp_ns : ns Pp.t
(** [pp_ns] prints a namespace prefix. *)

val pp : t Pp.t
(** [pp] prints a selector. *)
