(** Selector reading: the CSS Selectors 4 grammar over a {!Cursor.t}, and
    {!of_string} with its single-token fallback. *)

open Selector_intf

val read_aria_attr : Cursor.t -> aria_attr
(** [read_aria_attr t] parses an [aria_attr] from [t]. *)

val read_attr_name : Cursor.t -> attr_name
(** [read_attr_name t] parses an [attr_name] from [t]. *)

val read_ns : Cursor.t -> ns option
(** [read_ns t] parses an optional attribute or selector namespace. *)

val read_combinator : Cursor.t -> combinator
(** [read_combinator t] parses a combinator. *)

val read_attribute_match : Cursor.t -> attribute_match
(** [read_attribute_match t] parses an attribute matcher. *)

val read_attr_flag : Cursor.t -> attr_flag option
(** [read_attr_flag t] parses an attribute selector flag ([i] or [s]). *)

val read_nth : Cursor.t -> nth
(** [read_nth t] parses an An+B expression. *)

val read_nth_selector : Cursor.t -> nth * t list option
(** [read_nth_selector t] parses an An+B expression with an optional [of S]. *)

val read_selector_list : Cursor.t -> t
(** [read_selector_list t] reads a selector list without checking for the end of
    input. *)

val read_strict_selector_list : Cursor.t -> t
(** [read_strict_selector_list t] reads a non-forgiving selector list. *)

val read : Cursor.t -> t
(** [read t] parses a whole CSS selector. *)

val read_relative : Cursor.t -> t
(** [read_relative t] parses a relative selector, which may start with a
    combinator. *)

val drop_redundant_nesting_prefix : t -> t
(** [drop_redundant_nesting_prefix sel] removes a leading [&] that is the whole
    left operand of a combinator (CSS Nesting 1 sec. 3). *)

val of_string : string -> t
(** [of_string s] is {!val-read} over [s], or the class, ID or type selector [s]
    spells as one name when the reader rejects it. *)
