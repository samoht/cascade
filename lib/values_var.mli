(** [var()], [env()] and [attr()] references, and preserved component values. *)

open Values_intf

val var_ref :
  ?fallback:'a fallback ->
  ?default:'a ->
  ?layer:string ->
  ?meta:meta ->
  ?runtime:bool ->
  string ->
  'a var
(** [var_ref ?fallback ?default ?layer ?meta ?runtime name] creates a CSS
    variable reference to [--name]. *)

val syntax_fallback : string -> 'a fallback
(** [syntax_fallback s] parses [s] as a CSS declaration-value fallback for a
    [var()] reference. *)

val string_of_number_percentage : number_percentage -> string
(** [string_of_number_percentage value] serializes a number/percentage value for
    CSS custom-property initial values. *)

val string_of_syntax_fallback : Pp.ctx -> Component.t list -> string
(** [string_of_syntax_fallback ctx v] is the spelling of the [var()] fallback
    stream [v] under [ctx]. *)

val first_top_level_comma_segment : string -> string
(** [first_top_level_comma_segment s] is [s] up to its first comma outside
    parentheses and quotes. *)

val pp_var : 'a Pp.t -> 'a var Pp.t
(** [pp_var pp] pretty-prints CSS variables using [pp] for the payload. *)

val pp_env : 'a Pp.t -> 'a env Pp.t
(** [pp_env pp] pretty-prints an [env()] reference using [pp] for the fallback.
*)

val pp_component_values : Pp.ctx -> Component.t list -> unit
(** [pp_component_values] pretty-prints preserved component values. *)

val read_component_values : Cursor.t -> Component.t list
(** [read_component_values t] returns the remaining component values from [t].
*)

val pp_invalid_value : Pp.ctx -> Component.t list -> unit
(** [pp_invalid_value] pretty-prints a preserved invalid value. *)

val read_invalid_value : Cursor.t -> Component.t list
(** [read_invalid_value t] reads a preserved invalid value. *)

val pp_attr_syntax : Pp.ctx -> attr_syntax -> unit
(** [pp_attr_syntax] pretty-prints an [attr()] type() syntax keyword. *)

val pp_attr_type : Pp.ctx -> attr_type -> unit
(** [pp_attr_type] pretty-prints an [attr()] type hint. *)

val pp_attr_call : 'a Pp.t -> 'a attr_call Pp.t
(** [pp_attr_call pp] pretty-prints an [attr()] call using [pp] for typed
    fallback values. *)

val attr_syntax_of_string : string -> attr_syntax option
(** [attr_syntax_of_string s] is the [attr()] [type()] syntax spelled [s]. *)

val read_attr_syntax : Cursor.t -> attr_syntax
(** [read_attr_syntax] reads an [attr()] type() syntax keyword. *)

val read_attr_type : Cursor.t -> attr_type
(** [read_attr_type] reads an [attr()] type hint. *)

val read_var_body : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var_body read t] parses a [var()] reference body -- the contents of a
    [var(...)] form without the surrounding [var(] and [)] -- using [read] for
    the fallback's payload. *)

val read_var : (Cursor.t -> 'a) -> Cursor.t -> 'a var
(** [read_var read t] parses a CSS variable with [var(...)] syntax using [read]
    for the payload. *)

val read_env : (Cursor.t -> 'a) -> Cursor.t -> 'a env
(** [read_env read t] parses an [env(...)] reference using [read] for the
    fallback. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without --). *)

val var_layer : 'a var -> string option
(** [var_layer v] is [v]'s optional layer name. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is [v]'s optional metadata. *)

val with_fallback : 'a var -> 'a -> 'a var
(** [with_fallback var_ref fallback_value] creates a new variable reference with
    the same variable name but a different fallback value. *)
