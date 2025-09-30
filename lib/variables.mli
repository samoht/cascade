(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include module type of Variables_intf

(** {1 Custom Property Support} *)

val pp_syntax : 'a syntax Pp.t
(** [pp_syntax] pretty-prints a syntax descriptor to a CSS syntax string. *)

val pp_value : 'a syntax -> 'a Pp.t
(** [pp_value syntax value] pretty-prints a value according to its syntax type.
*)

val read_syntax : Reader.t -> any_syntax
(** [read_syntax r] reads a CSS syntax descriptor from input. *)

val read_value : Reader.t -> 'a syntax -> 'a
(** [read_value r syntax] reads a value according to the given [syntax]. *)

(** {1 Meta handling} *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] creates injection and projection functions for metadata. *)

(** {1 Variable creation} *)

val var :
  ?default:'a ->
  ?fallback:'a fallback ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?default ?fallback ?layer ?meta name kind value] creates a CSS variable
    declaration with the given value and a typed variable handle. The value is
    used both for the declaration and as the default for inline mode (unless
    overridden by [?default]). *)

(** {1 Variable extraction} *)

val vars_of_calc : 'a calc -> any_var list
(** [vars_of_calc calc] extracts all variables from a calc expression. *)

val vars_of_property : 'a property -> 'a -> any_var list
(** [vars_of_property prop value] extracts variables from a property value. *)

val vars_of_declarations : declaration list -> any_var list
(** [vars_of_declarations decls] extracts all unique variables from declarations
    . *)

val compare_vars_by_name : any_var -> any_var -> int
(** [compare_vars_by_name v1 v2] compares variables by name. *)

(** {1 Variable name utilities} *)

val any_var_name : any_var -> string
(** [any_var_name v] returns the CSS variable name with [--] prefix. *)

(** {1 Advanced variable extraction} *)

val analyze_declarations : declaration list -> any_var list
(** [analyze_declarations decls] finds all variable references in declarations .
*)

val custom_declarations : ?layer:string -> declaration list -> declaration list
(** [custom_declarations ?layer decls] filters only custom property
    declarations. If [layer] is provided, only declarations from that layer are
    returned. *)

val extract_custom_declarations :
  ?layer:string -> declaration list -> declaration list
[@@deprecated "Use custom_declarations instead"]
(** @deprecated Use {!custom_declarations} instead. *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] returns the variable name if it's a custom
    declaration. *)

val pp_any_syntax : any_syntax Pp.t
(** [pp_any_syntax] pretty-prints any CSS syntax descriptor. *)

val read_any_syntax : Reader.t -> any_syntax
(** [read_any_syntax t] parses a CSS syntax descriptor. *)

val parse_var_reference : Reader.t -> string * string option
(** [parse_var_reference t] parses a CSS var() function and returns the variable
    name (without -- prefix) and optional fallback string. This is a lower-level
    function that doesn't create a variable handle. *)
