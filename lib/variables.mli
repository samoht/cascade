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

(** {1 Meta handling} *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] creates injection and projection functions for metadata. *)

(** {1 Variable creation} *)

val var :
  ?fallback:'a fallback ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?fallback ?layer ?meta name kind value] creates a CSS variable
    declaration and a typed variable handle. *)

(** {1 Variable extraction} *)

val vars_of_calc : 'a calc -> any_var list
(** [vars_of_calc calc] extracts all variables from a calc expression. *)

val vars_of_property : 'a property -> 'a -> any_var list
(** [vars_of_property prop value] extracts variables from a property value. *)

val vars_of_value : 'a kind -> 'a -> any_var list
(** [vars_of_value kind value] extracts variables from a typed value. *)

val vars_of_declarations : declaration list -> any_var list
(** [vars_of_declarations decls] extracts all unique variables from declarations
    . *)

val compare_vars_by_name : any_var -> any_var -> int
(** [compare_vars_by_name v1 v2] compares variables by name. *)

(** {1 Variable name utilities} *)

val any_var_name : any_var -> string
(** [any_var_name v] returns the CSS variable name with [--] prefix. *)

(** {1 Advanced variable extraction} *)

val extract_vars_from_prop_value : 'a property -> 'a -> any_var list
(** [extract_vars_from_prop_value prop value] extracts variables from property
    values. *)

val extract_vars_from_declaration : declaration -> any_var list
(** [extract_vars_from_declaration decl] extracts variables from a declaration .
*)

val analyze_declarations : declaration list -> any_var list
(** [analyze_declarations decls] finds all variable references in declarations .
*)

val extract_custom_declarations : declaration list -> declaration list
(** [extract_custom_declarations decls] filters only custom property
    declarations. *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] returns the variable name if it's a custom
    declaration. *)

val pp_any_syntax : any_syntax Pp.t
(** [pp_any_syntax] pretty-prints any CSS syntax descriptor. *)

val read_any_syntax : Reader.t -> any_syntax
(** [read_any_syntax t] parses a CSS syntax descriptor. *)

val pp_any_var : any_var Pp.t
(** [pp_any_var] pretty-prints any CSS variable. *)

val read_any_var : Reader.t -> any_var
(** [read_any_var t] parses a CSS variable reference. *)
