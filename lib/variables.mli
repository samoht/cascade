(** CSS variables and variable extraction utilities *)

open Values
open Properties
open Declaration
include module type of Variables_intf

(** {1 Meta handling} *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] creates injection and projection functions for metadata *)

(** {1 Variable creation} *)

val var :
  ?fallback:'a ->
  ?layer:string ->
  ?meta:meta ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?fallback ?layer ?meta name kind value] creates a CSS variable
    declaration and a typed variable handle *)

(** {1 Variable extraction} *)

val vars_of_calc : 'a calc -> any_var list
(** [vars_of_calc calc] extracts all variables from a calc expression *)

val vars_of_property : 'a property -> 'a -> any_var list
(** [vars_of_property prop value] extracts variables from a property value *)

val vars_of_value : 'a kind -> 'a -> any_var list
(** [vars_of_value kind value] extracts variables from a typed value *)

val vars_of_declarations : declaration list -> any_var list
(** [vars_of_declarations decls] extracts all unique variables from declarations
*)

val compare_vars_by_name : any_var -> any_var -> int
(** [compare_vars_by_name v1 v2] compares variables by name *)

(** {1 Variable name utilities} *)

val any_var_name : any_var -> string
(** [any_var_name v] returns the CSS variable name with -- prefix *)

(** {1 Advanced variable extraction} *)

val extract_vars_from_prop_value : 'a property -> 'a -> any_var list
(** [extract_vars_from_prop_value prop value] extracts variables from property
    values *)

val extract_vars_from_declaration : declaration -> any_var list
(** [extract_vars_from_declaration decl] extracts variables from a declaration
*)

val analyze_declarations : declaration list -> any_var list
(** [analyze_declarations decls] finds all variable references in declarations
*)

val extract_custom_declarations : declaration list -> declaration list
(** [extract_custom_declarations decls] filters only custom property
    declarations *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] returns the variable name if it's a custom
    declaration *)

(** {1 Stylesheet variable extraction} *)

val vars_of_rules : Stylesheet.rule list -> any_var list
(** [vars_of_rules rules] extracts variables from CSS rules *)

val vars_of_media_queries : Stylesheet.media_rule list -> any_var list
(** [vars_of_media_queries media] extracts variables from media queries *)

val vars_of_container_queries : Stylesheet.container_rule list -> any_var list
(** [vars_of_container_queries container] extracts variables from container
    queries *)

val vars_of_stylesheet : Stylesheet.t -> any_var list
(** [vars_of_stylesheet ss] extracts all variables from a stylesheet *)
