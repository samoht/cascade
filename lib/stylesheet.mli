(** CSS stylesheet types and construction functions *)

open Declaration
include module type of Stylesheet_intf

(** {1 Accessors} *)

val stylesheet_rules : t -> rule list
(** [stylesheet_rules t] returns the top-level rules of the stylesheet *)

val stylesheet_layers : t -> layer_rule list
(** [stylesheet_layers t] returns the layers of the stylesheet *)

val stylesheet_media_queries : t -> media_rule list
(** [stylesheet_media_queries t] returns the media queries of the stylesheet *)

val stylesheet_container_queries : t -> container_rule list
(** [stylesheet_container_queries t] returns the container queries of the
    stylesheet *)

(** {1 Creation} *)

val rule : selector:Selector.t -> declaration list -> rule
(** [rule ~selector declarations] creates a CSS rule *)

val selector : rule -> Selector.t
(** [selector rule] returns the selector of a rule *)

val declarations : rule -> declaration list
(** [declarations rule] returns the declarations of a rule *)

val media : condition:string -> rule list -> media_rule
(** [media ~condition rules] creates a media query rule *)

val media_condition : media_rule -> string
(** [media_condition media] returns the condition of a media rule *)

val media_rules : media_rule -> rule list
(** [media_rules media] returns the rules within a media query *)

val supports : condition:string -> rule list -> supports_rule
(** [supports ~condition rules] creates a supports query rule *)

val supports_nested :
  condition:string -> rule list -> supports_rule list -> supports_rule
(** [supports_nested ~condition rules nested] creates a nested supports rule *)

val container : ?name:string -> condition:string -> rule list -> container_rule
(** [container ?name ~condition rules] creates a container query rule *)

val property :
  syntax:string ->
  ?initial_value:string ->
  ?inherits:bool ->
  string ->
  property_rule
(** [property ~syntax ?initial_value ?inherits name] creates a [@property] rule
*)

val property_rule_name : property_rule -> string
(** [property_rule_name r] returns the name of a property rule *)

val property_rule_initial : property_rule -> string option
(** [property_rule_initial r] returns the initial value of a property rule *)

val default_decl_of_property_rule : property_rule -> declaration
(** [default_decl_of_property_rule r] creates a default declaration for a
    property rule *)

val rule_to_nested : rule -> nested_rule
(** [rule_to_nested rule] converts a rule to a nested rule *)

val supports_to_nested : supports_rule -> nested_rule
(** [supports_to_nested supports] converts a supports rule to a nested rule *)

val layer :
  name:string ->
  ?media:media_rule list ->
  ?container:container_rule list ->
  ?supports:supports_rule list ->
  nested_rule list ->
  layer_rule
(** [layer ~name ?media ?container ?supports rules] creates a layer rule *)

val layer_name : layer_rule -> string
(** [layer_name layer] returns the name of a layer *)

val layer_rules : layer_rule -> nested_rule list
(** [layer_rules layer] returns the nested rules of a layer *)

val empty : t
(** [empty] is an empty stylesheet *)

val concat : t list -> t
(** [concat stylesheets] concatenates multiple stylesheets *)

val stylesheet : sheet_item list -> t
(** [stylesheet items] creates a stylesheet from a list of items *)

val stylesheet_items : t -> sheet_item list
(** [stylesheet_items t] converts a stylesheet to a list of items *)
