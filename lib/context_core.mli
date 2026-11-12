(** Context constructors and lookups: the values {!Context} re-exports, and the
    cascade-layer queries every evaluator part reads the context through. *)

open Context_intf

(** {1 Constructors} *)

val equal_property_registration :
  property_registration -> property_registration -> bool
(** [equal_property_registration a b] tests registrations structurally. *)

val empty : t
(** [empty] is the context with every field empty or [None]. *)

val v :
  ?custom_properties:Declaration.declaration list ->
  ?runtime_vars:string list ->
  ?inherited_values:Declaration.declaration list ->
  ?initial_values:Declaration.declaration list ->
  ?layer_order:string list ->
  ?layer:string ->
  ?cascade_rules:cascade_rule list ->
  ?base_url:string ->
  ?root_font_size:Values.length ->
  ?parent_font_size:Values.length ->
  ?current_color:Values.color ->
  ?viewport_width:Values.length ->
  ?viewport_height:Values.length ->
  ?container_width:Values.length ->
  ?container_height:Values.length ->
  unit ->
  t
(** [v ()] is {!empty} with each given field overridden. *)

val empty_document : document
(** [empty_document] is the document context with no element data. *)

val document :
  ?root:string ->
  ?scope:Selector.t ->
  ?element:string ->
  ?classes:string list ->
  ?ids:string list ->
  ?attributes:(string * string option) list ->
  ?pseudo_classes:string list ->
  ?pseudo_elements:string list ->
  unit ->
  document
(** [document ()] is {!empty_document} with each given field overridden. *)

val empty_query : query
(** [empty_query] is the query context exposing no feature. *)

val query :
  ?media_type:string ->
  ?media_features:Media.t list ->
  ?media_inapplicable:Media.name list ->
  ?supports:Supports.t list ->
  ?container_name:string ->
  ?container_features:Container.t list ->
  unit ->
  query
(** [query ()] is {!empty_query} with each given field overridden. *)

val empty_loader : loader
(** [empty_loader] is the loader with no base URL and no import. *)

val loader :
  ?base_url:string -> ?imports:(string * string) list -> unit -> loader
(** [loader ()] is {!empty_loader} with each given field overridden. *)

val empty_animation : animation
(** [empty_animation] is the animation context with no timeline data. *)

val animation :
  ?timeline_time:string ->
  ?progress:float ->
  ?animated_properties:string list ->
  unit ->
  animation
(** [animation ()] is {!empty_animation} with each given field overridden. *)

val empty_property_registry : property_registry
(** [empty_property_registry] is the registry with no registration. *)

val property_registration :
  ?initial_value:string ->
  inherits:bool ->
  string ->
  Variables.any_syntax ->
  property_registration
(** [property_registration ~inherits name syntax] is the registration of [name].

    @raise Invalid_argument if [name] is not a dashed custom-property name. *)

val property_registry :
  ?property_registrations:property_registration list ->
  unit ->
  property_registry
(** [property_registry ()] is the registry holding the given registrations. *)

(** {1 Cascade lookups} *)

val custom_property : string -> t -> Declaration.declaration option
(** [custom_property name ctx] is the first custom-property declaration of [ctx]
    named [name]. *)

val inherited_value : string -> t -> Declaration.declaration option
(** [inherited_value property ctx] is the inherited declaration of [property] in
    [ctx]. *)

val initial_value : string -> t -> Declaration.declaration option
(** [initial_value property ctx] is the initial declaration of [property] in
    [ctx]. *)

val cascade_rule_chain :
  t -> property_name:string -> important:bool -> cascade_rule list option
(** [cascade_rule_chain ctx ~property_name ~important] is the rules for
    [property_name] that [revert-layer] can roll back to from the current layer,
    strongest first. [None] when [ctx] has no cascade table and no layer scope.
*)

val remove_cascade_rule : cascade_rule -> cascade_rule list -> cascade_rule list
(** [remove_cascade_rule rule rules] is [rules] without the rule at the source
    order of [rule]. *)

val scope : ?layer_order:string list -> ?layer:string -> t -> t
(** [scope ?layer_order ?layer ctx] is [ctx] with the given layer order and
    layer, when given. *)

val lookup_custom_property :
  ?layer:string ->
  ?layer_order:string list ->
  t ->
  string ->
  Declaration.declaration option
(** [lookup_custom_property ctx name] is the cascade winner among the
    declarations of [--name] in [ctx], per CSS Cascade 5 sec. 6.4.3. [None] when
    the scoped layer is unknown or nothing wins. *)

val winning_custom_declaration :
  layer_order:string list ->
  Declaration.declaration list ->
  Declaration.declaration option
(** [winning_custom_declaration ~layer_order decls] is the cascade winner among
    same-property custom-property declarations, each read in its own layer. *)

(** {1 Query, document and loader lookups} *)

val media_feature_value : string -> Media.t -> Media.value option
(** [media_feature_value name m] is the value [m] pins feature [name] to. *)

val media_feature : string -> query -> Media.value option
(** [media_feature name q] is the plain media-feature value named [name] in [q].
*)

val container_feature : string -> query -> Media.value option
(** [container_feature name q] is the plain container size-feature value named
    [name] in [q]. *)

val has_class : string -> document -> bool
(** [has_class name doc] is whether [doc] lists class [name]. *)

val has_id : string -> document -> bool
(** [has_id name doc] is whether [doc] lists id [name]. *)

val attribute : string -> document -> string option option
(** [attribute name doc] is [Some value] when [doc] carries attribute [name]
    with [value], which is [None] for a valueless attribute. *)

val import_source : string -> loader -> string option
(** [import_source url l] is the stylesheet text [l] maps [url] to. *)

val animates_property : string -> animation -> bool
(** [animates_property property a] is whether [a] animates [property]. *)

val registered_property :
  string -> property_registry -> property_registration option
(** [registered_property name registry] is the registration of [name]. *)

val validate_registered_custom_property :
  property_registry -> Declaration.declaration -> (unit, string) result
(** [validate_registered_custom_property registry decl] checks [decl] against
    the syntax [registry] registers for it; unregistered custom properties pass.
*)
