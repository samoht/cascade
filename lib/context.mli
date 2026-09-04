(** Explicit closed contexts for CSS AST/value transforms.

    Context records carry the surrounding-document information that a CSS
    transform needs to resolve relative references like [var(--gap)], [inherit],
    [initial], [rem]/[em], [currentColor], or viewport units. Construct with
    {!v} (all fields optional) or start from {!empty}. *)

type cascade_rule = {
  property_name : string;
  important : bool;
  layer : string option;
  source_order : int;
  declaration : Declaration.declaration;
}
(** One observable declaration in the cascade table. *)

type t = {
  custom_properties : Declaration.declaration list;
      (** Custom property declarations, e.g. [--gap: 1rem]. *)
  runtime_vars : string list;
      (** Custom-property names (without the [--] prefix) kept as live [var()]
          references: a resolver treats them like a [~runtime] var, folding only
          value-independent simplifications (calc identities) and never
          collapsing the reference to a theme value. *)
  inherited_values : Declaration.declaration list;
      (** Parent/inherited property declarations. *)
  initial_values : Declaration.declaration list;
      (** Initial property declarations. *)
  layer_order : string list;  (** Known cascade layer order, low to high. *)
  layer : string option;  (** Current cascade layer, if evaluation is scoped. *)
  cascade_rules : cascade_rule list option;
      (** Optional cascade table for [revert-layer]. [None] means the observable
          is missing; [Some []] means it is known empty. *)
  base_url : string option;  (** Base URL for URL transforms. *)
  root_font_size : Values.length option;  (** Root font size resolving [rem]. *)
  parent_font_size : Values.length option;
      (** Parent font size resolving [em]. *)
  current_color : Values.color option;  (** Resolves [currentColor]. *)
  viewport_width : Values.length option;  (** Viewport-unit width. *)
  viewport_height : Values.length option;  (** Viewport-unit height. *)
  container_width : Values.length option;  (** Container-unit width. *)
  container_height : Values.length option;  (** Container-unit height. *)
}
(** Explicit context for CSS property-value transforms. *)

type document = {
  root : string option;
  scope : Selector.t option;
  element : string option;
  classes : string list;
  ids : string list;
  attributes : (string * string option) list;
  pseudo_classes : string list;
  pseudo_elements : string list;
}
(** Explicit context for selector-aware transforms. This is not a DOM tree; it
    is the closed information a selector transform is allowed to inspect. *)

type query = {
  media_type : string option;
  media_features : Media.t list;
      (** Media features the rendering environment claims to expose. Build
          entries with {!Media.val-feature} or {!Media.boolean}. *)
  media_inapplicable : Media.name list;
      (** Recognized features that cannot match any value in this environment.
          Their tests are false, unlike unknown features. *)
  supports : Supports.t list;
      (** Capability flags the rendering environment claims to support. Each
          entry is normally a [Supports.Property] or [Supports.Func] leaf, built
          with {!Supports.property} / {!Supports.func}. Compound forms ([And] /
          [Or] / [Not]) are accepted but only match a query that is structurally
          identical. *)
  container_name : string option;
  container_features : Container.t list;
      (** Container capabilities exposed by the matching container. Build size
          features with {!Container.feature}, style queries with
          {!Container.style}, and scroll-state queries with
          {!Container.scroll_state}. *)
}
(** Explicit context for media/supports/container query transforms. *)

type loader = { base_url : string option; imports : (string * string) list }
(** Explicit context for URL and [@import] transforms. {!field-imports} maps
    import URLs to stylesheet source text. *)

type animation = {
  timeline_time : string option;
  progress : float option;
  animated_properties : string list;
}
(** Explicit context for animation/keyframe transforms. *)

type property_registration = {
  name : string;
  syntax : Variables.any_syntax;
  inherits : bool;
  initial_value : string option;
}
(** Explicit context entry for a registered custom property. This models the
    parser-visible data from an [@property] rule without creating live CSSOM
    registration state. *)

val equal_property_registration :
  property_registration -> property_registration -> bool
(** [equal_property_registration a b] tests registrations structurally. *)

type property_registry = { property_registrations : property_registration list }
(** Explicit context for registration-aware custom property validation. *)

val empty : t
(** A context with no known custom properties, inherited values, dimensions,
    color, font data, or URL base. *)

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
(** [v ()] returns a context. Each labelled argument overrides the corresponding
    field; defaults are empty/[None]. *)

val empty_document : document
(** Empty selector/document context. *)

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
(** [document ()] constructs a selector/document context. *)

val empty_query : query
(** Empty query context. *)

val query :
  ?media_type:string ->
  ?media_features:Media.t list ->
  ?media_inapplicable:Media.name list ->
  ?supports:Supports.t list ->
  ?container_name:string ->
  ?container_features:Container.t list ->
  unit ->
  query
(** [query ()] constructs a media/supports/container context. Build
    {!field-supports} entries with {!Supports.property} and {!Supports.func}. *)

val empty_loader : loader
(** Empty URL/import-loader context. *)

val loader :
  ?base_url:string -> ?imports:(string * string) list -> unit -> loader
(** [loader ()] constructs a URL/import-loader context. *)

val empty_animation : animation
(** Empty animation context. *)

val animation :
  ?timeline_time:string ->
  ?progress:float ->
  ?animated_properties:string list ->
  unit ->
  animation
(** [animation ()] constructs an animation context. *)

val empty_property_registry : property_registry
(** Empty registered-property context. *)

val property_registration :
  ?initial_value:string ->
  inherits:bool ->
  string ->
  Variables.any_syntax ->
  property_registration
(** [property_registration name syntax ~inherits ?initial_value] describes a
    registered custom property. [name] must be a dashed custom-property name. *)

val property_registry :
  ?property_registrations:property_registration list ->
  unit ->
  property_registry
(** [property_registry ()] constructs a registered-property context. *)

(** {1 Pretty-printing}

    Debug-style record dumps, intended for inspection and logging. They are not
    CSS source. *)

val pp : t Pp.t
(** [pp] dumps a property-value context. *)

val pp_document : document Pp.t
(** [pp_document] dumps a document context. *)

val pp_query : query Pp.t
(** [pp_query] dumps a query context. *)

val pp_loader : loader Pp.t
(** [pp_loader] dumps a loader context. *)

val pp_animation : animation Pp.t
(** [pp_animation] dumps an animation context. *)

val pp_property_registry : property_registry Pp.t
(** [pp_property_registry] dumps a registered-property context. *)

(** {1 Lookups} *)

val custom_property : string -> t -> Declaration.declaration option
(** [custom_property name ctx] is the custom property declaration named [name]
    in [ctx], if any. *)

val winning_custom_declaration :
  layer_order:string list ->
  Declaration.declaration list ->
  Declaration.declaration option
(** [winning_custom_declaration ~layer_order decls] is the cascade winner among
    same-property custom-property declarations per CSS Cascade 5 sec. 6.4.3:
    [!important] beats normal, [revert-layer] rolls back a layer, and layer
    order (reversed for [!important]) breaks ties. Each declaration's layer is
    read from its own annotation. [None] when [decls] is empty or resolves to
    unset. *)

val inherited_value : string -> t -> Declaration.declaration option
(** [inherited_value property ctx] is the inherited declaration for [property]
    in [ctx], if any. *)

val initial_value : string -> t -> Declaration.declaration option
(** [initial_value property ctx] is the initial declaration for [property] in
    [ctx], if any. *)

val media_feature : string -> query -> Media.value option
(** [media_feature name ctx] is the plain media feature value named [name] in
    [ctx], if any. Boolean and range media features do not produce a value. *)

val container_feature : string -> query -> Media.value option
(** [container_feature name ctx] is the plain container size feature value named
    [name] in [ctx], if any. Style, scroll-state, boolean, and range container
    features do not produce a value. *)

val has_class : string -> document -> bool
(** [has_class name ctx] checks [ctx.classes]. *)

val has_id : string -> document -> bool
(** [has_id name ctx] checks [ctx.ids]. *)

val attribute : string -> document -> string option option
(** [attribute name ctx] looks up an attribute. [None] means absent; [Some None]
    means present without a value. *)

val eval :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Declaration.declaration ->
  Declaration.declaration
(** [eval ?layer_order ?layer ctx decl] abstract-interprets [decl] against [ctx]
    using typed AST walkers. It returns a more-defined declaration in the same
    CSS AST, preserving unresolved subtrees as residual syntax. *)

val matches_selector : document -> Selector.t -> bool
(** [matches_selector doc sel] tests whether [sel] would match an element
    described by [doc]. Approximate: classes/ids/attributes/element name are
    checked but the document is not a tree. *)

val matches_media : query -> Media.t -> bool
(** [matches_media q m] evaluates [m] against [q.media_features]. Missing values
    remain unknown through boolean operators, unless the feature is explicitly
    {!field-media_inapplicable}. Only a true final result matches. An empty
    media list matches. *)

val matches_supports : query -> Supports.t -> bool
(** [matches_supports q cond] evaluates [cond] against [q.supports_table]. *)

val matches_container : query -> ?name:string -> Container.t -> bool
(** [matches_container q ?name cond] evaluates [cond] against the container
    query state in [q]. The supplied container must expose every size or
    scroll-state feature used by the condition before boolean evaluation. *)

val resolve_url : loader -> string -> (string, string) result
(** [resolve_url loader href] resolves [href] against
    {!type-loader.field-base_url} according to RFC 3986. *)

val import_source : string -> loader -> string option
(** [import_source url ctx] looks up imported stylesheet text. *)

val animates_property : string -> animation -> bool
(** [animates_property property ctx] checks [ctx.animated_properties]. *)

val load_import :
  ?query:query ->
  ?layer_order:string list ->
  loader ->
  Stylesheet.import_rule ->
  (Stylesheet.t, string) result
(** [load_import ?query ?layer_order loader rule] loads the stylesheet
    referenced by [rule] from {!type-loader.field-imports}, applying any
    [@import] media/supports/layer guards. Returns [Error] if the import path
    isn't in {!type-loader.field-imports} or a guard rejects it. *)

val registered_property :
  string -> property_registry -> property_registration option
(** [registered_property name registry] is the registration for [name], if any.
*)

val validate_registered_custom_property :
  property_registry -> Declaration.declaration -> (unit, string) result
(** [validate_registered_custom_property registry decl] validates a custom
    property declaration against [registry] when the property is registered.
    Unregistered custom properties are accepted. Non-custom declarations and
    registered values that do not match the registered syntax return [Error]. *)
