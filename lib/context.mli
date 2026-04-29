(** Explicit closed contexts for CSS AST/value transforms.

    Context records carry the surrounding-document information that a CSS
    transform needs to resolve relative references like [var(--gap)], [inherit],
    [initial], [rem]/[em], [currentColor], or viewport units. Construct with
    {!v} (all fields optional) or start from {!empty}. *)

type t = {
  custom_properties : Declaration.declaration list;
      (** Custom property declarations, e.g. [--gap: 1rem]. *)
  inherited_values : Declaration.declaration list;
      (** Parent/inherited property declarations. *)
  initial_values : Declaration.declaration list;
      (** Initial property declarations. *)
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
  scope : string option;
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
  media_features : (string * string) list;
  supports_declarations : (string * string) list;
  supports_functions : (string * string) list;
  supports : Supports.t list;
      (** Capability flags the rendering environment claims to support. Each
          entry is normally a [Supports.Property] or [Supports.Func] leaf, built
          with {!Supports.property} / {!Supports.func}. Compound forms ([And] /
          [Or] / [Not]) are accepted but only match a query that is structurally
          identical. *)
  container_name : string option;
  container_features : (string * string) list;
}
(** Explicit context for media/supports/container query transforms. *)

type loader = { base_url : string option; imports : (string * string) list }
(** Explicit context for URL and [@import] transforms. [imports] maps import
    URLs to stylesheet source text. *)

type animation = {
  timeline_time : string option;
  progress : float option;
  animated_properties : string list;
}
(** Explicit context for animation/keyframe transforms. *)

val empty : t
(** A context with no known custom properties, inherited values, dimensions,
    color, font data, or URL base. *)

val v :
  ?custom_properties:Declaration.declaration list ->
  ?inherited_values:Declaration.declaration list ->
  ?initial_values:Declaration.declaration list ->
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
  ?scope:string ->
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
  ?media_features:(string * string) list ->
  ?supports_declarations:(string * string) list ->
  ?supports_functions:(string * string) list ->
  ?supports:Supports.t list ->
  ?container_name:string ->
  ?container_features:(string * string) list ->
  unit ->
  query
(** [query ()] constructs a media/supports/container context. Build [supports]
    entries with {!Supports.property} and {!Supports.func}. *)

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

(** {1 Lookups} *)

val custom_property : string -> t -> Declaration.declaration option
(** [custom_property name ctx] is the custom property declaration named [name]
    in [ctx], if any. *)

val inherited_value : string -> t -> Declaration.declaration option
(** [inherited_value property ctx] is the inherited declaration for [property]
    in [ctx], if any. *)

val initial_value : string -> t -> Declaration.declaration option
(** [initial_value property ctx] is the initial declaration for [property] in
    [ctx], if any. *)

val has_class : string -> document -> bool
(** [has_class name ctx] checks [ctx.classes]. *)

val has_id : string -> document -> bool
(** [has_id name ctx] checks [ctx.ids]. *)

val attribute : string -> document -> string option option
(** [attribute name ctx] looks up an attribute. [None] means absent; [Some None]
    means present without a value. *)

val media_feature : string -> query -> string option
(** [media_feature name ctx] looks up a media feature. *)

val computed_value :
  ?layer_order:string list ->
  ?layer:string ->
  t ->
  Declaration.declaration ->
  (string, string) result
(** [computed_value ?layer_order ?layer ctx decl] resolves the declaration
    against [ctx]: applies the CSS-wide keywords ([initial], [inherit],
    [unset]), expands [var(...)] references, evaluates [currentColor], and
    converts [rem]/[em]/viewport/container relative lengths into absolute pixels
    when the relevant base size is supplied. Returns [Error msg] when a
    referenced source is missing from the context. *)

val matches_selector : document -> Selector.t -> bool
(** [matches_selector doc sel] tests whether [sel] would match an element
    described by [doc]. Approximate: classes/ids/attributes/element name are
    checked but the document is not a tree. *)

val matches_media : query -> Media.t -> bool
(** [matches_media q m] evaluates [m] against [q.media_features]. *)

val matches_supports : query -> Supports.t -> bool
(** [matches_supports q cond] evaluates [cond] against [q.supports_table]. *)

val matches_container : query -> ?name:string -> Container.t -> bool
(** [matches_container q ?name cond] evaluates [cond] against the container
    query state in [q]. *)

val resolve_url : loader -> string -> (string, string) result
(** [resolve_url loader href] resolves [href] against [loader.base_url] using
    simple relative-URL handling. *)

val container_feature : string -> query -> string option
(** [container_feature name ctx] looks up a container feature. *)

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
    referenced by [rule] from [loader.imports], applying any [@import]
    media/supports/layer guards. Returns [Error] if the import path isn't in
    [loader.imports] or a guard rejects it. *)
