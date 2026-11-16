(** The scope a custom property is seen in: the selector cover between a
    definition and a consumer, the chain of at-rule wrappers a statement sits
    inside, and the first pass that records every rule's custom declarations
    with their scope. *)

val effective_selector : parents:Selector.t list -> Selector.t -> Selector.t
(** [effective_selector ~parents sel] is [sel] with [&] and nesting resolved
    against the enclosing rule selectors [parents], innermost first. *)

val selector_covers : ancestor:Selector.t -> consumer:Selector.t -> bool
(** [selector_covers ~ancestor ~consumer] is [true] when a custom property
    declared on [ancestor] reaches [consumer]: [ancestor] is a universal
    selector or a static descendant prefix of [consumer]. *)

(** One at-rule wrapper a statement sits inside. *)
type at_node =
  | Media of Media.t
  | Layer of Stylesheet.layer_name option
  | Supports of Supports.t
  | Moz_document of Stylesheet.moz_document_condition list
  | Container of string option * Container.t option
  | Starting_style
  | When of Stylesheet.conditional
  | Else of Stylesheet.conditional option
  | Origin of Stylesheet.cascade_origin
  | Scope of Selector.t option * Selector.t option

val at_path_prefix : outer:at_node list -> inner:at_node list -> bool
(** [at_path_prefix ~outer ~inner] is [true] when a definition under the at-rule
    path [outer] is visible to a consumer under [inner]. Paths are innermost
    first and ignore [\@layer] wrappers. *)

val at_wrapper :
  Stylesheet.statement ->
  (at_node * Stylesheet.t * (Stylesheet.t -> Stylesheet.statement)) option
(** [at_wrapper stmt] is the at-rule node [stmt] wraps, its body, and the
    function rebuilding [stmt] around a new body, or [None] when [stmt] is not
    such a wrapper. *)

type scope = {
  at_path : at_node list;
  selector : Selector.t;
  customs : Declaration.declaration list;
}
(** The custom declarations one rule makes, with its at-rule path and effective
    selector. *)

type scope_set = { all : scope list; declared : (string, unit) Hashtbl.t }
(** Every scope of a stylesheet, and the custom names it declares anywhere. *)

val custom_name : Declaration.declaration -> string option
(** [custom_name d] is the name [d] declares when it is a custom property. *)

val rebuild_custom :
  ?layer:string -> string -> string -> Declaration.declaration option
(** [rebuild_custom ?layer name value] is the custom-property declaration of
    [name] with [value], or [None] when [value] cannot be written back. *)

val collect_scopes : kept:string list -> Stylesheet.t -> scope_set
(** [collect_scopes ~kept stylesheet] records the scope of every rule in
    [stylesheet] that declares a custom property, and a [\@property] initial
    value as a declaration on [*]. A property named in [kept] is left out. *)
