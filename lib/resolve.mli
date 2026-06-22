(** Resolve a stylesheet against the nodes of a tree: selector matching and the
    cascade, decoupled from any particular DOM.

    A caller supplies its node type through {!NODE}; {!Make} then matches
    selectors and resolves the winning declarations per node. Nesting is
    flattened with {!Flatten}, so a stylesheet produced by the optimiser
    resolves the same as the authored form. *)

module type NODE = sig
  type t

  val equal : t -> t -> bool
  (** Node identity, used to locate a node among its siblings. *)

  val name : t -> string option
  (** The element name (e.g. ["div"]), or [None] for an anonymous node. *)

  val id : t -> string option
  (** The [id] attribute, or [None]. *)

  val classes : t -> string list
  (** The class names from the [class] attribute. *)

  val attribute : t -> string -> string option
  (** [attribute t name] is the value of attribute [name], or [None]. *)

  val parent : t -> t option
  (** The parent element, or [None] at the root. *)

  val children : t -> t list
  (** The child elements, in document order. *)
end

module Make (N : NODE) : sig
  val matches : Selector.t -> N.t -> bool
  (** [matches sel node] is whether [sel] matches [node]. Stateful and generated
      forms ([:hover], [::before], unknown pseudo-classes, ...) never match. *)

  val resolve : Stylesheet.t -> N.t -> Declaration.declaration list
  (** [resolve sheet node] is the declarations that win for [node] after
      flattening nesting and applying the cascade: selector matching,
      specificity, source order, then [!important] over normal. Conditional and
      at-rule groups ([@media], [@layer], ...) are not considered. *)
end
