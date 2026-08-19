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

  val text_children : t -> string list
  (** The data of the node's direct child text nodes, in document order, and
      [[]] for an element that holds none. Only text counts: a comment or a
      processing instruction is not a text node and must not be reported here,
      since neither affects whether the element is [:empty] (selectors-4 sec.
      13.2). A backend that answers [[]] for an element that does hold text
      makes that element match [:empty]. *)
end

(** What the matcher can say about a selector and a node.

    Selectors 4 describes far more than a matcher with no document behind it can
    decide, so the negative answer is split in two. [Unsupported] is not "does
    not match": it says this library has no model for the selector, and a caller
    must not read it as the selector having been ruled out. *)
type match_result =
  | Matches  (** The selector matches the node. *)
  | No_match  (** The selector is modelled and does not match the node. *)
  | Unsupported
      (** The selector is outside what the matcher models, so neither answer is
          available, for any node. *)

val supported : Selector.t -> bool
(** [supported sel] is whether the matcher has a model for [sel], that is,
    whether {!Make.match_selector} answers it with something other than
    {!constructor-Unsupported}. The answer is a fact about [sel] alone, which is
    why it needs no node.

    Modelled: the universal, type, class, id and attribute selectors, each
    without a namespace and, for an attribute, without a case flag; [:root],
    [:empty], [:first-child], [:last-child], [:only-child]; the descendant,
    child and sibling combinators; and [:is()], [:where()], [:not()] and
    selector lists over those - a list is only as modelled as its least modelled
    branch. Everything else, every stateful pseudo-class and every
    pseudo-element among it, is not. *)

val layer_order : Stylesheet.t -> string list
(** [layer_order sheet] is the cascade layer order [sheet] declares, weakest
    first, as one dotted path per layer: [a.b] is the sublayer [b] of [a],
    however it was written ([@layer a.b] or [@layer a { @layer b }]). Layers
    come in order of first appearance with each sublayer inside its parent's run
    (css-cascade-5 sec. 6.4.2), and an [@layer a, b;] statement declares its
    names there just as a block does. Every anonymous [@layer { ... }] block is
    a layer of its own, keyed by a path holding a U+0000 that no author can
    write - a caller that prints these paths has to spell those out itself.
    Layers declared inside a conditional group are not counted, as
    {!Make.resolve} does not consider such groups either.

    This is the [~layer_order] that {!Stylesheet.cascade_layer_precedence_rank}
    expects. *)

module Make (N : NODE) : sig
  val match_selector : Selector.t -> N.t -> match_result
  (** [match_selector sel node] is what the matcher can say about [sel] and
      [node]. It is [Unsupported] exactly when [sel] is not {!supported}, for
      every [node]. *)

  val matches : Selector.t -> N.t -> bool
  (** [matches sel node] is whether [sel] is known to match [node], that is,
      whether {!match_selector} is [Matches]. It folds [Unsupported] in with
      [No_match], so a caller that has to tell the two apart wants
      {!match_selector}. *)

  val resolve : Stylesheet.t -> N.t -> Declaration.declaration list
  (** [resolve sheet node] is the declarations that win for [node] after
      flattening nesting and applying the cascade: selector matching,
      [!important] over normal, then cascade layer, specificity and source
      order. [@layer] blocks and [@layer a, b;] statements order the layers by
      first appearance, sublayers included; among normal declarations the last
      layer wins and an unlayered declaration beats them all, and for
      [!important] declarations that order reverses. Conditional groups
      ([@media], [@supports], [@container]) are not considered. *)
end
