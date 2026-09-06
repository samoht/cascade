(** Resolve a stylesheet against the nodes of a tree: selector matching and the
    cascade, decoupled from any particular DOM.

    A caller supplies its node type through {!NODE}; {!Make} then matches
    selectors and resolves the winning declarations per node. Nesting is
    flattened as by {!Css.flatten_nesting}, so a stylesheet produced by the
    optimiser resolves the same as the authored form. *)

module type NODE = sig
  type t

  val equal : t -> t -> bool
  (** Node identity, used to locate a node among its siblings. *)

  val name : t -> string option
  (** The element name (e.g. ["div"]), or [None] for an anonymous node. A name
      is read ASCII case-insensitively, against a type selector and between two
      siblings alike, so an anonymous node is of the same type as another
      anonymous node and of no named one. *)

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
      (** Neither answer is available. Most forms go unanswered for every node,
          and [:empty] under {!constructor-Browser} for some: see
          {!type-reading}. *)

(** Which reading of Selectors the matcher answers by.

    Two forms selectors-4 defines are implemented by no engine: sec. 6.3's [s]
    attribute flag, which engines refuse along with the rule carrying it, and
    sec. 13.2's [:empty] over an element holding nothing but document white
    space, the Level 4 change engines have not taken.

    The two are declined differently, because sec. 6.3 is about the selector and
    sec. 13.2 about the element. The [s] flag is declined for every node. Sec.
    13.2's note records that Level 2 and Level 3 matched an element with no
    children at all, which is what every engine still does, so the readings part
    over the white-space element alone: an element with no children of any kind
    is [:empty] to both and one holding an element or other text to neither, and
    {!constructor-Browser} answers those as {!constructor-Spec} does. *)
type reading =
  | Browser
      (** Decline what the specification and the engines answer differently. An
          answer there would be a fact about the specification, and {!Apply}
          inlines a rule out of the stylesheet while {!Prune} deletes it, so
          either would rewrite a page against the way its own browser renders
          it. *)
  | Spec  (** Answer them as selectors-4 defines them. *)

val supported : ?reading:reading -> Selector.t -> bool
(** [supported ?reading sel] is whether the matcher answers [sel] for every
    node, that is, whether {!Make.match_selector} is something other than
    {!constructor-Unsupported} whatever it is given. The answer is a fact about
    [sel] alone, which is why it needs no node, and it is the conservative half
    of the question: [false] says some node goes unanswered, not that this one
    does. A caller deciding per rule, before it has a node, wants exactly that;
    one holding a node wants {!Make.match_selector}, which under
    {!constructor-Browser} answers [:empty] for every element but the one the
    readings part over. [reading] defaults to {!constructor-Browser}.

    Modelled: the universal, type, class, id and attribute selectors, each
    without a namespace, an attribute carrying the [i] case flag or none;
    [:root] and [:scope], which name the same element here (selectors-4 sec.
    8.4) since no scoping root is ever handed to the matcher; the child-indexed
    [:first-child], [:last-child], [:only-child], [:nth-child()] and
    [:nth-last-child()], the last two with or without their [of S] argument; the
    typed [:first-of-type], [:last-of-type], [:only-of-type], [:nth-of-type()]
    and [:nth-last-of-type()]; [:has()]; the descendant, child and sibling
    combinators; and [:is()], [:where()], [:not()] and selector lists over those
    \- a list is only as modelled as its least modelled branch, and so is an
    [of S] or a [:has()] argument.

    Modelled under {!constructor-Spec} alone: [:empty] and an attribute carrying
    the [s] case flag, the two forms {!type-reading} is about.

    Not modelled, and for two different reasons. Some forms would need a {!NODE}
    to carry more than a tree of named elements: anything with a namespace,
    which no accessor here reports, and [:lang()], whose content language the
    document language defines rather than the element tree - HTML derives it
    from a [lang] attribute but also from a [meta] pragma and from the
    transport, so reading the attribute alone would answer
    {!constructor-No_match} for a document that tags its language elsewhere. The
    rest are outside any tree: every stateful pseudo-class, whether it needs the
    user ([:hover], [:focus], [:active], [:focus-visible], [:focus-within]), the
    document's history ([:visited], [:link], [:target]), or a form or media
    element's own state ([:checked], [:indeterminate], [:default], [:playing],
    [:paused], [:muted], [:open], [:closed]); every pseudo-element, which names
    no element at all; the shadow-tree and column forms ([:host], [::part()],
    [::slotted()], [||], [>>>], [/deep/]); and the nesting selector [&], which
    {!prepare} resolves away before the matcher sees it. Two forms are simply
    not selectors and are refused as such: [:nth-of-type(An+B of S)], which sec.
    13.4.1 does not define, and an attribute presence test carrying a case flag,
    which sec. 16's grammar admits only after a matcher and a value. *)

val layer_order : Stylesheet.t -> string list
(** [layer_order sheet] is the cascade layer order [sheet] declares, weakest
    first, as one dotted path per layer: [a.b] is the sublayer [b] of [a],
    however it was written ([@layer a.b] or [@layer a { @layer b }]). Each ident
    of a path carries the escapes that read it back (css-syntax-3 sec. 2.1), so
    the layer named [a.b] is the path [a\.b] and stays apart from the sublayer
    [a.b]. Sibling layers come in order of first appearance, each sublayer
    inside its parent's run (css-cascade-5 sec. 6.4.2) and the parent itself at
    the end of that run, since its own rules sort after every rule in a sublayer
    (sec. 6.4.3). An [@layer a, b;] statement declares its names there just as a
    block does. Every anonymous [@layer { ... }] block is a layer of its own,
    keyed by a path holding a U+0000 that no author can write - a caller that
    prints these paths has to spell those out itself. The layers counted are
    those {!Make.resolve} ranks against, so a layer declared inside one of the
    blocks it does not walk - a conditional group rule ([@media], [@supports],
    [@container], [@-moz-document], [@when], [@else]), [@starting-style],
    [@scope], or an origin wrapper - is not part of this order.

    This is the [~layer_order] that {!Stylesheet.cascade_layer_precedence_rank}
    expects. *)

type prepared
(** A stylesheet with everything {!Make.resolve} can settle without a node
    worked out: the flattened rules with their layers, and the layer order the
    cascade ranks by. *)

val prepare : Stylesheet.t -> prepared
(** [prepare sheet] is [sheet] with its nesting flattened and its rules bucketed
    by layer. A caller resolving many nodes against one sheet prepares it once
    and passes the result to {!Make.resolve_prepared}, rather than paying for
    the flattening per node. *)

module Make (N : NODE) : sig
  val match_selector : ?reading:reading -> Selector.t -> N.t -> match_result
  (** [match_selector ?reading sel node] is what the matcher can say about [sel]
      and [node]. [supported ?reading sel] holding means this never answers
      {!constructor-Unsupported}, for any [node]; it failing means some node
      goes unanswered, which under {!constructor-Browser} is the [:empty] one
      the readings part over and need not be this [node]. [reading] defaults to
      {!constructor-Browser}. *)

  val matches : ?reading:reading -> Selector.t -> N.t -> bool
  (** [matches ?reading sel node] is whether [sel] is known to match [node],
      that is, whether {!match_selector} is [Matches]. It folds [Unsupported] in
      with [No_match], so a caller that has to tell the two apart wants
      {!match_selector}. *)

  val resolve_prepared :
    ?reading:reading -> prepared -> N.t -> Declaration.declaration list
  (** [resolve_prepared ?reading prepared node] is {!resolve} against an already
      {!prepare}d sheet. Same result, without redoing the sheet-only work per
      node. *)

  val resolve :
    ?reading:reading -> Stylesheet.t -> N.t -> Declaration.declaration list
  (** [resolve ?reading sheet node] is [resolve_prepared (prepare sheet) node]:
      the declarations that win for [node] after flattening nesting and applying
      the cascade: selector matching, [!important] over normal, then cascade
      layer, specificity and source order. [@layer] blocks and [@layer a, b;]
      statements order the layers by first appearance, sublayers included and
      each parent behind its own sublayers; among normal declarations the last
      layer wins and an unlayered declaration beats them all, and for
      [!important] declarations that order reverses.

      Style rules and [@layer] are the only blocks walked. A rule inside a
      conditional group rule ([@media], [@supports], [@container],
      [@-moz-document], [@when], [@else]) contributes nothing, since the
      condition needs a viewport, a UA feature table, a container layout or a
      document URL that a {!NODE} does not carry. Nor does one inside
      [@starting-style], which declares a before-change style rather than an
      ordinary one (css-transitions-2 sec. 5); inside [@scope], whose scoping
      root, scoping limit and proximity criterion (css-cascade-6) the matcher
      does not model; or inside an origin wrapper, whose origin outranks the
      layer in the cascade sorting order (css-cascade-5 sec. 6.1) and is not
      weighed here. {!Apply.Make} keeps each of those blocks whole in the
      stylesheet it emits rather than projecting it onto an element. *)
end
