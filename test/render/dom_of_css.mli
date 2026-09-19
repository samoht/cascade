(** Synthesise an HTML document from the selectors of a stylesheet.

    Random markup matches almost nothing, so the differential test derives its
    document from the sheet under test: every selector is walked, and elements
    carrying the tags, classes, ids, attributes, nesting and sibling positions
    it asks for are added to the document. A selector that cannot hold without a
    user interaction ([:hover]), a shadow tree ([::part]) or a live document
    ([:target]) is not synthesised, but it is counted and reported - it is never
    silently dropped.

    The document is rendered, so every element that can hold text holds a word:
    a declaration on an empty box paints nothing. *)

type t
(** A synthesised document. *)

val of_stylesheet : ?max_elements:int -> Cascade.Css.t -> t
(** [of_stylesheet sheet] derives a document from the selectors of [sheet],
    including the selectors of nested rules and of rules inside [@media],
    [@layer], [@supports] and [@container]. Synthesis stops once the document
    reaches [max_elements] elements (default 4000); the selectors left over are
    counted under the ["document element cap"] reason. *)

val to_html : t -> string
(** [to_html t] is the document as HTML text, with a doctype, the classes and
    attributes the root [html] element must carry, and the element tree under
    [body]. The HTML parser rewrites some nesting the selectors asked for (a
    [td] outside a table, a child of a void element), so what a browser builds
    from it is what [Cascade_html.Html.parse] builds, not the tree derived; the
    probes say which selectors that costs. *)

val probes : t -> Cascade.Selector.t list
(** [probes t] is one selector per synthesised selector, its pseudo-elements
    stripped: the selectors the document built for, to be matched against the
    tree parsed back from {!to_html}. *)

val selectors : t -> int
(** [selectors t] is the number of complex selectors the stylesheet declared,
    counting each branch of a selector list separately. *)

val synthesised : t -> int
(** [synthesised t] is the number of those selectors an element was built for.
*)

val elements : t -> int
(** [elements t] is the number of elements in the document. *)

val skipped : t -> (string * int) list
(** [skipped t] pairs each reason a selector was not synthesised with how many
    selectors it covers, most frequent first. *)

val skipped_example : t -> string -> string option
(** [skipped_example t reason] is one selector skipped for [reason]. *)
