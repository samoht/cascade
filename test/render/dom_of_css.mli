(** Synthesise an HTML document from the selectors of a stylesheet.

    Random markup matches almost nothing, so the differential test derives its
    document from the sheet under test: every selector is walked, and elements
    carrying the tags, classes, ids, attributes, nesting and sibling positions
    it asks for are added to the document. A selector that cannot hold without a
    user interaction ([:hover]), a shadow tree ([::part]) or a live document
    ([:target]) is not synthesised, but it is counted and reported - it is never
    silently dropped. *)

type t
(** A synthesised document. *)

val of_stylesheet : ?max_elements:int -> Cascade.Css.t -> t
(** [of_stylesheet sheet] derives a document from the selectors of [sheet],
    including the selectors of nested rules and of rules inside [@media],
    [@layer], [@supports] and [@container]. Synthesis stops once the document
    reaches [max_elements] elements (default 4000); the selectors left over are
    counted under the ["document element cap"] reason. *)

val to_json : t -> Json.t
(** [to_json t] is the document as the browser driver consumes it: the element
    tree, the classes and attributes the root [html] element must carry, the
    pseudo-elements to sample, and the probe selectors. *)

val selectors : t -> int
(** [selectors t] is the number of complex selectors the stylesheet declared,
    counting each branch of a selector list separately. *)

val synthesised : t -> int
(** [synthesised t] is the number of those selectors an element was built for.
*)

val elements : t -> int
(** [elements t] is the number of elements in the document. *)

val pseudo_elements : t -> string list
(** [pseudo_elements t] is the pseudo-elements the stylesheet targets that the
    driver samples with [getComputedStyle], such as [::before]. *)

val skipped : t -> (string * int) list
(** [skipped t] pairs each reason a selector was not synthesised with how many
    selectors it covers, most frequent first. *)

val skipped_example : t -> string -> string option
(** [skipped_example t reason] is one selector skipped for [reason]. *)
