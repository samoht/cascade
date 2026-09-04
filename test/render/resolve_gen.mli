(** Seeded documents and stylesheets for the cascade differential.

    The sheets here are built to make the cascade decide: most rules write the
    same handful of properties, so several declarations compete for one slot on
    one element and something has to win. Which one is the question
    {!Cascade.Resolve} answers and the browser settles.

    Document and sheet are drawn from one fixed vocabulary of tags, classes, ids
    and attributes, so a generated selector has elements to match. Neither ever
    names [html], [head] or [body]: those three carry the user-agent style the
    comparison leaves alone on both sides. *)

type element
(** An element of a synthesised document. *)

type doc
(** A synthesised document: the [html], [head] and [body] scaffolding, and the
    elements a sheet is resolved against. *)

module Node : Cascade.Resolve.NODE with type t = element
(** The document as the library's matcher reads it. *)

val elt :
  ?id:string ->
  ?classes:string list ->
  ?attrs:(string * string) list ->
  string ->
  element list ->
  element
(** [elt ?id ?classes ?attrs tag children] is one element. Parent links are tied
    by {!doc}, so an element belongs to the one document it is passed to. *)

val doc : element list -> doc
(** [doc children] is the document whose [body] holds [children]. *)

val document : seed:int -> doc
(** [document ~seed] is the document for [seed]. *)

val stylesheet : seed:int -> Cascade.Css.t
(** [stylesheet ~seed] is the sheet for [seed]: rules, [@layer] blocks and
    statements, nested rules and [!important], over the vocabulary {!document}
    builds from. *)

val subjects : doc -> element list
(** [subjects d] is every element under [body], in document order: the elements
    the differential resolves and the browser reports on, and in the order the
    driver enumerates them. *)

val tag : element -> string
(** [tag e] is [e]'s element name. *)

val label : doc -> element -> string
(** [label d e] names [e] in a report: its tag, id and classes, and its path
    from [body]. *)

val json_of_doc : doc -> Json.t
(** [json_of_doc d] is [d] as [dom.js] builds it. *)
