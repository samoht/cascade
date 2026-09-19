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
(** A synthesised document: the elements a sheet is resolved against, in the
    order they stand under [body]. *)

val elt :
  ?id:string ->
  ?classes:string list ->
  ?attrs:(string * string) list ->
  ?text:bool ->
  string ->
  element list ->
  element
(** [elt ?id ?classes ?attrs ?text tag children] is one element. It holds a
    letter before its children unless [text] is [false], so that what is
    resolved onto it paints. *)

val doc : element list -> doc
(** [doc children] is the document whose [body] holds [children]. *)

val document : seed:int -> doc
(** [document ~seed] is the document for [seed]. *)

val stylesheet : seed:int -> Cascade.Css.t
(** [stylesheet ~seed] is the sheet for [seed]: rules, [@layer] blocks and
    statements, nested rules and [!important], over the vocabulary {!document}
    builds from. *)

val html_of_doc : doc -> string
(** [html_of_doc d] is [d] as HTML text, the elements alone with no [html] or
    [body] around them. The vocabulary holds no tag the HTML parser closes or
    moves on its own, so every parser builds the tree written. *)
