(** [\@import] inlining: each import is replaced by the body of the imported
    stylesheet, its layer, supports and media guards evaluated or kept. *)

val imports :
  ?query:Context.query ->
  ?layer_order:string list ->
  Context.loader ->
  Stylesheet.t ->
  Stylesheet.t
(** [imports ?query ?layer_order loader stylesheet] replaces every [\@import] in
    [stylesheet] that [loader] resolves with the imported body. *)
