(** Exact declaration-to-row index. *)

type t
(** Immutable index from declarations to sorted row ids. *)

val v :
  ?same:(Declaration.t -> Declaration.t -> bool) ->
  ?keep:(Stylesheet.rule -> bool) ->
  Stylesheet.rule array ->
  t
(** [v ?same ?keep rules] indexes declarations from rules accepted by [keep].
    Duplicate declarations within one rule contribute that row only once. *)

val rows : t -> Declaration.t -> int array
(** [rows t decl] returns the sorted row ids that contain [decl]. It returns an
    empty array when [decl] is absent. *)

val iter : t -> (Declaration.t -> int array -> unit) -> unit
(** [iter t f] calls [f decl rows] for each indexed declaration. {!val-rows} is
    sorted ascending and contains no duplicate row ids. *)
