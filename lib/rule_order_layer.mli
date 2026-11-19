(** The cascade-layer half of {!Rule_order}'s canonical projection. CSS Cascade
    5 sec. 6.4.3 sorts layers by the order in which they are first declared, so
    what a layer statement or block decides is a position in that order, and two
    sheets that reach one order by different statements project alike. *)

val fold_layer_pins : Stylesheet.statement list -> Stylesheet.statement list
(** [fold_layer_pins stmts] drops from each top-level [@layer] statement every
    name whose removal leaves the sheet's layer order alone, and the statement
    once it names none. A position the projection cannot read blocks the fold
    across it. *)

val sort_layer_blocks : Stylesheet.statement list -> Stylesheet.statement list
(** [sort_layer_blocks stmts] moves each named layer block among the statements
    it passes without deciding anything with them into the rank order of the
    sheet's layers, then folds the pins again. *)

val canonical_layer_declarations :
  Stylesheet.statement list -> Stylesheet.statement list
(** [canonical_layer_declarations stmts] rewrites every barrier-free run of
    layer constructs, at any depth, to one [@layer] statement naming the run's
    layers in order followed by its layer blocks in rank order. *)
