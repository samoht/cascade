(** Two-longhand shorthands: the logical-axis pairs ([margin-inline],
    [border-block-width], [grid-row] and the like), [gap], the [place-*]
    families and the other two-part shorthands ([flex-flow], [container],
    [background-position], [white-space], [text-wrap] and the like). *)

val compose_pair_via_index : ctx:Ctx.t -> Rule_index.t -> unit
(** [compose_pair_via_index ~ctx idx] absorbs every adjacent same-importance
    longhand pair of [idx] into its two-part shorthand. *)
