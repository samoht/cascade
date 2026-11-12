(** Layout shorthands: [flex], [offset], [columns], [grid-template] and [grid].
*)

val compose_flex_via_index : Rule_index.t -> unit
(** [compose_flex_via_index idx] absorbs every [flex-grow]/[-shrink]/[-basis]
    run of [idx] into [flex]. *)

val compose_offset_via_index : Rule_index.t -> unit
(** [compose_offset_via_index idx] absorbs every run of the five [offset]
    longhands of [idx] into [offset]. *)

val compose_columns_via_index : Rule_index.t -> unit
(** [compose_columns_via_index idx] absorbs every [columns] longhand run of
    [idx] into [columns]. *)

val compose_grid_template_via_index : Rule_index.t -> unit
(** [compose_grid_template_via_index idx] absorbs every
    [grid-template-rows]/[-columns]/[-areas] run of [idx] into [grid-template].
*)

val compose_grid_via_index : Rule_index.t -> unit
(** [compose_grid_via_index idx] absorbs every [grid] longhand run of [idx] into
    [grid]. *)
