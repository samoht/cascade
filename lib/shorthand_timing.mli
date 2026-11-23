(** Timing shorthands: composing [transition], [animation] and [view-timeline].
*)

val compose_view_timeline_via_index : Rule_index.t -> unit
(** [compose_view_timeline_via_index idx] absorbs every [view-timeline-*] run of
    [idx] into [view-timeline]. *)

val compose_transition_via_index :
  ctx:Ctx.t -> held:Shorthand_held.held -> Rule_index.t -> unit
(** [compose_transition_via_index ~ctx ~held idx] absorbs every single-layer
    [transition] longhand run of [idx] into [transition], unless [held] or an
    earlier declaration holds a slot the run leaves out. *)

val compose_animation_via_index :
  ctx:Ctx.t -> held:Shorthand_held.held -> Rule_index.t -> unit
(** [compose_animation_via_index ~ctx ~held idx] is
    {!val-compose_transition_via_index} for [animation]. *)
