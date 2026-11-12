(** Timing shorthands: which properties a rule's [transition] reaches, and
    composing [transition], [animation] and [view-timeline]. *)

val transitioned_in_rule :
  Declaration.declaration list -> Declaration.declaration -> bool
(** [transitioned_in_rule decls d] is [true] when one of [decls] declares a
    transition reaching the property [d] writes. *)

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
