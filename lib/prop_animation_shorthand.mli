(** CSS Animations 2: the animation shorthand and its animation-* longhands. *)

open Properties_intf

val normalize_animation_iteration_count :
  ctx:Values.calc_ctx -> animation_iteration_count -> animation_iteration_count
(** [normalize_animation_iteration_count ~ctx v] is [v] with static math on each
    count folded under [ctx]. *)

val pp_animation_direction : animation_direction Pp.t
(** [pp_animation_direction] prints an [animation_direction]. *)

val pp_animation_fill_mode : animation_fill_mode Pp.t
(** [pp_animation_fill_mode] prints an [animation_fill_mode]. *)

val pp_animation_iteration_count : animation_iteration_count Pp.t
(** [pp_animation_iteration_count] prints an [animation_iteration_count]. *)

val pp_animation_name : animation_name Pp.t
(** [pp_animation_name] prints an [animation_name]. *)

val pp_animation_play_state : animation_play_state Pp.t
(** [pp_animation_play_state] prints an [animation_play_state]. *)

val pp_animation_composition_item : Pp.ctx -> animation_composition_item -> unit
(** [pp_animation_composition_item] prints [replace], [add] or [accumulate]. *)

val pp_animation_composition : animation_composition Pp.t
(** [pp_animation_composition] prints an [animation_composition]. *)

val read_animation_direction : Cursor.t -> animation_direction
(** [read_animation_direction t] reads an [animation_direction] from [t]. *)

val read_animation_fill_mode : Cursor.t -> animation_fill_mode
(** [read_animation_fill_mode t] reads an [animation_fill_mode] from [t]. *)

val read_animation_iteration_count : Cursor.t -> animation_iteration_count
(** [read_animation_iteration_count t] reads an [animation_iteration_count] from
    [t]. *)

val read_animation_name : Cursor.t -> animation_name
(** [read_animation_name t] reads an [animation_name] from [t]. *)

val read_animation_play_state : Cursor.t -> animation_play_state
(** [read_animation_play_state t] reads an [animation_play_state] from [t]. *)

val read_animation_composition_item : Cursor.t -> animation_composition_item
(** [read_animation_composition_item t] reads an [animation_composition_item]
    from [t]. *)

val read_animation_composition : Cursor.t -> animation_composition
(** [read_animation_composition t] reads an [animation_composition] from [t]. *)

val read_animation_shorthand : Cursor.t -> animation_shorthand
(** [read_animation_shorthand t] reads an [animation] shorthand value from [t].
*)

val pp_animation_shorthand : animation_shorthand Pp.t
(** [pp_animation_shorthand] prints an [animation] shorthand value. *)

val normalize_animation : ctx:Values.calc_ctx -> animation -> animation
(** [normalize_animation ~ctx v] is [v] in its canonical spelling, static math
    folded under [ctx]; a shorthand whose components are all initial becomes
    [none]. *)

val pp_animation : animation Pp.t
(** [pp_animation] prints an [animation]. *)

val read_animation : Cursor.t -> animation
(** [read_animation t] reads an [animation] from [t]. *)

val read_animations : Cursor.t -> animation list
(** [read_animations t] reads a non-empty comma-separated list of animations
    from [t]. *)

val animation_shorthand :
  ?name:string ->
  ?duration:Values.duration ->
  ?timing_function:timing_function ->
  ?delay:Values.duration ->
  ?iteration_count:animation_iteration_count ->
  ?direction:animation_direction ->
  ?fill_mode:animation_fill_mode ->
  ?play_state:animation_play_state ->
  ?timeline:animation_timeline ->
  unit ->
  animation
(** [animation_shorthand ?name ?duration ?timing_function ?delay
     ?iteration_count ?direction ?fill_mode ?play_state ?timeline ()] is the
    [animation] shorthand with the given components. *)
