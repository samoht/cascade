(** CSS Scroll Animations 1 and View Transitions 2 properties:
    animation-timeline, animation-range, the scroll and view timelines,
    timeline-scope and view-transition-name / -class. *)

open Properties_intf

val normalize_animation_range : animation_range -> animation_range
(** [normalize_animation_range v] is [v] in its canonical spelling. *)

val normalize_timeline_inset : timeline_inset -> timeline_inset
(** [normalize_timeline_inset v] is [v] in its canonical spelling. *)

val pp_animation_timeline : animation_timeline Pp.t
(** [pp_animation_timeline] prints an [animation_timeline]. *)

val pp_animation_range_name : animation_range_name Pp.t
(** [pp_animation_range_name] prints an [animation_range_name]. *)

val pp_animation_range_item : animation_range_item Pp.t
(** [pp_animation_range_item] prints an [animation_range_item]. *)

val pp_animation_range : animation_range Pp.t
(** [pp_animation_range] prints an [animation_range]. *)

val pp_view_transition_name : view_transition_name Pp.t
(** [pp_view_transition_name] prints a [view_transition_name]. *)

val pp_view_transition_class : view_transition_class Pp.t
(** [pp_view_transition_class] prints a [view_transition_class]. *)

val pp_timeline_axis : timeline_axis Pp.t
(** [pp_timeline_axis] prints a [timeline_axis]. *)

val pp_timeline_ident : timeline_ident Pp.t
(** [pp_timeline_ident] prints a [timeline_ident]. *)

val pp_timeline_scope : timeline_scope Pp.t
(** [pp_timeline_scope] prints a [timeline_scope]. *)

val pp_timeline_name : timeline_name Pp.t
(** [pp_timeline_name] prints a [timeline_name]. *)

val pp_timeline_shorthand_item : timeline_shorthand_item Pp.t
(** [pp_timeline_shorthand_item] prints a [timeline_shorthand_item]. *)

val normalize_timeline_shorthand : timeline_shorthand -> timeline_shorthand
(** [normalize_timeline_shorthand v] is [v] in its canonical spelling. *)

val normalize_view_timeline_shorthand :
  view_timeline_shorthand -> view_timeline_shorthand
(** [normalize_view_timeline_shorthand v] is [v] in its canonical spelling. *)

val pp_timeline_shorthand : timeline_shorthand Pp.t
(** [pp_timeline_shorthand] prints a [timeline_shorthand]. *)

val pp_timeline_inset_item : timeline_inset_item Pp.t
(** [pp_timeline_inset_item] prints a [timeline_inset_item]. *)

val pp_timeline_inset : timeline_inset Pp.t
(** [pp_timeline_inset] prints a [timeline_inset]. *)

val pp_view_timeline_shorthand_item : view_timeline_shorthand_item Pp.t
(** [pp_view_timeline_shorthand_item] prints a [view_timeline_shorthand_item].
*)

val pp_view_timeline_shorthand : view_timeline_shorthand Pp.t
(** [pp_view_timeline_shorthand] prints a [view_timeline_shorthand]. *)

val read_animation_timeline : Cursor.t -> animation_timeline
(** [read_animation_timeline t] reads an [animation_timeline] from [t]. *)

val read_view_transition_name : Cursor.t -> view_transition_name
(** [read_view_transition_name t] reads a [view_transition_name] from [t]. *)

val read_view_transition_class : Cursor.t -> view_transition_class
(** [read_view_transition_class t] reads a [view_transition_class] from [t]. *)

val read_timeline_axis : Cursor.t -> timeline_axis
(** [read_timeline_axis t] reads a [timeline_axis] from [t]. *)

val read_timeline_ident : Cursor.t -> timeline_ident
(** [read_timeline_ident t] reads a [timeline_ident] from [t]. *)

val read_timeline_name : Cursor.t -> timeline_name
(** [read_timeline_name t] reads a [timeline_name] from [t]. *)

val read_timeline_scope : Cursor.t -> timeline_scope
(** [read_timeline_scope t] reads a [timeline_scope] from [t]. *)

val read_timeline_shorthand_item : Cursor.t -> timeline_shorthand_item
(** [read_timeline_shorthand_item t] reads a [timeline_shorthand_item] from [t].
*)

val read_timeline_shorthand : Cursor.t -> timeline_shorthand
(** [read_timeline_shorthand t] reads a [timeline_shorthand] from [t]. *)

val read_timeline_inset_item : Cursor.t -> timeline_inset_item
(** [read_timeline_inset_item t] reads a [timeline_inset_item] from [t]. *)

val read_timeline_inset : Cursor.t -> timeline_inset
(** [read_timeline_inset t] reads a [timeline_inset] from [t]. *)

val read_view_timeline_shorthand_item : Cursor.t -> view_timeline_shorthand_item
(** [read_view_timeline_shorthand_item t] reads a [view_timeline_shorthand_item]
    from [t]. *)

val read_view_timeline_shorthand : Cursor.t -> view_timeline_shorthand
(** [read_view_timeline_shorthand t] reads a [view_timeline_shorthand] from [t].
*)

val read_animation_range_name : Cursor.t -> animation_range_name
(** [read_animation_range_name t] reads an [animation_range_name] from [t]. *)

val read_animation_range_item : Cursor.t -> animation_range_item
(** [read_animation_range_item t] reads an [animation_range_item] from [t]. *)

val read_animation_range : Cursor.t -> animation_range
(** [read_animation_range t] reads an [animation_range] from [t]. *)
