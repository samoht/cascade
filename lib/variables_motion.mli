(** Variable extraction from the transform, animation, transition, timeline and
    offset-path property values. *)

val vars_of_rotate_value :
  Properties.rotate_value -> Variables_intf.any_var list
(** [vars_of_rotate_value v] is the variables [v] references. *)

val vars_of_transform : Properties.transform -> Variables_intf.any_var list
(** [vars_of_transform v] is the variables [v] references. *)

val vars_of_transform_list :
  Properties.transform list -> Variables_intf.any_var list
(** [vars_of_transform_list v] is the variables [v] references. *)

val vars_of_scale : Properties.scale -> Variables_intf.any_var list
(** [vars_of_scale v] is the variables [v] references. *)

val vars_of_translate_value :
  Properties.translate_value -> Variables_intf.any_var list
(** [vars_of_translate_value v] is the variables [v] references. *)

val vars_of_animation : Properties.animation -> Variables_intf.any_var list
(** [vars_of_animation v] is the variables [v] references. *)

val vars_of_transition_property_value :
  Properties.transition_property_value -> Variables_intf.any_var list
(** [vars_of_transition_property_value v] is the variables [v] references. *)

val vars_of_transition_property_list :
  Properties.transition_property -> Variables_intf.any_var list
(** [vars_of_transition_property_list v] is the variables [v] references. *)

val vars_of_transition : Properties.transition -> Variables_intf.any_var list
(** [vars_of_transition v] is the variables [v] references. *)

val vars_of_animation_name :
  Properties.animation_name -> Variables_intf.any_var list
(** [vars_of_animation_name v] is the variables [v] references. *)

val vars_of_transform_origin :
  Properties.transform_origin -> Variables_intf.any_var list
(** [vars_of_transform_origin v] is the variables [v] references. *)

val vars_of_will_change : Properties.will_change -> Variables_intf.any_var list
(** [vars_of_will_change v] is the variables [v] references. *)

val vars_of_animation_timeline :
  Properties.animation_timeline -> Variables_intf.any_var list
(** [vars_of_animation_timeline v] is the variables [v] references. *)

val vars_of_animation_range_item :
  Properties.animation_range_item -> Variables_intf.any_var list
(** [vars_of_animation_range_item v] is the variables [v] references. *)

val vars_of_animation_range :
  Properties.animation_range -> Variables_intf.any_var list
(** [vars_of_animation_range v] is the variables [v] references. *)

val vars_of_view_transition_name :
  Properties.view_transition_name -> Variables_intf.any_var list
(** [vars_of_view_transition_name v] is the variables [v] references. *)

val vars_of_view_transition_class :
  Properties.view_transition_class -> Variables_intf.any_var list
(** [vars_of_view_transition_class v] is the variables [v] references. *)

val vars_of_offset_path : Properties.offset_path -> Variables_intf.any_var list
(** [vars_of_offset_path v] is the variables [v] references. *)

val vars_of_offset_anchor :
  Properties.offset_anchor -> Variables_intf.any_var list
(** [vars_of_offset_anchor v] is the variables [v] references. *)

val vars_of_offset_position :
  Properties.offset_position -> Variables_intf.any_var list
(** [vars_of_offset_position v] is the variables [v] references. *)

val vars_of_offset_rotate :
  Properties.offset_rotate -> Variables_intf.any_var list
(** [vars_of_offset_rotate v] is the variables [v] references. *)

val vars_of_offset : Properties.offset -> Variables_intf.any_var list
(** [vars_of_offset v] is the variables [v] references. *)

val vars_of_transform_style :
  Properties.transform_style -> Variables_intf.any_var list
(** [vars_of_transform_style v] is the variables [v] references. *)

val vars_of_transform_box :
  Properties.transform_box -> Variables_intf.any_var list
(** [vars_of_transform_box v] is the variables [v] references. *)

val vars_of_backface_visibility :
  Properties.backface_visibility -> Variables_intf.any_var list
(** [vars_of_backface_visibility v] is the variables [v] references. *)

val vars_of_animation_direction :
  Properties.animation_direction -> Variables_intf.any_var list
(** [vars_of_animation_direction v] is the variables [v] references. *)

val vars_of_animation_fill_mode :
  Properties.animation_fill_mode -> Variables_intf.any_var list
(** [vars_of_animation_fill_mode v] is the variables [v] references. *)

val vars_of_animation_play_state :
  Properties.animation_play_state -> Variables_intf.any_var list
(** [vars_of_animation_play_state v] is the variables [v] references. *)

val vars_of_animation_iteration_count :
  Properties.animation_iteration_count -> Variables_intf.any_var list
(** [vars_of_animation_iteration_count v] is the variables [v] references. *)

val vars_of_transition_behavior :
  Properties.transition_behavior -> Variables_intf.any_var list
(** [vars_of_transition_behavior v] is the variables [v] references. *)

val vars_of_perspective_origin :
  Properties.perspective_origin -> Variables_intf.any_var list
(** [vars_of_perspective_origin v] is the variables [v] references. *)

val vars_of_timeline_axis :
  Properties.timeline_axis -> Variables_intf.any_var list
(** [vars_of_timeline_axis v] is the variables [v] references. *)

val vars_of_timeline_scope :
  Properties.timeline_scope -> Variables_intf.any_var list
(** [vars_of_timeline_scope v] is the variables [v] references. *)

val vars_of_timeline_name :
  Properties.timeline_name -> Variables_intf.any_var list
(** [vars_of_timeline_name v] is the variables [v] references. *)

val vars_of_timeline_shorthand :
  Properties.timeline_shorthand -> Variables_intf.any_var list
(** [vars_of_timeline_shorthand v] is the variables [v] references. *)

val vars_of_timeline_inset :
  Properties.timeline_inset -> Variables_intf.any_var list
(** [vars_of_timeline_inset v] is the variables [v] references. *)

val vars_of_view_timeline_shorthand :
  Properties.view_timeline_shorthand -> Variables_intf.any_var list
(** [vars_of_view_timeline_shorthand v] is the variables [v] references. *)
