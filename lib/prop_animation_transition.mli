(** CSS Easing 1 and CSS Transitions 2: easing functions, the transition
    shorthand and its longhands, and overlay. *)

open Properties_intf

val pp_steps_direction : steps_direction Pp.t
(** [pp_steps_direction] prints a [steps_direction]. *)

val pp_timing_function : timing_function Pp.t
(** [pp_timing_function] prints a [timing_function]. *)

val normalize_timing_function : timing_function -> timing_function
(** [normalize_timing_function v] is [v] in its canonical spelling. *)

val pp_transition_property_value : transition_property_value Pp.t
(** [pp_transition_property_value] prints a [transition_property_value]. *)

val pp_transition_property : transition_property Pp.t
(** [pp_transition_property] prints a [transition_property]. *)

val pp_transition_behavior : transition_behavior Pp.t
(** [pp_transition_behavior] prints a [transition_behavior]. *)

val pp_overlay : overlay Pp.t
(** [pp_overlay] prints an [overlay]. *)

val pp_transition_shorthand : transition_shorthand Pp.t
(** [pp_transition_shorthand] prints a [transition] shorthand value. *)

val normalize_transition : transition -> transition
(** [normalize_transition v] is [v] in its canonical spelling. *)

val pp_transition : transition Pp.t
(** [pp_transition] prints a [transition]. *)

val read_steps_direction : Cursor.t -> steps_direction
(** [read_steps_direction t] reads a [steps_direction] from [t]. *)

val read_timing_function : Cursor.t -> timing_function
(** [read_timing_function t] reads a [timing_function] from [t]. *)

val read_timing_function_list : Cursor.t -> timing_function
(** [read_timing_function_list t] reads a non-empty comma-separated list of
    easing functions from [t]; one is returned bare, several as
    [Timing_functions]. *)

val read_duration_list :
  (Cursor.t -> Values.duration) -> Cursor.t -> Values.duration
(** [read_duration_list read_one t] reads a comma-separated list of [read_one]
    values from [t]; one value is returned bare, several as [Durations]. *)

val read_transition_property_value : Cursor.t -> transition_property_value
(** [read_transition_property_value t] reads a [transition_property_value] from
    [t]. *)

val read_transition_property : Cursor.t -> transition_property
(** [read_transition_property t] reads a [transition_property] from [t]. *)

val read_transition_behavior : Cursor.t -> transition_behavior
(** [read_transition_behavior t] reads a [transition_behavior] from [t]. *)

val read_transition_behavior_list : Cursor.t -> transition_behavior
(** [read_transition_behavior_list t] reads a non-empty comma-separated list of
    transition behaviours from [t]; one is returned bare, several as
    [Behaviors]. *)

val read_overlay : Cursor.t -> overlay
(** [read_overlay t] reads an [overlay] from [t]. *)

val read_transition_shorthand : Cursor.t -> transition_shorthand
(** [read_transition_shorthand t] reads a [transition] shorthand value from [t].
*)

val read_transition : Cursor.t -> transition
(** [read_transition t] reads a [transition] from [t]. *)

val read_transitions : Cursor.t -> transition list
(** [read_transitions t] reads a non-empty comma-separated list of transitions
    from [t]. *)

val transition_shorthand :
  ?property:transition_property_value ->
  ?duration:Values.duration ->
  ?timing_function:timing_function ->
  ?delay:Values.duration ->
  ?behavior:transition_behavior ->
  unit ->
  transition
(** [transition_shorthand ?property ?duration ?timing_function ?delay ?behavior
     ()] is the [transition] shorthand with the given components; [property]
    defaults to [all]. *)
