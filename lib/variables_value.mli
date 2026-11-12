(** Variable extraction from the shared CSS value types: lengths, [calc()],
    colours, angles, numbers, durations and timing functions. *)

val vars_of_calc : 'a Values.calc -> Variables_intf.any_var list
(** [vars_of_calc v] is the variables [v] references. *)

val vars_of_length : Values.length -> Variables_intf.any_var list
(** [vars_of_length v] is the variables [v] references. *)

val vars_of_length_list : Values.length list -> Variables_intf.any_var list
(** [vars_of_length_list v] is the variables [v] references. *)

val vars_of_lvc :
  [< `Calc of 'a Values.calc
  | `Length of Values.length
  | `Other
  | `Var of Variables_intf.any_var ] ->
  Variables_intf.any_var list
(** [vars_of_lvc v] is the variables [v] references. *)

val vars_of_length_percentage :
  Values.length_percentage -> Variables_intf.any_var list
(** [vars_of_length_percentage v] is the variables [v] references. *)

val vars_of_angle : Values.angle -> Variables_intf.any_var list
(** [vars_of_angle v] is the variables [v] references. *)

val vars_of_channel : Values.channel -> Variables_intf.any_var list
(** [vars_of_channel v] is the variables [v] references. *)

val vars_of_rgb : Values.rgb -> Variables_intf.any_var list
(** [vars_of_rgb v] is the variables [v] references. *)

val vars_of_percentage : Values.percentage -> Variables_intf.any_var list
(** [vars_of_percentage v] is the variables [v] references. *)

val vars_of_color : Values.color -> Variables_intf.any_var list
(** [vars_of_color v] is the variables [v] references. *)

val vars_of_duration : Values.duration -> Variables_intf.any_var list
(** [vars_of_duration v] is the variables [v] references. *)

val vars_of_optional_length :
  Values.length option -> Variables_intf.any_var list
(** [vars_of_optional_length v] is the variables [v] references. *)

val vars_of_number_value : Values.number -> Variables_intf.any_var list
(** [vars_of_number_value v] is the variables [v] references. *)

val vars_of_timing_function :
  Properties.timing_function -> Variables_intf.any_var list
(** [vars_of_timing_function v] is the variables [v] references. *)

val vars_of_css_wide : Properties.css_wide -> Variables_intf.any_var list
(** [vars_of_css_wide v] is the variables [v] references. *)
