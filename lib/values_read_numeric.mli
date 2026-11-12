(** Readers for lengths, angles, durations, numbers and percentages. *)

open Values_intf

val read_length :
  ?allow_negative:bool ->
  ?with_keywords:bool ->
  ?sizing:bool ->
  ?length_only:bool ->
  Cursor.t ->
  length
(** [read_length t] parses a CSS length. *)

val read_non_negative_length :
  ?with_keywords:bool -> ?length_only:bool -> Cursor.t -> length
(** [read_non_negative_length reader] parses a length value that must be
    non-negative. *)

val read_angle : Cursor.t -> angle
(** [read_angle t] parses a CSS angle. *)

val read_angle_unit_required : Cursor.t -> angle
(** [read_angle_unit_required t] parses a generic CSS [<angle>] value, where
    bare zero is invalid under CSS Values 4 sec. 7.1. Legacy property contexts
    that allow unitless zero use {!read_angle}. *)

val read_duration : Cursor.t -> duration
(** [read_duration t] parses a CSS duration. *)

val read_duration_preserve_ms : ?allow_negative:bool -> Cursor.t -> duration
(** [read_duration_preserve_ms t] parses a CSS duration without canonicalizing
    milliseconds to seconds. *)

val read_time : Cursor.t -> duration
(** [read_time t] parses a CSS time value (can be negative). *)

val read_number : Cursor.t -> number
(** [read_number t] parses a CSS number (int/float). *)

val read_transition_behavior : Cursor.t -> transition_behavior
(** [read_transition_behavior t] parses a CSS transition-behavior value. *)

val read_percentage : Cursor.t -> percentage
(** [read_percentage t] parses a CSS percentage. *)

val read_length_percentage :
  ?allow_negative:bool ->
  ?with_keywords:bool ->
  ?sizing:bool ->
  Cursor.t ->
  length_percentage
(** [read_length_percentage t] parses a CSS length or percentage. *)

val read_number_percentage : Cursor.t -> number_percentage
(** [read_number_percentage t] parses a CSS number or percentage. *)

val read_padding_shorthand : Cursor.t -> length list
(** [read_padding_shorthand reader] parses a padding shorthand property
    accepting 1-4 space-separated non-negative length values (CSS Box 4 sec.
    4.2). *)

val read_margin_length : ?global:bool -> Cursor.t -> length
(** [read_margin_length ?global t] parses one margin component: CSS Box 4 sec.
    3.1 gives a margin longhand [<length-percentage> | auto], so this takes
    [auto] and a length and no sizing function. *)

val read_margin_shorthand : Cursor.t -> length list
(** [read_margin_shorthand reader] parses a margin shorthand property accepting
    1-4 space-separated length values (CSS Box 4 sec. 3.2). *)
