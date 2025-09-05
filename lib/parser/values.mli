(** CSS Values & Units parsing using Reader API. *)

val read_length : Reader.t -> Css.length
(** [read_length t] reads a CSS length or keyword. *)

val read_color : Reader.t -> Css.color
(** [read_color t] reads a CSS color value (hex, rgb()/rgba(), keyword). *)

val read_angle : Reader.t -> Css.angle
(** [read_angle t] reads a CSS angle value. *)

val read_duration : Reader.t -> Css.duration
(** [read_duration t] reads a CSS duration value. *)

val read_number : Reader.t -> Css.number
(** [read_number t] reads a CSS number (int or float). *)

val read_percentage : Reader.t -> float
(** [read_percentage t] reads a percentage value and returns the numeric part.
*)

val read_calc : Reader.t -> Css.length Css.calc
(** [read_calc t] reads a calc() expression or a value promotable to calc. *)
