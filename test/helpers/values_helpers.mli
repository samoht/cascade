(** Value checkers shared by the Values test suites.

    Each [check_<type> input] reads [input] with the [<type>] reader, prints it
    back and checks the result is [input], or [expected] when given. *)

type check :=
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit

val check_length : check
(** [check_length] checks a [<length>]. *)

val check_color :
  ?minify:bool ->
  ?roundtrip:bool ->
  ?expected:string ->
  ?optimized:string ->
  string ->
  unit
(** [check_color input] checks a [<color>], and checks that optimizing a [color]
    declaration holding it prints [optimized] (default: the held form). *)

val check_angle : check
(** [check_angle] checks an [<angle>]. *)

val check_duration : check
(** [check_duration] checks a [<time>]. *)

val check_percentage : check
(** [check_percentage] checks a [<percentage>]. *)

val check_number : check
(** [check_number] checks a [<number>]. *)

val check_math_arg : check
(** [check_math_arg] checks a math function argument. *)

val check_component : check
(** [check_component] checks a colour component. *)

val check_channel : check
(** [check_channel] checks a colour channel. *)

val check_rgb : check
(** [check_rgb] checks an [rgb()] colour. *)
