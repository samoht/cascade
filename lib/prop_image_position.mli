(** The [<position>] value (CSS Values 4 sec. 8.3) that gradients, backgrounds,
    masks and the transform and offset families share. *)

open Properties_intf

val pp_position_value : position_value Pp.t
(** [pp_position_value] prints a [position_value]. *)

val position_value_is_center : position_value -> bool
(** [position_value_is_center p] is [true] iff [p] is spelt [center], [50%] or
    [50% 50%]. *)

val position_xy : position_value -> (Values.length * Values.length) option
(** [position_xy p] is the horizontal and vertical components of [p]; [None]
    when a component is dynamic or is a non-zero length offset from [right] or
    [bottom]. *)

val position_of_xy : Values.length -> Values.length -> position_value
(** [position_of_xy x y] is the shortest [<position>] spelling of the static
    point ([x], [y]). *)

val normalize_position_value : ?strip:bool -> position_value -> position_value
(** [normalize_position_value ?strip p] is [p] in its canonical spelling;
    [strip] (default [true]) drops the unit on a zero length. *)

val read_position_value : Cursor.t -> position_value
(** [read_position_value t] reads a [position_value] from [t]. *)

val read_background_position_value : Cursor.t -> position_value
(** [read_background_position_value t] reads a [position_value] from [t]. *)
