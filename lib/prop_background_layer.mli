(** The background layers: attachment, repeat, box, size and position longhands
    and the [background] shorthand, shared with the mask layers. *)

val normalize_background_repeat :
  Properties_intf.background_repeat -> Properties_intf.background_repeat
(** [normalize_background_repeat v] is [v] in canonical form. *)

val normalize_background :
  ?lossless:bool -> Properties_intf.background -> Properties_intf.background
(** [normalize_background ?lossless v] is [v] in canonical form. *)

val pp_background_attachment : Properties_intf.background_attachment Pp.t
(** [pp_background_attachment] prints a [background-attachment] value. *)

val pp_background_repeat : Properties_intf.background_repeat Pp.t
(** [pp_background_repeat] prints a [background-repeat] value. *)

val pp_background_box : Properties_intf.background_box Pp.t
(** [pp_background_box] prints a [background-box] value. *)

val normalize_background_size :
  Properties_intf.background_size -> Properties_intf.background_size
(** [normalize_background_size v] is [v] in canonical form. *)

val pp_background_size : Properties_intf.background_size Pp.t
(** [pp_background_size] prints a [background-size] value. *)

val pp_background_position : Properties_intf.background_position Pp.t
(** [pp_background_position] prints a [background-position] value. *)

val pp_background_position_axis : Properties_intf.background_position_axis Pp.t
(** [pp_background_position_axis] prints a [background-position-axis] value. *)

val pp_bg_prop : (unit -> unit) -> ('a -> 'b -> unit) -> 'a -> 'b option -> unit
(** [pp_bg_prop maybe_space pp ctx v] calls [maybe_space ()] then prints the
    slot with [pp] when [v] is [Some _], and prints nothing otherwise. *)

val pp_background_shorthand : Properties_intf.background_shorthand Pp.t
(** [pp_background_shorthand] prints a [background-shorthand] value. *)

val pp_background : Properties_intf.background Pp.t
(** [pp_background] prints a [background] value. *)

val read_background_attachment :
  Cursor.t -> Properties_intf.background_attachment
(** [read_background_attachment t] reads a [background-attachment] value from
    [t]. *)

val read_background_repeat : Cursor.t -> Properties_intf.background_repeat
(** [read_background_repeat t] reads a [background-repeat] value from [t]. *)

val read_background_repeat_list : Cursor.t -> Properties_intf.background_repeat
(** [read_background_repeat_list t] reads a [background-repeat-list] value from
    [t]. *)

val read_background_size : Cursor.t -> Properties_intf.background_size
(** [read_background_size t] reads a [background-size] value from [t]. *)

val read_background_size_list : Cursor.t -> Properties_intf.background_size
(** [read_background_size_list t] reads a [background-size-list] value from [t].
*)

val background_shorthand :
  ?color:Values.color ->
  ?image:Properties_intf.background_image ->
  ?position:Properties_intf.position_value ->
  ?size:Properties_intf.background_size ->
  ?repeat:Properties_intf.background_repeat ->
  ?attachment:Properties_intf.background_attachment ->
  ?clip:Properties_intf.background_box ->
  ?origin:Properties_intf.background_box ->
  unit ->
  Properties_intf.background
(** [background_shorthand ?color ?image ?position ?size ?repeat ?attachment
     ?clip ?origin ()] is a [background] shorthand value. *)

val read_background_box : Cursor.t -> Properties_intf.background_box
(** [read_background_box t] reads a [background-box] value from [t]. *)

val read_background_box_list : Cursor.t -> Properties_intf.background_box
(** [read_background_box_list t] reads a [background-box-list] value from [t].
*)

val normalize_background_position_axis :
  Properties_intf.background_position_axis ->
  Properties_intf.background_position_axis
(** [normalize_background_position_axis v] is [v] in canonical form. *)

val read_background_position : Cursor.t -> Properties_intf.background_position
(** [read_background_position t] reads a [background-position] value from [t].
*)

val read_background_position_x :
  Cursor.t -> Properties_intf.background_position_axis
(** [read_background_position_x t] reads a [background-position-x] value from
    [t]. *)

val read_background_position_y :
  Cursor.t -> Properties_intf.background_position_axis
(** [read_background_position_y t] reads a [background-position-y] value from
    [t]. *)

val read_background_shorthand : Cursor.t -> Properties_intf.background_shorthand
(** [read_background_shorthand t] reads a [background-shorthand] value from [t].
*)

val read_background : Cursor.t -> Properties_intf.background
(** [read_background t] reads a [background] value from [t]. *)

val read_backgrounds : Cursor.t -> Properties_intf.background list
(** [read_backgrounds t] reads a [backgrounds] value from [t]. *)
