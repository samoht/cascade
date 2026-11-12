(** The border family: border-style, border-width with its math functions,
    border-radius, the [border] and logical border shorthands, border-collapse
    and border-spacing. *)

val pp_border_style : Properties_intf.border_style Pp.t
(** [pp_border_style] prints a [border-style] value. *)

val pp_border_radius : Properties_intf.border_radius Pp.t
(** [pp_border_radius] prints a [border-radius] value. *)

val normalize_border_radius :
  ?strip:bool -> Properties_intf.border_radius -> Properties_intf.border_radius
(** [normalize_border_radius ?strip v] is [v] in canonical form. [strip]
    (default [true]) is passed to [Values.normalize_length_percentage]. *)

val normalize_logical_border_color :
  ?lossless:bool ->
  Properties_intf.logical_border_color ->
  Properties_intf.logical_border_color
(** [normalize_logical_border_color ?lossless v] is [v] in canonical form. *)

val pp_border_width : Properties_intf.border_width Pp.t
(** [pp_border_width] prints a [border-width] value. *)

val pp_border_shorthand : Properties_intf.border_shorthand Pp.t
(** [pp_border_shorthand] prints a [border-shorthand] value. *)

val pp_border : Properties_intf.border Pp.t
(** [pp_border] prints a [border] value. *)

val pp_logical_border_color : Properties_intf.logical_border_color Pp.t
(** [pp_logical_border_color] prints a [logical-border-color] value. *)

val pp_logical_border_width : Properties_intf.logical_border_width Pp.t
(** [pp_logical_border_width] prints a [logical-border-width] value. *)

val pp_logical_border_style : Properties_intf.logical_border_style Pp.t
(** [pp_logical_border_style] prints a [logical-border-style] value. *)

val pp_border_spacing : Properties_intf.border_spacing Pp.t
(** [pp_border_spacing] prints a [border-spacing] value. *)

val normalize_border_spacing :
  Properties_intf.border_spacing -> Properties_intf.border_spacing
(** [normalize_border_spacing v] is [v] in canonical form. *)

val pp_border_collapse : Properties_intf.border_collapse Pp.t
(** [pp_border_collapse] prints a [border-collapse] value. *)

val read_border_style : Cursor.t -> Properties_intf.border_style
(** [read_border_style t] reads a [border-style] value from [t]. *)

val read_border_style_box : Cursor.t -> Properties_intf.border_style list
(** [read_border_style_box t] reads a [border-style-box] value from [t]. *)

val read_border_width : Cursor.t -> Properties_intf.border_width
(** [read_border_width t] reads a [border-width] value from [t]. *)

val read_border_shorthand : Cursor.t -> Properties_intf.border_shorthand
(** [read_border_shorthand t] reads a [border-shorthand] value from [t]. *)

val read_border : Cursor.t -> Properties_intf.border
(** [read_border t] reads a [border] value from [t]. *)

val read_logical_border_color : Cursor.t -> Properties_intf.logical_border_color
(** [read_logical_border_color t] reads a [logical-border-color] value from [t].
*)

val read_logical_border_width : Cursor.t -> Properties_intf.logical_border_width
(** [read_logical_border_width t] reads a [logical-border-width] value from [t].
*)

val read_logical_border_style : Cursor.t -> Properties_intf.logical_border_style
(** [read_logical_border_style t] reads a [logical-border-style] value from [t].
*)

val read_border_collapse : Cursor.t -> Properties_intf.border_collapse
(** [read_border_collapse t] reads a [border-collapse] value from [t]. *)

val border_shorthand :
  ?width:Properties_intf.border_width ->
  ?style:Properties_intf.border_style ->
  ?color:Values.color ->
  unit ->
  Properties_intf.border
(** [border_shorthand ?width ?style ?color ()] is a [border] shorthand value. *)

val read_border_radius_inline : Cursor.t -> Properties_intf.border_radius
(** [read_border_radius_inline t] reads a [border-radius-inline] value from [t].
*)

val read_border_radius : Cursor.t -> Properties_intf.border_radius
(** [read_border_radius t] reads a [border-radius] value from [t]. *)

val normalize_border_width :
  Properties_intf.border_width -> Properties_intf.border_width
(** [normalize_border_width v] is [v] in canonical form. *)

val border_width_has_runtime_subst : Properties_intf.border_width -> bool
(** [border_width_has_runtime_subst w] is [true] when [w] holds a substitution
    resolved at computed-value time, such as [var()]. *)

val normalize_logical_border_width :
  Properties_intf.logical_border_width -> Properties_intf.logical_border_width
(** [normalize_logical_border_width v] is [v] in canonical form. *)

val normalize_logical_border_style :
  Properties_intf.logical_border_style -> Properties_intf.logical_border_style
(** [normalize_logical_border_style v] is [v] in canonical form. *)

val drop_initial_line_width :
  Properties_intf.border_width option -> Properties_intf.border_width option
(** [drop_initial_line_width w] is [None] when [w] is the initial [medium]
    width, and [w] otherwise. *)

val normalize_border :
  ?lossless:bool -> Properties_intf.border -> Properties_intf.border
(** [normalize_border ?lossless v] is [v] in canonical form. *)

val read_border_spacing : Cursor.t -> Properties_intf.border_spacing
(** [read_border_spacing t] reads a [border-spacing] value from [t]. *)
