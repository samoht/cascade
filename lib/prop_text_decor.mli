(** CSS Text Decoration 4 properties: the text-decoration and text-emphasis
    families, text-underline-position, text-shadow and -webkit-text-stroke. *)

open Properties_intf

val read_text_decoration_line : Cursor.t -> text_decoration_line
(** [read_text_decoration_line t] reads a [text_decoration_line] from [t]. *)

val read_text_decoration_style : Cursor.t -> text_decoration_style
(** [read_text_decoration_style t] reads a [text_decoration_style] from [t]. *)

val read_text_decoration_shorthand : Cursor.t -> text_decoration_shorthand
(** [read_text_decoration_shorthand t] reads a [text-decoration] shorthand value
    from [t]. *)

val read_text_decoration : Cursor.t -> text_decoration
(** [read_text_decoration t] reads a [text_decoration] from [t]. *)

val read_text_decoration_skip : Cursor.t -> text_decoration_skip
(** [read_text_decoration_skip t] reads a [text_decoration_skip] from [t]. *)

val read_text_decoration_skip_self : Cursor.t -> text_decoration_skip_self
(** [read_text_decoration_skip_self t] reads a [text_decoration_skip_self] from
    [t]. *)

val read_text_decoration_skip_box : Cursor.t -> text_decoration_skip_box
(** [read_text_decoration_skip_box t] reads a [text_decoration_skip_box] from
    [t]. *)

val read_text_decoration_skip_inset : Cursor.t -> text_decoration_skip_inset
(** [read_text_decoration_skip_inset t] reads a [text_decoration_skip_inset]
    from [t]. *)

val read_text_decoration_skip_space : Cursor.t -> text_decoration_skip_space
(** [read_text_decoration_skip_space t] reads a [text_decoration_skip_space]
    from [t]. *)

val read_text_decoration_skip_spaces : Cursor.t -> text_decoration_skip_spaces
(** [read_text_decoration_skip_spaces t] reads a [text_decoration_skip_spaces]
    from [t]. *)

val pp_text_emphasis_fill : Pp.ctx -> text_emphasis_fill -> unit
(** [pp_text_emphasis_fill] prints a [text_emphasis_fill]. *)

val pp_text_emphasis_shape : Pp.ctx -> text_emphasis_shape -> unit
(** [pp_text_emphasis_shape] prints a [text_emphasis_shape]. *)

val pp_text_emphasis_style : text_emphasis_style Pp.t
(** [pp_text_emphasis_style] prints a [text_emphasis_style]. *)

val pp_text_emphasis : text_emphasis Pp.t
(** [pp_text_emphasis] prints a [text_emphasis]. *)

val read_text_emphasis_fill : Cursor.t -> text_emphasis_fill
(** [read_text_emphasis_fill t] reads a [text_emphasis_fill] from [t]. *)

val read_text_emphasis_shape : Cursor.t -> text_emphasis_shape
(** [read_text_emphasis_shape t] reads a [text_emphasis_shape] from [t]. *)

val read_text_emphasis_style : Cursor.t -> text_emphasis_style
(** [read_text_emphasis_style t] reads a [text_emphasis_style] from [t]. *)

val read_text_emphasis : Cursor.t -> text_emphasis
(** [read_text_emphasis t] reads a [text_emphasis] from [t]. *)

val read_text_emphasis_skip_keyword : Cursor.t -> text_emphasis_skip_keyword
(** [read_text_emphasis_skip_keyword t] reads a [text_emphasis_skip_keyword]
    from [t]. *)

val read_text_emphasis_skip : Cursor.t -> text_emphasis_skip
(** [read_text_emphasis_skip t] reads a [text_emphasis_skip] from [t]. *)

val pp_text_emphasis_line : text_emphasis_line Pp.t
(** [pp_text_emphasis_line] prints a [text_emphasis_line]. *)

val pp_text_emphasis_side : text_emphasis_side Pp.t
(** [pp_text_emphasis_side] prints a [text_emphasis_side]. *)

val pp_text_emphasis_skip_keyword : text_emphasis_skip_keyword Pp.t
(** [pp_text_emphasis_skip_keyword] prints a [text_emphasis_skip_keyword]. *)

val pp_text_emphasis_skip : text_emphasis_skip Pp.t
(** [pp_text_emphasis_skip] prints a [text_emphasis_skip]. *)

val pp_text_emphasis_position : text_emphasis_position Pp.t
(** [pp_text_emphasis_position] prints a [text_emphasis_position]. *)

val read_text_emphasis_line : Cursor.t -> text_emphasis_line
(** [read_text_emphasis_line t] reads a [text_emphasis_line] from [t]. *)

val read_text_emphasis_side : Cursor.t -> text_emphasis_side
(** [read_text_emphasis_side t] reads a [text_emphasis_side] from [t]. *)

val read_text_emphasis_position : Cursor.t -> text_emphasis_position
(** [read_text_emphasis_position t] reads a [text_emphasis_position] from [t].
*)

val pp_text_underline_position_keyword : text_underline_position_keyword Pp.t
(** [pp_text_underline_position_keyword] prints a
    [text_underline_position_keyword]. *)

val pp_text_underline_position : text_underline_position Pp.t
(** [pp_text_underline_position] prints a [text_underline_position]. *)

val read_text_underline_position_keyword :
  Cursor.t -> text_underline_position_keyword
(** [read_text_underline_position_keyword t] reads a
    [text_underline_position_keyword] from [t]. *)

val read_text_underline_position : Cursor.t -> text_underline_position
(** [read_text_underline_position t] reads a [text_underline_position] from [t].
*)

val normalize_text_decoration :
  ?lossless:bool -> text_decoration -> text_decoration
(** [normalize_text_decoration ?lossless v] is [v] in its canonical spelling;
    [lossless] keeps colour channels unrounded. *)

val normalize_text_emphasis : ?lossless:bool -> text_emphasis -> text_emphasis
(** [normalize_text_emphasis ?lossless v] is [v] in its canonical spelling;
    [lossless] keeps colour channels unrounded. *)

val normalize_text_shadow : ?lossless:bool -> text_shadow -> text_shadow
(** [normalize_text_shadow ?lossless v] is [v] in its canonical spelling;
    [lossless] keeps colour channels unrounded. *)

val pp_text_decoration_line : text_decoration_line Pp.t
(** [pp_text_decoration_line] prints a [text_decoration_line]. *)

val pp_text_decoration_style : text_decoration_style Pp.t
(** [pp_text_decoration_style] prints a [text_decoration_style]. *)

val pp_text_decoration_shorthand : text_decoration_shorthand Pp.t
(** [pp_text_decoration_shorthand] prints a [text-decoration] shorthand. *)

val pp_text_decoration : text_decoration Pp.t
(** [pp_text_decoration] prints a [text_decoration]. *)

val pp_text_decoration_skip : text_decoration_skip Pp.t
(** [pp_text_decoration_skip] prints a [text_decoration_skip]. *)

val pp_text_decoration_skip_self : text_decoration_skip_self Pp.t
(** [pp_text_decoration_skip_self] prints a [text_decoration_skip_self]. *)

val pp_text_decoration_skip_box : text_decoration_skip_box Pp.t
(** [pp_text_decoration_skip_box] prints a [text_decoration_skip_box]. *)

val pp_text_decoration_skip_inset : text_decoration_skip_inset Pp.t
(** [pp_text_decoration_skip_inset] prints a [text_decoration_skip_inset]. *)

val pp_text_decoration_skip_space : text_decoration_skip_space Pp.t
(** [pp_text_decoration_skip_space] prints a [text_decoration_skip_space]. *)

val pp_text_decoration_skip_spaces : text_decoration_skip_spaces Pp.t
(** [pp_text_decoration_skip_spaces] prints a [text_decoration_skip_spaces]. *)

val pp_text_shadow : text_shadow Pp.t
(** [pp_text_shadow] prints a [text_shadow]. *)

val pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t
(** [pp_text_decoration_skip_ink] prints a [text_decoration_skip_ink]. *)

val pp_webkit_text_stroke : webkit_text_stroke Pp.t
(** [pp_webkit_text_stroke] prints a [webkit_text_stroke]. *)

val read_webkit_text_stroke : Cursor.t -> webkit_text_stroke
(** [read_webkit_text_stroke t] reads a [webkit_text_stroke] from [t]. *)

val read_text_decoration_skip_ink : Cursor.t -> text_decoration_skip_ink
(** [read_text_decoration_skip_ink t] reads a [text_decoration_skip_ink] from
    [t]. *)

val read_text_shadow : Cursor.t -> text_shadow
(** [read_text_shadow t] reads a [text_shadow] from [t]. *)

val read_text_shadows : Cursor.t -> text_shadow list
(** [read_text_shadows t] reads a [text_shadow list] from [t]. *)

val text_decoration_shorthand :
  ?lines:text_decoration_line list ->
  ?style:text_decoration_style ->
  ?color:Values.color ->
  ?thickness:Values.length ->
  unit ->
  text_decoration
(** [text_decoration_shorthand ?lines ?style ?color ?thickness ()] is the
    [text-decoration] shorthand with the given components; [lines] defaults to
    none. *)
