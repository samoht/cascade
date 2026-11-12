(** The [@font-face] descriptors with a value of their own (unicode-range,
    font-display and src) and the vendor font-smoothing properties. *)

val pp_font_display : Properties_intf.font_display Pp.t
(** [pp_font_display] prints a [font-display] value. *)

val pp_unicode_range : Properties_intf.unicode_range Pp.t
(** [pp_unicode_range] prints a [unicode-range] value. *)

val pp_webkit_font_smoothing : Properties_intf.webkit_font_smoothing Pp.t
(** [pp_webkit_font_smoothing] prints a [webkit-font-smoothing] value. *)

val pp_moz_osx_font_smoothing : Properties_intf.moz_osx_font_smoothing Pp.t
(** [pp_moz_osx_font_smoothing] prints a [moz-osx-font-smoothing] value. *)

val read_webkit_font_smoothing :
  Cursor.t -> Properties_intf.webkit_font_smoothing
(** [read_webkit_font_smoothing t] reads a [webkit-font-smoothing] value from
    [t]. *)

val read_moz_osx_font_smoothing :
  Cursor.t -> Properties_intf.moz_osx_font_smoothing
(** [read_moz_osx_font_smoothing t] reads a [moz-osx-font-smoothing] value from
    [t]. *)

val read_font_display : Cursor.t -> Properties_intf.font_display
(** [read_font_display t] reads a [font-display] value from [t]. *)

val read_unicode_range : Cursor.t -> Properties_intf.unicode_range
(** [read_unicode_range t] reads a [unicode-range] value from [t]. *)

val pp_font_src : Font_face.src Pp.t
(** [pp_font_src] prints an [@font-face] [src] descriptor value. *)

val read_font_src : Cursor.t -> Font_face.src
(** [read_font_src t] reads an [@font-face] [src] descriptor value from [t]. *)
