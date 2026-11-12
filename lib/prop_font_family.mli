(** The [font-family] property: family names, the generic and CSS-wide keywords,
    and the reader, printer and normal form of a family list. *)

val unquote_font_family_strings : Component.t list -> Component.t list
(** [unquote_font_family_strings components] rewrites each [<string>] whose
    content is an identifier sequence as that sequence, and is [components]
    itself when none is. *)

val pp_font_family : Properties_intf.font_family Pp.t
(** [pp_font_family] prints a [font-family] value. *)

val pp_font_family_name : Properties_intf.font_family Pp.t
(** [pp_font_family_name] prints a [font-family-name] value. *)

val read_font_family_name : Cursor.t -> Properties_intf.font_family
(** [read_font_family_name t] reads a [font-family-name] value from [t]. *)

val is_font_family_name_value : Properties_intf.font_family -> bool
(** [is_font_family_name_value v] is [true] when [v] is a family name rather
    than a generic family, a CSS-wide keyword, a list or an invalid value. *)

val read_font_family : Cursor.t -> Properties_intf.font_family
(** [read_font_family t] reads a [font-family] value from [t]. *)

val components_have_generic_family : Component.t list -> bool
(** [components_have_generic_family components] is [true] when a bare ident in
    [components] is a generic family name. *)

val long_generic_family_start : Cursor.t -> bool
(** [long_generic_family_start r] is [true] when the next comma-separated item
    of [r] holds several tokens and starts with a generic family name. *)

val normalize_font_family :
  Properties_intf.font_family -> Properties_intf.font_family
(** [normalize_font_family v] is [v] in canonical form. *)
