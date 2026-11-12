(** List-style printers, normalisation and readers. *)

open Properties_intf

val pp_symbols_type : symbols_type Pp.t
(** [pp_symbols_type] pretty-prints a [symbols()] system type. *)

val pp_list_style_symbol : list_style_symbol Pp.t
(** [pp_list_style_symbol] pretty-prints one [symbols()] symbol. *)

val pp_list_style_type : list_style_type Pp.t
(** [pp_list_style_type] is the pretty-printer for [list_style_type]. *)

val pp_list_style_position : list_style_position Pp.t
(** [pp_list_style_position] is the pretty-printer for [list_style_position]. *)

val pp_list_style_image : list_style_image Pp.t
(** [pp_list_style_image] is the pretty-printer for [list_style_image]. *)

val pp_list_style_shorthand : list_style_shorthand Pp.t
(** [pp_list_style_shorthand] is the pretty-printer for [list_style_shorthand].
*)

val normalize_list_style : list_style -> list_style
(** [normalize_list_style v] is [v] with its shorthand components normalised. *)

val pp_list_style : list_style Pp.t
(** [pp_list_style] is the pretty-printer for [list_style]. *)

val read_symbols_type : Cursor.t -> symbols_type
(** [read_symbols_type t] parses a [symbols()] system type. *)

val read_list_style_symbol : Cursor.t -> list_style_symbol
(** [read_list_style_symbol t] parses one [symbols()] symbol. *)

val read_list_style_type : Cursor.t -> list_style_type
(** [read_list_style_type t] is the [list_style_type] parsed from [t]. *)

val read_list_style_position : Cursor.t -> list_style_position
(** [read_list_style_position t] is the [list_style_position] parsed from [t].
*)

val read_list_style_image : Cursor.t -> list_style_image
(** [read_list_style_image t] is the [list_style_image] parsed from [t]. *)

val read_list_style_shorthand : Cursor.t -> list_style_shorthand
(** [read_list_style_shorthand t] is the [list_style_shorthand] record parsed
    from [t]. *)

val read_list_style : Cursor.t -> list_style
(** [read_list_style t] is the [list_style] shorthand parsed from [t]. *)
