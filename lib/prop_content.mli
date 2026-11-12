(** Generated-content, counter and quotes printers and readers. *)

open Properties_intf

val pp_content : content Pp.t
(** [pp_content] is the pretty-printer for [content]. *)

val normalize_content : content -> content
(** [normalize_content c] is [c] with every quoted string folded onto [String],
    the one serialisation a string has under minify. *)

val pp_counter_item : counter_item Pp.t
(** [pp_counter_item] pretty-prints one [counter-reset]/[counter-increment]
    item. *)

val pp_counter_set : counter_set Pp.t
(** [pp_counter_set] is the pretty-printer for [counter_set]. *)

val pp_quotes : quotes Pp.t
(** [pp_quotes] is the pretty-printer for [quotes]. *)

val read_content : Cursor.t -> content
(** [read_content t] is the [content] parsed from [t]. *)

val read_counter_item : Cursor.t -> counter_item
(** [read_counter_item t] parses one [counter-reset]/[counter-increment] item.
*)

val read_counter_set : Cursor.t -> counter_set
(** [read_counter_set t] is the [counter_set] parsed from [t]. *)

val read_quotes : Cursor.t -> quotes
(** [read_quotes t] is the [quotes] parsed from [t]. *)
