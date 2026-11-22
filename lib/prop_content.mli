(** Generated-content, counter and quotes printers and readers. *)

open Properties_intf

val pp_content : content Pp.t
(** [pp_content] is the pretty-printer for [content]. *)

val normalize_content : lossless:bool -> content -> content
(** [normalize_content ~lossless c] is [c] with every quoted string folded onto
    [String], the one serialisation a string has under minify, and every image
    in its normal form. *)

val pp_counter_item : counter_item Pp.t
(** [pp_counter_item] pretty-prints one [counter-reset]/[counter-increment]
    item. *)

val pp_counter_set : counter_set Pp.t
(** [pp_counter_set] is the pretty-printer for [counter_set]. *)

val pp_quotes : quotes Pp.t
(** [pp_quotes] is the pretty-printer for [quotes]. *)

val read_content : Cursor.t -> content
(** [read_content t] is the [content] parsed from [t]. *)

val read_counter_item : reversed:bool -> Cursor.t -> counter_item
(** [read_counter_item ~reversed t] parses one
    [counter-reset]/[counter-increment] item. [reversed] admits a
    [reversed( <counter-name> )] item, which CSS Lists 3 sec. 4.1 grants
    [counter-reset] alone. *)

val read_counter_set : reversed:bool -> Cursor.t -> counter_set
(** [read_counter_set ~reversed t] is the [counter_set] parsed from [t];
    [reversed] is as for {!read_counter_item}. *)

val read_quotes : Cursor.t -> quotes
(** [read_quotes t] is the [quotes] parsed from [t]. *)
