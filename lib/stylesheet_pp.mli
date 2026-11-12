(** Printing a stylesheet: every statement and at-rule printer, and the
    minify-time normalisation of [@charset] and shadows. *)

open Stylesheet_intf

val trim_unknown_at_prelude : string -> string
(** [trim_unknown_at_prelude prelude] is [prelude] without its trailing [;], [}]
    and whitespace. *)

val font_face_participates : font_face_descriptor list -> bool
(** [font_face_participates descriptors] holds when [descriptors] carry both
    [font-family] and [src]; the minified printer drops an [@font-face] that
    does not. *)

val string_of_layer_name : layer_name -> string
(** [string_of_layer_name name] is what {!pp_layer_name} prints. *)

val pp_layer_name : layer_name Pp.t
(** [pp_layer_name] prints a [<layer-name>], escaping each ident so it reads
    back. *)

val pp_import_rule : import_rule Pp.t
(** [pp_import_rule] prints an [@import] rule. *)

val pp_rule : rule Pp.t
(** [pp_rule] prints a style rule. *)

val pp_stylesheet : stylesheet Pp.t
(** [pp_stylesheet] prints a stylesheet, normalised first when minifying. *)

val pp : stylesheet Pp.t
(** [pp] is {!pp_stylesheet}. *)
