(** Readers for the at-rules a grammar reads without descriptors or nested
    statements: [@charset], [@import], [@namespace], [@keyframes] in every
    spelling, the [@-moz-document] prelude and an unknown at-rule kept as raw
    text, with the item-level recovery the body readers share. *)

open Stylesheet_intf

val read_charset : Cursor.t -> statement
(** [read_charset r] reads an [@charset] statement. *)

val read_layer_name_component : Cursor.t -> layer_name
(** [read_layer_name_component r] reads a [<layer-name>], refusing a CSS-wide
    keyword. *)

val read_import : Cursor.t -> statement
(** [read_import r] reads an [@import] statement. *)

val read_import_rule : Cursor.t -> import_rule
(** [read_import_rule r] reads an [@import] prelude, dropping the authored
    spelling of its url. *)

val read_namespace : Cursor.t -> statement
(** [read_namespace r] reads an [@namespace] statement. *)

val skip_past_rule : Cursor.t -> unit
(** [skip_past_rule r] discards the rule [r] sits on, through its block or its
    [;]. *)

val skip_invalid_item : Cursor.t -> unit
(** [skip_invalid_item r] discards the declaration or at-rule [r] sits on. *)

val read_items_with_recovery :
  skip:(Cursor.t -> unit) ->
  construct:Error.Recovery.construct ->
  (Cursor.t -> 'a -> [< `Done of 'b | `More of 'a ]) ->
  Cursor.t ->
  'a ->
  'b
(** [read_items_with_recovery ~skip ~construct step r init] reads a body one
    item at a time with [step]; an item that fails is dropped with [skip] and
    reported as a lost [construct]. *)

val read_keyframes : Cursor.t -> statement
(** [read_keyframes r] reads an [@keyframes] rule. *)

val read_webkit_keyframes : Cursor.t -> statement
(** [read_webkit_keyframes r] is {!read_keyframes} for [@-webkit-keyframes]. *)

val read_moz_keyframes : Cursor.t -> statement
(** [read_moz_keyframes r] is {!read_keyframes} for [@-moz-keyframes]. *)

val read_moz_document_condition : Cursor.t -> moz_document_condition
(** [read_moz_document_condition r] reads one URL-matching function of an
    [@-moz-document] prelude. *)

val read_unknown_at_rule : string -> Cursor.t -> statement
(** [read_unknown_at_rule name r] reads the at-rule [name] as raw prelude and
    block text, closing whatever the text leaves open. *)
