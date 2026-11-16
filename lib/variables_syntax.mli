(** Registered custom-property syntax and typed custom values: the [\@property]
    syntax descriptor reader and printer, typed variable declarations and
    handles, and the [var()] reference readers and scanners. *)

val custom_value_ident : string -> Component.t list
(** [custom_value_ident name] is a custom-property value made from one CSS
    ident. *)

val custom_value_var_empty_fallback : string -> Component.t list
(** [custom_value_var_empty_fallback name] is [var(--name,)] as components. *)

val string_of_custom_value : Component.t list -> string
(** [string_of_custom_value v] serializes a custom-property token stream. *)

val var_refs_in_components : string list -> Component.t list -> string list
(** [var_refs_in_components acc cs] is [acc] with the [--name] of every [var()]
    call anywhere in [cs] added. *)

val var_refs_in_value_string : string -> string list
(** [var_refs_in_value_string v] is the [--name]s referenced through a [var()]
    call anywhere in [v]. *)

val substitution_fn_in_value_string : string -> string Option.t
(** [substitution_fn_in_value_string v] is the name of the first [var()],
    [attr()] or [env()] call anywhere in [v]. *)

val element_relative_in_value_string : string -> string Option.t
(** [element_relative_in_value_string v] is the first element-relative unit in
    [v]. *)

val pp_syntax : 'a Variables_intf.syntax Pp.t
(** [pp_syntax] prints a syntax descriptor. *)

val pp_value : 'a Variables_intf.syntax -> 'a Pp.t
(** [pp_value syntax] prints a value according to [syntax]. *)

val typed_custom_property :
  ?layer:string ->
  string ->
  'a Variables_intf.syntax ->
  'a ->
  Declaration.declaration
(** [typed_custom_property ?layer name syntax v] is the custom-property
    declaration binding [name] to [v], spelled at [syntax]. *)

val read_syntax : Cursor.t -> Variables_intf.any_syntax
(** [read_syntax t] reads a syntax descriptor from [t]. *)

val read_value : Cursor.t -> 'a Variables_intf.syntax -> 'a
(** [read_value t syntax] reads a value of [syntax] from [t]. *)

val normalize_value : ?lossless:bool -> 'a Variables_intf.syntax -> 'a -> 'a
(** [normalize_value ?lossless syntax v] is [v] in canonical form for [syntax].
*)

val meta : unit -> ('t -> Values.meta) * (Values.meta -> 't option)
(** [meta ()] is a fresh injection and projection pair for values of type ['t]
    stored as a [Values.meta]. *)

val var :
  ?default:'a ->
  ?fallback:'a Values.fallback ->
  ?layer:string ->
  ?meta:Values.meta ->
  ?runtime:bool ->
  string ->
  'a Declaration.kind ->
  'a ->
  Declaration.declaration * 'a Values.var
(** [var ?default ?fallback ?layer ?meta ?runtime name kind v] is the
    declaration of [--name] with value [v] and its typed handle. *)

val pp_any_syntax : Variables_intf.any_syntax Pp.t
(** [pp_any_syntax] prints a syntax descriptor. *)

val read_any_syntax : Cursor.t -> Variables_intf.any_syntax
(** [read_any_syntax t] reads a syntax descriptor from [t]. *)

val read_reference_body_as_string : Cursor.t -> string * string option
(** [read_reference_body_as_string t] reads the arguments of a [var()] call from
    [t] as {!read_reference} does. *)

val read_reference : Cursor.t -> string * string option
(** [read_reference t] reads a [var()] call from [t]: the name without its [--]
    prefix and the fallback text. *)

val read_reference_body : (Cursor.t -> 'a) -> Cursor.t -> 'a Values.var
(** [read_reference_body read t] reads the arguments of a [var()] call from [t],
    the fallback with [read]. *)
