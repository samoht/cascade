(** Typed property values: the per-family readers {!read_value} dispatches to.
*)

open Declaration_intf

val validate_no_extra_tokens : Cursor.t -> unit
(** [validate_no_extra_tokens t] fails unless the value ends at the next [;],
    [!] or the end of [t]. *)

val canonical_prop_name : 'a Properties.property -> string
(** [canonical_prop_name prop] is the unminified name of [prop], the spelling it
    parses back from and a diagnostic names. *)

val read_value : 'a Properties.property -> Cursor.t -> declaration
(** [read_value prop t] reads the value of [prop] from [t] as a declaration. *)
