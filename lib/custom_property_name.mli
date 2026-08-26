(** Internal custom-property name classification and validation. *)

val has_prefix : string -> bool
(** [has_prefix name] reports whether [name] starts with [--]. This is a lexical
    classification only: the bare reserved keyword [--] has the prefix but is
    not a valid custom-property name. *)

val is_valid : string -> bool
(** [is_valid name] reports whether [name] is a custom-property name: a
    [<dashed-ident>] other than the bare reserved keyword [--]. *)

val add_prefix : string -> string
(** [add_prefix name] adds [--] unless it is already present. *)

val strip_prefix : string -> string
(** [strip_prefix name] removes [--] when present. *)
