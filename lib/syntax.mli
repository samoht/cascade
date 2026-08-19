(** Shared byte-level CSS syntax helpers.

    Predicates and small string operations used across the parser, lexer,
    selector, and the container/media/supports/font-face readers. *)

val is_ascii_ident_start : char -> bool
(** [is_ascii_ident_start c] returns true if [c] is a valid ASCII ident-start
    code point: [a-z], [A-Z], [_], or [-]. *)

val is_ascii_ident_continue : char -> bool
(** [is_ascii_ident_continue c] returns true if [c] is a valid ASCII
    ident-continue code point: [is_ascii_ident_start c] or [0-9]. *)

val is_hex : char -> bool
(** [is_hex c] returns true if [c] is a hexadecimal digit ([0-9], [a-f], [A-F]).
*)

val url_needs_quotes : string -> bool
(** [url_needs_quotes s] returns true when serializing [s] as a bare [url(...)]
    argument would change tokenization; the caller must wrap [s] in quotes. *)

val split_top_level_colon :
  Component.t list -> (Component.t list * Component.t list) option
(** [split_top_level_colon cvs] splits a component-value sequence at the first
    top-level [:] token, returning [Some (before, after)]; the colon itself is
    dropped. Returns [None] if no top-level colon is present. *)

val strip_url_suffix : string -> string
(** [strip_url_suffix url] strips a query string ([?...]) and fragment ([#...])
    from [url]. Used when resolving [@import] URLs against the host filesystem,
    where neither suffix is meaningful. *)

val url_file_path : string -> string
(** [url_file_path url] decodes the path component of [url] for a filesystem
    lookup. *)

val is_remote_url : string -> bool
(** [is_remote_url url] is true when [url] has a scheme or authority. *)
