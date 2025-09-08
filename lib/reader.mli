(** Simple CSS parser API — direct and readable. *)

type t
(** [t] is the parser context. *)

exception Parse_error of string * t
(** [Parse_error (msg, reader)] is raised on parse errors with the reader state.
*)

(** {1 Error Helpers} *)

val err_invalid : t -> string -> 'a
(** [err_invalid t what] raises a parse error for an invalid [what]. *)

(** {1 Running Parsers} *)

val of_string : string -> t
(** [of_string s] creates a parser from a string. *)

val is_done : t -> bool
(** [is_done t] is [true] when at end of input. *)

(** {1 Looking Ahead} *)

val peek : t -> char option
(** [peek t] returns the current character without consuming it. *)

val peek_string : t -> int -> string
(** [peek_string t n] returns the next [n] characters without consuming them. *)

val looking_at : t -> string -> bool
(** [looking_at t s] is [true] if input at [t] starts with [s]. *)

(** {1 Reading Characters} *)

val skip : t -> unit
(** [skip t] consumes one character or raises on EOF. *)

val skip_n : t -> int -> unit
(** [skip_n t n] consumes [n] characters or raises on EOF. *)

val char : t -> char
(** [char t] reads and consumes one character. *)

val expect : t -> char -> unit
(** [expect t c] consumes [c] or raises [Parse_error]. *)

val expect_string : t -> string -> unit
(** [expect_string t s] consumes [s] or raises [Parse_error]. *)

(** {1 Reading Strings} *)

val while_ : t -> (char -> bool) -> string
(** [while_ t p] reads characters while predicate [p] holds. *)

val until : t -> char -> string
(** [until t c] reads until character [c] (not including it). *)

val until_string : t -> string -> string
(** [until_string t s] reads until string [s] (not including it). *)

val ident : t -> string
(** [ident t] reads a CSS identifier. *)

val ident_lc : t -> string
(** [ident_lc t] reads an identifier and lowercases it. *)

val string : t -> string
(** [string t] reads a quoted string (handles escapes). *)

val number : t -> float
(** [number t] reads a number (integer or float). *)

val int : t -> int
(** [int t] reads an integer. *)

(** {1 Whitespace} *)

val skip_ws : t -> unit
(** [skip_ws t] skips whitespace and CSS comments. *)

val ws : t -> unit
(** [ws t] is an alias for [skip_ws]. *)

val is_ws : char -> bool
(** [is_ws c] returns true if [c] is a whitespace character. *)

val is_token_separator : char -> bool
(** [is_token_separator c] returns true if [c] is a CSS token separator
    (whitespace, semicolon, closing paren/brace, comma, or exclamation). *)

val token : t -> string
(** [token t] reads a non-whitespace CSS token, stopping at separators. *)

(** {1 Backtracking} *)

val save : t -> unit
(** [save t] saves current position for potential backtracking. *)

val restore : t -> unit
(** [restore t] restores to last saved position. *)

val commit : t -> unit
(** [commit t] discards the last saved position. *)

val try_parse : (t -> 'a) -> t -> 'a option
(** [try_parse f t] tries [f t], restoring position and returning [None] on
    failure. *)

(** {1 Structured Parsing} *)

val between : t -> char -> char -> (t -> 'a) -> 'a
(** [between t open_c close_c f] parses using [f] between [open_c] and
    [close_c]. *)

val parens : t -> (t -> 'a) -> 'a
(** [parens t f] parses between parentheses. *)

val brackets : t -> (t -> 'a) -> 'a
(** [brackets t f] parses between brackets. *)

val braces : t -> (t -> 'a) -> 'a
(** [braces t f] parses between braces. *)

val separated : t -> (t -> 'a) -> (t -> unit) -> 'a list
(** [separated t parse_item parse_sep] parses a list separated by [parse_sep].
*)

(** {1 Function Call Helpers} *)

val comma : t -> unit
(** [comma t] consumes a comma with surrounding optional whitespace. *)

val slash : t -> unit
(** [slash t] consumes a slash with surrounding optional whitespace. *)

val pair : ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
(** [pair ~sep p1 p2 t] parses [p1], optional [sep], then [p2]. Default [sep]
    does nothing. *)

val triple :
  ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
(** [triple ~sep p1 p2 p3 t] parses [p1], [sep], [p2], [sep], [p3]. Default
    [sep] does nothing. *)

val list : ?sep:(t -> unit) -> (t -> 'a) -> t -> 'a list
(** [list ~sep item t] parses a list of [item] separated by [sep]. *)

val call : string -> (t -> 'a) -> t -> 'a
(** [call name p t] expects the ident [name] then parses [p] inside parentheses.
*)

val call_2 : string -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
(** [call_2 name p1 p2 t] parses [name(a, b)] with comma separation. *)

val call_3 : string -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
(** [call_3 name p1 p2 p3 t] parses [name(a, b, c)] with comma separation. *)

val call_list : string -> (t -> 'a) -> t -> 'a list
(** [call_list name item t] parses [name(a, b, ...)] as a comma-separated list.
*)

val url : t -> string
(** [url t] parses [url(...)] returning the inner content. Supports quoted or
    unquoted content (unquoted is trimmed). *)

(** {1 Character Predicates} *)

val is_ident_start : char -> bool
(** [is_ident_start c] is [true] if [c] can start an identifier. *)

val pp : t -> string
(** [pp] pretty-printer for parser state. *)

val context_string : ?window:int -> t -> string * string
(** [context_string ?window t] returns (before, after) strings around the
    current position. [window] defaults to 40 characters. *)

val position : t -> int
(** [position t] returns the current position in the input. *)

val length : t -> int
(** [length t] returns the total length of the input. *)
