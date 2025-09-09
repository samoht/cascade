(** Simple CSS parser API — direct and readable. *)

type t
(** [t] is the parser context. *)

exception Parse_error of string * string option * t
(** [Parse_error (expected, got, reader)] is raised on parse errors.
    - [expected]: what was expected (e.g. "number", "red")
    - [got]: what was actually found (e.g. Some "yellow"), None if unknown
    - [reader]: the reader state *)

(** {1 Core} *)

val of_string : string -> t
(** [of_string s] creates a parser from a string. *)

val is_done : t -> bool
(** [is_done t] is [true] when at end of input. *)

val position : t -> int
(** [position t] returns the current position in the input. *)

(** {1 Error Handling} *)

val err : ?got:string -> t -> string -> 'a
(** [err ?got t expected] raises a parse error. *)

val err_invalid : t -> string -> 'a
(** [err_invalid t what] raises a parse error for an invalid [what]. *)

val try_parse : (t -> 'a) -> t -> 'a option
(** [try_parse f t] tries [f t], returning [None] on failure. *)

(** {1 Characters & Strings} *)

val peek : t -> char option
(** [peek t] returns the current character without consuming it. *)

val peek_string : t -> int -> string
(** [peek_string t n] returns next [n] chars without consuming them. *)

val char : t -> char
(** [char t] reads and consumes one character. *)

val skip : t -> unit
(** [skip t] consumes one character. *)

val expect : char -> t -> unit
(** [expect c t] consumes [c] or raises [Parse_error]. *)

val expect_string : string -> t -> unit
(** [expect_string s t] consumes [s] or raises [Parse_error]. *)

val looking_at : t -> string -> bool
(** [looking_at t s] is [true] if input starts with [s]. *)

val while_ : t -> (char -> bool) -> string
(** [while_ t p] reads characters while predicate [p] holds. *)

val until : t -> char -> string
(** [until t c] reads until character [c]. *)

val css_value : stops:char list -> t -> string
(** [css_value ~stops t] reads a CSS value until [stops], handling nested
    functions and blocks. *)

val is_alpha : char -> bool
(** [is_alpha c] returns true if [c] is alphabetic. *)

val is_ident_start : char -> bool
(** [is_ident_start c] returns true if [c] can start a CSS identifier. *)

(** {1 CSS Tokens} *)

val ident : ?keep_case:bool -> t -> string
(** [ident ?keep_case t] reads a CSS identifier. By default identifiers are
    treated case-insensitively and the returned string is lowercased (ASCII).
    Pass [~keep_case:true] to preserve the original spelling when needed
    (e.g., when storing and re-emitting author-provided identifiers). *)

val token : t -> string
(** [token t] reads a non-whitespace token. *)

val string : t -> string
(** [string t] reads a quoted string (handles escapes). *)

val number : t -> float
(** [number t] reads a number. *)

val int : t -> int
(** [int t] reads an integer. *)

val ws : t -> unit
(** [ws t] skips whitespace and CSS comments. *)

(** {1 Structured Parsing} *)

val parens : (t -> 'a) -> t -> 'a
(** [parens f t] parses [f] between parentheses. *)

val braces : (t -> 'a) -> t -> 'a
(** [braces f t] parses [f] between braces. *)

val comma : t -> unit
(** [comma t] consumes comma with optional whitespace. *)

val slash : t -> unit
(** [slash t] consumes slash with optional whitespace. *)

(** {1 High-Level Combinators} *)

val take : int -> (t -> 'a) -> t -> 'a list
(** [take n parser t] parses up to [n] items. Requires at least 1. *)

val many : (t -> 'a) -> t -> 'a list * string option
(** [many parser t] repeatedly applies [parser] until failure. *)

val one_of : (t -> 'a) list -> t -> 'a
(** [one_of parsers t] tries each parser until one succeeds. *)

val optional : (t -> 'a) -> t -> 'a option
(** [optional parser t] tries [parser], returning [None] if it fails. *)

val enum : ?default:(t -> 'a) -> string -> (string * 'a) list -> t -> 'a
(** [enum ?default label cases t] reads identifier and matches against cases. If
    no match found, tries [default] function if provided. *)

val list : ?sep:(t -> unit) -> (t -> 'a) -> t -> 'a list
(** [list ~sep item t] parses items separated by [sep] (default: no separator).
*)

val pair : ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
(** [pair ~sep p1 p2 t] parses two items with optional separator. *)

val triple :
  ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
(** [triple ~sep p1 p2 p3 t] parses three items with optional separator. *)

val url : t -> string
(** [url t] parses [url(...)] content. *)

(* Note: CSS identifiers are matched case-insensitively by default. This API
   follows that convention by lowercasing identifiers unless [~keep_case:true]
   is provided. *)

(** {1 Debugging} *)
