(** Simple CSS parser API — direct and readable. *)

type t
(** [t] is the parser context. *)

type parse_error = {
  message : string;
  got : string option;
  position : int;
  filename : string;
  context_window : string;
  marker_pos : int;
  callstack : string list;
}
(** Parse error information with structured details. *)

exception Parse_error of parse_error
(** [Parse_error error] is raised on parse errors with structured debugging
    information. *)

val pp_parse_error : parse_error -> string
(** [pp_parse_error error] formats a parse error as a string, including call
    stack if available. *)

(** {1 Core} *)

val of_string : string -> t
(** [of_string s] creates a parser from a string. *)

val is_done : t -> bool
(** [is_done t] is [true] when at end of input. *)

val position : t -> int
(** [position t] returns the current position in the input. *)

val context_window : ?before:int -> ?after:int -> t -> string * int
(** [context_window ~before ~after t] returns [(context, marker_pos)] where
    [context] is text around the current position and [marker_pos] indicates
    where in the context the current position is. Used for better error
    messages. *)

(** {1 Call Stack Management} *)

val push_context : t -> string -> unit
(** [push_context t context] pushes a parsing context onto the call stack. *)

val pop_context : t -> unit
(** [pop_context t] pops the top parsing context from the call stack. *)

val with_context : t -> string -> (unit -> 'a) -> 'a
(** [with_context t context f] runs [f] with [context] pushed onto the call
    stack, automatically popping it when done (even if [f] raises an exception).
*)

val callstack : t -> string list
(** [callstack t] returns the current parsing call stack for debugging. *)

(** {1 Error Handling} *)

val err : ?got:string -> t -> string -> 'a
(** [err ?got t expected] raises a parse error. *)

val err_eof : t -> 'a
(** [err_eof t] raises an "unexpected end of input" error. *)

val err_expected : t -> string -> 'a
(** [err_expected t what] raises an "expected [what]" error. *)

val err_invalid_number : t -> 'a
(** [err_invalid_number t] raises an "invalid number" error. *)

val err_invalid : t -> string -> 'a
(** [err_invalid t what] raises a parse error for an invalid [what]. *)

(** {1 Error Utilities} *)

val with_filename : parse_error -> string -> parse_error
(** [with_filename error filename] returns error with updated filename. *)

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

val is_digit : char -> bool
(** [is_digit c] returns true if [c] is a digit (0-9). *)

(** {1 CSS Tokens} *)

val ident : ?keep_case:bool -> t -> string
(** [ident ?keep_case t] reads a CSS identifier. By default identifiers are
    treated case-insensitively and the returned string is lowercased (ASCII).
    Pass [~keep_case:true] to preserve the original spelling when needed (e.g.,
    when storing and re-emitting author-provided identifiers). *)

val token : t -> string
(** [token t] reads a non-whitespace token. *)

val string : ?trim:bool -> t -> string
(** [string ?trim t] reads a quoted string (handles escapes). If [trim] is true
    (default: false), trims whitespace from the result. *)

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

val consume_if : char -> t -> bool
(** [consume_if c t] consumes [c] if present, returns [true] if consumed. *)

val comma_opt : t -> bool
(** [comma_opt t] consumes an optional comma (with surrounding whitespace).
    Returns [true] if a comma was consumed. *)

val slash_opt : t -> bool
(** [slash_opt t] consumes an optional slash (with surrounding whitespace).
    Returns [true] if a slash was consumed. *)

(** {1 High-Level Combinators} *)

val take : int -> (t -> 'a) -> t -> 'a list
(** [take n parser t] parses up to [n] items. Requires at least 1. *)

val many : (t -> 'a) -> t -> 'a list * string option
(** [many parser t] repeatedly applies [parser] until failure. *)

val one_of : (t -> 'a) list -> t -> 'a
(** [one_of parsers t] tries each parser until one succeeds. *)

val option : (t -> 'a) -> t -> 'a option
(** [option parser t] tries [parser], returning [None] if it fails. *)

val enum : ?default:(t -> 'a) -> string -> (string * 'a) list -> t -> 'a
(** [enum ?default label cases t] reads identifier and matches against cases. If
    no match is found and [~default] is provided, the input position is not
    consumed and [default t] is tried from the original position. *)

val list :
  ?sep:(t -> unit) -> ?at_least:int -> ?at_most:int -> (t -> 'a) -> t -> 'a list
(** [list ~sep ~at_least ~at_most item t] parses items separated by [sep]
    (default: no separator). Enforces optional cardinality constraints. *)

val pair : ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
(** [pair ~sep p1 p2 t] parses two items with optional separator. *)

val triple :
  ?sep:(t -> unit) -> (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
(** [triple ~sep p1 p2 p3 t] parses three items with optional separator. *)

val url : t -> string
(** [url t] parses [url(...)] content. *)

val call : string -> t -> (t -> 'a) -> 'a
(** [call name p t] parses a case-insensitive function call [name(...)] and
    applies [p] to the contents between parentheses with surrounding whitespace
    handled. *)

val enum_calls : ?default:(t -> 'a) -> (string * (t -> 'a)) list -> t -> 'a
(** [enum_calls ?default cases t] reads a case-insensitive function name and
    dispatches to the matching parser, parsing the body between parentheses. If
    no match is found and [~default] is provided, the input position is not
    consumed and [default t] is tried from the original position. *)

val enum_or_calls :
  ?default:(t -> 'a) ->
  string ->
  (string * 'a) list ->
  calls:(string * (t -> 'a)) list ->
  t ->
  'a
(** [enum_or_calls ?default label idents ~calls t] handles values that can be
    either a keyword ([enum]) or a function call ([enum_calls]). It peeks after
    the identifier: if a '(' follows, dispatches to [enum_calls]; otherwise
    matches against [idents]. If no match is found and [~default] is provided,
    the input position is restored before trying [default t]. *)

val fold_many :
  (t -> 'a) -> init:'s -> f:('s -> 'a -> 's) -> t -> 's * string option
(** [fold_many parser ~init ~f t] like [many] but folds into an accumulator as
    it parses. Returns the final accumulator and the last error (if any). *)

val number_with_unit : t -> float * string
(** [number_with_unit t] parses a number followed by a unit identifier (e.g.,
    "12px", "3.5rem"). Returns [(number, unit_string)]. *)

val unit : string -> t -> float
(** [unit expected t] parses a number followed by the expected unit identifier.
    Returns just the number if the unit matches, raises Parse_error otherwise.
*)
