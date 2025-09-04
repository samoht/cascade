(** Simple CSS parser API — direct and readable. *)

type t
(** [t] is the parser context. *)

exception Parse_error of string
(** [Parse_error msg] is raised on parse errors. *)

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

(** {1 CSS-Specific Helpers} *)

val hex_color : t -> string
(** [hex_color t] reads a hex color without '#'. *)

val percentage : t -> float
(** [percentage t] reads a percentage value (without '%'). *)

val dimension : t -> float * string
(** [dimension t] reads a number with unit, e.g., ["10px"] → [(10.0, "px")]. *)

val angle : t -> float * string
(** [angle t] reads an angle value, e.g., ["45deg"] → [(45.0, "deg")]. *)

val duration : t -> float * string
(** [duration t] reads a duration, e.g., ["2s"] → [(2.0, "s")], ["500ms"] →
    [(500.0, "ms")]. *)

val color_keyword : t -> string option
(** [color_keyword t] reads CSS color keywords like "red", "blue", etc. *)

val rgb_function : t -> (int * int * int * float option) option
(** [rgb_function t] reads rgb() or rgba() function calls. *)

(** {1 Character Predicates} *)

val is_ident_start : char -> bool
(** [is_ident_start c] is [true] if [c] can start an identifier. *)

val pp : t Fmt.t
(** [pp] pretty-printer for parser state. *)
