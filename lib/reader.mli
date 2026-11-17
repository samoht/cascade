(** Character cursor over CSS source text.

    {!Lexer} drives a reader to produce tokens; {!Cursor} is the equivalent
    cursor one layer up, over the component values {!Parser} builds from those
    tokens. CSS-level reading lives there, so this module peeks, skips and
    reports where it stopped, and nothing here reads a CSS construct.

    Cascade parses already-decoded UTF-8 text. It does not implement the CSS
    Syntax 3 (ED) sec. 3.2 byte-stream decoding layer: BOM handling, HTTP or
    environment charset fallback, and exact [@charset "...";] byte sniffing are
    caller responsibilities before constructing a {!Reader.t}. *)

type t
(** [t] is the cursor: an input string and a byte position in it. *)

val pp : t Pp.t
(** [pp] pretty-prints the parser context with current position and a small
    context window for debugging. *)

(** {1 Core} *)

val of_string : ?enforce_spec:bool -> string -> t
(** [of_string s] creates a parser from an already-decoded UTF-8 string.
    [enforce_spec] (default [false]) restricts non-ASCII identifiers to the CSS
    Syntax 3 (ED) sec. 4.2 range list. *)

val enforce_spec : t -> bool
(** [enforce_spec t] is the identifier rule [t] was built with; see
    {!val-of_string}. *)

val source : t -> string
(** [source t] is the input [t] was built from after CSS Syntax 3 sec. 3.3
    preprocessing, which every position of [t] indexes. *)

val indexed_source : t -> Error.Source.t
(** [indexed_source t] is {!source} with the index of its positions. Every error
    located in [t]'s input carries this one value, so the input is walked once
    however many positions are asked for. *)

type origin
(** [origin] maps positions in {!source} back to the string {!val-of_string} was
    given. *)

val origin : t -> origin
(** [origin t] is the map for [t]'s input. *)

val original_offset : origin -> int -> int
(** [original_offset o i] is the byte offset in the original string of position
    [i] in {!source}. Position [0] lands after a leading BOM, a position after a
    CRLF lands after both bytes, and a position inside the U+FFFD that replaced
    a NUL lands on that NUL. Raises [Invalid_argument] when [i] is negative or
    past the end of {!source}. *)

val is_done : t -> bool
(** [is_done t] is [true] when at end of input. *)

val peek_utf8 : t -> (int * int) option
(** [peek_utf8 t] decodes the UTF-8 code point starting at the current position.
    Returns [Some (code_point, byte_length)] or [None] at EOF or on a malformed
    sequence. Byte length is in [[1..4]]. *)

val peek_utf8_at : t -> int -> (int * int) option
(** [peek_utf8_at t off] decodes the UTF-8 code point at [position t + off],
    without advancing. Returns [None] when [off] is negative or outside the
    remaining input. *)

val skip_utf8 : t -> unit
(** [skip_utf8 t] advances past the next UTF-8 code point. If the lead byte is
    malformed, advances by one byte. *)

val position : t -> int
(** [position t] returns the current position in the input. *)

val context_window : ?before:int -> ?after:int -> t -> string * int
(** [context_window ~before ~after t] returns [(context, marker_pos)] where
    [context] is text around the current position and {!field-marker_pos} counts
    the characters of [context] before it. Used for better error messages.

    A character is one Unicode scalar value, which a combining mark and a wide
    glyph each make approximate on a terminal. [before] and [after] are target
    radiuses rather than caps: a boundary falling inside a UTF-8 sequence moves
    outward to the lead byte, so [context] is never a truncated code point. *)

(** {1 Characters} *)

val peek : t -> char option
(** [peek t] returns the current character without consuming it. *)

val peek_at : t -> int -> char option
(** [peek_at t off] returns the character at [position t + off] without
    consuming it. *)

val peek_byte : t -> int
(** [peek_byte t] returns the current byte as an [int 0..255], or [-1] at end of
    input. Non-allocating variant of {!peek}. *)

val peek_byte_at : t -> int -> int
(** [peek_byte_at t off] returns the byte at [position t + off] as an
    [int 0..255], or [-1] at end of input. Non-allocating variant of {!peek_at}.
*)

val peek_string : t -> int -> string
(** [peek_string t n] returns next [n] chars without consuming them. *)

val skip : t -> unit
(** [skip t] consumes one character.

    @raise Invalid_argument if [t] is at end of input. *)

val looking_at : t -> string -> bool
(** [looking_at t s] is [true] if input starts with [s]. *)

(** {1:deprecated Deprecated, removed in 1.4.0}

    The 1.2 error API, with its 1.2 behaviour. {!err} and its variants raise
    this module's own {!exception-Parse_error}, which no reader in the library
    raises and {!Error.catch} does not catch: the readers fail with
    {!Error.Parse_error}, and a public [of_string] returns that error as a
    result. *)

include sig
  [@@@alert "-deprecated"]

  type parse_error = {
    message : string;
    got : string option;
    position : int;
    filename : string;
    line : int;
    col : int;
    context_window : string;
    marker_pos : int;
    callstack : string list;
  }
  [@@deprecated "use Error.t; removed in 1.4.0"]
  (** Parse error information with structured details. {!field-filename} names
      the source: an error raised by {!err} carries ["<CSS input>"] until
      {!with_filename} replaces it. {!field-position} is a byte offset in
      {!source}, {!field-line} and {!field-col} are the 1-based line and column
      of that offset, and {!field-marker_pos} counts the characters of
      {!field-context_window} before it. A column is one Unicode scalar value,
      as for {!context_window}. *)

  exception
    Parse_error of parse_error
        [@deprecated "use Error.Parse_error; removed in 1.4.0"]
  (** [Parse_error error] is raised by {!err} and its variants. *)

  val pp_parse_error : parse_error -> string
  [@@deprecated "use Error.to_string; removed in 1.4.0"]
  (** [pp_parse_error error] formats [error], locating it as
      [filename:line:column], then the call stack when there is one, then the
      context window with a caret under the error's position. *)

  val push_context : t -> string -> unit
  [@@deprecated "use Error.with_context; removed in 1.4.0"]
  (** [push_context t context] pushes [context] onto [t]'s call stack. *)

  val pop_context : t -> unit
  [@@deprecated "use Error.with_context; removed in 1.4.0"]
  (** [pop_context t] pops the innermost context off [t]'s call stack, if any.
  *)

  val with_context : t -> string -> (unit -> 'a) -> 'a
  [@@deprecated "use Error.with_context; removed in 1.4.0"]
  (** [with_context t context f] runs [f ()] with [context] pushed onto [t]'s
      call stack, and pops it when [f] returns or raises. *)

  val callstack : t -> string list
  [@@deprecated "use the path field of Error.t; removed in 1.4.0"]
  (** [callstack t] is [t]'s call stack, outermost context first. *)

  val err : ?got:string -> t -> string -> 'a
  [@@deprecated
    "use Error.fail (Error.bad_value loc ~property ~reason); removed in 1.4.0"]
  (** [err ?got t message] raises {!exception-Parse_error} with [message] and
      [got], located at [t]'s position. *)

  val err_eof : t -> 'a
  [@@deprecated
    "use Error.fail (Error.unterminated loc sort); removed in 1.4.0"]
  (** [err_eof t] is [err t "unexpected end of input"]. *)

  val err_expected : t -> string -> 'a
  [@@deprecated
    "use Error.fail (Error.missing_token loc ~sort what); removed in 1.4.0"]
  (** [err_expected t what] is [err t ("expected " ^ what)]. *)

  val err_expected_but_eof : t -> string -> 'a
  [@@deprecated
    "use Error.fail (Error.missing_token loc ~sort what); removed in 1.4.0"]
  (** [err_expected_but_eof t what] is
      [err t ("Expected " ^ what ^ " but reached end of input")]. *)

  val err_invalid : t -> string -> 'a
  [@@deprecated
    "use Error.fail (Error.bad_value loc ~property ~reason); removed in 1.4.0"]
  (** [err_invalid t what] is [err t ("invalid " ^ what)]. *)

  val with_filename : parse_error -> string -> parse_error
  [@@deprecated "use Error.with_filename; removed in 1.4.0"]
  (** [with_filename error filename] is [error] with [filename] as its source
      name. The location is unchanged. *)
end
