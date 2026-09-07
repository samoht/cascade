(** CSS Syntax 3 (ED) sec. 4.2: token taxonomy.

    Types only; the section 4.3 tokenization algorithm lives in {!Lexer}. Every
    token carries the source {!Loc.t} it was read from. *)

(** Whether a {!Hash} starts an identifier ([#abc]) or is unrestricted ([#123]).
    Only id-flag hashes are valid as ID selectors. *)
type hash_flag = Id | Unrestricted

(** Whether a {!number} was written as an integer or not. *)
type number_flag = Integer | Number

type number = { value : float; repr : string; number_flag : number_flag }
(** The parsed numeric value, its original textual representation, and the
    integer-vs-number flag. *)

(** The three kinds of balanced bracket character used in CSS blocks. *)
type bracket =
  | Curly  (** [\{ ... \}] *)
  | Paren  (** [( ... )] *)
  | Square  (** [[ ... ]] *)

type unicode_range_form =
  | Single of { width : int }
  | Range of { start_width : int; end_width : int }
  | Wildcard of { prefix_width : int; wildcards : int }

(** Token payload: the section 4.2 variants without the location wrapper. *)
type kind =
  | Ident of string
  | Function of string  (** Ident immediately followed by [(]. *)
  | At_keyword of string  (** [@] followed by an ident. *)
  | Hash of { value : string; hash_flag : hash_flag }
  | String of { value : string; quote : char; terminated : bool }
      (** String literal. [quote] is the opening quote character (double or
          single); the spec treats both as equivalent delimiters (CSS Syntax 3
          (ED) sec. 4.3.5) but we record it for quote-sensitive rules (e.g.
          [@charset] per CSS Syntax 3 (ED) sec. 8.3) and to round-trip the input
          style. [terminated] is [false] when the lexer reached EOF without
          seeing the closing quote (CSS Syntax 3 (ED) sec. 4.3.5 returns the
          string token but flags a parse error); the serializer writes the
          closing quote either way, since what the token holds is a string. *)
  | Bad_string
      (** Unterminated string (newline or EOF before the closing quote). *)
  | Url of string
  | Bad_url
      (** Malformed [url(...)] body (unquoted content with invalid chars). *)
  | Delim of string
      (** Any single Unicode code point not consumed by another token rule.
          Stored as the UTF-8 byte sequence (1 to 4 bytes). *)
  | Number_tok of number
  | Percentage of number
  | Dimension of { number : number; unit_ : string }
  | Whitespace of string
      (** A run of whitespace characters, carrying its own text. CSS Custom
          Properties 1 (ED) sec. 4.1 forbids normalizing the whitespace of a
          custom property's token stream, so the run is kept rather than
          collapsed to one space. *)
  | Unicode_range of {
      start_value : int;
      end_value : int;
      form : unicode_range_form;
    }
      (** [U+XXXX] / [U+XXXX-YYYY] / [U+XX??] (CSS Syntax 3 (ED) sec. 4.3.14).
          The three syntactic forms are normalised to the
          [[start_value, end_value]] inclusive range; [start_value = end_value]
          for the single-codepoint form. [form] keeps the typed token shape for
          non-minified fidelity. *)
  | Cdo  (** [<!--] at top level. *)
  | Cdc  (** [-->] at top level. *)
  | Colon
  | Semicolon
  | Comma
  | Open of bracket  (** Opening bracket of a balanced group. *)
  | Close of bracket  (** Closing bracket of a balanced group. *)
  | Eof

type t = { kind : kind; loc : Loc.t; repr : string option }
(** A located token: the section 4.2 payload, the source range it covers, and
    the source text when that text is not what serializing the payload gives
    back. [repr] is [Some] only for a token written with an escape, which is the
    one spelling section 9.1's serialization does not preserve: [colo\r] and
    [color] are the same {!constructor-Ident}, and a stream that must round-trip
    byte for byte needs to tell them apart. *)

val equal_hash_flag : hash_flag -> hash_flag -> bool
(** [equal_hash_flag a b] tests hash token flags for equality. *)

val equal_number_flag : number_flag -> number_flag -> bool
(** [equal_number_flag a b] tests number token flags for equality. *)

val integer_opt : number -> int option
(** [integer_opt n] is the exact integer represented by [n], when [n] is an
    integer token within the native integer range. *)

val equal_bracket : bracket -> bracket -> bool
(** [equal_bracket a b] tests bracket kinds for equality. *)

val compare_bracket : bracket -> bracket -> int
(** [compare_bracket a b] totally orders the bracket characters. *)

val equal_kind : kind -> kind -> bool
(** [equal_kind a b] tests token payloads for structural equality. *)

val compare_kind : kind -> kind -> int
(** [compare_kind a b] totally orders token payloads. A payload carries no
    {!Loc.t}, so the order is over what the token spells, not where it was read.
*)

val v : kind:kind -> loc:Loc.t -> t
(** [v ~kind ~loc] is a token with the given payload and location, spelled the
    way serializing the payload spells it. *)

val of_source : repr:string option -> kind:kind -> loc:Loc.t -> t
(** [of_source ~repr ~kind ~loc] is {!val-v} carrying the source text the token
    was read from; see {!type-t} for when that differs. *)

val synthetic : kind -> t
(** [synthetic k] is a token with payload [k] and {!Loc.dummy} - for test
    fixtures and synthetic values that don't come from a real input. *)

val pp_kind : kind Pp.t
(** [pp_kind] renders just the payload, e.g. [<ident foo>]. *)

val pp : t Pp.t
(** [pp] renders a located token, e.g. [<ident foo>@[3-6]]. *)

val to_string : t -> string
(** [to_string token] renders a located token. *)
