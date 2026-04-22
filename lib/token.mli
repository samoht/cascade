(** CSS Syntax Module Level 3 section 4: tokens and tokenization.

    Defines the CSS token taxonomy (section 4.2) and tokenizes a character
    stream from a {!Reader.t} into tokens (section 4.3). Comments are stripped
    during tokenization and do not produce tokens.

    The parser algorithms in {!Parser} consume the token stream produced by
    {!next}. *)

(** {1 Token taxonomy} *)

(** Whether a {!Hash} starts an identifier ([#abc]) or is unrestricted ([#123]).
    Only id-flag hashes are valid as ID selectors. *)
type hash_flag = Id | Unrestricted

(** Whether a {!number} was written as an integer or not. *)
type number_flag = Integer | Number

type number = { value : float; repr : string; number_flag : number_flag }
(** The parsed value, the original textual representation, and the
    integer-vs-number flag for a numeric token payload. *)

(** The three kinds of balanced bracket character used in CSS blocks. *)
type bracket =
  | Curly  (** [\{ ... \}] *)
  | Paren  (** [( ... )] *)
  | Square  (** [[ ... ]] *)

(** A CSS token, one variant per terminal defined in CSS Syntax section 4.2. *)
type t =
  | Ident of string
  | Function of string  (** Ident immediately followed by [(]. *)
  | At_keyword of string  (** [@] followed by an ident. *)
  | Hash of { value : string; hash_flag : hash_flag }
  | String of string
  | Bad_string
      (** Unterminated string (newline or EOF before the closing quote). *)
  | Url of string
  | Bad_url
      (** Malformed [url(...)] body (unquoted content with invalid chars). *)
  | Delim of char
      (** Any single code point not consumed by another token rule. *)
  | Number_tok of number
  | Percentage of number
  | Dimension of { number : number; unit_ : string }
  | Whitespace  (** Any run of whitespace characters. *)
  | Cdo  (** [<!--] at top level. *)
  | Cdc  (** [-->] at top level. *)
  | Colon
  | Semicolon
  | Comma
  | Open of bracket  (** Opening bracket of a balanced group. *)
  | Close of bracket  (** Closing bracket of a balanced group. *)
  | Eof

val pp : t Pp.t
(** [pp] pretty-prints a token for debugging, e.g. [<ident foo>], [<number 10>].
*)

val to_string : t -> string
(** [to_string t] is the string rendering of {!pp}. *)
