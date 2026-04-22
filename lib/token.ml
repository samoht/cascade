(** CSS Syntax Module Level 3 4.2/4.3: tokens and tokenization.

    This module defines the CSS token taxonomy and ports the tokenization
    algorithm from https://www.w3.org/TR/css-syntax-3/#tokenization, including
    bad-string and bad-url recovery variants. Comments are stripped during
    tokenization and do not produce tokens.

    The parser algorithms in {!Parser} consume this stream. *)

(** {1 Token taxonomy} *)

type hash_flag =
  | Id
      (** The hash value would start an identifier (e.g. [#abc]). Only id-flag
          hashes are valid as ID selectors. *)
  | Unrestricted  (** Any other hash, e.g. [#123]. *)

type number_flag =
  | Integer  (** Written as a whole number with no decimal or exponent. *)
  | Number  (** Otherwise. *)

type number = {
  value : float;  (** The numeric value. *)
  repr : string;  (** The original textual representation. *)
  number_flag : number_flag;
}

type bracket = Curly | Paren | Square

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
  | Whitespace
      (** Any run of whitespace characters. Comments are skipped; they do not
          produce tokens. *)
  | Cdo  (** [<!--] at top level. *)
  | Cdc  (** [-->] at top level. *)
  | Colon
  | Semicolon
  | Comma
  | Open of bracket
  | Close of bracket
  | Eof

let pp : t Pp.t =
 fun ctx -> function
  | Ident s ->
      Pp.string ctx "<ident ";
      Pp.string ctx s;
      Pp.char ctx '>'
  | Function s ->
      Pp.string ctx "<function ";
      Pp.string ctx s;
      Pp.string ctx "(>"
  | At_keyword s ->
      Pp.string ctx "<@";
      Pp.string ctx s;
      Pp.char ctx '>'
  | Hash { value; _ } ->
      Pp.string ctx "<#";
      Pp.string ctx value;
      Pp.char ctx '>'
  | String s ->
      Pp.string ctx "<string ";
      Pp.string ctx s;
      Pp.char ctx '>'
  | Bad_string -> Pp.string ctx "<bad-string>"
  | Url s ->
      Pp.string ctx "<url ";
      Pp.string ctx s;
      Pp.char ctx '>'
  | Bad_url -> Pp.string ctx "<bad-url>"
  | Delim c ->
      Pp.string ctx "<delim '";
      Pp.char ctx c;
      Pp.string ctx "'>"
  | Number_tok { repr; _ } ->
      Pp.string ctx "<number ";
      Pp.string ctx repr;
      Pp.char ctx '>'
  | Percentage { repr; _ } ->
      Pp.string ctx "<percentage ";
      Pp.string ctx repr;
      Pp.string ctx "%>"
  | Dimension { number; unit_ } ->
      Pp.string ctx "<dimension ";
      Pp.string ctx number.repr;
      Pp.string ctx unit_;
      Pp.char ctx '>'
  | Whitespace -> Pp.string ctx "<ws>"
  | Cdo -> Pp.string ctx "<CDO>"
  | Cdc -> Pp.string ctx "<CDC>"
  | Colon -> Pp.string ctx "<:>"
  | Semicolon -> Pp.string ctx "<;>"
  | Comma -> Pp.string ctx "<,>"
  | Open Square -> Pp.string ctx "<[>"
  | Close Square -> Pp.string ctx "<]>"
  | Open Paren -> Pp.string ctx "<(>"
  | Close Paren -> Pp.string ctx "<)>"
  | Open Curly -> Pp.string ctx "<{>"
  | Close Curly -> Pp.string ctx "<}>"
  | Eof -> Pp.string ctx "<eof>"

let to_string t = Pp.to_string pp t
