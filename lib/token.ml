(** CSS Syntax Module Level 3 section 4.2: token taxonomy.

    Types only; the sec. 4.3 tokenization algorithm lives in {!Lexer}. *)

type hash_flag = Id | Unrestricted
type number_flag = Integer | Number
type number = { value : float; repr : string; number_flag : number_flag }
type bracket = Curly | Paren | Square

type unicode_range_form =
  | Single of { width : int }
  | Range of { start_width : int; end_width : int }
  | Wildcard of { prefix_width : int; wildcards : int }

type kind =
  | Ident of string
  | Function of string
  | At_keyword of string
  | Hash of { value : string; hash_flag : hash_flag }
  | String of { value : string; quote : char; terminated : bool }
  | Bad_string
  | Url of string
  | Bad_url
  | Delim of string
  | Number_tok of number
  | Percentage of number
  | Dimension of { number : number; unit_ : string }
  | Whitespace
  | Unicode_range of {
      start_value : int;
      end_value : int;
      form : unicode_range_form;
    }
  | Cdo
  | Cdc
  | Colon
  | Semicolon
  | Comma
  | Open of bracket
  | Close of bracket
  | Eof

type t = { kind : kind; loc : Loc.t }

let equal_hash_flag (a : hash_flag) b = a = b
let equal_number_flag (a : number_flag) b = a = b
let equal_bracket (a : bracket) b = a = b
let equal_kind (a : kind) b = a = b
let v ~kind ~loc = { kind; loc }
let synthetic kind = { kind; loc = Loc.dummy }

let pp_kind : kind Pp.t =
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
  | String { value; _ } ->
      Pp.string ctx "<string ";
      Pp.string ctx value;
      Pp.char ctx '>'
  | Bad_string -> Pp.string ctx "<bad-string>"
  | Url s ->
      Pp.string ctx "<url ";
      Pp.string ctx s;
      Pp.char ctx '>'
  | Bad_url -> Pp.string ctx "<bad-url>"
  | Delim s ->
      Pp.string ctx "<delim '";
      Pp.string ctx s;
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
  | Unicode_range { start_value; end_value; _ } ->
      Pp.string ctx "<unicode-range U+";
      Pp.hex ctx start_value;
      if end_value <> start_value then (
        Pp.char ctx '-';
        Pp.hex ctx end_value);
      Pp.char ctx '>'
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

let pp : t Pp.t =
 fun ctx { kind; loc } ->
  pp_kind ctx kind;
  Pp.char ctx '@';
  Loc.pp ctx loc

let to_string t = Pp.to_string pp t
