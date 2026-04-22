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

(** {1 Tokenization} *)

(* 4.2 "name code points" *)

let is_name_start c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c = '_'
  || Char.code c >= 0x80

let is_digit c = c >= '0' && c <= '9'
let is_name c = is_name_start c || is_digit c || c = '-'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\012'
let is_newline c = c = '\n' || c = '\r' || c = '\012'

(* 4.3.3 Check if two code points are a valid escape. The reader is at the
   first; check whether ('\\', next) forms a valid escape (i.e. first is '\\'
   and next is not a newline). *)
let valid_escape_at r =
  let s = Reader.peek_string r 2 in
  String.length s >= 2 && s.[0] = '\\' && not (is_newline s.[1])

(* 4.3.4 Check if three code points starting at [offset] would start an ident
   sequence. *)
let would_start_ident_sequence_at r offset =
  let s = Reader.peek_string r (offset + 3) in
  let len = String.length s in
  if len <= offset then false
  else
    match s.[offset] with
    | '-' ->
        len > offset + 1
        && (is_name_start s.[offset + 1] || s.[offset + 1] = '-')
        || len > offset + 2
           && s.[offset + 1] = '\\'
           && not (is_newline s.[offset + 2])
    | '\\' -> len > offset + 1 && not (is_newline s.[offset + 1])
    | c -> is_name_start c

let would_start_ident_sequence r = would_start_ident_sequence_at r 0

(* 4.3.5 Check if three code points would start a number. *)
let would_start_number_at r offset =
  let s = Reader.peek_string r (offset + 3) in
  let len = String.length s in
  if len <= offset then false
  else
    match s.[offset] with
    | '+' | '-' ->
        (len > offset + 1 && is_digit s.[offset + 1])
        || (len > offset + 2 && s.[offset + 1] = '.' && is_digit s.[offset + 2])
    | '.' -> len > offset + 1 && is_digit s.[offset + 1]
    | c -> is_digit c

let would_start_number r = would_start_number_at r 0

let utf8_of_codepoint cp =
  let buf = Buffer.create 4 in
  if cp < 0 || cp > 0x10FFFF || cp = 0 || (cp >= 0xD800 && cp <= 0xDFFF) then
    Buffer.add_string buf "\u{FFFD}"
  else if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
  else if cp <= 0x7FF then (
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp <= 0xFFFF then (
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))));
  Buffer.contents buf

(* 4.3.7 Consume an escaped code point. Assumes the leading [\] was consumed. *)
let consume_escape r =
  match Reader.peek r with
  | None -> "\u{FFFD}" (* parse error *)
  | Some c when is_hex c ->
      let buf = Buffer.create 6 in
      Buffer.add_char buf c;
      Reader.skip r;
      let rec take n =
        if n = 0 then ()
        else
          match Reader.peek r with
          | Some c when is_hex c ->
              Buffer.add_char buf c;
              Reader.skip r;
              take (n - 1)
          | _ -> ()
      in
      take 5;
      (match Reader.peek r with
      | Some c when is_ws c ->
          if
            c = '\r'
            && String.length (Reader.peek_string r 2) = 2
            && (Reader.peek_string r 2).[1] = '\n'
          then (
            Reader.skip r;
            Reader.skip r)
          else Reader.skip r
      | _ -> ());
      let hex = Buffer.contents buf in
      let cp = try int_of_string ("0x" ^ hex) with Failure _ -> 0xFFFD in
      utf8_of_codepoint cp
  | Some c ->
      Reader.skip r;
      String.make 1 c

(* 4.3.8 Consume an ident sequence. *)
let consume_ident_sequence r =
  let buf = Buffer.create 16 in
  let rec loop () =
    match Reader.peek r with
    | Some c when is_name c ->
        Buffer.add_char buf c;
        Reader.skip r;
        loop ()
    | Some '\\' when valid_escape_at r ->
        Reader.skip r;
        Buffer.add_string buf (consume_escape r);
        loop ()
    | _ -> ()
  in
  loop ();
  Buffer.contents buf

(* 4.3.11 Consume a string token. Assumes opening quote already consumed. *)
let consume_string_token ~quote r =
  let buf = Buffer.create 32 in
  let rec loop () =
    match Reader.peek r with
    | None -> String (Buffer.contents buf)
    | Some c when c = quote ->
        Reader.skip r;
        String (Buffer.contents buf)
    | Some c when is_newline c -> Bad_string (* do not consume the newline *)
    | Some '\\' -> (
        Reader.skip r;
        match Reader.peek r with
        | None -> loop ()
        | Some c when is_newline c ->
            Reader.skip r;
            (if c = '\r' then
               match Reader.peek r with Some '\n' -> Reader.skip r | _ -> ());
            loop ()
        | Some _ ->
            Buffer.add_string buf (consume_escape r);
            loop ())
    | Some c ->
        Buffer.add_char buf c;
        Reader.skip r;
        loop ()
  in
  loop ()

(* 4.3.14 Consume a number. *)
let consume_number r =
  let buf = Buffer.create 16 in
  let is_int = ref true in
  let take_digits () =
    let rec loop () =
      match Reader.peek r with
      | Some c when is_digit c ->
          Buffer.add_char buf c;
          Reader.skip r;
          loop ()
      | _ -> ()
    in
    loop ()
  in
  (match Reader.peek r with
  | Some (('+' | '-') as c) ->
      Buffer.add_char buf c;
      Reader.skip r
  | _ -> ());
  take_digits ();
  (match Reader.peek_string r 2 with
  | s when String.length s = 2 && s.[0] = '.' && is_digit s.[1] ->
      Buffer.add_char buf '.';
      Reader.skip r;
      is_int := false;
      take_digits ()
  | _ -> ());
  (match Reader.peek_string r 3 with
  | s when String.length s >= 2 && (s.[0] = 'e' || s.[0] = 'E') ->
      let has_sign_digit =
        String.length s >= 3 && (s.[1] = '+' || s.[1] = '-') && is_digit s.[2]
      in
      let has_digit = is_digit s.[1] in
      if has_digit || has_sign_digit then (
        Buffer.add_char buf s.[0];
        Reader.skip r;
        is_int := false;
        (match Reader.peek r with
        | Some (('+' | '-') as c) ->
            Buffer.add_char buf c;
            Reader.skip r
        | _ -> ());
        take_digits ())
  | _ -> ());
  let repr = Buffer.contents buf in
  let value = try float_of_string repr with Failure _ -> 0.0 in
  let number_flag = if !is_int then Integer else Number in
  { value; repr; number_flag }

(* 4.3.3 Consume a numeric token. *)
let consume_numeric_token r =
  let n = consume_number r in
  if would_start_ident_sequence r then
    let unit_ = consume_ident_sequence r in
    Dimension { number = n; unit_ }
  else
    match Reader.peek r with
    | Some '%' ->
        Reader.skip r;
        Percentage n
    | _ -> Number_tok n

(* 4.3.6 Consume the remnants of a bad url. *)
let consume_remnants_of_bad_url r =
  let rec loop () =
    match Reader.peek r with
    | None -> ()
    | Some ')' -> Reader.skip r
    | Some '\\' when valid_escape_at r ->
        Reader.skip r;
        let _ = consume_escape r in
        loop ()
    | Some _ ->
        Reader.skip r;
        loop ()
  in
  loop ()

(* 4.3.13 Consume a url token. Assumes "url(" and any whitespace has been
   consumed. *)
let consume_url_token r =
  let buf = Buffer.create 32 in
  let rec loop () =
    match Reader.peek r with
    | None -> Url (Buffer.contents buf)
    | Some ')' ->
        Reader.skip r;
        Url (Buffer.contents buf)
    | Some c when is_ws c -> (
        let rec skip_ws () =
          match Reader.peek r with
          | Some c when is_ws c ->
              Reader.skip r;
              skip_ws ()
          | _ -> ()
        in
        skip_ws ();
        match Reader.peek r with
        | None -> Url (Buffer.contents buf)
        | Some ')' ->
            Reader.skip r;
            Url (Buffer.contents buf)
        | _ ->
            consume_remnants_of_bad_url r;
            Bad_url)
    | Some ('"' | '\'' | '(') ->
        consume_remnants_of_bad_url r;
        Bad_url
    | Some c when Char.code c < 0x20 || Char.code c = 0x7F ->
        consume_remnants_of_bad_url r;
        Bad_url
    | Some '\\' ->
        if valid_escape_at r then (
          Reader.skip r;
          Buffer.add_string buf (consume_escape r);
          loop ())
        else (
          consume_remnants_of_bad_url r;
          Bad_url)
    | Some c ->
        Buffer.add_char buf c;
        Reader.skip r;
        loop ()
  in
  loop ()

(* 4.3.10 Consume an ident-like token. *)
let consume_ident_like_token r =
  let name = consume_ident_sequence r in
  let lower = String.lowercase_ascii name in
  if lower = "url" && Reader.peek r = Some '(' then (
    Reader.skip r;
    (* While the next two code points are whitespace, consume one. *)
    let rec skip_ws_keep_one () =
      let s = Reader.peek_string r 2 in
      if String.length s = 2 && is_ws s.[0] && is_ws s.[1] then (
        Reader.skip r;
        skip_ws_keep_one ())
    in
    skip_ws_keep_one ();
    (* If the next one or two code points are a quote, or ws followed by a
       quote, emit <function-token>, not a url token. *)
    let is_function_url =
      match Reader.peek_string r 2 with
      | s when String.length s >= 1 && (s.[0] = '"' || s.[0] = '\'') -> true
      | s
        when String.length s = 2 && is_ws s.[0] && (s.[1] = '"' || s.[1] = '\'')
        ->
          true
      | _ -> false
    in
    if is_function_url then Function name else consume_url_token r)
  else if Reader.peek r = Some '(' then (
    Reader.skip r;
    Function name)
  else Ident name

(* 4.3.2 Consume one comment body, assuming [/*] was already consumed. An
   unterminated comment is a parse error per the spec but we just stop at EOF
   silently. *)
let rec skip_comment_body r =
  if Reader.is_done r then ()
  else if Reader.looking_at r "*/" then (
    Reader.skip r;
    Reader.skip r)
  else (
    Reader.skip r;
    skip_comment_body r)

let rec consume_comments r =
  if Reader.looking_at r "/*" then (
    Reader.skip r;
    Reader.skip r;
    skip_comment_body r;
    consume_comments r)

(* Consume a run of whitespace code points and return <whitespace-token>. *)
let rec consume_whitespace_run r =
  match Reader.peek r with
  | Some c when is_ws c ->
      Reader.skip r;
      consume_whitespace_run r
  | _ -> ()

let hash_flag_now r = if would_start_ident_sequence r then Id else Unrestricted

(* 4.3.1 sub-case: [#] has already been consumed. *)
let consume_hash_token r =
  match Reader.peek r with
  | Some c when is_name c ->
      let hash_flag = hash_flag_now r in
      let value = consume_ident_sequence r in
      Hash { value; hash_flag }
  | Some '\\' when valid_escape_at r ->
      let hash_flag = hash_flag_now r in
      let value = consume_ident_sequence r in
      Hash { value; hash_flag }
  | _ -> Delim '#'

(* 4.3.1 sub-case: reader is at [-], not yet consumed. *)
let consume_hyphen_start r =
  if would_start_number r then consume_numeric_token r
  else if Reader.looking_at r "-->" then (
    Reader.skip r;
    Reader.skip r;
    Reader.skip r;
    Cdc)
  else if would_start_ident_sequence r then consume_ident_like_token r
  else (
    Reader.skip r;
    Delim '-')

(* 4.3.1 sub-case: reader is at [<]. *)
let consume_less_than r =
  if Reader.looking_at r "<!--" then (
    Reader.skip r;
    Reader.skip r;
    Reader.skip r;
    Reader.skip r;
    Cdo)
  else (
    Reader.skip r;
    Delim '<')

(* 4.3.1 sub-case: [@] has already been consumed. *)
let consume_at_start r =
  if would_start_ident_sequence r then At_keyword (consume_ident_sequence r)
  else Delim '@'

(* 4.3.1 Consume a token. *)
let next r =
  consume_comments r;
  match Reader.peek r with
  | None -> Eof
  | Some c when is_ws c ->
      consume_whitespace_run r;
      Whitespace
  | Some '"' ->
      Reader.skip r;
      consume_string_token ~quote:'"' r
  | Some '\'' ->
      Reader.skip r;
      consume_string_token ~quote:'\'' r
  | Some '#' ->
      Reader.skip r;
      consume_hash_token r
  | Some '(' ->
      Reader.skip r;
      Open Paren
  | Some ')' ->
      Reader.skip r;
      Close Paren
  | Some '+' when would_start_number r -> consume_numeric_token r
  | Some '+' ->
      Reader.skip r;
      Delim '+'
  | Some ',' ->
      Reader.skip r;
      Comma
  | Some '-' -> consume_hyphen_start r
  | Some '.' when would_start_number r -> consume_numeric_token r
  | Some '.' ->
      Reader.skip r;
      Delim '.'
  | Some ':' ->
      Reader.skip r;
      Colon
  | Some ';' ->
      Reader.skip r;
      Semicolon
  | Some '<' -> consume_less_than r
  | Some '@' ->
      Reader.skip r;
      consume_at_start r
  | Some '[' ->
      Reader.skip r;
      Open Square
  | Some '\\' when valid_escape_at r -> consume_ident_like_token r
  | Some '\\' ->
      Reader.skip r;
      Delim '\\'
  | Some ']' ->
      Reader.skip r;
      Close Square
  | Some '{' ->
      Reader.skip r;
      Open Curly
  | Some '}' ->
      Reader.skip r;
      Close Curly
  | Some c when is_digit c -> consume_numeric_token r
  | Some c when is_name_start c -> consume_ident_like_token r
  | Some c ->
      Reader.skip r;
      Delim c
