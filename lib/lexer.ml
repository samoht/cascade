(** Stage 2 stream: characters -> Token.t.

    Wraps a {!Reader.t} char cursor, maintains one-token pushback for
    {!reconsume}, and exposes the CSS Syntax section 4 tokenizer through the
    uniform [next / peek / reconsume] triple. *)

open Token

type t = { reader : Reader.t; mutable lookback : Token.t option }

let of_reader reader = { reader; lookback = None }
let of_string s = of_reader (Reader.of_string s)
let source t = Reader.source t.reader

(** {1 Tokenization} *)

(* 4.2 "name code points" *)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\012'
let is_newline c = c = '\n' || c = '\r' || c = '\012'

(* CSS Syntax Level 3 section 4.2 "non-ASCII ident code point": the specific
   code-point ranges allowed in identifiers. Kept as a sealed check so that
   byte-level heuristics ([>= 0x80]) don't over-accept code points the spec
   explicitly excludes (e.g. most symbols, emoji, BMP non-characters). *)
let is_non_ascii_ident_cp cp =
  cp = 0xB7
  || (cp >= 0xC0 && cp <= 0xD6)
  || (cp >= 0xD8 && cp <= 0xF6)
  || (cp >= 0xF8 && cp <= 0x37D)
  || (cp >= 0x37F && cp <= 0x1FFF)
  || cp = 0x200C || cp = 0x200D || cp = 0x203F || cp = 0x2040
  || (cp >= 0x2070 && cp <= 0x218F)
  || (cp >= 0x2C00 && cp <= 0x2FEF)
  || (cp >= 0x3001 && cp <= 0xD7FF)
  || (cp >= 0xF900 && cp <= 0xFDCF)
  || (cp >= 0xFDF0 && cp <= 0xFFFD)
  || cp >= 0x10000

(* [is_name_start_byte] answers the ASCII fast-path only. Callers that have a
   byte [>= 0x80] must decode and consult [is_non_ascii_ident_cp] directly. *)
let is_name_start_ascii c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* [is_name_start r] checks whether the code point at the reader's current
   position is a valid ident-start. For bytes [< 0x80] we answer ASCII directly;
   otherwise decode the UTF-8 sequence and check the spec range list. Returns
   [false] on malformed UTF-8. *)
let is_name_start_at r offset =
  match Reader.peek_utf8_at r offset with
  | None -> false
  | Some (cp, _) when cp < 0x80 -> is_name_start_ascii (Char.chr cp)
  | Some (cp, _) -> is_non_ascii_ident_cp cp

(* [is_name_at r offset] is ident-start or digit or [-]. *)
let is_name_at r offset =
  match Reader.peek_utf8_at r offset with
  | None -> false
  | Some (cp, _) when cp < 0x80 ->
      let c = Char.chr cp in
      is_name_start_ascii c || is_digit c || c = '-'
  | Some (cp, _) -> is_non_ascii_ident_cp cp

(* Legacy byte-level helpers kept for the hot paths where the caller has already
   materialised the byte. For anything ASCII they match spec; for bytes [>=
   0x80] they are now conservative (reject), and callers that might see
   non-ASCII must use the [_at] variants above. *)
let is_name_start c = c < '\x80' && is_name_start_ascii c
let is_name c = is_name_start c || is_digit c || c = '-'

(* 4.3.3 Check if two code points are a valid escape. The reader is at the
   first; check whether ('\\', next) forms a valid escape (i.e. first is '\\'
   and next is not a newline). *)
let valid_escape_at r =
  let s = Reader.peek_string r 2 in
  String.length s >= 2 && s.[0] = '\\' && not (is_newline s.[1])

(* 4.3.4 Check if three code points starting at [offset] would start an ident
   sequence. Multi-byte code points are consulted via [is_name_start_at] (which
   decodes at the given byte offset) rather than byte-level checks. *)
let would_start_ident_sequence_at r offset =
  let s = Reader.peek_string r (offset + 3) in
  let len = String.length s in
  if len <= offset then false
  else
    match s.[offset] with
    | '-' ->
        len > offset + 1
        && (is_name_start_at r (offset + 1) || s.[offset + 1] = '-')
        || len > offset + 2
           && s.[offset + 1] = '\\'
           && not (is_newline s.[offset + 2])
    | '\\' -> len > offset + 1 && not (is_newline s.[offset + 1])
    | _ -> is_name_start_at r offset

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

(* 4.3.8 Consume an ident sequence. Iterates at the code-point level so
   multi-byte characters are accepted only when their code point is a valid
   ident code point per section 4.2 (see [is_name_at]). The full UTF-8 byte
   sequence of each accepted code point is copied into the ident buffer
   verbatim. *)
let consume_ident_sequence r =
  let buf = Buffer.create 16 in
  let src = Reader.source r in
  let rec loop () =
    match Reader.peek r with
    | Some '\\' when valid_escape_at r ->
        Reader.skip r;
        Buffer.add_string buf (consume_escape r);
        loop ()
    | Some _ -> (
        match Reader.peek_utf8 r with
        | None -> () (* malformed UTF-8 stops the ident *)
        | Some (_, nbytes) when is_name_at r 0 ->
            let start = Reader.position r in
            Buffer.add_substring buf src start nbytes;
            for _ = 1 to nbytes do
              Reader.skip r
            done;
            loop ()
        | Some _ -> ())
    | None -> ()
  in
  loop ();
  Buffer.contents buf

(* 4.3.11 Consume a string token. Assumes opening quote already consumed. *)
let consume_string_token ~quote r =
  let buf = Buffer.create 32 in
  let rec loop () =
    match Reader.peek r with
    | None -> String { value = Buffer.contents buf; quote }
    | Some c when c = quote ->
        Reader.skip r;
        String { value = Buffer.contents buf; quote }
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

(* 4.3.6 Consume a url token. Assumes "url(" has been consumed. *)
let consume_url_token r =
  let buf = Buffer.create 32 in
  let rec skip_ws () =
    match Reader.peek r with
    | Some c when is_ws c ->
        Reader.skip r;
        skip_ws ()
    | _ -> ()
  in
  skip_ws ();
  let rec loop () =
    match Reader.peek r with
    | None -> Url (Buffer.contents buf)
    | Some ')' ->
        Reader.skip r;
        Url (Buffer.contents buf)
    | Some c when is_ws c -> (
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

(* 4.3.4 Check if three code points would start a unicode-range token. The
   reader is positioned at the leading [U]/[u]; we look at offsets 0..2. *)
let would_start_unicode_range r =
  match (Reader.peek r, Reader.peek_string r 3) with
  | Some ('U' | 'u'), s
    when String.length s = 3 && s.[1] = '+' && (is_hex s.[2] || s.[2] = '?') ->
      true
  | _ -> false

(* 4.3.14 Consume a unicode-range token. Assumes [would_start_unicode_range]
   just returned true. *)
let consume_unicode_range_token r =
  Reader.skip r;
  Reader.skip r;
  let buf = Buffer.create 6 in
  let rec consume_hex n =
    if n = 6 then n
    else
      match Reader.peek r with
      | Some c when is_hex c ->
          Buffer.add_char buf c;
          Reader.skip r;
          consume_hex (n + 1)
      | _ -> n
  in
  let n_hex = consume_hex 0 in
  let rec consume_q n =
    if n_hex + n = 6 then n
    else
      match Reader.peek r with
      | Some '?' ->
          Reader.skip r;
          consume_q (n + 1)
      | _ -> n
  in
  let n_q = consume_q 0 in
  let start_repr = Buffer.contents buf in
  let parse_hex s = int_of_string ("0x" ^ s) in
  if n_q > 0 then
    let start_value = parse_hex (start_repr ^ String.make n_q '0') in
    let end_value = parse_hex (start_repr ^ String.make n_q 'F') in
    Unicode_range { start_value; end_value }
  else
    let start_value = parse_hex start_repr in
    let has_range_tail =
      match Reader.peek_string r 2 with
      | s when String.length s = 2 && s.[0] = '-' && is_hex s.[1] -> true
      | _ -> false
    in
    if has_range_tail then (
      Reader.skip r;
      let end_buf = Buffer.create 6 in
      let rec consume_end n =
        if n = 6 then ()
        else
          match Reader.peek r with
          | Some c when is_hex c ->
              Buffer.add_char end_buf c;
              Reader.skip r;
              consume_end (n + 1)
          | _ -> ()
      in
      consume_end 0;
      let end_value = parse_hex (Buffer.contents end_buf) in
      Unicode_range { start_value; end_value })
    else Unicode_range { start_value; end_value = start_value }

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
  | _ -> Delim "#"

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
    Delim "-")

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
    Delim "<")

(* 4.3.1 sub-case: [@] has already been consumed. *)
let consume_at_start r =
  if would_start_ident_sequence r then At_keyword (consume_ident_sequence r)
  else Delim "@"

(* 4.3.1 Consume a token. *)
let next_token r =
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
      Delim "+"
  | Some ',' ->
      Reader.skip r;
      Comma
  | Some '-' -> consume_hyphen_start r
  | Some '.' when would_start_number r -> consume_numeric_token r
  | Some '.' ->
      Reader.skip r;
      Delim "."
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
      Delim "\\"
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
  | Some ('U' | 'u') when would_start_unicode_range r ->
      consume_unicode_range_token r
  | Some c when is_name_start_ascii c || c >= '\x80' ->
      (* ASCII fast path or a multi-byte lead -- consult [is_name_start_at]
         which decodes the full UTF-8 code point and checks the spec range list.
         Non-ident code points fall through to [Delim], where a multi-byte lead
         consumes the whole UTF-8 sequence so the delim token holds the full
         code point (CSS Syntax section 4.3.1). *)
      if is_name_start_at r 0 then consume_ident_like_token r
      else if c >= '\x80' then (
        match Reader.peek_utf8 r with
        | Some (_, n) ->
            let bytes = Reader.peek_string r n in
            for _ = 1 to n do
              Reader.skip r
            done;
            Delim bytes
        | None ->
            Reader.skip r;
            Delim (String.make 1 c))
      else (
        Reader.skip r;
        Delim (String.make 1 c))
  | Some c ->
      Reader.skip r;
      Delim (String.make 1 c)

(** {1 Stream API (uniform with other stages)} *)

(* Wrap a tokenizer step with source-location capture. *)
let tokenize_with_loc reader =
  let start_pos = Reader.position reader in
  let kind = next_token reader in
  let end_pos = Reader.position reader in
  Token.v ~kind ~loc:(Loc.v ~start_pos ~end_pos)

let next t =
  match t.lookback with
  | Some tok ->
      t.lookback <- None;
      tok
  | None -> tokenize_with_loc t.reader

let peek t =
  match t.lookback with
  | Some tok -> tok
  | None ->
      let tok = tokenize_with_loc t.reader in
      t.lookback <- Some tok;
      tok

let reconsume t tok =
  assert (t.lookback = None);
  t.lookback <- Some tok

let is_done t = t.lookback = None && Reader.is_done t.reader
