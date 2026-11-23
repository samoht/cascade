(** Stage 2 stream: characters -> Token.t.

    Wraps a {!Reader.t} char cursor and exposes the CSS Syntax 3 (ED) sec. 4
    tokenizer through the uniform [next / peek / reconsume] triple, whose token
    buffer {!save}/{!restore} replay. *)

open Token

type comment = { loc : Loc.t; terminated : bool }

type t = {
  reader : Reader.t;
  on_comment : (comment -> unit) option;
  (* CSS Syntax 3 (ED) sec. 4.3.1 takes "unicode ranges allowed", defaulting to
     false; sec. 4.3.14 names its one caller, the value of a unicode-range
     descriptor. With it unset, [u+a] is an ident, a delim and an ident. *)
  mutable unicode_ranges : bool;
  (* The token {!peek} read ahead, or [none]. It comes before [buffer]. *)
  mutable ahead : Token.t;
  (* Token buffer: most-recently-pushed-back at the head. [next] takes from here
     before falling through to the reader. *)
  mutable buffer : Token.t list;
  (* The token [next] returned last, or [none]. *)
  mutable last : Token.t;
  (* Stack of saved positions. Each entry holds the tokens consumed since [save]
     was called; [restore] replays them at the head of [buffer]. *)
  mutable saves : save list;
  (* The first backslash at or after the start of the last token, or [max_int]
     when the rest of the source has none. The reader only moves forward, so one
     scan of the source answers every token. *)
  mutable backslash : int;
}

and save = { mutable trace : Token.t list }

(* No token: compared physically, so a token the lexer reads is never it. *)
let none = Token.v ~kind:Eof ~loc:Loc.dummy

let of_reader ?on_comment ?(unicode_ranges = false) reader =
  {
    reader;
    on_comment;
    unicode_ranges;
    ahead = none;
    buffer = [];
    last = none;
    saves = [];
    backslash = -1;
  }

let of_string ?enforce_spec ?on_comment ?unicode_ranges s =
  of_reader ?on_comment ?unicode_ranges (Reader.of_string ?enforce_spec s)

let with_unicode_ranges t f =
  let saved = t.unicode_ranges in
  t.unicode_ranges <- true;
  Fun.protect ~finally:(fun () -> t.unicode_ranges <- saved) f

let source t = Reader.source t.reader
let indexed_source t = Reader.indexed_source t.reader

(** {1 Tokenization} *)

(* 4.2 "name code points" *)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\012'
let is_newline c = c = '\n' || c = '\r' || c = '\012'

(* The byte [offset] past the reader's position, or ['\000'] at or past the end
   of input. Preprocessing (CSS Syntax 3 (ED) sec. 3.3) replaces every NUL, so
   the byte marks only the end, and reading it allocates no option. *)
let at r offset =
  let b = Reader.peek_byte_at r offset in
  if b < 0 then '\000' else Char.unsafe_chr b

let is_end c = Char.equal c '\000'

(* Every one-byte string, built once: delimiters and single spaces are too
   common to copy out of the source one token at a time. *)
let byte_strings = Array.init 256 (fun i -> String.make 1 (Char.chr i))
let byte_string c = Array.unsafe_get byte_strings (Char.code c)
let looking_at_comment r = at r 0 = '/' && at r 1 = '*'

(* CSS Syntax 3 (ED) section 4.2 "non-ASCII ident code point" ranges: a sealed
   list that excludes most BMP symbols, among them the arrows.

   Real stylesheets carry those anyway - Tailwind emits [.text-\u{2197}] - and
   dropping a rule is a worse failure for a minifier than accepting a code point
   the list omits, so reading takes any code point >= U+0080 by default and this
   list is what [~enforce_spec:true] selects. Writing is unaffected: a code
   point outside the list is emitted as a hex escape, which every parser
   reads. *)
let spec_non_ascii_ident_cp cp =
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

let is_non_ascii_ident_cp r cp =
  (not (Reader.enforce_spec r)) || spec_non_ascii_ident_cp cp

(* [is_name_start_byte] answers the ASCII fast-path only. Callers that have a
   byte [>= 0x80] must decode and consult [is_non_ascii_ident_cp] directly. *)
let is_name_start_ascii c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* Whether the code point at [offset] is a valid ident-start. ASCII bytes answer
   directly; only [>= 0x80] falls back to [peek_utf8_at]. *)
let is_name_start_at r offset =
  let b = Reader.peek_byte_at r offset in
  if b < 0 then false
  else if b < 0x80 then is_name_start_ascii (Char.unsafe_chr b)
  else
    match Reader.peek_utf8_at r offset with
    | None -> false
    | Some (cp, _) -> is_non_ascii_ident_cp r cp

(* [is_name_at r offset] is ident-start or digit or [-]. *)
let is_name_at r offset =
  let b = Reader.peek_byte_at r offset in
  if b < 0 then false
  else if b < 0x80 then
    let c = Char.unsafe_chr b in
    is_name_start_ascii c || is_digit c || c = '-'
  else
    match Reader.peek_utf8_at r offset with
    | None -> false
    | Some (cp, _) -> is_non_ascii_ident_cp r cp

(* Byte-level helpers for hot paths where the caller already has the byte. ASCII
   matches spec; [>= 0x80] is conservative (reject), so callers that may see
   non-ASCII must use the [_at] variants above. *)
(* 4.3.3 Check if two code points are a valid escape. The reader is at the
   first; check whether ('\\', next) forms a valid escape (i.e. first is '\\'
   and next is not a newline). *)
let valid_escape_at r =
  at r 0 = '\\'
  &&
  let c = at r 1 in
  (not (is_end c)) && not (is_newline c)

(* 4.3.4 Check if three code points starting at [offset] would start an ident
   sequence. Multi-byte code points are consulted via [is_name_start_at] (which
   decodes at the given byte offset) rather than byte-level checks. *)
let would_start_ident_sequence_at r offset =
  let escapes c = (not (is_end c)) && not (is_newline c) in
  match at r offset with
  | '\000' -> false
  | '-' -> (
      match at r (offset + 1) with
      | c1 when is_name_start_at r (offset + 1) || c1 = '-' -> true
      | '\\' -> escapes (at r (offset + 2))
      | _ -> false)
  | '\\' -> escapes (at r (offset + 1))
  | _ -> is_name_start_at r offset

let would_start_ident_sequence r = would_start_ident_sequence_at r 0

(* 4.3.5 Check if three code points would start a number. *)
let would_start_number_at r offset =
  match at r offset with
  | '+' | '-' -> (
      match at r (offset + 1) with
      | c1 when is_digit c1 -> true
      | '.' -> is_digit (at r (offset + 2))
      | _ -> false)
  | '.' -> is_digit (at r (offset + 1))
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
let consume_escape_trailing_ws r =
  let c = at r 0 in
  if is_ws c then (
    Reader.skip r;
    if c = '\r' && at r 0 = '\n' then Reader.skip r)

let consume_escape_hex r first =
  let buf = Buffer.create 6 in
  Buffer.add_char buf first;
  Reader.skip r;
  let rec take n =
    let c = at r 0 in
    if n > 0 && is_hex c then (
      Buffer.add_char buf c;
      Reader.skip r;
      take (n - 1))
  in
  take 5;
  consume_escape_trailing_ws r;
  let hex = Buffer.contents buf in
  let cp =
    try int_of_string (String.concat "" [ "0x"; hex ])
    with Failure _ -> 0xFFFD
  in
  utf8_of_codepoint cp

let consume_escape r =
  match at r 0 with
  | '\000' -> "\u{FFFD}" (* parse error *)
  | c when is_hex c -> consume_escape_hex r c
  | c ->
      Reader.skip r;
      String.make 1 c

(* Hot loop: walk bytes directly from the source. ASCII name bytes (nearly all
   idents) advance by one without allocating; a backslash bails to the slow
   escape path; [>= 0x80] falls back to UTF-8 decode for spec-conformant
   rejection. *)
let rec scan_ident_fast r src src_len =
  let pos = Reader.position r in
  if pos >= src_len then `End
  else
    let b = Char.code (String.unsafe_get src pos) in
    if b = Char.code '\\' && valid_escape_at r then `Escape
    else if b < 0x80 then
      let c = Char.unsafe_chr b in
      if is_name_start_ascii c || is_digit c || c = '-' then (
        Reader.skip r;
        scan_ident_fast r src src_len)
      else `End
    else
      match Reader.peek_utf8_at r 0 with
      | None -> `End
      | Some (cp, nbytes) when is_non_ascii_ident_cp r cp ->
          for _ = 1 to nbytes do
            Reader.skip r
          done;
          scan_ident_fast r src src_len
      | Some _ -> `End

let is_ascii_name c = is_name_start_ascii c || is_digit c || c = '-'

let rec skip_ascii_name r =
  let b = Reader.peek_byte r in
  if b >= 0 && is_ascii_name (Char.unsafe_chr b) then (
    Reader.skip r;
    skip_ascii_name r)

(* Slow path: an escape has been encountered. Seeded with the already-consumed
   no-escape prefix in [buf], extend [buf] byte-by-byte from here on. *)
let scan_ident_slow r src buf =
  let consume_hash_suffix () =
    if at r 0 = '#' then (
      Buffer.add_char buf '#';
      Reader.skip r)
  in
  let rec loop () =
    match at r 0 with
    | '\\' when valid_escape_at r ->
        let esc_start = Reader.position r in
        let hex_escape = is_hex (at r 1) in
        Reader.skip r;
        (match at r 0 with
        | c when (not (is_end c)) && not (is_hex c) ->
            Buffer.add_char buf c;
            Reader.skip r
        | _ -> Buffer.add_string buf (consume_escape r));
        let consumed_hex_terminator =
          let pos = Reader.position r in
          hex_escape && pos > esc_start + 2 && is_ws src.[pos - 1]
        in
        if not consumed_hex_terminator then consume_hash_suffix ();
        loop ()
    | c when is_ascii_name c ->
        let p = Reader.position r in
        skip_ascii_name r;
        Buffer.add_substring buf src p (Reader.position r - p);
        loop ()
    | '\000' -> ()
    | _ -> (
        match Reader.peek_utf8 r with
        | None -> ()
        | Some (_, nbytes) when is_name_at r 0 ->
            let p = Reader.position r in
            Buffer.add_substring buf src p nbytes;
            for _ = 1 to nbytes do
              Reader.skip r
            done;
            loop ()
        | Some _ -> ())
  in
  loop ();
  Buffer.contents buf

(* 4.3.12 Consume an ident sequence, at the code-point level (section 4.2, see
   [is_name_at]). Fast path (no [\] escape) returns [String.sub] of the source
   with no Buffer; the slow path seeds a Buffer with the copied prefix. *)
let consume_ident_sequence r =
  let src = Reader.source r in
  let src_len = String.length src in
  let start = Reader.position r in
  match scan_ident_fast r src src_len with
  | `End -> String.sub src start (Reader.position r - start)
  | `Escape ->
      let buf = Buffer.create 16 in
      Buffer.add_substring buf src start (Reader.position r - start);
      scan_ident_slow r src buf

let consume_string_escape r buf =
  Reader.skip r;
  match at r 0 with
  | '\000' -> ()
  | c when is_newline c ->
      Reader.skip r;
      if c = '\r' && at r 0 = '\n' then Reader.skip r
  | _ -> Buffer.add_string buf (consume_escape r)

(* The first offset at or after [i] that ends the plain run of a string opened
   by [quote]: the closing quote, a backslash, a newline or the end. *)
let rec plain_string_end src len quote i =
  if i >= len then i
  else
    let c = String.unsafe_get src i in
    if c = quote || c = '\\' || is_newline c then i
    else plain_string_end src len quote (i + 1)

let rec skip_bytes r n =
  if n > 0 then (
    Reader.skip r;
    skip_bytes r (n - 1))

(* 4.3.11 Consume a string token. Assumes opening quote already consumed. A
   string with no escape, which is nearly every one, is a slice of the source;
   the rest is built in a buffer seeded with the plain prefix. *)
let consume_string_token ~quote r =
  let src = Reader.source r in
  let start = Reader.position r in
  let stop = plain_string_end src (String.length src) quote start in
  skip_bytes r (stop - start);
  if at r 0 = quote then (
    Reader.skip r;
    String
      { value = String.sub src start (stop - start); quote; terminated = true })
  else
    let buf = Buffer.create (stop - start + 16) in
    Buffer.add_substring buf src start (stop - start);
    let quote_byte = Char.code quote in
    let rec loop () =
      let b = Reader.peek_byte r in
      if b = -1 then
        String { value = Buffer.contents buf; quote; terminated = false }
      else if b = quote_byte then (
        Reader.skip r;
        String { value = Buffer.contents buf; quote; terminated = true })
      else
        let c = Char.unsafe_chr b in
        if is_newline c then Bad_string
        else if c = '\\' then (
          consume_string_escape r buf;
          loop ())
        else (
          Buffer.add_char buf c;
          Reader.skip r;
          loop ())
    in
    loop ()

let rec skip_number_digits r =
  let b = Reader.peek_byte r in
  if b >= Char.code '0' && b <= Char.code '9' then (
    Reader.skip r;
    skip_number_digits r)

let skip_number_sign r =
  match at r 0 with '+' | '-' -> Reader.skip r | _ -> ()

let skip_fraction r is_int =
  if at r 0 = '.' && is_digit (at r 1) then (
    Reader.skip r;
    is_int := false;
    skip_number_digits r)

let exponent_continues_number r =
  match at r 0 with
  | 'e' | 'E' -> (
      match at r 1 with
      | c when is_digit c -> true
      | '+' | '-' -> is_digit (at r 2)
      | _ -> false)
  | _ -> false

let skip_exponent r is_int =
  if exponent_continues_number r then (
    Reader.skip r;
    is_int := false;
    skip_number_sign r;
    skip_number_digits r)

(* Whether [repr] can name a value the eight-decimal print does not reach: more
   than eight fraction digits, or an exponent that moves them. *)
let rec past_printed_precision_from repr i fraction =
  if i >= String.length repr then fraction > 8
  else
    match String.unsafe_get repr i with
    | 'e' | 'E' -> true
    | '.' -> past_printed_precision_from repr (i + 1) 0
    | '0' .. '9' when fraction >= 0 ->
        past_printed_precision_from repr (i + 1) (fraction + 1)
    | _ -> past_printed_precision_from repr (i + 1) fraction

let past_printed_precision repr = past_printed_precision_from repr 0 (-1)

(* 4.3.14 Consume a number. Its representation is the run of source it spans,
   which no escape can enter. *)
let consume_number r =
  let start = Reader.position r in
  let is_int = ref true in
  skip_number_sign r;
  skip_number_digits r;
  skip_fraction r is_int;
  skip_exponent r is_int;
  let repr = String.sub (Reader.source r) start (Reader.position r - start) in
  let value = try float_of_string repr with Failure _ -> 0.0 in
  (* The printer writes a number to eight decimals, so a value read past them is
     the one that print reads back as: the node one text names. The
     representation keeps every authored digit for a stream that is printed as
     written. *)
  let value =
    if (not (Float.is_integer value)) && past_printed_precision repr then
      Pp.printed value
    else value
  in
  let number_flag = if !is_int then Integer else Number in
  { value; repr; number_flag }

(* 4.3.3 Consume a numeric token. *)
let consume_numeric_token r =
  let n = consume_number r in
  if would_start_ident_sequence r then
    let unit_ = consume_ident_sequence r in
    Dimension { number = n; unit_ }
  else if Reader.peek_byte r = Char.code '%' then (
    Reader.skip r;
    Percentage n)
  else Number_tok n

(* 4.3.6 Consume the remnants of a bad url. *)
let consume_remnants_of_bad_url r =
  let rec loop () =
    match at r 0 with
    | '\000' -> ()
    | ')' -> Reader.skip r
    | '\\' when valid_escape_at r ->
        Reader.skip r;
        let _ = consume_escape r in
        loop ()
    | _ ->
        Reader.skip r;
        loop ()
  in
  loop ()

(* The first offset at or after [i] that ends the plain run of a url: a byte
   that closes it, breaks it or starts an escape, or the end. *)
let rec plain_url_end src len i =
  if i >= len then i
  else
    match String.unsafe_get src i with
    | ')' | '"' | '\'' | '(' | '\\' -> i
    | c when is_ws c || Char.code c < 0x20 || Char.code c = 0x7F -> i
    | _ -> plain_url_end src len (i + 1)

let rec skip_ws r =
  if is_ws (at r 0) then (
    Reader.skip r;
    skip_ws r)

(* The rest of a url token from where its plain run ended, [buf] holding the
   run. *)
let rec consume_url_rest r buf =
  match at r 0 with
  (* CSS Syntax 3 (ED) section 4.3.6: EOF inside [url(] is a parse error but
     still returns a [<url-token>], surfacing via recovery warnings rather than
     a [<bad-url-token>]. *)
  | '\000' -> Url (Buffer.contents buf)
  | ')' ->
      Reader.skip r;
      Url (Buffer.contents buf)
  | c when is_ws c -> (
      skip_ws r;
      match at r 0 with
      | '\000' -> Url (Buffer.contents buf)
      | ')' ->
          Reader.skip r;
          Url (Buffer.contents buf)
      | _ ->
          consume_remnants_of_bad_url r;
          Bad_url)
  | '"' | '\'' | '(' ->
      consume_remnants_of_bad_url r;
      Bad_url
  | c when Char.code c < 0x20 || Char.code c = 0x7F ->
      consume_remnants_of_bad_url r;
      Bad_url
  | '\\' ->
      if valid_escape_at r then (
        Reader.skip r;
        Buffer.add_string buf (consume_escape r);
        consume_url_rest r buf)
      else (
        consume_remnants_of_bad_url r;
        Bad_url)
  | c ->
      Buffer.add_char buf c;
      Reader.skip r;
      consume_url_rest r buf

(* 4.3.6 Consume a url token. Assumes "url(" has been consumed. A url with no
   escape and no space before its [)], nearly every one, is a slice of the
   source; the rest is built in a buffer seeded with the plain prefix. *)
let consume_url_token r =
  skip_ws r;
  let src = Reader.source r in
  let start = Reader.position r in
  let stop = plain_url_end src (String.length src) start in
  skip_bytes r (stop - start);
  if at r 0 = ')' then (
    Reader.skip r;
    Url (String.sub src start (stop - start)))
  else
    let buf = Buffer.create (stop - start + 16) in
    Buffer.add_substring buf src start (stop - start);
    consume_url_rest r buf

(* 4.3.4 Check if three code points would start a unicode-range token. The
   reader is positioned at the leading [U]/[u]; we look at offsets 0..2. *)
let would_start_unicode_range r =
  match at r 0 with
  | 'U' | 'u' ->
      at r 1 = '+'
      &&
      let c = at r 2 in
      is_hex c || c = '?'
  | _ -> false

(* 4.3.14 Consume a unicode-range token. Assumes [would_start_unicode_range]
   just returned true. *)
let consume_unicode_range_hex r buf =
  let rec loop n =
    let c = at r 0 in
    if n < 6 && is_hex c then (
      Buffer.add_char buf c;
      Reader.skip r;
      loop (n + 1))
    else n
  in
  loop 0

let consume_unicode_range_wildcards r n_hex =
  let rec loop n =
    if n_hex + n <> 6 && at r 0 = '?' then (
      Reader.skip r;
      loop (n + 1))
    else n
  in
  loop 0

let unicode_range_has_tail r = at r 0 = '-' && is_hex (at r 1)
let unicode_range_value s = int_of_string (String.concat "" [ "0x"; s ])

let unicode_range_wildcard_token start_repr n_q =
  let start_value =
    unicode_range_value (String.concat "" [ start_repr; String.make n_q '0' ])
  in
  let end_value =
    unicode_range_value (String.concat "" [ start_repr; String.make n_q 'F' ])
  in
  let form =
    Token.Wildcard { prefix_width = String.length start_repr; wildcards = n_q }
  in
  Unicode_range { start_value; end_value; form }

let unicode_range_tail_token r start_repr start_value =
  Reader.skip r;
  let end_buf = Buffer.create 6 in
  ignore (consume_unicode_range_hex r end_buf : int);
  let end_repr = Buffer.contents end_buf in
  let end_value = unicode_range_value end_repr in
  let form =
    Token.Range
      {
        start_width = String.length start_repr;
        end_width = String.length end_repr;
      }
  in
  Unicode_range { start_value; end_value; form }

let consume_unicode_range_token r =
  Reader.skip r;
  Reader.skip r;
  let buf = Buffer.create 6 in
  let n_hex = consume_unicode_range_hex r buf in
  let n_q = consume_unicode_range_wildcards r n_hex in
  let start_repr = Buffer.contents buf in
  if n_q > 0 then unicode_range_wildcard_token start_repr n_q
  else
    let start_value = unicode_range_value start_repr in
    if unicode_range_has_tail r then
      unicode_range_tail_token r start_repr start_value
    else
      let form = Token.Single { width = String.length start_repr } in
      Unicode_range { start_value; end_value = start_value; form }

(* [name] is [url] in any ASCII case. *)
let is_url_name name =
  String.length name = 3
  && Char.equal (Char.lowercase_ascii name.[0]) 'u'
  && Char.equal (Char.lowercase_ascii name.[1]) 'r'
  && Char.equal (Char.lowercase_ascii name.[2]) 'l'

(* 4.3.10 Consume an ident-like token. *)
let consume_ident_like_token ~force_url_function r =
  let name = consume_ident_sequence r in
  if is_url_name name && Reader.peek_byte r = Char.code '(' then (
    Reader.skip r;
    (* While the next two code points are whitespace, consume one. *)
    let rec skip_ws_keep_one () =
      if is_ws (at r 0) && is_ws (at r 1) then (
        Reader.skip r;
        skip_ws_keep_one ())
    in
    skip_ws_keep_one ();
    (* If the next one or two code points are a quote, or ws followed by a
       quote, emit <function-token>, not a url token. *)
    let is_quote c = c = '"' || c = '\'' in
    let is_function_url =
      let c0 = at r 0 in
      is_quote c0 || (is_ws c0 && is_quote (at r 1))
    in
    if force_url_function || is_function_url then Function name
    else consume_url_token r)
  else if Reader.peek_byte r = Char.code '(' then (
    Reader.skip r;
    Function name)
  else Ident name

(* 4.3.2 Consume one comment body, assuming [/*] was already consumed. An
   unterminated comment is a parse error per the spec but we just stop at EOF
   silently. *)
let rec skip_comment_body r =
  if Reader.is_done r then false
  else if at r 0 = '*' && at r 1 = '/' then (
    Reader.skip r;
    Reader.skip r;
    true)
  else (
    Reader.skip r;
    skip_comment_body r)

let consume_comment on_comment r =
  let start_pos = Reader.position r in
  Reader.skip r;
  Reader.skip r;
  let terminated = skip_comment_body r in
  let end_pos = Reader.position r in
  Option.iter
    (fun f -> f { loc = Loc.v ~start_pos ~end_pos; terminated })
    on_comment

(* Skip a run of comments without consuming surrounding whitespace: CSS Syntax 3
   (ED) sec. 4.3.2 treats comments as "nothing", so a comment between two
   non-whitespace points disappears rather than becoming a
   <whitespace-token>. *)
let rec skip_comment_run on_comment r =
  if looking_at_comment r then (
    consume_comment on_comment r;
    skip_comment_run on_comment r)

(* Consume a run of whitespace code points and any interleaved comments,
   collecting the whitespace into [buf]. CSS Custom Properties 1 (ED) sec. 4.1
   forbids normalizing the whitespace of a custom property's token stream, so
   the run's own text is what the token carries; a comment inside it is not
   whitespace and does not join it. *)
let rec consume_whitespace_run buf on_comment r =
  let c = at r 0 in
  if is_ws c then (
    Buffer.add_char buf c;
    Reader.skip r;
    consume_whitespace_run buf on_comment r)
  else if looking_at_comment r then (
    consume_comment on_comment r;
    consume_whitespace_run buf on_comment r)

let hash_flag_now r = if would_start_ident_sequence r then Id else Unrestricted

(* 4.3.1 sub-case: [#] has already been consumed. *)
let consume_hash_token r =
  if is_name_at r 0 || valid_escape_at r then
    let hash_flag = hash_flag_now r in
    let value = consume_ident_sequence r in
    Hash { value; hash_flag }
  else Delim "#"

(* 4.3.1 sub-case: reader is at [-], not yet consumed. *)
let consume_hyphen_start r =
  if would_start_number r then consume_numeric_token r
  else if Reader.looking_at r "-->" then (
    Reader.skip r;
    Reader.skip r;
    Reader.skip r;
    Cdc)
  else if would_start_ident_sequence r then
    consume_ident_like_token ~force_url_function:false r
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
(* Multi-byte lead or ASCII name-start: consume a full ident-like token, or emit
   a single-code-point [Delim] whose UTF-8 bytes are consumed atomically so the
   delim carries the whole code point. *)
let consume_name_start_or_delim ~force_url_function r c =
  if is_name_start_at r 0 then consume_ident_like_token ~force_url_function r
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
        Delim (byte_string c))
  else (
    Reader.skip r;
    Delim (byte_string c))

let next_token ~force_url_function ~unicode_ranges ~on_comment r =
  let b = Reader.peek_byte r in
  if b = -1 then Eof
  else
    let c = Char.unsafe_chr b in
    if is_ws c then (
      let src = Reader.source r in
      let start = Reader.position r in
      skip_ws r;
      let stop = Reader.position r in
      if looking_at_comment r then (
        let buf = Buffer.create 8 in
        Buffer.add_substring buf src start (stop - start);
        consume_whitespace_run buf on_comment r;
        Whitespace (Buffer.contents buf))
      else if stop - start = 1 then Whitespace (byte_string c)
      else Whitespace (String.sub src start (stop - start)))
    else
      match c with
      | '"' ->
          Reader.skip r;
          consume_string_token ~quote:'"' r
      | '\'' ->
          Reader.skip r;
          consume_string_token ~quote:'\'' r
      | '#' ->
          Reader.skip r;
          consume_hash_token r
      | '(' ->
          Reader.skip r;
          Open Paren
      | ')' ->
          Reader.skip r;
          Close Paren
      | '+' when would_start_number r -> consume_numeric_token r
      | '+' ->
          Reader.skip r;
          Delim "+"
      | ',' ->
          Reader.skip r;
          Comma
      | '-' -> consume_hyphen_start r
      | '.' when would_start_number r -> consume_numeric_token r
      | '.' ->
          Reader.skip r;
          Delim "."
      | ':' ->
          Reader.skip r;
          Colon
      | ';' ->
          Reader.skip r;
          Semicolon
      | '<' -> consume_less_than r
      | '@' ->
          Reader.skip r;
          consume_at_start r
      | '[' ->
          Reader.skip r;
          Open Square
      | '\\' when valid_escape_at r ->
          consume_ident_like_token ~force_url_function:false r
      | '\\' ->
          Reader.skip r;
          Delim "\\"
      | ']' ->
          Reader.skip r;
          Close Square
      | '{' ->
          Reader.skip r;
          Open Curly
      | '}' ->
          Reader.skip r;
          Close Curly
      | c when is_digit c -> consume_numeric_token r
      | ('U' | 'u') when unicode_ranges && would_start_unicode_range r ->
          consume_unicode_range_token r
      | c when is_name_start_ascii c || c >= '\x80' ->
          consume_name_start_or_delim ~force_url_function r c
      | c ->
          Reader.skip r;
          Delim (byte_string c)

(** {1 Stream API (uniform with other stages)} *)

(* Whether the source holds a [\\] in [\[start, stop)]. *)
let has_backslash t start stop =
  if t.backslash < start then
    t.backslash <-
      (match String.index_from_opt (Reader.source t.reader) start '\\' with
      | Some i -> i
      | None -> max_int);
  t.backslash < stop

(* Wrap a tokenizer step with source-location capture. The comments before a
   token are skipped first, so its location and source text start at the token
   itself: a rule opened by that token does not span a comment written in front
   of it. *)
let tokenize_with_loc t ~force_url_function =
  let reader = t.reader and on_comment = t.on_comment in
  skip_comment_run on_comment reader;
  let start_pos = Reader.position reader in
  let kind =
    next_token ~force_url_function ~unicode_ranges:t.unicode_ranges ~on_comment
      reader
  in
  let end_pos = Reader.position reader in
  (* Section 9.1 serializes an ident by escaping only what must be, so a token
     the author wrote with any other escape does not come back byte for byte.
     Keep the source text for exactly those; a backslash is what marks one. *)
  let repr =
    if has_backslash t start_pos end_pos then
      Some (String.sub (Reader.source reader) start_pos (end_pos - start_pos))
    else None
  in
  Token.of_source ~repr ~kind ~loc:(Loc.v ~start_pos ~end_pos)

let record_consume t tok =
  t.last <- tok;
  match t.saves with
  | [] -> ()
  | saves -> List.iter (fun save -> save.trace <- tok :: save.trace) saves

(* True iff the previous token was [Url(...)] with no separator (whitespace or
   comment) after: the next [url(...)] would re-enter the url branch and merge,
   so force a function-call to keep them distinct. Per CSS Syntax 3 (ED) sec.
   4.3.2 comments act like whitespace, so a [/* */] between the urls also
   disarms this. Only a token read from the source asks, and every token pushed
   back has been taken again by then, so [last] is the one the source position
   follows. *)
let force_url_function t =
  match t.last with
  | { kind = Url _; loc; _ } when loc.end_pos = Reader.position t.reader ->
      not (is_ws (at t.reader 0) || looking_at_comment t.reader)
  | _ -> false

let read t = tokenize_with_loc t ~force_url_function:(force_url_function t)

let next t =
  let tok =
    if t.ahead != none then (
      let tok = t.ahead in
      t.ahead <- none;
      tok)
    else
      match t.buffer with
      | tok :: rest ->
          t.buffer <- rest;
          tok
      | [] -> read t
  in
  record_consume t tok;
  tok

let peek t =
  if t.ahead != none then t.ahead
  else
    match t.buffer with
    | tok :: _ -> tok
    | [] ->
        let tok = read t in
        t.ahead <- tok;
        tok

(* [ahead] moves to the front of [buffer], so whatever goes back goes before
   it. *)
let flush_ahead t =
  if t.ahead != none then (
    t.buffer <- t.ahead :: t.buffer;
    t.ahead <- none)

let reconsume t tok =
  (* Pushback returns [tok] to the front of the buffer; also undo its trace
     entry so a subsequent {!save}/{!restore} pair sees the correct tokens. *)
  flush_ahead t;
  t.buffer <- tok :: t.buffer;
  List.iter
    (fun save ->
      match save.trace with
      | hd :: rest when hd == tok -> save.trace <- rest
      | _ -> ())
    t.saves

let save t = t.saves <- { trace = [] } :: t.saves

let restore t =
  match t.saves with
  | [] -> failwith "Lexer.restore: no saved position"
  | save :: rest ->
      t.saves <- rest;
      flush_ahead t;
      t.buffer <- List.rev_append save.trace t.buffer

let commit t =
  match t.saves with
  | [] -> failwith "Lexer.commit: no saved position"
  | save :: rest -> (
      t.saves <- rest;
      match rest with
      | [] -> ()
      | parent :: _ -> parent.trace <- List.rev_append save.trace parent.trace)

let is_done t = t.ahead == none && t.buffer = [] && Reader.is_done t.reader
