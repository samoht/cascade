(** Stage 2 stream: characters -> Token.t.

    Wraps a {!Reader.t} char cursor and exposes the CSS Syntax 3 (ED) sec. 4
    tokenizer through the uniform [next / peek / reconsume] triple. The token
    buffer doubles as a backtracking stack: {!save}/{!restore} mark and replay,
    mirroring {!Reader.save}/{!Reader.restore} for the Token layer. *)

open Token

type comment = { loc : Loc.t; terminated : bool }

type t = {
  reader : Reader.t;
  on_comment : (comment -> unit) option;
  (* CSS Syntax 3 (ED) sec. 4.3.1 takes "unicode ranges allowed", defaulting to
     false; sec. 4.3.14 names its one caller, the value of a unicode-range
     descriptor. With it unset, [u+a] is an ident, a delim and an ident. *)
  mutable unicode_ranges : bool;
  (* Token buffer: most-recently-pushed-back at the head. [next] takes from here
     before falling through to the reader. *)
  mutable buffer : Token.t list;
  mutable history : Token.t list;
  (* Stack of saved positions. Each entry holds the tokens consumed since [save]
     was called; [restore] replays them at the head of [buffer]. *)
  mutable saves : save list;
}

and save = { trace : Token.t list ref; saved_history : Token.t list }

let of_reader ?on_comment ?(unicode_ranges = false) reader =
  { reader; on_comment; unicode_ranges; buffer = []; history = []; saves = [] }

let of_string ?enforce_spec ?on_comment ?unicode_ranges s =
  of_reader ?on_comment ?unicode_ranges (Reader.of_string ?enforce_spec s)

let with_unicode_ranges t f =
  let saved = t.unicode_ranges in
  t.unicode_ranges <- true;
  Fun.protect ~finally:(fun () -> t.unicode_ranges <- saved) f

let source t = Reader.source t.reader

(** {1 Tokenization} *)

(* 4.2 "name code points" *)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ws c = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\012'
let is_newline c = c = '\n' || c = '\r' || c = '\012'

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
  match (Reader.peek r, Reader.peek_at r 1) with
  | Some '\\', Some c -> not (is_newline c)
  | _ -> false

(* 4.3.4 Check if three code points starting at [offset] would start an ident
   sequence. Multi-byte code points are consulted via [is_name_start_at] (which
   decodes at the given byte offset) rather than byte-level checks. *)
let would_start_ident_sequence_at r offset =
  match Reader.peek_at r offset with
  | None -> false
  | Some c -> (
      match c with
      | '-' -> (
          match Reader.peek_at r (offset + 1) with
          | Some c1 when is_name_start_at r (offset + 1) || c1 = '-' -> true
          | Some '\\' -> (
              match Reader.peek_at r (offset + 2) with
              | Some c2 -> not (is_newline c2)
              | None -> false)
          | _ -> false)
      | '\\' -> (
          match Reader.peek_at r (offset + 1) with
          | Some c1 -> not (is_newline c1)
          | None -> false)
      | _ -> is_name_start_at r offset)

let would_start_ident_sequence r = would_start_ident_sequence_at r 0

(* 4.3.5 Check if three code points would start a number. *)
let would_start_number_at r offset =
  match Reader.peek_at r offset with
  | None -> false
  | Some c -> (
      match c with
      | '+' | '-' -> (
          match Reader.peek_at r (offset + 1) with
          | Some c1 when is_digit c1 -> true
          | Some '.' -> (
              match Reader.peek_at r (offset + 2) with
              | Some c2 -> is_digit c2
              | None -> false)
          | _ -> false)
      | '.' -> (
          match Reader.peek_at r (offset + 1) with
          | Some c1 -> is_digit c1
          | None -> false)
      | c -> is_digit c)

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
  match Reader.peek r with
  | Some c when is_ws c ->
      if
        c = '\r'
        && match Reader.peek_at r 1 with Some '\n' -> true | _ -> false
      then (
        Reader.skip r;
        Reader.skip r)
      else Reader.skip r
  | _ -> ()

let consume_escape_hex r first =
  let buf = Buffer.create 6 in
  Buffer.add_char buf first;
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
  consume_escape_trailing_ws r;
  let hex = Buffer.contents buf in
  let cp = try int_of_string ("0x" ^ hex) with Failure _ -> 0xFFFD in
  utf8_of_codepoint cp

let consume_escape r =
  match Reader.peek r with
  | None -> "\u{FFFD}" (* parse error *)
  | Some c when is_hex c -> consume_escape_hex r c
  | Some c ->
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

(* Slow path: an escape has been encountered. Seeded with the already-consumed
   no-escape prefix in [buf], extend [buf] byte-by-byte from here on. *)
let scan_ident_slow r src buf =
  let consume_hash_suffix () =
    match Reader.peek r with
    | Some '#' ->
        Buffer.add_char buf '#';
        Reader.skip r
    | _ -> ()
  in
  let rec loop () =
    match Reader.peek r with
    | Some '\\' when valid_escape_at r ->
        let esc_start = Reader.position r in
        let hex_escape =
          match Reader.peek_at r 1 with Some c -> is_hex c | None -> false
        in
        Reader.skip r;
        Buffer.add_string buf (consume_escape r);
        let consumed_hex_terminator =
          let pos = Reader.position r in
          hex_escape && pos > esc_start + 2 && is_ws src.[pos - 1]
        in
        if not consumed_hex_terminator then consume_hash_suffix ();
        loop ()
    | Some _ -> (
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
    | None -> ()
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
  match Reader.peek r with
  | None -> ()
  | Some c when is_newline c -> (
      Reader.skip r;
      if c = '\r' then
        match Reader.peek r with Some '\n' -> Reader.skip r | _ -> ())
  | Some _ -> Buffer.add_string buf (consume_escape r)

(* 4.3.11 Consume a string token. Assumes opening quote already consumed. *)
let consume_string_token ~quote r =
  let buf = Buffer.create 32 in
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

let take_number_digits r buf =
  let rec loop () =
    let b = Reader.peek_byte r in
    if b >= Char.code '0' && b <= Char.code '9' then (
      Buffer.add_char buf (Char.unsafe_chr b);
      Reader.skip r;
      loop ())
  in
  loop ()

let consume_number_sign r buf =
  match Reader.peek r with
  | Some (('+' | '-') as c) ->
      Buffer.add_char buf c;
      Reader.skip r
  | _ -> ()

let consume_fraction r buf is_int =
  match (Reader.peek r, Reader.peek_at r 1) with
  | Some '.', Some c when is_digit c ->
      Buffer.add_char buf '.';
      Reader.skip r;
      is_int := false;
      take_number_digits r buf
  | _ -> ()

let exponent_continues_number r =
  match (Reader.peek r, Reader.peek_at r 1, Reader.peek_at r 2) with
  | Some ('e' | 'E'), Some c, _ when is_digit c -> true
  | Some ('e' | 'E'), Some ('+' | '-'), Some c when is_digit c -> true
  | _ -> false

let consume_exponent r buf is_int =
  let b = Reader.peek_byte r in
  if b >= 0 && exponent_continues_number r then (
    Buffer.add_char buf (Char.unsafe_chr b);
    Reader.skip r;
    is_int := false;
    consume_number_sign r buf;
    take_number_digits r buf)

(* 4.3.14 Consume a number. *)
let consume_number r =
  let buf = Buffer.create 16 in
  let is_int = ref true in
  consume_number_sign r buf;
  take_number_digits r buf;
  consume_fraction r buf is_int;
  consume_exponent r buf is_int;
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
  else if Reader.peek_byte r = Char.code '%' then (
    Reader.skip r;
    Percentage n)
  else Number_tok n

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
    (* CSS Syntax 3 (ED) section 4.3.6: EOF inside [url(] is a parse error but
       still returns a [<url-token>], surfacing via recovery warnings rather
       than a [<bad-url-token>]. *)
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
  match (Reader.peek r, Reader.peek_at r 1, Reader.peek_at r 2) with
  | Some ('U' | 'u'), Some '+', Some c when is_hex c || c = '?' -> true
  | _ -> false

(* 4.3.14 Consume a unicode-range token. Assumes [would_start_unicode_range]
   just returned true. *)
let consume_unicode_range_hex r buf =
  let rec loop n =
    match (n, Reader.peek r) with
    | 6, _ -> n
    | _, Some c when is_hex c ->
        Buffer.add_char buf c;
        Reader.skip r;
        loop (n + 1)
    | _ -> n
  in
  loop 0

let consume_unicode_range_wildcards r n_hex =
  let rec loop n =
    match (n_hex + n, Reader.peek r) with
    | 6, _ -> n
    | _, Some '?' ->
        Reader.skip r;
        loop (n + 1)
    | _ -> n
  in
  loop 0

let unicode_range_has_tail r =
  match (Reader.peek r, Reader.peek_at r 1) with
  | Some '-', Some c when is_hex c -> true
  | _ -> false

let unicode_range_value s = int_of_string ("0x" ^ s)

let unicode_range_wildcard_token start_repr n_q =
  let start_value = unicode_range_value (start_repr ^ String.make n_q '0') in
  let end_value = unicode_range_value (start_repr ^ String.make n_q 'F') in
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

(* 4.3.10 Consume an ident-like token. *)
let consume_ident_like_token ?(force_url_function = false) r =
  let name = consume_ident_sequence r in
  let lower = String.lowercase_ascii name in
  if lower = "url" && Reader.peek_byte r = Char.code '(' then (
    Reader.skip r;
    (* While the next two code points are whitespace, consume one. *)
    let rec skip_ws_keep_one () =
      match (Reader.peek r, Reader.peek_at r 1) with
      | Some c0, Some c1 when is_ws c0 && is_ws c1 ->
          Reader.skip r;
          skip_ws_keep_one ()
      | _ -> ()
    in
    skip_ws_keep_one ();
    (* If the next one or two code points are a quote, or ws followed by a
       quote, emit <function-token>, not a url token. *)
    let is_function_url =
      match (Reader.peek r, Reader.peek_at r 1) with
      | Some ('"' | '\''), _ -> true
      | Some c0, Some ('"' | '\'') when is_ws c0 -> true
      | _ -> false
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
  else if Reader.looking_at r "*/" then (
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
  if Reader.looking_at r "/*" then (
    consume_comment on_comment r;
    skip_comment_run on_comment r)

(* Consume a run of whitespace code points and any interleaved comments,
   returning [true] when at least one whitespace code point was seen. *)
let rec consume_whitespace_run on_comment r =
  match Reader.peek r with
  | Some c when is_ws c ->
      Reader.skip r;
      consume_whitespace_run on_comment r
  | _ ->
      if Reader.looking_at r "/*" then (
        consume_comment on_comment r;
        consume_whitespace_run on_comment r)

let hash_flag_now r = if would_start_ident_sequence r then Id else Unrestricted

(* 4.3.1 sub-case: [#] has already been consumed. *)
let consume_hash_token r =
  match Reader.peek r with
  | Some _ when is_name_at r 0 ->
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
        Delim (String.make 1 c))
  else (
    Reader.skip r;
    Delim (String.make 1 c))

let next_token ?(force_url_function = false) ?(unicode_ranges = false)
    ?on_comment r =
  skip_comment_run on_comment r;
  let b = Reader.peek_byte r in
  if b = -1 then Eof
  else
    let c = Char.unsafe_chr b in
    if is_ws c then (
      consume_whitespace_run on_comment r;
      Whitespace)
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
      | '\\' when valid_escape_at r -> consume_ident_like_token r
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
          Delim (String.make 1 c)

(** {1 Stream API (uniform with other stages)} *)

(* Wrap a tokenizer step with source-location capture. *)
let tokenize_with_loc ?(force_url_function = false) ?(unicode_ranges = false)
    ?on_comment reader =
  let start_pos = Reader.position reader in
  let kind =
    next_token ~force_url_function ~unicode_ranges ?on_comment reader
  in
  let end_pos = Reader.position reader in
  Token.v ~kind ~loc:(Loc.v ~start_pos ~end_pos)

(* [history] need only retain tokens since the last active [save]. With no saves
   the head is all [force_url_function]/[reconsume] read, so keep a one-element
   list; with saves active keep the full list so [restore] can rewind. *)
let record_consume t tok =
  match t.saves with
  | [] -> t.history <- [ tok ]
  | saves ->
      t.history <- tok :: t.history;
      List.iter (fun save -> save.trace := tok :: !(save.trace)) saves

(* True iff the previous token was [Url(...)] with no separator (whitespace or
   comment) after: the next [url(...)] would re-enter the url branch and merge,
   so force a function-call to keep them distinct. Per CSS Syntax 3 (ED) sec.
   4.3.2 comments act like whitespace, so a [/* */] between the urls also
   disarms this. *)
let force_url_function t =
  match t.history with
  | { kind = Url _; loc; _ } :: _ when loc.end_pos = Reader.position t.reader
    -> (
      match Reader.peek t.reader with
      | Some c when is_ws c -> false
      | Some _ when Reader.looking_at t.reader "/*" -> false
      | _ -> true)
  | _ -> false

let next t =
  match t.buffer with
  | tok :: rest ->
      t.buffer <- rest;
      record_consume t tok;
      tok
  | [] ->
      let tok =
        tokenize_with_loc ~force_url_function:(force_url_function t)
          ~unicode_ranges:t.unicode_ranges ?on_comment:t.on_comment t.reader
      in
      record_consume t tok;
      tok

let peek t =
  match t.buffer with
  | tok :: _ -> tok
  | [] ->
      let tok =
        tokenize_with_loc ~force_url_function:(force_url_function t)
          ~unicode_ranges:t.unicode_ranges ?on_comment:t.on_comment t.reader
      in
      t.buffer <- [ tok ];
      tok

let reconsume t tok =
  (* Pushback returns [tok] to the front of the buffer; also undo its trace
     entry so a subsequent {!save}/{!restore} pair sees the correct tokens. *)
  t.buffer <- tok :: t.buffer;
  (match t.history with
  | hd :: rest when hd == tok -> t.history <- rest
  | _ -> ());
  List.iter
    (fun save ->
      match !(save.trace) with
      | hd :: rest when hd == tok -> save.trace := rest
      | _ -> ())
    t.saves

let save t =
  let trace = ref [] in
  t.saves <- { trace; saved_history = t.history } :: t.saves

let restore t =
  match t.saves with
  | [] -> failwith "Lexer.restore: no saved position"
  | save :: rest ->
      t.saves <- rest;
      t.history <- save.saved_history;
      t.buffer <- List.rev_append !(save.trace) t.buffer

let commit t =
  match t.saves with
  | [] -> failwith "Lexer.commit: no saved position"
  | save :: rest -> (
      t.saves <- rest;
      match rest with
      | [] -> ()
      | parent :: _ ->
          parent.trace := List.rev_append !(save.trace) !(parent.trace))

let is_done t = t.buffer = [] && Reader.is_done t.reader
