(** Simple CSS parser implementation. *)

type t = {
  input : string;
  len : int;
  mutable pos : int;
  mutable saved : int list; (* Stack of saved positions for backtracking *)
  mutable call_stack : string list; (* Stack of parsing contexts for debugging *)
}

type parse_error = {
  message : string;
  got : string option;
  position : int;
  filename : string;
  context_window : string;
  marker_pos : int;
  callstack : string list;
}
(** Parse error information with structured details. *)

exception Parse_error of parse_error

(** Pretty print parse error with debugging information *)
let pp_parse_error (err : parse_error) =
  let callstack_str =
    if err.callstack = [] then ""
    else "\n    [stack: " ^ String.concat " -> " err.callstack ^ "]"
  in
  let context_str =
    if err.context_window = "" then ""
    else
      (* Don't trim the context - show it as-is to preserve position accuracy *)
      let context_lines = String.split_on_char '\n' err.context_window in
      let context_display =
        match context_lines with
        | [] -> err.context_window
        | [ line ] -> line (* Single line - show it all *)
        | _ ->
            (* If multi-line, show each line *)
            String.concat "\n" context_lines
      in
      let marker =
        if
          err.marker_pos > 0
          && err.marker_pos <= String.length err.context_window
        then String.make err.marker_pos ' ' ^ "^"
        else
          (* Fallback if marker position is out of bounds *)
          String.make 20 ' ' ^ "^"
      in
      "\n" ^ context_display ^ "\n" ^ marker
  in
  err.message ^ " at " ^ err.filename ^ ":" ^ string_of_int err.position
  ^ callstack_str ^ context_str

(* Pretty-printer for the parser state *)
let pp (ctx : Pp.ctx) (t : t) =
  Pp.string ctx "<reader pos=";
  Pp.string ctx (string_of_int t.pos);
  Pp.string ctx ">"

(** {1 Creation} *)

(* CSS Syntax Level 3 section 3.3 "Preprocessing the input stream": - Strip a
   leading U+FEFF BYTE ORDER MARK. - Replace any U+0000 NULL or surrogate code
   point with U+FFFD REPLACEMENT. (We operate post-UTF-8-decode; surrogates
   don't occur in valid UTF-8 so the NUL byte is the practical concern.) -
   Replace U+000D CARRIAGE RETURN, U+000C FORM FEED, and U+000D U+000A CRLF
   pairs with a single U+000A LINE FEED. *)
let has_bom input =
  let len = String.length input in
  len >= 3 && input.[0] = '\xEF' && input.[1] = '\xBB' && input.[2] = '\xBF'

let rec needs_preprocess input len i =
  i < len
  &&
  match input.[i] with
  | '\x00' | '\r' | '\x0C' -> true
  | _ -> needs_preprocess input len (i + 1)

let copy_preprocessed input len start =
  let buf = Buffer.create len in
  let fffd = "\xEF\xBF\xBD" in
  let i = ref start in
  while !i < len do
    let c = input.[!i] in
    (match c with
    | '\x00' -> Buffer.add_string buf fffd
    | '\r' ->
        Buffer.add_char buf '\n';
        if !i + 1 < len && input.[!i + 1] = '\n' then incr i
    | '\x0C' -> Buffer.add_char buf '\n'
    | _ -> Buffer.add_char buf c);
    incr i
  done;
  Buffer.contents buf

let preprocess input =
  let len = String.length input in
  let bom = has_bom input in
  let start = if bom then 3 else 0 in
  if (not bom) && not (needs_preprocess input len 0) then input
  else copy_preprocessed input len start

let of_string input =
  let input = preprocess input in
  { input; len = String.length input; pos = 0; saved = []; call_stack = [] }

let source t = t.input
let is_done t = t.pos >= t.len

let utf8_byte_length cp =
  if cp < 0x80 then 1
  else if cp < 0x800 then 2
  else if cp < 0x10000 then 3
  else 4

(* Decode the UTF-8 code point at [t.pos + offset] to [Some (cp, byte_length)],
   [None] at EOF or on a malformed sequence. Uutf rejects overlong/surrogate/
   out-of-range sequences per the Unicode spec. Returns [Some] only when the
   *first* decoded element is a valid [Uchar]: [fold_utf_8] resyncs past a
   [Malformed] start, which would fold bad bytes into the following code point
   (and into an ident-like unit token). *)
(* ASCII fast path before falling back to Uutf: skips the ref/closure
   allocation that a [Uutf.String.fold_utf_8] requires per peek. *)
let first_utf8_chunk_at input p len =
  if len <= 0 then None
  else
    let b = Char.code (String.unsafe_get input p) in
    if b < 0x80 then Some (Uchar.unsafe_of_int b)
    else
      let result = ref None in
      let seen = ref false in
      let folder () _ chunk =
        if !seen then ()
        else (
          seen := true;
          match chunk with `Uchar u -> result := Some u | `Malformed _ -> ())
      in
      Uutf.String.fold_utf_8 ~pos:p ~len folder () input;
      !result

let peek_utf8_at t offset =
  let p = t.pos + offset in
  if p >= t.len then None
  else
    let b = Char.code (String.unsafe_get t.input p) in
    if b < 0x80 then Some (b, 1)
    else
      let len = min 4 (t.len - p) in
      match first_utf8_chunk_at t.input p len with
      | None -> None
      | Some u ->
          let cp = Uchar.to_int u in
          Some (cp, utf8_byte_length cp)

let peek_utf8 t = peek_utf8_at t 0

let skip_utf8 t =
  match peek_utf8 t with
  | None -> if t.pos < t.len then t.pos <- t.pos + 1 (* advance past bad byte *)
  | Some (_, n) -> t.pos <- t.pos + n

(** {1 Call Stack Management} *)

let push_context t context = t.call_stack <- context :: t.call_stack

let pop_context t =
  match t.call_stack with
  | [] -> () (* No context to pop *)
  | _ :: rest -> t.call_stack <- rest

let with_context t context f =
  push_context t context;
  try
    let result = f () in
    pop_context t;
    result
  with exn ->
    pop_context t;
    raise exn

let callstack t = List.rev t.call_stack

let context_window ?(before = 40) ?(after = 40) t =
  let pos = t.pos in
  let start_pos = max 0 (pos - before) in
  let end_pos = min t.len (pos + after) in
  let before_text = String.sub t.input start_pos (pos - start_pos) in
  let after_text = String.sub t.input pos (end_pos - pos) in
  let context = before_text ^ after_text in
  let marker_pos = String.length before_text in
  (context, marker_pos)

(** Error helpers *)
let err ?got t expected =
  let context, marker_pos = context_window t in
  (* Calculate line and column numbers for better error reporting *)
  let line, col =
    let rec count_lines pos line col =
      if pos <= 0 then (line, col)
      else if pos >= String.length t.input then (line, col)
      else if t.input.[pos] = '\n' then count_lines (pos - 1) (line + 1) 1
      else count_lines (pos - 1) line (col + 1)
    in
    count_lines (t.pos - 1) 1 1
  in
  let better_filename =
    "<CSS input>:" ^ string_of_int line ^ ":" ^ string_of_int col
  in
  raise
    (Parse_error
       {
         message = expected;
         got;
         position = t.pos;
         filename = better_filename;
         context_window = context;
         marker_pos;
         callstack = callstack t;
       })

let err_eof t = err t "unexpected end of input"
let err_expected t what = err t ("expected " ^ what)

let err_expected_but_eof t what =
  err t ("Expected " ^ what ^ " but reached end of input")

let err_invalid_number t = err t "invalid number"
let err_invalid t what = err t ("invalid " ^ what)

(** {1 Error Utilities} *)

let with_filename error filename = { error with filename }

(** {1 Looking Ahead} *)

let peek t = if t.pos >= t.len then None else Some t.input.[t.pos]

let peek_at t offset =
  let p = t.pos + offset in
  if p < 0 || p >= t.len then None else Some (String.unsafe_get t.input p)

let peek_byte t =
  if t.pos >= t.len then -1 else Char.code (String.unsafe_get t.input t.pos)

let peek_byte_at t offset =
  let p = t.pos + offset in
  if p < 0 || p >= t.len then -1 else Char.code (String.unsafe_get t.input p)

let peek2 t =
  let n = min 2 (t.len - t.pos) in
  String.sub t.input t.pos n

let peek_string t n =
  let n = min n (t.len - t.pos) in
  String.sub t.input t.pos n

let looking_at t s =
  let slen = String.length s in
  if t.pos + slen > t.len then false
  else
    (* Iterative byte compare so [looking_at] (called once per [next_token]
       through [skip_comment_run]) does not allocate a closure for the inner
       loop. *)
    let i = ref 0 in
    let ok = ref true in
    while !ok && !i < slen do
      if String.unsafe_get t.input (t.pos + !i) <> String.unsafe_get s !i then
        ok := false
      else incr i
    done;
    !ok

(** {1 Reading Characters} *)

let skip t =
  if t.pos >= t.len then err_eof t;
  t.pos <- t.pos + 1

let skip_n t n =
  if t.pos + n > t.len then err_eof t;
  t.pos <- t.pos + n

let char t =
  if t.pos >= t.len then err_eof t;
  let c = t.input.[t.pos] in
  t.pos <- t.pos + 1;
  c

let expect c t =
  let actual_pos = t.pos in
  if t.pos >= t.len then err_expected_but_eof t ("'" ^ String.make 1 c ^ "'")
  else
    let actual = char t in
    if actual <> c then (
      (* Go back one position for better context display *)
      t.pos <- actual_pos;
      err t
        ("Expected '" ^ String.make 1 c ^ "' but got '" ^ String.make 1 actual
       ^ "'"))

let expect_string s t =
  let slen = String.length s in
  if not (looking_at t s) then err t ("expected \"" ^ s ^ "\"");
  skip_n t slen

(** Get current position in input *)
let position t = t.pos

(** Get context window around current position for better error messages *)
(* Removed duplicate context_window function - already defined above *)

let consume_if c t =
  if peek t = Some c then (
    skip t;
    true)
  else false

(** {1 Reading Strings} *)

let while_ t pred =
  let start = t.pos in
  while t.pos < t.len && pred t.input.[t.pos] do
    t.pos <- t.pos + 1
  done;
  String.sub t.input start (t.pos - start)

let until t c =
  let start = t.pos in
  while t.pos < t.len && t.input.[t.pos] <> c do
    t.pos <- t.pos + 1
  done;
  String.sub t.input start (t.pos - start)

(* CSS Syntax Module Level 3:
   https://www.w3.org/TR/css-syntax-3/#ident-start-code-point "An ident-start
   code point is a letter, a non-ASCII code point, or U+005F LOW LINE (_)." *)
let is_ident_start c =
  (* Allow ASCII letters and underscore, plus any non-ASCII byte (UTF-8). *)
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c = '_'
  || Char.code c >= 0x80

(* Check if a hyphen can start an identifier: valid unless followed by digit *)
let hyphen_can_start_ident t =
  if t.pos + 1 < t.len then
    let next_char = t.input.[t.pos + 1] in
    not (next_char >= '0' && next_char <= '9')
  else true (* Single dash at end is valid *)

(* CSS Syntax Module Level 3:
   https://www.w3.org/TR/css-syntax-3/#would-start-an-identifier Check if the
   current position would start a valid CSS identifier. *)
let would_start_identifier t =
  if t.pos >= t.len then false
  else
    match peek t with
    | Some c when is_ident_start c -> true
    | Some '\\' -> true (* Escape sequence *)
    | Some '-' -> hyphen_can_start_ident t
    | _ -> false

let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9') || c = '-'
let is_hex = Syntax.is_hex

(* Encode a Unicode codepoint as UTF-8. Out-of-range, surrogate, or negative
   inputs fall back to U+FFFD, matching CSS Syntax section 3.3. *)
let utf8_of_codepoint cp =
  let u =
    if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then Uchar.rep
    else Uchar.of_int cp
  in
  let buf = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buf u;
  Buffer.contents buf

let read_escape_consume_hex_digits t =
  let rec consume n =
    if n = 6 then ()
    else
      match peek t with
      | Some c when is_hex c ->
          ignore (char t);
          consume (n + 1)
      | _ -> ()
  in
  consume 0

let read_escape_hex t =
  (* Consume up to 6 hex digits *)
  let start = t.pos in
  read_escape_consume_hex_digits t;
  let hex = String.sub t.input start (t.pos - start) in
  (* Optional whitespace after hex escape consumes one space *)
  ignore (consume_if ' ' t);
  let cp = int_of_string_opt ("0x" ^ hex) |> Option.value ~default:0x3F in
  utf8_of_codepoint cp

let read_escape t =
  (* Assumes the leading backslash has already been consumed. *)
  match peek t with
  | Some c when is_hex c -> read_escape_hex t
  | Some c ->
      (* Simple escape of a single character within an identifier: return the
         unescaped character so escapes normalize to their intended
         codepoint. *)
      ignore (char t);
      String.make 1 c
  | None -> err_eof t

let ident_read_first_char t chars =
  match peek t with
  | Some '\\' ->
      ignore (char t);
      let escaped = read_escape t in
      String.iter (fun c -> chars := (c, true) :: !chars) escaped
  | Some '-' ->
      (* Use shared logic for hyphen validation *)
      if hyphen_can_start_ident t then (
        chars := ('-', false) :: !chars;
        ignore (char t))
      else err_invalid t "identifier cannot start with dash followed by digit"
  | Some c when is_ident_start c ->
      chars := (c, false) :: !chars;
      ignore (char t)
  | _ -> err_expected t "identifier"

let ident_track_bracket bracket_depth c =
  if c = '[' then incr bracket_depth
  else if c = ']' then bracket_depth := max 0 (!bracket_depth - 1)

let ident_handle_escape t chars bracket_depth =
  ignore (char t);
  let escaped = read_escape t in
  String.iter
    (fun c ->
      ident_track_bracket bracket_depth c;
      chars := (c, true) :: !chars)
    escaped

let ident_build_result ~keep_case chars =
  let buf = Buffer.create 16 in
  List.iter
    (fun (c, is_escaped) ->
      let c' = if keep_case || is_escaped then c else Char.lowercase_ascii c in
      Buffer.add_char buf c')
    (List.rev chars);
  Buffer.contents buf

let ident ?(keep_case = false) t =
  if t.pos >= t.len then err_expected t "identifier";
  let chars = ref [] in
  (* Track escaped chars separately - they keep their case *)
  (* First char: ident-start or escape *)
  ident_read_first_char t chars;
  (* Rest: ident-char or escape sequences. bracket_depth tracks escaped brackets
     \[...\] so we accept any character inside them (Tailwind bracket notation
     like \[&_p\] contains characters not valid in CSS identifiers). *)
  let bracket_depth = ref 0 in
  let rec loop () =
    match peek t with
    | Some '\\' ->
        ident_handle_escape t chars bracket_depth;
        loop ()
    | Some c when is_ident_char c || !bracket_depth > 0 ->
        chars := (c, false) :: !chars;
        ignore (char t);
        loop ()
    | _ -> ()
  in
  loop ();
  (* Build result, only lowercasing non-escaped chars *)
  let result = ident_build_result ~keep_case !chars in
  (* CSS spec: Invalid identifier patterns *)
  if result = "-" then err_invalid t "CSS identifier cannot be single dash '-'"
  else result

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let validate_unicode_code t hex =
  try
    let code = int_of_string ("0x" ^ hex) in
    if code > 0x10FFFF then err_invalid t "unicode escape out of range"
  with
  | Invalid_argument _ -> err_invalid t "invalid unicode escape"
  | Failure _ -> err_invalid t "invalid unicode escape"

let read_unicode_escape t buf =
  (* Unicode escape: read up to 6 hex digits *)
  let hex_buf = Buffer.create 6 in
  let rec read_hex count =
    if count >= 6 then ()
    else
      match peek t with
      | Some h when is_hex h ->
          skip t;
          Buffer.add_char hex_buf h;
          read_hex (count + 1)
      | _ -> ()
  in
  read_hex 0;
  let hex = Buffer.contents hex_buf in
  if String.length hex = 0 then err_invalid t "empty unicode escape";
  validate_unicode_code t hex;
  Buffer.add_char buf '\\';
  Buffer.add_string buf hex;
  (* Optional whitespace after unicode escape *)
  match peek t with
  | Some (' ' | '\t' | '\n' | '\r') -> skip t
  | _ -> ()

let read_single_char_escape t buf c =
  (* Single-character escape: per CSS, any character can be escaped. Be liberal
     and accept all simple escapes by consuming the next character literally. *)
  skip t;
  Buffer.add_char buf c

let string ?(trim = false) t =
  let quote = char t in
  if quote <> '"' && quote <> '\'' then err_expected t "string quote";
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek t with
    | None -> err t "unclosed string"
    | Some '\\' -> (
        skip t;
        match peek t with
        | None -> err t "incomplete escape sequence"
        | Some c when is_hex c ->
            read_unicode_escape t buf;
            loop ()
        | Some c ->
            read_single_char_escape t buf c;
            loop ())
    | Some c when c = quote ->
        skip t;
        let result = Buffer.contents buf in
        if trim then String.trim result else result
    | Some c ->
        skip t;
        Buffer.add_char buf c;
        loop ()
  in
  loop ()

let number_read_decimal t whole =
  if peek t = Some '.' then (
    skip t;
    let frac = while_ t is_digit in
    if String.length whole = 0 && String.length frac = 0 then
      err_invalid_number t;
    "." ^ frac)
  else ""

let number_read_exp_sign t =
  if peek t = Some '+' then (
    skip t;
    "+")
  else if peek t = Some '-' then (
    skip t;
    "-")
  else ""

let number_consume_exponent t =
  skip t;
  let exp_sign = number_read_exp_sign t in
  let exp_digits = while_ t is_digit in
  if String.length exp_digits = 0 then err_invalid_number t;
  "e" ^ exp_sign ^ exp_digits

let number_read_exponent t =
  (* Handle scientific notation: e or E followed by optional sign and digits.
     Only treat as exponent if followed by digit or sign+digit. *)
  match peek t with
  | Some ('e' | 'E') -> (
      let next_pos = t.pos + 1 in
      if next_pos >= String.length t.input then ""
      else
        match t.input.[next_pos] with
        | '0' .. '9' | '+' | '-' -> number_consume_exponent t
        | _ -> "" (* Not scientific notation, could be a unit like 'em' *))
  | _ -> ""

let number_parse t num_str =
  match float_of_string_opt num_str with
  | Some v -> v
  | None -> err_invalid t ("invalid number: " ^ num_str)

let number ?(allow_negative = true) t =
  let negative = peek t = Some '-' in
  let sign = peek t = Some '-' || peek t = Some '+' in
  if sign then skip t;
  let whole = while_ t is_digit in
  let decimal = number_read_decimal t whole in
  let exponent = number_read_exponent t in
  let num_str = whole ^ decimal ^ exponent in
  if String.length num_str = 0 || num_str = "." then err_invalid_number t;
  let value = number_parse t num_str in
  let result = if negative then -.value else value in
  if (not allow_negative) && result < 0.0 then
    err_invalid t "negative values not allowed"
  else result

let int t = int_of_float (number t)

let hex t =
  let buf = Buffer.create 6 in
  let rec loop () =
    match peek t with
    | Some c
      when (c >= '0' && c <= '9')
           || (c >= 'A' && c <= 'F')
           || (c >= 'a' && c <= 'f') ->
        Buffer.add_char buf c;
        skip t;
        loop ()
    | _ -> Buffer.contents buf
  in
  let hex_str = loop () in
  if String.length hex_str = 0 then err_invalid t "expected hex digits"
  else
    match int_of_string_opt ("0x" ^ hex_str) with
    | Some n -> n
    | None -> err_invalid t "invalid hex value"

(** {1 Whitespace} *)

(* Helper: skip comment content until finding the closing */ *)
let skip_comment_content t =
  let rec loop () =
    if looking_at t "*/" then skip_n t 2
    else
      match peek t with
      | None -> err_expected_but_eof t "*/ to close comment"
      | Some _ ->
          skip t;
          loop ()
  in
  loop ()

(* Skip whitespace and comments *)
let rec skip_ws t =
  (* Skip spaces *)
  let _ = while_ t (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') in
  (* Skip comments if present *)
  if not (looking_at t "/*") then ()
  else (
    skip_n t 2;
    skip_comment_content t;
    skip_ws t)

let ws = skip_ws

(** Check if a character is whitespace *)
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** Check if a character is a token separator in CSS *)
let is_token_separator c =
  is_ws c || c = ';' || c = ')' || c = '}' || c = ',' || c = '!'

(** Read a non-whitespace token *)
let token t =
  skip_ws t;
  while_ t (fun c -> not (is_token_separator c))

(** {1 Backtracking} *)

let save t = t.saved <- t.pos :: t.saved

let restore t =
  match t.saved with
  | [] -> err t "no saved position to restore"
  | pos :: rest ->
      t.pos <- pos;
      t.saved <- rest

let commit t =
  match t.saved with
  | [] -> err t "no saved position to commit"
  | _ :: rest -> t.saved <- rest

(* Atomic parsing: consume on success, rollback on failure. Like Parsec's 'try':
   if parser succeeds, keep position advances; if it fails, restore to original
   position. *)
let atomic t f =
  let saved_count_before = List.length t.saved in
  save t;
  try
    let result = f () in
    commit t;
    (* Invariant: save stack should be same size as before *)
    assert (List.length t.saved = saved_count_before);
    result
  with exn ->
    restore t;
    (* Invariant: save stack should be same size as before *)
    assert (List.length t.saved = saved_count_before);
    raise exn

(* Lookahead: run [f t] and ALWAYS restore the position, regardless of success
   or failure. On success, returns [f]'s result without consuming input. On
   failure, re-raises after restoring. *)
let lookahead f t =
  let saved_count_before = List.length t.saved in
  save t;
  try
    let v = f t in
    restore t;
    assert (List.length t.saved = saved_count_before);
    v
  with exn ->
    restore t;
    assert (List.length t.saved = saved_count_before);
    raise exn

let option f t =
  match atomic t (fun () -> f t) with
  | value -> Some value
  | exception Parse_error _ -> None

let try_parse_err f t =
  match atomic t (fun () -> f t) with
  | value -> Ok value
  | exception Parse_error error -> Error error.message

let many_handle_no_progress acc =
  (* Parser succeeded but made no progress - abort to prevent infinite loop *)
  if acc = [] then ([], Some "parser made no progress")
  else (List.rev acc, Some "parser made no progress")

let many_handle_error t acc msg =
  (* If we haven't parsed anything yet, this might be a fatal error. But if
     we've already parsed some items, it's just the end of the sequence. *)
  if acc = [] then
    (* No items parsed - check if it's just empty input *)
    if is_done t then ([], None) else ([], Some msg)
  else if
    (* We parsed some items - only report error if not at end *)
    is_done t
  then (List.rev acc, None)
  else (List.rev acc, Some msg)

let many f t =
  let rec loop acc =
    ws t;
    (* Try to parse another item *)
    let pos_before = t.pos in
    match try_parse_err f t with
    | Ok item ->
        if t.pos = pos_before then many_handle_no_progress acc
        else loop (item :: acc)
    | Error msg -> many_handle_error t acc msg
  in
  loop []

let one_of_merge_got got_value error_got =
  match got_value with
  | None -> error_got (* Use the first 'got' value we encounter *)
  | some -> some (* Keep the existing got value *)

let one_of_fail t errors got_value =
  let msg =
    if errors = [] then "no parsers provided"
    else "expected one of: " ^ String.concat ", " (List.rev errors)
  in
  err ?got:got_value t msg

let one_of parsers t =
  let rec try_parsers parsers_list errors got_value parser_idx =
    match parsers_list with
    | [] -> one_of_fail t errors got_value
    | parser :: rest -> (
        try atomic t (fun () -> parser t)
        with Parse_error error ->
          let new_got = one_of_merge_got got_value error.got in
          try_parsers rest (error.message :: errors) new_got (parser_idx + 1))
  in
  try_parsers parsers [] None 1

let should_stop_reading t =
  match peek t with
  | None -> true (* End of input *)
  | Some _ -> looking_at t "!" || looking_at t ";"

let try_read_item parser items t =
  try
    let item = atomic t (fun () -> parser t) in
    items := item :: !items;
    true
  with Parse_error _ -> false

let take max_count parser t =
  if max_count < 1 then invalid_arg "take: max_count must be >= 1";

  ws t;
  (* Read the first item (required) *)
  let first_item = parser t in
  let items = ref [ first_item ] in

  (* Try to read additional items (up to max_count - 1 more) *)
  let rec read_more () =
    ws t;
    if should_stop_reading t then ()
    else if List.length !items >= max_count then ()
    else if try_read_item parser items t then read_more ()
  in

  read_more ();

  (* Now check if there are more valid items that would exceed max_count *)
  ws t;
  (if (not (should_stop_reading t)) && List.length !items >= max_count then
     match option (fun t -> lookahead parser t) t with
     | Some _ ->
         err t
           ("too many values (maximum " ^ string_of_int max_count ^ " allowed)")
     | None ->
         (* Not a valid item, that's fine *)
         ());

  (* Return items in correct order *)
  List.rev !items

(* New enhanced combinators *)

let css_value_handle_quoted_escape t acc =
  (* Backslash inside a quoted string: consume the next char literally. *)
  match peek t with
  | None -> ("\\" :: acc, false)
  | Some next_c ->
      skip t;
      (String.make 1 next_c :: "\\" :: acc, true)

let css_value_in_quote t acc c quote_char depth =
  skip t;
  if c = quote_char then `Continue (String.make 1 c :: acc, depth, false, '\000')
  else if c = '\\' then
    let acc', _ = css_value_handle_quoted_escape t acc in
    `Continue (acc', depth, true, quote_char)
  else `Continue (String.make 1 c :: acc, depth, true, quote_char)

let css_value ~stops t =
  let rec collect_until acc depth in_quote quote_char =
    match peek t with
    | None -> String.concat "" (List.rev acc)
    | Some c when in_quote -> (
        match css_value_in_quote t acc c quote_char depth with
        | `Continue (acc', d, iq, qc) -> collect_until acc' d iq qc)
    | Some (('"' | '\'') as q) ->
        skip t;
        collect_until (String.make 1 q :: acc) depth true q
    | Some (('(' | '[' | '{') as c) ->
        skip t;
        collect_until (String.make 1 c :: acc) (depth + 1) in_quote quote_char
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        skip t;
        collect_until (String.make 1 c :: acc) (depth - 1) in_quote quote_char
    | Some c when depth = 0 && List.mem c stops ->
        String.concat "" (List.rev acc)
    | Some c ->
        skip t;
        collect_until (String.make 1 c :: acc) depth in_quote quote_char
  in
  String.trim (collect_until [] 0 false '\000')

let enum_impl ?default label mapping t =
  (* Try to match enum value first using option combinator *)
  ws t;
  let matched =
    if would_start_identifier t then
      (* Try to parse and match an identifier *)
      option
        (fun t ->
          let value = ident t in
          match
            List.find_opt
              (fun (k, _) -> String.lowercase_ascii k = value)
              mapping
          with
          | Some (_, result) -> result
          | None -> err t "not in enum")
        t
    else None
  in
  match matched with
  | Some result -> result
  | None -> (
      (* No match, try default if available *)
      match default with
      | Some f -> f t
      | None ->
          if would_start_identifier t then
            let value = ident t in
            let options = List.map fst mapping |> String.concat ", " in
            err t (label ^ ": expected one of: " ^ options ^ ", got: " ^ value)
          else err t (label ^ ": expected " ^ label))

(** {1 Structured Parsing} *)

let between open_c close_c f t =
  expect open_c t;
  ws t;
  let result = f t in
  ws t;
  expect close_c t;
  result

let parens f t = between '(' ')' f t
let braces f t = between '{' '}' f t

(* Helpers for reading function calls and simple combinators *)
let comma t =
  ws t;
  expect ',' t;
  ws t

let slash t =
  ws t;
  expect '/' t;
  ws t

let comma_opt t =
  ws t;
  if peek t = Some ',' then (
    comma t;
    true)
  else false

let slash_opt t =
  ws t;
  if peek t = Some '/' then (
    slash t;
    true)
  else false

let pair ?(sep = fun (_ : t) -> ()) p1 p2 t =
  atomic t (fun () ->
      let a = p1 t in
      sep t;
      let b = p2 t in
      (a, b))

let triple ?(sep = fun (_ : t) -> ()) p1 p2 p3 t =
  atomic t (fun () ->
      let a = p1 t in
      sep t;
      let b = p2 t in
      sep t;
      let c = p3 t in
      (a, b, c))

let list_impl_check_at_least t item at_least items =
  if List.length items >= at_least then ()
  else if List.length items = 0 && at_least > 0 then
    (* If we got no items and at_least:1 was specified, try to parse one more
       time to get a better error message that includes the nested context. This
       will fail and propagate the error with full context. *)
    let _ = item t in
    err t ("expected at least " ^ string_of_int at_least ^ " item(s)")
  else err t ("expected at least " ^ string_of_int at_least ^ " item(s)")

let list_impl_check_at_most t at_most items =
  match at_most with
  | Some n when List.length items > n ->
      err t ("too many values (maximum " ^ string_of_int n ^ " allowed)")
  | _ -> ()

let list_impl_continue t sep acc =
  if option sep t = Some () then `Continue acc else `Done (List.rev acc)

let list_impl_step t sep item acc =
  let pos_before = t.pos in
  match option item t with
  | None -> `Done (List.rev acc)
  | Some _ when t.pos = pos_before -> err t "parser made no progress in list"
  | Some parsed_item -> list_impl_continue t sep (parsed_item :: acc)

let list_impl ?(sep = fun (_ : t) -> ()) ?(at_least = 0) ?at_most item t =
  let rec loop acc =
    match list_impl_step t sep item acc with
    | `Continue acc -> loop acc
    | `Done items -> items
  in
  let items = loop [] in
  list_impl_check_at_least t item at_least items;
  list_impl_check_at_most t at_most items;
  items

(* Helpers for reading function calls *)
let call name t p =
  let got = ident t in
  if got <> String.lowercase_ascii name then
    err t ("expected function '" ^ name ^ "', got '" ^ got ^ "'")
  else
    parens
      (fun t ->
        ws t;
        let r = p t in
        ws t;
        r)
      t

let quoted_or_unquoted_url t =
  ws t;
  let url_content =
    match peek t with
    | Some ('"' | '\'') -> string t
    | _ -> String.trim (until t ')')
  in
  (* Per CSS spec, empty URLs are invalid *)
  if url_content = "" then err t "empty URL is not allowed" else url_content

let url t = call "url" t quoted_or_unquoted_url

let enum_calls_impl ?default cases t =
  ws t;
  if not (would_start_identifier t) then
    match default with
    | Some f -> f t
    | None ->
        let options = List.map fst cases |> String.concat ", " in
        err t ("expected one of functions: " ^ options)
  else
    let got = lookahead ident t in
    match
      List.find_opt (fun (n, _) -> String.lowercase_ascii n = got) cases
    with
    | None -> (
        match default with
        | Some f -> f t
        | None ->
            let options = List.map fst cases |> String.concat ", " in
            err t ("expected one of functions: " ^ options))
    | Some (_, p) -> p t

let enum_or_calls_impl ?default label idents ~calls t =
  let find_assoc_ci name lst =
    List.find_map
      (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None)
      lst
  in
  let consume_ident_result name result =
    let consumed_name = ident t in
    assert (consumed_name = name);
    result
  in
  let run_default options =
    match default with Some f -> f t | None -> err t options
  in
  let read_ident_call name =
    match find_assoc_ci name calls with
    | Some p -> p t
    | None -> (
        match find_assoc_ci name idents with
        | Some result -> consume_ident_result name result
        | None ->
            let options = List.map fst calls |> String.concat ", " in
            run_default ("expected one of functions: " ^ options))
  in
  let read_ident_value name =
    match find_assoc_ci name idents with
    | Some result -> consume_ident_result name result
    | None ->
        let options = List.map fst idents |> String.concat ", " in
        run_default (label ^ ": expected one of: " ^ options ^ ", got: " ^ name)
  in
  atomic t (fun () ->
      ws t;
      if not (would_start_identifier t) then
        run_default (label ^ ": expected " ^ label)
      else
        let name, has_paren =
          lookahead
            (fun t ->
              let n = ident t in
              (n, peek t = Some '('))
            t
        in
        if has_paren then read_ident_call name else read_ident_value name)

let fold_many_handle_no_progress t acc consumed =
  (* Parser succeeded but made no progress - abort to prevent infinite loop *)
  if consumed then (acc, Some "parser made no progress")
  else err_invalid t "parser made no progress (potential infinite loop)"

let fold_many_handle_error t acc init consumed msg =
  match (consumed, is_done t) with
  | true, true -> (acc, None)
  | true, false -> (acc, Some msg)
  | false, true -> (init, None)
  | false, false -> err_invalid t "value"

let fold_many_step parser ~init ~f t acc consumed =
  ws t;
  let pos_before = t.pos in
  match try_parse_err parser t with
  | Ok _ when t.pos = pos_before ->
      `Done (fold_many_handle_no_progress t acc consumed)
  | Ok item -> `Continue (f acc item, true)
  | Error msg -> `Done (fold_many_handle_error t acc init consumed msg)

let fold_many parser ~init ~f t =
  with_context t "fold_many" @@ fun () ->
  let rec loop acc consumed =
    match fold_many_step parser ~init ~f t acc consumed with
    | `Continue (acc, consumed) -> loop acc consumed
    | `Done result -> result
  in
  loop init false

let number_with_unit t =
  ws t;
  let n = number t in
  let u =
    if looking_at t "%" then (
      ignore (char t);
      Some "%")
    else option ident t
  in
  (n, u)

let unit expected t =
  ws t;
  let n = number t in
  let u = ident t in
  if String.equal u expected then n
  else err t ("expected unit '" ^ expected ^ "', got '" ^ u ^ "'")

let pct ?(clamp = false) t =
  ws t;
  let n = number t in
  expect '%' t;
  (* CSS percentages are NOT clamped by default. Properties like width, margin,
     font-size can exceed 100%. Only specific contexts like alpha values require
     clamping. Ref: https://www.w3.org/TR/css-values-4/#percentages *)
  if clamp then max 0. (min 100. n) else n

let bool t =
  ws t;
  let id = ident t in
  match id with
  | "true" -> true
  | "false" -> false
  | _ ->
      err_invalid t
        ("invalid boolean value: " ^ id ^ " (expected true or false)")

(** {1 Context Wrappers} *)

let enum ?default label mapping t =
  with_context t ("enum:" ^ label) @@ fun () ->
  (* Make enum atomic: if it fails, restore position *)
  atomic t (fun () -> enum_impl ?default label mapping t)

let list ?sep ?at_least ?at_most item t =
  with_context t "list" @@ fun () -> list_impl ?sep ?at_least ?at_most item t

let enum_calls ?default cases t =
  with_context t "enum_calls" @@ fun () -> enum_calls_impl ?default cases t

let enum_or_calls ?default label idents ~calls t =
  with_context t ("enum_or_calls:" ^ label) @@ fun () ->
  enum_or_calls_impl ?default label idents ~calls t
