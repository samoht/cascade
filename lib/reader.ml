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
  let open Buffer in
  add_string ctx.buf "<reader pos=";
  add_string ctx.buf (string_of_int t.pos);
  add_string ctx.buf ">"

(** {1 Creation} *)

let of_string input =
  { input; len = String.length input; pos = 0; saved = []; call_stack = [] }

let is_done t = t.pos >= t.len

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

let peek2 t =
  let n = min 2 (t.len - t.pos) in
  String.sub t.input t.pos n

let peek_string t n =
  let n = min n (t.len - t.pos) in
  String.sub t.input t.pos n

let looking_at t s =
  let slen = String.length s in
  t.pos + slen <= t.len && String.sub t.input t.pos slen = s

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
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

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

let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

(* Encode a Unicode codepoint as UTF-8 *)
let utf8_of_codepoint cp =
  if cp < 0 then "?"
  else if cp <= 0x7F then String.make 1 (Char.chr cp)
  else if cp <= 0x7FF then
    let b1 = 0xC0 lor (cp lsr 6) in
    let b2 = 0x80 lor (cp land 0x3F) in
    Bytes.init 2 (function 0 -> Char.chr b1 | _ -> Char.chr b2)
    |> Bytes.to_string
  else if cp <= 0xFFFF then
    let b1 = 0xE0 lor (cp lsr 12) in
    let b2 = 0x80 lor ((cp lsr 6) land 0x3F) in
    let b3 = 0x80 lor (cp land 0x3F) in
    Bytes.init 3 (function
      | 0 -> Char.chr b1
      | 1 -> Char.chr b2
      | _ -> Char.chr b3)
    |> Bytes.to_string
  else if cp <= 0x10FFFF then
    let b1 = 0xF0 lor (cp lsr 18) in
    let b2 = 0x80 lor ((cp lsr 12) land 0x3F) in
    let b3 = 0x80 lor ((cp lsr 6) land 0x3F) in
    let b4 = 0x80 lor (cp land 0x3F) in
    Bytes.init 4 (function
      | 0 -> Char.chr b1
      | 1 -> Char.chr b2
      | 2 -> Char.chr b3
      | _ -> Char.chr b4)
    |> Bytes.to_string
  else "?"

let read_escape t =
  (* Assumes the leading backslash has already been consumed. *)
  match peek t with
  | Some c when is_hex c ->
      (* Consume up to 6 hex digits *)
      let start = t.pos in
      let rec consume n =
        if n = 6 then ()
        else
          match peek t with
          | Some c when is_hex c ->
              ignore (char t);
              consume (n + 1)
          | _ -> ()
      in
      consume 0;
      let hex = String.sub t.input start (t.pos - start) in
      (* Optional whitespace after hex escape consumes one space *)
      ignore (consume_if ' ' t);
      let cp = int_of_string_opt ("0x" ^ hex) |> Option.value ~default:0x3F in
      utf8_of_codepoint cp
  | Some c ->
      (* Simple escape of a single character within an identifier: return the
         unescaped character so escapes normalize to their intended
         codepoint. *)
      ignore (char t);
      String.make 1 c
  | None -> err_eof t

let ident ?(keep_case = false) t =
  if t.pos >= t.len then err_expected t "identifier";
  let chars = ref [] in
  (* Track escaped chars separately - they keep their case *)
  (* First char: ident-start or escape *)
  (match peek t with
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
  | _ -> err_expected t "identifier");
  (* Rest: ident-char or escape sequences *)
  let rec loop () =
    match peek t with
    | Some '\\' ->
        ignore (char t);
        let escaped = read_escape t in
        String.iter (fun c -> chars := (c, true) :: !chars) escaped;
        loop ()
    | Some c when is_ident_char c ->
        chars := (c, false) :: !chars;
        ignore (char t);
        loop ()
    | _ -> ()
  in
  loop ();
  (* Build result, only lowercasing non-escaped chars *)
  let buf = Buffer.create 16 in
  List.iter
    (fun (c, is_escaped) ->
      let c' = if keep_case || is_escaped then c else Char.lowercase_ascii c in
      Buffer.add_char buf c')
    (List.rev !chars);
  let result = Buffer.contents buf in
  (* CSS spec: Invalid identifier patterns *)
  if result = "--" then err_invalid t "CSS identifier cannot be '--' alone"
  else if result = "-" then
    err_invalid t "CSS identifier cannot be single dash '-'"
  else if String.length result >= 3 && String.sub result 0 3 = "---" then
    err_invalid t "CSS identifier cannot start with triple dash '---'"
  else result

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let read_unicode_escape t buf =
  (* Unicode escape: read up to 6 hex digits *)
  let hex_buf = Buffer.create 6 in
  let rec read_hex count =
    if count < 6 then
      match peek t with
      | Some h when is_hex h ->
          skip t;
          Buffer.add_char hex_buf h;
          read_hex (count + 1)
      | _ -> ()
    else ()
  in
  read_hex 0;
  let hex = Buffer.contents hex_buf in
  (if String.length hex = 0 then err_invalid t "empty unicode escape"
   else
     (* Validate that it forms valid unicode *)
     try
       let code = int_of_string ("0x" ^ hex) in
       if code > 0x10FFFF then err_invalid t "unicode escape out of range";
       (* Add the unicode character as a backslash followed by hex *)
       Buffer.add_char buf '\\';
       Buffer.add_string buf hex
     with
     | Invalid_argument _ -> err_invalid t "invalid unicode escape"
     | Failure _ -> err_invalid t "invalid unicode escape");
  (* Optional whitespace after unicode escape *)
  match peek t with
  | Some (' ' | '\t' | '\n' | '\r') -> skip t
  | _ -> ()

let read_single_char_escape t buf c =
  (* Single character escape or invalid *)
  skip t;
  if
    (not (is_alpha c || is_digit c))
    && c <> '"' && c <> '\'' && c <> '\\' && c <> '/' && c <> ' ' && c <> '\n'
    && c <> '\r' && c <> '\t'
  then
    (* For CSS, only certain characters can be escaped directly *)
    err_invalid t ("invalid escape sequence '" ^ "\\" ^ String.make 1 c ^ "'");
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

let number ?(allow_negative = true) t =
  let negative = peek t = Some '-' in
  let sign = peek t = Some '-' || peek t = Some '+' in
  if sign then skip t;

  let whole = while_ t is_digit in
  let decimal =
    if peek t = Some '.' then (
      skip t;
      let frac = while_ t is_digit in
      if String.length whole = 0 && String.length frac = 0 then
        err_invalid_number t;
      "." ^ frac)
    else ""
  in

  (* Handle scientific notation: e or E followed by optional sign and digits *)
  (* Only treat as exponent if followed by digit or sign+digit *)
  let exponent =
    match peek t with
    | Some ('e' | 'E') ->
        (* Look ahead to see if this is scientific notation *)
        let next_pos = t.pos + 1 in
        if next_pos < String.length t.input then
          let next_char = t.input.[next_pos] in
          match next_char with
          | '0' .. '9' | '+' | '-' ->
              skip t;
              let exp_sign =
                if peek t = Some '+' then (
                  skip t;
                  "+")
                else if peek t = Some '-' then (
                  skip t;
                  "-")
                else ""
              in
              let exp_digits = while_ t is_digit in
              if String.length exp_digits = 0 then err_invalid_number t;
              "e" ^ exp_sign ^ exp_digits
          | _ -> "" (* Not scientific notation, could be a unit like 'em' *)
        else ""
    | _ -> ""
  in

  let num_str = whole ^ decimal ^ exponent in
  if String.length num_str = 0 || num_str = "." then err_invalid_number t;

  let value =
    match float_of_string_opt num_str with
    | Some v -> v
    | None -> err_invalid t ("invalid number: " ^ num_str)
  in
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
  try Some (atomic t (fun () -> f t)) with Parse_error _ -> None

let try_parse_err f t =
  try Ok (atomic t (fun () -> f t))
  with Parse_error error -> Error error.message

let many f t =
  let rec loop acc =
    ws t;
    (* Try to parse another item *)
    let pos_before = t.pos in
    match try_parse_err f t with
    | Ok item ->
        if t.pos = pos_before then
          (* Parser succeeded but made no progress - abort to prevent infinite
             loop *)
          if acc = [] then ([], Some "parser made no progress")
          else (List.rev acc, Some "parser made no progress")
        else loop (item :: acc)
    | Error msg ->
        (* If we haven't parsed anything yet, this might be a fatal error *)
        (* But if we've already parsed some items, it's just the end of the sequence *)
        if acc = [] then
          (* No items parsed - check if it's just empty input *)
          if is_done t then ([], None) else ([], Some msg)
        else if
          (* We parsed some items - only report error if not at end *)
          is_done t
        then (List.rev acc, None)
        else (List.rev acc, Some msg)
  in
  loop []

let one_of parsers t =
  let rec try_parsers parsers_list errors got_value parser_idx =
    match parsers_list with
    | [] ->
        let msg =
          if errors = [] then "no parsers provided"
          else "expected one of: " ^ String.concat ", " (List.rev errors)
        in
        err ?got:got_value t msg
    | parser :: rest -> (
        try atomic t (fun () -> parser t)
        with Parse_error error ->
          let new_got =
            match got_value with
            | None -> error.got (* Use the first 'got' value we encounter *)
            | some -> some (* Keep the existing got value *)
          in
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

let css_value ~stops t =
  let rec parse_until acc depth in_quote quote_char =
    match peek t with
    | None -> String.concat "" (List.rev acc)
    | Some c when in_quote ->
        skip t;
        if c = quote_char then
          parse_until (String.make 1 c :: acc) depth false '\000'
        else if c = '\\' then (
          (* Handle escape sequence *)
          match peek t with
          | None ->
              parse_until (String.make 1 c :: acc) depth in_quote quote_char
          | Some next_c ->
              skip t;
              parse_until
                (String.make 1 next_c :: String.make 1 c :: acc)
                depth in_quote quote_char)
        else parse_until (String.make 1 c :: acc) depth in_quote quote_char
    | Some (('"' | '\'') as q) ->
        skip t;
        parse_until (String.make 1 q :: acc) depth true q
    | Some (('(' | '[' | '{') as c) ->
        skip t;
        parse_until (String.make 1 c :: acc) (depth + 1) in_quote quote_char
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        skip t;
        parse_until (String.make 1 c :: acc) (depth - 1) in_quote quote_char
    | Some c when depth = 0 && List.mem c stops ->
        String.concat "" (List.rev acc)
    | Some c ->
        skip t;
        parse_until (String.make 1 c :: acc) depth in_quote quote_char
  in
  String.trim (parse_until [] 0 false '\000')

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

let list_impl ?(sep = fun (_ : t) -> ()) ?(at_least = 0) ?at_most item t =
  let rec loop acc =
    let pos_before = t.pos in
    match option item t with
    | None -> List.rev acc
    | Some parsed_item ->
        if t.pos = pos_before then
          (* Parser succeeded but made no progress - abort to prevent infinite
             loop *)
          err t "parser made no progress in list"
        else
          let acc = parsed_item :: acc in
          if option sep t = Some () then loop acc else List.rev acc
  in
  let items = loop [] in
  if List.length items < at_least then
    (* If we got no items and at_least:1 was specified, try to parse one more
       time to get a better error message that includes the nested context *)
    if List.length items = 0 && at_least > 0 then
      (* This will fail and propagate the error with full context *)
      let _ = item t in
      err t ("expected at least " ^ string_of_int at_least ^ " item(s)")
    else err t ("expected at least " ^ string_of_int at_least ^ " item(s)");
  (match at_most with
  | Some n when List.length items > n ->
      err t ("too many values (maximum " ^ string_of_int n ^ " allowed)")
  | _ -> ());
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
  (* Atomic and simple: name( means call (no whitespace allowed); name means
     ident. *)
  let find_assoc_ci name lst =
    List.find_map
      (fun (k, v) -> if String.lowercase_ascii k = name then Some v else None)
      lst
  in
  let err_expected_ident name =
    let options = List.map fst idents |> String.concat ", " in
    err t (label ^ ": expected one of: " ^ options ^ ", got: " ^ name)
  in
  atomic t (fun () ->
      ws t;
      if not (would_start_identifier t) then
        match default with
        | Some f -> f t
        | None -> err t (label ^ ": expected " ^ label)
      else
        (* Peek identifier and '(' without consuming input *)
        let name, has_paren =
          lookahead
            (fun t ->
              let n = ident t in
              (n, peek t = Some '('))
            t
        in
        if has_paren then
          (* Has parenthesis - try as function call *)
          match find_assoc_ci name calls with
          | Some p -> p t (* Parser consumes identifier and parens *)
          | None -> (
              (* Not a valid call: do NOT consume; fall back appropriately *)
              match find_assoc_ci name idents with
              | Some v ->
                  (* Consume the ident now that we know it's a plain ident *)
                  let consumed_name = ident t in
                  assert (consumed_name = name);
                  v
              | None -> (
                  match default with
                  | Some f -> f t
                  | None ->
                      let options = List.map fst calls |> String.concat ", " in
                      err t ("expected one of functions: " ^ options)))
        else
          (* No parenthesis: only consume if it matches an ident; else
             default *)
          match find_assoc_ci name idents with
          | Some v ->
              let consumed_name = ident t in
              assert (consumed_name = name);
              v
          | None -> (
              match default with
              | Some f -> f t
              | None -> err_expected_ident name))

let fold_many parser ~init ~f t =
  with_context t "fold_many" @@ fun () ->
  let rec loop acc consumed =
    ws t;
    let pos_before = t.pos in
    match try_parse_err parser t with
    | Ok item ->
        if t.pos = pos_before then
          (* Parser succeeded but made no progress - abort to prevent infinite
             loop *)
          if consumed then (acc, Some "parser made no progress")
          else err_invalid t "parser made no progress (potential infinite loop)"
        else loop (f acc item) true
    | Error msg ->
        if consumed then
          (* End of sequence, return what we have - only report error if not at
             end *)
          if is_done t then (acc, None) else (acc, Some msg)
        else if
          (* No items parsed at all - check if just empty input *)
          is_done t
        then (init, None)
        else
          (* Real error that should propagate *)
          err_invalid t "value"
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
