(** Simple CSS parser implementation. *)

exception Parse_error of string

(** Error helpers *)
let err msg = raise (Parse_error msg)

let err_eof () = err "unexpected end of input"
let err_expected what = err ("expected " ^ what)
let err_invalid_number () = err "invalid number"
let err_invalid what = err ("invalid " ^ what)

type t = {
  input : string;
  len : int;
  mutable pos : int;
  mutable saved : int list; (* Stack of saved positions *)
}

(** {1 Creation} *)

let of_string input = { input; len = String.length input; pos = 0; saved = [] }
let is_done t = t.pos >= t.len

(** {1 Looking Ahead} *)

let peek t = if t.pos >= t.len then None else Some t.input.[t.pos]

let peek_string t n =
  let n = min n (t.len - t.pos) in
  String.sub t.input t.pos n

let looking_at t s =
  let slen = String.length s in
  t.pos + slen <= t.len && String.sub t.input t.pos slen = s

(** {1 Reading Characters} *)

let skip t =
  if t.pos >= t.len then err_eof ();
  t.pos <- t.pos + 1

let skip_n t n =
  if t.pos + n > t.len then err_eof ();
  t.pos <- t.pos + n

let char t =
  if t.pos >= t.len then err_eof ();
  let c = t.input.[t.pos] in
  t.pos <- t.pos + 1;
  c

let err_expect expected actual =
  err
    ("Expected '" ^ String.make 1 expected ^ "' but got '"
   ^ String.make 1 actual ^ "'")

let expect t c =
  let actual = char t in
  if actual <> c then err_expect c actual

let err_expect_string expected = err ("expected \"" ^ expected ^ "\"")

let expect_string t s =
  let slen = String.length s in
  if not (looking_at t s) then err_expect_string s;
  skip_n t slen

(** Pretty-printer for parser state. *)
let pp t =
  let remaining = max 0 (t.len - t.pos) in
  if remaining = 0 then "<EOF>"
  else
    let preview_len = min 20 remaining in
    String.sub t.input t.pos preview_len

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

let until_string t s =
  let start = t.pos in
  while t.pos < t.len && not (looking_at t s) do
    t.pos <- t.pos + 1
  done;
  String.sub t.input start (t.pos - start)

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'

let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')

let ident t =
  if t.pos >= t.len || not (is_ident_start t.input.[t.pos]) then
    err_expected "identifier";
  while_ t is_ident_char

let string t =
  let quote = char t in
  if quote <> '"' && quote <> '\'' then err_expected "string quote";
  let rec loop acc =
    match char t with
    | '\\' ->
        let escaped = char t in
        loop (escaped :: '\\' :: acc)
    | c when c = quote -> String.concat "" (List.rev_map (String.make 1) acc)
    | c -> loop (c :: acc)
  in
  loop []

let is_digit c = c >= '0' && c <= '9'

let number t =
  let negative = peek t = Some '-' in
  let sign = peek t = Some '-' || peek t = Some '+' in
  if sign then skip t;

  let whole = while_ t is_digit in
  let decimal =
    if peek t = Some '.' then (
      skip t;
      let frac = while_ t is_digit in
      if String.length whole = 0 && String.length frac = 0 then
        err_invalid_number ();
      "." ^ frac)
    else ""
  in

  let num_str = whole ^ decimal in
  if String.length num_str = 0 || num_str = "." then err_invalid_number ();

  let value = float_of_string num_str in
  if negative then -.value else value

let int t = int_of_float (number t)

(** {1 Whitespace} *)

let rec skip_ws t =
  (* Skip spaces *)
  let _ = while_ t (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r') in
  (* Skip comments *)
  if looking_at t "/*" then (
    skip_n t 2;
    while (not (looking_at t "*/")) && not (is_done t) do
      skip t
    done;
    if not (is_done t) then skip_n t 2;
    skip_ws t)

let ws = skip_ws

(** {1 Backtracking} *)

let save t = t.saved <- t.pos :: t.saved

let restore t =
  match t.saved with
  | [] -> err "no saved position to restore"
  | pos :: rest ->
      t.pos <- pos;
      t.saved <- rest

let commit t =
  match t.saved with
  | [] -> err "no saved position to commit"
  | _ :: rest -> t.saved <- rest

let try_parse f t =
  save t;
  try
    let result = f t in
    commit t;
    Some result
  with Parse_error _ ->
    restore t;
    None

(** {1 Structured Parsing} *)

let between t open_c close_c f =
  expect t open_c;
  ws t;
  let result = f t in
  ws t;
  expect t close_c;
  result

let parens t f = between t '(' ')' f
let brackets t f = between t '[' ']' f
let braces t f = between t '{' '}' f

let separated t parse_item parse_sep =
  let rec loop acc =
    match try_parse parse_item t with
    | None -> List.rev acc
    | Some item ->
        let acc = item :: acc in
        if try_parse parse_sep t = Some () then loop acc else List.rev acc
  in
  loop []

(** {1 CSS-Specific Helpers} *)

let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let hex_color t =
  let hex = while_ t is_hex in
  if String.length hex <> 3 && String.length hex <> 6 then
    err_invalid "hex color (must be 3 or 6 digits)";
  hex

let percentage t =
  let n = number t in
  expect t '%';
  n

let dimension t =
  let n = number t in
  let unit =
    while_ t (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
  in
  (n, unit)

let angle t =
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  (n, unit)

let duration t =
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  (n, unit)

let color_keywords =
  [
    "red";
    "blue";
    "green";
    "white";
    "black";
    "yellow";
    "cyan";
    "magenta";
    "gray";
    "grey";
    "orange";
    "purple";
    "pink";
    "silver";
    "maroon";
    "fuchsia";
    "lime";
    "olive";
    "navy";
    "teal";
    "aqua";
    "transparent";
    "currentcolor";
    "inherit";
    "initial";
    "unset";
  ]

let color_keyword t =
  let start_pos = t.pos in
  let word =
    while_ t (function 'a' .. 'z' | 'A' .. 'Z' | '-' -> true | _ -> false)
  in
  let word_lower = String.lowercase_ascii word in
  if List.mem word_lower color_keywords then Some word_lower
  else (
    t.pos <- start_pos;
    None)

let rgb_function t =
  try_parse
    (fun t ->
      let func_name = ident t in
      if func_name <> "rgb" && func_name <> "rgba" then err "not rgb function";
      expect t '(';
      ws t;
      let r = int t in
      ws t;
      expect t ',';
      ws t;
      let g = int t in
      ws t;
      expect t ',';
      ws t;
      let b = int t in
      let alpha =
        try_parse
          (fun t ->
            ws t;
            expect t ',';
            ws t;
            number t)
          t
      in
      ws t;
      expect t ')';
      (r, g, b, alpha))
    t
