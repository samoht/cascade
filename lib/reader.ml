(** Simple CSS parser implementation. *)

type t = {
  input : string;
  len : int;
  mutable pos : int;
  mutable saved : int list; (* Stack of saved positions *)
}

exception Parse_error of string * t

(** Error helpers *)
let err t msg = raise (Parse_error (msg, t))

let err_eof t = err t "unexpected end of input"
let err_expected t what = err t ("expected " ^ what)
let err_invalid_number t = err t "invalid number"
let err_invalid t what = err t ("invalid " ^ what)

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

let expect t c =
  let actual_pos = t.pos in
  let actual = char t in
  if actual <> c then (
    (* Go back one position for better context display *)
    t.pos <- actual_pos;
    err t
      ("Expected '" ^ String.make 1 c ^ "' but got '" ^ String.make 1 actual
     ^ "'"))

let expect_string t s =
  let slen = String.length s in
  if not (looking_at t s) then err t ("expected \"" ^ s ^ "\"");
  skip_n t slen

(** Get context around current position. Returns (before, after) strings. *)
let context_string ?(window = 40) t =
  let context_start = max 0 (t.pos - window) in
  let context_end = min t.len (t.pos + window) in
  let before = String.sub t.input context_start (t.pos - context_start) in
  let after =
    if t.pos < t.len then String.sub t.input t.pos (context_end - t.pos) else ""
  in
  (before, after)

(** Get current position in input *)
let position t = t.pos

(** Get total length of input *)
let length t = t.len

(** Simple pretty-printer for parser state. *)
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
      (match peek t with Some ' ' -> skip t | _ -> ());
      let cp = int_of_string_opt ("0x" ^ hex) |> Option.value ~default:0x3F in
      utf8_of_codepoint cp
  | Some c ->
      (* Simple escape of a single character within an identifier: return the
         unescaped character so escapes normalize to their intended
         codepoint. *)
      ignore (char t);
      String.make 1 c
  | None -> err_eof t

let ident t =
  if t.pos >= t.len then err_expected t "identifier";
  let buf = Buffer.create 16 in
  (* First char: ident-start or escape *)
  (match peek t with
  | Some '\\' ->
      ignore (char t);
      Buffer.add_string buf (read_escape t)
  | Some c when is_ident_start c ->
      Buffer.add_char buf c;
      ignore (char t)
  | _ -> err_expected t "identifier");
  (* Rest: ident-char or escape sequences *)
  let rec loop () =
    match peek t with
    | Some '\\' ->
        ignore (char t);
        Buffer.add_string buf (read_escape t);
        loop ()
    | Some c when is_ident_char c ->
        Buffer.add_char buf c;
        ignore (char t);
        loop ()
    | _ -> ()
  in
  loop ();
  Buffer.contents buf

let string t =
  let quote = char t in
  if quote <> '"' && quote <> '\'' then err_expected t "string quote";
  let rec loop acc =
    match char t with
    | '\\' ->
        let escaped = char t in
        loop (escaped :: acc)
    | c when c = quote -> String.concat "" (List.rev_map (String.make 1) acc)
    | c -> loop (c :: acc)
  in
  loop []

let ident_lc t = String.lowercase_ascii (ident t)
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
        err_invalid_number t;
      "." ^ frac)
    else ""
  in

  let num_str = whole ^ decimal in
  if String.length num_str = 0 || num_str = "." then err_invalid_number t;

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

(* Helpers for reading function calls and simple combinators *)
let comma t =
  ws t;
  expect t ',';
  ws t

let slash t =
  ws t;
  expect t '/';
  ws t

let pair ?(sep = fun (_ : t) -> ()) p1 p2 t =
  let a = p1 t in
  sep t;
  let b = p2 t in
  (a, b)

let triple ?(sep = fun (_ : t) -> ()) p1 p2 p3 t =
  let a = p1 t in
  sep t;
  let b = p2 t in
  sep t;
  let c = p3 t in
  (a, b, c)

let list ?(sep = fun (_ : t) -> ()) item t = separated t item sep

(* Helpers for reading function calls *)
let call name p t =
  let got = ident t |> String.lowercase_ascii in
  if got <> String.lowercase_ascii name then
    err t ("expected function '" ^ name ^ "', got '" ^ got ^ "'")
  else
    parens t (fun t ->
        ws t;
        let r = p t in
        ws t;
        r)

let call_2 name p1 p2 t = call name (pair ~sep:comma p1 p2) t
let call_3 name p1 p2 p3 t = call name (triple ~sep:comma p1 p2 p3) t
let call_list name item t = call name (fun t -> list ~sep:comma item t) t

let url t =
  call "url"
    (fun t ->
      ws t;
      match peek t with
      | Some ('"' | '\'') -> string t
      | _ -> String.trim (until t ')'))
    t
