(** Simple CSS parser implementation. *)

type t = {
  input : string;
  len : int;
  mutable pos : int;
  mutable saved : int list; (* Stack of saved positions *)
}

exception Parse_error of string * string option * t

(** Error helpers *)
let err ?got t expected = raise (Parse_error (expected, got, t))

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

let expect c t =
  let actual_pos = t.pos in
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
let context_window ?(before = 30) ?(after = 30) t =
  let pos = t.pos in
  let start_pos = max 0 (pos - before) in
  let end_pos = min t.len (pos + after) in
  let before_text = String.sub t.input start_pos (pos - start_pos) in
  let after_text = String.sub t.input pos (end_pos - pos) in
  let marker_pos = String.length before_text in
  (before_text ^ after_text, marker_pos)

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
  Buffer.contents buf

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_hex_digit c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let string ?(trim = false) t =
  let quote = char t in
  if quote <> '"' && quote <> '\'' then err_expected t "string quote";
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek t with
    | None -> err t "unclosed string"
    | Some '\\' -> (
        skip t;
        (* skip backslash *)
        (* Handle CSS escape sequences *)
        match peek t with
        | None -> err t "incomplete escape sequence"
        | Some c when is_hex_digit c ->
            (* Unicode escape: read up to 6 hex digits *)
            let hex_buf = Buffer.create 6 in
            let rec read_hex count =
              if count < 6 then
                match peek t with
                | Some h when is_hex_digit h ->
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
                 if code > 0x10FFFF then
                   err_invalid t "unicode escape out of range";
                 (* Add the unicode character *)
                 Buffer.add_string buf (Printf.sprintf "\\%s" hex)
               with _ -> err_invalid t "invalid unicode escape");
            (* Optional whitespace after unicode escape *)
            (match peek t with
            | Some (' ' | '\t' | '\n' | '\r') -> skip t
            | _ -> ());
            loop ()
        | Some c ->
            (* Single character escape or invalid *)
            skip t;
            if
              (not (is_alpha c || is_digit c))
              && c <> '"' && c <> '\'' && c <> '\\' && c <> '/' && c <> ' '
              && c <> '\n' && c <> '\r' && c <> '\t'
            then
              (* For CSS, only certain characters can be escaped directly *)
              err_invalid t (Printf.sprintf "invalid escape sequence '\\%c'" c);
            Buffer.add_char buf c;
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

let option f t =
  save t;
  try
    let result = f t in
    commit t;
    Some result
  with
  | Parse_error _ ->
      restore t;
      None
  | _ ->
      restore t;
      None

let try_parse_err f t =
  save t;
  try
    let result = f t in
    commit t;
    Ok result
  with Parse_error (msg, _, _) ->
    restore t;
    Error msg

let many f t =
  let rec loop acc last_error =
    ws t;
    if is_done t then (List.rev acc, last_error)
    else
      match try_parse_err f t with
      | Ok item -> loop (item :: acc) None
      | Error msg ->
          (* If we haven't parsed anything yet, this is a fatal error *)
          if acc = [] then ([], Some msg) else (List.rev acc, Some msg)
  in
  loop [] None

let one_of parsers t =
  let rec try_parsers parsers_list errors got_value =
    match parsers_list with
    | [] ->
        let msg =
          if errors = [] then "no parsers provided"
          else "expected one of: " ^ String.concat ", " errors
        in
        raise (Parse_error (msg, got_value, t))
    | parser :: rest -> (
        save t;
        try
          let result = parser t in
          commit t;
          result
        with Parse_error (expected, got, _) ->
          restore t;
          let new_got =
            match got_value with
            | None -> got (* Use the first 'got' value we encounter *)
            | some -> some (* Keep the existing got value *)
          in
          try_parsers rest (expected :: errors) new_got)
  in
  try_parsers parsers [] None

let take max_count parser t =
  if max_count < 1 then invalid_arg "take: max_count must be >= 1";

  ws t;
  (* Read the first item (required) *)
  let first_item = parser t in
  let items = ref [ first_item ] in

  (* Try to read additional items (up to max_count - 1 more) *)
  let rec read_more () =
    ws t;
    (* Check if we've reached end of value or special tokens *)
    if is_done t || looking_at t "!" || looking_at t ";" then ()
    else if List.length !items >= max_count then ()
    else (
      (* Save position in case this isn't a valid item *)
      save t;
      try
        let item = parser t in
        commit t;
        items := item :: !items;
        read_more ()
      with Parse_error _ ->
        restore t;
        (* Not a valid item, stop reading *)
        ())
  in

  read_more ();

  (* Now check if there are more valid items that would exceed max_count *)
  ws t;
  (if not (is_done t || looking_at t "!" || looking_at t ";") then
     (* Only check for excess if we've already read max_count items *)
     if List.length !items >= max_count then
       match option parser t with
       | Some _ ->
           err t
             ("too many values (maximum " ^ string_of_int max_count
            ^ " allowed)")
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

let enum ?default label mapping t =
  ws t;
  let starts_with_ident = is_ident_start (Option.value (peek t) ~default:' ') in
  if not starts_with_ident then
    match default with
    | Some f -> f t
    | None -> err t (label ^ ": expected " ^ label)
  else (
    (* Try identifier; on failure, restore and delegate to default if present *)
    save t;
    let value = ident t in
    match
      List.find_opt (fun (k, _) -> String.lowercase_ascii k = value) mapping
    with
    | Some (_, result) ->
        commit t;
        result
    | None -> (
        match default with
        | Some default_fn ->
            restore t;
            default_fn t
        | None ->
            let options = List.map fst mapping |> String.concat ", " in
            err t (label ^ ": expected one of: " ^ options ^ ", got: " ^ value)))

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

let list ?(sep = fun (_ : t) -> ()) ?at_least ?at_most item t =
  let rec loop acc =
    match option item t with
    | None -> List.rev acc
    | Some item ->
        let acc = item :: acc in
        if option sep t = Some () then loop acc else List.rev acc
  in
  let items = loop [] in
  (match at_least with
  | Some n when List.length items < n ->
      err t ("expected at least " ^ string_of_int n ^ " item(s)")
  | _ -> ());
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
  match peek t with
  | Some ('"' | '\'') -> string t
  | _ -> String.trim (until t ')')

let url t = call "url" t quoted_or_unquoted_url

let enum_calls ?default cases t =
  ws t;
  let starts_with_ident = is_ident_start (Option.value (peek t) ~default:' ') in
  if not starts_with_ident then
    match default with
    | Some f -> f t
    | None ->
        let options = List.map fst cases |> String.concat ", " in
        err t ("expected one of functions: " ^ options)
  else (
    save t;
    let got = ident t in
    match
      List.find_opt (fun (n, _) -> String.lowercase_ascii n = got) cases
    with
    | None -> (
        match default with
        | Some f ->
            restore t;
            f t
        | None ->
            let options = List.map fst cases |> String.concat ", " in
            err t ("expected one of functions: " ^ options))
    | Some (_, p) ->
        commit t;
        parens
          (fun t ->
            ws t;
            let r = p t in
            ws t;
            r)
          t)

let enum_or_calls ?default label idents ~calls t =
  enum ~default:(enum_calls ?default calls) label idents t

let fold_many parser ~init ~f t =
  let rec loop acc consumed last_error =
    ws t;
    if is_done t then (acc, last_error)
    else
      match try_parse_err parser t with
      | Ok item -> loop (f acc item) true None
      | Error msg -> if consumed then (acc, Some msg) else (init, Some msg)
  in
  loop init false None

let number_with_unit t =
  ws t;
  let n = number t in
  let u =
    if looking_at t "%" then (
      ignore (char t);
      "%")
    else Option.value ~default:"" (option ident t)
  in
  (n, u)

let unit expected t =
  ws t;
  let n = number t in
  let u = ident t in
  if String.equal u expected then n
  else err t ("expected unit '" ^ expected ^ "', got '" ^ u ^ "'")
