let is_ascii_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '-' -> true
  | _ -> false

let is_ascii_ident_continue = function
  | '0' .. '9' -> true
  | c -> is_ascii_ident_start c

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

(* Sec. 4.3.9 opens no ident on [-] then a digit, and a bare [-] is a delim, so
   both shapes slip past a per-character scan that reads [-] as ident-start. *)
let starts_dash_digit s =
  String.length s >= 2 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9'

let rec ascii_ident_continue_from s n i =
  i >= n
  || (is_ascii_ident_continue s.[i] && ascii_ident_continue_from s n (i + 1))

(* Sec. 4.2 shares one range list between ident-start and ident code points, so
   [-] and the digits are the only positional difference. *)
let is_ident_start_cp cp =
  if cp < 0x80 then is_ascii_ident_start (Char.unsafe_chr cp)
  else Lexer.spec_non_ascii_ident_cp cp

let is_ident_cp cp =
  if cp < 0x80 then is_ascii_ident_continue (Char.unsafe_chr cp)
  else Lexer.spec_non_ascii_ident_cp cp

(* [i] is a byte offset, so [i = 0] is the first code point. Malformed UTF-8
   names no code point and so names no ident. *)
let ident_code_point ok i = function
  | Common.String.Scalar u ->
      let cp = Uchar.to_int u in
      ok && if i = 0 then is_ident_start_cp cp else is_ident_cp cp
  | Common.String.Malformed _ -> false

let is_ident s =
  let n = String.length s in
  if n = 0 || starts_dash_digit s || (n = 1 && s.[0] = '-') then false
  else if is_ascii_ident_start s.[0] && ascii_ident_continue_from s n 1 then
    true
  else Common.String.utf8_fold ident_code_point true s

let url_needs_quotes =
  String.exists (function
    | ' ' | ')' | '"' | '\'' | '(' | '\\' -> true
    | _ -> false)

let split_top_level_colon cvs =
  let rec loop before = function
    | [] -> None
    | Component.Preserved { kind = Token.Colon; _ } :: after ->
        Some (List.rev before, after)
    | cv :: rest -> loop (cv :: before) rest
  in
  loop [] cvs

(* WHATWG URL sec. 4.4 path state, as RFC 3986 sec. 3.3: the path ends at the
   first [?] or [#]. The path is kept as written, backslashes included. *)
let strip_url_suffix url =
  let rec cut i =
    if i = String.length url then url
    else match url.[i] with '?' | '#' -> String.sub url 0 i | _ -> cut (i + 1)
  in
  cut 0

let is_ascii_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_scheme_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
  | _ -> false

(* WHATWG URL sec. 4.4 scheme start state and scheme state, as RFC 3986 sec.
   3.1. [Uri] accepts any run of code points other than [/?#] before [:]. *)
let url_scheme url =
  match String.index_opt url ':' with
  | Some n
    when n > 0
         && is_ascii_alpha url.[0]
         && String.for_all is_scheme_char (String.sub url 0 n) ->
      Some (String.lowercase_ascii (String.sub url 0 n))
  | Some _ | None -> None

(* Windows filesystem paths, which [--inline-imports] reads as local on every
   OS. WHATWG URL reads [C:/a.css] as scheme [c] (sec. 4.4 scheme state); its
   Windows drive letter (sec. 4.2) applies only inside a file URL. *)
let is_drive_path s =
  String.length s >= 2 && is_ascii_alpha s.[0] && s.[1] = ':'

let is_unc_path s = String.starts_with ~prefix:"\\\\" s

let local_absolute_path s =
  if is_drive_path s || is_unc_path s then
    Some (String.map (function '\\' -> '/' | c -> c) s)
  else None

(* Sec. 4.2: "starts with a Windows drive letter" needs the input to end, or
   continue with [/], [\], [?] or [#], after the letter and [:] or [|]. *)
let starts_with_drive_letter s =
  String.length s >= 2
  && is_ascii_alpha s.[0]
  && (s.[1] = ':' || s.[1] = '|')
  && (String.length s = 2
     || match s.[2] with '/' | '\\' | '?' | '#' -> true | _ -> false)

(* Sec. 4.4 file state and path state keep a drive letter as the first path
   segment and write it with [:]; the file host state reads [localhost] as the
   empty host, and any other host is the server of a UNC path. *)
let file_url_path url =
  let uri = Uri.of_string url in
  let path = Uri.pct_decode (Uri.path uri) in
  let segment =
    if String.starts_with ~prefix:"/" path then
      String.sub path 1 (String.length path - 1)
    else path
  in
  let path =
    if starts_with_drive_letter segment then
      String.concat ""
        [
          String.sub segment 0 1;
          ":";
          String.sub segment 2 (String.length segment - 2);
        ]
    else path
  in
  match Uri.host uri with
  | None | Some ("" | "localhost") -> path
  | Some host -> String.concat "" [ "//"; host; path ]

let url_file_path url =
  let url = strip_url_suffix url in
  match url_scheme url with
  | Some "file" -> file_url_path url
  | Some _ when not (is_drive_path url) ->
      Uri.pct_decode (Uri.path (Uri.of_string url))
  | Some _ | None -> Uri.pct_decode url

let is_remote_url url =
  match url_scheme url with
  | Some "file" -> false
  | Some _ -> not (is_drive_path url)
  | None -> String.starts_with ~prefix:"//" url
