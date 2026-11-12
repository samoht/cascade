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

let strip_url_suffix url =
  Uri.of_string url |> Uri.with_uri ~query:None ~fragment:None |> Uri.to_string

(* [C:/a.css] or [c:\a.css] names a file on a Windows drive. RFC 3986 reads the
   letter as a scheme, but WHATWG URL reads a one-letter scheme as a drive
   letter, and no URL scheme in use is one letter long. The decoded path is what
   says so, since [strip_url_suffix] encodes the backslash. *)
let is_drive_path uri =
  match Uri.scheme uri with
  | Some scheme when String.length scheme = 1 && Option.is_none (Uri.host uri)
    -> (
      match Uri.pct_decode (Uri.path uri) with
      | "" -> false
      | path -> path.[0] = '/' || path.[0] = '\\')
  | Some _ | None -> false

let url_file_path url =
  let uri = Uri.of_string url in
  let path = Uri.pct_decode (Uri.path uri) in
  if is_drive_path uri then String.concat "" [ String.sub url 0 2; path ]
  else path

let is_remote_url url =
  let uri = Uri.of_string url in
  (not (is_drive_path uri))
  && (Option.is_some (Uri.scheme uri) || Option.is_some (Uri.host uri))
