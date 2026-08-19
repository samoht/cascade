let is_ascii_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '-' -> true
  | _ -> false

let is_ascii_ident_continue = function
  | '0' .. '9' -> true
  | c -> is_ascii_ident_start c

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

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

let url_file_path url = Uri.of_string url |> Uri.path |> Uri.pct_decode

let is_remote_url url =
  let uri = Uri.of_string url in
  Option.is_some (Uri.scheme uri) || Option.is_some (Uri.host uri)
