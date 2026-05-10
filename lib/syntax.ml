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
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  url |> cut_at '?' |> cut_at '#'
