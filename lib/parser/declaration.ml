(** CSS declaration parser using simple Reader API. *)

open Css.Reader

(** Parse a property name *)
let property_name t =
  ws t;
  while_ t (fun c -> c <> ':' && c <> ';' && c <> '}')

(** Parse property value until semicolon or closing brace *)
let property_value t =
  let rec parse_value acc depth =
    peek t |> function
    | None -> String.concat "" (List.rev acc)
    | Some ';' when depth = 0 -> String.concat "" (List.rev acc)
    | Some '}' when depth = 0 -> String.concat "" (List.rev acc)
    | Some (('(' | '[' | '{') as c) ->
        expect t c;
        parse_value (String.make 1 c :: acc) (depth + 1)
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        expect t c;
        parse_value (String.make 1 c :: acc) (depth - 1)
    | Some (('"' | '\'') as q) ->
        (* Use proper string parsing that handles escapes *)
        let str = string t in
        parse_value ((String.make 1 q ^ str ^ String.make 1 q) :: acc) depth
    | _ -> parse_value (String.make 1 (char t) :: acc) depth
  in
  String.trim (parse_value [] 0)

(** Check for !important at the end of a value *)
let parse_importance value =
  let trimmed = String.trim value in
  if
    String.length trimmed > 10
    && String.sub trimmed (String.length trimmed - 10) 10 = "!important"
  then (String.sub trimmed 0 (String.length trimmed - 10) |> String.trim, true)
  else (trimmed, false)

(** Parse a single declaration. *)
let one t =
  ws t;
  peek t |> function
  | Some '}' | None -> None
  | _ ->
      let name = property_name t in
      ws t;
      expect t ':';
      ws t;
      let value = property_value t in
      ws t;
      (* Skip optional semicolon *)
      ignore
        (peek t = Some ';'
        &&
        (expect t ';';
         true));
      let value, is_important = parse_importance value in
      Some (name, value, is_important)

(** Parse all declarations in a block (without braces). *)
let declarations t =
  let rec loop acc =
    match one t with None -> List.rev acc | Some decl -> loop (decl :: acc)
  in
  loop []

(** Parse declaration block including braces. *)
let block t =
  ws t;
  expect t '{';
  ws t;
  let decls = declarations t in
  ws t;
  expect t '}';
  decls
