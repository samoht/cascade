(** CSS Custom Properties (CSS Variables) parsing using Reader API *)

open Reader

(** Check if a property name is a custom property (starts with --) *)
let is_custom_property name =
  String.length name > 2 && String.sub name 0 2 = "--"

(** Read a custom property name (including the -- prefix) *)
let read_custom_property_name t =
  expect_string t "--";
  let name = ident t in
  "--" ^ name

let rec read_fallback_raw t ~depth acc =
  match peek t with
  | None -> String.concat "" (List.rev acc)
  | Some ')' when depth = 0 -> String.concat "" (List.rev acc)
  | Some '(' ->
      skip t;
      read_fallback_raw t ~depth:(depth + 1) ("(" :: acc)
  | Some ')' when depth > 0 ->
      skip t;
      read_fallback_raw t ~depth:(depth - 1) (")" :: acc)
  | _ ->
      let c = char t in
      read_fallback_raw t ~depth (String.make 1 c :: acc)

let read_var_name t =
  if looking_at t "--" then read_custom_property_name t else ident t

(** Read a var() function. *)
let read_var t =
  expect_string t "var(";
  ws t;
  let name = read_var_name t in
  ws t;
  let fallback =
    match peek t with
    | Some ',' ->
        skip t;
        ws t;
        Some (String.trim (read_fallback_raw t ~depth:0 []))
    | _ -> None
  in
  expect t ')';
  (name, fallback)

(** Read a custom property declaration value *)
let read_custom_property_value t =
  (* Custom properties can contain almost anything except unbalanced
     delimiters *)
  let rec read_value depth acc =
    match peek t with
    | None -> String.concat "" (List.rev acc)
    | Some ';' when depth = 0 -> String.concat "" (List.rev acc)
    | Some '}' when depth = 0 -> String.concat "" (List.rev acc)
    | Some ('(' | '[' | '{') ->
        let c = char t in
        read_value (depth + 1) (String.make 1 c :: acc)
    | Some (')' | ']' | '}') when depth > 0 ->
        let c = char t in
        read_value (depth - 1) (String.make 1 c :: acc)
    | Some (('"' | '\'') as quote) ->
        let c = char t in
        let str =
          let rec read_string acc =
            match char t with
            | '\\' ->
                let escaped = char t in
                read_string (String.make 1 escaped :: "\\" :: acc)
            | c when c = quote ->
                String.concat "" (List.rev (String.make 1 c :: acc))
            | c -> read_string (String.make 1 c :: acc)
          in
          read_string [ String.make 1 c ]
        in
        read_value depth (str :: acc)
    | _ ->
        let c = char t in
        read_value depth (String.make 1 c :: acc)
  in
  read_value 0 []
