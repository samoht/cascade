(** Fuzz tests for CSS declaration parsing and serialization. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let parse_declaration input =
  let r = Css.Cursor.of_string input in
  try Css.Declaration.read_declaration r with Css.Cursor.Parse_error _ -> None

let serialize decl = Css.Declaration.string_of_declaration ~minify:true decl

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let test_read_declaration_crash_safety buf =
  ignore (parse_declaration (cssish buf))

let test_serialization_idempotent buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let once = serialize decl in
      match parse_declaration once with
      | None -> fail (Fmt.str "serialized declaration did not reparse: %S" once)
      | Some reparsed ->
          let twice = serialize reparsed in
          if once <> twice then
            fail
              (Fmt.str "declaration serialization changed: %S -> %S" once twice)
      )

let test_property_name_preserved_after_serialization buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let property = Css.Declaration.property_name decl in
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None -> fail "serialized declaration did not reparse"
      | Some reparsed ->
          let reparsed_property = Css.Declaration.property_name reparsed in
          if property <> reparsed_property then
            fail
              (Fmt.str "declaration property changed: %S -> %S" property
                 reparsed_property))

let test_important_flag_preserved_after_serialization buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let important = Css.Declaration.is_important decl in
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None -> fail "serialized declaration did not reparse"
      | Some reparsed ->
          if important <> Css.Declaration.is_important reparsed then
            fail "declaration !important flag changed across serialization")

let test_custom_property_serialization_shape buf =
  let name =
    "--fuzz-"
    ^ string_of_int (if String.length buf = 0 then 0 else Char.code buf.[0])
  in
  let value = cssish buf in
  let decl = Css.Declaration.custom_property name value in
  let serialized = serialize decl in
  if not (starts_with ~prefix:(name ^ ":") serialized) then
    fail (Fmt.str "custom property lost its name: %S" serialized);
  match parse_declaration serialized with
  | None -> ()
  | Some reparsed ->
      if Css.Declaration.property_name reparsed <> name then
        fail "custom property name changed after reparse"

let test_block_declarations_serialize_individually buf =
  let input = "{color:red;" ^ cssish buf ^ ";margin:1px}" in
  let r = Css.Cursor.of_string input in
  try
    let declarations = Css.Declaration.read_block r in
    List.iter
      (fun decl ->
        let serialized = serialize decl in
        match parse_declaration serialized with
        | None ->
            fail
              (Fmt.str "block declaration serialization did not reparse: %S"
                 serialized)
        | Some _ -> ())
      declarations
  with Css.Cursor.Parse_error _ -> ()

let test_url_declaration_serialization_stays_local buf =
  let url =
    match
      Char.code (if String.length buf = 0 then '\000' else buf.[0]) mod 4
    with
    | 0 -> "../img/logo.svg"
    | 1 -> "#fragment"
    | 2 -> "https://cdn.example/font.woff2"
    | _ -> "data:image/svg+xml,%3Csvg%3E"
  in
  let input = "background-image:url(" ^ url ^ ")" in
  match parse_declaration input with
  | None -> ()
  | Some decl -> (
      let serialized = serialize decl in
      if not (starts_with ~prefix:"background-image:url(" serialized) then
        fail (Fmt.str "url declaration changed shape: %S" serialized);
      match
        Css.Stylesheet.resolve_url_value ~base:"https://example.test/app.css"
          ~url
      with
      | Error (Css.Stylesheet.Requires_platform_context { feature; _ })
        when feature = "URL resolution" ->
          ()
      | Error _ -> fail "URL resolution stub returned wrong error kind"
      | Ok _ -> fail "URL resolution stub unexpectedly succeeded")

let test_custom_property_var_cycle_stub_is_computed_stage buf =
  let name =
    "--cycle-"
    ^ string_of_int (if String.length buf = 0 then 0 else Char.code buf.[0])
  in
  let specified = "var(" ^ name ^ ")" in
  let decl = Css.Declaration.custom_property name specified in
  let serialized = serialize decl in
  if not (starts_with ~prefix:(name ^ ":var(") serialized) then
    fail (Fmt.str "custom property var() shape changed: %S" serialized);
  match
    Css.Stylesheet.resolve_custom_property ~name ~specified
      ~environment:(":root{" ^ name ^ ":" ^ specified ^ "}")
  with
  | Error
      (Css.Stylesheet.Requires_document_context Css.Stylesheet.Computed_value)
    ->
      ()
  | Error _ -> fail "custom property resolution stub returned wrong error kind"
  | Ok _ -> fail "custom property resolution stub unexpectedly succeeded"

let suite =
  ( "declaration",
    [
      test_case "read_declaration crash safety" [ bytes ]
        test_read_declaration_crash_safety;
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "property name preserved after serialization" [ bytes ]
        test_property_name_preserved_after_serialization;
      test_case "important flag preserved after serialization" [ bytes ]
        test_important_flag_preserved_after_serialization;
      test_case "custom property serialization shape" [ bytes ]
        test_custom_property_serialization_shape;
      test_case "block declarations serialize individually" [ bytes ]
        test_block_declarations_serialize_individually;
      test_case "url declaration serialization stays local" [ bytes ]
        test_url_declaration_serialization_stays_local;
      test_case "custom property var cycle stub is computed stage" [ bytes ]
        test_custom_property_var_cycle_stub_is_computed_stage;
    ] )
