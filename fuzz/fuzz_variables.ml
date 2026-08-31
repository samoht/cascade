(** Fuzz tests for CSS custom properties and var() parsing. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let var_name buf =
  "fuzz-" ^ string_of_int (byte_at buf 0) ^ "-" ^ string_of_int (byte_at buf 1)

let fallback_value buf =
  pick
    [
      "red";
      "1rem";
      "calc(100% - 1rem)";
      "var(--gap, 2px)";
      "{ color: red }";
      "[a, b, c]";
    ]
    buf 2

let parse_var input =
  let r = Cursor.of_string input in
  try Some (Css.Variables.read_reference r)
  with Cursor.Parse_error _ | Reader.Parse_error _ -> None

let test_var_ref_crash_safety buf = ignore (parse_var (cssish buf))

let test_generated_var_reference_roundtrip buf =
  let name = var_name buf in
  let fallback = fallback_value buf in
  let input = "var(--" ^ name ^ ", " ^ fallback ^ ")" in
  match parse_var input with
  | None -> ()
  | Some (parsed_name, Some parsed_fallback) ->
      if parsed_name <> name then
        failf "var() name changed: %S -> %S" name parsed_name;
      if fallback <> "" && parsed_fallback = "" then
        failf "var() fallback was emptied: %S -> %S" fallback parsed_fallback
  | Some (_, None) -> fail "generated var() fallback was dropped"

let test_empty_fallback_is_preserved buf =
  let name = var_name buf in
  match parse_var ("var(--" ^ name ^ ",)") with
  | Some (parsed_name, Some "") when parsed_name = name -> ()
  | Some _ -> fail "empty var() fallback was not preserved"
  | None -> fail "empty var() fallback did not parse"

let test_nested_fallback_stays_balanced buf =
  let name = var_name buf in
  let fallback =
    "calc(100% - var(--gap, " ^ string_of_int (byte_at buf 2) ^ "px))"
  in
  match parse_var ("var(--" ^ name ^ ", " ^ fallback ^ ")") with
  | Some (parsed_name, Some parsed_fallback)
    when parsed_name = name && parsed_fallback = fallback ->
      ()
  | Some (_, Some parsed_fallback) ->
      failf "nested var() fallback changed: %S -> %S" fallback parsed_fallback
  | Some (_, None) -> fail "nested var() fallback was dropped"
  | None -> fail "nested var() fallback did not parse"

let test_custom_declaration_name_invariant buf =
  let name = "--" ^ var_name buf in
  (* [custom_property] refuses a value it cannot write back as one declaration,
     so the invariant holds over the pairs it accepts. *)
  match Css.Declaration.custom_property name (cssish buf) with
  | exception Failure _ -> ()
  | decl -> (
      match Css.Variables.custom_declaration_name decl with
      | Some parsed when parsed = name -> ()
      | Some parsed -> failf "custom declaration name changed: %S" parsed
      | None -> fail "custom property declaration was not recognized")

let test_var_compare_antisym buf =
  let _, a =
    Css.Variables.var (var_name buf) Css.Properties.Length (Css.Values.Px 0.)
  in
  let _, b =
    Css.Variables.var
      (var_name (String.sub (buf ^ "x") 1 (String.length buf)))
      Css.Properties.Length (Css.Values.Px 0.)
  in
  let va = Css.Variables.V a in
  let vb = Css.Variables.V b in
  let ab = Css.Variables.compare_vars_by_name va vb in
  let ba = Css.Variables.compare_vars_by_name vb va in
  if ab = 0 && ba <> 0 then fail "variable compare equality not symmetric";
  if ab < 0 && ba <= 0 then fail "variable compare not antisymmetric";
  if ab > 0 && ba >= 0 then fail "variable compare not antisymmetric"

(* CSS Properties and Values API 1 (ED) sec. 5.4.3 gives a component a [+] or
   [#] multiplier, and reading the list repeats the component reader. A reader
   that reports success without consuming anything never lets that repetition
   end, so every component reader rejects an empty stream. The universal syntax
   is the one reader that matches nothing, and sec. 5.4.2 returns it only for
   the syntax string [*] on its own, where no multiplier can reach it. *)
let component_names =
  [
    "<length>";
    "<color>";
    "<number>";
    "<integer>";
    "<percentage>";
    "<length-percentage>";
    "<angle>";
    "<time>";
    "<resolution>";
    "<custom-ident>";
    "<string>";
    "<url>";
    "<image>";
    "<transform-function>";
    "<transform-list>";
  ]

let syntax_string buf =
  if byte_at buf 0 mod 2 = 0 then pick component_names buf 1 else cssish buf

(* Only a component reader is exercised: a multiplied or alternated syntax is
   left alone so a reader that does not consume cannot spin here. *)
let check_leaf_rejects_empty : type a. string -> a Css.Variables.syntax -> unit
    =
 fun name syntax ->
  match syntax with
  | Css.Variables.Universal | Css.Variables.Plus _ | Css.Variables.Hash _
  | Css.Variables.Or _ ->
      ()
  | _ -> (
      match Css.Variables.read_value (Cursor.of_string "") syntax with
      | exception Cursor.Parse_error _ -> ()
      | exception Reader.Parse_error _ -> ()
      | _ -> failf "%S read a value from an empty stream" name)

let test_reader_rejects_empty_stream buf =
  let name = syntax_string buf in
  let quoted = String.concat "" [ "\""; name; "\"" ] in
  match Css.Variables.read_any_syntax (Cursor.of_string quoted) with
  | exception Cursor.Parse_error _ -> ()
  | exception Reader.Parse_error _ -> ()
  | Css.Variables.Syntax syntax -> check_leaf_rejects_empty name syntax

let suite =
  ( "variables",
    [
      test_case "parse var reference crash safety" [ bytes ]
        test_var_ref_crash_safety;
      test_case "generated var reference roundtrip" [ bytes ]
        test_generated_var_reference_roundtrip;
      test_case "empty fallback preserved" [ bytes ]
        test_empty_fallback_is_preserved;
      test_case "nested fallback stays balanced" [ bytes ]
        test_nested_fallback_stays_balanced;
      test_case "custom declaration name invariant" [ bytes ]
        test_custom_declaration_name_invariant;
      test_case "compare vars by name antisymmetric" [ bytes ]
        test_var_compare_antisym;
      test_case "component reader rejects empty stream" [ bytes ]
        test_reader_rejects_empty_stream;
    ] )
