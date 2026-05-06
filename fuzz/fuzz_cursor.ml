(** Fuzz tests for the component-value cursor layer. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let cursor buf = Css.Cursor.of_string (cssish buf)

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let safe_component_input buf =
  let ident =
    "x" ^ string_of_int (byte_at buf 0) ^ "-" ^ string_of_int (byte_at buf 1)
  in
  pick
    [
      ident;
      ident ^ " 1px";
      "calc(1rem + 2px)";
      "var(--" ^ ident ^ ",1px)";
      "[" ^ ident ^ "=open]";
      "{color:red}";
    ]
    buf 2

let test_cursor_crash_safety buf =
  let c = cursor buf in
  ignore (Css.Cursor.peek c);
  ignore (Css.Cursor.string_of_remaining c);
  ignore (Css.Cursor.consume_remaining_as_string c);
  ignore (Css.Cursor.is_done c)

let test_save_restore_remaining_stable buf =
  let c = cursor buf in
  let snap = Css.Cursor.save c in
  ignore (Css.Cursor.next c);
  ignore (Css.Cursor.next c);
  Css.Cursor.restore c snap;
  let before = Css.Cursor.string_of_remaining c in
  let snap2 = Css.Cursor.save c in
  ignore (Css.Cursor.next c);
  ignore (Css.Cursor.next c);
  Css.Cursor.restore c snap2;
  let after = Css.Cursor.string_of_remaining c in
  if before <> after then
    fail
      (Fmt.str "cursor restore changed remaining text: %S -> %S" before after)

let test_lookahead_does_not_advance buf =
  let c = cursor buf in
  let before = Css.Cursor.string_of_remaining c in
  ignore (Css.Cursor.lookahead (fun c -> Css.Cursor.next c) c);
  let after = Css.Cursor.string_of_remaining c in
  if before <> after then fail "cursor lookahead advanced the stream"

let test_option_rewinds_on_failure buf =
  let c = cursor buf in
  let before = Css.Cursor.string_of_remaining c in
  ignore (Css.Cursor.option (fun c -> Css.Cursor.expect '\000' c) c);
  let after = Css.Cursor.string_of_remaining c in
  if before <> after then fail "cursor option did not rewind after failure"

let test_components_to_string_idempotent buf =
  let input = safe_component_input buf in
  let c = Css.Cursor.of_string input in
  let once = Css.Cursor.string_of_remaining c in
  let c2 = Css.Cursor.of_string once in
  let twice = Css.Cursor.string_of_remaining c2 in
  let c3 = Css.Cursor.of_string twice in
  let thrice = Css.Cursor.string_of_remaining c3 in
  if twice <> thrice then
    fail
      (Fmt.str "cursor component serialization did not stabilize: %S -> %S"
         twice thrice)

let test_consume_to_boundaries_reparse buf =
  let input = cssish buf ^ "; tail { x: y } / rest" in
  let semi = Css.Cursor.of_string input |> Css.Cursor.consume_to_semicolon in
  ignore (Css.Cursor.of_string semi);
  let decl =
    Css.Cursor.of_string input |> Css.Cursor.consume_to_decl_end ~trim:true
  in
  ignore (Css.Cursor.of_string decl);
  let slash =
    Css.Cursor.of_string input
    |> Css.Cursor.consume_to_slash_or_semicolon ~trim:true
  in
  ignore (Css.Cursor.of_string slash)

let test_list_bounds_stable _buf =
  let c = Css.Cursor.of_string "a,b,c tail" in
  let parsed =
    Css.Cursor.list ~sep:Css.Cursor.comma ~at_least:1 ~at_most:2
      Css.Cursor.ident c
  in
  if List.length parsed > 2 then fail "cursor list ignored at_most";
  let remaining = Css.Cursor.string_of_remaining c in
  if remaining = "" then fail "cursor list overconsumed after at_most"

let suite =
  ( "cursor",
    [
      test_case "cursor crash safety" [ bytes ] test_cursor_crash_safety;
      test_case "save/restore remaining stable" [ bytes ]
        test_save_restore_remaining_stable;
      test_case "lookahead does not advance" [ bytes ]
        test_lookahead_does_not_advance;
      test_case "option rewinds on failure" [ bytes ]
        test_option_rewinds_on_failure;
      test_case "components_to_string idempotent" [ bytes ]
        test_components_to_string_idempotent;
      test_case "consume-to boundaries reparse" [ bytes ]
        test_consume_to_boundaries_reparse;
      test_case "list bounds do not overconsume" [ bytes ]
        test_list_bounds_stable;
    ] )
