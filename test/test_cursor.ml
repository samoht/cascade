(** Cursor module tests. *)

open Cascade

let cursor_of_string s =
  let r = Css.Reader.of_string s in
  let lexer = Css.Lexer.of_reader r in
  let parser = Css.Parser.of_lexer lexer in
  let rec loop acc =
    let cv = Css.Parser.next parser in
    match cv with
    | Css.Component.Preserved { kind = Css.Token.Eof; _ } -> List.rev acc
    | _ -> loop (cv :: acc)
  in
  Css.Cursor.of_components (loop [])

let test_ident () =
  let c = cursor_of_string "foo bar" in
  Alcotest.(check (option string)) "first" (Some "foo") (Css.Cursor.ident_opt c);
  Alcotest.(check (option string))
    "second" (Some "bar") (Css.Cursor.ident_opt c);
  Alcotest.(check (option string)) "eof" None (Css.Cursor.ident_opt c)

let test_number () =
  let c = cursor_of_string "1.5 42" in
  Alcotest.(check (option (float 0.001)))
    "first" (Some 1.5) (Css.Cursor.number_opt c);
  Alcotest.(check (option int)) "second" (Some 42) (Css.Cursor.integer_opt c)

let test_percentage_dimension () =
  let c = cursor_of_string "50% 10px" in
  Alcotest.(check (option (float 0.001)))
    "pct" (Some 50.0)
    (Css.Cursor.percentage_opt c);
  match Css.Cursor.dimension_opt c with
  | Some (n, u) ->
      Alcotest.(check (float 0.001)) "dim value" 10.0 n;
      Alcotest.(check string) "dim unit" "px" u
  | None -> Alcotest.fail "expected dimension"

let test_parens () =
  let c = cursor_of_string "(red)" in
  let inner = Css.Cursor.parens (fun inner -> Css.Cursor.ident inner) c in
  Alcotest.(check string) "inner" "red" inner;
  Alcotest.(check bool) "done" true (Css.Cursor.is_done c)

let test_function_call () =
  let c = cursor_of_string "rgb(1, 2, 3)" in
  let triple =
    Css.Cursor.function_call "rgb"
      (fun args ->
        let a = Css.Cursor.int args in
        Css.Cursor.comma args;
        let b = Css.Cursor.int args in
        Css.Cursor.comma args;
        let d = Css.Cursor.int args in
        (a, b, d))
      c
  in
  match triple with
  | Some (1, 2, 3) -> ()
  | _ -> Alcotest.fail "expected rgb(1, 2, 3)"

let test_function_call_miss () =
  let c = cursor_of_string "calc(1)" in
  let r = Css.Cursor.function_call "rgb" (fun _ -> ()) c in
  Alcotest.(check bool) "no match" true (r = None);
  Alcotest.(check (option string))
    "still there" (Some "calc")
    (match Css.Cursor.peek c with
    | Some (Css.Component.Func f) -> Some f.node.name
    | _ -> None)

let test_enum () =
  let c = cursor_of_string "block" in
  let v =
    Css.Cursor.enum "display" [ ("block", `Block); ("inline", `Inline) ] c
  in
  Alcotest.(check bool) "block matched" true (v = `Block)

let test_enum_miss () =
  let c = cursor_of_string "weird" in
  match Css.Cursor.enum "display" [ ("block", `Block) ] c with
  | exception Css.Cursor.Parse_error e ->
      Alcotest.(check bool)
        "is bad value" true
        (match e.kind with Css.Error.Bad_value _ -> true | _ -> false)
  | _ -> Alcotest.fail "expected Parse_error"

let test_err_unexpected () =
  let c = cursor_of_string "; " in
  match Css.Cursor.ident c with
  | exception Css.Cursor.Parse_error _ -> ()
  | _ -> Alcotest.fail "expected Parse_error"

let suite =
  ( "cursor",
    [
      Alcotest.test_case "ident" `Quick test_ident;
      Alcotest.test_case "number" `Quick test_number;
      Alcotest.test_case "percentage / dimension" `Quick
        test_percentage_dimension;
      Alcotest.test_case "parens" `Quick test_parens;
      Alcotest.test_case "function call" `Quick test_function_call;
      Alcotest.test_case "function call miss" `Quick test_function_call_miss;
      Alcotest.test_case "enum" `Quick test_enum;
      Alcotest.test_case "enum miss" `Quick test_enum_miss;
      Alcotest.test_case "err unexpected" `Quick test_err_unexpected;
    ] )
