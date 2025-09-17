(** Tests for CSS Reader module *)

open Alcotest
open Css.Reader

(* Helper to create parse_error for test expectations *)
let parse_error_expected ?(got = None) ?(filename = "<string>") message reader =
  let context_window, marker_pos = context_window reader in
  Parse_error
    {
      message;
      got;
      position = position reader;
      filename;
      context_window;
      marker_pos;
      callstack = callstack reader;
    }

(* Generic check function for reader tests - handles read operations *)
let check_read name reader ?expected input =
  let expected = Option.value ~default:input expected in
  let r = of_string input in
  let result = reader r in
  Alcotest.(check string) name expected result

let check_read_char name ?expected input =
  let r = of_string input in
  let result = char r in
  Alcotest.(check char)
    name
    (Option.value ~default:(String.get input 0) expected)
    result

let check_peek name expected input =
  let r = of_string input in
  let result = peek r in
  Alcotest.(check (option char)) name expected result

let check_looking_at name expected input pattern =
  let r = of_string input in
  let result = looking_at r pattern in
  Alcotest.(check bool) name expected result

(* Helper to check Parse_error fields match *)
let check_parse_error_fields name expected actual =
  if actual.message <> expected.message then
    Alcotest.failf "%s: expected message '%s' but got '%s'" name
      expected.message actual.message
  else if actual.got <> expected.got then
    Alcotest.failf "%s: expected got=%a but got=%a" name
      Fmt.(option string)
      expected.got
      Fmt.(option string)
      actual.got

(* Helper to check that a function raises a specific exception *)
let check_raises name expected_exn f =
  try
    f ();
    Alcotest.failf "%s: expected exception but none was raised" name
  with
  | Parse_error actual
    when match expected_exn with
         | Parse_error expected ->
             check_parse_error_fields name expected actual;
             true
         | _ -> false ->
      ()
  | exn when exn = expected_exn ->
      (* For other exceptions, use structural equality *)
      ()
  | exn ->
      Alcotest.failf "%s: expected %s but got %s" name
        (Printexc.to_string expected_exn)
        (Printexc.to_string exn)

(* Test basic operations *)
let basic () =
  (* Peek tests *)
  check_peek "peek h" (Some 'h') "hello";
  check_peek "peek empty" None "";

  (* Char reading *)
  check_read_char "read h" ~expected:'h' "hello";

  (* is_done tests *)
  let r = of_string "" in
  Alcotest.(check bool) "empty is done" true (is_done r);
  let r = of_string "x" in
  Alcotest.(check bool) "not done" false (is_done r);
  ignore (char r);
  Alcotest.(check bool) "done after read" true (is_done r)

(* Test string operations *)
let string_ops () =
  (* peek_string *)
  let r = of_string "hello world" in
  Alcotest.(check string) "peek 5 chars" "hello" (peek_string r 5);
  Alcotest.(check string) "peek 0 chars" "" (peek_string r 0);

  (* looking_at *)
  check_looking_at "looking at hello" true "hello world" "hello";
  check_looking_at "not looking at world" false "hello world" "world";
  check_looking_at "looking at empty" true "anything" "";

  (* skip 3 chars manually *)
  let r = of_string "hello" in
  skip r;
  skip r;
  skip r;
  Alcotest.(check (option char)) "after skip 3" (Some 'l') (peek r)

(* Test whitespace handling *)
let whitespace () =
  (* Basic whitespace *)
  let r = of_string "  \t\n test" in
  Css.Reader.ws r;
  Alcotest.(check (option char)) "after ws" (Some 't') (peek r);

  (* No whitespace *)
  let r = of_string "test" in
  Css.Reader.ws r;
  Alcotest.(check (option char)) "no ws to skip" (Some 't') (peek r);

  (* Only whitespace *)
  let r = of_string "   \t\n  " in
  Css.Reader.ws r;
  Alcotest.(check bool) "all ws done" true (is_done r)

(* Test comment skipping *)
let comments () =
  (* Single comment *)
  let r = of_string "/*comment*/x" in
  Css.Reader.ws r;
  Alcotest.(check (option char)) "after comment" (Some 'x') (peek r);

  (* Multiple comments *)
  let r = of_string "/*c1*/  /*c2*/y" in
  Css.Reader.ws r;
  Alcotest.(check (option char)) "after comments" (Some 'y') (peek r);

  (* Nested comments not supported - should stop at first closing *)
  let r = of_string "/*outer/*inner*/*/x" in
  Css.Reader.ws r;
  Alcotest.(check (option char)) "nested comment" (Some '*') (peek r)

(* Test take_while *)
let take_while_case () =
  (* Digits *)
  let r = of_string "123abc" in
  let digits = while_ r (fun c -> c >= '0' && c <= '9') in
  Alcotest.(check string) "digits" "123" digits;

  (* Letters *)
  let letters = while_ r (fun c -> c >= 'a' && c <= 'z') in
  Alcotest.(check string) "letters" "abc" letters;

  (* Empty match *)
  let r = of_string "!@#" in
  let alphas = while_ r (fun c -> c >= 'a' && c <= 'z') in
  Alcotest.(check string) "no match" "" alphas

(* Test expect *)
let expect_case () =
  (* Success case *)
  let r = of_string "test" in
  expect 't' r;
  Alcotest.(check (option char)) "after expect" (Some 'e') (peek r);

  (* Failure case *)
  let r = of_string "test" in
  check_raises "expect wrong char"
    (parse_error_expected "Expected 'x' but got 't'" r) (fun () -> expect 'x' r)

(* Test expect_string *)
let expect_string_case () =
  (* Success *)
  let r = of_string "hello world" in
  expect_string "hello" r;
  Alcotest.(check (option char)) "after expect string" (Some ' ') (peek r);

  (* Failure *)
  let r = of_string "hello" in
  check_raises "expect wrong string"
    (parse_error_expected "expected \"world\"" r) (fun () ->
      expect_string "world" r)

(* Test between delimiters *)
let between_case () =
  (* Parentheses *)
  let r = of_string "(content)" in
  let result = parens (fun r -> while_ r (fun c -> c != ')')) r in
  Alcotest.(check string) "parens" "content" result;

  (* Brackets *)
  let r = of_string "[data]" in
  expect '[' r;
  let result = while_ r (fun c -> c != ']') in
  expect ']' r;
  Alcotest.(check string) "brackets" "data" result;

  (* Braces *)
  let r = of_string "{block}" in
  let result = braces (fun r -> while_ r (fun c -> c != '}')) r in
  Alcotest.(check string) "braces" "block" result;

  (* Nested - just test that parens works with complex content *)
  let r = of_string "(content)" in
  let result = parens (fun r -> while_ r (fun c -> c != ')')) r in
  Alcotest.(check string) "parens with content" "content" result

(* Test try_parse backtracking (replaces save/restore functionality) *)
let backtrack () =
  let r = of_string "test" in
  (* Test that option backtracks on failure *)
  let result =
    Css.Reader.option
      (fun r ->
        ignore (char r);
        (* consume 't' *)
        if char r = 'x' then "success" else failwith "not x")
      r
  in
  Alcotest.(check (option string)) "option failed" None result;
  (* Position should be restored *)
  Alcotest.(check (option char)) "position restored" (Some 't') (peek r)

(* Test option *)
let option_case () =
  (* Success *)
  let r = of_string "123" in
  let result = option (fun r -> while_ r (fun c -> c >= '0' && c <= '9')) r in
  Alcotest.(check (option string)) "parsed digits" (Some "123") result;

  (* Failure - position restored *)
  let r = of_string "abc" in
  let result =
    option
      (fun r ->
        expect 'x' r;
        (* This will fail *)
        "never reached")
      r
  in
  Alcotest.(check (option string)) "parse failed" None result;
  Alcotest.(check (option char)) "position restored" (Some 'a') (peek r)

(* Test commit *)
let commit_case () =
  (* Test that option commits changes on success *)
  let r = of_string "test" in
  let result =
    Css.Reader.option
      (fun r ->
        ignore (char r);
        (* consume 't' *)
        "success")
      r
  in
  Alcotest.(check (option string)) "option success" (Some "success") result;
  (* Position should remain advanced after successful option *)
  Alcotest.(check (option char)) "position advanced" (Some 'e') (peek r)

(* Test number parsing *)
let numbers () =
  (* Float *)
  let r = of_string "3.14" in
  let n = number r in
  Alcotest.(check (float 0.001)) "float" 3.14 n;

  let r = of_string "42" in
  let n = number r in
  Alcotest.(check (float 0.001)) "float no decimal" 42.0 n;

  let r = of_string ".5" in
  let n = number r in
  Alcotest.(check (float 0.001)) "float leading dot" 0.5 n;

  let r = of_string "-2.5" in
  let n = number r in
  Alcotest.(check (float 0.001)) "negative float" (-2.5) n;

  (* Scientific notation tests *)
  let r = of_string "1e2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "scientific notation" 100.0 n;

  let r = of_string "1E2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "scientific notation uppercase" 100.0 n;

  let r = of_string "1.5e2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "scientific notation with decimal" 150.0 n;

  let r = of_string "1e+2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "scientific notation with plus" 100.0 n;

  let r = of_string "1e-2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "scientific notation negative exp" 0.01 n;

  let r = of_string "-3.4e-2" in
  let n = number r in
  Alcotest.(check (float 0.001)) "complex scientific notation" (-0.034) n;

  let r = of_string ".5e3" in
  let n = number r in
  Alcotest.(check (float 0.001)) "leading dot with exponent" 500.0 n

(* Test unit parsing *)
let units () =
  (* Units are parsed as identifiers *)
  let r = of_string "px" in
  let unit = ident r in
  Alcotest.(check string) "px unit" "px" unit;

  let r = of_string "rem" in
  let unit = ident r in
  Alcotest.(check string) "rem unit" "rem" unit;

  (* Percentage *)
  let r = of_string "%" in
  let c = char r in
  Alcotest.(check char) "percent" '%' c

(* Test identifier parsing *)
let ident_case () =
  (* Simple ident *)
  check_read "simple ident" ident ~expected:"hello" "hello";
  check_read "with dash" ident ~expected:"my-class" "my-class";
  check_read "with underscore" ident ~expected:"my_var" "my_var";
  check_read "with number" ident ~expected:"h1" "h1";

  (* Starting with dash *)
  check_read "dash prefix" ident ~expected:"-webkit" "-webkit";

  (* Custom property *)
  check_read "custom prop" ident ~expected:"--my-var" "--my-var"

(* Test string parsing *)
let string_literals () =
  (* Double quotes return content without quotes *)
  check_read "double quote" string ~expected:"hello" "\"hello\"";
  check_read "double empty" string ~expected:"" "\"\"";

  (* Single quotes *)
  check_read "single quote" string ~expected:"world" "'world'";
  check_read "single empty" string ~expected:"" "''";

  (* Escaped quotes yield unescaped content *)
  check_read "escaped double" string ~expected:"a\"b" "\"a\\\"b\"";
  check_read "escaped single" string ~expected:"a'b" "'a\\'b'"

(* Test until_string *)
let until_string () =
  let r = of_string "data;more" in
  let data = until r ';' in
  Alcotest.(check string) "until semicolon" "data" data;

  let r = of_string "no delimiter here" in
  let all = while_ r (fun _ -> true) in
  Alcotest.(check string) "delimiter not found" "no delimiter here" all

(* Test hex colors *)
let hex_case () =
  (* Hex colors are just parsed as strings starting with # *)
  let r = of_string "#abc" in
  expect '#' r;
  let hex =
    while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  Alcotest.(check string) "3 digit hex" "abc" hex;

  let r = of_string "#123456" in
  expect '#' r;
  let hex =
    while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  Alcotest.(check string) "6 digit hex" "123456" hex

(* Test identifier with escapes *)
let ident_with_escapes () =
  (* Unicode escape *)
  let r = of_string "\\0041 bc" in
  (* \0041 is 'A' *)
  let id = ident r in
  Alcotest.(check string) "unicode escape" "Abc" id;

  (* Escaped special char *)
  let r = of_string "\\@foo" in
  let id = ident r in
  Alcotest.(check string) "escaped @" "@foo" id;

  (* Multiple escapes *)
  let r = of_string "\\31 \\32 \\33" in
  (* \31 \32 \33 are '1' '2' '3' *)
  let id = ident r in
  Alcotest.(check string) "multiple escapes" "123" id

(* Test failure cases *)
let failures () =
  (* EOF in string *)
  let r = of_string "\"unclosed" in
  check_raises "unclosed string" (parse_error_expected "unclosed string" r)
    (fun () -> ignore (string r));

  (* Invalid number *)
  let r = of_string "abc" in
  check_raises "not a number" (parse_error_expected "invalid number" r)
    (fun () -> ignore (number r))

(* Test option helper *)
let option_parser () =
  (* Success case *)
  let r = of_string "123 abc" in
  let result = option number r in
  (match result with
  | Some n -> Alcotest.(check (float 0.001)) "parsed number" 123.0 n
  | None -> Alcotest.fail "Expected success but got None");

  (* Error case with message preservation *)
  let r = of_string "not-a-number" in
  let result = option number r in
  (match result with
  | Some _ -> Alcotest.fail "Expected error but got success"
  | None -> Alcotest.(check bool) "parse failed" true true);

  (* Position restoration on error *)
  Alcotest.(check (option char))
    "position restored after error" (Some 'n') (peek r)

(* Test parse_many helper *)
let parse_many () =
  (* Parse multiple numbers *)
  let r = of_string "1 2 3 xyz" in
  let numbers, error = many number r in
  Alcotest.(check (list (float 0.001)))
    "parsed numbers" [ 1.0; 2.0; 3.0 ] numbers;
  Alcotest.(check (option string))
    "error on non-number" (Some "invalid number") error;

  (* Empty input *)
  let r = of_string "" in
  let numbers, error = many number r in
  Alcotest.(check (list (float 0.001))) "empty list" [] numbers;
  Alcotest.(check (option string)) "no error" None error;

  (* All valid items *)
  let r = of_string "10 20 30" in
  let numbers, error = many number r in
  Alcotest.(check (list (float 0.001))) "all valid" [ 10.0; 20.0; 30.0 ] numbers;
  Alcotest.(check (option string)) "no error at end" None error;

  (* Parse identifiers *)
  let r = of_string "foo bar baz 123" in
  let ids, error = many ident r in
  Alcotest.(check (list string)) "parsed idents" [ "foo"; "bar"; "baz" ] ids;
  Alcotest.(check bool) "error is some" true (Option.is_some error)

(* Test take helper *)
let take_case () =
  (* Parse exactly 2 numbers *)
  let r = of_string "1 2" in
  let numbers = take 2 number r in
  Alcotest.(check (list (float 0.001))) "parsed 2 numbers" [ 1.0; 2.0 ] numbers;

  (* Parse 1 number (within limit of 3) *)
  let r = of_string "5" in
  let numbers = take 3 number r in
  Alcotest.(check (list (float 0.001))) "parsed 1 number" [ 5.0 ] numbers;

  (* Parse exactly max count (4 numbers) *)
  let r = of_string "10 20 30 40" in
  let numbers = take 4 number r in
  Alcotest.(check (list (float 0.001)))
    "parsed 4 numbers" [ 10.0; 20.0; 30.0; 40.0 ] numbers;

  (* Too many values should fail *)
  let r = of_string "1 2 3 4 5" in
  check_raises "too many values"
    (parse_error_expected "too many values (maximum 4 allowed)" r) (fun () ->
      ignore (take 4 number r));

  (* Empty input should fail *)
  let r = of_string "" in
  check_raises "empty input" (parse_error_expected "invalid number" r)
    (fun () -> ignore (take 2 number r));

  (* Parse stops at non-matching content *)
  let r = of_string "1 2 abc 3" in
  let numbers = take 4 number r in
  Alcotest.(check (list (float 0.001)))
    "parse stops at non-number" [ 1.0; 2.0 ] numbers;

  (* Parse stops at semicolon *)
  let r = of_string "10 20; more content" in
  let numbers = take 3 number r in
  Alcotest.(check (list (float 0.001)))
    "parse stops at semicolon" [ 10.0; 20.0 ] numbers;

  (* Parse stops at !important *)
  let r = of_string "5 10 !important" in
  let numbers = take 4 number r in
  Alcotest.(check (list (float 0.001)))
    "parse stops at !important" [ 5.0; 10.0 ] numbers

(* Test one_of helper *)
let one_of_case () =
  (* Helper to parse specific keywords *)
  let parse_keyword kw r =
    let id = ident r in
    if id = kw then id else err ~got:id r kw
  in

  (* Helper to parse number as string *)
  let number_as_string r =
    try
      let n = number r in
      Fmt.str "%.0f" n
    with Parse_error _ ->
      let found = ident r in
      err ~got:found r "number"
  in

  let parsers =
    [
      (fun r -> parse_keyword "red" r);
      (fun r -> parse_keyword "blue" r);
      (fun r -> parse_keyword "green" r);
      number_as_string;
    ]
  in

  (* Match first parser *)
  let r = of_string "red" in
  let result = one_of parsers r in
  Alcotest.(check string) "matched red" "red" result;

  (* Match second parser *)
  let r = of_string "blue" in
  let result = one_of parsers r in
  Alcotest.(check string) "matched blue" "blue" result;

  (* Match last parser (number) *)
  let r = of_string "42" in
  let result = one_of parsers r in
  Alcotest.(check string) "matched number" "42" result;

  (* No match - should raise with descriptive error *)
  let r = of_string "yellow" in
  check_raises "no match"
    (parse_error_expected ~got:(Some "yellow")
       "expected one of: number, green, blue, red" r) (fun () ->
      ignore (one_of parsers r))

let enum_case () =
  (* Test basic enum parsing *)
  let r = of_string "bold" in
  let result =
    enum "font-weight" [ ("normal", 1); ("bold", 2); ("lighter", 3) ] r
  in
  Alcotest.(check int) "enum bold" 2 result;

  (* Test case insensitive matching *)
  let r = of_string "BOLD" in
  let result =
    enum "font-weight" [ ("normal", 1); ("bold", 2); ("lighter", 3) ] r
  in
  Alcotest.(check int) "enum BOLD" 2 result;

  (* Test enum with unknown value should fail *)
  let r = of_string "unknown" in
  check_raises "enum unknown"
    (parse_error_expected
       "font-weight: expected one of: normal, bold, lighter, got: unknown" r)
    (fun () ->
      let _ =
        enum "font-weight" [ ("normal", 1); ("bold", 2); ("lighter", 3) ] r
      in
      ())

let enum_with_default () =
  (* Test enum with default handler for identifiers *)
  let r = of_string "bold" in
  let result =
    enum "test"
      [ ("normal", "n"); ("bold", "b") ]
      r
      ~default:(fun _ -> "default")
  in
  Alcotest.(check string) "enum bold" "b" result;

  (* Test enum with default handler for unknown identifier *)
  let r = of_string "unknown" in
  let result =
    enum "test"
      [ ("normal", "n"); ("bold", "b") ]
      r
      ~default:(fun _ -> "default")
  in
  Alcotest.(check string) "enum unknown uses default" "default" result;

  (* Test enum with default handler for numeric value *)
  let r = of_string "123" in
  let result =
    enum "font-weight"
      [ ("normal", 0); ("bold", 700) ]
      r
      ~default:(fun t -> int_of_float (number t))
  in
  Alcotest.(check int) "enum numeric with default" 123 result;

  (* Test enum with default handler for float value *)
  let r = of_string "1.5" in
  let result =
    enum "line-height" [ ("normal", 0.0) ] r ~default:(fun t -> number t)
  in
  check (float 0.01) "enum float with default" 1.5 result;

  (* Test that default is called first for non-identifier start *)
  let r = of_string "400" in
  let result =
    enum "font-weight"
      [ ("normal", 0); ("bold", 700) ]
      r
      ~default:(fun t -> int_of_float (number t))
  in
  Alcotest.(check int) "enum calls default first for number" 400 result;

  (* Test that default is NOT called when identifier matches an enum value *)
  let r = of_string "normal" in
  let result =
    enum "font-weight"
      [ ("normal", 100); ("bold", 700) ]
      r
      ~default:(fun _ -> failwith "should not call default")
  in
  Alcotest.(check int) "enum uses matching value, not default" 100 result

(* Test call stack functionality *)
let callstack_case () =
  let r = of_string "test" in

  (* Initially empty call stack *)
  Alcotest.(check (list string)) "initial callstack empty" [] (callstack r);

  (* Push context *)
  push_context r "context1";
  Alcotest.(check (list string)) "single context" [ "context1" ] (callstack r);

  (* Push another context *)
  push_context r "context2";
  Alcotest.(check (list string))
    "nested contexts" [ "context1"; "context2" ] (callstack r);

  (* Pop context *)
  pop_context r;
  Alcotest.(check (list string)) "after pop" [ "context1" ] (callstack r);

  (* Pop last context *)
  pop_context r;
  Alcotest.(check (list string)) "empty after pop all" [] (callstack r);

  (* Pop empty stack should not crash *)
  pop_context r;
  Alcotest.(check (list string)) "pop empty stack" [] (callstack r)

let with_context_case () =
  let r = of_string "test" in
  let result = ref [] in

  (* Test with_context preserves and cleans up context *)
  with_context r "test_context" (fun () ->
      result := callstack r;
      42)
  |> ignore;

  Alcotest.(check (list string))
    "context during execution" [ "test_context" ] !result;
  Alcotest.(check (list string)) "context cleaned up after" [] (callstack r)

let with_context_exception () =
  let r = of_string "test" in

  (* Test that context is cleaned up even when exception is raised *)
  (try
     with_context r "test_context" (fun () -> failwith "test exception")
     |> ignore
   with Failure _ -> ());

  Alcotest.(check (list string))
    "context cleaned up after exception" [] (callstack r)

let enum_call_stack () =
  let r = of_string "invalid" in
  let call_stack_during_error = ref [] in

  (* Test that enum combinator populates call stack when failing *)
  (try
     enum "test-enum" [ ("valid", 42) ] r |> ignore;
     Alcotest.fail "Should have raised Parse_error"
   with Css.Reader.Parse_error error ->
     call_stack_during_error := error.callstack);

  (* Should contain the enum context when error occurred *)
  Alcotest.(check (list string))
    "enum adds context to call stack" [ "enum:test-enum" ]
    !call_stack_during_error

let enum_or_calls_stack () =
  let r = of_string "invalid" in
  let call_stack_during_error = ref [] in

  (* Test enum_or_calls populates call stack *)
  (try
     enum_or_calls "test-property" [ ("valid", 42) ] ~calls:[] r |> ignore;
     Alcotest.fail "Should have raised Parse_error"
   with Css.Reader.Parse_error error ->
     call_stack_during_error := error.callstack);

  (* Should contain the enum_or_calls context *)
  Alcotest.(check (list string))
    "enum_or_calls adds context to call stack"
    [ "enum_or_calls:test-property" ]
    !call_stack_during_error

let concatenated_var_functions () =
  (* Test that the CSS parser can handle concatenated var() functions. The
     handler must actually consume the input to avoid infinite loops. *)
  let test_concatenated_vars input expected_vars desc =
    let r = of_string input in
    let vars_found = ref [] in

    (* Proper var parser that actually consumes the function *)
    let read_var_function r =
      (* Parse "var(" *)
      expect_string "var" r;
      expect '(' r;
      ws r;

      (* Parse the variable name *)
      expect_string "--" r;
      let name = ident ~keep_case:true r in
      ws r;

      (* Parse optional fallback *)
      if peek r = Some ',' then (
        comma r;
        ws r (* For empty fallback, just skip to closing paren *));

      (* Parse closing paren *)
      expect ')' r;

      vars_found := name :: !vars_found;
      `Var name
    in

    (* Parse concatenated var functions without spaces between them *)
    let rec parse_vars acc =
      ws r;
      if is_done r then List.rev acc
      else if looking_at r "var(" then
        let var = read_var_function r in
        parse_vars (var :: acc)
      else List.rev acc
    in

    let tokens = parse_vars [] in
    let found = List.rev !vars_found in

    Alcotest.(check (list string)) (desc ^ " - var names") expected_vars found;
    Alcotest.(check int)
      (desc ^ " - token count")
      (List.length expected_vars)
      (List.length tokens)
  in

  (* Test cases *)
  test_concatenated_vars "var(--a)" [ "a" ] "single var";
  test_concatenated_vars "var(--a) var(--b)" [ "a"; "b" ] "spaced vars";
  test_concatenated_vars "var(--a,)var(--b,)" [ "a"; "b" ]
    "concatenated vars with empty fallback";
  test_concatenated_vars "var(--a,)var(--b,)var(--c,)" [ "a"; "b"; "c" ]
    "three concatenated vars";
  test_concatenated_vars "var(--tw-ordinal,)var(--tw-slashed-zero,)"
    [ "tw-ordinal"; "tw-slashed-zero" ]
    "tailwind-like vars"

let list_call_stack () =
  let r = of_string "invalid" in
  let call_stack_during_error = ref [] in
  let failing_parser r = enum "failing" [ ("valid", 42) ] r in

  (* Test that list combinator adds context *)
  (try
     Css.Reader.list ~at_least:1 failing_parser r |> ignore;
     Alcotest.fail "Should have raised Parse_error"
   with Css.Reader.Parse_error error ->
     call_stack_during_error := error.callstack);

  (* Should contain both list and enum contexts *)
  let expected = [ "list"; "enum:failing" ] in
  Alcotest.(check (list string))
    "list and enum add nested contexts" expected !call_stack_during_error

let fold_many_call_stack () =
  (* Test that fold_many preserves call stack on errors *)
  let r = of_string "valid invalid" in
  let parser r = enum "item" [ ("valid", 1); ("good", 2) ] r in
  let acc, error_opt =
    Css.Reader.fold_many parser ~init:[] ~f:(fun acc x -> x :: acc) r
  in

  (* Should have parsed one item before failing *)
  Alcotest.(check (list int)) "parsed one valid item" [ 1 ] acc;
  Alcotest.(check bool) "has error message" true (Option.is_some error_opt);

  (* Now test that the error preserves context *)
  let r2 = of_string "invalid" in
  try
    with_context r2 "fold-context" (fun () ->
        let _, _ =
          Css.Reader.fold_many parser ~init:[] ~f:(fun acc x -> x :: acc) r2
        in
        ())
  with Css.Reader.Parse_error error ->
    (* Should have fold-context in the call stack *)
    Alcotest.(check bool)
      "fold_many preserves context" true
      (List.mem "fold-context" error.callstack)

let fold_many_with_enum_context () =
  (* Test that fold_many inside enum preserves both contexts *)
  let r = of_string "invalid" in

  (* This simulates what happens with border parsing *)
  let parser r =
    enum "outer-enum" []
      ~default:(fun r ->
        let parse_component r =
          one_of
            [
              (fun r -> enum "inner1" [ ("valid1", 1) ] r);
              (fun r -> enum "inner2" [ ("valid2", 2) ] r);
            ]
            r
        in
        let acc, _ =
          Css.Reader.fold_many parse_component ~init:[]
            ~f:(fun acc x -> x :: acc)
            r
        in
        acc)
      r
  in

  try
    let _ = parser r in
    Alcotest.fail "Should have raised Parse_error"
  with Css.Reader.Parse_error error ->
    (* Check if outer-enum is in the call stack *)
    Alcotest.(check bool)
      "enum context preserved with fold_many" true
      (List.mem "enum:outer-enum" error.callstack
      || List.mem "outer-enum" error.callstack)

let many_call_stack () =
  (* Test that many preserves call stack on errors *)
  let r = of_string "valid invalid" in
  let parser r = enum "item" [ ("valid", 1); ("good", 2) ] r in
  let items, error_opt = Css.Reader.many parser r in

  (* Should have parsed one item before failing *)
  Alcotest.(check (list int)) "parsed one valid item" [ 1 ] items;
  Alcotest.(check bool) "has error message" true (Option.is_some error_opt);

  (* Now test error context preservation *)
  let r2 = of_string "invalid" in
  try
    with_context r2 "many-context" (fun () ->
        let _, _ = Css.Reader.many parser r2 in
        ())
  with Css.Reader.Parse_error error ->
    (* Should have many-context in the call stack *)
    Alcotest.(check bool)
      "many preserves context" true
      (List.mem "many-context" error.callstack)

let triple_call_stack () =
  (* Test that triple combinator preserves context *)
  let r = of_string "1 2 invalid" in
  let parse_int r = int_of_float (Css.Reader.number r) in

  try
    with_context r "triple-context" (fun () ->
        let _ = Css.Reader.triple ~sep:ws parse_int parse_int parse_int r in
        ())
  with Css.Reader.Parse_error error ->
    (* Should have triple-context in the call stack *)
    Alcotest.(check bool)
      "triple preserves context" true
      (List.mem "triple-context" error.callstack)

(* Test error message formatting for different input types *)
let error_formatting_multiline () =
  (* Test error on multi-line CSS with previous line context *)
  let multiline_css =
    ".class1 {\n  color: red;\n}\n.class2 {\n  invalid-prop: value;\n}"
  in
  let r = of_string multiline_css in

  (* Navigate to the error position (around "invalid-prop") *)
  let rec skip_to_pattern pattern =
    if (not (is_done r)) && not (looking_at r pattern) then (
      skip r;
      skip_to_pattern pattern)
  in
  skip_to_pattern "invalid";

  try
    expect 'x' r;
    (* This will fail *)
    Alcotest.fail "Should have raised Parse_error"
  with Parse_error error ->
    (* Check that context includes previous line *)
    Alcotest.(check bool)
      "has newline in context" true
      (String.contains error.context_window '\n');
    (* Check that the context isn't too long (should be around 80 chars) *)
    Alcotest.(check bool)
      "context reasonable length" true
      (String.length error.context_window < 120);
    (* Check that filename includes line:col info *)
    Alcotest.(check bool)
      "filename has line info" true
      (String.contains error.filename ':')

let error_formatting_long_line () =
  (* Test error on very long single line (minified CSS) *)
  let long_css = String.make 200 'x' ^ "invalid" ^ String.make 200 'y' in
  let r = of_string long_css in

  (* Navigate to around the middle *)
  for _ = 1 to 205 do
    skip r
  done;

  try
    expect 'z' r;
    (* This will fail *)
    Alcotest.fail "Should have raised Parse_error"
  with Parse_error error ->
    (* Check that context is around 80 characters *)
    let context_len = String.length error.context_window in
    Alcotest.(check bool)
      "context around 80 chars" true
      (context_len >= 70 && context_len <= 90);
    (* Check that marker position is roughly in the middle *)
    Alcotest.(check bool)
      "marker near middle" true
      (error.marker_pos >= 30 && error.marker_pos <= 50);
    (* Should not have newlines for single line *)
    Alcotest.(check bool)
      "no newlines in single line" false
      (String.contains error.context_window '\n')

let error_formatting_short_input () =
  (* Test error on very short input *)
  let short_css = "ab" in
  let r = of_string short_css in

  try
    expect 'x' r;
    (* This will fail *)
    Alcotest.fail "Should have raised Parse_error"
  with Parse_error error ->
    (* Check that context is the entire short string *)
    Alcotest.(check string)
      "context is full short string" short_css error.context_window;
    (* Check marker position *)
    Alcotest.(check int) "marker at start" 0 error.marker_pos

let error_formatting_at_start () =
  (* Test error at very beginning of input *)
  let css = "invalid syntax here" in
  let r = of_string css in

  try
    expect 'x' r;
    (* This will fail immediately *)
    Alcotest.fail "Should have raised Parse_error"
  with Parse_error error ->
    (* Check that context starts from beginning *)
    Alcotest.(check int) "marker at start" 0 error.marker_pos;
    (* Check that we get reasonable context from the start *)
    Alcotest.(check bool)
      "has some context" true
      (String.length error.context_window > 0)

let error_formatting_at_end () =
  (* Test error near end of input *)
  let css = "some valid css syntax" in
  let r = of_string css in

  (* Navigate to near the end *)
  while not (is_done r) do
    skip r
  done;

  try
    expect 'x' r;
    (* This will fail at EOF *)
    Alcotest.fail "Should have raised Parse_error"
  with Parse_error error ->
    (* Should have context leading up to the end *)
    Alcotest.(check bool)
      "has leading context" true
      (String.length error.context_window > 0);
    (* Marker should be at end of context *)
    Alcotest.(check bool)
      "marker near end" true
      (error.marker_pos >= String.length error.context_window - 5)

let suite =
  [
    ( "reader",
      [
        (* Basic operations *)
        test_case "basic" `Quick basic;
        test_case "string" `Quick string_ops;
        test_case "whitespace" `Quick whitespace;
        test_case "comments" `Quick comments;
        (* Parsing helpers *)
        test_case "take while" `Quick take_while_case;
        test_case "expect" `Quick expect_case;
        test_case "expect string" `Quick expect_string_case;
        test_case "between" `Quick between_case;
        (* Backtracking *)
        test_case "backtrack" `Quick backtrack;
        test_case "option" `Quick option_case;
        test_case "commit" `Quick commit_case;
        (* Value parsing *)
        test_case "numbers" `Quick numbers;
        test_case "units" `Quick units;
        test_case "ident" `Quick ident_case;
        test_case "string literals" `Quick string_literals;
        test_case "until string" `Quick until_string;
        test_case "hex" `Quick hex_case;
        (* Special cases *)
        test_case "ident with escapes" `Quick ident_with_escapes;
        (* Error cases *)
        test_case "failures" `Quick failures;
        (* New helper functions *)
        test_case "option" `Quick option_parser;
        test_case "parse_many" `Quick parse_many;
        test_case "one_of" `Quick one_of_case;
        test_case "take" `Quick take_case;
        (* enum combinator tests *)
        test_case "enum" `Quick enum_case;
        test_case "enum with default" `Quick enum_with_default;
        (* call stack tests *)
        test_case "call stack" `Quick callstack_case;
        test_case "with context" `Quick with_context_case;
        test_case "with context exception" `Quick with_context_exception;
        test_case "enum call stack" `Quick enum_call_stack;
        test_case "enum_or_calls call stack" `Quick enum_or_calls_stack;
        test_case "concatenated var functions" `Quick concatenated_var_functions;
        test_case "list call stack" `Quick list_call_stack;
        test_case "fold_many call stack" `Quick fold_many_call_stack;
        test_case "fold_many with enum context" `Quick
          fold_many_with_enum_context;
        test_case "many call stack" `Quick many_call_stack;
        test_case "triple call stack" `Quick triple_call_stack;
        (* Error formatting tests *)
        test_case "error formatting multiline" `Quick error_formatting_multiline;
        test_case "error formatting long line" `Quick error_formatting_long_line;
        test_case "error formatting short input" `Quick
          error_formatting_short_input;
        test_case "error formatting at start" `Quick error_formatting_at_start;
        test_case "error formatting at end" `Quick error_formatting_at_end;
      ] );
  ]
