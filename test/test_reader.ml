(** Tests for CSS Reader module *)

open Alcotest
open Css.Reader

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

(* Test basic operations *)
let test_reader_basic () =
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
let test_reader_string () =
  (* peek_string *)
  let r = of_string "hello world" in
  Alcotest.(check string) "peek 5 chars" "hello" (peek_string r 5);
  Alcotest.(check string) "peek 0 chars" "" (peek_string r 0);

  (* looking_at *)
  check_looking_at "looking at hello" true "hello world" "hello";
  check_looking_at "not looking at world" false "hello world" "world";
  check_looking_at "looking at empty" true "anything" "";

  (* skip_n *)
  let r = of_string "hello" in
  skip_n r 3;
  Alcotest.(check (option char)) "after skip 3" (Some 'l') (peek r)

(* Test whitespace handling *)
let test_reader_whitespace () =
  (* Basic whitespace *)
  let r = of_string "  \t\n test" in
  skip_ws r;
  Alcotest.(check (option char)) "after ws" (Some 't') (peek r);

  (* No whitespace *)
  let r = of_string "test" in
  skip_ws r;
  Alcotest.(check (option char)) "no ws to skip" (Some 't') (peek r);

  (* Only whitespace *)
  let r = of_string "   \t\n  " in
  skip_ws r;
  Alcotest.(check bool) "all ws done" true (is_done r)

(* Test comment skipping *)
let test_reader_comments () =
  (* Single comment *)
  let r = of_string "/*comment*/x" in
  skip_ws r;
  Alcotest.(check (option char)) "after comment" (Some 'x') (peek r);

  (* Multiple comments *)
  let r = of_string "/*c1*/  /*c2*/y" in
  skip_ws r;
  Alcotest.(check (option char)) "after comments" (Some 'y') (peek r);

  (* Nested comments not supported - should stop at first closing *)
  let r = of_string "/*outer/*inner*/*/x" in
  skip_ws r;
  Alcotest.(check (option char)) "nested comment" (Some '*') (peek r)

(* Test take_while *)
let test_reader_take_while () =
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
let test_reader_expect () =
  (* Success case *)
  let r = of_string "test" in
  expect r 't';
  Alcotest.(check (option char)) "after expect" (Some 'e') (peek r);

  (* Failure case *)
  let r = of_string "test" in
  check_raises "expect wrong char"
    (Parse_error ("Expected 'x' but got 't'", r))
    (fun () -> expect r 'x')

(* Test expect_string *)
let test_reader_expect_string () =
  (* Success *)
  let r = of_string "hello world" in
  expect_string r "hello";
  Alcotest.(check (option char)) "after expect string" (Some ' ') (peek r);

  (* Failure *)
  let r = of_string "hello" in
  check_raises "expect wrong string"
    (Parse_error ("expected \"world\"", r))
    (fun () -> expect_string r "world")

(* Test between delimiters *)
let test_reader_between () =
  (* Parentheses *)
  let r = of_string "(content)" in
  let result = between r '(' ')' (fun r -> while_ r (fun c -> c != ')')) in
  Alcotest.(check string) "parens" "content" result;

  (* Brackets *)
  let r = of_string "[data]" in
  let result = between r '[' ']' (fun r -> while_ r (fun c -> c != ']')) in
  Alcotest.(check string) "brackets" "data" result;

  (* Braces *)
  let r = of_string "{block}" in
  let result = between r '{' '}' (fun r -> while_ r (fun c -> c != '}')) in
  Alcotest.(check string) "braces" "block" result;

  (* Nested *)
  let r = of_string "(outer(inner))" in
  let result =
    between r '(' ')' (fun r ->
        let rec read_until_close depth acc =
          if is_done r then acc
          else (
            save r;
            let c = char r in
            if c = '(' then read_until_close (depth + 1) (acc ^ "(")
            else if c = ')' then
              if depth = 0 then (
                (* Do not consume the closing ')'; restore to before it *)
                restore r;
                acc)
              else read_until_close (depth - 1) (acc ^ ")")
            else read_until_close depth (acc ^ String.make 1 c))
        in
        read_until_close 0 "")
  in
  Alcotest.(check string) "nested parens" "outer(inner)" result

(* Test save/restore *)
let test_reader_save_restore () =
  let r = of_string "test" in
  save r;
  ignore (char r);
  ignore (char r);
  Alcotest.(check (option char)) "moved forward" (Some 's') (peek r);
  restore r;
  Alcotest.(check (option char)) "restored" (Some 't') (peek r)

(* Test try_parse *)
let test_reader_try_parse () =
  (* Success *)
  let r = of_string "123" in
  let result =
    try_parse (fun r -> while_ r (fun c -> c >= '0' && c <= '9')) r
  in
  Alcotest.(check (option string)) "parsed digits" (Some "123") result;

  (* Failure - position restored *)
  let r = of_string "abc" in
  let result =
    try_parse
      (fun r ->
        expect r 'x';
        (* This will fail *)
        "never reached")
      r
  in
  Alcotest.(check (option string)) "parse failed" None result;
  Alcotest.(check (option char)) "position restored" (Some 'a') (peek r)

(* Test commit *)
let test_reader_commit () =
  let r = of_string "test" in
  save r;
  ignore (char r);
  commit r;
  (* After commit, restore should fail; position remains advanced *)
  check_raises "restore after commit"
    (Parse_error ("no saved position to restore", r))
    (fun () -> restore r);
  Alcotest.(check (option char)) "position after commit" (Some 'e') (peek r)

(* Test number parsing *)
let test_reader_numbers () =
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
  Alcotest.(check (float 0.001)) "negative float" (-2.5) n

(* Test unit parsing *)
let test_reader_units () =
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
let test_reader_ident () =
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
let test_reader_string_literals () =
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
let test_reader_until_string () =
  let r = of_string "data;more" in
  let data = until_string r ";" in
  Alcotest.(check string) "until semicolon" "data" data;

  let r = of_string "no delimiter here" in
  let all = until_string r "xyz" in
  Alcotest.(check string) "delimiter not found" "no delimiter here" all

(* Test hex colors *)
let test_reader_hex () =
  (* Hex colors are just parsed as strings starting with # *)
  let r = of_string "#abc" in
  expect r '#';
  let hex =
    while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  Alcotest.(check string) "3 digit hex" "abc" hex;

  let r = of_string "#123456" in
  expect r '#';
  let hex =
    while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  Alcotest.(check string) "6 digit hex" "123456" hex

(* Test identifier with escapes *)
let test_reader_ident_with_escapes () =
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
let test_reader_failures () =
  (* EOF in string *)
  let r = of_string "\"unclosed" in
  check_raises "unclosed string"
    (Parse_error ("unexpected end of input", r))
    (fun () -> ignore (string r));

  (* Invalid number *)
  let r = of_string "abc" in
  check_raises "not a number"
    (Parse_error ("invalid number", r))
    (fun () -> ignore (number r))

let suite =
  [
    ( "reader",
      [
        (* Basic operations *)
        test_case "basic" `Quick test_reader_basic;
        test_case "string" `Quick test_reader_string;
        test_case "whitespace" `Quick test_reader_whitespace;
        test_case "comments" `Quick test_reader_comments;
        (* Parsing helpers *)
        test_case "take while" `Quick test_reader_take_while;
        test_case "expect" `Quick test_reader_expect;
        test_case "expect string" `Quick test_reader_expect_string;
        test_case "between" `Quick test_reader_between;
        (* Backtracking *)
        test_case "save restore" `Quick test_reader_save_restore;
        test_case "try parse" `Quick test_reader_try_parse;
        test_case "commit" `Quick test_reader_commit;
        (* Value parsing *)
        test_case "numbers" `Quick test_reader_numbers;
        test_case "units" `Quick test_reader_units;
        test_case "ident" `Quick test_reader_ident;
        test_case "string literals" `Quick test_reader_string_literals;
        test_case "until string" `Quick test_reader_until_string;
        test_case "hex" `Quick test_reader_hex;
        (* Special cases *)
        test_case "ident with escapes" `Quick test_reader_ident_with_escapes;
        (* Error cases *)
        test_case "failures" `Quick test_reader_failures;
      ] );
  ]
