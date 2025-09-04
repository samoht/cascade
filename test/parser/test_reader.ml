open Alcotest
open Css_parser

let test_basic_operations () =
  let r = Reader.of_string "hello world" in
  check (option char) "peek" (Some 'h') (Reader.peek r);
  check bool "not done" false (Reader.is_done r);

  let c = Reader.char r in
  check char "char" 'h' c;
  check (option char) "peek after char" (Some 'e') (Reader.peek r);

  Reader.skip_n r 10;
  check bool "at end after skip" true (Reader.is_done r)

let test_string_operations () =
  let r = Reader.of_string "hello world" in
  let s = Reader.peek_string r 5 in
  check string "peek_string" "hello" s;

  check bool "looking_at hello" true (Reader.looking_at r "hello");
  check bool "not looking_at world" false (Reader.looking_at r "world");

  Reader.skip_n r 6;
  check bool "looking_at world after skip" true (Reader.looking_at r "world")

let test_whitespace () =
  let r = Reader.of_string "  \t\n test" in
  Reader.skip_ws r;
  check (option char) "after skip whitespace" (Some 't') (Reader.peek r)

let test_take_while () =
  let r = Reader.of_string "123abc" in
  let digits = Reader.while_ r (fun c -> c >= '0' && c <= '9') in
  check string "digits" "123" digits;

  let letters =
    Reader.while_ r (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
  in
  check string "letters" "abc" letters;

  check bool "at end" true (Reader.is_done r)

let test_expect () =
  let r = Reader.of_string "test" in
  Reader.expect r 't';
  check (option char) "after expect t" (Some 'e') (Reader.peek r);

  let expect_fail () = Reader.expect r 'x' in
  check_raises "expect wrong char"
    (Reader.Parse_error "Expected 'x' but got 'e'") expect_fail

let suite =
  [
    ( "reader",
      [
        test_case "basic_operations" `Quick test_basic_operations;
        test_case "string_operations" `Quick test_string_operations;
        test_case "whitespace" `Quick test_whitespace;
        test_case "take_while" `Quick test_take_while;
        test_case "expect" `Quick test_expect;
      ] );
  ]
