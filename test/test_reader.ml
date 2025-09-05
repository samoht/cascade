open Alcotest
open Css

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
    (Reader.Parse_error ("Expected 'x' but got 'e'", r))
    expect_fail

let test_expect_string_and_between () =
  let r = Reader.of_string "(a[b]{c})" in
  (* between/parens/brackets/braces should round-trip and consume correctly *)
  let par =
    Reader.parens r (fun r ->
        let content = Reader.until r ')' in
        String.trim content)
  in
  check string "parens" "a[b]{c}" par;
  (* parens already consumed the closing ')', so we should be done *)
  check bool "done?" true (Reader.is_done r)

let test_save_restore_try_parse () =
  let r = Reader.of_string "foo" in
  Reader.save r;
  ignore (Reader.try_parse (fun r -> Reader.expect_string r "foo") r);
  (* committed? we didn't commit, so restore state manually and check still at
     0 *)
  Reader.restore r;
  check (option char) "back to start" (Some 'f') (Reader.peek r);
  (* Now try a failing parse and ensure try_parse returns None and position
     unchanged *)
  let before = Reader.peek_string r 3 in
  let res = Reader.try_parse (fun r -> Reader.expect_string r "bar") r in
  check bool "None on failure" true (Option.is_none res);
  let after = Reader.peek_string r 3 in
  check string "position unchanged" before after

let test_numbers_and_units () =
  (* number, int, percentage, dimension/angle/duration *)
  let r = Reader.of_string "-12.5 42 33% 1.5rem 90deg 250ms" in
  let n = Reader.number r in
  Alcotest.(check (float 0.0001)) "number" (-12.5) n;
  Reader.ws r;
  let i = Reader.int r in
  check int "int" 42 i;
  Reader.ws r;
  let pct = Reader.percentage r in
  Alcotest.(check (float 0.0001)) "percentage" 33. pct;
  Reader.ws r;
  let d1, u1 = Reader.dimension r in
  Alcotest.(check (float 0.0001)) "dimension value" 1.5 d1;
  check string "dimension unit" "rem" u1;
  Reader.ws r;
  let ang, au = Reader.angle r in
  Alcotest.(check (float 0.0001)) "angle value" 90. ang;
  check string "angle unit" "deg" au;
  Reader.ws r;
  let dur, du = Reader.duration r in
  Alcotest.(check (float 0.0001)) "duration value" 250. dur;
  check string "duration unit" "ms" du

let test_hex_and_colors () =
  (* hex_color requires 3 or 6 hex digits *)
  let ok3 = Reader.(hex_color (of_string "abc")) in
  check string "hex 3" "abc" ok3;
  let ok6 = Reader.(hex_color (of_string "a1b2c3")) in
  check string "hex 6" "a1b2c3" ok6;
  let bad_hex () =
    let r = Reader.of_string "ab" in
    ignore (Reader.hex_color r)
  in
  (try
     bad_hex ();
     fail "Expected Parse_error exception"
   with
  | Reader.Parse_error ("invalid hex color (must be 3 or 6 digits)", _) -> ()
  | _ -> fail "Expected Parse_error with correct message");
  (* color_keyword succeeds for known names, resets on unknown *)
  let r = Reader.of_string "red" in
  (match Reader.color_keyword r with Some "red" -> () | _ -> fail "red");
  let r = Reader.of_string "unknown" in
  match Reader.color_keyword r with
  | None -> ()
  | _ -> fail "unknown should be None"

let test_rgb_function () =
  let ok = Reader.(rgb_function (of_string "rgba(255, 0, 10, 0.5)")) in
  (match ok with
  | Some (255, 0, 10, Some a) -> Alcotest.(check (float 0.0001)) "alpha" 0.5 a
  | _ -> fail "expected rgba tuple");
  let ok2 = Reader.(rgb_function (of_string "rgb(10,20,30)")) in
  (match ok2 with Some (10, 20, 30, None) -> () | _ -> fail "rgb tuple");
  let none = Reader.(rgb_function (of_string "hsl(10,10%,10%)")) in
  check bool "not rgb" true (Option.is_none none)

let suite =
  [
    ( "reader",
      [
        test_case "basic_operations" `Quick test_basic_operations;
        test_case "string_operations" `Quick test_string_operations;
        test_case "whitespace" `Quick test_whitespace;
        test_case "take_while" `Quick test_take_while;
        test_case "expect" `Quick test_expect;
        test_case "expect_string_and_between" `Quick
          test_expect_string_and_between;
        test_case "save_restore_try_parse" `Quick test_save_restore_try_parse;
        test_case "numbers_and_units" `Quick test_numbers_and_units;
        test_case "hex_and_colors" `Quick test_hex_and_colors;
        test_case "rgb_function" `Quick test_rgb_function;
      ] );
  ]
