(** Tests for the CSS Reader: the character cursor {!Lexer} drives. *)

open Alcotest
open Cascade
open Reader

let check_peek name expected input =
  let r = of_string input in
  Alcotest.(check (option char)) name expected (peek r)

let check_looking_at name expected input pattern =
  let r = of_string input in
  Alcotest.(check bool) name expected (looking_at r pattern)

let basic () =
  check_peek "peek h" (Some 'h') "hello";
  check_peek "peek empty" None "";
  check_looking_at "prefix" true "hello" "he";
  check_looking_at "not a prefix" false "hello" "el";
  check_looking_at "past the end" false "he" "hello";
  let r = of_string "" in
  Alcotest.(check bool) "empty is done" true (is_done r);
  let r = of_string "x" in
  Alcotest.(check bool) "not done" false (is_done r);
  skip r;
  Alcotest.(check bool) "done after skip" true (is_done r);
  Alcotest.(check bool)
    "skipping past the end raises" true
    (match skip r with exception Invalid_argument _ -> true | () -> false)

let peeks () =
  let r = of_string "abc" in
  Alcotest.(check string) "source" "abc" (source r);
  Alcotest.(check bool) "default identifier rule" false (enforce_spec r);
  Alcotest.(check (option char)) "peek_at 0" (Some 'a') (peek_at r 0);
  Alcotest.(check (option char)) "peek_at 2" (Some 'c') (peek_at r 2);
  Alcotest.(check (option char)) "peek_at past end" None (peek_at r 3);
  Alcotest.(check (option char)) "peek_at negative" None (peek_at r (-1));
  Alcotest.(check int) "peek_byte" (Char.code 'a') (peek_byte r);
  Alcotest.(check int) "peek_byte_at 2" (Char.code 'c') (peek_byte_at r 2);
  Alcotest.(check int) "peek_byte_at past end" (-1) (peek_byte_at r 3);
  Alcotest.(check string) "peek_string" "ab" (peek_string r 2);
  Alcotest.(check string) "peek_string past end" "abc" (peek_string r 9);
  Alcotest.(check int) "peeking does not advance" 0 (position r)

(* U+20AC EURO SIGN, three UTF-8 bytes, spelled as escapes because cascade
   source stays 7-bit. *)
let euro = "\xe2\x82\xac"
let euros n = String.concat "" (List.init n (fun _ -> euro))

(* A local UTF-8 validator, so the oracle does not run through the decoder under
   test. *)
let is_utf8 s =
  let len = String.length s in
  let rec go i =
    if i >= len then true
    else
      let b = Char.code s.[i] in
      let n =
        if b < 0x80 then 1
        else if b land 0xe0 = 0xc0 then 2
        else if b land 0xf0 = 0xe0 then 3
        else if b land 0xf8 = 0xf0 then 4
        else 0
      in
      if n = 0 || i + n > len then false
      else
        let rec continuations k =
          k = n
          || (Char.code s.[i + k] land 0xc0 = 0x80 && continuations (k + 1))
        in
        continuations 1 && go (i + n)
  in
  go 0

let skips_code_points () =
  let r = of_string (euro ^ "z") in
  skip_utf8 r;
  Alcotest.(check int) "skipped the whole code point" 3 (position r);
  skip_utf8 r;
  Alcotest.(check bool) "done" true (is_done r);
  let r = of_string "\xe2z" in
  skip_utf8 r;
  Alcotest.(check int) "a malformed lead byte advances one byte" 1 (position r)

let utf8_peek_bounds () =
  let r = of_string "ab" in
  skip r;
  let position = position r in
  Alcotest.(check (option (pair int int)))
    "negative offset" None (peek_utf8_at r (-1));
  Alcotest.(check (option (pair int int)))
    "past end" None (peek_utf8_at r max_int);
  Alcotest.(check (option (pair int int)))
    "current byte"
    (Some (Char.code 'b', 1))
    (peek_utf8_at r 0);
  Alcotest.(check int) "does not advance" position (Reader.position r)

(* Helper to skip to a specific pattern in reader *)
let rec skip_to_pattern r pattern =
  if (not (is_done r)) && not (looking_at r pattern) then (
    skip r;
    skip_to_pattern r pattern)

let context_window_multiline () =
  (* The window around a position on a later line reaches the line before. *)
  let multiline_css =
    ".class1 {\n  color: red;\n}\n.class2 {\n  invalid-prop: value;\n}"
  in
  let r = of_string multiline_css in
  skip_to_pattern r "invalid";
  let context, _ = context_window r in
  Alcotest.(check bool)
    "has newline in context" true
    (String.contains context '\n');
  Alcotest.(check bool)
    "context reasonable length" true
    (String.length context < 120)

let context_window_long_line () =
  (* A window in the middle of a long line is about 80 characters, with the
     marker near its middle. *)
  let long_css = String.make 200 'x' ^ "invalid" ^ String.make 200 'y' in
  let r = of_string long_css in
  for _ = 1 to 205 do
    skip r
  done;
  let context, marker_pos = context_window r in
  let context_len = String.length context in
  Alcotest.(check bool)
    "context around 80 chars" true
    (context_len >= 70 && context_len <= 90);
  Alcotest.(check bool)
    "marker near middle" true
    (marker_pos >= 30 && marker_pos <= 50);
  Alcotest.(check bool)
    "no newlines in single line" false
    (String.contains context '\n')

let context_window_short_input () =
  let short_css = "ab" in
  let context, marker_pos = context_window (of_string short_css) in
  Alcotest.(check string) "context is full short string" short_css context;
  Alcotest.(check int) "marker at start" 0 marker_pos

let context_window_at_start () =
  let context, marker_pos = context_window (of_string "invalid syntax here") in
  Alcotest.(check int) "marker at start" 0 marker_pos;
  Alcotest.(check bool) "has some context" true (String.length context > 0)

let context_window_at_end () =
  let r = of_string "some valid css syntax" in
  while not (is_done r) do
    skip r
  done;
  let context, marker_pos = context_window r in
  Alcotest.(check bool) "has leading context" true (String.length context > 0);
  Alcotest.(check bool)
    "marker near end" true
    (marker_pos >= String.length context - 5)

(* The context window at byte [pos] of [input]. *)
let context_at input pos =
  let r = of_string input in
  for _ = 1 to pos do
    skip r
  done;
  context_window r

(* A marker counts Unicode scalar values, so three euro signs put three
   characters before it rather than nine bytes; pure ASCII stays one per
   byte. *)
let context_window_counts_characters () =
  Alcotest.(check int)
    "marker counts characters" 3
    (snd (context_at (euros 3) 9));
  Alcotest.(check int)
    "ascii marker" 40
    (snd (context_at (String.make 50 'a') 50))

(* A window boundary landing inside a UTF-8 sequence moves out to the lead byte,
   so the context a caret is drawn under is never a truncated code point. *)
let context_window_keeps_code_points () =
  let before, _ = context_at (euros 40 ^ "xx") 122 in
  Alcotest.(check bool)
    "leading boundary keeps code points" true (is_utf8 before);
  let after, _ = context_at ("x" ^ euros 40) 1 in
  Alcotest.(check bool)
    "trailing boundary keeps code points" true (is_utf8 after)

let tests_context_window () =
  context_window_multiline ();
  context_window_long_line ();
  context_window_short_input ();
  context_window_at_start ();
  context_window_at_end ();
  context_window_counts_characters ();
  context_window_keeps_code_points ()

(* The 1.2 error API, kept until 1.4.0 with its 1.2 behaviour: the expectations
   are what 1.2.1 raised and printed. *)
module Deprecated = struct
  [@@@alert "-deprecated"]

  let call_stack () =
    let r = of_string "test" in
    Alcotest.(check (list string)) "initially empty" [] (callstack r);
    push_context r "outer";
    push_context r "inner";
    Alcotest.(check (list string))
      "outermost first" [ "outer"; "inner" ] (callstack r);
    pop_context r;
    pop_context r;
    pop_context r;
    Alcotest.(check (list string)) "popping an empty stack" [] (callstack r);
    let seen = with_context r "ctx" (fun () -> callstack r) in
    Alcotest.(check (list string)) "pushed while running" [ "ctx" ] seen;
    Alcotest.(check (list string)) "popped on return" [] (callstack r);
    (match with_context r "ctx" (fun () -> failwith "boom") with
    | () -> Alcotest.fail "with_context swallowed the exception"
    | exception Failure _ -> ());
    Alcotest.(check (list string)) "popped on raise" [] (callstack r)

  (* The error [f] raises on a reader over [input] at byte [pos]. *)
  let raised ?(pos = 0) input f =
    let r = of_string input in
    for _ = 1 to pos do
      skip r
    done;
    match f r with
    | (_ : unit) -> Alcotest.fail "expected Reader.Parse_error"
    | exception Parse_error e -> e

  let messages () =
    let message f = (raised "abc" f).message in
    Alcotest.(check string) "err" "boom" (message (fun r -> err r "boom"));
    Alcotest.(check string)
      "err_eof" "unexpected end of input" (message err_eof);
    Alcotest.(check string)
      "err_expected" "expected ';'"
      (message (fun r -> err_expected r "';'"));
    Alcotest.(check string)
      "err_expected_but_eof" "Expected '}' but reached end of input"
      (message (fun r -> err_expected_but_eof r "'}'"));
    Alcotest.(check string)
      "err_invalid" "invalid colour"
      (message (fun r -> err_invalid r "colour"));
    Alcotest.(check (option string))
      "got" (Some "x") (raised "abc" (fun r -> err ~got:"x" r "boom")).got;
    Alcotest.(check (option string)) "no got" None (raised "abc" err_eof).got

  (* Lines and columns are 1-based, a column is one Unicode scalar value, and
     [position] stays a byte offset. *)
  let location () =
    let e = raised ~pos:5 "abc\ndef" err_eof in
    Alcotest.(check (pair int int)) "line and column" (2, 2) (e.line, e.col);
    Alcotest.(check int) "byte offset" 5 e.position;
    Alcotest.(check string) "default source name" "<CSS input>" e.filename;
    let e = raised ~pos:9 (euros 3) err_eof in
    Alcotest.(check (pair int int))
      "column counts characters" (1, 4) (e.line, e.col);
    Alcotest.(check int) "caret counts characters" 3 e.marker_pos;
    let stamped = with_filename e "theme.css" in
    Alcotest.(check string) "stamped source name" "theme.css" stamped.filename;
    Alcotest.(check (pair int int))
      "stamping keeps the location" (1, 4)
      (stamped.line, stamped.col)

  let call_stack_in_error () =
    let e =
      raised "abc" (fun r ->
          with_context r "rule" (fun () ->
              with_context r "value" (fun () -> err_eof r)))
    in
    Alcotest.(check (list string))
      "error carries the stack" [ "rule"; "value" ] e.callstack

  (* The 1.2 exception is its own: the readers' [Error.Parse_error] is another,
     and [Error.catch] lets this one through. *)
  let distinct_exception () =
    match Error.catch (fun () -> err_eof (of_string "abc")) with
    | Ok () | Error _ -> Alcotest.fail "Error.catch caught Reader.Parse_error"
    | exception Parse_error _ -> ()

  let rendering () =
    Alcotest.(check string)
      "caret below the marked line"
      (String.concat ""
         [
           "boom at <CSS input>:2:3\n"; "first\n"; "second\n"; "  ^\n"; "third";
         ])
      (pp_parse_error
         (raised ~pos:8 "first\nsecond\nthird" (fun r -> err r "boom")));
    Alcotest.(check string)
      "caret at the start, stamped name" "boom at theme.css:1:1\nabc\n^"
      (pp_parse_error
         (with_filename (raised "abc" (fun r -> err r "boom")) "theme.css"));
    Alcotest.(check string)
      "call stack line"
      "boom at <CSS input>:1:1\n    [stack: rule -> value]\nab\n^"
      (pp_parse_error
         (raised "ab" (fun r ->
              with_context r "rule" (fun () ->
                  with_context r "value" (fun () -> err r "boom")))))

  let tests () =
    call_stack ();
    messages ();
    location ();
    call_stack_in_error ();
    distinct_exception ();
    rendering ()
end

let suite =
  ( "reader",
    [
      test_case "basic" `Quick basic;
      test_case "peeks" `Quick peeks;
      test_case "UTF-8 skip" `Quick skips_code_points;
      test_case "UTF-8 peek bounds" `Quick utf8_peek_bounds;
      test_case "context window" `Quick tests_context_window;
      test_case "1.2 error API" `Quick Deprecated.tests;
    ] )
