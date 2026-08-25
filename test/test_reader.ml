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
    (match skip r with exception Parse_error _ -> true | () -> false)

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

(* Helper to skip to a specific pattern in reader *)
let rec skip_to_pattern r pattern =
  if (not (is_done r)) && not (looking_at r pattern) then (
    skip r;
    skip_to_pattern r pattern)

let error_formatting_multiline () =
  (* Test error on multi-line CSS with previous line context *)
  let multiline_css =
    ".class1 {\n  color: red;\n}\n.class2 {\n  invalid-prop: value;\n}"
  in
  let r = of_string multiline_css in

  (* Navigate to the error position (around "invalid-prop") *)
  skip_to_pattern r "invalid";

  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.fail "expected a parse error"
  | exception Parse_error error ->
      (* Check that context includes previous line *)
      Alcotest.(check bool)
        "has newline in context" true
        (String.contains error.context_window '\n');
      (* Check that the context isn't too long (should be around 80 chars) *)
      Alcotest.(check bool)
        "context reasonable length" true
        (String.length error.context_window < 120);
      (* The location lives in [line] and [col]; [filename] names the source and
         carries no location, so it holds no separator. *)
      Alcotest.(check bool)
        "filename holds no location" false
        (String.contains error.filename ':');
      Alcotest.(check (pair int int))
        "location of the failing byte" (5, 3) (error.line, error.col)

let error_formatting_long_line () =
  (* Test error on very long single line (minified CSS) *)
  let long_css = String.make 200 'x' ^ "invalid" ^ String.make 200 'y' in
  let r = of_string long_css in

  (* Navigate to around the middle *)
  for _ = 1 to 205 do
    skip r
  done;

  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.fail "expected a parse error"
  | exception Parse_error error ->
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

  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.fail "expected a parse error"
  | exception Parse_error error ->
      (* Check that context is the entire short string *)
      Alcotest.(check string)
        "context is full short string" short_css error.context_window;
      (* Check marker position *)
      Alcotest.(check int) "marker at start" 0 error.marker_pos

let error_formatting_at_start () =
  (* Test error at very beginning of input *)
  let css = "invalid syntax here" in
  let r = of_string css in

  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.fail "expected a parse error"
  | exception Parse_error error ->
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

  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.fail "expected a parse error"
  | exception Parse_error error ->
      (* Should have context leading up to the end *)
      Alcotest.(check bool)
        "has leading context" true
        (String.length error.context_window > 0);
      (* Marker should be at end of context *)
      Alcotest.(check bool)
        "marker near end" true
        (error.marker_pos >= String.length error.context_window - 5)

(* Fail at byte [pos] of [input] and hand back the error record. *)
let error_at input pos =
  let r = of_string input in
  for _ = 1 to pos do
    skip r
  done;
  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.failf "expected a parse error at byte %d" pos
  | exception Parse_error error -> error

(* Lines and columns are 1-based and counted from the start of the input: the
   byte just past a newline opens the next line at column 1, and the position
   past the last byte of a line is one column beyond it. *)
let error_line_and_column () =
  let check name input pos expected =
    let error = error_at input pos in
    Alcotest.(check (pair int int)) name expected (error.line, error.col)
  in
  check "start of the second line" "abc\n" 4 (2, 1);
  check "start of the second line, longer first" "aaaaa\n" 6 (2, 1);
  check "start of the third line" "ab\ncd\n" 6 (3, 1);
  check "past the last character" "abc" 3 (1, 4)

(* A column is one Unicode scalar value, for the reported column and for the
   caret alike, so three euro signs put the error in column 4 with three
   characters before the caret rather than nine bytes. *)
let error_column_counts_characters () =
  let error = error_at (euros 3) 9 in
  Alcotest.(check (pair int int))
    "column counts characters" (1, 4) (error.line, error.col);
  Alcotest.(check int) "caret counts characters" 3 error.marker_pos

(* The caret of pure ASCII stays where it was, one column per byte. *)
let error_ascii_caret_holds () =
  let error = error_at (String.make 50 'a') 50 in
  Alcotest.(check (pair int int)) "ascii column" (1, 51) (error.line, error.col);
  Alcotest.(check int) "ascii caret" 40 error.marker_pos

(* A window boundary landing inside a UTF-8 sequence moves out to the lead byte,
   so the context a caret is drawn under is never a truncated code point. *)
let error_context_window_keeps_code_points () =
  let before = error_at (euros 40 ^ "xx") 122 in
  Alcotest.(check bool)
    "leading boundary keeps code points" true
    (is_utf8 before.context_window);
  let after = error_at ("x" ^ euros 40) 1 in
  Alcotest.(check bool)
    "trailing boundary keeps code points" true
    (is_utf8 after.context_window)

(* [filename] names the source and holds nothing else. A reader built by
   [of_string] has no name of its own, so it carries the placeholder, and
   [with_filename] swaps in the caller's name without disturbing the location in
   [line], [col] and [position]. *)
let error_filename_names_the_source () =
  let error = error_at "abc\ndef" 5 in
  Alcotest.(check string) "default source name" "<CSS input>" error.filename;
  Alcotest.(check (pair int int))
    "location of the failing byte" (2, 2) (error.line, error.col);
  let stamped = with_filename error "theme.css" in
  Alcotest.(check string) "stamped source name" "theme.css" stamped.filename;
  Alcotest.(check (pair int int))
    "stamping a name keeps the location" (2, 2)
    (stamped.line, stamped.col);
  Alcotest.(check int) "stamping a name keeps the offset" 5 stamped.position

(* The rendered location is the three-part [source:line:column] an editor can
   jump to. The byte offset stays in [position] for a caller that wants it, and
   is not a fourth part of the rendered location. *)
let error_renders_three_part_location () =
  let first_line error =
    List.hd (String.split_on_char '\n' (pp_parse_error error))
  in
  let error = error_at "abc\ndef" 5 in
  Alcotest.(check bool)
    "renders source:line:column" true
    (String.ends_with ~suffix:" at <CSS input>:2:2" (first_line error));
  Alcotest.(check bool)
    "renders the stamped source name" true
    (String.ends_with ~suffix:" at theme.css:2:2"
       (first_line (with_filename error "theme.css")))

let tests_call_stack () =
  callstack_case ();
  with_context_case ();
  with_context_exception ()

let tests_error_formatting () =
  error_formatting_multiline ();
  error_formatting_long_line ();
  error_formatting_short_input ();
  error_formatting_at_start ();
  error_formatting_at_end ()

let tests_error_position () =
  error_line_and_column ();
  error_column_counts_characters ();
  error_ascii_caret_holds ();
  error_context_window_keeps_code_points ();
  error_filename_names_the_source ();
  error_renders_three_part_location ()

let suite =
  ( "reader",
    [
      test_case "basic" `Quick basic;
      test_case "peeks" `Quick peeks;
      test_case "UTF-8 skip" `Quick skips_code_points;
      test_case "UTF-8 peek bounds" `Quick utf8_peek_bounds;
      test_case "call stack" `Quick tests_call_stack;
      test_case "error formatting" `Quick tests_error_formatting;
      test_case "error position" `Quick tests_error_position;
    ] )
