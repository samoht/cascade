(** Tests for CSS Reader module *)

open Alcotest
open Css.Reader
open Test_helpers

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

(* One-liner check function for identifiers *)
let check_ident = check_read "ident" ident

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

(* Uses Test_helpers for check_parse_error_fields and check_raises *)

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
        if char r = 'x' then "success" else err r "expected x, got e")
      r
  in
  Alcotest.(check (option string)) "option failed" None result;
  (* Position should be restored *)
  Alcotest.(check (option char)) "position restored" (Some 't') (peek r)

(* Comprehensive backtracking tests for all combinators *)
let backtrack_pair () =
  (* Test pair backtracking when first parser succeeds but second fails *)
  let r = of_string "hello world" in
  let initial_pos = position r in

  (* Try a parser that should fail: first succeeds (consumes "hello"), second
     fails *)
  try
    let _ =
      pair
        ~sep:(fun r -> ws r) (* whitespace separator *)
        (fun r -> while_ r (fun c -> c >= 'a' && c <= 'z'))
          (* succeeds: "hello" *)
        (fun r ->
          let digits = while_ r (fun c -> c >= '0' && c <= '9') in
          if digits = "" then err r "no digits found" else digits)
          (* fails: "world" is not digits *)
        r
    in
    Alcotest.fail "pair should have failed"
  with Parse_error _ ->
    (* Position should be restored to initial position *)
    let final_pos = position r in
    Alcotest.(check int)
      "pair restores position on failure" initial_pos final_pos;
    Alcotest.(check (option char))
      "pair restores buffer position" (Some 'h') (peek r)

let backtrack_triple () =
  (* Test triple backtracking when second parser fails *)
  let r = of_string "abc 123 def" in
  let initial_pos = position r in

  try
    let _ =
      triple
        ~sep:(fun r -> ws r)
        (fun r -> while_ r (fun c -> c >= 'a' && c <= 'z'))
          (* succeeds: "abc" *)
        (fun r ->
          let letters = while_ r (fun c -> c >= 'a' && c <= 'z') in
          if letters = "" then err r "no letters found" else letters)
          (* fails: "123" is not letters *)
        (fun r -> while_ r (fun c -> c >= 'a' && c <= 'z'))
          (* would succeed: "def" *)
        r
    in
    Alcotest.fail "triple should have failed"
  with Parse_error _ ->
    let final_pos = position r in
    Alcotest.(check int)
      "triple restores position on failure" initial_pos final_pos;
    Alcotest.(check (option char))
      "triple restores buffer position" (Some 'a') (peek r)

let backtrack_list () =
  (* Test list backtracking - this one is trickier since list might partially
     succeed *)
  let r = of_string "a,b,123" in

  (* Parse a list that should partially succeed then fail *)
  let result =
    try
      list
        ~sep:(fun r -> expect ',' r)
        ~at_least:3
        (fun r ->
          let c = char r in
          if c >= 'a' && c <= 'z' then String.make 1 c
          else failwith "not a letter")
        r
    with Parse_error _ | Failure _ -> []
  in

  (* List should have failed because third item "123" is not a letter *)
  (* But behavior depends on at_least constraint - let's check it doesn't leave partial state *)
  Alcotest.(check bool)
    "list handles failure gracefully" true
    (List.length result < 3)
(* Should not have parsed all 3 required items *)

let backtrack_one_of () =
  (* Test that one_of properly backtracks for each failed parser *)
  let r = of_string "hello123" in

  let result =
    one_of
      [
        (* Parser 1: tries to parse digits, fails on "hello" *)
        (fun r ->
          let digits = while_ r (fun c -> c >= '0' && c <= '9') in
          if digits = "" then err r "no digits" else digits);
        (* Parser 2: tries to parse letters then digits, partially succeeds then
           fails *)
        (fun r ->
          let letters = while_ r (fun c -> c >= 'a' && c <= 'z') in
          (* succeeds: "hello" *)
          expect '!' r;
          (* fails: next char is '1', not '!' *)
          letters);
        (* Parser 3: should succeed - parses letters only *)
        (fun r -> while_ r (fun c -> c >= 'a' && c <= 'z'));
      ]
      r
  in

  (* Should succeed with parser 3 *)
  Alcotest.(check string) "one_of eventually succeeds" "hello" result;

  (* Position should be after "hello" *)
  Alcotest.(check (option char))
    "one_of final position correct" (Some '1') (peek r)

let backtrack_enum_with_default () =
  (* Test enum with default function backtracking *)
  let r = of_string "unknown123" in

  (* enum should fail and not leave position in middle of "unknown" *)
  (* This should raise an exception, let's catch it *)
  try
    let result =
      enum "test-enum"
        [ ("known", 1); ("valid", 2) ]
        ~default:(fun r ->
          let text = while_ r (fun c -> c >= 'a' && c <= 'z') in
          (* consumes "unknown" *)
          if text = "special" then 99 else err r "not special") (* fails *)
        r
    in
    ignore result;
    Alcotest.fail "enum should have failed"
  with Parse_error _ ->
    (* Position should be restored to beginning when enum completely fails *)
    let final_pos = position r in
    Alcotest.(check int)
      "enum with default restores position on failure" 0 final_pos

let backtrack_complex_nested () =
  (* Test complex nested combinator backtracking like in gradient parsing *)
  let r = of_string "var(--color) var(--size)" in
  let initial_pos = position r in

  (* Simulate the gradient parsing that was failing *)
  let parse_var_name r =
    expect_string "var(" r;
    let name = while_ r (fun c -> c <> ')') in
    expect ')' r;
    name
  in

  let parse_number r =
    let num_str = while_ r (fun c -> c >= '0' && c <= '9') in
    if num_str = "" then err r "no number" else int_of_string num_str
  in

  (* Try to parse as (var, number) pair - should fail *)
  try
    let _ = pair ~sep:(fun r -> ws r) parse_var_name parse_number r in
    Alcotest.fail "complex nested should have failed"
  with Parse_error _ ->
    let final_pos = position r in
    Alcotest.(check int)
      "complex nested restores position" initial_pos final_pos;
    Alcotest.(check (option char))
      "complex nested buffer restored" (Some 'v') (peek r)

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
  check_ident "hello";
  check_ident "my-class";
  check_ident "my_var";
  check_ident "h1";

  (* CSS spec-compliant dash prefixes - VALID cases *)
  check_ident "-webkit-transform";
  check_ident "-moz-border-radius";
  check_ident "-ms-filter";
  check_ident "-webkit";
  check_ident "-_foo";
  check_ident "--my-variable";
  check_ident "--primary-color";

  (* Additional valid edge cases *)
  check_ident "-a-b";
  check_ident "-_-foo";

  (* CSS spec-compliant dash prefixes - INVALID cases *)
  neg ident "--";
  (* Double dash alone *)
  neg ident "-";
  (* Single dash alone *)
  neg ident "---webkit";
  (* Triple dash - questionable identifier *)
  neg ident "-2s";
  neg ident "-123abc";
  neg ident "-0webkit";
  neg ident "-9px";
  neg ident "-1";
  neg ident "-2px";
  neg ident "-3em";
  neg ident "-4rem";
  neg ident "-0a";
  neg ident "-1a";
  neg ident "-9z"

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

(* Additional identifier edge cases *)
let ident_more_edges () =
  (* Escaped leading digit should form a valid ident starting with digit *)
  let r = of_string "\\31 abc" in
  Alcotest.(check string) "escape-leading-digit" "1abc" (ident r);

  (* Hyphen + escaped digit is allowed, unlike plain hyphen+digit *)
  let r = of_string "-\\31 a" in
  Alcotest.(check string) "hyphen+escaped-digit" "-1a" (ident r);

  (* Trailing hyphen is valid and preserved *)
  let r = of_string "foo-" in
  Alcotest.(check string) "trailing-hyphen" "foo-" (ident r);

  (* Keep-case interactions with escapes *)
  let r = of_string "AbC-DeF" in
  Alcotest.(check string) "default-lowercases" "abc-def" (ident r);
  let r = of_string "AbC-DeF" in
  Alcotest.(check string)
    "keep-case-preserved" "AbC-DeF" (ident ~keep_case:true r);

  (* Escaped uppercase stays uppercase; surrounding chars lowercase without
     keep_case *)
  let r = of_string "A\\42 C" in
  Alcotest.(check string) "escape-preserves-case" "aBc" (ident r);
  let r = of_string "A\\42 C" in
  Alcotest.(check string) "escape-keep-case" "ABC" (ident ~keep_case:true r);

  (* Unicode escape without space is valid only if next char isn't hex *)
  let r = of_string "\\0041Z" in
  Alcotest.(check string) "unicode-no-space" "Az" (ident r);

  (* Unterminated escape should error with EOF *)
  let r = of_string "foo\\" in
  check_raises "unterminated-escape"
    (parse_error_expected "unexpected end of input" r) (fun () ->
      ignore (ident r))

(* enum_or_calls behavior and backtracking tests *)
let enum_or_calls_behavior () =
  (* Helper: parse rgb(...) function returning a string *)
  let parse_rgb_call t =
    call "rgb" t (fun t ->
        let a = int_of_float (number t) in
        comma t;
        let b = int_of_float (number t) in
        comma t;
        let c = int_of_float (number t) in
        Fmt.str "rgb(%d,%d,%d)" a b c)
  in

  let parser t =
    enum_or_calls "color"
      [ ("red", "red"); ("rgb", "rgb-ident") ]
      ~calls:[ ("rgb", parse_rgb_call) ]
      t
  in

  (* Ident match when bare name provided *)
  let r = of_string "rgb" in
  Alcotest.(check string) "ident-wins-for-bare-name" "rgb-ident" (parser r);

  (* When '(' immediately follows, prefer the call even if ident exists *)
  let r = of_string "rgb(1,2,3)" in
  Alcotest.(check string) "call-when-parens" "rgb(1,2,3)" (parser r);

  (* Space before '(' is not an immediate call; choose ident branch *)
  let r = of_string "rgb (1,2,3)" in
  Alcotest.(check string) "space-before-paren-uses-ident" "rgb-ident" (parser r);
  Alcotest.(check (option char))
    "rest-unconsumed-after-ident" (Some ' ') (peek r);

  (* Backtracking from failing calls to other parsers using one_of *)
  let r = of_string "red" in
  let result =
    one_of
      [
        (fun t ->
          (* This branch expects an rgb() call and should fail on "red" *)
          enum_calls [ ("rgb", parse_rgb_call) ] t);
        (fun t -> enum "color" [ ("red", "red"); ("blue", "blue") ] t);
      ]
      r
  in
  Alcotest.(check string) "backtrack-from-call-to-ident" "red" result;

  (* Parser without overlapping ident: calls are used when present *)
  let parser_calls t =
    enum_or_calls "color"
      [ ("red", "red") ]
      ~calls:[ ("rgb", parse_rgb_call) ]
      t
  in
  let r = of_string "rgb(1,2,3)" in
  Alcotest.(check string)
    "call-used-when-no-ident" "rgb(1,2,3)" (parser_calls r);

  (* Bare function name without parens with no ident mapping: treated as ident;
     error against idents set *)
  let r = of_string "rgb" in
  check_raises "bare-function-name-error"
    (parse_error_expected "color: expected one of: red, got: rgb" r) (fun () ->
      ignore (parser_calls r));

  (* Error when only calls provided and unknown name *)
  let r = of_string "abc" in
  check_raises "no-matching-function"
    (parse_error_expected "expected one of functions: rgb" r) (fun () ->
      ignore (enum_calls [ ("rgb", parse_rgb_call) ] r))

(* var() via enum_or_calls: ensures call branch consumes correctly and makes
   progress *)
let var_via_enum_or_calls () =
  let parse_fallback t =
    comma t;
    ws t;
    if peek t = Some ')' then "empty" else "fb:" ^ ident t
  in

  let parse_var_content t =
    ws t;
    expect_string "--" t;
    let name = ident ~keep_case:true t in
    ws t;
    let tag =
      match option parse_fallback t with Some t -> t | None -> "none"
    in
    "var:" ^ name ^ ":" ^ tag
  in

  let parse_var_simple t = call "var" t parse_var_content in

  let parse_value t =
    enum_or_calls "value" [] ~calls:[ ("var", parse_var_simple) ] t
  in

  let r = of_string "var(--X)" in
  Alcotest.(check string) "var-none-fallback" "var:X:none" (parse_value r);

  let r = of_string "var(--X,)" in
  Alcotest.(check string) "var-empty-fallback" "var:X:empty" (parse_value r);

  let r = of_string "var(--X, y)" in
  Alcotest.(check string) "var-ident-fallback" "var:X:fb:y" (parse_value r);

  (* Concatenated var()s should make progress and parse all *)
  let r = of_string "var(--a,)var(--b,)" in
  let v1 = parse_value r in
  let v2 = parse_value r in
  Alcotest.(check string) "concat-var-1" "var:a:empty" v1;
  Alcotest.(check string) "concat-var-2" "var:b:empty" v2;
  Alcotest.(check bool) "concat-done" true (is_done r)

(* list_impl edge cases *)
let list_edges () =
  (* Trailing comma: current behavior returns parsed items so far *)
  let r = of_string "a," in
  let items = list ~sep:comma ~at_least:1 ident r in
  Alcotest.(check (list string)) "trailing-comma-returns-items" [ "a" ] items;

  (* at_most enforcement with separators *)
  let r = of_string "a,b,c" in
  check_raises "list-at-most"
    (parse_error_expected "too many values (maximum 2 allowed)" r) (fun () ->
      ignore (list ~sep:comma ~at_most:2 ident r));

  (* No-progress guard for list items *)
  let r = of_string "abc" in
  let zero_progress _ = "x" in
  check_raises "list-no-progress"
    (parse_error_expected "parser made no progress in list" r) (fun () ->
      ignore (list zero_progress r))

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

(* Helper to parse specific keywords *)
let parse_keyword kw r =
  let id = ident r in
  if id = kw then id else err ~got:id r kw

(* Helper to parse number as string *)
let number_as_string r =
  try
    let n = number r in
    (* Use proper integer formatting for whole numbers *)
    if Float.is_integer n then string_of_int (int_of_float n)
    else string_of_float n
  with Parse_error _ ->
    let found = ident r in
    err ~got:found r "number"

(* Test one_of helper *)
let one_of_case () =
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

  (* Test backtracking with partial consumption *)
  let partial_parser r =
    let _ = ident r in
    (* Consumes first identifier *)
    ws r;
    (* Skips whitespace *)
    let _ = number r in
    (* Tries to parse number - will fail on "blue" *)
    "ident_number"
  in
  let single_ident_parser r =
    let first = ident r in
    first
  in

  let r = of_string "red blue" in
  let result = one_of [ partial_parser; single_ident_parser ] r in
  (* Should parse as just "red" and leave " blue" unconsumed *)
  Alcotest.(check string) "backtracking result" "red" result;
  Alcotest.(check bool) "should not consume entire input" false (is_done r);

  (* Test recursive one_of calls *)
  let complex_parser1 r =
    let _ = Css.Values.read_color r in
    ws r;
    let _ =
      one_of
        [
          (fun r ->
            let _ = Css.Values.read_length r in
            "length");
          (fun r ->
            let _ = Css.Values.read_percentage r in
            "percentage");
        ]
        r
    in
    "color_complex"
  in
  let simple_color_parser r =
    let _ = Css.Values.read_color r in
    "simple_color"
  in

  let r = of_string "red blue" in
  let result = one_of [ complex_parser1; simple_color_parser ] r in
  (* The complex parser should fail because "g" can't be parsed as length or
     percentage *)
  Alcotest.(check string) "recursive one_of result" "simple_color" result;
  Alcotest.(check bool)
    "recursive should not consume entire input" false (is_done r);

  (* No match - should raise with descriptive error *)
  let r = of_string "yellow" in
  check_raises "no match"
    (parse_error_expected ~got:(Some "yellow")
       "expected one of: red, blue, green, number" r) (fun () ->
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

let enum_with_default_ident () =
  let r = of_string "bold" in
  let result =
    enum "test"
      [ ("normal", "n"); ("bold", "b") ]
      r
      ~default:(fun _ -> "default")
  in
  Alcotest.(check string) "enum bold" "b" result

let enum_with_default_unknown () =
  let r = of_string "unknown" in
  let result =
    enum "test"
      [ ("normal", "n"); ("bold", "b") ]
      r
      ~default:(fun _ -> "default")
  in
  Alcotest.(check string) "enum unknown uses default" "default" result

let enum_with_default_numeric () =
  let r = of_string "123" in
  let result =
    enum "font-weight"
      [ ("normal", 0); ("bold", 700) ]
      r
      ~default:(fun t -> int_of_float (number t))
  in
  Alcotest.(check int) "enum numeric with default" 123 result

let enum_with_default_float () =
  let r = of_string "1.5" in
  let result =
    enum "line-height" [ ("normal", 0.0) ] r ~default:(fun t -> number t)
  in
  check (float 0.01) "enum float with default" 1.5 result

let enum_default_number_first () =
  let r = of_string "400" in
  let result =
    enum "font-weight"
      [ ("normal", 0); ("bold", 700) ]
      r
      ~default:(fun t -> int_of_float (number t))
  in
  Alcotest.(check int) "enum calls default first for number" 400 result

let enum_with_default_matching_identifier () =
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

(* Helper to read a var() function for testing *)
let read_var_function_for_test r vars_found =
  expect_string "var" r;
  expect '(' r;
  ws r;
  expect_string "--" r;
  let name = ident ~keep_case:true r in
  ws r;
  if peek r = Some ',' then (
    comma r;
    ws r);
  expect ')' r;
  vars_found := name :: !vars_found;
  `Var name

(* Helper to parse concatenated var functions from input *)
let parse_concatenated_vars input =
  let r = of_string input in
  let vars_found = ref [] in
  let rec loop acc =
    ws r;
    if is_done r then List.rev acc
    else if looking_at r "var(" then
      let v = read_var_function_for_test r vars_found in
      loop (v :: acc)
    else List.rev acc
  in
  let tokens = loop [] in
  (List.rev !vars_found, tokens)

(* Helper to test concatenated var functions *)
let test_concatenated_vars input expected_vars desc =
  let found, tokens = parse_concatenated_vars input in
  Alcotest.(check (list string)) (desc ^ " - var names") expected_vars found;
  Alcotest.(check int)
    (desc ^ " - token count")
    (List.length expected_vars)
    (List.length tokens)

let concatenated_var_functions () =
  (* Test that the CSS parser can handle concatenated var() functions. The
     handler must actually consume the input to avoid infinite loops. *)

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

let parse_fold_component r =
  one_of
    [
      (fun r -> enum "inner1" [ ("valid1", 1) ] r);
      (fun r -> enum "inner2" [ ("valid2", 2) ] r);
    ]
    r

let fold_many_with_enum_context () =
  (* Test that fold_many inside enum preserves both contexts *)
  let r = of_string "invalid" in

  (* This simulates what happens with border parsing *)
  let parser r =
    enum "outer-enum" []
      ~default:(fun r ->
        let acc, _ =
          Css.Reader.fold_many parse_fold_component ~init:[]
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

(* Helper to skip to a specific pattern in reader *)
let rec skip_to_pattern r pattern =
  if (not (is_done r)) && not (looking_at r pattern) then (
    skip r;
    skip_to_pattern r pattern)

(* Test error message formatting for different input types *)
let error_formatting_multiline () =
  (* Test error on multi-line CSS with previous line context *)
  let multiline_css =
    ".class1 {\n  color: red;\n}\n.class2 {\n  invalid-prop: value;\n}"
  in
  let r = of_string multiline_css in

  (* Navigate to the error position (around "invalid-prop") *)
  skip_to_pattern r "invalid";

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

(* Grouped test runners by feature for simpler suite entries *)

let tests_backtracking () =
  backtrack ();
  backtrack_pair ();
  backtrack_triple ();
  backtrack_enum_with_default ();
  backtrack_complex_nested ()

let tests_idents () =
  ident_case ();
  ident_with_escapes ();
  ident_more_edges ()

let tests_enums () =
  enum_case ();
  enum_with_default_ident ();
  enum_with_default_unknown ();
  enum_with_default_numeric ();
  enum_with_default_float ();
  enum_default_number_first ();
  enum_with_default_matching_identifier ()

let tests_enum_or_calls () =
  enum_or_calls_behavior ();
  var_via_enum_or_calls ()

let tests_lists () =
  backtrack_list ();
  list_edges ();
  list_call_stack ()

let tests_one_of () =
  one_of_case ();
  backtrack_one_of ()

let tests_call_stack () =
  callstack_case ();
  with_context_case ();
  with_context_exception ();
  enum_call_stack ();
  enum_or_calls_stack ();
  concatenated_var_functions ();
  fold_many_call_stack ();
  fold_many_with_enum_context ();
  many_call_stack ();
  triple_call_stack ()

let tests_error_formatting () =
  error_formatting_multiline ();
  error_formatting_long_line ();
  error_formatting_short_input ();
  error_formatting_at_start ();
  error_formatting_at_end ()

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
        test_case "backtracking" `Quick tests_backtracking;
        test_case "option" `Quick option_case;
        test_case "commit" `Quick commit_case;
        (* Value parsing *)
        test_case "numbers" `Quick numbers;
        test_case "units" `Quick units;
        test_case "idents" `Quick tests_idents;
        test_case "string literals" `Quick string_literals;
        test_case "until string" `Quick until_string;
        test_case "hex" `Quick hex_case;
        (* Error cases *)
        test_case "failures" `Quick failures;
        (* New helper functions *)
        test_case "option" `Quick option_parser;
        test_case "parse_many" `Quick parse_many;
        test_case "one_of" `Quick tests_one_of;
        test_case "take" `Quick take_case;
        (* enum and function-call parsing *)
        test_case "enums" `Quick tests_enums;
        test_case "enum_or_calls" `Quick tests_enum_or_calls;
        (* lists *)
        test_case "lists" `Quick tests_lists;
        (* call stack *)
        test_case "call stack" `Quick tests_call_stack;
        (* Error formatting *)
        test_case "error formatting" `Quick tests_error_formatting;
      ] );
  ]
