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

let test_comment_skipping () =
  let r = Reader.of_string "/*c1*/  /*c2*/x" in
  Reader.skip_ws r;
  check (option char) "after comments" (Some 'x') (Reader.peek r)

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

let test_brackets_and_braces () =
  let r = Reader.of_string "[x]{y}" in
  let b = Reader.brackets r (fun r -> Reader.until r ']') in
  check string "brackets" "x" b;
  Reader.ws r;
  let br = Reader.braces r (fun r -> Reader.until r '}') in
  check string "braces" "y" br;
  check bool "done" true (Reader.is_done r)

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

let test_commit_behaviour () =
  let r = Reader.of_string "foobar" in
  Reader.save r;
  Reader.expect_string r "foo";
  Reader.commit r;
  (* After commit, restore should fail; but position advanced to after foo *)
  check string "peek after commit" "bar" (Reader.peek_string r 3);
  (* Now save+try_parse succeeds and commit keeps progress *)
  Reader.save r;
  (match Reader.try_parse (fun r -> Reader.expect_string r "bar") r with
  | Some () -> Reader.commit r
  | None -> fail "expected parse success");
  check bool "done after commit" true (Reader.is_done r)

let test_numbers_and_units () =
  (* number, int, percentage, dimension/angle/duration *)
  let r = Reader.of_string "-12.5 42 33% 1.5rem 90deg 250ms" in
  let n = Reader.number r in
  Alcotest.(check (float 0.0001)) "number" (-12.5) n;
  Reader.ws r;
  let i = Reader.int r in
  check int "int" 42 i;
  Reader.ws r;
  let pct = Reader.number r in
  Reader.expect r '%';
  Alcotest.(check (float 0.0001)) "percentage" 33. pct;
  Reader.ws r;
  (* Use Values.read_dimension *)
  let d1, u1 = Css.Values.read_dimension r in
  Alcotest.(check (float 0.0001)) "dimension value" 1.5 d1;
  check string "dimension unit" "rem" u1;
  Reader.ws r;
  (* Parse angle using Values *)
  let ang = Css.Values.read_angle r in
  (match ang with
  | Css.Values.Deg v ->
      Alcotest.(check (float 0.0001)) "angle value" 90. v;
      check string "angle unit" "deg" "deg"
  | _ -> fail "Expected Deg angle");
  Reader.ws r;
  (* Parse duration using Values *)
  let dur = Css.Values.read_duration r in
  match dur with
  | Css.Values.Ms v ->
      Alcotest.(check (float 0.0001)) "duration value" 250. v;
      check string "duration unit" "ms" "ms"
  | _ -> fail "Expected Ms duration"

let test_ident_and_string () =
  let r = Reader.of_string "my-ident 'a\\'b'\"c\"\ndone" in
  let id = Reader.ident r in
  check string "ident" "my-ident" id;
  Reader.ws r;
  let s1 = Reader.string r in
  check string "single-quoted with escape" "a'b" s1;
  Reader.ws r;
  let s2 = Reader.string r in
  check string "double-quoted string" "c" s2;
  Reader.ws r;
  check string "rest" "done" (Reader.peek_string r 4)

let test_until_string_and_separated () =
  let r = Reader.of_string "item1,item2 , item3;rest" in
  let items =
    Reader.separated r
      (fun r ->
        let item = Reader.while_ r (fun c -> c <> ',' && c <> ';') in
        String.trim item)
      (fun r ->
        Reader.ws r;
        Reader.expect r ',';
        Reader.ws r)
  in
  check (list string) "separated items" [ "item1"; "item2"; "item3" ] items;
  Reader.expect r ';';
  (* Use position/length to read rest; also validate until_string separately *)
  let remaining = Reader.length r - Reader.position r in
  check string "rest after ;" "rest" (Reader.peek_string r remaining);
  (* Separate check for until_string tokenized read *)
  let r2 = Reader.of_string "abcXYZdef" in
  let before = Reader.until_string r2 "XYZ" in
  check string "until_string before token" "abc" before;
  check string "after until_string" "XYZdef" (Reader.peek_string r2 7)

let test_context_and_pp () =
  let r = Reader.of_string "abcdefghij" in
  Reader.skip_n r 5;
  let before, after = Reader.context_string ~window:3 r in
  check string "context before" "cde" before;
  check string "context after" "fgh" after;
  let r2 = Reader.of_string "hello world" in
  Reader.skip_n r2 6;
  check string "pp preview" "world" (Reader.pp r2)

let test_ident_failure () =
  let r = Reader.of_string "1bad" in
  let f () = ignore (Reader.ident r) in
  check_raises "ident must start correctly"
    (Reader.Parse_error ("expected identifier", r))
    f

let test_ident_with_escapes () =
  (* Test escaped colon in identifier (Tailwind-style class) *)
  let r = Reader.of_string "hover\\:text-red" in
  let id = Reader.ident r in
  (* Should read the escaped colon as part of the identifier *)
  check string "ident with escaped colon" "hover\\:text-red" id;

  (* Test escaped dot *)
  let r = Reader.of_string "v1\\.0\\.1" in
  let id = Reader.ident r in
  check string "ident with escaped dots" "v1\\.0\\.1" id;

  (* Test Unicode escape sequence *)
  let r = Reader.of_string "\\000026B" in
  let id = Reader.ident r in
  (* Should parse as the character '&B' *)
  check string "unicode escape" "&B" id;

  (* Test simple backslash escape *)
  let r = Reader.of_string "test\\-name" in
  let id = Reader.ident r in
  check string "escaped hyphen" "test-name" id

let test_ident_hex_termination () =
  (* Exactly 6 hex digits are consumed; next hex-like char remains *)
  let r = Reader.of_string "\\000041Z" in
  let id = Reader.ident r in
  check string "hex escape termination at 6 digits" "AZ" id;
  (* Delimiter after ident is preserved for next parse *)
  let r = Reader.of_string "abc.def" in
  let id = Reader.ident r in
  check string "ident before delimiter" "abc" id;
  check (option char) "delimiter remains" (Some '.') (Reader.peek r)

let test_ident_roundtrip_pp () =
  (* Roundtrip via Selector.class_ to_string should preserve escaped forms *)
  let samples = [ "simple"; "hover\\:text-red"; "v1\\.0\\.1"; "éxämple" ] in
  List.iter
    (fun s ->
      let cls = Selector.(to_string (class_ s)) in
      check string (Fmt.str "roundtrip class %s" s) ("." ^ s) cls)
    samples

let test_hex_and_colors () =
  (* hex colors are parsed in Values module, not Reader *)
  let r = Reader.of_string "#abc #a1b2c3" in
  Reader.expect r '#';
  (* Read hex digits manually using while_ *)
  let hex3 =
    Reader.while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  check string "hex 3" "abc" hex3;
  Reader.ws r;
  Reader.expect r '#';
  let hex6 =
    Reader.while_ r (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  check string "hex 6" "a1b2c3" hex6;
  (* color_keyword succeeds for known names, resets on unknown *)
  (* color keywords are parsed in Values module, not Reader *)
  let r = Reader.of_string "red" in
  (match Values.read_color r with
  | Values.Named Values.Red -> ()
  | _ -> fail "expected red color");
  let r = Reader.of_string "unknown" in
  match Reader.try_parse Values.read_color r with
  | None -> () (* expected to fail *)
  | _ -> fail "unknown should fail"

let test_rgb_function () =
  (* RGB functions are parsed in Values module *)
  let r = Reader.of_string "rgba(255, 0, 10, 0.5)" in
  (match Values.read_color r with
  | Values.Rgba { r = Int 255; g = Int 0; b = Int 10; a = Num a } ->
      Alcotest.(check (float 0.0001)) "alpha" 0.5 a
  | _ -> fail "expected rgba value");
  let r = Reader.of_string "rgb(10, 20, 30)" in
  (match Values.read_color r with
  | Values.Rgb { r = Int 10; g = Int 20; b = Int 30 } -> ()
  | _ -> fail "expected rgb value");
  let r = Reader.of_string "hsl(10, 10%, 10%)" in
  match Values.read_color r with
  | Values.Hsl _ -> ()
  | _ -> fail "expected hsl value"

let suite =
  [
    ( "reader",
      [
        test_case "basic_operations" `Quick test_basic_operations;
        test_case "string_operations" `Quick test_string_operations;
        test_case "whitespace" `Quick test_whitespace;
        test_case "comment_skipping" `Quick test_comment_skipping;
        test_case "take_while" `Quick test_take_while;
        test_case "expect" `Quick test_expect;
        test_case "expect_string_and_between" `Quick
          test_expect_string_and_between;
        test_case "brackets_and_braces" `Quick test_brackets_and_braces;
        test_case "save_restore_try_parse" `Quick test_save_restore_try_parse;
        test_case "commit_behaviour" `Quick test_commit_behaviour;
        test_case "numbers_and_units" `Quick test_numbers_and_units;
        test_case "ident_and_string" `Quick test_ident_and_string;
        test_case "until_string_and_separated" `Quick
          test_until_string_and_separated;
        test_case "context_and_pp" `Quick test_context_and_pp;
        test_case "ident_failure" `Quick test_ident_failure;
        test_case "ident_with_escapes" `Quick test_ident_with_escapes;
        test_case "ident_hex_termination" `Quick test_ident_hex_termination;
        test_case "ident_roundtrip_pp" `Quick test_ident_roundtrip_pp;
        test_case "hex_and_colors" `Quick test_hex_and_colors;
        test_case "rgb_function" `Quick test_rgb_function;
      ] );
  ]
