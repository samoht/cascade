(** Tests for Cascade_diff.String_diff module *)

open Cascade

let () = ignore (Css.of_string ~strict:false "")

(* ===== first_diff_pos tests ===== *)

let first_diff_pos_identical () =
  Alcotest.(check (option int))
    "identical strings" None
    (Cascade_diff.String_diff.first_diff_pos "hello" "hello")

let first_diff_pos_empty () =
  Alcotest.(check (option int))
    "both empty" None
    (Cascade_diff.String_diff.first_diff_pos "" "")

let first_diff_pos_vs_nonempty () =
  Alcotest.(check (option int))
    "empty vs nonempty" (Some 0)
    (Cascade_diff.String_diff.first_diff_pos "" "a")

let first_diff_pos_vs_empty () =
  Alcotest.(check (option int))
    "nonempty vs empty" (Some 0)
    (Cascade_diff.String_diff.first_diff_pos "a" "")

let first_diff_pos_at_start () =
  Alcotest.(check (option int))
    "differ at start" (Some 0)
    (Cascade_diff.String_diff.first_diff_pos "abc" "xyz")

let first_diff_pos_at_middle () =
  Alcotest.(check (option int))
    "differ in middle" (Some 2)
    (Cascade_diff.String_diff.first_diff_pos "abcdef" "abXdef")

let first_diff_pos_at_end () =
  Alcotest.(check (option int))
    "differ at end" (Some 4)
    (Cascade_diff.String_diff.first_diff_pos "abcde" "abcdX")

let first_diff_pos_length_mismatch () =
  Alcotest.(check (option int))
    "shorter vs longer" (Some 3)
    (Cascade_diff.String_diff.first_diff_pos "abc" "abcdef")

(* ===== diff tests ===== *)

let diff_identical () =
  Alcotest.(check bool)
    "identical returns None" true
    (Cascade_diff.String_diff.diff ~expected:"hello" "hello" = None)

let diff_single_char () =
  let result = Cascade_diff.String_diff.diff ~expected:"abc" "aXc" in
  Alcotest.(check bool) "single char diff is Some" true (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 1" 1 d.position

let diff_multiline () =
  let expected = "line1\nline2\nline3" in
  let actual = "line1\nlineX\nline3" in
  let result = Cascade_diff.String_diff.diff ~expected actual in
  Alcotest.(check bool) "multiline diff is Some" true (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 10" 10 d.position;
  Alcotest.(check int) "line_expected is 1" 1 d.line_expected

let diff_empty_vs_nonempty () =
  let result = Cascade_diff.String_diff.diff ~expected:"" "something" in
  Alcotest.(check bool) "empty vs nonempty is Some" true (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 0" 0 d.position

(* ===== truncate_middle tests ===== *)

let truncate_short () =
  let result = Cascade_diff.String_diff.truncate_middle 20 "hello" in
  Alcotest.(check string) "short string unchanged" "hello" result

let truncate_exact () =
  let s = "abcde" in
  let result = Cascade_diff.String_diff.truncate_middle 5 s in
  Alcotest.(check string) "exact length unchanged" "abcde" result

let truncate_long () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  let result = Cascade_diff.String_diff.truncate_middle 10 s in
  (* (10-3)/2 = 3 chars from each end: "abc...xyz" *)
  Alcotest.(check int) "truncated length" 9 (String.length result);
  Alcotest.(check bool) "contains ellipsis" true (String.contains result '.')

let truncate_preserves_start_and_end () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  let result = Cascade_diff.String_diff.truncate_middle 13 s in
  Alcotest.(check bool) "starts with 'a'" true (String.get result 0 = 'a');
  Alcotest.(check bool)
    "ends with 'z'" true
    (String.get result (String.length result - 1) = 'z')

(* ===== pp tests ===== *)

let pp_does_not_crash () =
  let result = Cascade_diff.String_diff.diff ~expected:"abc" "aXc" in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp buf d;
      let output = Buffer.contents buf in
      Alcotest.(check bool) "pp produces output" true (String.length output > 0)

let pp_with_labels () =
  let result = Cascade_diff.String_diff.diff ~expected:"abc" "aXc" in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp ~expected_label:"Old" ~actual_label:"New" buf
        d;
      let output = Buffer.contents buf in
      Alcotest.(check bool)
        "pp with labels produces output" true
        (String.length output > 0)

(* A file ending in a newline has N lines, not N+1: the empty string
   [split_on_char] leaves behind is the terminator. Printing it as context adds
   a line holding one space, below the difference the report is about. *)
let pp_trailing_newline_adds_no_context_line () =
  let expected = "a\nb\n" in
  let actual = "a\nx\n" in
  match Cascade_diff.String_diff.diff ~expected actual with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp buf d;
      Alcotest.(check string)
        "a terminating newline contributes no context line"
        (String.concat "\n"
           [
             "Strings differ at position 2 (line 1, col 0)";
             "";
             "--- Expected";
             "+++ Actual";
             "@@ position 2 @@";
             " a";
             "-b";
             "+x";
             " ^";
             "";
           ])
        (Buffer.contents buf)

(* A real empty line before the terminator is context and stays visible. Only
   the final empty element created by the terminating newline is synthetic. *)
let pp_trailing_blank_line_keeps_one_context_line () =
  let expected = "a\nb\n\n" in
  let actual = "a\nx\n\n" in
  match Cascade_diff.String_diff.diff ~expected actual with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp buf d;
      Alcotest.(check string)
        "one authored blank context line remains"
        (String.concat "\n"
           [
             "Strings differ at position 2 (line 1, col 0)";
             "";
             "--- Expected";
             "+++ Actual";
             "@@ position 2 @@";
             " a";
             "-b";
             "+x";
             " ^";
             " ";
             "";
           ])
        (Buffer.contents buf)

let pp_short_lines_keeps_after_context () =
  let expected = "a\nb\nc\nd\ne" in
  let actual = "a\nx\nc\nd\ne" in
  match Cascade_diff.String_diff.diff ~expected actual with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp buf d;
      Alcotest.(check string)
        "short lines keep trailing context"
        (String.concat "\n"
           [
             "Strings differ at position 2 (line 1, col 0)";
             "";
             "--- Expected";
             "+++ Actual";
             "@@ position 2 @@";
             " a";
             "-b";
             "+x";
             " ^";
             " c";
             " d";
             " e";
             "";
           ])
        (Buffer.contents buf)

(* A caret is placed in columns everywhere else in this repository, so a
   multi-byte scalar ahead of the difference moves it by one, not by its byte
   length. Three euro signs are three columns, nine bytes. *)
let pp_caret_counts_scalars_not_bytes () =
  let expected = "\xe2\x82\xac\xe2\x82\xac\xe2\x82\xaca" in
  let actual = "\xe2\x82\xac\xe2\x82\xac\xe2\x82\xacb" in
  match Cascade_diff.String_diff.diff ~expected actual with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      Cascade_diff.String_diff.pp buf d;
      let lines = String.split_on_char '\n' (Buffer.contents buf) in
      let caret =
        List.find_opt (fun l -> String.contains l '^') (List.tl lines)
      in
      let column = function
        | None -> Alcotest.fail "no caret line"
        | Some l -> String.index l '^'
      in
      (* One space for the hunk's own column, then three scalars. *)
      Alcotest.(check int) "caret sits after three columns" 4 (column caret)

(* The caret marks where the two rendered lines part company. A long line is
   shown through a truncated window whose leading ellipsis shifts every byte
   right, and an escape inside it widens what comes after, so the column has to
   be read off the rendered text rather than counted in source bytes. *)
let pp_caret_marks_the_rendered_difference () =
  let filler = String.make 80 'a' in
  let expected = String.concat "" [ filler; "\tx"; filler ] in
  let actual = String.concat "" [ filler; "\ty"; filler ] in
  match Cascade_diff.String_diff.diff ~expected actual with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 512 in
      Cascade_diff.String_diff.pp buf d;
      let lines = String.split_on_char '\n' (Buffer.contents buf) in
      (* The hunk starts after the [@@] marker; the [---] and [+++] headers
         above it also open with the prefix characters. *)
      let rec after_marker = function
        | [] -> Alcotest.fail "no hunk marker"
        | l :: rest when String.length l >= 2 && String.sub l 0 2 = "@@" -> rest
        | _ :: rest -> after_marker rest
      in
      let hunk = after_marker lines in
      let starting c =
        List.find_opt (fun l -> String.length l > 0 && l.[0] = c) hunk
      in
      let get name = function
        | Some l -> l
        | None -> Alcotest.fail (String.concat "" [ "no "; name; " line" ])
      in
      let minus = get "-" (starting '-') and plus = get "+" (starting '+') in
      let caret =
        get "caret" (List.find_opt (fun l -> String.contains l '^') hunk)
      in
      let rec first_diff i =
        if i >= String.length minus || i >= String.length plus then i
        else if minus.[i] <> plus.[i] then i
        else first_diff (i + 1)
      in
      Alcotest.(check int)
        "the caret column is where the rendered lines differ" (first_diff 1)
        (String.index caret '^')

let pp_crlf_vs_lf () =
  let expected = ".a{color:red}\r\n" in
  let actual = ".a{color:red}\n" in
  let d = Option.get (Cascade_diff.String_diff.diff ~expected actual) in
  let buf = Buffer.create 256 in
  Cascade_diff.String_diff.pp buf d;
  let output = Buffer.contents buf in
  let caret = String.concat "" [ String.make (13 + 1) ' '; "^\n" ] in
  let expected_output =
    String.concat ""
      [
        "Strings differ at position 13 (line 0, col 13)\n\n";
        "--- Expected\n";
        "+++ Actual\n";
        "@@ position 13 @@\n";
        "-.a{color:red}\\r\n";
        "+.a{color:red}\n";
        caret;
      ]
  in
  Alcotest.(check string) "CRLF vs LF renders escaped CR" expected_output output

let pp_escape_byte () =
  let expected = ".a\x1B" in
  let actual = ".a" in
  let d = Option.get (Cascade_diff.String_diff.diff ~expected actual) in
  let buf = Buffer.create 256 in
  Cascade_diff.String_diff.pp buf d;
  let output = Buffer.contents buf in
  let caret = String.concat "" [ String.make (2 + 1) ' '; "^\n" ] in
  let expected_output =
    String.concat ""
      [
        "Strings differ at position 2 (line 0, col 2)\n\n";
        "--- Expected\n";
        "+++ Actual\n";
        "@@ position 2 @@\n";
        "-.a\\x1b\n";
        "+.a\n";
        caret;
      ]
  in
  Alcotest.(check string)
    "ESC byte is rendered as escape" expected_output output

(* ===== Suite ===== *)

let suite =
  ( "string_diff",
    [
      Alcotest.test_case "first_diff_pos identical" `Quick
        first_diff_pos_identical;
      Alcotest.test_case "first_diff_pos empty" `Quick first_diff_pos_empty;
      Alcotest.test_case "first_diff_pos empty vs nonempty" `Quick
        first_diff_pos_vs_nonempty;
      Alcotest.test_case "first_diff_pos nonempty vs empty" `Quick
        first_diff_pos_vs_empty;
      Alcotest.test_case "first_diff_pos at start" `Quick
        first_diff_pos_at_start;
      Alcotest.test_case "first_diff_pos at middle" `Quick
        first_diff_pos_at_middle;
      Alcotest.test_case "first_diff_pos at end" `Quick first_diff_pos_at_end;
      Alcotest.test_case "first_diff_pos length mismatch" `Quick
        first_diff_pos_length_mismatch;
      Alcotest.test_case "diff identical" `Quick diff_identical;
      Alcotest.test_case "diff single char" `Quick diff_single_char;
      Alcotest.test_case "diff multiline" `Quick diff_multiline;
      Alcotest.test_case "diff empty vs nonempty" `Quick diff_empty_vs_nonempty;
      Alcotest.test_case "truncate short" `Quick truncate_short;
      Alcotest.test_case "truncate exact" `Quick truncate_exact;
      Alcotest.test_case "truncate long" `Quick truncate_long;
      Alcotest.test_case "truncate preserves start and end" `Quick
        truncate_preserves_start_and_end;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "pp with labels" `Quick pp_with_labels;
      Alcotest.test_case "pp short lines keeps after context" `Quick
        pp_short_lines_keeps_after_context;
      Alcotest.test_case "pp trailing newline adds no context line" `Quick
        pp_trailing_newline_adds_no_context_line;
      Alcotest.test_case "pp trailing blank line keeps one context line" `Quick
        pp_trailing_blank_line_keeps_one_context_line;
      Alcotest.test_case "pp caret marks the rendered difference" `Quick
        pp_caret_marks_the_rendered_difference;
      Alcotest.test_case "pp caret counts scalars not bytes" `Quick
        pp_caret_counts_scalars_not_bytes;
      Alcotest.test_case "pp crlf vs lf" `Quick pp_crlf_vs_lf;
      Alcotest.test_case "pp escape byte" `Quick pp_escape_byte;
    ] )
