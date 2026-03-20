(** Tests for Css_tools.String_diff module *)

open Cascade

let () = ignore Css.of_string

(* ===== first_diff_pos tests ===== *)

let first_diff_pos_identical () =
  Alcotest.(check (option int)) "identical strings" None
    (Css_tools.String_diff.first_diff_pos "hello" "hello")

let first_diff_pos_empty () =
  Alcotest.(check (option int)) "both empty" None
    (Css_tools.String_diff.first_diff_pos "" "")

let first_diff_pos_vs_nonempty () =
  Alcotest.(check (option int)) "empty vs nonempty" (Some 0)
    (Css_tools.String_diff.first_diff_pos "" "a")

let first_diff_pos_vs_empty () =
  Alcotest.(check (option int)) "nonempty vs empty" (Some 0)
    (Css_tools.String_diff.first_diff_pos "a" "")

let first_diff_pos_at_start () =
  Alcotest.(check (option int)) "differ at start" (Some 0)
    (Css_tools.String_diff.first_diff_pos "abc" "xyz")

let first_diff_pos_at_middle () =
  Alcotest.(check (option int)) "differ in middle" (Some 2)
    (Css_tools.String_diff.first_diff_pos "abcdef" "abXdef")

let first_diff_pos_at_end () =
  Alcotest.(check (option int)) "differ at end" (Some 4)
    (Css_tools.String_diff.first_diff_pos "abcde" "abcdX")

let first_diff_pos_length_mismatch () =
  Alcotest.(check (option int)) "shorter vs longer" (Some 3)
    (Css_tools.String_diff.first_diff_pos "abc" "abcdef")

(* ===== diff tests ===== *)

let diff_identical () =
  Alcotest.(check bool) "identical returns None" true
    (Css_tools.String_diff.diff ~expected:"hello" "hello" = None)

let diff_single_char () =
  let result = Css_tools.String_diff.diff ~expected:"abc" "aXc" in
  Alcotest.(check bool) "single char diff is Some" true
    (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 1" 1 d.position

let diff_multiline () =
  let expected = "line1\nline2\nline3" in
  let actual = "line1\nlineX\nline3" in
  let result = Css_tools.String_diff.diff ~expected actual in
  Alcotest.(check bool) "multiline diff is Some" true
    (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 10" 10 d.position;
  Alcotest.(check int) "line_expected is 1" 1 d.line_expected

let diff_empty_vs_nonempty () =
  let result = Css_tools.String_diff.diff ~expected:"" "something" in
  Alcotest.(check bool) "empty vs nonempty is Some" true
    (Option.is_some result);
  let d = Option.get result in
  Alcotest.(check int) "position is 0" 0 d.position

(* ===== truncate_middle tests ===== *)

let truncate_short () =
  let result = Css_tools.String_diff.truncate_middle 20 "hello" in
  Alcotest.(check string) "short string unchanged" "hello" result

let truncate_exact () =
  let s = "abcde" in
  let result = Css_tools.String_diff.truncate_middle 5 s in
  Alcotest.(check string) "exact length unchanged" "abcde" result

let truncate_long () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  let result = Css_tools.String_diff.truncate_middle 10 s in
  (* (10-3)/2 = 3 chars from each end: "abc...xyz" *)
  Alcotest.(check int) "truncated length" 9 (String.length result);
  Alcotest.(check bool) "contains ellipsis" true
    (String.contains result '.')

let truncate_preserves_start_and_end () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  let result = Css_tools.String_diff.truncate_middle 13 s in
  Alcotest.(check bool) "starts with 'a'" true
    (String.get result 0 = 'a');
  Alcotest.(check bool) "ends with 'z'" true
    (String.get result (String.length result - 1) = 'z')

(* ===== pp tests ===== *)

let pp_does_not_crash () =
  let result = Css_tools.String_diff.diff ~expected:"abc" "aXc" in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      Css_tools.String_diff.pp fmt d;
      Format.pp_print_flush fmt ();
      let output = Buffer.contents buf in
      Alcotest.(check bool) "pp produces output" true
        (String.length output > 0)

let pp_with_labels () =
  let result = Css_tools.String_diff.diff ~expected:"abc" "aXc" in
  match result with
  | None -> Alcotest.fail "expected Some"
  | Some d ->
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      Css_tools.String_diff.pp ~expected_label:"Old" ~actual_label:"New" fmt d;
      Format.pp_print_flush fmt ();
      let output = Buffer.contents buf in
      Alcotest.(check bool) "pp with labels produces output" true
        (String.length output > 0)

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
    ] )
