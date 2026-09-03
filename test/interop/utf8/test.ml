(** UTF-8 boundary harness.

    Feeds Markus Kuhn's UTF-8 stress test through
    [Cascade.Css.of_string ~strict:false] and asserts the parser does not raise,
    regardless of ill-formed input. CSS Syntax L3 sec. 3.3 requires malformed
    UTF-8 to be replaced with U+FFFD rather than rejected. How many U+FFFD one
    ill-formed run stands for is what the counting tests below pin, since a
    parse error's column and its caret are measured in those elements.

    Traces: [traces/UTF-8-test.txt] from
    [https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt] (CC BY 4.0).
    Regenerate: [REGEN=1 dune build @@test/interop/utf8/regen-traces]. *)

module Reader = Cascade.Reader

let trace_path = "traces/UTF-8-test.txt"

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

(* Wrap the bytes in a synthetic stylesheet body so the parser is exercised
   end-to-end. Whatever malformed sequences land in identifiers, comments, or
   strings must survive without exception. *)
let wrap body = Fmt.str "/* %s */ a { content: \"%s\" }" body body

let parses_without_crash bytes =
  match Cascade.Css.of_string ~strict:false (wrap bytes) with
  | Ok _ | Error _ -> true

let test_whole_file () =
  let bytes = read_file trace_path in
  Alcotest.(check bool) "whole file accepted" true (parses_without_crash bytes)

let test_per_line () =
  let bytes = read_file trace_path in
  let lines = String.split_on_char '\n' bytes in
  List.iteri
    (fun i line ->
      Alcotest.(check bool)
        (Fmt.str "line %d accepted" (i + 1))
        true
        (parses_without_crash line))
    lines

(* The Unicode standard, sec. 3.9 "U+FFFD Substitution of Maximal Subparts",
   works this very sequence: [61 F1 80 80 E1 80 C2 62 80 63 80 BF 64] converts
   to [a FFFD FFFD FFFD b FFFD c FFFD FFFD d]. An ill-formed run yields one
   U+FFFD per maximal subpart, the longest prefix that could still open a
   well-formed sequence, so the six subparts here are [F1 80 80], [E1 80], [C2],
   [80], [80] and [BF]: six replacement characters plus four letters, ten
   elements from thirteen bytes. Browsers substitute by this rule, which is what
   makes a column counted in these elements the column a browser reports. *)
let maximal_subparts = "a\xf1\x80\x80\xe1\x80\xc2b\x80c\x80\xbfd"

let test_maximal_subpart_count () =
  Alcotest.(check int)
    "one element per maximal subpart" 10
    (Cascade.Common.String.utf8_length maximal_subparts);
  (* [E1 80] could still open a three-byte sequence, [A] cannot continue one, so
     the run ends there and the letter is an element of its own. *)
  Alcotest.(check int)
    "an ASCII byte after a truncated sequence stands alone" 2
    (Cascade.Common.String.utf8_length "\xe1\x80A")

(* Fail at byte [pos] of [input] and report where the reader says that is. *)
let error_line_and_column input pos =
  let open Reader in
  let r = of_string input in
  for _ = 1 to pos do
    skip r
  done;
  match err r "a character the input never holds" with
  | (_ : unit) -> Alcotest.failf "expected a parse error at byte %d" pos
  | exception Parse_error error -> (error.line, error.col)

(* A column is one element of the decoded text, counted from 1, so an error just
   past the ten elements of [maximal_subparts] sits in column 11. *)
let test_error_column_after_ill_formed () =
  Alcotest.(check (pair int int))
    "column past the tenth element" (1, 11)
    (error_line_and_column maximal_subparts (String.length maximal_subparts));
  Alcotest.(check (pair int int))
    "column past a truncated sequence and the letter after it" (1, 3)
    (error_line_and_column "\xe1\x80A" 3)

let () =
  Alcotest.run "utf8_boundary"
    [
      ( "kuhn",
        [
          ("whole file", `Quick, test_whole_file);
          ("per line", `Quick, test_per_line);
        ] );
      ( "maximal subparts",
        [
          ("element count", `Quick, test_maximal_subpart_count);
          ("error column", `Quick, test_error_column_after_ill_formed);
        ] );
    ]
