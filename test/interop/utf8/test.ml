(** UTF-8 boundary harness.

    Feeds Markus Kuhn's UTF-8 stress test through
    [Cascade.Css.of_string ~strict:false] and asserts the parser does not raise,
    regardless of ill-formed input. CSS Syntax L3 sec. 3.3 requires malformed
    UTF-8 to be replaced with U+FFFD rather than rejected.

    Traces: [traces/UTF-8-test.txt] from
    [https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt] (CC BY 4.0).
    Regenerate: [dune build @regen-traces]. *)

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

let () =
  Alcotest.run "utf8_boundary"
    [
      ( "kuhn",
        [
          ("whole file", `Quick, test_whole_file);
          ("per line", `Quick, test_per_line);
        ] );
    ]
