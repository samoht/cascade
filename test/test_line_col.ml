(** Line_col module tests: the indexed lookup against a direct reading of CSS
    Syntax 3 (ED) sec. 3.3, and what a lookup costs. *)

open Cascade

(* Sec. 3.3 read literally, one decoded element at a time from byte 0: CR, FF,
   LF and the CRLF pair each end one line, anything else is one column, and so
   is a sequence the end of the window cuts short. *)
let reference source pos =
  let stop = max 0 (min pos (String.length source)) in
  let rec go i line col after_cr =
    if i >= stop then (line, col)
    else
      let d = String.get_utf_8_uchar source i in
      let n = Uchar.utf_decode_length d in
      if i + n > stop || not (Uchar.utf_decode_is_valid d) then
        go (min stop (i + n)) line (col + 1) false
      else
        match Uchar.to_int (Uchar.utf_decode_uchar d) with
        | 0x0A when after_cr -> go (i + n) line col false
        | 0x0A | 0x0C -> go (i + n) (line + 1) 1 false
        | 0x0D -> go (i + n) (line + 1) 1 true
        | _ -> go (i + n) line (col + 1) false
  in
  go 0 1 1 false

let repeat n s = String.concat "" (List.init n (fun _ -> s))

(* Each source runs over several strides, with a length that shares no factor
   with the stride so that the indexed positions fall on every byte of the
   repeated text in turn: inside a code point, between a CR and its LF, on an
   ill-formed byte. *)
let sources =
  [
    ("empty", "");
    ("shorter than a stride", "a\nb\r\nc");
    ("ascii on one line", repeat 5_000 "a");
    ("one line a byte", repeat 5_000 "\n");
    ( "every break and width",
      (* U+20AC, CRLF, CR, FF, LF, U+1F600, then a lead byte with one of its two
         continuation bytes. *)
      repeat 300 "a\xE2\x82\xACb\r\nc\rd\x0Ce\n\xF0\x9F\x98\x80\xE2\x82" );
    ("CRLF on even bytes", repeat 2_000 "\r\n");
    ("CRLF on odd bytes", "a" ^ repeat 2_000 "\r\n");
    ("three-byte code points", repeat 1_500 "\xE2\x82\xAC");
    ("four-byte code points", "ab" ^ repeat 1_100 "\xF0\x9F\x98\x80");
    ("continuation bytes alone", repeat 3_000 "\x80");
  ]

let pos = Alcotest.(pair int int)

let agrees_with_the_reference () =
  List.iter
    (fun (name, source) ->
      let index = Line_col.v source in
      for at = -1 to String.length source + 1 do
        let expected = reference source at in
        let label what = Fmt.str "%s: %s of byte %d" name what at in
        Alcotest.check pos (label "locate") expected (Line_col.locate index at)
      done)
    sources

(* A lookup allocates a constant amount however far it walks, so the bound is on
   the bytes read: the source once to build the index, then no more than a
   stride and one code point for each lookup, on a source that is a single line
   as much as on one of many. *)
let lookups_read_a_stride () =
  List.iter
    (fun (name, source) ->
      let index = Line_col.v source in
      let length = String.length source in
      let lookups = 1_000 in
      let probes = ref 0 in
      for k = 1 to lookups do
        let (_ : int * int) =
          Line_col.locate ~probes index (k * length / lookups)
        in
        ()
      done;
      let bound = length + (lookups * (Line_col.stride + 4)) in
      let read = !probes in
      if read > bound then
        Alcotest.failf "%s: %d lookups in %d bytes read %d bytes, over %d" name
          lookups length read bound)
    [
      ("one line", repeat 1_000_000 "a");
      ("a line a rule", repeat 50_000 ".x{float:center}\n");
    ]

let suite =
  ( "line_col",
    [
      Alcotest.test_case "agrees with the reference" `Quick
        agrees_with_the_reference;
      Alcotest.test_case "lookups read a stride" `Quick lookups_read_a_stride;
    ] )
