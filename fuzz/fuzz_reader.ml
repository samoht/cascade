(** Fuzz tests for the CSS Reader module.

    Tests crash safety of the character cursor on arbitrary input. *)

open Cascade
open Alcobar

(** Reader.of_string + is_done -- must not crash. *)
let test_of_string buf =
  let r = Reader.of_string buf in
  ignore (Reader.is_done r)

(** Walking the whole input with peek/skip terminates and does not crash. *)
let test_walk buf =
  let r = Reader.of_string buf in
  while not (Reader.is_done r) do
    ignore (Reader.peek r);
    ignore (Reader.peek_byte r);
    Reader.skip r
  done

(** Reader.skip_utf8 walks the input by code point and terminates. *)
let test_walk_utf8 buf =
  let r = Reader.of_string buf in
  while not (Reader.is_done r) do
    ignore (Reader.peek_utf8 r);
    Reader.skip_utf8 r
  done

(** Offset peeks stay in bounds for any offset, including negative ones. *)
let test_peek_at buf =
  let r = Reader.of_string buf in
  let len = String.length buf in
  List.iter
    (fun off ->
      ignore (Reader.peek_at r off);
      ignore (Reader.peek_byte_at r off);
      ignore (Reader.peek_utf8_at r off))
    [ -1; 0; 1; len; len + 1; max_int ]

(** Reader.peek_string clamps to the remaining input. *)
let test_peek_string buf =
  let r = Reader.of_string buf in
  let n = String.length (Reader.peek_string r max_int) in
  assert (n <= String.length (Reader.source r))

(** Reader.looking_at -- must not crash on an arbitrary needle. *)
let test_looking_at buf =
  let r = Reader.of_string buf in
  ignore (Reader.looking_at r buf);
  ignore (Reader.looking_at r "")

(** The error snippet is built at any position without crashing. *)
let test_context_window buf =
  let r = Reader.of_string buf in
  while not (Reader.is_done r) do
    ignore (Reader.context_window r);
    Reader.skip r
  done;
  match Reader.err r "boom" with
  | (_ : unit) -> assert false
  | exception Reader.Parse_error e -> ignore (Reader.pp_parse_error e)

let suite =
  ( "reader",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "byte walk crash safety" [ bytes ] test_walk;
      test_case "code point walk crash safety" [ bytes ] test_walk_utf8;
      test_case "offset peek crash safety" [ bytes ] test_peek_at;
      test_case "peek_string crash safety" [ bytes ] test_peek_string;
      test_case "looking_at crash safety" [ bytes ] test_looking_at;
      test_case "error snippet crash safety" [ bytes ] test_context_window;
    ] )
