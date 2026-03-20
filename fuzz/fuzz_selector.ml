(** Fuzz tests for the CSS Selector module.

    Tests crash safety of selector parsing and roundtrip consistency. *)

open Cascade
open Alcobar

(** Selector.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Selector.of_string buf) with
  | Css.Reader.Parse_error _ -> ()
  | Invalid_argument _ -> ()

(** Selector.read — must not crash. *)
let test_read buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Selector.read r) with Css.Reader.Parse_error _ -> ()

(** Selector.read_selector_list — must not crash. *)
let test_read_selector_list buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Selector.read_selector_list r)
  with Css.Reader.Parse_error _ -> ()

(** Selector.read_combinator — must not crash. *)
let test_read_combinator buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Selector.read_combinator r)
  with Css.Reader.Parse_error _ -> ()

(** Selector.read_attribute_match — must not crash. *)
let test_read_attribute_match buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Selector.read_attribute_match r)
  with Css.Reader.Parse_error _ -> ()

(** Selector.read_nth — must not crash. *)
let test_read_nth buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Selector.read_nth r) with Css.Reader.Parse_error _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let s = Css.Selector.to_string sel in
      try ignore (Css.Selector.of_string s)
      with Css.Reader.Parse_error _ | Invalid_argument _ ->
        fail "roundtrip re-parse crashed")

(** pp — must not crash on any parsed selector. *)
let test_pp buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> ignore (Css.Selector.to_string sel)

let suite =
  ( "selector",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "read crash safety" [ bytes ] test_read;
      test_case "read_selector_list crash safety" [ bytes ]
        test_read_selector_list;
      test_case "read_combinator crash safety" [ bytes ] test_read_combinator;
      test_case "read_attribute_match crash safety" [ bytes ]
        test_read_attribute_match;
      test_case "read_nth crash safety" [ bytes ] test_read_nth;
      test_case "roundtrip" [ bytes ] test_roundtrip;
      test_case "pp crash safety" [ bytes ] test_pp;
    ] )
