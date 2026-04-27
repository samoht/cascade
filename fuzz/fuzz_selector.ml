(** Fuzz tests for the CSS Selector module.

    Tests crash safety of selector parsing and roundtrip consistency. *)

open Cascade
open Alcobar

(** Selector.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Selector.of_string buf) with
  | Css.Cursor.Parse_error _ -> ()
  | Invalid_argument _ -> ()

(** Selector.read — must not crash. *)
let test_read buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read r) with Css.Cursor.Parse_error _ -> ()

(** Selector.read_selector_list — must not crash. *)
let test_read_selector_list buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_selector_list r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_combinator — must not crash. *)
let test_read_combinator buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_combinator r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_attribute_match — must not crash. *)
let test_read_attribute_match buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_attribute_match r)
  with Css.Cursor.Parse_error _ -> ()

(** Selector.read_nth — must not crash. *)
let test_read_nth buf =
  let r = Css.Cursor.of_string buf in
  try ignore (Css.Selector.read_nth r) with Css.Cursor.Parse_error _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let s = Css.Selector.to_string sel in
      try ignore (Css.Selector.of_string s)
      with Css.Cursor.Parse_error _ | Invalid_argument _ ->
        fail "roundtrip re-parse crashed")

(** pp — must not crash on any parsed selector. *)
let test_pp buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> ignore (Css.Selector.to_string sel)

(** Selectors specificity is structural and must be stable across
    parse/serialize/reparse for accepted selectors. *)
let test_specificity_roundtrip buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel -> (
      let serialized = Css.Selector.to_string ~minify:true sel in
      match
        try Some (Css.Selector.of_string serialized)
        with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
      with
      | None -> fail "specificity roundtrip serialization did not reparse"
      | Some reparsed ->
          let before = Css.Selector.specificity sel in
          let after = Css.Selector.specificity reparsed in
          if before <> after then
            fail
              (Fmt.str "specificity changed across serialization: %S -> %S" buf
                 serialized))

let test_serialization_idempotent buf =
  match
    try Some (Css.Selector.of_string buf)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some sel ->
      let once = Css.Selector.to_string ~minify:true sel in
      let twice = Css.Selector.(once |> of_string |> to_string ~minify:true) in
      if once <> twice then
        fail (Fmt.str "selector serialization drifted: %S -> %S" once twice)

let test_selector_list_serialization_idempotent buf =
  let r = Css.Cursor.of_string buf in
  match
    try Some (Css.Selector.read_selector_list r)
    with Css.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some selectors ->
      let once = Css.Selector.to_string ~minify:true selectors in
      let r2 = Css.Cursor.of_string once in
      let selectors2 = Css.Selector.read_selector_list r2 in
      let twice = Css.Selector.to_string ~minify:true selectors2 in
      if once <> twice then fail "selector-list serialization drifted"

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
      test_case "specificity roundtrip" [ bytes ] test_specificity_roundtrip;
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "selector list serialization idempotent" [ bytes ]
        test_selector_list_serialization_idempotent;
    ] )
