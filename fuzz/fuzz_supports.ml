(** Fuzz tests for the CSS Supports module.

    Tests crash safety of @supports condition parsing and roundtrip. *)

open Cascade
open Alcobar

(** Supports.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Supports.of_string buf)
  with Css.Reader.Parse_error _ | Invalid_argument _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Supports.of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> (
      let s = Css.Supports.to_string cond in
      try ignore (Css.Supports.of_string s)
      with Css.Reader.Parse_error _ | Invalid_argument _ ->
        fail "supports roundtrip re-parse failed")

(** pp — must not crash on any parsed condition. *)
let test_pp buf =
  match
    try Some (Css.Supports.of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> ignore (Css.Supports.to_string cond)

(** compare — must not crash on any pair of parsed conditions. *)
let test_compare buf1 buf2 =
  match
    ( (try Some (Css.Supports.of_string buf1)
       with Css.Reader.Parse_error _ | Invalid_argument _ -> None),
      try Some (Css.Supports.of_string buf2)
      with Css.Reader.Parse_error _ | Invalid_argument _ -> None )
  with
  | Some a, Some b ->
      ignore (Css.Supports.compare a b);
      ignore (Css.Supports.equal a b)
  | _ -> ()

let suite =
  ( "supports",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "roundtrip" [ bytes ] test_roundtrip;
      test_case "pp crash safety" [ bytes ] test_pp;
      test_case "compare crash safety" [ bytes; bytes ] test_compare;
    ] )
