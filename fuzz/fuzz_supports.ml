(** Fuzz tests for the CSS Supports module.

    Tests crash safety of @supports condition parsing and roundtrip. *)

open Cascade
open Alcobar

(** Supports.of_string — must not crash on arbitrary input. *)
let test_of_string buf =
  try ignore (Css.Supports.of_string buf)
  with Failure _ | Invalid_argument _ -> ()

(** Roundtrip: parse → to_string → parse should not crash. *)
let test_roundtrip buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> (
      let s = Css.Supports.to_string cond in
      try ignore (Css.Supports.of_string s)
      with Failure _ | Invalid_argument _ ->
        fail "supports roundtrip re-parse failed")

let test_serialization_idempotent buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond ->
      let once = Css.Supports.to_string cond in
      let twice = Css.Supports.(once |> of_string |> to_string) in
      if once <> twice then fail "supports serialization drifted"

let rec has_mixed_operator = function
  | Css.Supports.And (Css.Supports.Or _, _)
  | Css.Supports.And (_, Css.Supports.Or _)
  | Css.Supports.Or (Css.Supports.And _, _)
  | Css.Supports.Or (_, Css.Supports.And _) ->
      true
  | Css.Supports.And (a, b) | Css.Supports.Or (a, b) ->
      has_mixed_operator a || has_mixed_operator b
  | Css.Supports.Not a -> has_mixed_operator a
  | Css.Supports.Property _ | Css.Supports.Func _ -> false

let test_mixed_operator_serialization_reparse buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | Some cond when has_mixed_operator cond ->
      let serialized = Css.Supports.to_string cond in
      let reparsed =
        try Some (Css.Supports.of_string serialized)
        with Failure _ | Invalid_argument _ -> None
      in
      if Option.is_none reparsed then
        fail "mixed and/or supports serialization did not reparse"
  | _ -> ()

(** pp — must not crash on any parsed condition. *)
let test_pp buf =
  match
    try Some (Css.Supports.of_string buf)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some cond -> ignore (Css.Supports.to_string cond)

(** compare — must not crash on any pair of parsed conditions. *)
let test_compare buf1 buf2 =
  match
    ( (try Some (Css.Supports.of_string buf1)
       with Failure _ | Invalid_argument _ -> None),
      try Some (Css.Supports.of_string buf2)
      with Failure _ | Invalid_argument _ -> None )
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
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "mixed operator serialization reparses" [ bytes ]
        test_mixed_operator_serialization_reparse;
      test_case "pp crash safety" [ bytes ] test_pp;
      test_case "compare crash safety" [ bytes; bytes ] test_compare;
    ] )
