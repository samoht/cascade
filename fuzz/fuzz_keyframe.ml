(** Fuzz tests for the CSS Keyframe module.

    Tests crash safety of keyframe position/selector parsing and roundtrip. *)

open Cascade
open Alcobar

(** position_of_string — must not crash. *)
let test_position buf = ignore (Css.Keyframe.position_of_string buf)

(** selector_of_string — must not crash (always succeeds, falls back to Raw). *)
let test_selector buf = ignore (Css.Keyframe.selector_of_string buf)

(** position roundtrip: parse → to_string → parse. *)
let test_position_roundtrip buf =
  match Css.Keyframe.position_of_string buf with
  | None -> ()
  | Some pos -> (
      let s = Css.Keyframe.position_to_string pos in
      match Css.Keyframe.position_of_string s with
      | None -> fail "position roundtrip failed"
      | Some pos2 ->
          if Css.Keyframe.position_compare pos pos2 <> 0 then
            fail "position roundtrip mismatch")

(** selector roundtrip: parse → to_string → parse. *)
let test_selector_roundtrip buf =
  let sel = Css.Keyframe.selector_of_string buf in
  let s = Css.Keyframe.selector_to_string sel in
  let sel2 = Css.Keyframe.selector_of_string s in
  if not (Css.Keyframe.selector_equal sel sel2) then
    fail "selector roundtrip mismatch"

let position_in_spec_range = function
  | Css.Keyframe.From | Css.Keyframe.To -> true
  | Css.Keyframe.Percent p -> Float.is_finite p && p >= 0. && p <= 100.

let test_position_spec_range buf =
  match Css.Keyframe.position_of_string buf with
  | None -> ()
  | Some pos ->
      if not (position_in_spec_range pos) then
        fail "keyframe position outside the spec's 0%..100% range"

let test_selector_spec_range buf =
  match Css.Keyframe.selector_of_string buf with
  | Css.Keyframe.Raw _ -> ()
  | Css.Keyframe.Positions positions ->
      if positions = [] then fail "parsed keyframe selector has no positions";
      if not (List.for_all position_in_spec_range positions) then
        fail "keyframe selector contains out-of-range position"

let test_position_serialization_idempotent buf =
  match Css.Keyframe.position_of_string buf with
  | None -> ()
  | Some pos -> (
      let once = Css.Keyframe.position_to_string pos in
      match Css.Keyframe.position_of_string once with
      | None -> fail "serialized keyframe position did not re-parse"
      | Some pos2 ->
          let twice = Css.Keyframe.position_to_string pos2 in
          if once <> twice then fail "keyframe position serialization drifted")

(** position_compare — must not crash on any valid pair. *)
let test_position_compare buf1 buf2 =
  match
    (Css.Keyframe.position_of_string buf1, Css.Keyframe.position_of_string buf2)
  with
  | Some a, Some b ->
      let c = Css.Keyframe.position_compare a b in
      let c' = Css.Keyframe.position_compare b a in
      if c > 0 && c' > 0 then fail "compare not antisymmetric";
      if c < 0 && c' < 0 then fail "compare not antisymmetric";
      if c = 0 && c' <> 0 then fail "compare not antisymmetric"
  | _ -> ()

let suite =
  ( "keyframe",
    [
      test_case "position crash safety" [ bytes ] test_position;
      test_case "selector crash safety" [ bytes ] test_selector;
      test_case "position roundtrip" [ bytes ] test_position_roundtrip;
      test_case "selector roundtrip" [ bytes ] test_selector_roundtrip;
      test_case "position spec range" [ bytes ] test_position_spec_range;
      test_case "selector spec range" [ bytes ] test_selector_spec_range;
      test_case "position serialization idempotent" [ bytes ]
        test_position_serialization_idempotent;
      test_case "position_compare antisymmetry" [ bytes; bytes ]
        test_position_compare;
    ] )
