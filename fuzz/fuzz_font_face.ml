(** Fuzz tests for the CSS Font_face module.

    Tests crash safety of font-face descriptor parsing and roundtrip. *)

open Cascade
open Alcobar

(** metric_override_of_string — must not crash. *)
let test_metric_override buf =
  try ignore (Css.Font_face.metric_override_of_string buf)
  with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** size_adjust_of_string — must not crash. *)
let test_size_adjust buf =
  try ignore (Css.Font_face.size_adjust_of_string buf)
  with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** src_of_string — must not crash. *)
let test_src buf =
  try ignore (Css.Font_face.src_of_string buf)
  with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** metric_override roundtrip: parse → to_string → parse. *)
let test_metric_override_roundtrip buf =
  match
    try Some (Css.Font_face.metric_override_of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None
  with
  | None -> ()
  | Some m -> (
      let s = Css.Font_face.metric_override_to_string m in
      try ignore (Css.Font_face.metric_override_of_string s)
      with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ ->
        fail "metric_override roundtrip failed")

(** src roundtrip: parse → to_string → parse. *)
let test_src_roundtrip buf =
  match
    try Some (Css.Font_face.src_of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None
  with
  | None -> ()
  | Some src -> (
      let s = Css.Font_face.src_to_string src in
      try ignore (Css.Font_face.src_of_string s)
      with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ ->
        fail "src roundtrip failed")

let suite =
  ( "font_face",
    [
      test_case "metric_override crash safety" [ bytes ] test_metric_override;
      test_case "size_adjust crash safety" [ bytes ] test_size_adjust;
      test_case "src crash safety" [ bytes ] test_src;
      test_case "metric_override roundtrip" [ bytes ]
        test_metric_override_roundtrip;
      test_case "src roundtrip" [ bytes ] test_src_roundtrip;
    ] )
