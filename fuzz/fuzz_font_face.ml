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

let test_metric_override_non_negative buf =
  match
    try Some (Css.Font_face.metric_override_of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None
  with
  | None | Some Css.Font_face.Normal -> ()
  | Some (Css.Font_face.Percent p) ->
      if p < 0. then fail "metric override parsed a negative percentage"

let test_size_adjust_non_negative buf =
  match
    try Some (Css.Font_face.size_adjust_of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None
  with
  | None -> ()
  | Some p -> if p < 0. then fail "size-adjust parsed a negative percentage"

let test_metric_override_serialization_idempotent buf =
  match
    try Some (Css.Font_face.metric_override_of_string buf)
    with Css.Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None
  with
  | None -> ()
  | Some metric ->
      let once = Css.Font_face.metric_override_to_string metric in
      let reparsed = Css.Font_face.metric_override_of_string once in
      let twice = Css.Font_face.metric_override_to_string reparsed in
      if once <> twice then
        fail
          (Fmt.str "metric override serialization changed: %S -> %S" once twice)

let test_generated_src_serialization_idempotent buf =
  let entry =
    match (if String.length buf = 0 then 0 else Char.code buf.[0]) mod 4 with
    | 0 -> "local(\"Brand\")"
    | 1 -> "url(\"brand.woff2\") format(\"woff2\")"
    | 2 -> "url(\"color.woff2\") format(\"woff2\") tech(color-COLRv1)"
    | _ -> "url(\"variable.woff2\") tech(variations)"
  in
  let once = Css.Font_face.(entry |> src_of_string |> src_to_string) in
  let twice = Css.Font_face.(once |> src_of_string |> src_to_string) in
  if once <> twice then
    fail (Fmt.str "font src serialization changed: %S -> %S" once twice)

let test_generated_metric_edge_idempotent buf =
  let input =
    match (if String.length buf = 0 then 0 else Char.code buf.[0]) mod 5 with
    | 0 -> "normal"
    | 1 -> "0%"
    | 2 -> "100%"
    | 3 -> "125.5%"
    | _ -> "-1%"
  in
  let metric = Css.Font_face.metric_override_of_string input in
  let once = Css.Font_face.metric_override_to_string metric in
  let twice =
    Css.Font_face.(
      once |> metric_override_of_string |> metric_override_to_string)
  in
  if once <> twice then fail "generated metric serialization drifted"

let suite =
  ( "font_face",
    [
      test_case "metric_override crash safety" [ bytes ] test_metric_override;
      test_case "size_adjust crash safety" [ bytes ] test_size_adjust;
      test_case "src crash safety" [ bytes ] test_src;
      test_case "metric_override roundtrip" [ bytes ]
        test_metric_override_roundtrip;
      test_case "src roundtrip" [ bytes ] test_src_roundtrip;
      test_case "metric override non-negative" [ bytes ]
        test_metric_override_non_negative;
      test_case "size-adjust non-negative" [ bytes ]
        test_size_adjust_non_negative;
      test_case "metric override serialization idempotent" [ bytes ]
        test_metric_override_serialization_idempotent;
      test_case "generated src serialization idempotent" [ bytes ]
        test_generated_src_serialization_idempotent;
      test_case "generated metric edge idempotent" [ bytes ]
        test_generated_metric_edge_idempotent;
    ] )
