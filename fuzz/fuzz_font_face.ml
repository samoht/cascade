(** Fuzz tests for the CSS Font_face module.

    Tests crash safety of font-face descriptor parsing and roundtrip. *)

open Cascade
open Alcobar

let byte_at buf idx =
  if String.length buf = 0 then 0 else Char.code buf.[idx mod String.length buf]

let pick values buf salt =
  List.nth values (byte_at buf salt mod List.length values)

let parse_metric input =
  try Some (Css.Font_face.metric_override_of_string input)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None

let parse_size_adjust input =
  try Some (Css.Font_face.size_adjust_of_string input)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None

let parse_src input =
  try Some (Css.Font_face.src_of_string input)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> None

(** metric_override_of_string — must not crash. *)
let test_metric_override buf =
  try ignore (Css.Font_face.metric_override_of_string buf)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** size_adjust_of_string — must not crash. *)
let test_size_adjust buf =
  try ignore (Css.Font_face.size_adjust_of_string buf)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** src_of_string — must not crash. *)
let test_src buf =
  try ignore (Css.Font_face.src_of_string buf)
  with Reader.Parse_error _ | Invalid_argument _ | Failure _ -> ()

(** metric_override roundtrip: parse → to_string → parse. *)
let test_metric_override_roundtrip buf =
  match parse_metric buf with
  | None -> ()
  | Some m -> (
      let s = Css.Font_face.string_of_metric_override m in
      try ignore (Css.Font_face.metric_override_of_string s)
      with Reader.Parse_error _ | Invalid_argument _ | Failure _ ->
        fail "metric_override roundtrip failed")

(** src roundtrip: parse → to_string → parse. *)
let test_src_roundtrip buf =
  match parse_src buf with
  | None -> ()
  | Some src -> (
      let s = Css.Font_face.string_of_src src in
      try ignore (Css.Font_face.src_of_string s)
      with Reader.Parse_error _ | Invalid_argument _ | Failure _ ->
        fail "src roundtrip failed")

let test_metric_override_non_negative buf =
  match parse_metric buf with
  | None | Some Css.Font_face.Normal -> ()
  | Some (Css.Font_face.Percent p) ->
      if p < 0. then fail "metric override parsed a negative percentage"

let test_size_adjust_non_negative buf =
  match parse_size_adjust buf with
  | None -> ()
  | Some p -> if p < 0. then fail "size-adjust parsed a negative percentage"

let test_metric_override_serialization_idempotent buf =
  match parse_metric buf with
  | None -> ()
  | Some metric ->
      let once = Css.Font_face.string_of_metric_override metric in
      let reparsed = Css.Font_face.metric_override_of_string once in
      let twice = Css.Font_face.string_of_metric_override reparsed in
      if once <> twice then
        failf "metric override serialization changed: %S -> %S" once twice

let test_generated_src_serialization_idempotent buf =
  let entry =
    match byte_at buf 0 mod 4 with
    | 0 -> "local(\"Brand\")"
    | 1 -> "url(\"brand.woff2\") format(\"woff2\")"
    | 2 -> "url(\"color.woff2\") format(\"woff2\") tech(color-COLRv1)"
    | _ -> "url(\"variable.woff2\") tech(variations)"
  in
  let once = Css.Font_face.(entry |> src_of_string |> string_of_src) in
  let twice = Css.Font_face.(once |> src_of_string |> string_of_src) in
  if once <> twice then
    failf "font src serialization changed: %S -> %S" once twice

let test_generated_metric_edge_idempotent buf =
  let input =
    match byte_at buf 0 mod 4 with
    | 0 -> "normal"
    | 1 -> "0%"
    | 2 -> "100%"
    | _ -> "125.5%"
  in
  let metric = Css.Font_face.metric_override_of_string input in
  let once = Css.Font_face.string_of_metric_override metric in
  let twice =
    Css.Font_face.(
      once |> metric_override_of_string |> string_of_metric_override)
  in
  if once <> twice then fail "generated metric serialization drifted"

let test_spec_src_vectors buf =
  let input =
    pick
      [
        "local(\"Brand\")";
        "url(\"brand.woff2\") format(\"woff2\")";
        "url(\"color.woff2\") format(\"woff2\") tech(color-COLRv1)";
        "url(\"variable.woff2\") tech(variations)";
        "local(\"Brand\"), url(\"brand.woff2\") format(\"woff2\")";
      ]
      buf 1
  in
  match parse_src input with
  | None -> failf "valid font-face src vector rejected: %S" input
  | Some src ->
      let serialized = Css.Font_face.string_of_src src in
      let reparsed = Css.Font_face.src_of_string serialized in
      if src <> reparsed then
        failf "font-face src structure changed: %S -> %S" input serialized

let test_invalid_src_vectors buf =
  let input =
    pick
      [
        "format(\"woff2\")";
        "tech(variations)";
        "local()";
        "url(\"font.woff2\") format()";
        "url(\"font.woff2\") tech()";
      ]
      buf 2
  in
  match parse_src input with
  | None -> ()
  | Some src ->
      failf "invalid font-face src vector parsed: %S -> %S" input
        (Css.Font_face.string_of_src src)

let test_spec_metric_vectors buf =
  let input, expected =
    pick
      [
        ("normal", Css.Font_face.Normal);
        ("0%", Css.Font_face.Percent 0.);
        ("100%", Css.Font_face.Percent 100.);
        ("125.5%", Css.Font_face.Percent 125.5);
      ]
      buf 3
  in
  match parse_metric input with
  | Some actual when actual = expected -> ()
  | Some actual ->
      failf "font metric structure changed: %S -> %S" input
        (Css.Font_face.string_of_metric_override actual)
  | None -> failf "valid font metric vector rejected: %S" input

let test_invalid_metric_vectors buf =
  let input = pick [ "-1%"; "auto"; "100"; "calc(1%)" ] buf 4 in
  match parse_metric input with
  | None -> ()
  | Some metric ->
      failf "invalid font metric vector parsed: %S -> %S" input
        (Css.Font_face.string_of_metric_override metric)

let test_spec_size_adjust_vectors buf =
  let input, expected =
    pick [ ("0%", 0.); ("100%", 100.); ("125.5%", 125.5) ] buf 5
  in
  match parse_size_adjust input with
  | Some actual when actual = expected -> ()
  | Some actual ->
      failf "font size-adjust structure changed: %S -> %g" input actual
  | None -> failf "valid font size-adjust vector rejected: %S" input

let test_invalid_size_adjust_vectors buf =
  let input = pick [ "-1%"; "normal"; "auto"; "100"; "calc(100%)" ] buf 6 in
  match parse_size_adjust input with
  | None -> ()
  | Some size_adjust ->
      failf "invalid font size-adjust vector parsed: %S -> %g" input size_adjust

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
      test_case "spec font src vectors" [ bytes ] test_spec_src_vectors;
      test_case "invalid font src vectors rejected" [ bytes ]
        test_invalid_src_vectors;
      test_case "spec font metric vectors" [ bytes ] test_spec_metric_vectors;
      test_case "invalid font metric vectors rejected" [ bytes ]
        test_invalid_metric_vectors;
      test_case "spec size-adjust vectors" [ bytes ]
        test_spec_size_adjust_vectors;
      test_case "invalid size-adjust vectors rejected" [ bytes ]
        test_invalid_size_adjust_vectors;
    ] )
