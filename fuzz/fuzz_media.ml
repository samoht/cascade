(** Fuzz tests for the CSS Media module. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let media buf i =
  let open Css.Media in
  match byte_at buf i mod 22 with
  | 0 -> Min_width (Float.of_int (byte_at buf (i + 1)))
  | 1 -> Max_width (Float.of_int (byte_at buf (i + 1)))
  | 2 -> Not_min_width (Float.of_int (byte_at buf (i + 1)))
  | 3 -> Min_width_rem (Float.of_int (byte_at buf (i + 1)) /. 4.)
  | 4 -> Not_min_width_rem (Float.of_int (byte_at buf (i + 1)) /. 4.)
  | 5 -> Prefers_reduced_motion `No_preference
  | 6 -> Prefers_reduced_motion `Reduce
  | 7 -> Prefers_contrast `More
  | 8 -> Prefers_contrast `Less
  | 9 -> Prefers_color_scheme `Dark
  | 10 -> Prefers_color_scheme `Light
  | 11 -> Forced_colors `Active
  | 12 -> Forced_colors `None
  | 13 -> Inverted_colors `Inverted
  | 14 -> Pointer `Coarse
  | 15 -> Pointer `Fine
  | 16 -> Any_pointer `Coarse
  | 17 -> Scripting `Enabled
  | 18 -> Hover
  | 19 -> Print
  | 20 -> Orientation `Landscape
  | _ ->
      of_string
        (pick
           [
             "(width >= 40em)";
             "(30em <= width < 60em)";
             "(dynamic-range: high)";
             "(prefers-reduced-data: reduce)";
           ]
           buf (i + 2))

let test_to_string_no_empty buf =
  let s = Css.Media.to_string (media buf 0) in
  if s = "" then fail "media serialization produced an empty query"

let test_pp_matches_to_string buf =
  let query = media buf 0 in
  let direct = Css.Media.to_string query in
  let pretty = Css.Pp.to_string Css.Media.pp query in
  if direct <> pretty then
    fail (Fmt.str "media pp/to_string mismatch: %S <> %S" direct pretty)

let test_compare_antisymmetric buf =
  let a = media buf 0 in
  let b = media buf 7 in
  let ab = Css.Media.compare a b in
  let ba = Css.Media.compare b a in
  if ab = 0 && ba <> 0 then fail "media compare equality not symmetric";
  if ab < 0 && ba <= 0 then fail "media compare not antisymmetric";
  if ab > 0 && ba >= 0 then fail "media compare not antisymmetric"

let test_compare_transitive buf =
  let sorted =
    List.sort Css.Media.compare [ media buf 0; media buf 5; media buf 9 ]
  in
  match sorted with
  | [ a; b; c ] ->
      if Css.Media.compare a b > 0 || Css.Media.compare b c > 0 then
        fail "media compare sort result is not ordered"
  | _ -> fail "media sort changed list length"

let test_negated_kind_invariant buf =
  let query = media buf 0 in
  let negated = Css.Media.Negated query in
  if Css.Media.kind query <> Css.Media.kind negated then
    fail "negated media query changed kind bucket"

let test_media_evaluation_stub_identity buf =
  match
    Css.Stylesheet.evaluate_media_query ~condition:(media buf 0)
      ~environment:"screen width=80em"
  with
  | Error (Css.Stylesheet.Requires_platform_context { feature; detail }) ->
      if feature <> "media query evaluation" || detail = "" then
        fail "media evaluation stub lost feature/detail identity"
  | Error (Css.Stylesheet.Requires_document_context _) ->
      fail "media evaluation returned document-context error"
  | Error (Css.Stylesheet.Unsupported_value_alias _) ->
      fail "media evaluation returned value-alias error"
  | Ok _ -> fail "media evaluation stub unexpectedly succeeded"

let test_raw_range_serialization_stable buf =
  let raw =
    pick
      [
        "(width)";
        "(40em < width)";
        "(width <= 60em)";
        "(400px <= width <= 1200px)";
        "screen and (width >= 40em), print";
      ]
      buf 0
  in
  let once = Css.Media.to_string (Css.Media.of_string raw) in
  let twice = Css.Media.to_string (Css.Media.of_string once) in
  if once <> twice then
    fail
      (Fmt.str "media of_string/to_string not idempotent: %S vs %S" once twice)

let suite =
  ( "media",
    [
      test_case "to_string non-empty" [ bytes ] test_to_string_no_empty;
      test_case "pp matches to_string" [ bytes ] test_pp_matches_to_string;
      test_case "compare antisymmetric" [ bytes ] test_compare_antisymmetric;
      test_case "compare transitive" [ bytes ] test_compare_transitive;
      test_case "negated kind invariant" [ bytes ] test_negated_kind_invariant;
      test_case "media evaluation stub identity" [ bytes ]
        test_media_evaluation_stub_identity;
      test_case "raw range serialization stable" [ bytes ]
        test_raw_range_serialization_stable;
    ] )
