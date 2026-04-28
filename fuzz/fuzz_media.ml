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

let test_media_context_shape buf =
  let open Css.Values in
  let ctx =
    {
      Css.Context.empty with
      viewport_width = Some (Px (float_of_int (byte_at buf 0 + 1)));
      viewport_height = Some (Px (float_of_int (byte_at buf 1 + 1)));
    }
  in
  if ctx.viewport_width = None || ctx.viewport_height = None then
    fail "media context dimensions were not preserved"

let test_raw_range_serialization_stable buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_positive buf 0 in
  let raw = row.input in
  let once = Css.Media.to_string (Css.Media.of_string raw) in
  let twice = Css.Media.to_string (Css.Media.of_string once) in
  if once <> twice then
    fail
      (Fmt.str "media of_string/to_string not idempotent: %S vs %S" once twice)

let spec_media_vector buf =
  let open Css.Media in
  let length l = Length l in
  pick
    [
      ("(min-width: 640px)", Min_width 640.);
      ("(max-width: 768px)", Max_width 768.);
      ("(prefers-reduced-motion: reduce)", Prefers_reduced_motion `Reduce);
      ("print", Print);
      ("not print", Negated Print);
      ("not all and (min-width: 40px)", Not_min_width 40.);
      ( "(width > 40em)",
        Custom
          (Cond (Feature (Range ("width", Gt, length (Css.Values.Em 40.))))) );
      ( "(40em < width)",
        Custom
          (Cond (Feature (Range_rev (length (Css.Values.Em 40.), Lt, "width"))))
      );
      ( "(30em <= width < 60em)",
        Custom
          (Cond
             (Feature
                (Interval
                   ( length (Css.Values.Em 30.),
                     Le,
                     "width",
                     Lt,
                     length (Css.Values.Em 60.) )))) );
      ( "screen and (hover: hover)",
        Custom
          (Type
             {
               prefix = None;
               type_ = Screen;
               trailing = Some (Feature (Plain ("hover", Ident "hover")));
             }) );
      ( "screen and (width >= 40em), print",
        Custom
          (List
             [
               Type
                 {
                   prefix = None;
                   type_ = Screen;
                   trailing =
                     Some
                       (Feature
                          (Range ("width", Ge, length (Css.Values.Em 40.))));
                 };
               Type { prefix = None; type_ = Print; trailing = None };
             ]) );
    ]
    buf 0

let test_spec_media_structural_vectors buf =
  let input, expected = spec_media_vector buf in
  let actual = Css.Media.of_string input in
  if not (Css.Media.equal expected actual) then
    fail
      (Fmt.str "media vector parsed to wrong AST: %S -> %S" input
         (Css.Media.to_string actual))

let test_spec_media_invalid_vectors buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_negative buf 0 in
  let input = row.input in
  match
    try Some (Css.Media.of_string input)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some actual ->
      fail
        (Fmt.str "invalid media vector parsed: %S -> %S" input
           (Css.Media.to_string actual))

let test_spec_media_feature_family_vectors buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_positive buf 2 in
  let input = row.input in
  let once = Css.Media.to_string (Css.Media.of_string input) in
  let twice = Css.Media.to_string (Css.Media.of_string once) in
  if once <> twice then
    fail
      (Fmt.str "media feature family serialization drifted: %S -> %S" once twice)

let test_spec_media_invalid_feature_family_vectors buf =
  let valid = pick Cascade_spec_inventory.Query_grammar.media_positive buf 3 in
  let input =
    if byte_at buf 4 mod 2 = 0 then
      (pick Cascade_spec_inventory.Query_grammar.media_negative buf 5).input
    else
      Cascade_spec_inventory.Query_grammar.mutate_invalid valid (byte_at buf 6)
  in
  match
    try Some (Css.Media.of_string input)
    with Failure _ | Invalid_argument _ -> None
  with
  | None -> ()
  | Some actual ->
      fail
        (Fmt.str "invalid media feature family vector parsed: %S -> %S" input
           (Css.Media.to_string actual))

let suite =
  ( "media",
    [
      test_case "to_string non-empty" [ bytes ] test_to_string_no_empty;
      test_case "pp matches to_string" [ bytes ] test_pp_matches_to_string;
      test_case "compare antisymmetric" [ bytes ] test_compare_antisymmetric;
      test_case "compare transitive" [ bytes ] test_compare_transitive;
      test_case "negated kind invariant" [ bytes ] test_negated_kind_invariant;
      test_case "media context shape invariant" [ bytes ]
        test_media_context_shape;
      test_case "raw range serialization stable" [ bytes ]
        test_raw_range_serialization_stable;
      test_case "spec media structural vectors" [ bytes ]
        test_spec_media_structural_vectors;
      test_case "spec media invalid vectors rejected" [ bytes ]
        test_spec_media_invalid_vectors;
      test_case "spec media feature family vectors" [ bytes ]
        test_spec_media_feature_family_vectors;
      test_case "spec invalid media feature family vectors rejected" [ bytes ]
        test_spec_media_invalid_feature_family_vectors;
    ] )
