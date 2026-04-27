open Cascade
open Css.Media

let test_to_string () =
  Alcotest.(check string)
    "min-width" "(min-width: 640px)"
    (to_string (Min_width 640.));
  Alcotest.(check string)
    "max-width" "(max-width: 768px)"
    (to_string (Max_width 768.));
  Alcotest.(check string)
    "prefers-color-scheme" "(prefers-color-scheme: dark)"
    (to_string (Prefers_color_scheme `Dark));
  Alcotest.(check string)
    "prefers-reduced-motion" "(prefers-reduced-motion: reduce)"
    (to_string (Prefers_reduced_motion `Reduce));
  Alcotest.(check string) "print" "print" (to_string Print);
  Alcotest.(check string)
    "media queries 5 dynamic range" "(dynamic-range: high)"
    (to_string (Raw "(dynamic-range: high)"));
  Alcotest.(check string)
    "media queries 5 reduced data" "(prefers-reduced-data: reduce)"
    (to_string (Raw "(prefers-reduced-data: reduce)"));
  Alcotest.(check string)
    "media queries range comparison" "(width >= 40em)"
    (to_string (Raw "(width >= 40em)"));
  Alcotest.(check string)
    "media queries chained range" "(30em <= width < 60em)"
    (to_string (Raw "(30em <= width < 60em)"));
  Alcotest.(check string)
    "media queries hdr" "(video-dynamic-range: high)"
    (to_string (Raw "(video-dynamic-range: high)"));
  Alcotest.(check string)
    "media queries update" "(update: fast)"
    (to_string (Raw "(update: fast)"));
  Alcotest.(check string)
    "media queries scripting" "(scripting: enabled)"
    (to_string (Raw "(scripting: enabled)"));
  Alcotest.(check string)
    "media queries prefers contrast" "(prefers-contrast: more)"
    (to_string (Raw "(prefers-contrast: more)"))

let spec_media_l45_vectors () =
  let check_raw name input =
    Alcotest.(check string) name input (to_string (Raw input))
  in
  check_raw "range greater than" "(width > 40em)";
  check_raw "range greater equal" "(width >= 40em)";
  check_raw "range less than" "(width < 60em)";
  check_raw "chained inclusive exclusive" "(30em <= width < 60em)";
  check_raw "height range" "(height >= 20rem)";
  check_raw "aspect-ratio range" "(aspect-ratio > 16/9)";
  check_raw "resolution range" "(resolution >= 2dppx)";
  check_raw "boolean color" "(color)";
  check_raw "boolean monochrome" "(monochrome)";
  check_raw "update slow" "(update: slow)";
  check_raw "overflow block paged" "(overflow-block: paged)";
  check_raw "overflow inline scroll" "(overflow-inline: scroll)";
  check_raw "environment blending" "(environment-blending: additive)";
  check_raw "nav controls" "(nav-controls)";
  check_raw "prefers reduced transparency"
    "(prefers-reduced-transparency: reduce)";
  check_raw "prefers reduced data" "(prefers-reduced-data: reduce)";
  Alcotest.(check string)
    "negated print" "not print"
    (to_string (Negated Print));
  Alcotest.(check string)
    "negated min width" "not all and (min-width: 40px)"
    (to_string (Negated (Min_width 40.)));
  Alcotest.(check string)
    "not min width rem" "not all and (min-width: 30rem)"
    (to_string (Not_min_width_rem 30.))

let test_kind () =
  Alcotest.(check bool)
    "min-width is responsive" true
    (match kind (Min_width 640.) with Kind_responsive _ -> true | _ -> false);
  Alcotest.(check bool)
    "prefers-color-scheme is appearance" true
    (match kind (Prefers_color_scheme `Dark) with
    | Kind_preference_appearance -> true
    | _ -> false);
  Alcotest.(check bool)
    "prefers-reduced-motion is accessibility" true
    (match kind (Prefers_reduced_motion `Reduce) with
    | Kind_preference_accessibility -> true
    | _ -> false)

let test_compare () =
  let cmp = compare (Min_width 640.) (Min_width 768.) in
  Alcotest.(check bool) "640 < 768" true (cmp < 0)

let test_spec_media_sorting_edges () =
  let sorted =
    List.sort compare
      [
        Prefers_color_scheme `Dark;
        Min_width 768.;
        Hover;
        Max_width 1024.;
        Prefers_reduced_motion `Reduce;
        Print;
        Not_min_width 768.;
        Min_width 320.;
      ]
    |> List.map to_string
  in
  Alcotest.(check (list string))
    "media ordering keeps interaction, other, preferences, and responsive \
     buckets stable"
    [
      "(hover: hover)";
      "print";
      "(prefers-reduced-motion: reduce)";
      "not all and (min-width: 768px)";
      "(max-width: 1024px)";
      "(min-width: 320px)";
      "(min-width: 768px)";
      "(prefers-color-scheme: dark)";
    ]
    sorted

let spec_media_eval_boundary () =
  let expect_platform condition environment =
    match Css.Stylesheet.evaluate_media_query ~condition ~environment with
    | Error (Css.Stylesheet.Requires_platform_context actual) ->
        Alcotest.(check string)
          "feature" "media query evaluation" actual.feature
    | Error (Css.Stylesheet.Requires_document_context _) ->
        Alcotest.fail "expected platform context"
    | Error (Css.Stylesheet.Unsupported_value_alias _) ->
        Alcotest.fail "expected platform context"
    | Ok _ -> Alcotest.fail "expected media evaluation boundary"
  in
  List.iter
    (fun (condition, environment) -> expect_platform condition environment)
    [
      (Print, "print");
      (Min_width 640., "screen width=800px");
      (Max_width 640., "screen width=320px");
      (Raw "(width >= 40em)", "screen width=50em");
      (Raw "(30em <= width < 60em)", "screen width=45em");
      (Raw "(prefers-reduced-data: reduce)", "reduced-data");
      (Raw "(dynamic-range: high)", "hdr");
      (Negated Print, "screen");
    ]

let spec_media_query_vectors () =
  let raw_cases =
    [
      "(width)";
      "(height)";
      "(color)";
      "(monochrome)";
      "(grid)";
      "(width = 40em)";
      "(40em < width)";
      "(width <= 60em)";
      "(400px <= width <= 1200px)";
      "screen and (width >= 40em), print and (resolution >= 300dpi)";
      "not screen and (hover: hover)";
      "only screen and (pointer: fine)";
    ]
  in
  List.iter
    (fun input -> Alcotest.(check string) input input (to_string (Raw input)))
    raw_cases

let suite =
  let open Alcotest in
  ( "media",
    [
      test_case "to_string" `Quick test_to_string;
      test_case "spec media query level 4/5 vectors" `Quick
        spec_media_l45_vectors;
      test_case "kind" `Quick test_kind;
      test_case "compare" `Quick test_compare;
      test_case "spec media sorting edges" `Quick test_spec_media_sorting_edges;
      test_case "spec media query environment boundary" `Quick
        spec_media_eval_boundary;
      test_case "spec media query boolean and range vectors" `Quick
        spec_media_query_vectors;
    ] )
