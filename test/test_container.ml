open Cascade

let test_to_string () =
  let open Css.Container in
  Alcotest.(check string)
    "min-width rem" "(min-width:24rem)"
    (to_string (Min_width_rem 24.));
  Alcotest.(check string)
    "min-width px" "(min-width:640px)"
    (to_string (Min_width_px 640));
  Alcotest.(check string)
    "named" "sidebar (min-width:24rem)"
    (to_string (Named ("sidebar", Min_width_rem 24.)));
  Alcotest.(check string)
    "raw" "(width > 0px)"
    (to_string (Raw "(width > 0px)"));
  Alcotest.(check string)
    "style query raw" "style(--theme: dark)"
    (to_string (Raw "style(--theme: dark)"));
  Alcotest.(check string)
    "range query raw" "(inline-size >= 30em)"
    (to_string (Raw "(inline-size >= 30em)"));
  Alcotest.(check string)
    "scroll-state query raw" "scroll-state(stuck: top)"
    (to_string (Raw "scroll-state(stuck: top)"));
  Alcotest.(check string)
    "named style query raw" "card style(--variant: featured)"
    (to_string (Named ("card", Raw "style(--variant: featured)")));
  Alcotest.(check string)
    "chained range query raw" "(30em <= inline-size < 60em)"
    (to_string (Raw "(30em <= inline-size < 60em)"));
  Alcotest.(check string)
    "aspect ratio query raw" "(aspect-ratio > 1/1)"
    (to_string (Raw "(aspect-ratio > 1/1)"))

let spec_container_l3_vectors () =
  let open Css.Container in
  let check_raw name input =
    Alcotest.(check string) name input (to_string (Raw input))
  in
  check_raw "inline-size lower bound" "(inline-size > 30em)";
  check_raw "width range" "(width >= 400px)";
  check_raw "height range" "(height < 50rem)";
  check_raw "chained range" "(30em <= inline-size < 60em)";
  check_raw "orientation" "(orientation: landscape)";
  check_raw "aspect ratio" "(aspect-ratio > 16/9)";
  check_raw "style query custom property" "style(--theme: dark)";
  check_raw "style query declaration" "style(color: red)";
  check_raw "style query boolean custom property" "style(--featured)";
  check_raw "scroll-state stuck" "scroll-state(stuck: top)";
  check_raw "scroll-state snapped" "scroll-state(snapped: y)";
  Alcotest.(check string)
    "named size query" "card (inline-size > 30em)"
    (to_string (Named ("card", Raw "(inline-size > 30em)")));
  Alcotest.(check string)
    "named style query" "card style(--variant: featured)"
    (to_string (Named ("card", Raw "style(--variant: featured)")));
  Alcotest.(check string)
    "nested names remain explicit" "outer inner (min-width:640px)"
    (to_string (Named ("outer", Named ("inner", Min_width_px 640))))

let test_compare () =
  let open Css.Container in
  Alcotest.(check int)
    "same rem" 0
    (compare (Min_width_rem 24.) (Min_width_rem 24.));
  Alcotest.(check bool)
    "smaller rem < larger rem" true
    (compare (Min_width_rem 24.) (Min_width_rem 48.) < 0);
  Alcotest.(check bool)
    "rem < px" true
    (compare (Min_width_rem 24.) (Min_width_px 640) < 0);
  Alcotest.(check bool)
    "px < named" true
    (compare (Min_width_px 640) (Named ("x", Min_width_rem 24.)) < 0)

let test_kind () =
  let open Css.Container in
  Alcotest.(check bool)
    "min-width rem is Kind_min_width" true
    (kind (Min_width_rem 24.) = Kind_min_width);
  Alcotest.(check bool)
    "named min-width is Kind_min_width" true
    (kind (Named ("x", Min_width_rem 24.)) = Kind_min_width);
  Alcotest.(check bool) "raw is Kind_other" true (kind (Raw "foo") = Kind_other)

let test_spec_container_compare_edges () =
  let open Css.Container in
  let sorted =
    List.sort compare
      [
        Raw "(inline-size > 30em)";
        Named ("card", Min_width_rem 24.);
        Min_width_px 640;
        Min_width_rem 24.;
        Named ("aside", Min_width_rem 24.);
      ]
    |> List.map to_string
  in
  Alcotest.(check (list string))
    "container ordering keeps typed breakpoints before named and raw queries"
    [
      "(min-width:24rem)";
      "(min-width:640px)";
      "aside (min-width:24rem)";
      "card (min-width:24rem)";
      "(inline-size > 30em)";
    ]
    sorted

let spec_container_context_vectors () =
  let open Css.Container in
  List.iter
    (fun (condition, expected) ->
      Alcotest.(check string)
        "container condition syntax" expected (to_string condition))
    [
      (Min_width_px 400, "(min-width:400px)");
      (Min_width_rem 30., "(min-width:30rem)");
      (Raw "(inline-size > 30em)", "(inline-size > 30em)");
      (Raw "(30em <= inline-size < 60em)", "(30em <= inline-size < 60em)");
      (Raw "style(--theme: dark)", "style(--theme: dark)");
      (Raw "scroll-state(stuck: top)", "scroll-state(stuck: top)");
      (Named ("card", Raw "(width >= 400px)"), "card (width >= 400px)");
    ]

let spec_container_query_vectors () =
  let open Css.Container in
  let raw_cases =
    [
      "(width)";
      "(height)";
      "(inline-size)";
      "(block-size >= 20rem)";
      "(400px <= width <= 1200px)";
      "(orientation: portrait)";
      "(aspect-ratio > 16/9)";
      "style(color: red)";
      "style(--theme)";
      "style(--theme: dark)";
      "scroll-state(stuck: top)";
      "scroll-state(snapped: block)";
    ]
  in
  List.iter
    (fun input -> Alcotest.(check string) input input (to_string (Raw input)))
    raw_cases

let tests =
  Alcotest.
    [
      test_case "to_string" `Quick test_to_string;
      test_case "spec container query level 3 vectors" `Quick
        spec_container_l3_vectors;
      test_case "compare" `Quick test_compare;
      test_case "kind" `Quick test_kind;
      test_case "spec container compare edges" `Quick
        test_spec_container_compare_edges;
      test_case "spec container query context syntax vectors" `Quick
        spec_container_context_vectors;
      test_case "spec container query boolean/range/style vectors" `Quick
        spec_container_query_vectors;
    ]

let suite = ("container", tests)
