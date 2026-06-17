open Cascade

let rejects_invalid input =
  let open Css.Container in
  match of_string input with
  | exception Failure _ -> ()
  | query ->
      Alcotest.failf "invalid container query parsed: %s -> %s" input
        (to_string query)

let test_string_output () =
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
    (to_string (of_string "(width > 0px)"));
  Alcotest.(check string)
    "style query raw" "style(--theme: dark)"
    (to_string (of_string "style(--theme: dark)"));
  Alcotest.(check string)
    "range query raw" "(inline-size >= 30em)"
    (to_string (of_string "(inline-size >= 30em)"));
  Alcotest.(check string)
    "scroll-state query raw" "scroll-state(stuck: top)"
    (to_string (of_string "scroll-state(stuck: top)"));
  Alcotest.(check string)
    "named style query raw" "card style(--variant: featured)"
    (to_string (Named ("card", of_string "style(--variant: featured)")));
  (* CSS Conditional Rules 5 §4: [not] takes one query-in-parens, and a
     [style()] function is one, so [not style(--a)] needs no extra wrapping. *)
  Alcotest.(check string)
    "negated style query" "not style(--a)"
    (to_string ~minify:true (of_string "not style(--a)"));
  Alcotest.(check string)
    "named negated style query" "card not style(--variant)"
    (to_string ~minify:true (Named ("card", of_string "not style(--variant)")));
  Alcotest.(check string)
    "chained range query raw" "(30em <= inline-size < 60em)"
    (to_string (of_string "(30em <= inline-size < 60em)"));
  Alcotest.(check string)
    "aspect ratio query raw" "(aspect-ratio > 1/1)"
    (to_string (of_string "(aspect-ratio > 1/1)"))

let spec_container_l3_vectors () =
  let open Css.Container in
  let check_raw name input =
    Alcotest.(check string) name input (to_string (of_string input))
  in
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.row) ->
      check_raw row.branch row.input)
    Cascade_spec_inventory.Query_grammar.container_positive;
  Alcotest.(check string)
    "named size query" "card (inline-size > 30em)"
    (to_string (Named ("card", of_string "(inline-size > 30em)")));
  Alcotest.(check string)
    "named style query" "card style(--variant: featured)"
    (to_string (Named ("card", of_string "style(--variant: featured)")));
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
    "min-width rem is Min_width" true
    (kind (Min_width_rem 24.) = Min_width);
  Alcotest.(check bool)
    "named min-width is Min_width" true
    (kind (Named ("x", Min_width_rem 24.)) = Min_width);
  Alcotest.(check bool)
    "raw is Other" true
    (kind (of_string "style(--theme: dark)") = Other)

let test_spec_container_compare_edges () =
  let open Css.Container in
  let sorted =
    List.sort compare
      [
        of_string "(inline-size > 30em)";
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
      (of_string "(inline-size > 30em)", "(inline-size > 30em)");
      (of_string "(30em <= inline-size < 60em)", "(30em <= inline-size < 60em)");
      (of_string "style(--theme: dark)", "style(--theme: dark)");
      (of_string "scroll-state(stuck: top)", "scroll-state(stuck: top)");
      (Named ("card", of_string "(width >= 400px)"), "card (width >= 400px)");
    ]

let spec_container_query_vectors () =
  let open Css.Container in
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.row) ->
      Alcotest.(check string)
        row.branch row.expected
        (to_string (of_string row.input)))
    Cascade_spec_inventory.Query_grammar.container_positive

let spec_container_invalid_vectors () =
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.invalid_row) ->
      rejects_invalid row.input)
    Cascade_spec_inventory.Query_grammar.container_negative

let tests =
  Alcotest.
    [
      test_case "to_string" `Quick test_string_output;
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
      test_case "spec container invalid query vectors" `Quick
        spec_container_invalid_vectors;
    ]

let suite = ("container", tests)
