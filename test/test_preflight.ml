open Cascade
open Css_test_helpers

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let test_empty_input () =
  let t = Preflight.summarize Size.default [] in
  Alcotest.(check int) "no declarations" 0 (Preflight.declaration_count t);
  Alcotest.(check int) "no source units" 0 (Preflight.source_units t);
  Alcotest.(check int) "no estimated gain" 0 (Preflight.estimated_gain t);
  Alcotest.(check bool) "empty input is useful" true (Preflight.useful t)

let test_small_input_always_useful () =
  let rs = rules ".a{color:red}.b{width:1px}" in
  let t = Preflight.summarize Size.default rs in
  Alcotest.(check bool)
    "two-decl input below threshold is useful" true (Preflight.useful t);
  Alcotest.(check bool)
    "declaration_count is non-negative" true
    (Preflight.declaration_count t >= 0)

let test_declaration_count_scales () =
  let single = Preflight.summarize Size.default (rules ".a{color:red}") in
  let many =
    Preflight.summarize Size.default
      (rules ".a{color:red;width:1px;height:2px;margin:0;padding:0}")
  in
  Alcotest.(check bool)
    "more declarations counted" true
    (Preflight.declaration_count many > Preflight.declaration_count single)

(* Most segments are below the threshold, where the gains are never consulted,
   so summarizing one costs its counters and no hash tables: a few dozen words,
   where two tables of the default size alone take over a thousand. *)
let test_small_segment_builds_no_tables () =
  let rs =
    rules
      (String.concat ""
         (List.init 20 (fun i -> Fmt.str ".c%d{color:red;width:%dpx}" i i)))
  in
  let words = measure (fun () -> Preflight.summarize Size.default rs) in
  if words > 100. then
    Alcotest.failf "summarizing 20 rules allocated %.0f words" words

let test_small_threshold_positive () =
  Alcotest.(check bool)
    "small threshold is a positive constant" true
    (Preflight.small_declaration_threshold > 0)

let suite =
  ( "preflight",
    [
      Alcotest.test_case "empty input" `Quick test_empty_input;
      Alcotest.test_case "small segment builds no tables" `Quick
        test_small_segment_builds_no_tables;
      Alcotest.test_case "small input is always useful" `Quick
        test_small_input_always_useful;
      Alcotest.test_case "declaration count scales with rules" `Quick
        test_declaration_count_scales;
      Alcotest.test_case "small threshold is positive" `Quick
        test_small_threshold_positive;
    ] )
