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

let suite =
  let open Alcotest in
  ( "media",
    [
      test_case "to_string" `Quick test_to_string;
      test_case "kind" `Quick test_kind;
      test_case "compare" `Quick test_compare;
    ] )
