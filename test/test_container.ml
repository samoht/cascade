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
    (to_string (Raw "(width > 0px)"))

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

let tests =
  Alcotest.
    [
      test_case "to_string" `Quick test_to_string;
      test_case "compare" `Quick test_compare;
      test_case "kind" `Quick test_kind;
    ]

let suite = ("container", tests)
