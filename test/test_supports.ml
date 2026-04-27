open Cascade
open Css.Supports

let test_to_string () =
  let cases =
    [
      (Property ("display", "grid"), "(display: grid)");
      (Not (Property ("display", "grid")), "(not (display: grid))");
      ( Or (Property ("display", "grid"), Property ("gap", "1rem")),
        "(display: grid) or (gap: 1rem)" );
      ( And (Property ("display", "grid"), Property ("gap", "1rem")),
        "(display: grid) and (gap: 1rem)" );
      (* Tailwind quirk: double space before "or" after Not *)
      ( Or
          ( Not (Property ("-webkit-appearance", "-apple-pay-button")),
            Property ("contain-intrinsic-size", "1px") ),
        "(not (-webkit-appearance: -apple-pay-button))  or \
         (contain-intrinsic-size: 1px)" );
    ]
  in
  List.iter
    (fun (supports, expected) ->
      Alcotest.(check string) expected expected (to_string supports))
    cases

let test_of_string () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name (to_string expected) (to_string actual)
  in
  check "simple property" "(display: grid)" (Property ("display", "grid"));
  check "property no space" "(display:grid)" (Property ("display", "grid"));
  check "not condition" "(not (display: grid))"
    (Not (Property ("display", "grid")));
  check "or condition" "(display: grid) or (gap: 1rem)"
    (Or (Property ("display", "grid"), Property ("gap", "1rem")));
  check "and condition" "(display: grid) and (gap: 1rem)"
    (And (Property ("display", "grid"), Property ("gap", "1rem")));
  check "complex: not or property"
    "(not (-webkit-appearance: -apple-pay-button)) or (contain-intrinsic-size: \
     1px)"
    (Or
       ( Not (Property ("-webkit-appearance", "-apple-pay-button")),
         Property ("contain-intrinsic-size", "1px") ));
  check "nested function value" "(color: color-mix(in lab, red, red))"
    (Property ("color", "color-mix(in lab, red, red)"));
  check "double parens around property" "((-webkit-hyphens: none))"
    (Property ("-webkit-hyphens", "none"));
  check "complex browser detection"
    "((-webkit-hyphens: none) and (not (margin-trim: inline))) or \
     ((-moz-orient: inline) and (not (color: rgb(from red r g b))))"
    (Or
       ( And
           ( Property ("-webkit-hyphens", "none"),
             Not (Property ("margin-trim", "inline")) ),
         And
           ( Property ("-moz-orient", "inline"),
             Not (Property ("color", "rgb(from red r g b)")) ) ))

let test_current_work_vectors () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name expected (to_string actual)
  in
  check "supports selector has" "selector(:has(img))" "selector(:has(img))";
  check "supports font tech" "(font-tech(color-COLRv1))"
    "(font-tech(color-COLRv1))";
  check "supports complex selector and property"
    "selector(:has(img)) and (container-type: inline-size)"
    "selector(:has(img)) and (container-type: inline-size)"

let test_spec_conditional_supports_feature_vectors () =
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check string) name expected (to_string actual)
  in
  check "selector forgiving relative branch" "selector(:has(+ img))"
    "selector(:has(+ img))";
  check "selector current pseudo" "selector(:popover-open)"
    "selector(:popover-open)";
  check "font format feature" "font-format(woff2)" "font-format(woff2)";
  check "font tech feature" "font-tech(variations)" "font-tech(variations)";
  check "general enclosed function" "unknown-feature(foo bar)"
    "unknown-feature(foo bar)";
  check "unknown declaration feature" "(-vendor-flag: enabled)"
    "(-vendor-flag: enabled)";
  check "nested not selector" "not selector(:has(article > img))"
    "(not selector(:has(article > img)))";
  check "and chain" "(display: grid) and (gap: 1rem) and (selector(:has(img)))"
    "(display: grid) and (gap: 1rem) and selector(:has(img))";
  check "or chain"
    "(font-format(woff2)) or (font-tech(color-COLRv1)) or (display: grid)"
    "font-format(woff2) or font-tech(color-COLRv1) or (display: grid)";
  check "grouped and inside or"
    "((display: grid) and (gap: 1rem)) or selector(:has(img))"
    "((display: grid) and (gap: 1rem)) or selector(:has(img))"

let test_spec_conditional_supports_negative_vectors () =
  let expect_error name input =
    try
      ignore (of_string input);
      Alcotest.failf "%s: expected invalid @supports condition" name
    with Failure _ | Invalid_argument _ -> ()
  in
  expect_error "empty condition" "";
  expect_error "empty parentheses" "()";
  expect_error "bare property without parentheses" "display: grid";
  expect_error "missing right operand" "(display: grid) and";
  expect_error "mixed operator without right operand" "(display: grid) or";
  expect_error "unclosed selector function" "selector(:has(img)";
  expect_error "unclosed property feature" "(display: grid"

let test_roundtrip () =
  let cases =
    [
      Property ("display", "grid");
      Not (Property ("display", "grid"));
      Or (Property ("display", "grid"), Property ("gap", "1rem"));
      And (Property ("display", "grid"), Property ("gap", "1rem"));
      Or
        ( Not (Property ("-webkit-appearance", "-apple-pay-button")),
          Property ("contain-intrinsic-size", "1px") );
    ]
  in
  List.iter
    (fun cond ->
      let s = to_string cond in
      let parsed = of_string s in
      Alcotest.(check string) ("roundtrip: " ^ s) s (to_string parsed))
    cases

let suite =
  let open Alcotest in
  ( "supports",
    [
      test_case "to_string" `Quick test_to_string;
      test_case "of_string" `Quick test_of_string;
      test_case "current-work vectors" `Quick test_current_work_vectors;
      test_case "spec conditional supports feature vectors" `Quick
        test_spec_conditional_supports_feature_vectors;
      test_case "spec conditional supports negative vectors" `Quick
        test_spec_conditional_supports_negative_vectors;
      test_case "roundtrip" `Quick test_roundtrip;
    ] )
