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
      test_case "roundtrip" `Quick test_roundtrip;
    ] )
