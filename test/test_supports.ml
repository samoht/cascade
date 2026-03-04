open Css.Supports

let test_to_string () =
  let cases =
    [
      (Property ("display", "grid"), "(display: grid)");
      (Not (Property ("display", "grid")), "not (display: grid)");
    ]
  in
  List.iter
    (fun (supports, expected) ->
      Alcotest.(check string) expected expected (to_string supports))
    cases

let test_and_or () =
  let a = Property ("display", "grid") in
  let b = Property ("gap", "1rem") in
  let combined = And (a, b) in
  let s = to_string combined in
  Alcotest.(check bool) "and contains both" true (String.length s > 0);
  let ored = Or (a, b) in
  let s2 = to_string ored in
  Alcotest.(check bool) "or contains both" true (String.length s2 > 0)

let suite =
  let open Alcotest in
  ( "supports",
    [
      test_case "to_string" `Quick test_to_string;
      test_case "and/or" `Quick test_and_or;
    ] )
