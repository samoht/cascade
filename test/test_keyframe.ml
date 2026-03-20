open Cascade
open Css.Keyframe

let test_position_roundtrip () =
  let cases = [ ("from", From); ("to", To); ("50%", Percent 50.) ] in
  List.iter
    (fun (str, expected) ->
      Alcotest.(check string)
        ("position " ^ str) str
        (position_to_string expected))
    cases

let test_position_parse () =
  let cases =
    [ ("from", Some From); ("to", Some To); ("50%", Some (Percent 50.)) ]
  in
  List.iter
    (fun (str, expected) ->
      let result = position_of_string str in
      Alcotest.(check bool)
        ("parse " ^ str) true
        (match (result, expected) with
        | Some a, Some b -> position_compare a b = 0
        | None, None -> true
        | _ -> false))
    cases

let test_selector_roundtrip () =
  let sel = Positions [ From; To ] in
  let s = selector_to_string sel in
  Alcotest.(check string) "from, to" "from, to" s

let suite =
  let open Alcotest in
  ( "keyframe",
    [
      test_case "position roundtrip" `Quick test_position_roundtrip;
      test_case "position parse" `Quick test_position_parse;
      test_case "selector roundtrip" `Quick test_selector_roundtrip;
    ] )
