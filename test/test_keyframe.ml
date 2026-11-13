open Cascade
open Css.Keyframe

let test_position_roundtrip () =
  let cases = [ ("from", From); ("to", To); ("50%", Percent 50.) ] in
  List.iter
    (fun (str, expected) ->
      Alcotest.(check string)
        ("position " ^ str) str
        (string_of_position expected))
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

let test_spec_keyframe_position_vectors () =
  let positive =
    [
      ("0%", Some (Percent 0.));
      ("100%", Some (Percent 100.));
      ("0.5%", Some (Percent 0.5));
      ("25.5%", Some (Percent 25.5));
      (" 75% ", Some (Percent 75.));
      ("from", Some From);
      ("to", Some To);
    ]
  in
  List.iter
    (fun (str, expected) ->
      let actual = position_of_string str in
      Alcotest.(check bool)
        ("valid keyframe position " ^ str)
        true
        (match (actual, expected) with
        | Some a, Some b -> position_compare a b = 0
        | None, None -> true
        | _ -> false))
    positive;
  let negative =
    [
      "-0.01%"; "-1%"; "100.01%"; "101%"; "calc(50%)"; "50px"; "middle"; ""; "%";
    ]
  in
  List.iter
    (fun str ->
      Alcotest.(check bool)
        ("invalid keyframe position " ^ str)
        true
        (Option.is_none (position_of_string str)))
    negative

let test_spec_keyframe_selector_vectors () =
  let check_positions name input expected =
    match selector_of_string input with
    | Error e -> Alcotest.failf "%s: %s" name (Error.to_string e)
    | Ok (Positions actual) ->
        Alcotest.(check int)
          (name ^ " length") (List.length expected) (List.length actual);
        List.iter2
          (fun e a ->
            Alcotest.(check int) (name ^ " item") 0 (position_compare e a))
          expected actual
  in
  let check_raw name input =
    match selector_of_string input with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s: expected invalid selector" name
  in
  check_positions "from to list" "from, to" [ From; To ];
  check_positions "percentage list" "0%, 50%, 100%"
    [ Percent 0.; Percent 50.; Percent 100. ];
  check_positions "mixed list" "from, 50%, to" [ From; Percent 50.; To ];
  check_raw "out of range selector" "0%, 101%";
  check_raw "negative selector" "-1%, 50%";
  check_raw "empty item selector" "from,,to";
  check_raw "non-keyframe selector" "from, middle, to"

let test_selector_roundtrip () =
  let sel = Positions [ From; To ] in
  let s = to_string sel in
  Alcotest.(check string) "from, to" "from, to" s

let spec_keyframe_duplicate_offsets () =
  (* Keyframes allow duplicate offsets; the cascade of declarations at a given
     offset is resolved later by the animation engine. The parser-level selector
     model must preserve the authored selector list shape. *)
  let check input expected =
    Alcotest.(check string)
      input expected
      (selector_of_string input |> Css_test_helpers.ok |> to_string)
  in
  check "50%, 50%" "50%, 50%";
  check "from, 0%, 100%, to" "from, 0%, 100%, to";
  check "0.000%, 100.000%" "0%, 100%";
  Alcotest.(check int)
    "from compares as 0%" 0
    (position_compare From (Percent 0.));
  Alcotest.(check int)
    "to compares as 100%" 0
    (position_compare To (Percent 100.));
  Alcotest.(check bool)
    "50% sorts before to" true
    (position_compare (Percent 50.) To < 0)

let spec_keyframe_invalid_edges () =
  (* CSS Animations 1 sec. 3: an empty item, a function, a non-percentage
     dimension, a percentage outside [0,100] and an unknown keyword are not
     keyframe selectors, and the parser says so in its result. *)
  List.iter
    (fun input ->
      match selector_of_string input with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "expected invalid selector: %s" input)
    [
      "from,";
      ",to";
      "from,,to";
      "calc(50%)";
      "50px";
      "0%, 101%";
      "-1%, 50%";
      "from, middle, to";
    ]

(* Scroll-driven Animations 1, Appendix A "Named Timeline Range Keyframe
   Selectors": [<keyframe-selector> = from | to | <percentage [0,100]> |
   <timeline-range-name> <percentage>]. The plain percentage is bounded, the one
   after a range name is not, and Chrome keeps [entry 1e30%] and [entry -1e30%]
   while it drops a plain [1e30%] (curated/keyframe_range_offset_* in
   test/render/parse_recovery.ml).

   A number past 2^62 prints like every other CSS number: [1e30], never C
   printf's [1e+30] or mingw's [1e+030]. CSS Syntax 3 sec. 4.3.13 reads the
   exponent marker in either case and its sign as optional, so [1E30], [1e+30]
   and [1e30] are one value and the shortest spelling is the minifier's; this is
   not CSSOM sec. 6.7.2 serialisation, which never uses an exponent. A magnitude
   too small for the fixed decimals keeps its value rather than printing as
   [0%]. *)
let test_large_offset () =
  Alcotest.(check string)
    "timeline range offset" "entry 1e30%"
    (string_of_position (Timeline_range ("entry", 1e30)));
  let check input expected =
    Alcotest.(check (option string))
      input (Some expected)
      (Option.map string_of_position (position_of_string input))
  in
  check "entry 1E30%" "entry 1e30%";
  check "entry 1e+30%" "entry 1e30%";
  check "entry -1e30%" "entry -1e30%";
  check "exit 4e-33%" "exit 4e-33%";
  check "exit -4e-33%" "exit -4e-33%";
  match selector_of_string "1e30%" with
  | exception Invalid_argument _ -> ()
  | Positions _ -> Alcotest.fail "a plain 1e30% is past <percentage [0,100]>"

let suite =
  let open Alcotest in
  ( "keyframe",
    [
      test_case "position roundtrip" `Quick test_position_roundtrip;
      test_case "position parse" `Quick test_position_parse;
      test_case "spec keyframe position vectors" `Quick
        test_spec_keyframe_position_vectors;
      test_case "spec keyframe selector vectors" `Quick
        test_spec_keyframe_selector_vectors;
      test_case "selector roundtrip" `Quick test_selector_roundtrip;
      test_case "spec keyframe cascade and duplicate offsets" `Quick
        spec_keyframe_duplicate_offsets;
      test_case "spec keyframe invalid selector recovery edges" `Quick
        spec_keyframe_invalid_edges;
      test_case "large offset" `Quick test_large_offset;
    ] )
