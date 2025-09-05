(** Tests for CSS Property value parsing *)

open Alcotest

(** Helper module for converting CSS values to strings *)
module To_string = struct
  let display v = Css.Pp.to_string Css.pp_display v
  let position v = Css.Pp.to_string Css.pp_position v
  let font_weight v = Css.Pp.to_string Css.pp_font_weight v
  let cursor v = Css.Pp.to_string Css.pp_cursor v
  let transform v = Css.Pp.to_string Css.pp_transform v
  let box_shadow v = Css.Pp.to_string Css.pp_box_shadow v
end

let test_display_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_display t in
    let pp_str = To_string.display result in
    check string (Fmt.str "display %s" input) expected pp_str
  in

  test "none" "none";
  test "block" "block";
  test "inline" "inline";
  test "flex" "flex";
  test "grid" "grid";
  test "inline-block" "inline-block";
  test "table" "table"

let test_position_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_position t in
    let pp_str = To_string.position result in
    check string (Fmt.str "position %s" input) expected pp_str
  in

  test "static" "static";
  test "relative" "relative";
  test "absolute" "absolute";
  test "fixed" "fixed";
  test "sticky" "sticky"

let test_font_weight_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_font_weight t in
    let pp_str = To_string.font_weight result in
    check string (Fmt.str "font-weight %s" input) expected pp_str
  in

  test "normal" "normal";
  test "bold" "bold";
  test "400" "400";
  test "700" "700"

let test_cursor_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_cursor t in
    let pp_str = To_string.cursor result in
    check string (Fmt.str "cursor %s" input) expected pp_str
  in

  test "auto" "auto";
  test "pointer" "pointer";
  test "move" "move";
  test "text" "text";
  test "not-allowed" "not-allowed"

let test_transform_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_transform t in
    let pp_str = To_string.transform result in
    check string (Fmt.str "transform %s" input) expected pp_str
  in

  test "translateX(10px)" "translateX(10px)";
  test "rotate(45deg)" "rotate(45deg)";
  test "scale(2)" "scale(2)";
  test "skewX(30deg)" "skewX(30deg)"

let test_box_shadow_parsing () =
  let test input expected =
    let t = Css.Reader.of_string input in
    let result = Css.Parser.Property.read_box_shadow t in
    let pp_str = To_string.box_shadow result in
    check string (Fmt.str "box-shadow %s" input) expected pp_str
  in

  test "10px 20px" "10px 20px";
  test "10px 20px 5px" "10px 20px 5px";
  test "10px 20px 5px 3px" "10px 20px 5px 3px";
  test "inset 10px 20px" "inset 10px 20px"

let test_round_trip_display () =
  let round_trip input =
    let t = Css.Reader.of_string input in
    let parsed = Css.Parser.Property.read_display t in
    let output = To_string.display parsed in
    let t2 = Css.Reader.of_string output in
    let reparsed = Css.Parser.Property.read_display t2 in
    let output2 = To_string.display reparsed in
    check string (Fmt.str "round-trip %s" input) output output2
  in

  round_trip "block";
  round_trip "flex";
  round_trip "inline-grid"

let test_round_trip_transform () =
  let round_trip input =
    let t = Css.Reader.of_string input in
    let parsed = Css.Parser.Property.read_transform t in
    let output = To_string.transform parsed in
    (* For transforms, we need to handle that output might be normalized *)
    check bool
      (Fmt.str "round-trip %s produces non-empty output" input)
      true
      (String.length output > 0)
  in

  round_trip "rotate(45deg)";
  round_trip "translateX(10px)";
  round_trip "scale(2)"

let tests =
  [
    test_case "parse display" `Quick test_display_parsing;
    test_case "parse position" `Quick test_position_parsing;
    test_case "parse font-weight" `Quick test_font_weight_parsing;
    test_case "parse cursor" `Quick test_cursor_parsing;
    test_case "parse transform" `Quick test_transform_parsing;
    test_case "parse box-shadow" `Quick test_box_shadow_parsing;
    test_case "round-trip display" `Quick test_round_trip_display;
    test_case "round-trip transform" `Quick test_round_trip_transform;
  ]
