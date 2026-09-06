open Cascade

let red = Declaration.v Properties.Color (Current : Values.color)
let blue = Declaration.v Properties.Color (Transparent : Values.color)

let solid_outline_width =
  Declaration.v Properties.Outline_width (Px 1.0 : Properties.border_width)

let solid_outline_style =
  Declaration.v Properties.Outline_style (Solid : Properties.outline_style)

let _solid_outline_color =
  Declaration.v Properties.Outline_color (Current : Values.color)

let stub_shorthand = red

(* A shorthand the index has to refuse: CSS Cascade 5 sec. 7.3 gives a CSS-wide
   keyword no meaning beside another component. *)
let mixed_shorthand =
  Declaration.v Properties.Margin ([ Px 1.0; Inherit ] : Values.length list)

let commits name t ~at ~absorbed ~shorthand =
  Alcotest.(check bool) name true (Rule_index.absorb t ~at ~absorbed ~shorthand)

let test_empty_index () =
  let t = Rule_index.build [] in
  Alcotest.(check int) "empty index has length 0" 0 (Rule_index.length t);
  Alcotest.(check int)
    "to_list of empty is empty" 0
    (List.length (Rule_index.to_list t))

let test_positions_in_cascade_order () =
  let t = Rule_index.build [ red; solid_outline_width; blue ] in
  Alcotest.(check (list int))
    "Color positions in cascade order" [ 0; 2 ]
    (Rule_index.positions t Properties.Color);
  Alcotest.(check (list int))
    "Outline_width position" [ 1 ]
    (Rule_index.positions t Properties.Outline_width);
  Alcotest.(check (list int))
    "absent property returns empty" []
    (Rule_index.positions t Properties.Background_color)

let test_absorb_marks_positions () =
  let t = Rule_index.build [ red; blue ] in
  Alcotest.(check bool)
    "position 0 not absorbed initially" false
    (Rule_index.is_absorbed t 0);
  commits "absorb commits" t ~at:0 ~absorbed:[ 0; 1 ] ~shorthand:stub_shorthand;
  Alcotest.(check bool)
    "position 0 carries the shorthand (not 'absorbed')" false
    (Rule_index.is_absorbed t 0);
  Alcotest.(check bool)
    "position 1 reports absorbed" true
    (Rule_index.is_absorbed t 1)

let test_to_list_emits_shorthand_in_place () =
  let t =
    Rule_index.build [ red; solid_outline_width; solid_outline_style; blue ]
  in
  commits "absorb commits" t ~at:1 ~absorbed:[ 1; 2 ] ~shorthand:stub_shorthand;
  let out = Rule_index.to_list t in
  Alcotest.(check int) "length unchanged minus absorbed" 3 (List.length out);
  match out with
  | [ a; b; c ] ->
      Alcotest.(check bool) "first preserved" true (a == red);
      Alcotest.(check bool)
        "shorthand emitted at absorbed earliest position" true
        (b == stub_shorthand);
      Alcotest.(check bool) "last preserved" true (c == blue)
  | _ -> Alcotest.fail "expected 3-element list"

let test_absorb_double_consume_raises () =
  let t = Rule_index.build [ red; blue ] in
  commits "absorb commits" t ~at:0 ~absorbed:[ 0; 1 ] ~shorthand:stub_shorthand;
  let raised =
    try
      let (_ : bool) =
        Rule_index.absorb t ~at:1 ~absorbed:[ 1 ] ~shorthand:stub_shorthand
      in
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "re-absorbing a slot fails" true raised

let test_absorb_refuses_css_wide_mix () =
  let t = Rule_index.build [ red; blue ] in
  Alcotest.(check bool)
    "a shorthand mixing a css-wide keyword is refused" false
    (Rule_index.absorb t ~at:0 ~absorbed:[ 0; 1 ] ~shorthand:mixed_shorthand);
  Alcotest.(check bool)
    "the refused positions stay live" false
    (Rule_index.is_absorbed t 1);
  Alcotest.(check int)
    "the rule keeps both longhands" 2
    (List.length (Rule_index.to_list t))

let suite =
  ( "rule_index",
    [
      Alcotest.test_case "empty" `Quick test_empty_index;
      Alcotest.test_case "positions cascade order" `Quick
        test_positions_in_cascade_order;
      Alcotest.test_case "absorb marks positions" `Quick
        test_absorb_marks_positions;
      Alcotest.test_case "to_list emits shorthand in place" `Quick
        test_to_list_emits_shorthand_in_place;
      Alcotest.test_case "double-absorb raises" `Quick
        test_absorb_double_consume_raises;
      Alcotest.test_case "absorb refuses a css-wide mix" `Quick
        test_absorb_refuses_css_wide_mix;
    ] )
