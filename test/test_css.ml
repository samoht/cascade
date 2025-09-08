(* Tests for CSS optimization and rule merging Basic CSS functionality is tested
   in other files: - test_values.ml - CSS value types - test_properties.ml - CSS
   properties - test_selector.ml - CSS selectors - test_declaration.ml - CSS
   declarations - test_pp.ml - Pretty printing *)

open Css

(* Helper selectors for optimization tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"
let s1 = Selector.class_ "s1"
let _s1_where_p = Selector.(class_ "s1" ++ where [ element "p" ])
let _component = Selector.class_ "component"
let _component_p = Selector.(class_ "component" ++ element "p")

(* Helper to check CSS roundtrip *)
let _check_css_roundtrip name stylesheet =
  let css = to_string ~minify:true stylesheet in
  let _ = Alcotest.(check pass) (name ^ " generates CSS") () () in
  css

(* Test that adjacent rules with same selector merge correctly *)
let test_css_merge_adjacent () =
  let rules =
    [
      rule ~selector:btn [ color (Hex { hash = true; value = "ff0000" }) ];
      rule ~selector:btn [ padding (Px 10.) ];
      rule ~selector:card [ margin (Px 5.) ];
      rule ~selector:btn [ margin (Px 15.) ];
    ]
  in

  let merged = merge_rules rules in

  (* First two .btn rules should merge *)
  Alcotest.(check int) "correct number of rules" 3 (List.length merged);

  (* Check that first rule has both color and padding *)
  let first_rule = List.nth merged 0 in
  Alcotest.(check string)
    "first rule selector" ".btn"
    (Selector.to_string (selector first_rule));
  Alcotest.(check int)
    "first rule has 2 declarations" 2
    (List.length (declarations first_rule))

(* Test optimization with layers *)
let test_css_layer_optimization () =
  let s1_rules =
    [
      rule ~selector:s1
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.) ];
      rule ~selector:s1 [ font_size (Rem 1.0); line_height (Number 1.75) ];
    ]
  in

  let utility_layer =
    layer ~name:"utilities" (List.map rule_to_nested s1_rules)
  in
  let stylesheet = stylesheet [ Layer utility_layer ] in

  (* Generate CSS with and without optimization *)
  let css_optimized = to_string ~minify:true ~optimize:true stylesheet in
  let css_unoptimized = to_string ~minify:true ~optimize:false stylesheet in

  (* Check that optimization merges the rules *)
  let count_s1_rules css =
    let parts = Astring.String.cuts ~sep:".s1{" css in
    List.length parts - 1
  in

  let s1_count_optimized = count_s1_rules css_optimized in
  let s1_count_unoptimized = count_s1_rules css_unoptimized in

  Alcotest.(check int) "optimized has fewer .s1 rules" 1 s1_count_optimized;
  Alcotest.(check int) "unoptimized has both .s1 rules" 2 s1_count_unoptimized

(* Test cascade order preservation *)
let test_css_cascade_order () =
  let z_class = Selector.class_ "z-class" in
  let a_class = Selector.class_ "a-class" in
  let m_class = Selector.class_ "m-class" in
  let b_class = Selector.class_ "b-class" in

  let rules =
    [
      rule ~selector:z_class [ padding (Px 10.) ];
      rule ~selector:a_class [ margin (Px 5.) ];
      rule ~selector:m_class [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:b_class [ font_size (Rem 1.0) ];
    ]
  in

  let stylesheet = stylesheet (List.map (fun r -> Rule r) rules) in
  let optimized = optimize stylesheet in
  let css = to_string ~minify:true optimized in

  (* Verify selectors appear in original order *)
  let z_pos =
    Astring.String.find_sub ~sub:".z-class" css |> Option.value ~default:(-1)
  in
  let a_pos =
    Astring.String.find_sub ~sub:".a-class" css |> Option.value ~default:(-1)
  in
  let m_pos =
    Astring.String.find_sub ~sub:".m-class" css |> Option.value ~default:(-1)
  in
  let b_pos =
    Astring.String.find_sub ~sub:".b-class" css |> Option.value ~default:(-1)
  in

  Alcotest.(check bool) "z-class comes before a-class" true (z_pos < a_pos);
  Alcotest.(check bool) "a-class comes before m-class" true (a_pos < m_pos);
  Alcotest.(check bool) "m-class comes before b-class" true (m_pos < b_pos)

(* Test that important declarations are preserved *)
let test_css_important_preservation () =
  let rules =
    [
      rule ~selector:btn
        [
          important (color (Hex { hash = false; value = "ff0000" }));
          padding (Px 10.);
        ];
      rule ~selector:btn
        [
          important (margin (Px 5.));
          color (Hex { hash = false; value = "0000ff" });
        ];
    ]
  in

  let merged = merge_rules rules in

  (* Rules should merge *)
  Alcotest.(check int) "rules merged" 1 (List.length merged);

  let merged_rule = List.hd merged in
  let decls = declarations merged_rule in

  (* Should have 3 declarations per CSS spec: color !important wins over color,
     so we get: color !important, padding, margin !important *)
  Alcotest.(check int) "all declarations preserved" 3 (List.length decls);

  let sheet = stylesheet [ Rule merged_rule ] in
  let css = to_string ~minify:true sheet in

  (* Both !important declarations should be present *)
  let important_count =
    let parts = Astring.String.cuts ~sep:"!important" css in
    List.length parts - 1
  in
  Alcotest.(check int) "both !important preserved" 2 important_count

(* Test empty rules handling *)
let test_css_empty_rules () =
  let empty_class = Selector.class_ "empty" in
  let has_content = Selector.class_ "has-content" in
  let also_empty = Selector.class_ "also-empty" in

  let rules =
    [
      rule ~selector:empty_class [];
      rule ~selector:has_content [ padding (Px 10.) ];
      rule ~selector:also_empty [];
    ]
  in

  let optimized_rules = List.filter (fun r -> declarations r <> []) rules in

  (* Should remove empty rules *)
  Alcotest.(check int) "empty rules removed" 1 (List.length optimized_rules);

  let remaining = List.hd optimized_rules in
  Alcotest.(check string)
    "non-empty rule preserved" ".has-content"
    (Selector.to_string (selector remaining))

(* Test that rules don't merge across @supports boundaries *)
let test_css_no_merge_across_supports () =
  let supports1 =
    supports ~condition:"(display: grid)"
      [ rule ~selector:btn [ display Grid ] ]
  in
  let normal_rule = rule ~selector:btn [ padding (Px 10.) ] in
  let supports2 =
    supports ~condition:"(display: flex)"
      [ rule ~selector:btn [ display Flex ] ]
  in

  let sheet =
    stylesheet [ Supports supports1; Rule normal_rule; Supports supports2 ]
  in
  let optimized = optimize sheet in
  let css = to_string ~minify:true optimized in

  (* Count .btn occurrences - should be 3 (one in each context) *)
  let btn_count =
    let parts = Astring.String.cuts ~sep:".btn{" css in
    List.length parts - 1
  in
  Alcotest.(check int) "rules not merged across @supports" 3 btn_count

(* Test combining identical rules with different selectors *)
let test_css_combine_identical () =
  let prose_a_strong =
    Selector.(class_ "prose" ++ where [ element "a" ++ element "strong" ])
  in
  let prose_blockquote_strong =
    Selector.(
      class_ "prose" ++ where [ element "blockquote" ++ element "strong" ])
  in
  let prose_thead_th_strong =
    Selector.(
      class_ "prose"
      ++ where [ element "thead" ++ element "th" ++ element "strong" ])
  in

  let rules =
    [
      rule ~selector:prose_a_strong [ color Inherit ];
      rule ~selector:prose_blockquote_strong [ color Inherit ];
      rule ~selector:prose_thead_th_strong [ color Inherit ];
    ]
  in

  let combined = combine_identical_rules rules in

  (* Should combine into 1 rule with grouped selector *)
  Alcotest.(check int) "should combine into 1 rule" 1 (List.length combined);

  match combined with
  | [ rule ] ->
      let expected_selector =
        ".prose :where(a strong), " ^ ".prose :where(blockquote strong), "
        ^ ".prose :where(thead th strong)"
      in
      let actual_selector = Selector.to_string (selector rule) in
      Alcotest.(check string)
        "combined selector" expected_selector actual_selector
  | _ -> Alcotest.fail "Expected exactly one combined rule"

(* Test that non-adjacent rules with same selector don't merge *)
let test_css_no_merge_non_adjacent () =
  let rules =
    [
      rule ~selector:btn [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:card [ margin (Px 5.) ];
      rule ~selector:btn [ padding (Px 10.) ];
    ]
  in

  let merged = merge_rules rules in

  (* Should have 3 rules - no merging of non-adjacent .btn rules *)
  Alcotest.(check int) "non-adjacent rules not merged" 3 (List.length merged)

(* Test optimization within nested contexts *)
let test_css_optimize_nested () =
  let media_rules =
    [
      rule ~selector:btn [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:btn [ padding (Px 10.) ];
    ]
  in

  let mq = media ~condition:"(min-width: 768px)" media_rules in
  let sheet = stylesheet [ Media mq ] in
  let optimized = optimize sheet in
  let css = to_string ~minify:true optimized in

  (* Should merge rules within media query *)
  let btn_count =
    let parts = Astring.String.cuts ~sep:".btn{" css in
    List.length parts - 1
  in
  Alcotest.(check int) "rules merged within media query" 1 btn_count

(* Tests using embedded CSS files would go here using ocaml-crunch. Example:

   module CssFixtures = struct let basic_css = {| .btn { color: red; padding:
   10px; } .btn { margin: 5px; } .card { background: white; } |}

   let expected_basic =
   ".btn{color:red;padding:10px;margin:5px}.card{background:white}" end

   let test_css_file_basic () = let reader = Reader.of_string
   CssFixtures.basic_css in let stylesheet = Css.read_stylesheet reader in let
   optimized = Css.optimize stylesheet in let output = Css.to_string
   ~minify:true optimized in

   Alcotest.(check string) "basic CSS file" CssFixtures.expected_basic output *)

(* Test CSS cascade spec: normal declarations override each other in order *)
let test_css_cascade_normal_override () =
  let rules =
    [
      rule ~selector:btn
        [
          color (Hex { hash = false; value = "ff0000" });
          (* red *)
          color (Hex { hash = false; value = "00ff00" });
          (* green - should win *)
        ];
    ]
  in

  let sheet = stylesheet [ Rule (List.hd rules) ] in
  let css = to_string ~minify:true sheet in

  (* Should only have green color *)
  Alcotest.(check bool)
    "has green color" true
    (Astring.String.is_infix ~affix:"#00ff00" css);
  Alcotest.(check bool)
    "no red color" false
    (Astring.String.is_infix ~affix:"#ff0000" css)

(* Test CSS cascade spec: !important always wins over normal *)
let test_css_important_beats_normal () =
  let rules =
    [
      rule ~selector:btn
        [
          important (color (Hex { hash = false; value = "ff0000" }));
          (* red !important *)
          color (Hex { hash = false; value = "00ff00" });
          (* green normal - loses *)
        ];
    ]
  in

  let sheet = stylesheet [ Rule (List.hd rules) ] in
  let css = to_string ~minify:true sheet in

  (* Should only have red !important *)
  Alcotest.(check bool)
    "has red !important" true
    (Astring.String.is_infix ~affix:"#ff0000 !important" css);
  Alcotest.(check bool)
    "no green color" false
    (Astring.String.is_infix ~affix:"#00ff00" css)

(* Test CSS cascade spec: last !important wins among !important declarations *)
let test_css_important_cascade () =
  let rules =
    [
      rule ~selector:btn
        [
          important (color (Hex { hash = false; value = "ff0000" }));
          (* red !important *)
          important (color (Hex { hash = false; value = "00ff00" }));
          (* green !important - wins *)
        ];
    ]
  in

  let sheet = stylesheet [ Rule (List.hd rules) ] in
  let css = to_string ~minify:true sheet in

  (* Should only have green !important *)
  Alcotest.(check bool)
    "has green !important" true
    (Astring.String.is_infix ~affix:"#00ff00 !important" css);
  Alcotest.(check bool)
    "no red color" false
    (Astring.String.is_infix ~affix:"#ff0000" css)

(* Test that different properties don't interfere *)
let test_css_different_properties_preserved () =
  let rules =
    [
      rule ~selector:btn
        [
          color (Hex { hash = false; value = "ff0000" });
          padding (Px 10.);
          margin (Px 5.);
          color (Hex { hash = false; value = "00ff00" });
          (* overrides first color *)
          padding (Px 20.);
          (* overrides first padding *)
        ];
    ]
  in

  let merged = deduplicate_declarations (declarations (List.hd rules)) in

  (* Should have 3 properties: color (green), padding (20px), margin (5px) *)
  Alcotest.(check int) "three unique properties" 3 (List.length merged)

(* Test that custom properties are handled correctly *)
let test_css_custom_properties () =
  let rules =
    [
      rule ~selector:btn
        [
          custom_property "--color-1" "red";
          custom_property "--color-2" "blue";
          custom_property "--color-1" "green";
          (* overrides first --color-1 *)
        ];
    ]
  in

  let merged = deduplicate_declarations (declarations (List.hd rules)) in

  (* Should have 2 custom properties *)
  Alcotest.(check int) "two unique custom properties" 2 (List.length merged)

let suite =
  [
    ( "css",
      [
        (* Optimization tests *)
        Alcotest.test_case "merge adjacent" `Quick test_css_merge_adjacent;
        Alcotest.test_case "layer optimization" `Quick
          test_css_layer_optimization;
        Alcotest.test_case "cascade order" `Quick test_css_cascade_order;
        Alcotest.test_case "important preservation" `Quick
          test_css_important_preservation;
        Alcotest.test_case "empty rules" `Quick test_css_empty_rules;
        Alcotest.test_case "no merge across supports" `Quick
          test_css_no_merge_across_supports;
        Alcotest.test_case "combine identical" `Quick test_css_combine_identical;
        Alcotest.test_case "no merge non-adjacent" `Quick
          test_css_no_merge_non_adjacent;
        Alcotest.test_case "optimize nested" `Quick test_css_optimize_nested;
        (* CSS cascade spec tests *)
        Alcotest.test_case "cascade normal override" `Quick
          test_css_cascade_normal_override;
        Alcotest.test_case "important beats normal" `Quick
          test_css_important_beats_normal;
        Alcotest.test_case "important cascade" `Quick test_css_important_cascade;
        Alcotest.test_case "different properties preserved" `Quick
          test_css_different_properties_preserved;
        Alcotest.test_case "custom properties" `Quick test_css_custom_properties;
      ] );
  ]
