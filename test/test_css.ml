(* Tests for CSS module *)
open Css

(* Basic helpers to standardize checks in this file, similar to other suites. *)

(* Toplevel selectors for reuse *)
let test = Selector.class_ "test"
let responsive = Selector.class_ "responsive"
let grid = Selector.class_ "grid"
let container_item = Selector.class_ "container-item"
let spin = Selector.class_ "spin"
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"
let s1 = Selector.class_ "s1"
let s1_where_p = Selector.(class_ "s1" ++ where [ element "p" ])
let s1_complex_p = Selector.(class_ "s1" ++ element "p")
let s1_complex_h1 = Selector.(class_ "s1" ++ element "h1")

let s1_where_a_strong =
  Selector.(class_ "s1" ++ where [ element "a" ++ element "strong" ])

let s1_where_blockquote_strong =
  Selector.(class_ "s1" ++ where [ element "blockquote" ++ element "strong" ])

let s1_where_thead_th_strong =
  Selector.(
    class_ "s1" ++ where [ element "thead" ++ element "th" ++ element "strong" ])

let diff_selectorerent = Selector.class_ "diff_selectorerent"
let component = Selector.class_ "component"
let component_p = Selector.(class_ "component" ++ element "p")
let component_h1 = Selector.(class_ "component" ++ element "h1")
let component_h2 = Selector.(class_ "component" ++ element "h2")
let component_a = Selector.(class_ "component" ++ element "a")
let component_strong = Selector.(class_ "component" ++ element "strong")
let universal = Selector.universal
let btn_primary = Selector.class_ "btn-primary"
let btn_secondary = Selector.class_ "btn-secondary"
let base = Selector.class_ "base"
let override = Selector.class_ "override"
let a = Selector.class_ "a"
let b = Selector.class_ "b"
let c = Selector.class_ "c"
let btn_hover = Selector.(class_ "btn" && pseudo_class "hover")
let feature = Selector.class_ "feature"
let critical = Selector.class_ "critical"
let normal = Selector.class_ "normal"
let empty_class = Selector.class_ "empty"
let has_content = Selector.class_ "has-content"
let also_empty = Selector.class_ "also-empty"
let another = Selector.class_ "another"
let other = Selector.class_ "other"
let component_where_p = Selector.(class_ "component" ++ where [ element "p" ])

let component_where_component_last_child =
  Selector.(
    class_ "component"
    ++ where [ class_ "component" >> pseudo_class "last-child" ])

let z_class = Selector.class_ "z-class"
let a_class = Selector.class_ "a-class"
let m_class = Selector.class_ "m-class"
let b_class = Selector.class_ "b-class"
let margins = Selector.class_ "margins"
let colors = Selector.class_ "colors"
let more_margins = Selector.class_ "more-margins"
let paddings = Selector.class_ "paddings"
let more_colors = Selector.class_ "more-colors"
let s2 = Selector.class_ "s2"
let s3 = Selector.class_ "s3"
let s4 = Selector.class_ "s4"
let s5 = Selector.class_ "s5"
let s6 = Selector.class_ "s6"

let prose_a_strong =
  Selector.(class_ "prose" ++ where [ element "a" ++ element "strong" ])

let prose_blockquote_strong =
  Selector.(
    class_ "prose" ++ where [ element "blockquote" ++ element "strong" ])

let prose_thead_th_strong =
  Selector.(
    class_ "prose"
    ++ where [ element "thead" ++ element "th" ++ element "strong" ])

(* Generic debug helper for CSS tests *)
let with_debug test_name f =
  try f ()
  with e ->
    Fmt.pr "@[<v>@,=== TEST FAILURE DEBUG: %s ===@," test_name;
    Fmt.pr "Exception: %s@," (Printexc.to_string e);
    Fmt.pr "Backtrace:@,%s@," (Printexc.get_backtrace ());
    Fmt.pr "==========================================@,@]@.";
    raise e

(* Helper to debug stylesheet transformations *)
let debug_stylesheet ?(label = "") original optimized =
  Fmt.pr "@[<v>@,=== STYLESHEET DEBUG%s ===@,"
    (if label = "" then "" else ": " ^ label);
  Fmt.pr "Original rules: %d@," (List.length (stylesheet_rules original));
  Fmt.pr "Optimized rules: %d@," (List.length (stylesheet_rules optimized));
  Fmt.pr "@,Original CSS:@,%s@," (to_string ~minify:true original);
  Fmt.pr "@,Optimized CSS:@,%s@," (to_string ~minify:true optimized);
  Fmt.pr "==========================================@,@]@."

(* Helper to debug CSS string output *)
let debug_css ?(label = "") css_string expected_patterns =
  Fmt.pr "@[<v>@,=== CSS OUTPUT DEBUG%s ===@,"
    (if label = "" then "" else ": " ^ label);
  Fmt.pr "CSS length: %d chars@," (String.length css_string);
  Fmt.pr "CSS output:@,%s@," css_string;
  Fmt.pr "@,Expected patterns:@,";
  List.iter
    (fun pattern ->
      let found = Astring.String.is_infix ~affix:pattern css_string in
      Fmt.pr "  '%s': %s@," pattern (if found then "FOUND" else "NOT FOUND"))
    expected_patterns;
  Fmt.pr "==========================================@,@]@."

(* Roundtrip: pp -> parse -> pp should be stable (minified) *)
let check_css_roundtrip name sheet =
  let s1 = to_string ~minify:true sheet in
  let parsed = Css_parser.of_string_exn s1 in
  let s2 = to_string ~minify:true parsed in
  Alcotest.(check string) name s1 s2;
  s1

let test_property_creation () =
  let color_prop = color (Hex { hash = true; value = "ff0000" }) in
  let padding_prop = padding (Px 10.) in

  (* Verify properties are created correctly *)
  let sheet1 =
    stylesheet [ Rule (rule ~selector:(Selector.class_ "test") [ color_prop ]) ]
  in
  let sheet2 =
    stylesheet
      [ Rule (rule ~selector:(Selector.class_ "test") [ padding_prop ]) ]
  in
  let css1 = check_css_roundtrip "rt property creation 1" sheet1 in
  let css2 = check_css_roundtrip "rt property creation 2" sheet2 in

  Alcotest.(check bool)
    "color property creates valid CSS" true
    (Astring.String.is_infix ~affix:"color:" css1);
  Alcotest.(check bool)
    "padding property creates valid CSS" true
    (Astring.String.is_infix ~affix:"padding:" css2)

let test_property_deduplication () =
  let props =
    [
      color (Hex { hash = true; value = "ff0000" });
      padding (Px 10.);
      color (Hex { hash = true; value = "0000ff" });
      (* Should override first color *)
      margin (Px 5.);
    ]
  in

  let deduped = deduplicate_declarations props in

  (* Should have 3 properties after deduplication *)
  Alcotest.(check int)
    "deduplication removes duplicates" 3 (List.length deduped);

  (* Create a stylesheet to check the output *)
  let sheet =
    stylesheet [ Rule (rule ~selector:(Selector.class_ "test") deduped) ]
  in
  let css = check_css_roundtrip "rt merge rules" sheet in

  (* Should have blue, not red *)
  Alcotest.(check bool)
    "last property wins" true
    (Astring.String.is_infix ~affix:"#0000ff" css);
  Alcotest.(check bool)
    "red was overridden" false
    (Astring.String.is_infix ~affix:"#ff0000" css)

let test_minification () =
  let test_rule =
    rule ~selector:test
      [
        padding Zero;
        margin Zero;
        color (Rgb { r = Int 255; g = Int 0; b = Int 0 });
      ]
  in

  let sheet = stylesheet [ Rule test_rule ] in
  let normal = to_string ~minify:false sheet in
  let minified = check_css_roundtrip "rt minification" sheet in

  (* Minified should be shorter *)
  Alcotest.(check bool)
    "minified is shorter" true
    (String.length minified < String.length normal);

  (* Minified should not have newlines or extra spaces *)
  Alcotest.(check bool)
    "no newlines in minified" false
    (String.contains minified '\n');

  (* Should still contain the selector and properties *)
  Alcotest.(check bool)
    "contains selector" true
    (Astring.String.is_infix ~affix:".test" minified);
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color:" minified)

let test_media_query () =
  let rules = [ rule ~selector:responsive [ padding (Px 20.) ] ] in
  let mq = media ~condition:"(min-width: 768px)" rules in
  let sheet = stylesheet [ Media mq ] in
  let output = check_css_roundtrip "rt media query" sheet in

  (* Should contain media query *)
  Alcotest.(check bool)
    "contains media query" true
    (Astring.String.is_infix ~affix:"@media (min-width: 768px)" output);

  (* Should contain nested rule *)
  Alcotest.(check bool)
    "contains nested selector" true
    (Astring.String.is_infix ~affix:".responsive" output)

let test_inline_style () =
  let props =
    [
      color (Hex { hash = true; value = "ff0000" });
      padding (Px 10.);
      margin (Px 5.);
    ]
  in

  let inline = inline_style_of_declarations props in

  (* Should create an inline style string *)
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color: #ff0000" inline);
  Alcotest.(check bool)
    "contains padding" true
    (Astring.String.is_infix ~affix:"padding: 10px" inline);
  Alcotest.(check bool)
    "contains margin" true
    (Astring.String.is_infix ~affix:"margin: 5px" inline);

  (* Should be semicolon-separated *)
  Alcotest.(check bool) "has semicolons" true (String.contains inline ';')

let test_grid_template () =
  let template_cols = Tracks [ Fr 1.0; Track_size (Px 200.); Fr 2.0 ] in
  let prop = grid_template_columns template_cols in
  let sheet = stylesheet [ Rule (rule ~selector:grid [ prop ]) ] in
  let output = check_css_roundtrip "rt grid template" sheet in

  (* Should contain grid template *)
  Alcotest.(check bool)
    "contains grid template" true
    (Astring.String.is_infix ~affix:"1fr 200px 2fr" output)

let test_container_query () =
  let rules = [ rule ~selector:container_item [ padding (Px 30.) ] ] in
  let cq = Css.container ~condition:"min-width: 400px" rules in
  let sheet = stylesheet [ Container cq ] in
  let output = check_css_roundtrip "rt container query" sheet in

  (* Should contain container query - check for both possible formats *)
  let has_container =
    Astring.String.is_infix ~affix:"@container" output
    && Astring.String.is_infix ~affix:"min-width: 400px" output
  in
  Alcotest.(check bool) "contains container query" true has_container

let test_var_extraction () =
  let _, v1 = var "test-var" Length Zero in
  let _, v2 = var "test-color" Color Current in
  let props = [ padding (Var v1); color (Var v2) ] in
  let extracted = analyze_declarations props in

  (* Should extract both variables *)
  Alcotest.(check int) "extracts two vars" 2 (List.length extracted)

let test_animation () =
  let anim =
    animation
      {
        name = Some "spin";
        duration = Some (S 1.0);
        timing_function = Some Linear;
        iteration_count = Some Infinite;
        delay = None;
        direction = None;
        fill_mode = None;
        play_state = None;
      }
  in
  let sheet = stylesheet [ Rule (rule ~selector:spin [ anim ]) ] in
  let output = check_css_roundtrip "rt animation" sheet in

  (* Should contain animation properties *)
  Alcotest.(check bool)
    "contains animation name" true
    (Astring.String.is_infix ~affix:"spin" output);
  Alcotest.(check bool)
    "contains duration" true
    (Astring.String.is_infix ~affix:"1s" output);
  Alcotest.(check bool)
    "contains infinite" true
    (Astring.String.is_infix ~affix:"infinite" output)

let test_merge_rules () =
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
    (List.length (declarations first_rule));

  (* .card should be second *)
  let second_rule = List.nth merged 1 in
  Alcotest.(check string)
    "second rule selector" (Selector.to_string card)
    (Selector.to_string (selector second_rule));

  (* Last .btn should be third (not merged due to intervening .card) *)
  let third_rule = List.nth merged 2 in
  Alcotest.(check string)
    "third rule selector" ".btn"
    (Selector.to_string (selector third_rule))

(* Test that adjacent rules with same selector merge correctly *)
let test_adjacent_same_selector_merge () =
  let _, prose_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let rules =
    [
      rule ~selector:s1 [ color (Var prose_body_var); max_width (Ch 65.) ];
      rule ~selector:s1 [ font_size (Rem 1.0); line_height (Number 1.75) ];
      rule ~selector:s1_where_p [ margin_top (Em 1.25) ];
    ]
  in

  let merged = merge_rules rules in

  (* The two adjacent rules with same selector should merge *)
  Alcotest.(check int) "adjacent rules merged" 2 (List.length merged);

  let first_rule = List.nth merged 0 in
  Alcotest.(check string)
    "merged selector" (Selector.to_string s1)
    (Selector.to_string (selector first_rule));
  Alcotest.(check int)
    "merged rule has all 4 properties" 4
    (List.length (declarations first_rule));

  let second_rule = List.nth merged 1 in
  Alcotest.(check string)
    "s1 :where(p) selector preserved"
    (Selector.to_string s1_where_p)
    (Selector.to_string (selector second_rule))

(* Test optimization with layers like prose generates *)
let test_layer_adjacent_rule_optimization () =
  let _, prose_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let s1_rules =
    [
      rule ~selector:s1 [ color (Var prose_body_var); max_width (Ch 65.) ];
      rule ~selector:s1 [ font_size (Rem 1.0); line_height (Number 1.75) ];
    ]
  in

  let utility_layer =
    layer ~name:"utilities" (List.map rule_to_nested s1_rules)
  in
  let stylesheet = stylesheet [ Layer utility_layer ] in
  let _ = check_css_roundtrip "rt layer adjacent optimization" stylesheet in

  (* Generate CSS with and without optimization *)
  let css_optimized = to_string ~minify:true ~optimize:true stylesheet in
  let css_unoptimized = to_string ~minify:true ~optimize:false stylesheet in

  (* Check that optimization merges the rules *)
  let count_s1_rules css =
    let rec count_occurrences str sub acc idx =
      try
        let idx' = String.index_from str idx sub.[0] in
        if
          String.length str >= idx' + String.length sub
          && String.sub str idx' (String.length sub) = sub
        then count_occurrences str sub (acc + 1) (idx' + 1)
        else count_occurrences str sub acc (idx' + 1)
      with Not_found -> acc
    in
    count_occurrences css ".s1{" 0 0
  in

  let s1_count_optimized = count_s1_rules css_optimized in
  let s1_count_unoptimized = count_s1_rules css_unoptimized in

  (* Adjacent .s1 rules should merge when optimized *)
  Alcotest.(check int) "unoptimized has two .s1 rules" 2 s1_count_unoptimized;
  Alcotest.(check int)
    "optimized merges to single .s1 rule" 1 s1_count_optimized;

  (* Verify optimization preserves CSS length or reduces it *)
  Alcotest.(check bool)
    "optimization reduces or maintains size" true
    (String.length css_optimized <= String.length css_unoptimized)

(* Test that non-adjacent rules with same selector don't merge *)
let test_no_merge_non_adjacent () =
  let rules =
    [
      rule ~selector:s1 [ color (Hex { hash = false; value = "000000" }) ];
      rule ~selector:s1_where_p [ margin_top (Em 1.0) ];
      rule ~selector:s1 [ font_size (Rem 1.0) ];
    ]
  in

  let merged = merge_rules rules in

  (* Should not merge the two .s1 rules because they're not adjacent *)
  Alcotest.(check int) "non-adjacent not merged" 3 (List.length merged)

(* Test non-adjacent same-selector merge behavior *)
let test_non_adjacent_same_selector () =
  let rules =
    [
      (* First .s1 rule with basic properties *)
      rule ~selector:s1
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.0) ];
      (* Descendant selectors in between *)
      rule ~selector:s1_complex_p [ margin_top (Em 1.25) ];
      rule ~selector:s1_complex_h1 [ font_size (Em 2.25) ];
      (* Second .s1 rule with diff_selectorerent properties *)
      rule ~selector:s1 [ line_height (Number 1.6); font_weight Bold ];
    ]
  in

  let merged = merge_rules rules in

  (* Should NOT merge the two .s1 rules because they're not adjacent *)
  Alcotest.(check int)
    "s1 rules stay separate when not adjacent" 4 (List.length merged);

  (* Test by converting to CSS and checking for separate .s1 rules *)
  let css =
    check_css_roundtrip "rt non-adjacent same-selector"
      (stylesheet (List.map (fun r -> Rule r) merged))
  in
  let s1_count = Astring.String.cuts ~sep:".s1{" css |> List.length |> pred in
  Alcotest.(check int) "both s1 rules in output" 2 s1_count

(* Test that rules with diff_selectorerent descendants are never merged *)
let test_no_merge_different_descendants () =
  let rules =
    [
      (* First .s1 rule - base selector *)
      rule ~selector:s1
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.0) ];
      (* Descendant rule with diff_selectorerent selector - should not be
         merged *)
      rule ~selector:s1_where_p
        [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
      (* Second .s1 rule - same base selector as first *)
      rule ~selector:s1 [ line_height (Number 1.75); font_size (Rem 1.0) ];
    ]
  in

  let merged = merge_rules rules in

  (* Should NOT merge the two .s1 rules because there's a descendant rule in
     between *)
  Alcotest.(check int) "descendant prevents merging" 3 (List.length merged);

  (* Test by converting to CSS and checking structure *)
  let css =
    check_css_roundtrip "rt no-merge different descendants"
      (stylesheet (List.map (fun r -> Rule r) merged))
  in
  let s1_base_count =
    Astring.String.cuts ~sep:".s1{" css |> List.length |> pred
  in
  let s1_where_count =
    Astring.String.cuts ~sep:".s1 :where(p){" css |> List.length |> pred
  in
  Alcotest.(check int) "two separate .s1 base rules" 2 s1_base_count;
  Alcotest.(check int) "one .s1 :where(p) rule" 1 s1_where_count

(* Test that reproduces the exact CSS rule grouping/merging cascade bug *)
let test_rules_grouping_cascade_bug () =
  (* This reproduces the core issue: when CSS rules are processed through
     optimize(), non-adjacent rules with the same selector get incorrectly
     merged even when diff_selectorerent-selector rules are between them.

     Structure that should be preserved: 1. .component rule (basic styles) 2.
     .component descendant rules (:where(), etc) 3. .component rule
     (variables/additional styles)

     The bug: rules 1 and 3 get merged despite rule 2 being between them. *)
  let rules =
    [
      (* First .component rule with basic styles *)
      rule ~selector:component
        [ color (Hex { hash = false; value = "333333" }); max_width (Ch 50.0) ];
      (* Descendant rules that should prevent merging *)
      rule ~selector:component_p [ margin_top (Em 1.0) ];
      rule ~selector:component_h1 [ font_size (Em 2.0) ];
      rule ~selector:component_h2 [ font_size (Em 1.5) ];
      rule ~selector:component_a [ text_decoration Underline ];
      rule ~selector:component_strong [ font_weight Bold ];
      (* Second .component rule with additional styles *)
      rule ~selector:component [ line_height (Number 1.5); font_size (Rem 1.0) ];
    ]
  in

  (* Create utilities layer and apply CSS optimization *)
  let utilities_layer =
    layer ~name:"utilities" (List.map rule_to_nested rules)
  in
  let stylesheet_obj = stylesheet [ Layer utilities_layer ] in
  let optimized_stylesheet = optimize stylesheet_obj in
  let css_string = to_string ~minify:true optimized_stylesheet in

  (* Count .component{} rules - should be 2 separate rules *)
  let component_count =
    Astring.String.cuts ~sep:".component{" css_string |> List.length |> pred
  in

  (* This test documents the expected behavior - should be 2 separate rules *)
  Alcotest.(check int)
    "CSS optimization should preserve non-adjacent same-selector rules" 2
    component_count

(* Test optimization in full stylesheet like tw generates *)
let test_full_optimization_with_layers () =
  let base_layer =
    layer ~name:"base"
      [
        rule_to_nested
          (rule ~selector:universal [ margin (Px 0.); padding (Px 0.) ]);
      ]
  in

  let _, s1_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let utilities_layer =
    layer ~name:"utilities"
      [
        rule_to_nested (rule ~selector:s1 [ color (Var s1_body_var) ]);
        rule_to_nested (rule ~selector:s1 [ font_size (Rem 1.0) ]);
        rule_to_nested (rule ~selector:s1_where_p [ margin_top (Em 1.25) ]);
      ]
  in

  let stylesheet = stylesheet [ Layer base_layer; Layer utilities_layer ] in
  let css_optimized = to_string ~minify:true ~optimize:true stylesheet in
  let css_unoptimized = to_string ~minify:true ~optimize:false stylesheet in

  (* Helper to count occurrences *)
  let count_occurrences str sub =
    let rec count acc idx =
      try
        let idx' = String.index_from str idx sub.[0] in
        if
          String.length str >= idx' + String.length sub
          && String.sub str idx' (String.length sub) = sub
        then count (acc + 1) (idx' + 1)
        else count acc (idx' + 1)
      with Not_found -> acc
    in
    count 0 0
  in

  (* Count .s1 rules in both versions *)
  let s1_count_optimized = count_occurrences css_optimized ".s1{" in
  let s1_count_unoptimized = count_occurrences css_unoptimized ".s1{" in

  (* Verify optimization behavior - adjacent .s1 rules should merge *)
  Alcotest.(check int)
    "unoptimized has two separate .s1 rules" 2 s1_count_unoptimized;
  Alcotest.(check int)
    "optimized merges into single .s1 rule" 1 s1_count_optimized;

  (* Optimization should reduce size *)
  Alcotest.(check bool)
    "optimized CSS is shorter or equal" true
    (String.length css_optimized <= String.length css_unoptimized);

  (* Both versions should contain the same CSS properties (semantic
     preservation) *)
  Alcotest.(check bool)
    "both contain color property" true
    (Astring.String.is_infix ~affix:"color:" css_optimized
    && Astring.String.is_infix ~affix:"color:" css_unoptimized);
  Alcotest.(check bool)
    "both contain font-size property" true
    (Astring.String.is_infix ~affix:"font-size:" css_optimized
    && Astring.String.is_infix ~affix:"font-size:" css_unoptimized)

(* CSS Optimization Tests - Based on cascade semantics *)

let test_layer_precedence_respected () =
  (* 1. Define a rule for a 'base' layer *)
  let base_rule =
    rule ~selector:btn
      [ background_color (Hex { hash = false; value = "0000ff" }) ]
  in
  let base_layer = layer ~name:"base" [ rule_to_nested base_rule ] in

  (* 2. Define an overriding rule for a 'utilities' layer *)
  let utility_rule =
    rule ~selector:btn
      [ background_color (Hex { hash = false; value = "ff0000" }) ]
  in
  let utility_layer = layer ~name:"utilities" [ rule_to_nested utility_rule ] in

  (* 3. Create a stylesheet with the layers in the correct order *)
  let stylesheet = stylesheet [ Layer base_layer; Layer utility_layer ] in

  (* 4. Generate the CSS with optimizations enabled (minify includes
     optimization) *)
  let css = to_string ~minify:true stylesheet in

  (* 5. Find the position of the generated colors *)
  let base_pos = Astring.String.find_sub ~sub:"#0000ff" css in
  let util_pos = Astring.String.find_sub ~sub:"#ff0000" css in

  (* 6. Assert that the utility rule's style appears AFTER the base rule's
     style *)
  match (base_pos, util_pos) with
  | Some b, Some u ->
      Alcotest.(check bool) "Utility rule must override base rule" true (u > b)
  | _, _ ->
      Alcotest.fail
        "One or both CSS rules for .btn were not found in the output"

let test_source_order_within_selector () =
  let rule =
    rule ~selector:card
      [ padding (Px 10.); padding (Px 20.) (* Later declaration should win *) ]
  in
  let stylesheet = stylesheet [ Rule rule ] in
  let optimized = optimize stylesheet in
  (* The rule should have deduplicated padding - only the last one *)
  Alcotest.(check int) "One rule" 1 (List.length (stylesheet_rules optimized));
  let opt_rule = List.hd (stylesheet_rules optimized) in
  Alcotest.(check int)
    "One declaration after dedup" 1
    (List.length (declarations opt_rule))

let test_merging_correctness () =
  let rule1 =
    rule ~selector:btn_primary
      [
        background_color (Hex { hash = false; value = "0000ff" });
        color (Hex { hash = false; value = "ffffff" });
      ]
  in
  let rule2 =
    rule ~selector:btn_secondary
      [
        background_color (Hex { hash = false; value = "0000ff" });
        color (Hex { hash = false; value = "ffffff" });
      ]
  in
  let stylesheet = stylesheet [ Rule rule1; Rule rule2 ] in
  let optimized = optimize stylesheet in
  (* Rules with identical declarations should be combined *)
  Alcotest.(check int)
    "Rules combined" 1
    (List.length (stylesheet_rules optimized));
  let rule = List.hd (stylesheet_rules optimized) in
  Alcotest.(check string)
    "Combined selector" ".btn-primary,.btn-secondary"
    (Selector.to_string ~minify:true (selector rule))

let test_non_merging_different_rules () =
  let rule1 =
    rule ~selector:btn
      [ background_color (Hex { hash = false; value = "0000ff" }) ]
  in
  let rule2 =
    rule ~selector:btn [ color (Hex { hash = false; value = "ffffff" }) ]
  in
  let stylesheet = stylesheet [ Rule rule1; Rule rule2 ] in
  let optimized = optimize stylesheet in
  (* Rules with same selector but diff_selectorerent properties should merge *)
  Alcotest.(check int)
    "Rules merged" 1
    (List.length (stylesheet_rules optimized));
  let rule = List.hd (stylesheet_rules optimized) in
  Alcotest.(check int)
    "Both declarations preserved" 2
    (List.length (declarations rule))

let test_cascade_with_intervening () =
  let rule1 =
    rule ~selector:base [ color (Hex { hash = false; value = "000000" }) ]
  in
  let rule2 =
    rule ~selector:override [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let rule3 =
    rule ~selector:base
      [ background_color (Hex { hash = false; value = "ffffff" }) ]
  in
  let stylesheet = stylesheet [ Rule rule1; Rule rule2; Rule rule3 ] in
  let optimized = optimize stylesheet in
  (* The .base rules should NOT be merged since they have intervening rule *)
  Alcotest.(check int)
    "Three rules preserved" 3
    (List.length (stylesheet_rules optimized));
  (* Verify order is preserved *)
  let selectors =
    List.map
      (fun r -> Selector.to_string (selector r))
      (stylesheet_rules optimized)
  in
  Alcotest.(check (list string))
    "Order preserved"
    [ Selector.to_string base; ".override"; Selector.to_string base ]
    selectors

let test_merge_truly_adjacent () =
  (* Test that only truly adjacent rules get merged *)
  let rule1 =
    rule ~selector:a [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let rule2 = rule ~selector:a [ font_size (Px 14.) ] in
  (* Adjacent - should merge *)
  let rule3 = rule ~selector:b [ margin Zero ] in
  let rule4 = rule ~selector:a [ padding (Px 10.) ] in
  (* Not adjacent to other .a rules *)
  let rule5 = rule ~selector:a [ border_width (Px 1.) ] in
  (* Adjacent to previous .a - should merge *)

  let rules = [ Rule rule1; Rule rule2; Rule rule3; Rule rule4; Rule rule5 ] in
  let stylesheet = stylesheet rules in
  let optimized = optimize stylesheet in
  let merged = stylesheet_rules optimized in

  (* Should result in 3 rules: merged .a, .b, merged .a *)
  Alcotest.(check int) "rule count" 3 (List.length merged);

  (* First merged .a rule should have 2 declarations *)
  let first = List.nth merged 0 in
  Alcotest.(check string)
    "first selector" ".a"
    (Selector.to_string (selector first));
  Alcotest.(check int) "first decl count" 2 (List.length (declarations first));

  (* Middle .b rule *)
  let middle = List.nth merged 1 in
  Alcotest.(check string)
    "middle selector" ".b"
    (Selector.to_string (selector middle));

  (* Last merged .a rule should have 2 declarations *)
  let last = List.nth merged 2 in
  Alcotest.(check string)
    "last selector" ".a"
    (Selector.to_string (selector last));
  Alcotest.(check int) "last decl count" 2 (List.length (declarations last))

let test_cascade_order_preservation () =
  (* Test that merge preserves cascade semantics for specificity conflicts *)
  let btn_base =
    rule ~selector:btn [ color (Hex { hash = false; value = "0000ff" }) ]
  in
  let btn_hover =
    rule ~selector:btn_hover [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let btn_bg =
    rule ~selector:btn
      [ background_color (Hex { hash = false; value = "ffffff" }) ]
  in

  let rules = [ Rule btn_base; Rule btn_hover; Rule btn_bg ] in
  let stylesheet = stylesheet rules in
  let optimized = optimize stylesheet in
  let merged = stylesheet_rules optimized in

  (* The two .btn rules should NOT be merged due to intervening :hover rule *)
  Alcotest.(check int) "rule count" 3 (List.length merged);

  (* Verify order is preserved *)
  let selectors = List.map (fun r -> Selector.to_string (selector r)) merged in
  Alcotest.(check (list string))
    "Order preserved"
    [ ".btn"; ".btn:hover"; ".btn" ]
    selectors

(* Test that non-adjacent rules with identical properties NEVER merge *)
let test_no_merge_by_properties () =
  let open Css in
  (* Create non-adjacent rules with identical properties *)
  let rules =
    [
      Css.rule ~selector:a [ color (Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:b [ margin Zero ];
      Css.rule ~selector:c [ color (Hex { hash = false; value = "ff0000" }) ];
      (* Same as .a but not adjacent *)
    ]
  in

  let stylesheet =
    Css.stylesheet
      [ Rule (List.hd rules); Rule (List.nth rules 1); Rule (List.nth rules 2) ]
  in

  (* Generate CSS with optimization and minification *)
  let css = Css.to_string ~minify:true ~optimize:true stylesheet in

  (* Check that .a and .c are NOT merged - they should appear separately *)
  Alcotest.(check bool)
    ".a appears in output" true
    (Astring.String.is_infix ~affix:".a{" css);
  Alcotest.(check bool)
    ".c appears in output" true
    (Astring.String.is_infix ~affix:".c{" css);

  (* They should NOT be combined like ".a,.c{color:#ff0000}" *)
  Alcotest.(check bool)
    ".a and .c NOT merged" false
    (Astring.String.is_infix ~affix:".a,.c{" css);

  (* Verify the exact structure: .a{...}.b{...}.c{...} *)
  let expected_structure = ".a{color:#ff0000}.b{margin:0}.c{color:#ff0000}" in
  Alcotest.(check string) "exact CSS structure" expected_structure css

(* Test for combine_identical_rules function *)
let test_combine_identical_rules () =
  let open Css in
  (* Test that consecutive rules with identical properties are combined *)
  let rules =
    [
      Css.rule ~selector:s1_where_a_strong [ color Inherit ];
      Css.rule ~selector:s1_where_blockquote_strong [ color Inherit ];
      Css.rule ~selector:s1_where_thead_th_strong [ color Inherit ];
      Css.rule ~selector:diff_selectorerent [ font_size (Rem 1.0) ];
    ]
  in

  let combined = combine_identical_rules rules in

  (* Should combine the first three into one rule *)
  Alcotest.(check int) "combined rule count" 2 (List.length combined);

  (* Check the combined selector *)
  let first_rule = List.hd combined in
  Alcotest.(check bool)
    "selectors combined with comma" true
    (String.contains (Selector.to_string (Css.selector first_rule)) ',');

  (* Non-consecutive identical rules should not be combined *)
  let rules2 =
    [
      Css.rule ~selector:a [ color (Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:b [ color (Hex { hash = false; value = "0000ff" }) ];
      Css.rule ~selector:c [ color (Hex { hash = false; value = "ff0000" }) ];
    ]
  in

  let combined2 = combine_identical_rules rules2 in
  Alcotest.(check int) "non-consecutive not combined" 3 (List.length combined2)

(* Test that optimization doesn't merge across @supports boundaries *)
let test_no_merge_across_supports () =
  with_debug "no_merge_across_supports_boundary" @@ fun () ->
  let open Css in
  (* Create rules with same selector separated by @supports *)
  let rule1 = rule ~selector:feature [ padding (Px 10.) ] in
  let rule2 = rule ~selector:feature [ margin (Px 5.) ] in
  let supports_rule =
    supports ~condition:"(display: grid)"
      [ rule ~selector:feature [ display Grid ] ]
  in

  (* Create stylesheet with rule -> @supports -> rule structure *)
  let stylesheet =
    stylesheet [ Rule rule1; Supports supports_rule; Rule rule2 ]
  in

  let optimized = optimize stylesheet in
  let rules = stylesheet_rules optimized in

  (* Debug output on failure *)
  if List.length rules <> 2 then
    debug_stylesheet ~label:"@supports boundary test" stylesheet optimized;

  (* The two .feature rules should NOT merge across @supports boundary *)
  Alcotest.(check int) "rules not merged across @supports" 2 (List.length rules);

  (* Verify @supports block is preserved *)
  let css = to_string ~minify:true optimized in
  if
    (not (Astring.String.is_infix ~affix:"@supports" css))
    || not (Astring.String.is_infix ~affix:"display:grid" css)
  then
    debug_css ~label:"@supports content check" css
      [ "@supports"; "display:grid" ];

  Alcotest.(check bool)
    "contains @supports" true
    (Astring.String.is_infix ~affix:"@supports" css);
  Alcotest.(check bool)
    "contains display:grid in supports" true
    (Astring.String.is_infix ~affix:"display:grid" css)

(* Test that !important declarations are preserved during optimization *)
let test_important_preservation () =
  let open Css in
  (* Create rules with !important declarations *)
  let rule1 =
    rule ~selector:critical
      [ important (color (Hex { hash = false; value = "ff0000" })) ]
  in
  let rule2 = rule ~selector:critical [ padding (Px 10.) ] in
  let rule3 =
    rule ~selector:normal [ color (Hex { hash = false; value = "0000ff" }) ]
  in

  let stylesheet = stylesheet [ Rule rule1; Rule rule2; Rule rule3 ] in
  let optimized = optimize stylesheet in
  let css = to_string ~minify:true optimized in

  (* Check !important is preserved *)
  Alcotest.(check bool)
    "!important preserved" true
    (Astring.String.is_infix ~affix:"!important" css);

  (* Rules should be merged but !important maintained *)
  let merged_rules = stylesheet_rules optimized in
  Alcotest.(check int)
    "adjacent .critical rules merged" 2 (List.length merged_rules)

(* Test that empty rules are preserved but can be merged if adjacent *)
let test_empty_rules_handling () =
  let open Css in
  let rule1 = rule ~selector:empty_class [] in
  let rule2 = rule ~selector:empty_class [] in
  (* Adjacent empty with same selector *)
  let rule3 = rule ~selector:has_content [ padding (Px 10.) ] in
  let rule4 = rule ~selector:also_empty [] in
  let rule5 = rule ~selector:another [ margin (Px 5.) ] in

  let stylesheet =
    stylesheet [ Rule rule1; Rule rule2; Rule rule3; Rule rule4; Rule rule5 ]
  in
  let optimized = optimize stylesheet in
  let remaining_rules = stylesheet_rules optimized in

  (* Empty rules should be preserved - total of 4 rules after merging adjacent
     .empty *)
  Alcotest.(check int)
    "empty rules preserved, adjacent ones merged" 4
    (List.length remaining_rules);

  (* First rule should be the merged .empty rule (still empty) *)
  let first = List.hd remaining_rules in
  Alcotest.(check string)
    "first rule is .empty" ".empty"
    (Selector.to_string (selector first));
  Alcotest.(check int)
    "first rule still empty" 0
    (List.length (declarations first))

(* Test optimization within nested contexts like @media *)
let test_optimize_within_nested_contexts () =
  let open Css in
  (* Create adjacent rules inside @media that should merge *)
  let media_rules =
    [
      rule ~selector:responsive [ padding (Px 10.) ];
      rule ~selector:responsive [ margin (Px 5.) ];
      rule ~selector:other [ color (Hex { hash = false; value = "000000" }) ];
    ]
  in

  let mq = media ~condition:"(min-width: 768px)" media_rules in
  let stylesheet = stylesheet [ Media mq ] in
  let optimized = optimize stylesheet in
  let css = to_string ~minify:true optimized in

  (* Count .responsive rules in output - should be merged to 1 *)
  let responsive_count =
    Astring.String.cuts ~sep:".responsive{" css |> List.length |> pred
  in
  Alcotest.(check int) "adjacent rules merged within @media" 1 responsive_count;

  (* Verify the @media structure is preserved *)
  Alcotest.(check bool)
    "media query preserved" true
    (Astring.String.is_infix ~affix:"@media (min-width: 768px)" css)

(* Regression test for duplicate last-child bug mentioned in optimize.md *)
let test_no_duplicate_last_child () =
  let open Css in
  (* Simulate component structure with last-child rule *)
  let rules =
    [
      rule ~selector:component
        [ color (Hex { hash = false; value = "374151" }) ];
      rule ~selector:component_where_p [ margin_top (Em 1.0) ];
      rule ~selector:component_where_component_last_child [ margin_bottom Zero ];
      rule ~selector:component [ font_size (Rem 1.0) ];
    ]
  in

  let utilities_layer =
    layer ~name:"utilities" (List.map rule_to_nested rules)
  in
  let stylesheet = stylesheet [ Layer utilities_layer ] in
  let optimized = optimize stylesheet in
  let css = to_string ~minify:true optimized in

  (* Count occurrences of the last-child rule - should be exactly 1 *)
  let last_child_count =
    let rec count_occurrences str pattern acc idx =
      try
        let idx' = String.index_from str idx pattern.[0] in
        if
          String.length str >= idx' + String.length pattern
          && String.sub str idx' (String.length pattern) = pattern
        then count_occurrences str pattern (acc + 1) (idx' + 1)
        else count_occurrences str pattern acc (idx' + 1)
      with Not_found -> acc
    in
    count_occurrences css ".component :where(.component>:last-child){" 0 0
  in

  Alcotest.(check int) "last-child rule appears exactly once" 1 last_child_count;

  (* Verify the two .component rules are NOT merged (they're not adjacent) *)
  let component_count =
    Astring.String.cuts ~sep:".component{" css |> List.length |> pred
  in
  Alcotest.(check int)
    "non-adjacent .component rules not merged" 2 component_count

(* Test that minification produces compact output *)
(* value-specific minification checks are covered in test_values.ml *)

(* Test that selector ordering is preserved (no reordering) *)
let test_order_preservation () =
  let open Css in
  (* Create rules with diff_selectorerent selectors in specific order *)
  let rules =
    [
      rule ~selector:z_class [ padding (Px 10.) ];
      rule ~selector:a_class [ margin (Px 5.) ];
      rule ~selector:m_class [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:b_class [ font_size (Rem 1.0) ];
    ]
  in

  let stylesheet = stylesheet (List.map (fun r -> Rule r) rules) in
  let _ = check_css_roundtrip "rt order preservation base" stylesheet in
  let optimized = optimize stylesheet in
  let _ = check_css_roundtrip "rt order preservation optimized" optimized in
  let css = to_string ~minify:true optimized in

  (* Verify selectors appear in original order, not alphabetically *)
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
  Alcotest.(check bool) "m-class comes before b-class" true (m_pos < b_pos);

  (* Verify exact selector order from optimized rules *)
  let opt_rules = stylesheet_rules optimized in
  let selectors =
    List.map (fun r -> Selector.to_string (selector r)) opt_rules
  in
  Alcotest.(check (list string))
    "selector order preserved exactly"
    [ ".z-class"; ".a-class"; ".m-class"; ".b-class" ]
    selectors

(* Test that rules are not reordered based on properties *)
let test_no_property_based_reordering () =
  let open Css in
  (* Create rules that might be tempting to reorder by property type *)
  let rules =
    [
      rule ~selector:margins [ margin (Px 10.) ];
      rule ~selector:colors [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:more_margins [ margin (Px 20.) ];
      rule ~selector:paddings [ padding (Px 5.) ];
      rule ~selector:more_colors
        [ color (Hex { hash = false; value = "0000ff" }) ];
    ]
  in

  let stylesheet = stylesheet (List.map (fun r -> Rule r) rules) in
  let optimized = optimize stylesheet in

  (* Verify rules are not grouped by property type *)
  let opt_rules = stylesheet_rules optimized in
  let selectors =
    List.map (fun r -> Selector.to_string (selector r)) opt_rules
  in
  Alcotest.(check (list string))
    "no reordering by property type"
    [ ".margins"; ".colors"; ".more-margins"; ".paddings"; ".more-colors" ]
    selectors

(* Test that cross-context optimization is prevented *)
let test_no_cross_context_optimization () =
  let open Css in
  (* Same selector in diff_selectorerent contexts should not merge *)
  let base_layer =
    layer ~name:"base"
      [ rule_to_nested (rule ~selector:btn [ padding (Px 10.) ]) ]
  in
  let utilities_layer =
    layer ~name:"utilities"
      [ rule_to_nested (rule ~selector:btn [ margin (Px 5.) ]) ]
  in

  let stylesheet = stylesheet [ Layer base_layer; Layer utilities_layer ] in
  let optimized = optimize stylesheet in
  let css = to_string ~minify:true optimized in

  (* Both .btn rules should exist in their respective layers *)
  Alcotest.(check bool)
    "base layer has .btn" true
    (Astring.String.is_infix ~affix:"@layer base{.btn{" css);
  Alcotest.(check bool)
    "utilities layer has .btn" true
    (Astring.String.is_infix ~affix:"@layer utilities{.btn{" css);

  (* Count total .btn rules - should be 2 (one per layer) *)
  let btn_count = Astring.String.cuts ~sep:".btn{" css |> List.length |> pred in
  Alcotest.(check int) "cross-layer rules not merged" 2 btn_count

(* Test that optimize_nested_rules doesn't merge non-adjacent rules with same
   selector *)
let test_optimize_nested_non_adjacent () =
  let open Css in
  (* Create test rules: .s1 rule, then 5 other rules, then another .s1 rule *)
  let nested_rules =
    [
      rule_to_nested
        (rule ~selector:s1 [ color (Hex { hash = false; value = "ff0000" }) ]);
      rule_to_nested
        (rule ~selector:s2 [ color (Hex { hash = false; value = "0000ff" }) ]);
      rule_to_nested
        (rule ~selector:s3 [ color (Hex { hash = false; value = "00ff00" }) ]);
      rule_to_nested
        (rule ~selector:s4 [ color (Hex { hash = false; value = "ffff00" }) ]);
      rule_to_nested
        (rule ~selector:s5 [ color (Hex { hash = false; value = "ff00ff" }) ]);
      rule_to_nested
        (rule ~selector:s6 [ color (Hex { hash = false; value = "ffa500" }) ]);
      rule_to_nested
        (rule ~selector:s1
           [ background_color (Hex { hash = false; value = "ffffff" }) ]);
    ]
  in

  (* Create a layer with these rules and optimize *)
  let test_layer = layer ~name:"test" nested_rules in
  let sheet = stylesheet [ Layer test_layer ] in
  let optimized = optimize sheet in
  let css = to_string ~minify:true optimized in

  (* Count .s1 rules in the output *)
  let s1_count = Astring.String.cuts ~sep:".s1{" css |> List.length |> pred in

  (* Should still have 2 .s1 rules since they're not adjacent *)
  Alcotest.(check int) "Non-adjacent .s1 rules not merged" 2 s1_count

(* Test runner *)
(* Test that multiple selectors with identical properties get merged *)
let test_merge_prose_selectors () =
  let open Css in
  (* Test consecutive rules with identical properties - matching actual prose
     selectors *)
  let rules =
    [
      Css.rule ~selector:prose_a_strong [ color Inherit ];
      Css.rule ~selector:prose_blockquote_strong [ color Inherit ];
      Css.rule ~selector:prose_thead_th_strong [ color Inherit ];
    ]
  in

  (* Test combine_identical_rules *)
  let combined = combine_identical_rules rules in
  Printf.eprintf "DEBUG: Got %d rules back from combine_identical_rules\n"
    (List.length combined);
  List.iteri
    (fun i rule ->
      Printf.eprintf "  [%d] %s\n" i (Selector.to_string (Css.selector rule)))
    combined;
  Alcotest.(check int) "should combine into 1 rule" 1 (List.length combined);

  (* Check the combined selector *)
  match combined with
  | [ rule ] ->
      let expected_selector =
        ".prose :where(a strong)," ^ ".prose :where(blockquote strong),"
        ^ ".prose :where(thead th strong)"
      in
      let actual_selector =
        Selector.to_string ~minify:true (Css.selector rule)
      in
      Alcotest.(check string)
        "combined selector" expected_selector actual_selector
  | _ -> Alcotest.fail "Expected exactly 1 combined rule"

let suite =
  [
    ( "css",
      [
        Alcotest.test_case "property creation" `Quick test_property_creation;
        Alcotest.test_case "property deduplication" `Quick
          test_property_deduplication;
        Alcotest.test_case "minification" `Quick test_minification;
        Alcotest.test_case "media query" `Quick test_media_query;
        Alcotest.test_case "inline style" `Quick test_inline_style;
        (* calc expressions moved to test_values.ml *)
        (* CSS variables moved to test_values.ml *)
        Alcotest.test_case "grid template" `Quick test_grid_template;
        Alcotest.test_case "container query" `Quick test_container_query;
        Alcotest.test_case "var extraction" `Quick test_var_extraction;
        Alcotest.test_case "animation" `Quick test_animation;
        Alcotest.test_case "merge rules" `Quick test_merge_rules;
        Alcotest.test_case "adjacent same selector merge" `Quick
          test_adjacent_same_selector_merge;
        Alcotest.test_case "optimize layer with adjacent rules" `Quick
          test_layer_adjacent_rule_optimization;
        Alcotest.test_case "no merge non-adjacent" `Quick
          test_no_merge_non_adjacent;
        Alcotest.test_case "non-adjacent same selector" `Quick
          test_non_adjacent_same_selector;
        Alcotest.test_case "no merge diff_selectorerent descendants" `Quick
          test_no_merge_different_descendants;
        Alcotest.test_case "full optimization with layers" `Quick
          test_full_optimization_with_layers;
        (* pp float moved to test_values.ml *)
        (* var with fallback moved to test_values.ml *)
        (* CSS Optimization tests *)
        Alcotest.test_case "layer precedence respected" `Quick
          test_layer_precedence_respected;
        Alcotest.test_case "source order within selector" `Quick
          test_source_order_within_selector;
        Alcotest.test_case "selector merging correctness" `Quick
          test_merging_correctness;
        Alcotest.test_case "non merging different rules" `Quick
          test_non_merging_different_rules;
        Alcotest.test_case "cascade with intervening" `Quick
          test_cascade_with_intervening;
        Alcotest.test_case "merge truly adjacent" `Quick
          test_merge_truly_adjacent;
        Alcotest.test_case "cascade order preservation" `Quick
          test_cascade_order_preservation;
        Alcotest.test_case "merge prose selectors" `Quick
          test_merge_prose_selectors;
        Alcotest.test_case "no merge by properties" `Quick
          test_no_merge_by_properties;
        Alcotest.test_case "combine identical rules" `Quick
          test_combine_identical_rules;
        Alcotest.test_case "rules grouping breaks cascade order" `Quick
          test_rules_grouping_cascade_bug;
        (* New tests from optimize.md *)
        Alcotest.test_case "no merge across @supports boundary" `Quick
          test_no_merge_across_supports;
        Alcotest.test_case "!important preservation" `Quick
          test_important_preservation;
        Alcotest.test_case "empty rules handling" `Quick
          test_empty_rules_handling;
        Alcotest.test_case "optimize within nested contexts" `Quick
          test_optimize_within_nested_contexts;
        Alcotest.test_case "no duplicate last-child" `Quick
          test_no_duplicate_last_child;
        Alcotest.test_case "selector order preservation" `Quick
          test_order_preservation;
        Alcotest.test_case "no property-based reordering" `Quick
          test_no_property_based_reordering;
        Alcotest.test_case "no cross-context optimization" `Quick
          test_no_cross_context_optimization;
        Alcotest.test_case "optimize_nested_rules preserves non-adjacent" `Quick
          test_optimize_nested_non_adjacent;
        (* selector-specific tests moved to test/css/test_selector.ml *)
      ] );
  ]
