(* Tests for CSS module *)
open Css

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
  Fmt.pr "Stats: %s@," (Css.pp_stats (Css.stats optimized));
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

let test_property_creation () =
  let color_prop = color (Hex { hash = true; value = "ff0000" }) in
  let padding_prop = padding (Px 10) in

  (* Verify properties are created correctly *)
  let sheet1 = stylesheet [ Rule (rule ~selector:".test" [ color_prop ]) ] in
  let sheet2 = stylesheet [ Rule (rule ~selector:".test" [ padding_prop ]) ] in
  let css1 = to_string sheet1 in
  let css2 = to_string sheet2 in

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
      padding (Px 10);
      color (Hex { hash = true; value = "0000ff" });
      (* Should override first color *)
      margin (Px 5);
    ]
  in

  let deduped = deduplicate_declarations props in

  (* Should have 3 properties after deduplication *)
  Alcotest.(check int)
    "deduplication removes duplicates" 3 (List.length deduped);

  (* Create a stylesheet to check the output *)
  let sheet = stylesheet [ Rule (rule ~selector:".test" deduped) ] in
  let css = to_string sheet in

  (* Should have blue, not red *)
  Alcotest.(check bool)
    "last property wins" true
    (Astring.String.is_infix ~affix:"#0000ff" css);
  Alcotest.(check bool)
    "red was overridden" false
    (Astring.String.is_infix ~affix:"#ff0000" css)

let test_minification () =
  let test_rule =
    rule ~selector:".test"
      [ padding Zero; margin Zero; color (Rgb { r = 255; g = 0; b = 0 }) ]
  in

  let sheet = stylesheet [ Rule test_rule ] in
  let normal = to_string ~minify:false sheet in
  let minified = to_string ~minify:true sheet in

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
  let rules = [ rule ~selector:".responsive" [ padding (Px 20) ] ] in
  let mq = media ~condition:"(min-width: 768px)" rules in
  let sheet = stylesheet [ Media mq ] in
  let output = to_string sheet in

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
      padding (Px 10);
      margin (Px 5);
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

let test_calc () =
  let calc_len = Calc (Expr (Val (Px 100), Sub, Val (Rem 2.0))) in
  let prop = width calc_len in
  let sheet = stylesheet [ Rule (rule ~selector:".calc" [ prop ]) ] in
  let output = to_string sheet in

  (* Should contain calc expression *)
  Alcotest.(check bool)
    "contains calc" true
    (Astring.String.is_infix ~affix:"calc(100px - 2rem)" output)

let test_variables () =
  let _, ff =
    var
      ~fallback:[ Ui_sans_serif; System_ui ]
      "fonts" Font_family [ Ui_sans_serif ]
  in
  let font_decl = font_family [ Var ff ] in
  let sheet = stylesheet [ Rule (rule ~selector:".var-test" [ font_decl ]) ] in
  let output = to_string sheet in

  (* Should contain var with fallback *)
  Alcotest.(check bool)
    "contains var reference" true
    (Astring.String.is_infix ~affix:"var(--fonts" output);
  Alcotest.(check bool)
    "contains fallback" true
    (Astring.String.is_infix ~affix:"ui-sans-serif" output)

let test_grid_template () =
  let template_cols = Tracks [ Fr 1.0; Grid_length (Px 200); Fr 2.0 ] in
  let prop = grid_template_columns template_cols in
  let sheet = stylesheet [ Rule (rule ~selector:".grid" [ prop ]) ] in
  let output = to_string sheet in

  (* Should contain grid template *)
  Alcotest.(check bool)
    "contains grid template" true
    (Astring.String.is_infix ~affix:"1fr 200px 2fr" output)

let test_container_query () =
  let rules = [ rule ~selector:".container-item" [ padding (Px 30) ] ] in
  let cq = container ~condition:"min-width: 400px" rules in
  let sheet = stylesheet [ Container cq ] in
  let output = to_string sheet in

  (* Debug: print actual output *)
  (* Printf.printf "Container output: '%s'\n" output; *)

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
  let extracted = vars_of_declarations props in

  (* Should extract both variables *)
  Alcotest.(check int) "extracts two vars" 2 (List.length extracted)

let test_animation () =
  let anim = animation "spin 1s linear infinite" in
  let sheet = stylesheet [ Rule (rule ~selector:".spin" [ anim ]) ] in
  let output = to_string sheet in

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
      rule ~selector:".btn" [ color (Hex { hash = true; value = "ff0000" }) ];
      rule ~selector:".btn" [ padding (Px 10) ];
      rule ~selector:".card" [ margin (Px 5) ];
      rule ~selector:".btn" [ margin (Px 15) ];
    ]
  in

  let merged = merge_rules rules in

  (* First two .btn rules should merge *)
  Alcotest.(check int) "correct number of rules" 3 (List.length merged);

  (* Check that first rule has both color and padding *)
  let first_rule = List.nth merged 0 in
  Alcotest.(check string) "first rule selector" ".btn" (selector first_rule);
  Alcotest.(check int)
    "first rule has 2 declarations" 2
    (List.length (declarations first_rule));

  (* .card should be second *)
  let second_rule = List.nth merged 1 in
  Alcotest.(check string) "second rule selector" ".card" (selector second_rule);

  (* Last .btn should be third (not merged due to intervening .card) *)
  let third_rule = List.nth merged 2 in
  Alcotest.(check string) "third rule selector" ".btn" (selector third_rule)

(* Test that adjacent rules with same selector merge correctly *)
let test_adjacent_same_selector_merge () =
  let _, prose_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let rules =
    [
      rule ~selector:".s1" [ color (Var prose_body_var); max_width (Ch 65.) ];
      rule ~selector:".s1" [ font_size (Rem 1.0); line_height (Num 1.75) ];
      rule ~selector:".s1 :where(p)" [ margin_top (Em 1.25) ];
    ]
  in

  let merged = merge_rules rules in

  (* The two adjacent rules with same selector should merge *)
  Alcotest.(check int) "adjacent rules merged" 2 (List.length merged);

  let first_rule = List.nth merged 0 in
  Alcotest.(check string) "merged selector" ".s1" (selector first_rule);
  Alcotest.(check int)
    "merged rule has all 4 properties" 4
    (List.length (declarations first_rule));

  let second_rule = List.nth merged 1 in
  Alcotest.(check string)
    "s1 :where(p) selector preserved" ".s1 :where(p)" (selector second_rule)

(* Test optimization with layers like prose generates *)
let test_layer_adjacent_rule_optimization () =
  let _, prose_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let s1_rules =
    [
      rule ~selector:".s1" [ color (Var prose_body_var); max_width (Ch 65.) ];
      rule ~selector:".s1" [ font_size (Rem 1.0); line_height (Num 1.75) ];
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
      rule ~selector:".s1" [ color (Hex { hash = false; value = "000000" }) ];
      rule ~selector:".s1 :where(p)" [ margin_top (Em 1.0) ];
      rule ~selector:".s1" [ font_size (Rem 1.0) ];
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
      rule ~selector:".s1"
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.0) ];
      (* Descendant selectors in between *)
      rule
        ~selector:
          ".s1 \
           :where(p):not(:where([class~=\"not-prose\"],[class~=\"not-prose\"] \
           *))"
        [ margin_top (Em 1.25) ];
      rule
        ~selector:
          ".s1 \
           :where(h1):not(:where([class~=\"not-prose\"],[class~=\"not-prose\"] \
           *))"
        [ font_size (Em 2.25) ];
      (* Second .s1 rule with different properties *)
      rule ~selector:".s1" [ line_height (Num 1.6); font_weight Bold ];
    ]
  in

  let merged = merge_rules rules in

  (* Should NOT merge the two .s1 rules because they're not adjacent *)
  Alcotest.(check int)
    "s1 rules stay separate when not adjacent" 4 (List.length merged);

  (* Test by converting to CSS and checking for separate .s1 rules *)
  let css =
    to_string ~minify:true (stylesheet (List.map (fun r -> Rule r) merged))
  in
  let s1_count = Astring.String.cuts ~sep:".s1{" css |> List.length |> pred in
  Alcotest.(check int) "both s1 rules in output" 2 s1_count

(* Test that rules with different descendants are never merged *)
let test_no_merge_different_descendants () =
  let rules =
    [
      (* First .s1 rule - base selector *)
      rule ~selector:".s1"
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.0) ];
      (* Descendant rule with different selector - should not be merged *)
      rule ~selector:".s1 :where(p)"
        [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
      (* Second .s1 rule - same base selector as first *)
      rule ~selector:".s1" [ line_height (Num 1.75); font_size (Rem 1.0) ];
    ]
  in

  let merged = merge_rules rules in

  (* Should NOT merge the two .s1 rules because there's a descendant rule in
     between *)
  Alcotest.(check int) "descendant prevents merging" 3 (List.length merged);

  (* Test by converting to CSS and checking structure *)
  let css =
    to_string ~minify:true (stylesheet (List.map (fun r -> Rule r) merged))
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
     merged even when different-selector rules are between them.

     Structure that should be preserved: 1. .component rule (basic styles) 2.
     .component descendant rules (:where(), etc) 3. .component rule
     (variables/additional styles)

     The bug: rules 1 and 3 get merged despite rule 2 being between them. *)
  let rules =
    [
      (* First .component rule with basic styles *)
      rule ~selector:".component"
        [ color (Hex { hash = false; value = "333333" }); max_width (Ch 50.0) ];
      (* Descendant rules that should prevent merging *)
      rule ~selector:".component p" [ margin_top (Em 1.0) ];
      rule ~selector:".component h1" [ font_size (Em 2.0) ];
      rule ~selector:".component h2" [ font_size (Em 1.5) ];
      rule ~selector:".component a" [ text_decoration Underline ];
      rule ~selector:".component strong" [ font_weight Bold ];
      (* Second .component rule with additional styles *)
      rule ~selector:".component" [ line_height (Num 1.5); font_size (Rem 1.0) ];
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
      [ rule_to_nested (rule ~selector:"*" [ margin (Px 0); padding (Px 0) ]) ]
  in

  let _, s1_body_var =
    var "s1-body" Color (Hex { hash = false; value = "374151" })
  in
  let utilities_layer =
    layer ~name:"utilities"
      [
        rule_to_nested (rule ~selector:".s1" [ color (Var s1_body_var) ]);
        rule_to_nested (rule ~selector:".s1" [ font_size (Rem 1.0) ]);
        rule_to_nested (rule ~selector:".s1 :where(p)" [ margin_top (Em 1.25) ]);
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

let test_pp_float () =
  let r =
    rule ~selector:".f" [ letter_spacing (Rem 0.5); rotate (Turn (-0.5)) ]
  in
  let css = to_string (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "leading zero dropped" true
    (Astring.String.is_infix ~affix:"letter-spacing: .5rem" css);
  Alcotest.(check bool)
    "negative leading zero dropped" true
    (Astring.String.is_infix ~affix:"rotate: -.5turn" css)

let test_var_with_fallback () =
  let _, ff =
    var
      ~fallback:[ Ui_sans_serif; System_ui ]
      "fonts" Font_family [ Ui_sans_serif ]
  in
  let decl = font_family [ Var ff ] in
  let css = to_string (stylesheet [ Rule (rule ~selector:".ff" [ decl ]) ]) in
  Alcotest.(check bool)
    "font family var with fallback" true
    (Astring.String.is_infix ~affix:"font-family: var(--fonts" css
    && Astring.String.is_infix ~affix:"ui-sans-serif" css)

(* CSS Optimization Tests - Based on cascade semantics *)

let test_layer_precedence_respected () =
  (* 1. Define a rule for a 'base' layer *)
  let base_rule =
    rule ~selector:".btn"
      [ background_color (Hex { hash = false; value = "0000ff" }) ]
  in
  let base_layer = layer ~name:"base" [ rule_to_nested base_rule ] in

  (* 2. Define an overriding rule for a 'utilities' layer *)
  let utility_rule =
    rule ~selector:".btn"
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
    rule ~selector:".card"
      [ padding (Px 10); padding (Px 20) (* Later declaration should win *) ]
  in
  let stylesheet = stylesheet [ Rule rule ] in
  let optimized = optimize stylesheet in
  (* The rule should have deduplicated padding - only the last one *)
  Alcotest.(check int) "One rule" 1 (List.length (stylesheet_rules optimized));
  let opt_rule = List.hd (stylesheet_rules optimized) in
  Alcotest.(check int)
    "One declaration after dedup" 1
    (List.length (declarations opt_rule))

let test_selector_merging_correctness () =
  let rule1 =
    rule ~selector:".btn-primary"
      [
        background_color (Hex { hash = false; value = "0000ff" });
        color (Hex { hash = false; value = "ffffff" });
      ]
  in
  let rule2 =
    rule ~selector:".btn-secondary"
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
    "Combined selector" ".btn-primary,.btn-secondary" (selector rule)

let test_non_merging_different_rules () =
  let rule1 =
    rule ~selector:".btn"
      [ background_color (Hex { hash = false; value = "0000ff" }) ]
  in
  let rule2 =
    rule ~selector:".btn" [ color (Hex { hash = false; value = "ffffff" }) ]
  in
  let stylesheet = stylesheet [ Rule rule1; Rule rule2 ] in
  let optimized = optimize stylesheet in
  (* Rules with same selector but different properties should merge *)
  Alcotest.(check int)
    "Rules merged" 1
    (List.length (stylesheet_rules optimized));
  let rule = List.hd (stylesheet_rules optimized) in
  Alcotest.(check int)
    "Both declarations preserved" 2
    (List.length (declarations rule))

let test_cascade_with_intervening () =
  let rule1 =
    rule ~selector:".base" [ color (Hex { hash = false; value = "000000" }) ]
  in
  let rule2 =
    rule ~selector:".override"
      [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let rule3 =
    rule ~selector:".base"
      [ background_color (Hex { hash = false; value = "ffffff" }) ]
  in
  let stylesheet = stylesheet [ Rule rule1; Rule rule2; Rule rule3 ] in
  let optimized = optimize stylesheet in
  (* The .base rules should NOT be merged since they have intervening rule *)
  Alcotest.(check int)
    "Three rules preserved" 3
    (List.length (stylesheet_rules optimized));
  (* Verify order is preserved *)
  let selectors = List.map selector (stylesheet_rules optimized) in
  Alcotest.(check (list string))
    "Order preserved"
    [ ".base"; ".override"; ".base" ]
    selectors

let test_merge_truly_adjacent () =
  (* Test that only truly adjacent rules get merged *)
  let rule1 =
    rule ~selector:".a" [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let rule2 = rule ~selector:".a" [ font_size (Px 14) ] in
  (* Adjacent - should merge *)
  let rule3 = rule ~selector:".b" [ margin Zero ] in
  let rule4 = rule ~selector:".a" [ padding (Px 10) ] in
  (* Not adjacent to other .a rules *)
  let rule5 = rule ~selector:".a" [ border_width (Px 1) ] in
  (* Adjacent to previous .a - should merge *)

  let rules = [ Rule rule1; Rule rule2; Rule rule3; Rule rule4; Rule rule5 ] in
  let stylesheet = stylesheet rules in
  let optimized = optimize stylesheet in
  let merged = stylesheet_rules optimized in

  (* Should result in 3 rules: merged .a, .b, merged .a *)
  Alcotest.(check int) "rule count" 3 (List.length merged);

  (* First merged .a rule should have 2 declarations *)
  let first = List.nth merged 0 in
  Alcotest.(check string) "first selector" ".a" (selector first);
  Alcotest.(check int) "first decl count" 2 (List.length (declarations first));

  (* Middle .b rule *)
  let middle = List.nth merged 1 in
  Alcotest.(check string) "middle selector" ".b" (selector middle);

  (* Last merged .a rule should have 2 declarations *)
  let last = List.nth merged 2 in
  Alcotest.(check string) "last selector" ".a" (selector last);
  Alcotest.(check int) "last decl count" 2 (List.length (declarations last))

let test_cascade_order_preservation () =
  (* Test that merge preserves cascade semantics for specificity conflicts *)
  let btn_base =
    rule ~selector:".btn" [ color (Hex { hash = false; value = "0000ff" }) ]
  in
  let btn_hover =
    rule ~selector:".btn:hover"
      [ color (Hex { hash = false; value = "ff0000" }) ]
  in
  let btn_bg =
    rule ~selector:".btn"
      [ background_color (Hex { hash = false; value = "ffffff" }) ]
  in

  let rules = [ Rule btn_base; Rule btn_hover; Rule btn_bg ] in
  let stylesheet = stylesheet rules in
  let optimized = optimize stylesheet in
  let merged = stylesheet_rules optimized in

  (* The two .btn rules should NOT be merged due to intervening :hover rule *)
  Alcotest.(check int) "rule count" 3 (List.length merged);

  (* Verify order is preserved *)
  let selectors = List.map selector merged in
  Alcotest.(check (list string))
    "Order preserved"
    [ ".btn"; ".btn:hover"; ".btn" ]
    selectors

(* Test that non-adjacent rules with identical properties NEVER merge *)
let test_optimization_stats () =
  (* Create a stylesheet with multiple layers and rules that can be optimized *)
  let s1_rules =
    [
      rule ~selector:".s1"
        [ color (Hex { hash = false; value = "374151" }); max_width (Ch 65.) ];
      rule ~selector:".s1" [ font_size (Rem 1.0); line_height (Num 1.75) ];
      rule ~selector:".s1 h1" [ font_size (Em 2.0) ];
      rule ~selector:".s1 h1" [ font_weight Bold ];
    ]
  in

  let btn_rules =
    [
      rule ~selector:".btn"
        [
          padding (Px 10);
          background_color (Hex { hash = false; value = "3b82f6" });
        ];
      rule ~selector:".btn" [ border_radius (Px 4) ];
      rule ~selector:".btn-primary"
        [ background_color (Hex { hash = false; value = "3b82f6" }) ];
      rule ~selector:".btn-secondary"
        [ background_color (Hex { hash = false; value = "6b7280" }) ];
    ]
  in

  let base_layer =
    layer ~name:"base"
      (List.map rule_to_nested
         [
           rule ~selector:"*" [ margin (Px 0); padding (Px 0) ];
           rule ~selector:"body" [ font_family [ Sans_serif ] ];
         ])
  in

  let components_layer =
    layer ~name:"components" (List.map rule_to_nested btn_rules)
  in
  let utilities_layer =
    layer ~name:"utilities" (List.map rule_to_nested s1_rules)
  in

  let top_level_rules =
    [
      rule ~selector:".container" [ max_width (Px 1200); margin Auto ];
      rule ~selector:".container" [ padding (Px 20) ];
    ]
  in

  let stylesheet =
    stylesheet
      [
        Layer base_layer;
        Layer components_layer;
        Layer utilities_layer;
        Rule (List.hd top_level_rules);
        Rule (List.nth top_level_rules 1);
      ]
  in

  (* Get stats before optimization *)
  let stats_before = stats stylesheet in

  (* Optimize the stylesheet *)
  let optimized_stylesheet = optimize stylesheet in

  (* Get stats after optimization *)
  let stats_after = stats optimized_stylesheet in

  (* Pretty print the stats *)
  let stats_str = pp_stats stats_before in
  let stats_diff_str = pp_stats_diff ~before:stats_before ~after:stats_after in

  (* Print stats when test fails to help debug *)
  let utilities_layer_opt =
    List.find_opt (fun l -> l.name = "utilities") stats_after.layers
  in
  let utilities_rules =
    match utilities_layer_opt with Some layer -> layer.rules | None -> 0
  in
  if utilities_rules <> 2 then (
    print_endline "\n--- FAILING TEST DEBUG: Before Optimization ---";
    print_endline stats_str;
    print_endline "\n--- FAILING TEST DEBUG: After Optimization ---";
    print_endline stats_diff_str);

  (* Verify that optimization reduces rule counts *)
  Alcotest.(check bool)
    "optimization reduces or maintains rule count" true
    (stats_after.rules <= stats_before.rules);

  Alcotest.(check bool)
    "optimization reduces or maintains layer rule count" true
    (stats_after.layer_rules <= stats_before.layer_rules);

  (* Verify specific optimization results *)
  (* The utilities layer should have 3 rules after optimization (.s1 rules not merged, .s1 h1 merged) *)
  let utilities_after =
    List.find_opt (fun l -> l.name = "utilities") stats_after.layers
  in
  match utilities_after with
  | Some layer ->
      Alcotest.(check int) "utilities layer optimized to 2 rules" 2 layer.rules
  | None -> Alcotest.fail "utilities layer not found"

let test_no_merge_by_properties () =
  let open Css in
  (* Create non-adjacent rules with identical properties *)
  let rules =
    [
      Css.rule ~selector:".a" [ color (Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:".b" [ margin Zero ];
      Css.rule ~selector:".c" [ color (Hex { hash = false; value = "ff0000" }) ];
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
      Css.rule ~selector:".s1 :where(a strong)" [ color Inherit ];
      Css.rule ~selector:".s1 :where(blockquote strong)" [ color Inherit ];
      Css.rule ~selector:".s1 :where(thead th strong)" [ color Inherit ];
      Css.rule ~selector:".different" [ font_size (Rem 1.0) ];
    ]
  in

  let combined = combine_identical_rules rules in

  (* Should combine the first three into one rule *)
  Alcotest.(check int) "combined rule count" 2 (List.length combined);

  (* Check the combined selector *)
  let first_rule = List.hd combined in
  Alcotest.(check bool)
    "selectors combined with comma" true
    (String.contains (Css.selector first_rule) ',');

  (* Non-consecutive identical rules should not be combined *)
  let rules2 =
    [
      Css.rule ~selector:".a" [ color (Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:".b" [ color (Hex { hash = false; value = "0000ff" }) ];
      Css.rule ~selector:".c" [ color (Hex { hash = false; value = "ff0000" }) ];
    ]
  in

  let combined2 = combine_identical_rules rules2 in
  Alcotest.(check int) "non-consecutive not combined" 3 (List.length combined2)

(* Test that optimization doesn't merge across @supports boundaries *)
let test_no_merge_across_supports_boundary () =
  with_debug "no_merge_across_supports_boundary" @@ fun () ->
  let open Css in
  (* Create rules with same selector separated by @supports *)
  let rule1 = rule ~selector:".feature" [ padding (Px 10) ] in
  let rule2 = rule ~selector:".feature" [ margin (Px 5) ] in
  let supports_rule =
    supports ~condition:"(display: grid)"
      [ rule ~selector:".feature" [ display Grid ] ]
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
    rule ~selector:".critical"
      [ important (color (Hex { hash = false; value = "ff0000" })) ]
  in
  let rule2 = rule ~selector:".critical" [ padding (Px 10) ] in
  let rule3 =
    rule ~selector:".normal" [ color (Hex { hash = false; value = "0000ff" }) ]
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
  let rule1 = rule ~selector:".empty" [] in
  let rule2 = rule ~selector:".empty" [] in
  (* Adjacent empty with same selector *)
  let rule3 = rule ~selector:".has-content" [ padding (Px 10) ] in
  let rule4 = rule ~selector:".also-empty" [] in
  let rule5 = rule ~selector:".another" [ margin (Px 5) ] in

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
  Alcotest.(check string) "first rule is .empty" ".empty" (selector first);
  Alcotest.(check int)
    "first rule still empty" 0
    (List.length (declarations first))

(* Test optimization within nested contexts like @media *)
let test_optimize_within_nested_contexts () =
  let open Css in
  (* Create adjacent rules inside @media that should merge *)
  let media_rules =
    [
      rule ~selector:".responsive" [ padding (Px 10) ];
      rule ~selector:".responsive" [ margin (Px 5) ];
      rule ~selector:".other" [ color (Hex { hash = false; value = "000000" }) ];
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
      rule ~selector:".component"
        [ color (Hex { hash = false; value = "374151" }) ];
      rule ~selector:".component :where(p)" [ margin_top (Em 1.0) ];
      rule ~selector:".component :where(.component>:last-child)"
        [ margin_bottom Zero ];
      rule ~selector:".component" [ font_size (Rem 1.0) ];
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
let test_minify_values () =
  with_debug "minify_values" @@ fun () ->
  let open Css in
  let r =
    rule ~selector:".compact"
      [
        padding (Rem 0.5);
        margin (Px 0);
        opacity 0.5;
        transition_duration (Ms 500);
      ]
  in

  let stylesheet = stylesheet [ Rule r ] in
  let minified = to_string ~minify:true stylesheet in

  (* Debug output if any check will fail *)
  let expected_patterns =
    [ "padding:.5rem"; "opacity:.5"; "transition-duration:500ms"; "margin:0" ]
  in
  let has_unwanted = Astring.String.is_infix ~affix:"margin:0px" minified in
  let missing_pattern =
    List.exists
      (fun p -> not (Astring.String.is_infix ~affix:p minified))
      expected_patterns
  in

  if missing_pattern || has_unwanted then
    debug_css ~label:"minification test" minified
      (expected_patterns @ [ "margin:0px (should NOT be present)" ]);

  (* Check that leading zeros are dropped *)
  Alcotest.(check bool)
    "0.5rem becomes .5rem" true
    (Astring.String.is_infix ~affix:"padding:.5rem" minified);
  Alcotest.(check bool)
    "opacity .5 (not 0.5)" true
    (Astring.String.is_infix ~affix:"opacity:.5" minified);
  Alcotest.(check bool)
    "transition-duration 500ms" true
    (Astring.String.is_infix ~affix:"transition-duration:500ms" minified);

  (* Check that 0 values are simplified *)
  Alcotest.(check bool)
    "0px becomes 0" true
    (Astring.String.is_infix ~affix:"margin:0" minified
    && not (Astring.String.is_infix ~affix:"margin:0px" minified))

(* Test that selector ordering is preserved (no reordering) *)
let test_selector_order_preservation () =
  let open Css in
  (* Create rules with different selectors in specific order *)
  let rules =
    [
      rule ~selector:".z-class" [ padding (Px 10) ];
      rule ~selector:".a-class" [ margin (Px 5) ];
      rule ~selector:".m-class"
        [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:".b-class" [ font_size (Rem 1.0) ];
    ]
  in

  let stylesheet = stylesheet (List.map (fun r -> Rule r) rules) in
  let optimized = optimize stylesheet in
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
  let selectors = List.map selector opt_rules in
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
      rule ~selector:".margins" [ margin (Px 10) ];
      rule ~selector:".colors"
        [ color (Hex { hash = false; value = "ff0000" }) ];
      rule ~selector:".more-margins" [ margin (Px 20) ];
      rule ~selector:".paddings" [ padding (Px 5) ];
      rule ~selector:".more-colors"
        [ color (Hex { hash = false; value = "0000ff" }) ];
    ]
  in

  let stylesheet = stylesheet (List.map (fun r -> Rule r) rules) in
  let optimized = optimize stylesheet in

  (* Verify rules are not grouped by property type *)
  let opt_rules = stylesheet_rules optimized in
  let selectors = List.map selector opt_rules in
  Alcotest.(check (list string))
    "no reordering by property type"
    [ ".margins"; ".colors"; ".more-margins"; ".paddings"; ".more-colors" ]
    selectors

(* Test that cross-context optimization is prevented *)
let test_no_cross_context_optimization () =
  let open Css in
  (* Same selector in different contexts should not merge *)
  let base_layer =
    layer ~name:"base"
      [ rule_to_nested (rule ~selector:".btn" [ padding (Px 10) ]) ]
  in
  let utilities_layer =
    layer ~name:"utilities"
      [ rule_to_nested (rule ~selector:".btn" [ margin (Px 5) ]) ]
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
let test_optimize_nested_rules_preserves_non_adjacent () =
  let open Css in
  (* Create test rules: .s1 rule, then 5 other rules, then another .s1 rule *)
  let nested_rules =
    [
      rule_to_nested
        (rule ~selector:".s1"
           [ color (Hex { hash = false; value = "ff0000" }) ]);
      rule_to_nested
        (rule ~selector:".s2"
           [ color (Hex { hash = false; value = "0000ff" }) ]);
      rule_to_nested
        (rule ~selector:".s3"
           [ color (Hex { hash = false; value = "00ff00" }) ]);
      rule_to_nested
        (rule ~selector:".s4"
           [ color (Hex { hash = false; value = "ffff00" }) ]);
      rule_to_nested
        (rule ~selector:".s5"
           [ color (Hex { hash = false; value = "ff00ff" }) ]);
      rule_to_nested
        (rule ~selector:".s6"
           [ color (Hex { hash = false; value = "ffa500" }) ]);
      rule_to_nested
        (rule ~selector:".s1"
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
let tests =
  [
    ("property creation", `Quick, test_property_creation);
    ("property deduplication", `Quick, test_property_deduplication);
    ("minification", `Quick, test_minification);
    ("media query", `Quick, test_media_query);
    ("inline style", `Quick, test_inline_style);
    ("calc expressions", `Quick, test_calc);
    ("CSS variables", `Quick, test_variables);
    ("grid template", `Quick, test_grid_template);
    ("container query", `Quick, test_container_query);
    ("var extraction", `Quick, test_var_extraction);
    ("animation", `Quick, test_animation);
    ("merge rules", `Quick, test_merge_rules);
    ("adjacent same selector merge", `Quick, test_adjacent_same_selector_merge);
    ( "optimize layer with adjacent rules",
      `Quick,
      test_layer_adjacent_rule_optimization );
    ("no merge non-adjacent", `Quick, test_no_merge_non_adjacent);
    ("non-adjacent same selector", `Quick, test_non_adjacent_same_selector);
    ( "no merge different descendants",
      `Quick,
      test_no_merge_different_descendants );
    ("full optimization with layers", `Quick, test_full_optimization_with_layers);
    ("pp float", `Quick, test_pp_float);
    ("var with fallback", `Quick, test_var_with_fallback);
    (* CSS Optimization tests *)
    ("layer precedence respected", `Quick, test_layer_precedence_respected);
    ("source order within selector", `Quick, test_source_order_within_selector);
    ("selector merging correctness", `Quick, test_selector_merging_correctness);
    ("non merging different rules", `Quick, test_non_merging_different_rules);
    ("cascade with intervening", `Quick, test_cascade_with_intervening);
    ("merge truly adjacent", `Quick, test_merge_truly_adjacent);
    ("cascade order preservation", `Quick, test_cascade_order_preservation);
    ("no merge by properties", `Quick, test_no_merge_by_properties);
    ("combine identical rules", `Quick, test_combine_identical_rules);
    ("optimization stats", `Quick, test_optimization_stats);
    ( "rules grouping breaks cascade order",
      `Quick,
      test_rules_grouping_cascade_bug );
    (* New tests from optimize.md *)
    ( "no merge across @supports boundary",
      `Quick,
      test_no_merge_across_supports_boundary );
    ("!important preservation", `Quick, test_important_preservation);
    ("empty rules handling", `Quick, test_empty_rules_handling);
    ( "optimize within nested contexts",
      `Quick,
      test_optimize_within_nested_contexts );
    ("no duplicate last-child", `Quick, test_no_duplicate_last_child);
    ("minify values", `Quick, test_minify_values);
    ("selector order preservation", `Quick, test_selector_order_preservation);
    ("no property-based reordering", `Quick, test_no_property_based_reordering);
    ("no cross-context optimization", `Quick, test_no_cross_context_optimization);
    ( "optimize_nested_rules preserves non-adjacent",
      `Quick,
      test_optimize_nested_rules_preserves_non_adjacent );
  ]

let suite = [ ("css", tests) ]
