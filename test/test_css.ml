(* Tests for CSS module *)
open Css

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

let test_pp_float_edge_cases () =
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

  (* 4. Generate the CSS with optimizations enabled *)
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
    ("pp float", `Quick, test_pp_float);
    ("var with fallback", `Quick, test_var_with_fallback);
    ("pp float edge cases", `Quick, test_pp_float_edge_cases);
    (* CSS Optimization tests *)
    ("layer precedence respected", `Quick, test_layer_precedence_respected);
    ("source order within selector", `Quick, test_source_order_within_selector);
    ("selector merging correctness", `Quick, test_selector_merging_correctness);
    ("non merging different rules", `Quick, test_non_merging_different_rules);
    ("cascade with intervening", `Quick, test_cascade_with_intervening);
    ("merge truly adjacent", `Quick, test_merge_truly_adjacent);
    ("cascade order preservation", `Quick, test_cascade_order_preservation);
  ]

let suite = [ ("css", tests) ]
