(** High-level CSS integration tests

    Tests the integration between different CSS modules and end-to-end CSS
    generation using only the public Css module interface.

    Detailed module functionality is tested in dedicated files:
    - test_values.ml - CSS value types
    - test_properties.ml - CSS properties
    - test_selector.ml - CSS selectors
    - test_declaration.ml - CSS declarations
    - test_stylesheet.ml - Stylesheet construction
    - test_optimize.ml - CSS optimization
    - test_variables.ml - CSS variables
    - test_pp.ml - Pretty printing *)

open Cascade
open Css

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

(* Test end-to-end CSS generation *)
let generation () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ color (Hex { hash = true; value = "ff0000" }); padding [ Px 10. ] ];
        rule ~selector:card
          [
            margin [ Px 5. ];
            background_color (Hex { hash = false; value = "ffffff" });
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#ff0000;padding:10px}.card{margin:5px;background-color:#ffffff}\n"
    css

(* Test optimization flag works *)
let optimization_flag () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            color (Hex { hash = true; value = "ff0000" });
            color (Hex { hash = true; value = "0000ff" });
            (* duplicate - should be optimized *)
          ];
      ]
  in

  let css_optimized = Css.to_string ~minify:true ~optimize:true stylesheet in
  Alcotest.(check string)
    "optimized exact" ".btn{color:#0000ff}\n" css_optimized

(* Test layers work end-to-end *)
let layers_integration () =
  let utility_rule = rule ~selector:btn [ padding [ Px 10. ] ] in
  let stylesheet = Css.v [ layer ~name:"utilities" [ utility_rule ] ] in

  let css = Css.to_string ~minify:true stylesheet in

  (* Should contain @layer *)
  Alcotest.(check bool)
    "contains @layer" true
    (Astring.String.is_infix ~affix:"@layer" css);
  Alcotest.(check bool)
    "contains layer name" true
    (Astring.String.is_infix ~affix:"utilities" css)

(* Test media queries work end-to-end *)
let media_integration () =
  let mobile_rule = rule ~selector:btn [ font_size (Rem 0.875) ] in
  let stylesheet =
    Css.v
      [
        media
          ~condition:(Css.Media.of_string "(max-width: 640px)")
          [ mobile_rule ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "media exact" "@media (max-width:640px){.btn{font-size:.875rem}}\n" css

(* Test minify flag *)
let minify_flag () =
  let stylesheet =
    v [ rule ~selector:btn [ color (Hex { hash = true; value = "ff0000" }) ] ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#ff0000}\n" css_minified

(* Test important declarations *)
let important_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            important (color (Hex { hash = true; value = "ff0000" }));
            padding [ Px 10. ];
          ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#ff0000!important;padding:10px}\n" css

(* Test custom properties integration *)
let custom_properties_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ custom_property "--primary-color" "blue"; color (Named Blue) ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "custom properties exact" ".btn{--primary-color:blue;color:blue}\n" css

(* Regression: a custom property name starting with a digit after the -- is a
   valid dashed-ident per CSS Syntax §4.3.11. Tailwind emits these for
   arbitrary-value classes like text-[1A202C]. *)
let var_digit_after_dashes () =
  let css = ".x{font-size:var(--1A202C)}" in
  match Css.of_string css with
  | Error err -> Alcotest.fail ("parse failed: " ^ Css.pp_parse_error err)
  | Ok stylesheet ->
      let out = Css.to_string ~minify:true stylesheet in
      Alcotest.(check string) "var(--1A202C) roundtrip" (css ^ "\n") out

(* CSS Roundtrip Test: Parse generated CSS and compare roundtrip *)
let roundtrip () =
  let read_file path =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let original_css =
    let candidates =
      [ "examples/empty_tailwind.css"; "test/examples/empty_tailwind.css" ]
    in
    match List.find_opt Sys.file_exists candidates with
    | Some path -> read_file path
    | None ->
        Alcotest.fail
          "Could not find empty_tailwind.css (tried examples/ and \
           test/examples/)"
  in

  (* Parse the CSS with context on failure *)
  let parsed_stylesheet =
    match Css.of_string original_css with
    | Ok stylesheet -> stylesheet
    | Error err ->
        (* Format the structured error *)
        let formatted_error = Css.pp_parse_error err in
        Alcotest.fail ("Failed to parse CSS: " ^ formatted_error)
  in

  (* Render it back to string with same settings (minified) *)
  let roundtrip_css = Css.to_string ~minify:true parsed_stylesheet in

  (* Compare - they should be identical *)
  if original_css <> roundtrip_css then
    (* Show where the difference occurs *)
    match Css_tools.String_diff.first_diff_pos original_css roundtrip_css with
    | Some pos ->
        Fmt.epr "CSS roundtrip differs at position %d@." pos;
        Alcotest.fail "CSS roundtrip should be identical"
    | None -> Alcotest.fail "CSS roundtrip should be identical"
  else
    Alcotest.(check string)
      "CSS roundtrip should be identical" original_css roundtrip_css

(* Test AST introspection helpers *)
let test_layer_block () =
  let stylesheet =
    v
      [
        layer ~name:"theme" [ rule ~selector:btn [ color (hex "#ff0000") ] ];
        layer ~name:"utilities" [ rule ~selector:card [ padding [ Px 10. ] ] ];
        rule ~selector:(Selector.class_ "base") [ margin [ Px 5. ] ];
      ]
  in

  (* Test extracting theme layer *)
  let theme_stmts = layer_block "theme" stylesheet in
  Alcotest.(check bool) "theme layer found" true (Option.is_some theme_stmts);

  let theme_rules = theme_stmts |> Option.get |> rules_from_statements in
  Alcotest.(check int) "theme has one rule" 1 (List.length theme_rules);

  (* Test extracting non-existent layer *)
  let missing = layer_block "missing" stylesheet in
  Alcotest.(check bool) "missing layer not found" true (Option.is_none missing)

let test_rules_from_statements () =
  let stmts =
    [
      rule ~selector:btn [ color (hex "#ff0000") ];
      media
        ~condition:(Css.Media.of_string "(min-width: 768px)")
        [ rule ~selector:card [ padding [ Px 5. ] ] ];
      rule ~selector:card [ margin [ Px 10. ] ];
    ]
  in

  let rules = rules_from_statements stmts in
  Alcotest.(check int) "extracts 2 rules from statements" 2 (List.length rules);

  let selectors = List.map (fun (sel, _) -> Selector.to_string sel) rules in
  Alcotest.(check bool) "contains btn selector" true (List.mem ".btn" selectors);
  Alcotest.(check bool)
    "contains card selector" true
    (List.mem ".card" selectors)

let test_custom_prop_names () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let decls = [ color_def; margin_def; padding [ Px 10. ] ] in
  let prop_names = custom_prop_names decls in

  Alcotest.(check int) "finds 2 custom properties" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names)

let test_custom_props_from_rules () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let rules =
    [
      (btn, [ color_def; padding [ Px 10. ] ]);
      (card, [ margin_def; background_color (hex "#ffffff") ]);
    ]
  in

  let prop_names = custom_props_from_rules rules in

  Alcotest.(check int)
    "finds 2 custom properties total" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names);

  (* Test order preservation *)
  Alcotest.(check string)
    "first property is primary-color" "--primary-color" (List.hd prop_names)

(* Test Css.map - transforms rules in statements *)
let test_map () =
  let sel1 = Selector.class_ "foo" in
  let sel2 = Selector.class_ "bar" in
  let stmts =
    [
      rule ~selector:sel1 [ color (Hex { hash = true; value = "ff0000" }) ];
      rule ~selector:sel2 [ color (Hex { hash = true; value = "00ff00" }) ];
    ]
  in

  (* Map that changes all colors to blue *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Hex { hash = true; value = "0000ff" }) ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check string)
    "map changes all rules" ".foo{color:#0000ff}.bar{color:#0000ff}\n" css

(* Test Css.map with nested media queries *)
let test_map_nested () =
  let sel1 = Selector.class_ "foo" in
  let stmts =
    [
      media
        ~condition:(Css.Media.of_string "(min-width:768px)")
        [
          rule ~selector:sel1 [ color (Hex { hash = true; value = "ff0000" }) ];
        ];
    ]
  in

  (* Map should descend into media query *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Hex { hash = true; value = "0000ff" }) ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check bool)
    "map descends into media" true
    (String.contains css '0' && String.contains css 'f')

let test_spec_map_conditional_boundaries () =
  let recolor sel _decls =
    rule ~selector:sel [ color (Hex { hash = true; value = "0000ff" }) ]
  in
  let stmts =
    [
      supports
        ~condition:(Css.Supports.Func ("selector", ":has(img)"))
        [
          container ~name:"card"
            ~condition:(Css.Container.Raw "(inline-size > 30em)")
            [
              rule ~selector:(Selector.class_ "title") [ color (hex "#ff0000") ];
            ];
        ];
      layer ~name:"components"
        [ rule ~selector:(Selector.class_ "inside") [ color (hex "#ff0000") ] ];
    ]
  in
  let mapped = Css.map recolor stmts in
  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check bool)
    "map descends through supports/container" true
    (Astring.String.is_infix ~affix:".title{color:#0000ff}" css);
  Alcotest.(check bool)
    "map descends through layer" true
    (Astring.String.is_infix ~affix:".inside{color:#0000ff}" css);
  Alcotest.(check bool)
    "map preserves condition boundaries" true
    (Astring.String.is_infix ~affix:"@supports" css
    && Astring.String.is_infix ~affix:"@container" css
    && Astring.String.is_infix ~affix:"@layer" css)

(* Test Css.sort - sorts rules by custom comparison *)
let test_sort () =
  let sel1 = Selector.class_ "zzz" in
  let sel2 = Selector.class_ "aaa" in
  let sel3 = Selector.class_ "mmm" in
  let stmts =
    [
      rule ~selector:sel1 [ color (Hex { hash = true; value = "ff0000" }) ];
      rule ~selector:sel2 [ color (Hex { hash = true; value = "00ff00" }) ];
      rule ~selector:sel3 [ color (Hex { hash = true; value = "0000ff" }) ];
    ]
  in

  (* Sort alphabetically by selector *)
  let sorted =
    Css.sort
      (fun (sel1, _) (sel2, _) ->
        String.compare (Selector.to_string sel1) (Selector.to_string sel2))
      stmts
  in

  let css = Css.to_string ~minify:true (v sorted) in
  (* Should be .aaa, .mmm, .zzz order *)
  let aaa_pos = String.index css 'a' in
  let mmm_pos = String.index_from css (aaa_pos + 1) 'm' in
  let zzz_pos = String.index_from css (mmm_pos + 1) 'z' in
  Alcotest.(check bool)
    "sort orders rules alphabetically" true
    (aaa_pos < mmm_pos && mmm_pos < zzz_pos)

(* Test Css.sort with nested media queries *)
let test_sort_nested () =
  let sel1 = Selector.class_ "zzz" in
  let sel2 = Selector.class_ "aaa" in
  let stmts =
    [
      media
        ~condition:(Css.Media.of_string "(min-width:768px)")
        [
          rule ~selector:sel1 [ color (Hex { hash = true; value = "ff0000" }) ];
          rule ~selector:sel2 [ color (Hex { hash = true; value = "00ff00" }) ];
        ];
    ]
  in

  (* Sort should descend into media query and reorder *)
  let sorted =
    Css.sort
      (fun (sel1, _) (sel2, _) ->
        String.compare (Selector.to_string sel1) (Selector.to_string sel2))
      stmts
  in

  let css = Css.to_string ~minify:true (v sorted) in
  (* Inside media, should be .aaa before .zzz *)
  let media_start = String.index css '@' in
  let aaa_pos = String.index_from css media_start 'a' in
  let zzz_pos = String.index_from css aaa_pos 'z' in
  Alcotest.(check bool) "sort descends into media" true (aaa_pos < zzz_pos)

let test_spec_sort_conditional_boundaries () =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Selector.to_string sel1) (Selector.to_string sel2)
  in
  let stmts =
    [
      supports
        ~condition:(Css.Supports.Property ("display", "grid"))
        [
          container ~condition:(Css.Container.Raw "(inline-size > 30em)")
            [
              rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ];
              rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ];
            ];
        ];
      layer ~name:"base"
        [
          rule ~selector:(Selector.class_ "yyy") [ color (hex "#ff0000") ];
          rule ~selector:(Selector.class_ "bbb") [ color (hex "#00ff00") ];
        ];
    ]
  in
  let sorted = Css.sort cmp stmts in
  let css = Css.to_string ~minify:true (v sorted) in
  let aaa = Astring.String.find_sub ~sub:".aaa" css |> Option.get in
  let zzz = Astring.String.find_sub ~sub:".zzz" css |> Option.get in
  let bbb = Astring.String.find_sub ~sub:".bbb" css |> Option.get in
  let yyy = Astring.String.find_sub ~sub:".yyy" css |> Option.get in
  Alcotest.(check bool) "sort descends into container" true (aaa < zzz);
  Alcotest.(check bool) "sort descends into layer" true (bbb < yyy)

let public_fold_edges () =
  let title = Selector.class_ "title" in
  let nested = rule ~selector:title [ color (hex "#0000ff") ] in
  let parent =
    rule ~selector:(Selector.class_ "card")
      ~nested:
        [
          media ~condition:(Css.Media.of_string "(width >= 40em)") [ nested ];
          declarations [ background_color (hex "#ffffff") ];
        ]
      [ padding [ Rem 1. ] ]
  in
  let sheet =
    v
      [
        with_origin Author
          [
            layer ~name:"components"
              [
                supports
                  ~condition:(Css.Supports.Property ("display", "grid"))
                  [
                    container
                      ~condition:(Css.Container.Raw "(inline-size > 30em)")
                      [ parent ];
                  ];
              ];
          ];
        starting_style
          [
            rule ~selector:(Selector.class_ "entry")
              [ opacity (Opacity_number 0.) ];
          ];
      ]
  in
  let rule_count, decl_blocks, boundary_count =
    fold
      (fun (rules, decls, boundaries) stmt ->
        let rules =
          match as_rule stmt with Some _ -> rules + 1 | None -> rules
        in
        let decls =
          match as_declarations stmt with Some _ -> decls + 1 | None -> decls
        in
        let boundaries =
          boundaries
          +
          match
            ( as_origin stmt,
              as_layer stmt,
              as_supports stmt,
              as_container stmt,
              as_media stmt )
          with
          | Some _, _, _, _, _
          | _, Some _, _, _, _
          | _, _, Some _, _, _
          | _, _, _, Some _, _
          | _, _, _, _, Some _ ->
              1
          | _ -> 0
        in
        (rules, decls, boundaries))
      (0, 0, 0) sheet
  in
  Alcotest.(check int) "fold visits all nested rules" 3 rule_count;
  Alcotest.(check int) "fold visits bare declaration block" 1 decl_blocks;
  Alcotest.(check int) "fold visits cascade boundaries" 5 boundary_count

let public_custom_props_edges () =
  let root =
    rule ~selector:Selector.Root
      [ custom_property "--outside" "0"; color (hex "#111111") ]
  in
  let theme_rule =
    rule ~selector:Selector.Root [ custom_property "--brand" "#f00" ]
  in
  let nested_theme =
    media
      ~condition:(Css.Media.of_string "(prefers-color-scheme: dark)")
      [
        layer ~name:"theme"
          [
            rule ~selector:Selector.Root
              [ custom_property "--brand-dark" "#000" ];
          ];
      ]
  in
  let utilities =
    layer ~name:"utilities"
      [
        rule ~selector:(Selector.class_ "m")
          [ custom_property "--space" "1rem" ];
      ]
  in
  let sheet =
    v [ root; layer ~name:"theme" [ theme_rule ]; nested_theme; utilities ]
  in
  let all_props = custom_props sheet in
  let theme_props = custom_props ~layer:"theme" sheet in
  let has name props = List.mem name props in
  Alcotest.(check bool)
    "all props include unlayered" true
    (has "--outside" all_props);
  Alcotest.(check bool)
    "all props include theme" true
    (has "--brand" all_props && has "--brand-dark" all_props);
  Alcotest.(check bool)
    "all props include utilities" true (has "--space" all_props);
  Alcotest.(check bool)
    "theme props include named layer" true
    (has "--brand" theme_props && has "--brand-dark" theme_props);
  Alcotest.(check bool)
    "theme props exclude siblings" false
    (has "--outside" theme_props || has "--space" theme_props)

let public_property_edges () =
  let sheet =
    property ~name:"--gap" Length ~initial_value:(Px 1.) ~inherits:false ()
  in
  match statements sheet with
  | [ stmt ] -> (
      match as_property stmt with
      | Some (Property_info info) -> (
          Alcotest.(check string) "registered name" "--gap" info.name;
          Alcotest.(check bool) "inherits flag" false info.inherits;
          match (info.syntax, info.initial_value) with
          | Css.Variables.Length, Some (Px n) ->
              Alcotest.(check (float 0.0001)) "initial value" 1. n
          | _ -> Alcotest.fail "registered property shape changed")
      | None -> Alcotest.fail "expected @property statement")
  | _ -> Alcotest.fail "expected one @property statement"

let public_theme_edges () =
  let guarded = theme_guarded ~var_name:"brand" (color (hex "#ff0000")) in
  let sheet =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [ guarded; background_color (hex "#ffffff") ];
      ]
  in
  let empty_theme = Css.Pp.String_set.empty in
  let brand_theme = Css.Pp.String_set.add "brand" empty_theme in
  Alcotest.(check string)
    "guarded declaration hidden" ".card{background-color:#ffffff}\n"
    (to_string ~minify:true ~theme:empty_theme sheet);
  Alcotest.(check string)
    "guarded declaration shown"
    ".card{color:#ff0000;background-color:#ffffff}\n"
    (to_string ~minify:true ~theme:brand_theme sheet)

let public_parse_edges () =
  match of_string ~filename:"spec.css" ".a{color:rgb(300)}" with
  | Ok _ -> Alcotest.fail "strict parser accepted invalid declaration"
  | Error err ->
      let msg = pp_parse_error err in
      Alcotest.(check bool)
        "parse error carries filename" true
        (Astring.String.is_infix ~affix:"spec.css" msg);
      let parsed =
        parse ~filename:"spec.css"
          ".a{color:rgb(300)}.b{color:red}.c{color:rgb(301)}"
      in
      let rules = rule_statements parsed.stylesheet in
      let declaration_counts =
        List.map
          (fun statement ->
            match as_rule statement with
            | Some (_, declarations, _) -> List.length declarations
            | None -> Alcotest.fail "expected a qualified rule")
          rules
      in
      Alcotest.(check (list int))
        "partial parser discards invalid declarations, not containing rules"
        [ 0; 1; 0 ] declaration_counts;
      Alcotest.(check int)
        "partial parser reports invalid rules" 2
        (List.length parsed.warnings)

let suite =
  ( "css",
    [
      (* Integration tests using public Css interface only *)
      Alcotest.test_case "CSS generation end-to-end" `Quick generation;
      Alcotest.test_case "optimization flag works" `Quick optimization_flag;
      Alcotest.test_case "layers integration" `Quick layers_integration;
      Alcotest.test_case "media queries integration" `Quick media_integration;
      Alcotest.test_case "minify flag" `Quick minify_flag;
      Alcotest.test_case "important declarations" `Quick important_integration;
      Alcotest.test_case "custom properties" `Quick
        custom_properties_integration;
      Alcotest.test_case "var(--1A202C) parses" `Quick var_digit_after_dashes;
      Alcotest.test_case "CSS roundtrip parsing" `Quick roundtrip;
      (* AST introspection helpers *)
      Alcotest.test_case "layer_block extraction" `Quick test_layer_block;
      Alcotest.test_case "rules_from_statements" `Quick
        test_rules_from_statements;
      Alcotest.test_case "custom_prop_names" `Quick test_custom_prop_names;
      Alcotest.test_case "custom_props_from_rules" `Quick
        test_custom_props_from_rules;
      (* Statement transformation helpers *)
      Alcotest.test_case "map transforms rules" `Quick test_map;
      Alcotest.test_case "map nested in media" `Quick test_map_nested;
      Alcotest.test_case "spec map conditional boundaries" `Quick
        test_spec_map_conditional_boundaries;
      Alcotest.test_case "sort orders rules" `Quick test_sort;
      Alcotest.test_case "sort nested in media" `Quick test_sort_nested;
      Alcotest.test_case "spec sort conditional boundaries" `Quick
        test_spec_sort_conditional_boundaries;
      Alcotest.test_case "public fold edge traversal" `Quick public_fold_edges;
      Alcotest.test_case "public custom property scoping" `Quick
        public_custom_props_edges;
      Alcotest.test_case "public property introspection" `Quick
        public_property_edges;
      Alcotest.test_case "public theme guards" `Quick public_theme_edges;
      Alcotest.test_case "public parse recovery edges" `Quick public_parse_edges;
    ] )
