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

(* Test helper: compose optimize + minified to_string the way [to_string
   ~minify:true] used to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

(* Test end-to-end CSS generation *)
let generation () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ color (Css.Values.hex "ff0000"); padding [ Px 10. ] ];
        rule ~selector:card
          [ margin [ Px 5. ]; background_color (Css.Values.hex "ffffff") ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#f00;padding:10px}.card{margin:5px;background-color:#fff}" css;
  Alcotest.(check string)
    "generation optimize+minify"
    ".btn{color:red;padding:10px}.card{margin:5px;background-color:#fff}"
    (minify stylesheet)

(* Test optimization flag works *)
let optimization_flag () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            color (Css.Values.hex "ff0000");
            color (Css.Values.hex "0000ff");
            (* duplicate - should be optimized *)
          ];
      ]
  in

  let css_optimized = minify stylesheet in
  Alcotest.(check string) "optimized exact" ".btn{color:#00f}" css_optimized

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
    "media exact" "@media(max-width:640px){.btn{font-size:.875rem}}" css

(* Test minify flag *)
let minify_flag () =
  let stylesheet =
    v [ rule ~selector:btn [ color (Css.Values.hex "ff0000") ] ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#f00}" css_minified;
  Alcotest.(check string)
    "minified optimize+minify" ".btn{color:red}" (minify stylesheet)

let explicit_phase_pipeline () =
  let stylesheet =
    v
      [
        rule ~selector:(Selector.class_ "foo") [ color (hex "#0000ff") ];
        rule ~selector:(Selector.class_ "bar") [ color (hex "#0000ff") ];
      ]
  in
  Alcotest.(check string)
    "to_string minifies tokens without optimizing AST shape"
    ".foo{color:#00f}.bar{color:#00f}"
    (Css.to_string ~minify:true stylesheet);
  Alcotest.(check string)
    "optimize is the explicit AST optimization phase" ".bar,.foo{color:#00f}"
    (minify stylesheet);
  let theme = Css.Pp.String_set.(empty |> add "brand") in
  let guarded =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [
            theme_guarded ~var_name:"brand" (color (hex "#ff0000"));
            background_color (hex "#ffffff");
          ];
      ]
  in
  Alcotest.(check string)
    "to_string does not resolve theme guards"
    ".card{color:#f00;background-color:#fff}"
    (Css.to_string ~minify:true guarded);
  Alcotest.(check string)
    "resolve_theme is the explicit theme phase"
    ".card{color:#f00;background-color:#fff}"
    (guarded |> Css.resolve_theme ~theme |> Css.to_string ~minify:true);
  Alcotest.(check string)
    "resolve_theme output can still optimize"
    ".card{color:red;background-color:#fff}"
    (guarded |> Css.resolve_theme ~theme |> minify);
  Alcotest.(check string)
    "resolve_theme can drop inactive theme guards"
    ".card{background-color:#fff}"
    (guarded
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
    |> Css.to_string ~minify:true);
  let spacing_decl, spacing = var "spacing" Length (Rem 0.25) in
  let var_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [ spacing_decl; padding [ Var spacing ] ];
      ]
  in
  Alcotest.(check string)
    "to_string does not inline vars"
    ".p-1{--spacing:.25rem;padding:var(--spacing)}"
    (Css.to_string ~minify:true var_sheet);
  Alcotest.(check string)
    "inline_vars is the explicit variable substitution phase"
    ".p-1{padding:.25rem}"
    (var_sheet |> Css.inline_vars |> Css.to_string ~minify:true)

(* Test important declarations *)
let important_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ important (color (Css.Values.hex "ff0000")); padding [ Px 10. ] ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#f00!important;padding:10px}" css;
  Alcotest.(check string)
    "important optimize+minify" ".btn{color:red!important;padding:10px}"
    (minify stylesheet)

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
    "custom properties exact" ".btn{--primary-color:blue;color:blue}" css;
  Alcotest.(check string)
    "custom properties optimize+minify" ".btn{--primary-color:blue;color:#00f}"
    (minify stylesheet)

(* Regression: a custom property name starting with a digit after the -- is a
   valid dashed-ident per CSS Syntax §4.3.11. Tailwind emits these for
   arbitrary-value classes like text-[1A202C]. *)
let var_digit_after_dashes () =
  let css = ".x{font-size:var(--1A202C)}" in
  match Css.of_string ~strict:false css with
  | Error err -> Alcotest.fail ("parse failed: " ^ Cascade.Error.to_string err)
  | Ok parsed ->
      let out = Css.to_string ~minify:true parsed.stylesheet in
      Alcotest.(check string) "var(--1A202C) roundtrip" css out

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

  (* Parse-then-minify must be an idempotent fixed point: a second pass over the
     minified output should produce byte-identical CSS. The input fixture comes
     from Tailwind, whose valid minification choices do not have to match
     cascade's canonical printer byte for byte. *)
  let parse_or_fail label css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        let formatted_error = Cascade.Error.to_string err in
        Alcotest.fail ("Failed to parse " ^ label ^ ": " ^ formatted_error)
  in
  let first_pass =
    Css.to_string ~minify:true (parse_or_fail "input" original_css)
  in
  let second_pass =
    Css.to_string ~minify:true (parse_or_fail "first pass" first_pass)
  in
  if first_pass <> second_pass then
    match Cascade_diff.String_diff.first_diff_pos first_pass second_pass with
    | Some pos ->
        Fmt.epr "CSS roundtrip differs at position %d@." pos;
        Alcotest.fail "CSS roundtrip should be idempotent"
    | None -> Alcotest.fail "CSS roundtrip should be idempotent"
  else
    Alcotest.(check string)
      "CSS roundtrip should be idempotent" first_pass second_pass

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

  let theme_rules = theme_stmts |> Option.get |> rules_of_statements in
  Alcotest.(check int) "theme has one rule" 1 (List.length theme_rules);

  (* Test extracting non-existent layer *)
  let missing = layer_block "missing" stylesheet in
  Alcotest.(check bool) "missing layer not found" true (Option.is_none missing)

let test_rules_of_statements () =
  let stmts =
    [
      rule ~selector:btn [ color (hex "#ff0000") ];
      media
        ~condition:(Css.Media.of_string "(min-width: 768px)")
        [ rule ~selector:card [ padding [ Px 5. ] ] ];
      rule ~selector:card [ margin [ Px 10. ] ];
    ]
  in

  let rules = rules_of_statements stmts in
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

let test_custom_props_of_rules () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let rules =
    [
      (btn, [ color_def; padding [ Px 10. ] ]);
      (card, [ margin_def; background_color (hex "#ffffff") ]);
    ]
  in

  let prop_names = custom_props_of_rules rules in

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
      rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
      rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
    ]
  in

  (* Map that changes all colors to blue *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Css.Values.hex "0000ff") ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = minify (v mapped) in
  Alcotest.(check string) "map changes all rules" ".bar,.foo{color:#00f}" css

(* Test Css.map with nested media queries *)
let test_map_nested () =
  let sel1 = Selector.class_ "foo" in
  let stmts =
    [
      media
        ~condition:(Css.Media.of_string "(min-width:768px)")
        [ rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ] ];
    ]
  in

  (* Map should descend into media query *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Css.Values.hex "0000ff") ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check bool)
    "map descends into media" true
    (Astring.String.is_infix ~affix:"color:#00f" css)

let test_spec_map_conditional_boundaries () =
  let recolor sel _decls =
    rule ~selector:sel [ color (Css.Values.hex "0000ff") ]
  in
  let stmts =
    [
      supports
        ~condition:(Css.Supports.func "selector" ":has(img)")
        [
          container ~name:"card"
            ~condition:(Css.Container.of_string "(inline-size > 30em)")
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
    (Astring.String.is_infix ~affix:".title{color:#00f}" css);
  Alcotest.(check bool)
    "map descends through layer" true
    (Astring.String.is_infix ~affix:".inside{color:#00f}" css);
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
      rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
      rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
      rule ~selector:sel3 [ color (Css.Values.hex "0000ff") ];
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
          rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
          rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
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
        ~condition:(Css.Supports.property "display" "grid")
        [
          container
            ~condition:(Css.Container.of_string "(inline-size > 30em)")
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
                  ~condition:(Css.Supports.property "display" "grid")
                  [
                    container
                      ~condition:
                        (Css.Container.of_string "(inline-size > 30em)")
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
    rule ~selector:Selector.Root [ custom_property "--brand" "red" ]
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
    "guarded declaration hidden" ".card{background-color:#fff}"
    (sheet |> Css.resolve_theme ~theme:empty_theme |> to_string ~minify:true);
  Alcotest.(check string)
    "guarded declaration shown" ".card{color:#f00;background-color:#fff}"
    (sheet |> Css.resolve_theme ~theme:brand_theme |> to_string ~minify:true);
  Alcotest.(check string)
    "guarded declaration shown optimize+minify"
    ".card{color:red;background-color:#fff}"
    (sheet
    |> Css.resolve_theme ~theme:brand_theme
    |> Css.optimize |> to_string ~minify:true)

let public_value_combinator_edges () =
  let _spacing_decl, spacing = var "spacing" Length (Rem 0.25) in
  let check_padding_calc ?optimized label expected calc =
    let sheet =
      v [ rule ~selector:(Selector.class_ "p-1") [ padding [ Calc calc ] ] ]
    in
    Alcotest.(check string) label expected (to_string ~minify:true sheet);
    match optimized with
    | None -> ()
    | Some expected ->
        Alcotest.(check string)
          (label ^ " optimize+minify")
          expected (minify sheet)
  in
  let calc_var_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [ padding [ Calc (Var spacing) ] ];
      ]
  in
  Alcotest.(check string)
    "padding calc(var()) preserves runtime boundary"
    ".p-1{padding:calc(var(--spacing))}"
    (to_string ~minify:true calc_var_sheet);
  let calc_lifted_var_x1_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [
            padding
              [ Calc (Calc.mul (Calc.length (Var spacing)) (Calc.float 1.0)) ];
          ];
      ]
  in
  Alcotest.(check string)
    "padding calc lifted var times one keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing)*1)}"
    (to_string ~minify:true calc_lifted_var_x1_sheet);
  check_padding_calc "padding one times lifted var keeps var arithmetic"
    ".p-1{padding:calc(1*var(--spacing))}"
    (Calc.mul (Calc.float 1.0) (Calc.length (Var spacing)));
  check_padding_calc "padding lifted var divided by one keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing)/1)}"
    (Calc.div (Calc.length (Var spacing)) (Calc.float 1.0));
  check_padding_calc "padding lifted var plus zero keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing) + 0px)}"
    (Calc.add (Calc.length (Var spacing)) (Calc.length (Px 0.)));
  check_padding_calc "padding zero plus lifted var keeps var arithmetic"
    ".p-1{padding:calc(0px + var(--spacing))}"
    (Calc.add (Calc.length (Px 0.)) (Calc.length (Var spacing)));
  check_padding_calc "padding lifted var minus zero keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing) - 0px)}"
    (Calc.sub (Calc.length (Var spacing)) (Calc.length (Px 0.)));
  check_padding_calc "padding nested lifted var identities keep var arithmetic"
    ".p-1{padding:calc(var(--spacing)*1 + 0px)}"
    (Calc.add
       (Calc.mul (Calc.length (Var spacing)) (Calc.float 1.0))
       (Calc.length (Px 0.)));
  check_padding_calc ~optimized:".p-1{padding:calc(3px + var(--spacing))}"
    "padding var-free left subtree may fold before var"
    ".p-1{padding:calc(1px + 2px + var(--spacing))}"
    (Calc.add
       (Calc.add (Calc.length (Px 1.)) (Calc.length (Px 2.)))
       (Calc.length (Var spacing)));
  check_padding_calc ~optimized:".p-1{padding:calc(var(--spacing)*6)}"
    "padding var-free right subtree may fold after var"
    ".p-1{padding:calc(var(--spacing)*2*3)}"
    (Calc.mul
       (Calc.length (Var spacing))
       (Calc.mul (Calc.float 2.0) (Calc.float 3.0)));
  check_padding_calc ~optimized:".p-1{padding:calc(50% - var(--spacing))}"
    "padding var-free percentage subtree may fold before var"
    ".p-1{padding:calc(100%/2 - var(--spacing))}"
    (Calc.sub
       (Calc.div (Calc.length (Pct 100.)) (Calc.float 2.0))
       (Calc.length (Var spacing)));
  let bare_var_sheet =
    v [ rule ~selector:(Selector.class_ "p-1") [ padding [ Var spacing ] ] ]
  in
  Alcotest.(check string)
    "padding bare var stays bare" ".p-1{padding:var(--spacing)}"
    (to_string ~minify:true bare_var_sheet);

  (* [Calc.float] is generic ['a calc], so a non-length calc - here a flex_basis
     calc with a numeric multiplier - can be built through the typed API. *)
  let flex_basis_calc_sheet =
    v
      [
        rule
          ~selector:(Selector.class_ "basis-4")
          [ flex_basis (Calc Calc.(var "spacing" * float 4.)) ];
      ]
  in
  Alcotest.(check string)
    "flex-basis calc multiplier builds via generic Calc.float"
    ".basis-4{flex-basis:calc(var(--spacing)*4)}"
    (to_string ~minify:true flex_basis_calc_sheet);
  (* z-index carries a typed calc, so a var-based z-index calc builds through
     the typed API rather than a raw string. *)
  let z_index_calc_sheet =
    v
      [
        rule ~selector:(Selector.class_ "z")
          [ z_index (Calc (Calc.var "layer")) ];
      ]
  in
  Alcotest.(check string)
    "z-index calc builds via typed calc" ".z{z-index:calc(var(--layer))}"
    (to_string ~minify:true z_index_calc_sheet);
  (* order carries a typed calc, so a var-based order calc builds through the
     typed API rather than a raw string. *)
  let order_calc_sheet =
    v [ rule ~selector:(Selector.class_ "o") [ order (Calc (Calc.var "o")) ] ]
  in
  Alcotest.(check string)
    "order calc builds via typed calc" ".o{order:calc(var(--o))}"
    (to_string ~minify:true order_calc_sheet);
  (* grid-line carries a typed calc, so col-start-[calc(...)] and similar
     arbitraries build through the typed API rather than a raw string. *)
  let grid_line_calc_sheet =
    v
      [
        rule ~selector:(Selector.class_ "g")
          [ grid_column_start (Calc (Calc.var "n")) ];
      ]
  in
  Alcotest.(check string)
    "grid-line calc builds via typed calc"
    ".g{grid-column-start:calc(var(--n))}"
    (to_string ~minify:true grid_line_calc_sheet);

  let sheet =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [
            border_radius (radius (Rem 0.375));
            gap (gaps ~column:(Rem 0.75) (Rem 0.5));
            font_family (font_stack [ Ui_sans_serif; System_ui; Sans_serif ]);
            text_shadow
              (text_shadow_value ~blur:(Px 4.) ~color:(hex "#000000") (Px 1.)
                 (Px 2.));
            aspect_ratio (ratio 16. 9.);
            columns (columns_both (Rem 12.) 3);
            counter_reset (counter_set [ counter_item ~value:1 "section" ]);
            mask (mask_layers [ mask_layer ~image:(url "mask.svg") () ]);
            outline
              (outline_shorthand ~width:(Px 2.) ~style:Solid
                 ~color:(hex "#0000ff") ());
          ];
        rule
          ~selector:(Selector.class_ "helpers")
          [
            object_position (position_xy (Px 10.) (Px 20.));
            text_overflow (text_overflow_pair Clip (text_overflow_string "..."));
            content
              (content_list
                 [ content_string "Section "; content_counter "section" ]);
            background_image
              (conic_gradient
                 ~config:
                   (conic_gradient_config ~angle:(Deg 45.)
                      ~position:(position_length (Pct 50.))
                      ())
                 [ color_stop (hex "#ff0000"); color_stop (hex "#0000ff") ]);
            background_size (background_size_pair (Px 20.) (Px 30.));
            object_view_box (object_view_box_inset ~right:(Px 1.) (Px 0.));
            grid_template_columns
              (grid_tracks
                 [ Fr 1.; grid_repeat (Count 2) [ Min_max (Px 0., Fr 1.) ] ]);
            grid_row (grid_line_span 2, grid_line_name "footer");
            transform
              (transform_list
                 [ Translate (Px 1., Some (Px 2.)); Rotate (Deg 45.) ]);
            filter (filter_list [ Blur (Px 4.); Opacity (Pct 50.) ]);
            cursor (cursor_url ~hotspot:(1., 2.) ~fallback:Pointer "cursor.svg");
            contain (contain_list [ Layout; Paint ]);
            border_spacing (border_spacing_values [ Px 1.; Px 2. ]);
            border_inline_color
              (logical_border_colors (hex "#ffffff") (hex "#000000"));
            list_style_type
              (list_style_symbols ~kind:Cyclic
                 [
                   list_style_symbol_string "*"; list_style_symbol_url "dot.svg";
                 ]);
            list_style_image (list_style_image_url "bullet.svg");
            fill
              (svg_paint_url
                 ~fallback:(svg_paint_color (hex "#ff0000"))
                 "#paint");
          ];
      ]
  in
  Alcotest.(check string)
    "simple value helpers"
    ".card{border-radius:.375rem;gap:.5rem \
     .75rem;font-family:ui-sans-serif,system-ui,sans-serif;text-shadow:1px 2px \
     4px #000;aspect-ratio:16/9;columns:12rem 3;counter-reset:section \
     1;mask:url(mask.svg);outline:2px solid #00f}.helpers{object-position:10px \
     20px;text-overflow:clip \"...\";content:\"Section \" \
     counter(section);background-image:conic-gradient(from 45deg at \
     50%,#f00,#00f);background-size:20px 30px;object-view-box:inset(0px \
     1px);grid-template-columns:1fr repeat(2,minmax(0px,1fr));grid-row:span \
     2/footer;transform:translate(1px,2px)rotate(45deg);filter:blur(4px)opacity(50%);cursor:url(cursor.svg) \
     1 2,pointer;contain:layout paint;border-spacing:1px \
     2px;border-inline-color:#fff #000;list-style-type:symbols(cyclic\"*\" \
     url(dot.svg));list-style-image:url(bullet.svg);fill:url(#paint)#f00}"
    (to_string ~minify:true sheet);
  Alcotest.(check string)
    "simple value helpers optimize+minify"
    ".card{border-radius:.375rem;gap:.5rem \
     .75rem;font-family:ui-sans-serif,system-ui,sans-serif;text-shadow:1px 2px \
     4px #000;aspect-ratio:16/9;columns:12rem 3;counter-reset:section \
     1;mask:url(mask.svg);outline:2px solid #00f}.helpers{object-position:10px \
     20px;text-overflow:clip \"...\";content:\"Section \" \
     counter(section);background-image:conic-gradient(from 45deg at \
     50%,red,#00f);background-size:20px 30px;object-view-box:inset(0 \
     1px);grid-template-columns:1fr repeat(2,minmax(0,1fr));grid-row:span \
     2/footer;transform:translate(1px,2px)rotate(45deg);filter:blur(4px)opacity(.5);cursor:url(cursor.svg) \
     1 2,pointer;contain:layout paint;border-spacing:1px \
     2px;border-inline-color:#fff #000;list-style-type:symbols(cyclic\"*\" \
     url(dot.svg));list-style-image:url(bullet.svg);fill:url(#paint)red}"
    (minify sheet)

let public_theme_var_rendering_edges () =
  let sans_stack : font_family =
    font_stack [ Ui_sans_serif; System_ui; Sans_serif ]
  in
  let fallback_stack : font_family = font_stack [ Arial; Sans_serif ] in
  let _font_decl, font_sans = var "font-sans" Font_family sans_stack in
  let _font_fb_decl, font_fallback =
    var "font-fallback" Font_family ~fallback:(Fallback fallback_stack)
      sans_stack
  in
  let sheet_for decl =
    v [ rule ~selector:(Selector.class_ "font-sans") [ font_family decl ] ]
  in
  let empty_theme = Css.Pp.String_set.empty in
  let font_theme = Css.Pp.String_set.add "font-sans" empty_theme in
  let fallback_theme = Css.Pp.String_set.add "font-fallback" empty_theme in
  let resolve_font = function
    | "font-sans" -> Some "Arial,sans-serif"
    | "font-fallback" -> Some "Arial,sans-serif"
    | _ -> None
  in
  let render_theme ?theme ?theme_defaults sheet =
    sheet |> Css.resolve_theme ?theme ?theme_defaults |> to_string ~minify:true
  in
  Alcotest.(check string)
    "kept theme var keeps its reference and gains a :root default"
    ":root{--font-sans:Arial,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (render_theme ~theme:font_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "kept theme var with fallback gains a :root default"
    ":root{--font-fallback:Arial,sans-serif}.font-sans{font-family:var(--font-fallback,Arial,sans-serif)}"
    (render_theme ~theme:fallback_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_fallback)));
  Alcotest.(check string)
    "theme default emitted in :root without an explicit theme"
    ":root{--font-sans:Arial,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (render_theme ~theme_defaults:resolve_font (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "inline stylesheet may use typed default"
    ".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}"
    (sheet_for (Var font_sans) |> Css.inline_vars |> to_string ~minify:true);
  Alcotest.(check string)
    "inline stylesheet may use typed fallback default"
    ".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}"
    (sheet_for (Var font_fallback) |> Css.inline_vars |> to_string ~minify:true);
  Alcotest.(check string)
    "theme_defaults still resolves non-theme vars"
    ".font-sans{font-family:Arial,sans-serif}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "theme_defaults still resolves non-theme vars with fallback"
    ".font-sans{font-family:Arial,sans-serif}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_fallback)));
  (* A runtime channel var ([--tw-duration]) keeps its live [var()] reference,
     while a theme default reachable only through its fallback
     ([--default-transition-duration]) is inlined - the theme-inline transition
     shape. The kept wrapper must survive even though its fallback is a concrete
     duration. *)
  let tw_duration : duration var =
    var_ref ~fallback:(Var_fallback "default-transition-duration") "tw-duration"
  in
  let resolve_duration = function
    | "default-transition-duration" -> Some ".1s"
    | _ -> None
  in
  Alcotest.(check string)
    "kept var keeps its wrapper while the nested theme default inlines"
    ".transition{transition-duration:var(--tw-duration,.1s)}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_duration
       (v
          [
            rule
              ~selector:(Selector.class_ "transition")
              [ transition_duration (Var tw_duration) ];
          ]))

let public_parse_edges () =
  match of_string ~strict:true ~filename:"spec.css" ".a{color:rgb(300)}" with
  | Ok _ -> Alcotest.fail "strict parser accepted invalid declaration"
  | Error err ->
      let msg = Cascade.Error.to_string err in
      Alcotest.(check bool)
        "parse error carries filename" true
        (Astring.String.is_infix ~affix:"spec.css" msg);
      let parsed =
        match
          of_string ~strict:false ~filename:"spec.css"
            ".a{color:rgb(300)}.b{color:red}.c{color:rgb(301)}"
        with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected recoverable CSS: %s"
              (Cascade.Error.to_string err)
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
      Alcotest.test_case "explicit phase pipeline" `Quick
        explicit_phase_pipeline;
      Alcotest.test_case "important declarations" `Quick important_integration;
      Alcotest.test_case "custom properties" `Quick
        custom_properties_integration;
      Alcotest.test_case "var(--1A202C) parses" `Quick var_digit_after_dashes;
      Alcotest.test_case "CSS roundtrip parsing" `Quick roundtrip;
      (* AST introspection helpers *)
      Alcotest.test_case "layer_block extraction" `Quick test_layer_block;
      Alcotest.test_case "rules_of_statements" `Quick test_rules_of_statements;
      Alcotest.test_case "custom_prop_names" `Quick test_custom_prop_names;
      Alcotest.test_case "custom_props_of_rules" `Quick
        test_custom_props_of_rules;
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
      Alcotest.test_case "public value combinators" `Quick
        public_value_combinator_edges;
      Alcotest.test_case "public theme var rendering" `Quick
        public_theme_var_rendering_edges;
      Alcotest.test_case "public parse recovery edges" `Quick public_parse_edges;
    ] )
