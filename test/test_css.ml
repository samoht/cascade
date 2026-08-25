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
  let stylesheet = Css.v [ layer ~name:[ "utilities" ] [ utility_rule ] ] in

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
   valid dashed-ident per CSS Values 4 sec. 4.3. Tailwind emits these for
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
        layer ~name:[ "theme" ] [ rule ~selector:btn [ color (hex "#ff0000") ] ];
        layer ~name:[ "utilities" ]
          [ rule ~selector:card [ padding [ Px 10. ] ] ];
        rule ~selector:(Selector.class_ "base") [ margin [ Px 5. ] ];
      ]
  in

  (* Test extracting theme layer *)
  let theme_stmts = layer_block [ "theme" ] stylesheet in
  Alcotest.(check bool) "theme layer found" true (Option.is_some theme_stmts);

  let theme_rules = theme_stmts |> Option.get |> rules_of_statements in
  Alcotest.(check int) "theme has one rule" 1 (List.length theme_rules);

  (* Test extracting non-existent layer *)
  let missing = layer_block [ "missing" ] stylesheet in
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

(* Every conditional group at-rule holding [body]. [map] and [sort] speak of
   "all rules at all nesting levels", so each of these has to give the same
   answer as [@media]: they all wrap style rules, and which one wraps them is
   not something a caller rewriting or reordering rules asked about. *)
let conditional_groups body =
  [
    ("@media", Stylesheet.Media (Media.of_string "screen", body));
    ("@supports", Stylesheet.Supports (Supports.of_string "(top: 0)", body));
    ("@container", Stylesheet.Container (None, None, body));
    ("@layer", Stylesheet.Layer (Some [ "a" ], body));
    ("@origin", Stylesheet.Origin (Stylesheet.Author, body));
    ("@scope", Stylesheet.Scope (Some (Selector.class_ "card"), None, body));
    ("@starting-style", Stylesheet.Starting_style body);
    ( "@-moz-document",
      Stylesheet.Moz_document
        ([ Stylesheet.Url_prefix (Some "https://example.com/") ], body) );
    ( "@when",
      Stylesheet.When
        (Stylesheet.Media_condition (Media.of_string "screen"), body) );
    ("@else", Stylesheet.Else (None, body));
  ]

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
      layer ~name:[ "components" ]
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
    && Astring.String.is_infix ~affix:"@layer" css);
  let missed =
    List.filter_map
      (fun (label, stmt) ->
        let mapped = Css.map recolor [ stmt ] in
        let css = Css.to_string ~minify:true (v mapped) in
        if Astring.String.is_infix ~affix:"color:#00f" css then None
        else Some label)
      (conditional_groups
         [ rule ~selector:(Selector.class_ "a") [ color (hex "#ff0000") ] ])
  in
  if missed <> [] then
    Alcotest.failf "map did not reach the rules of: %s"
      (String.concat ", " missed)

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
      layer ~name:[ "base" ]
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
  Alcotest.(check bool) "sort descends into layer" true (bbb < yyy);
  let unsorted =
    List.filter_map
      (fun (label, stmt) ->
        let css = Css.to_string ~minify:true (v (Css.sort cmp [ stmt ])) in
        let at sub = Astring.String.find_sub ~sub css in
        match (at ".aaa", at ".zzz") with
        | Some a, Some z when a < z -> None
        | _ -> Some label)
      (conditional_groups
         [
           rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ];
           rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ];
         ])
  in
  if unsorted <> [] then
    Alcotest.failf "sort did not reach the rules of: %s"
      (String.concat ", " unsorted)

(* An [@else] answers the [@when] before it, so a chain is one unit: sorting may
   not put a rule between its links nor swap them. [sort] moves rules ahead of
   the at-rules they sit among and leaves the at-rules in source order, which
   holds the chain together wherever it sits, including in a block [sort] only
   reaches by descending. *)
let test_spec_sort_when_else_chain () =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Selector.to_string sel1) (Selector.to_string sel2)
  in
  let when_link =
    Stylesheet.When
      ( Stylesheet.Media_condition (Media.of_string "screen"),
        [ rule ~selector:(Selector.class_ "w") [ color (hex "#ff0000") ] ] )
  in
  let else_link =
    Stylesheet.Else
      (None, [ rule ~selector:(Selector.class_ "e") [ color (hex "#00ff00") ] ])
  in
  let chain = [ when_link; else_link ] in
  let zzz = rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ] in
  let aaa = rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ] in
  let check label stmts =
    let css = Css.to_string ~minify:true (v (Css.sort cmp stmts)) in
    Alcotest.(check bool)
      (label ^ ": @else still answers its @when")
      true
      (Astring.String.is_infix
         ~affix:"@when media(screen){.w{color:#f00}}@else{.e{color:#0f0}}" css)
  in
  check "top level" chain;
  check "rules around the chain" ((zzz :: chain) @ [ aaa ]);
  check "rule between the links" [ when_link; aaa; else_link ];
  check "inside @media" [ Stylesheet.Media (Media.of_string "print", chain) ];
  check "inside @scope"
    [ Stylesheet.Scope (Some (Selector.class_ "card"), None, chain) ];
  check "inside @when"
    [
      Stylesheet.When
        (Stylesheet.Media_condition (Media.of_string "print"), chain);
    ]

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
            layer ~name:[ "components" ]
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
        layer ~name:[ "theme" ]
          [
            rule ~selector:Selector.Root
              [ custom_property "--brand-dark" "#000" ];
          ];
      ]
  in
  let utilities =
    layer ~name:[ "utilities" ]
      [
        rule ~selector:(Selector.class_ "m")
          [ custom_property "--space" "1rem" ];
      ]
  in
  let sheet =
    v [ root; layer ~name:[ "theme" ] [ theme_rule ]; nested_theme; utilities ]
  in
  let all_props = custom_props sheet in
  let theme_props = custom_props ~layer:[ "theme" ] sheet in
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

(* [custom_props] reports a custom property wherever it is declared for an
   element: every conditional group that holds style rules, and a bare nesting
   block, whose declarations apply to the enclosing rule's subject. The other
   declaration sites belong to another cascade origin or to no element at all
   (CSS Cascading 5 sec. 6.1), so a name declared only there is not declared for
   the element that reads it. *)
let public_custom_props_declaration_sites () =
  let decl = custom_property "--x" "1" in
  let styled = rule ~selector:(Selector.class_ "a") [ decl ] in
  let frame : Stylesheet.keyframe =
    { selector = Keyframe.Positions [ Keyframe.From ]; declarations = [ decl ] }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ decl ] }
  in
  let reported =
    [
      ("@media", Stylesheet.Media (Media.of_string "screen", [ styled ]));
      ( "@supports",
        Stylesheet.Supports (Supports.of_string "(top: 0)", [ styled ]) );
      ("@container", Stylesheet.Container (None, None, [ styled ]));
      ("@layer", Stylesheet.Layer (Some [ "a" ], [ styled ]));
      ("@origin", Stylesheet.Origin (Stylesheet.Author, [ styled ]));
      ( "@scope",
        Stylesheet.Scope (Some (Selector.class_ "card"), None, [ styled ]) );
      ("@starting-style", Stylesheet.Starting_style [ styled ]);
      ( "@-moz-document",
        Stylesheet.Moz_document
          ([ Stylesheet.Url_prefix (Some "https://example.com/") ], [ styled ])
      );
      ( "@when",
        Stylesheet.When
          (Stylesheet.Media_condition (Media.of_string "screen"), [ styled ]) );
      ("@else", Stylesheet.Else (None, [ styled ]));
      ( "nesting block",
        rule ~selector:(Selector.class_ "a")
          ~nested:[ Stylesheet.Declarations [ decl ] ]
          [] );
    ]
  in
  let hidden =
    [
      ("@keyframes", Stylesheet.keyframes "k" [ frame ]);
      ("@page", Stylesheet.Page ([], [ decl ]));
      ("@page margin box", Stylesheet.Page_with_margins ([], [], [ margin ]));
      ("@position-try", Stylesheet.Position_try ("--pt", [ decl ]));
      ("@supports-condition", Stylesheet.Supports_condition ("--sc", [ decl ]));
    ]
  in
  let missing =
    List.filter_map
      (fun (label, stmt) ->
        if custom_props (v [ stmt ]) = [ "--x" ] then None else Some label)
      reported
  in
  if missing <> [] then
    Alcotest.failf "custom property not reported in: %s"
      (String.concat ", " missing);
  let leaked =
    List.filter_map
      (fun (label, stmt) ->
        if custom_props (v [ stmt ]) = [] then None else Some label)
      hidden
  in
  if leaked <> [] then
    Alcotest.failf "custom property reported outside element matching: %s"
      (String.concat ", " leaked);
  (* A layer holds whatever the conditional groups below it hold, so [layer]
     selects a name declared inside one of them and nothing beside it. *)
  let layered =
    v
      [
        Stylesheet.Layer
          (Some [ "a" ], [ Stylesheet.Scope (None, None, [ styled ]) ]);
        rule ~selector:(Selector.class_ "b") [ custom_property "--out" "2" ];
      ]
  in
  Alcotest.(check (list string))
    "layer selects through @scope" [ "--x" ]
    (custom_props ~layer:[ "a" ] layered);
  Alcotest.(check (list string))
    "unlayered sibling still reported" [ "--x"; "--out" ] (custom_props layered)

(* A [@layer] inside a conditional group is ordinary CSS: the group decides
   whether its contents apply, not whether the layer exists, and a layer named
   there is the same layer a sibling block names (css-cascade-5 sec. 6.4). A
   caller asking which layers a sheet declares gets a wrong answer, not a
   conservative one, when such a block is skipped. *)
let public_layers_conditional_groups () =
  let styled = rule ~selector:(Selector.class_ "a") [ color (hex "#111111") ] in
  let block = Stylesheet.Layer (Some [ "inner" ], [ styled ]) in
  let decl = Stylesheet.Layer_decl [ [ "declared" ] ] in
  let groups =
    [
      ("@media", fun b -> Stylesheet.Media (Media.of_string "screen", b));
      ( "@supports",
        fun b -> Stylesheet.Supports (Supports.of_string "(top: 0)", b) );
      ("@container", fun b -> Stylesheet.Container (None, None, b));
      ("@layer", fun b -> Stylesheet.Layer (Some [ "outer" ], b));
      ("@origin", fun b -> Stylesheet.Origin (Stylesheet.Author, b));
      ( "@scope",
        fun b -> Stylesheet.Scope (Some (Selector.class_ "card"), None, b) );
      ("@starting-style", fun b -> Stylesheet.Starting_style b);
      ( "@-moz-document",
        fun b ->
          Stylesheet.Moz_document
            ([ Stylesheet.Url_prefix (Some "https://example.com/") ], b) );
      ( "@when",
        fun b ->
          Stylesheet.When
            (Stylesheet.Media_condition (Media.of_string "screen"), b) );
      ("@else", fun b -> Stylesheet.Else (None, b));
      ("style rule", fun b -> rule ~selector:(Selector.class_ "b") ~nested:b []);
    ]
  in
  let qualify label name =
    if label = "@layer" then [ "outer"; name ] else [ name ]
  in
  let missing =
    List.filter_map
      (fun (label, group) ->
        let sheet = v [ group [ block; decl ] ] in
        let want_block = qualify label "inner" in
        let want_decl = qualify label "declared" in
        let names = layers sheet in
        if
          List.mem want_block names && List.mem want_decl names
          && layer_block want_block sheet <> None
        then None
        else Some label)
      groups
  in
  if missing <> [] then
    Alcotest.failf "layer not reported inside: %s" (String.concat ", " missing);
  (* A sublayer of an anonymous [@layer { }] has no name any caller can ask for,
     so it is not one of the sheet's declared layers. *)
  Alcotest.(check (list (list string)))
    "anonymous layer hides its sublayers" []
    (layers (v [ Stylesheet.Layer (None, [ block ]) ]));
  (* A layer statement declares a name without opening a block, so it must not
     stand in for the block that fills the layer in later. *)
  let forward_declared =
    v
      [
        Stylesheet.Layer_decl [ [ "one" ]; [ "two" ] ];
        Stylesheet.Layer (Some [ "one" ], [ styled ]);
      ]
  in
  Alcotest.(check (list (list string)))
    "statement declares the order" [ [ "one" ]; [ "two" ] ]
    (layers forward_declared);
  let block_text name sheet =
    match layer_block name sheet with
    | None -> "<no block>"
    | Some stmts -> to_string ~minify:true (v stmts)
  in
  Alcotest.(check string)
    "statement does not shadow the block" ".a{color:#111}"
    (block_text [ "one" ] forward_declared);
  Alcotest.(check string)
    "a name only declared opens no block" "<no block>"
    (block_text [ "two" ] forward_declared);
  (* Names come in source order, so a caller reading them reads the order the
     sheet introduces its layers in. *)
  Alcotest.(check (list (list string)))
    "names in source order"
    [ [ "first" ]; [ "second" ]; [ "third" ] ]
    (layers
       (v
          [
            Stylesheet.Layer_decl [ [ "first" ] ];
            Stylesheet.Media
              ( Media.of_string "screen",
                [ Stylesheet.Layer (Some [ "second" ], [ styled ]) ] );
            Stylesheet.Layer (Some [ "third" ], [ styled ]);
          ]))

(* [Stylesheet.layers] and [Css.layers] answer one question, so they answer it
   the same way: two functions in one library disagreeing about what a sheet
   declares makes the answer depend on which one a caller happened to reach
   for. *)
let stylesheet_layers_agree_with_css () =
  let styled = rule ~selector:(Selector.class_ "a") [ color (hex "#111111") ] in
  let sheets =
    [
      ("dotted name", v [ Stylesheet.Layer (Some [ "foo"; "bar" ], [ styled ]) ]);
      ( "layer in a group",
        v
          [
            Stylesheet.Media
              ( Media.of_string "screen",
                [ Stylesheet.Layer (Some [ "inner" ], [ styled ]) ] );
          ] );
      ( "layer in a rule",
        v
          [
            rule ~selector:(Selector.class_ "b")
              ~nested:[ Stylesheet.Layer (Some [ "deep" ], [ styled ]) ]
              [];
          ] );
      ( "statement form",
        v [ Stylesheet.Layer_decl [ [ "one" ]; [ "two"; "three" ] ] ] );
    ]
  in
  List.iter
    (fun (label, sheet) ->
      Alcotest.(check (list (list string)))
        (label ^ ": Stylesheet.layers matches Css.layers")
        (layers sheet)
        (Css.Stylesheet.layers sheet))
    sheets

(* A [@media] or [@container] is the same at-rule whether or not a group sits
   above it, and the rules it holds are the ones below its brace, not the ones
   that happen to be direct children. A walk that stops at the top level reports
   neither, so a caller gets a wrong answer rather than a partial one. *)
let stylesheet_queries_reach_nested () =
  let styled sel =
    rule ~selector:(Selector.class_ sel) [ color (hex "#111111") ]
  in
  let screen = Media.of_string "screen" in
  let wide = Container.of_string "(width > 10px)" in
  let selectors_of rules =
    List.map (fun (r : Stylesheet.rule) -> Selector.to_string r.selector) rules
  in
  (* The at-rule sits under a group. *)
  let grouped wrap =
    v [ Stylesheet.Supports (Supports.of_string "(top: 0)", [ wrap ]) ]
  in
  let media_in_group = grouped (Stylesheet.Media (screen, [ styled "a" ])) in
  Alcotest.(check (list string))
    "@media under @supports is still a media query" [ ".a" ]
    (List.concat_map
       (fun (_, rules) -> selectors_of rules)
       (Css.Stylesheet.media_queries media_in_group));
  let container_in_group =
    grouped (Stylesheet.Container (Some "card", Some wide, [ styled "a" ]))
  in
  Alcotest.(check (list string))
    "@container under @supports is still a container query" [ ".a" ]
    (List.concat_map
       (fun (_, _, rules) -> selectors_of rules)
       (Css.Stylesheet.container_queries container_in_group));
  (* The rules sit under something inside the at-rule. *)
  let deep_body =
    [
      rule ~selector:(Selector.class_ "outer")
        ~nested:[ styled "nested" ]
        [ color (hex "#111111") ];
      Stylesheet.Layer (Some [ "l" ], [ styled "layered" ]);
    ]
  in
  Alcotest.(check (list string))
    "a media query holds every rule below its brace"
    [ ".outer"; ".nested"; ".layered" ]
    (List.concat_map
       (fun (_, rules) -> selectors_of rules)
       (Css.Stylesheet.media_queries
          (v [ Stylesheet.Media (screen, deep_body) ])));
  Alcotest.(check (list string))
    "a container query holds every rule below its brace"
    [ ".outer"; ".nested"; ".layered" ]
    (List.concat_map
       (fun (_, _, rules) -> selectors_of rules)
       (Css.Stylesheet.container_queries
          (v [ Stylesheet.Container (None, Some wide, deep_body) ])));
  (* [Css.media_queries] is the same walk with the rules wrapped back up as
     statements, so it reaches what the one below it reaches. *)
  let statement_selectors =
    List.concat_map
      (fun (_, stmts) ->
        List.filter_map
          (fun stmt ->
            match as_rule stmt with
            | Some (sel, _, _) -> Some (Selector.to_string sel)
            | None -> None)
          stmts)
      (media_queries (grouped (Stylesheet.Media (screen, deep_body))))
  in
  Alcotest.(check (list string))
    "Css.media_queries reaches the same rules"
    [ ".outer"; ".nested"; ".layered" ]
    statement_selectors

(* [vars_of_rules] answers the same question as [vars_of_stylesheet] over the
   statements it is given, so it reports a reference wherever a declaration
   sits: nested in a rule, inside any grouping at-rule, and in an at-rule that
   holds declarations without a block. A [var()] no walk reaches is a variable a
   caller thinks nothing needs. *)
let public_vars_declaration_sites () =
  let decl = Declaration.of_string "color:var(--x)" in
  let styled = rule ~selector:(Selector.class_ "a") [ decl ] in
  let frame : Stylesheet.keyframe =
    { selector = Keyframe.Positions [ Keyframe.From ]; declarations = [ decl ] }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ decl ] }
  in
  let sites =
    [
      ("@media", Stylesheet.Media (Media.of_string "screen", [ styled ]));
      ( "@supports",
        Stylesheet.Supports (Supports.of_string "(top: 0)", [ styled ]) );
      ("@container", Stylesheet.Container (None, None, [ styled ]));
      ("@layer", Stylesheet.Layer (Some [ "a" ], [ styled ]));
      ("@origin", Stylesheet.Origin (Stylesheet.Author, [ styled ]));
      ( "@scope",
        Stylesheet.Scope (Some (Selector.class_ "card"), None, [ styled ]) );
      ("@starting-style", Stylesheet.Starting_style [ styled ]);
      ( "@-moz-document",
        Stylesheet.Moz_document
          ([ Stylesheet.Url_prefix (Some "https://example.com/") ], [ styled ])
      );
      ( "@when",
        Stylesheet.When
          (Stylesheet.Media_condition (Media.of_string "screen"), [ styled ]) );
      ("@else", Stylesheet.Else (None, [ styled ]));
      ("nested rule", rule ~selector:(Selector.class_ "a") ~nested:[ styled ] []);
      ( "nesting block",
        rule ~selector:(Selector.class_ "a")
          ~nested:[ Stylesheet.Declarations [ decl ] ]
          [] );
      ("@keyframes", Stylesheet.keyframes "k" [ frame ]);
      ("@page", Stylesheet.Page ([], [ decl ]));
      ("@page margin box", Stylesheet.Page_with_margins ([], [], [ margin ]));
      ("@position-try", Stylesheet.Position_try ("--pt", [ decl ]));
      ("@supports-condition", Stylesheet.Supports_condition ("--sc", [ decl ]));
    ]
  in
  let names stmts = List.map any_var_name (vars_of_rules stmts) in
  let missing =
    List.filter_map
      (fun (label, stmt) ->
        if names [ stmt ] = [ "--x" ] then None else Some label)
      sites
  in
  if missing <> [] then
    Alcotest.failf "var() not reported in: %s" (String.concat ", " missing);
  (* One question, one answer: the two entry points differ only in what they are
     handed. *)
  let disagree =
    List.filter_map
      (fun (label, stmt) ->
        if
          names [ stmt ]
          = List.map any_var_name (vars_of_stylesheet (v [ stmt ]))
        then None
        else Some label)
      sites
  in
  if disagree <> [] then
    Alcotest.failf "vars_of_rules and vars_of_stylesheet disagree on: %s"
      (String.concat ", " disagree);
  (* Deduplicated across statements, in source order. *)
  let y = Declaration.of_string "color:var(--y)" in
  Alcotest.(check (list string))
    "deduplicated in source order" [ "--x"; "--y" ]
    (names
       [
         rule ~selector:(Selector.class_ "a") [ decl ];
         Stylesheet.Media (Media.of_string "screen", [ styled ]);
         rule ~selector:(Selector.class_ "b") [ y ];
       ])

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

(* A theme guard is a compile-time filter on the declaration it wraps, so the
   keep-set decides its fate wherever that declaration sits. [@keyframes],
   [@page], [@position-try] and [@supports-condition] hold declarations directly
   rather than in a nested block, so a walk that only descends through blocks
   never reaches their guards and prints them as declarations the theme never
   selected. *)
let public_theme_guards_in_declaration_at_rules () =
  let guarded () = theme_guarded ~var_name:"brand" (color (hex "#ff0000")) in
  let resolved stmts =
    v stmts
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
    |> to_string ~minify:true
  in
  let frame : Stylesheet.keyframe =
    {
      selector = Keyframe.Positions [ Keyframe.From ];
      declarations = [ guarded () ];
    }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ guarded () ] }
  in
  let cases =
    [
      ("@keyframes", [ Stylesheet.keyframes "k" [ frame ] ]);
      ("@-webkit-keyframes", [ Stylesheet.Webkit_keyframes ("k", [ frame ]) ]);
      ("@-moz-keyframes", [ Stylesheet.Moz_keyframes ("k", [ frame ]) ]);
      ("@page", [ Stylesheet.Page ([], [ guarded () ]) ]);
      ("@page margin box", [ Stylesheet.Page_with_margins ([], [], [ margin ]) ]);
      ("@position-try", [ Stylesheet.Position_try ("--pt", [ guarded () ]) ]);
      ( "@supports-condition",
        [ Stylesheet.Supports_condition ("--sc", [ guarded () ]) ] );
    ]
  in
  match
    List.filter_map
      (fun (name, stmts) ->
        let printed = resolved stmts in
        if Astring.String.is_infix ~affix:"color" printed then
          Fmt.kstr Option.some "%s kept the guarded declaration: %S" name
            printed
        else Option.none)
      cases
  with
  | [] -> ()
  | kept ->
      Alcotest.failf "theme guards left unresolved:\n%s"
        (String.concat "\n" kept)

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

(* A newline inside a string produces a <bad-string-token>. In an at-rule
   prelude that token used to serialize to nothing, so [@media <bad-string>]
   reached the media-condition reader as an empty condition: the rule kept its
   body but lost its condition, with no diagnostic. *)
let public_bad_string_prelude_edges () =
  match of_string "@media \"abc\n{ .a { color: red } }" with
  | Error err ->
      Alcotest.failf "lenient parse rejected recoverable CSS: %s"
        (Cascade.Error.to_string err)
  | Ok parsed ->
      Alcotest.(check bool)
        "the lost media condition is reported" true (parsed.warnings <> []);
      Alcotest.(check bool)
        "no unconditional @media is emitted" false
        (Astring.String.is_infix ~affix:"@media{" (minify parsed.stylesheet))

(* The value parsers the [list-style] shorthand is built from, exposed so a
   caller can read a single [list-style-type] / [list-style-image]. *)
let list_style_value_parsers () =
  let ok what = function
    | Some _ -> ()
    | None -> Alcotest.failf "%s should parse" what
  in
  ok "square" (Css.parse_list_style_type "square");
  ok "upper-roman" (Css.parse_list_style_type "upper-roman");
  ok "url()" (Css.parse_list_style_image "url(/carrot.png)");
  ok "none" (Css.parse_list_style_image "none");
  Alcotest.(check bool)
    "an unknown counter style is rejected" true
    (Css.parse_list_style_type "nonsense-style" = None)

(* [font-family] takes a stack, and a token defined as one is what a theme feeds
   back in, so a var() among the entries has to survive the read. *)
let font_family_value_parser () =
  let roundtrip s =
    match Css.parse_font_family s with
    | None -> Alcotest.failf "%s should parse" s
    | Some v ->
        Alcotest.(check string)
          s s
          (Pp.to_string Css.Properties.pp_font_family v)
  in
  roundtrip "Georgia, serif";
  roundtrip "ui-sans-serif";
  roundtrip "var(--font-source-sans-pro), system-ui";
  roundtrip "var(--font-ubuntu-mono)"

(* CSS Syntax 3 sec. 5.4.2 "consume an at-rule" builds an at-rule node for any
   at-keyword, recognised or not; the block it consumes keeps its contents.
   Discarding an unrecognised at-rule is a user-agent cascade step (CSS 2.1 sec.
   4.2 "Rules for handling parsing errors"), not a serialisation step, and a
   transform cannot know which agent reads its output: an agent that later
   implements the name renders the input and a stripped output differently.
   Lightning CSS, esbuild, csso, clean-css and cssnano all keep the at-rule and
   its block. So [to_string] keeps it too, at every block depth, with a body or
   without one. [Optimize.drop_unknown_at_rules] stays available for a caller
   that does want user-agent-equivalent output. *)
let unknown_at_rule_reaches_output () =
  let parse input =
    match of_string ~strict:false input with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let keeps name input expected =
    Alcotest.(check string)
      (name ^ " (serialize)") expected
      (to_string ~minify:true (parse input));
    Alcotest.(check string)
      (name ^ " (optimize)") expected
      (parse input |> optimize |> to_string ~minify:true)
  in
  keeps "a statement-form at-rule reaches the output" "@foo bar;\na{color:red}"
    "@foo bar;a{color:red}";
  keeps "a block-form at-rule takes its contents with it"
    "@future x{a{color:red}}\nb{color:red}"
    "@future x{a{color:red}}b{color:red}";
  keeps "an unknown at-rule nested in @media survives"
    "@media screen{@foo bar;d{color:red}}"
    "@media screen{@foo bar;d{color:red}}";
  (* tw hands cascade [--input-css] written in this vocabulary. *)
  keeps "the Tailwind v4 authoring vocabulary survives"
    "@theme{--c:red}\n@utility btn{color:red}\n.x{color:red}"
    "@theme{--c:red}@utility btn{color:red}.x{color:red}";
  (* An at-rule alone in the stylesheet is the whole output, not nothing. *)
  keeps "an at-rule that is the entire stylesheet is the entire output"
    "@foo bar;" "@foo bar;";
  (* Control: a recognised at-rule is unaffected. *)
  keeps "a recognised at-rule is unaffected" "@media screen{a{color:red}}"
    "@media screen{a{color:red}}";
  (* Pretty output re-parses to the same statement list as the minified one. *)
  let input = "@future x{a{color:red}}\nb{color:red}" in
  Alcotest.(check string)
    "pretty output re-parses to the minified output"
    "@future x{a{color:red}}b{color:red}"
    (to_string ~minify:true (parse (to_string (parse input))))

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
      Alcotest.test_case "list-style value parsers" `Quick
        list_style_value_parsers;
      Alcotest.test_case "font-family value parser" `Quick
        font_family_value_parser;
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
      Alcotest.test_case "spec sort keeps a when/else chain" `Quick
        test_spec_sort_when_else_chain;
      Alcotest.test_case "public fold edge traversal" `Quick public_fold_edges;
      Alcotest.test_case "public custom property scoping" `Quick
        public_custom_props_edges;
      Alcotest.test_case "public custom property declaration sites" `Quick
        public_custom_props_declaration_sites;
      Alcotest.test_case "public layers in conditional groups" `Quick
        public_layers_conditional_groups;
      Alcotest.test_case "stylesheet layers agree with css layers" `Quick
        stylesheet_layers_agree_with_css;
      Alcotest.test_case "stylesheet queries reach nested" `Quick
        stylesheet_queries_reach_nested;
      Alcotest.test_case "public var declaration sites" `Quick
        public_vars_declaration_sites;
      Alcotest.test_case "public property introspection" `Quick
        public_property_edges;
      Alcotest.test_case "public theme guards" `Quick public_theme_edges;
      Alcotest.test_case "public theme guards in declaration at-rules" `Quick
        public_theme_guards_in_declaration_at_rules;
      Alcotest.test_case "public value combinators" `Quick
        public_value_combinator_edges;
      Alcotest.test_case "public theme var rendering" `Quick
        public_theme_var_rendering_edges;
      Alcotest.test_case "public parse recovery edges" `Quick public_parse_edges;
      Alcotest.test_case "public bad-string prelude edges" `Quick
        public_bad_string_prelude_edges;
      Alcotest.test_case "spec unknown at-rule reaches the output" `Quick
        unknown_at_rule_reaches_output;
    ] )
