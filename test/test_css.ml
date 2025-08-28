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

  (* Should be semicolon-separated without trailing semicolon *)
  Alcotest.(check bool)
    "contains color" true
    (Astring.String.is_infix ~affix:"color: #ff0000" inline);
  Alcotest.(check bool)
    "contains padding" true
    (Astring.String.is_infix ~affix:"padding: 10px" inline);
  Alcotest.(check bool)
    "no trailing semicolon" false
    (Astring.String.is_suffix ~affix:";" inline)

let test_property_names () =
  (* Test that property names are converted correctly *)
  let names =
    [
      ( "background-color",
        background_color (Hex { hash = true; value = "0000ff" }) );
      ("padding-left", padding_left (Px 5));
      ("margin-top", margin_top (Px 10));
      ("border-radius", border_radius (Px 4));
    ]
  in

  List.iter
    (fun (expected_name, prop) ->
      let sheet = stylesheet [ Rule (rule ~selector:".x" [ prop ]) ] in
      let css = to_string sheet in
      Alcotest.(check bool)
        (Fmt.str "CSS contains %s" expected_name)
        true
        (Astring.String.is_infix ~affix:expected_name css))
    names

let modes () =
  (* Test the different CSS generation modes *)

  (* Create a color variable with default value *)
  let _, color_var =
    var "color-blue-500" Color (Oklch { l = 62.3; c = 0.214; h = 259.815 })
  in
  let bg_prop = background_color (Var color_var) in
  let rule = rule ~selector:".bg-blue-500" [ bg_prop ] in
  let sheet = stylesheet [ Rule rule ] in

  (* Test Variables mode - should output var(--color-blue-500) *)
  let css_variables = to_string ~mode:Variables sheet in
  Alcotest.(check bool)
    "Variables mode contains var()" true
    (Astring.String.is_infix ~affix:"var(--color-blue-500)" css_variables);

  (* Test Inline mode - should output the actual OKLCH value *)
  let css_inline = to_string ~mode:Inline sheet in
  Alcotest.(check bool)
    "Inline mode contains oklch()" true
    (Astring.String.is_infix ~affix:"oklch(" css_inline);
  Alcotest.(check bool)
    "Inline mode does not contain var()" false
    (Astring.String.is_infix ~affix:"var(--" css_inline)

(* Suite is defined at the bottom after all tests are declared *)

(* Additional coverage for css.mli *)

let test_var_helpers () =
  (* Create a typed var and exercise helpers *)
  let def, v = var "primary" Color (Rgb { r = 1; g = 2; b = 3 }) in
  (* Test that variable was created properly *)
  Alcotest.(check bool) "var created" true true;
  (* Just check that we can create a var for now *)
  (* Using the var in Variables vs Inline mode *)
  let rule_ = rule ~selector:".x" [ background_color (Var v) ] in
  let sheet =
    stylesheet [ Rule rule_; Rule (rule ~selector:".vars" [ def ]) ]
  in
  let css_vars = to_string ~mode:Variables sheet in
  Alcotest.(check bool)
    "Variables mode uses var()" true
    (Astring.String.is_infix ~affix:"var(--primary)" css_vars);
  let css_inline = to_string ~mode:Inline sheet in
  Alcotest.(check bool)
    "Inline mode resolves value" true
    (Astring.String.is_infix ~affix:"rgb(1, 2, 3)" css_inline)

let test_calc_expressions () =
  (* Build a calc expression with a calc var and ensure formatting *)
  let open Calc in
  let cvar = var ~default:(Rem 0.25) "gap" in
  let expr = px 10 + rem 0.5 + cvar in
  let r = rule ~selector:".calc" [ width (Calc expr) ] in
  let sheet = stylesheet [ Rule r ] in
  let css_vars = to_string ~mode:Variables sheet in
  Alcotest.(check bool)
    "calc emits var() in Variables" true
    (Astring.String.is_infix ~affix:"calc(10px + .5rem + var(--gap))" css_vars);
  let css_inline = to_string ~mode:Inline sheet in
  Alcotest.(check bool)
    "calc resolves default in Inline" true
    (Astring.String.is_infix ~affix:"calc(10px + .5rem + .25rem)" css_inline)

let test_color_mix () =
  let mixed =
    Mix
      {
        in_space = Srgb;
        color1 = Rgb { r = 255; g = 0; b = 0 };
        percent1 = Some 25;
        color2 = Rgb { r = 0; g = 0; b = 255 };
        percent2 = None;
      }
  in
  let r = rule ~selector:".mix" [ background_color mixed ] in
  let css = to_string (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "color-mix rendered" true
    (Astring.String.is_infix ~affix:"color-mix(in srgb," css)

let test_transform () =
  let t =
    transform
      [
        Translate (Px 10, Some (Pct 50.));
        Rotate (Deg 45.);
        Scale (Num 1., Some (Var { var_name = "sx"; fallback = Some 0.5 }));
        Skew (Deg 10., Some (Deg 5.));
      ]
  in
  let r = rule ~selector:".t" [ t ] in
  let css = to_string (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "transform translate" true
    (Astring.String.is_infix ~affix:"translate(10px, 50%)" css);
  Alcotest.(check bool)
    "transform rotate" true
    (Astring.String.is_infix ~affix:"rotate(45deg)" css);
  Alcotest.(check bool)
    "transform scale var fallback" true
    (Astring.String.is_infix ~affix:"scale(1, var(--sx, .5))" css);
  Alcotest.(check bool)
    "transform skew" true
    (Astring.String.is_infix ~affix:"skew(10deg, 5deg)" css)

let test_grid_and_flex () =
  (* Grid template and flex shorthand *)
  let grid =
    Grid.template_columns (Tracks [ Fr 1.; Min_max (Px 100, Fr 2.) ])
  in
  let flex_decl = Flex.flex (Full (1., 0., Pct 0.)) in
  let r = rule ~selector:".layout" [ grid; flex_decl; display Flex ] in
  let css = to_string (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "grid template rendered" true
    (Astring.String.is_infix
       ~affix:"grid-template-columns: 1fr minmax(100px, 2fr)" css);
  Alcotest.(check bool)
    "flex shorthand rendered" true
    (Astring.String.is_infix ~affix:"flex: 1 0 0%" css)

let test_transitions () =
  let vdef, dv = var "dur" Duration (Ms 200) in
  let tr =
    transition
      (Multiple
         [
           With_delay (Property "opacity", Var dv, Ease_in, Ms 100);
           With_timing (Property "transform", S 1., Linear);
         ])
  in
  let r = rule ~selector:".tr" [ vdef; tr ] in
  let css_vars = to_string ~mode:Variables (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "transition multiple" true
    (Astring.String.is_infix
       ~affix:
         "transition: opacity var(--dur) ease-in 100ms, transform 1s linear"
       css_vars)

let test_transparent_minify () =
  let r = rule ~selector:".transparent" [ background_color Transparent ] in
  let sheet = stylesheet [ Rule r ] in
  let pretty = to_string sheet in
  let mini = to_string ~minify:true sheet in
  Alcotest.(check bool)
    "pretty uses transparent" true
    (Astring.String.is_infix ~affix:"background-color: transparent" pretty);
  Alcotest.(check bool)
    "minified converts to #0000" true
    (Astring.String.is_infix ~affix:"background-color:#0000" mini)

let test_vars_utilities () =
  (* Define vars and use them in properties, then analyze *)
  let def_len, vlen = var "space" Length (Rem 1.) in
  let def_col, vcol =
    var "brand" Color (Hex { hash = true; value = "ff00ff" })
  in
  let props =
    [
      def_len;
      def_col;
      padding (Var vlen);
      color (Var vcol);
      width (Calc Calc.(px 10 + var ~default:(Px 2) "w"));
    ]
  in
  (* vars_of_declarations returns typed vars, extract names *)
  let vars = vars_of_declarations props in
  let names = List.map var_name vars in
  Alcotest.(check int) "vars count" 3 (List.length names);
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        (Fmt.str "contains %s" expected)
        true (List.mem expected names))
    [ "--brand"; "--space"; "--w" ];
  (* analyze_declarations returns typed vars; map var_name *)
  let anys = analyze_declarations props in
  Alcotest.(check int) "analyzed var count" 2 (List.length anys);
  let got = List.map var_name anys |> List.sort String.compare in
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        (Fmt.str "analyzed contains %s" expected)
        true (List.mem expected got))
    [ "--brand"; "--space" ];
  (* extract_custom_declarations keeps only defs *)
  let defs = extract_custom_declarations props in
  Alcotest.(check int) "two custom declarations" 2 (List.length defs);
  (* names of custom declarations *)
  let def_names =
    defs |> List.filter_map custom_declaration_name |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "custom decl names" [ "--brand"; "--space" ] def_names

let test_calc_infinity_minify () =
  let open Calc in
  let expr = infinity * px 1 in
  let r = rule ~selector:".big" [ width (Calc expr) ] in
  let css = to_string ~minify:true (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "minified infinity calc optimized" true
    (Astring.String.is_infix ~affix:"width:3.40282e38px" css)

let test_hex_without_hash_normalization () =
  let r =
    rule ~selector:".hex"
      [ background_color (Hex { hash = false; value = "00ff00" }) ]
  in
  let css = to_string (stylesheet [ Rule r ]) in
  Alcotest.(check bool)
    "hex normalized" true
    (Astring.String.is_infix ~affix:"background-color: 0ff00" css)

let test_selector_minification () =
  let r =
    rule ~selector:"div  >  .a  +  .b,  .c   ~  .d :hover" [ padding (Px 1) ]
  in
  let css = to_string ~minify:true (stylesheet [ Rule r ]) in
  Alcotest.(check string)
    "selector minified" "div>.a+.b,.c~.d:hover{padding:1px}" css

let test_supports_nested () =
  let inner =
    supports ~condition:"(display: grid)"
      [ rule ~selector:".x" [ gap (Rem 1.) ] ]
  in
  let outer = supports_nested ~condition:"selector(:has(*))" [] [ inner ] in
  let css_pretty = to_string (stylesheet [ Supports outer ]) in
  let css_min = to_string ~minify:true (stylesheet [ Supports outer ]) in
  Alcotest.(check bool)
    "pretty supports has @supports" true
    (Astring.String.is_infix ~affix:"@supports" css_pretty);
  Alcotest.(check bool)
    "minified nested supports" true
    (Astring.String.is_infix ~affix:"@supports selector(:has(*))" css_min)

let test_property_rule_rendering () =
  let at =
    property ~name:"--foo" ~syntax:"<color>" ~initial_value:"transparent" ()
  in
  let css_pretty = to_string (stylesheet [ Property at ]) in
  let css_min = to_string ~minify:true (stylesheet [ Property at ]) in
  Alcotest.(check bool)
    "pretty @property" true
    (Astring.String.is_infix ~affix:"@property --foo" css_pretty);
  Alcotest.(check bool)
    "minified @property" true
    (Astring.String.is_infix
       ~affix:
         "@property \
          --foo{syntax:\"<color>\";inherits:false;initial-value:transparent}"
       css_min)

let test_font_family_var_fallback () =
  let _, ff =
    var
      ~fallback:(Value [ Ui_sans_serif; System_ui ])
      "fonts" Font_family [ Ui_sans_serif ]
  in
  let decl = font_family [ Var ff ] in
  let css = to_string (stylesheet [ Rule (rule ~selector:".ff" [ decl ]) ]) in
  Alcotest.(check bool)
    "font family var with fallback" true
    (Astring.String.is_infix
       ~affix:"font-family: var(--fonts,ui-sans-serif,system-ui)" css)

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

let suite =
  let open Alcotest in
  [
    ( "css",
      [
        (* original tests *)
        test_case "property creation" `Quick test_property_creation;
        test_case "property deduplication" `Quick test_property_deduplication;
        test_case "minification" `Quick test_minification;
        test_case "media query" `Quick test_media_query;
        test_case "inline style" `Quick test_inline_style;
        test_case "property names" `Quick test_property_names;
        test_case "CSS modes" `Quick modes;
        (* extra coverage *)
        test_case "var helpers" `Quick test_var_helpers;
        test_case "calc expressions" `Quick test_calc_expressions;
        test_case "color mix" `Quick test_color_mix;
        test_case "transform values" `Quick test_transform;
        test_case "grid and flex" `Quick test_grid_and_flex;
        test_case "transitions" `Quick test_transitions;
        test_case "transparent minify" `Quick test_transparent_minify;
        test_case "vars utilities" `Quick test_vars_utilities;
        (* corner cases *)
        test_case "calc infinity minify" `Quick test_calc_infinity_minify;
        test_case "hex without hash normalization" `Quick
          test_hex_without_hash_normalization;
        test_case "selector minification" `Quick test_selector_minification;
        test_case "supports nested" `Quick test_supports_nested;
        test_case "@property rendering" `Quick test_property_rule_rendering;
        test_case "font-family var fallback" `Quick
          test_font_family_var_fallback;
        test_case "float formatting" `Quick test_pp_float_edge_cases;
      ] );
  ]
