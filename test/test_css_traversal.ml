open Cascade
open Css
open Css_helpers

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
    ( "@supports",
      Stylesheet.Supports
        (Css_test_helpers.ok (Supports.of_string "(top: 0)"), body) );
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
            ~condition:
              (Css_test_helpers.ok
                 (Css.Container.of_string "(inline-size > 30em)"))
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
            ~condition:
              (Css_test_helpers.ok
                 (Css.Container.of_string "(inline-size > 30em)"))
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
                        (Css_test_helpers.ok
                           (Css.Container.of_string "(inline-size > 30em)"))
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
        Stylesheet.Supports
          (Css_test_helpers.ok (Supports.of_string "(top: 0)"), [ styled ]) );
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
        fun b ->
          Stylesheet.Supports
            (Css_test_helpers.ok (Supports.of_string "(top: 0)"), b) );
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
  let wide = Css_test_helpers.ok (Container.of_string "(width > 10px)") in
  let selectors_of rules =
    List.map (fun (r : Stylesheet.rule) -> Selector.to_string r.selector) rules
  in
  (* The at-rule sits under a group. *)
  let grouped wrap =
    v
      [
        Stylesheet.Supports
          (Css_test_helpers.ok (Supports.of_string "(top: 0)"), [ wrap ]);
      ]
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
  let decl = Css_test_helpers.ok (Declaration.of_string "color:var(--x)") in
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
        Stylesheet.Supports
          (Css_test_helpers.ok (Supports.of_string "(top: 0)"), [ styled ]) );
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
  let y = Css_test_helpers.ok (Declaration.of_string "color:var(--y)") in
  Alcotest.(check (list string))
    "deduplicated in source order" [ "--x"; "--y" ]
    (names
       [
         rule ~selector:(Selector.class_ "a") [ decl ];
         Stylesheet.Media (Media.of_string "screen", [ styled ]);
         rule ~selector:(Selector.class_ "b") [ y ];
       ])

let suite =
  ( "css_traversal",
    [
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
    ] )
