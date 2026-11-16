(** Stylesheet tests: scope boundaries, at-rule descriptor matrices and spec
    snapshots. *)

open Cascade
open Css.Stylesheet
open Css_test_helpers
open Stylesheet_helpers

let dom_selector_boundary () =
  (* Selector matching is DOM context, but selector syntax is CSS-file
     surface. *)
  let selector_cases =
    [
      (".card", ".card");
      ("article.card > h2:first-child", "article.card>h2:first-child");
      (":scope > .item", ":scope>.item");
      (".card:has(> img[alt])", ".card:has(>img[alt])");
      ("a:visited", "a:visited");
      ("::part(label)", "::part(label)");
      (":host(.active) .title", ":host(.active) .title");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        ("minify selector " ^ input)
        expected
        (Css.Selector.to_string ~minify:true (Css.Selector.v input)))
    selector_cases;
  neg_cursor read ".card:has(> ) { color: red }";
  neg_cursor read "::before::after { color: red }";
  neg_cursor read ":host-context() { color: red }"

let fetch_url_boundary () =
  (* @import and url(...) syntax is in scope; loading/resolution is not. *)
  let import_cases =
    [
      ( "@import url(base.css) layer(reset) supports(display: grid) screen;",
        "base.css",
        Some [ "reset" ] );
      ("@import \"print.css\" print;", "print.css", None);
      ("@import url(theme.css) layer();", "theme.css", Some []);
      ( "@import url(theme.css) layer(theme) supports(selector(:has(img))) \
         screen and (width >= 40em);",
        "theme.css",
        Some [ "theme" ] );
      ( "@import url(\"../fonts/brand.woff2\") layer(fonts);",
        "../fonts/brand.woff2",
        Some [ "fonts" ] );
    ]
  in
  List.iter
    (fun (input, url, layer) ->
      let r = Cursor.of_string input in
      let rule = Css.Stylesheet.read_import_rule r in
      Alcotest.(check string) "import url" url rule.url;
      Alcotest.(check (option (list string))) "import layer" layer rule.layer)
    import_cases;
  check_declaration ~expected:"background-image:url(../img/logo.svg)"
    "background-image: url(../img/logo.svg);";
  check_declaration ~expected:"cursor:url(cursor.cur),auto"
    "cursor: url(cursor.cur), auto";
  (* A [<url-token>] ends at its [)], so [format(] needs no separator after it
     and minify drops the space, as it does after [@import] just below. *)
  check_declaration ~expected:"src:url(brand.woff2)format(woff2)"
    "src: url(brand.woff2) format(woff2)";
  check_import_rule ~expected:"@import\"theme.css\"supports(display:);"
    "@import url(theme.css) supports(display:);";
  neg_cursor read_import_rule "@import url(theme.css) layer(theme) layer(base);";
  neg_cursor read_import_rule
    "@import url(theme.css) screen supports(display: grid);"

let environment_query_boundary () =
  (* Query syntax is in scope; matching needs explicit environment context. *)
  check_stylesheet
    ~expected:
      "@media(width>=40em){@supports(display:grid){@container card \
       style(--theme:dark){.card{display:grid}}}}"
    "@media (width >= 40em) { @supports (display: grid) { @container card \
     style(--theme: dark) { .card { display: grid } } } }";
  check_stylesheet ~expected:"@supports(display:){.x{color:red}}"
    "@supports (display:) { .x { color: red } }";
  check_stylesheet ~expected:"@media(width >= ){.x{color:red}}"
    "@media (width >= ) { .x { color: red } }";
  neg_cursor read "@container card style() { .x { color: red } }";
  check_stylesheet ~expected:"@container card (width >){.x{color:red}}"
    "@container card (width >) { .x { color: red } }"

let value_resolution_boundary () =
  let open Css.Values in
  let inherited_font_size = ok (Css.Declaration.of_string "font-size: 16px") in
  let inherited_color = ok (Css.Declaration.of_string "color: canvastext") in
  let initial_width = ok (Css.Declaration.of_string "width: auto") in
  let initial_display = ok (Css.Declaration.of_string "display: inline") in
  let ctx =
    {
      Css.Context.empty with
      inherited_values = [ inherited_font_size; inherited_color ];
      initial_values = [ initial_width; initial_display ];
      base_url = Some "https://example.test/css/app.css";
      root_font_size = Some (Px 16.);
      parent_font_size = Some (Px 16.);
      current_color = Some (System Canvas_text);
      viewport_width = Some (Px 1024.);
      viewport_height = Some (Px 768.);
      container_width = Some (Px 640.);
      container_height = Some (Px 480.);
    }
  in
  Alcotest.(check (option decl_t))
    "inherited font size" (Some inherited_font_size)
    (Css.Context.inherited_value "font-size" ctx);
  Alcotest.(check (option decl_t))
    "initial width" (Some initial_width)
    (Css.Context.initial_value "width" ctx);
  check_specified "inherit fallback before computed stage" "16px"
    "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "16px") ~cascaded:(Some "inherit"));
  check_specified "unset chooses inherited before computed stage" "canvastext"
    "unset-inherited"
    (Css.Stylesheet.value ~inherits:true ~initial:"black"
       ~inherited:(Some "canvastext") ~cascaded:(Some "unset"))

let custom_property_boundary () =
  let check_specified_value name css expected =
    let decl = ok (Css.Declaration.of_string css) in
    Alcotest.(check string)
      name expected
      (Css.Declaration.string_of_value ~minify:false decl)
  in
  let check_minified_value name css expected =
    let decl = ok (Css.Declaration.of_string css) in
    Alcotest.(check string)
      name expected
      (Css.Declaration.string_of_value ~minify:true decl)
  in
  let gap = ok (Css.Declaration.of_string "--gap: 1rem") in
  let ctx =
    {
      Css.Context.empty with
      custom_properties =
        [
          gap;
          ok (Css.Declaration.of_string "--a: var(--b)");
          ok (Css.Declaration.of_string "--b: var(--a, red)");
        ];
    }
  in
  Alcotest.(check (option decl_t))
    "custom property context" (Some gap)
    (Css.Context.custom_property "--gap" ctx);
  check_declaration ~expected:"--a:var(--b)" "--a: var(--b);";
  (* CSS Custom Properties requires specified custom-property values to
     serialize as authored. Assert the value serialization directly so this spec
     boundary does not depend on declaration-colon formatting. *)
  check_specified_value "custom property var fallback" "--b: var(--a, red);"
    "var(--a, red)";
  check_specified_value "var fallback in ordinary declaration"
    "color: var(--brand, red);" "var(--brand, red)";
  (* Minification may remove whitespace around the fallback comma while
     preserving the var() grammar. Keep this explicit and separate from the
     specified-value serialization checks above. *)
  check_minified_value "custom property var fallback minified"
    "--b: var(--a, red);" "var(--a,red)";
  check_minified_value "nested var fallback minified"
    "--nested: var(--a, var(--b, red));" "var(--a,var(--b,red))";
  neg_cursor Css.Declaration.read_declaration "--: var(--x);";
  check_declaration ~expected:"--tokens:{color:red}" "--tokens: { color: red };";
  check_declaration ~expected:"--empty:var(--missing,)"
    "--empty: var(--missing,);";
  check_specified_value "nested var fallback"
    "--nested: var(--a, var(--b, red));" "var(--a, var(--b, red))";
  neg_cursor read
    "@property --registered { syntax: \"<color>\"; inherits: false; \
     initial-value: 10px }"

let spec_current_at_rules () =
  check_stylesheet ~expected:"@media(dynamic-range:high){.photo{color:red}}"
    "@media (dynamic-range: high) { .photo { color: red } }";
  check_stylesheet
    ~expected:"@media(prefers-reduced-data:reduce){.hero{display:none}}"
    "@media (prefers-reduced-data: reduce) { .hero { display: none } }";
  check_stylesheet
    ~expected:"@supports selector(:has(img)){.card{display:block}}"
    "@supports selector(:has(img)) { .card { display: block } }";
  check_stylesheet
    ~expected:".card{color:red;@media(width>=40em){&>img{display:block}}}"
    ".card { color: red; @media (width >= 40em) { & > img { display: block } } \
     }";
  check_stylesheet ~expected:"@scope(.card)to (.footer){.title{color:red}}"
    "@scope (.card) to (.footer) { .title { color: red } }";
  (* [@scope] and [to] are both ident-shaped, so with nothing between them they
     lex as the one at-keyword [@scopeto]. The [)] closing a start selector
     already delimits them. *)
  check_stylesheet ~expected:"@scope to (.footer){.title{color:red}}"
    "@scope to (.footer) { .title { color: red } }";
  check_stylesheet ~expected:"@scope(.card){.title{color:red}}"
    "@scope (.card) { .title { color: red } }";
  (* The scope-end selector list is held in authored order by pp; only optimize
     sorts it into canonical order (and folds the color). *)
  assert_minify_and_optimize
    "@scope (:root) to (.stop, .end) { .title { color: blue } }"
    ~minified:"@scope(:root)to (.stop,.end){.title{color:blue}}"
    ~optimized:"@scope(:root)to (.end,.stop){.title{color:#00f}}";
  check_stylesheet
    ~expected:
      "@font-palette-values \
       --brand{font-family:Brand;base-palette:1;override-colors:0 red}"
    "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
     override-colors: 0 red; }";
  (* CSS Fonts 4 (ED) sec. 4 lists the @font-face descriptors and [font-tech] is
     not one: [<font-tech>] is the keyword sec. 11.1 defines for [tech()] inside
     [src] and for [font-tech()] in @supports. Chrome 153 drops the declaration,
     and BCD carries a css.at-rules.font-face key for every real descriptor and
     none for this one. *)
  check_stylesheet
    ~expected:
      "@font-face{font-family:ColorFont;src:url(color.woff2)tech(color-COLRv1)}"
    "@font-face { font-family: ColorFont; src: url(color.woff2) \
     tech(color-COLRv1); font-tech: color-COLRv1; }";
  check_stylesheet ~expected:"@view-transition{navigation:auto}"
    "@view-transition { navigation: auto; }";
  check_stylesheet
    ~expected:"@position-try --below{top:anchor(bottom);left:anchor(center)}"
    "@position-try --below { top: anchor(bottom); left: anchor(center); }";
  check_stylesheet
    ~expected:
      "@container card \
       style(--variant:featured){.card{view-transition-name:card}}"
    "@container card style(--variant: featured) { .card { \
     view-transition-name: card } }";
  check_stylesheet
    ~expected:"@container style(--variant:featured){.card{color:red}}"
    "@container style(--variant: featured) { .card { color: red } }";
  check_stylesheet
    ~expected:"@container scroll-state(stuck:top){.card{color:red}}"
    "@container scroll-state(stuck: top) { .card { color: red } }";
  neg_cursor read "@container style() { .card { color: red } }";
  neg_cursor read "@container scroll-state() { .card { color: red } }";
  check_stylesheet
    ~expected:
      "@container(30em<=inline-size<60em){@supports(display:grid){.grid{display:grid}}}"
    "@container (30em <= inline-size < 60em) { @supports (display: grid) { \
     .grid { display: grid } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog{opacity:0;translate:0 1rem}}"
    "@starting-style { .dialog { opacity: 0; translate: 0 1rem } }";
  check_stylesheet ~expected:"@page chapter:left{margin:2cm}"
    "@page chapter:left { margin: 2cm }";
  check_stylesheet ~expected:"@media(width >){.x{color:red}}"
    "@media (width >) { .x { color: red } }";
  (* Mediaqueries 5 sec. 3.1 sends a condition no grammar claims to
     <general-enclosed>, which a parser keeps and never matches. Chrome keeps
     both of these in cssText for the same reason. *)
  check_stylesheet ~expected:"@media(width >= ){.x{color:red}}"
    "@media (width >= ) { .x { color: red } }";
  check_stylesheet ~expected:"@supports selector(){.x{color:red}}"
    "@supports selector() { .x { color: red } }";
  neg_cursor read "@scope (.card) .title { color: red }";
  neg_cursor read "@font-palette-values { base-palette: 1; }";
  neg_cursor read "@position-try default { top: 0; }";
  check_stylesheet ~expected:"@container(){.x{color:red}}"
    "@container () { .x { color: red } }";
  neg_cursor read "@page : { margin: 1cm }"

(* CSS Syntax 3 (ED) sec. 4.3.1 consumes an ident sequence into the at-keyword,
   so the space in front of an ident prelude is the only thing naming the rule:
   [@scope to (...)] printed as [@scopeto (...)] is a different at-rule, and one
   every browser drops. A [(] and a [)] each end their token, so a prelude that
   opens or closes on one needs no separator; sec. 4.3.4 turns [to(] into a
   function token, so [to] keeps the space after it either way. *)
let at_keyword_prelude_separator () =
  minify_reads_back "scope end only"
    ~expected:"@scope to (.content>*){a{color:red}}"
    "@scope to (.content > *) { a { color: red } }";
  minify_reads_back "scope start and end"
    ~expected:"@scope(.card)to (.footer){a{color:red}}"
    "@scope (.card) to (.footer) { a { color: red } }";
  minify_reads_back "scope start only" ~expected:"@scope(.card){a{color:red}}"
    "@scope (.card) { a { color: red } }";
  minify_reads_back "scope with neither" ~expected:"@scope{a{color:red}}"
    "@scope { a { color: red } }";
  minify_reads_back "media type" ~expected:"@media screen{a{color:red}}"
    "@media screen { a { color: red } }";
  minify_reads_back "media not" ~expected:"@media not all{a{color:red}}"
    "@media not all { a { color: red } }";
  minify_reads_back "supports not"
    ~expected:"@supports not (display:grid){a{color:red}}"
    "@supports not (display: grid) { a { color: red } }";
  minify_reads_back "supports selector"
    ~expected:"@supports selector(a){a{color:red}}"
    "@supports selector(a) { a { color: red } }";
  minify_reads_back "container name"
    ~expected:"@container card (width>0px){a{color:red}}"
    "@container card (width > 0px) { a { color: red } }";
  minify_reads_back "container not"
    ~expected:"@container not (width>0px){a{color:red}}"
    "@container not (width > 0px) { a { color: red } }";
  minify_reads_back "page name" ~expected:"@page toc{margin:1cm}"
    "@page toc { margin: 1cm }";
  minify_reads_back "layer name" ~expected:"@layer a{b{color:red}}"
    "@layer a { b { color: red } }";
  (* An unknown at-rule keeps its block byte for byte, so this source is written
     minified: only the prelude separator is under test. *)
  minify_reads_back "unknown at-rule" ~expected:"@foo bar{a{color:red}}"
    "@foo bar{a{color:red}}"

let font_palette_values_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@font-palette-values \
         --brand{font-family:Brand;base-palette:1;override-colors:0 red,1 \
         color(display-p3 1 0 0)}",
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         override-colors: 0 red, 1 color(display-p3 1 0 0); }" );
      ( "@font-palette-values --dark{font-family:Color \
         Font,Brand;base-palette:dark}",
        "@font-palette-values --dark { font-family: \"Color Font\", Brand; \
         base-palette: dark; }" );
    ];
  List.iter (neg_cursor read)
    [
      "@font-palette-values brand { font-family: Brand; base-palette: 1 }";
      "@font-palette-values --brand;";
      "@font-palette-values --brand { font-family: Brand; override-colors: -1 \
       red }";
    ]

let spec_view_transition_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@view-transition{navigation:auto}",
        "@view-transition { navigation: auto; }" );
      ( "@view-transition{navigation:none}",
        "@view-transition { navigation: none; }" );
    ];
  List.iter (neg_cursor read)
    [
      "@view-transition page { navigation: auto; }";
      "@view-transition;";
      "@view-transition { navigation: always; }";
    ]

let spec_position_try_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@position-try \
         --below{top:anchor(bottom);left:anchor(center);width:anchor-size(width)}",
        "@position-try --below { top: anchor(bottom); left: anchor(center); \
         width: anchor-size(width); }" );
      ( "@position-try \
         --inline-start{inset-inline-end:anchor(start);margin-inline:1rem}",
        "@position-try --inline-start { inset-inline-end: anchor(start); \
         margin-inline: 1rem; }" );
    ];
  List.iter (neg_cursor read)
    [
      "@position-try default { top: 0; }";
      "@position-try --fallback;";
      "@position-try --fallback { @media screen { .x { color: red } } }";
    ]

let spec_at_rule_descriptor_matrix () =
  List.iter
    (fun (expected, input) -> check_stylesheet ~expected input)
    [
      ( "@property --accent{syntax:\"<color>\";inherits:true;initial-value:red}",
        "@property --accent { initial-value: red; inherits: true; syntax: \
         \"<color>\" }" );
      ( "@property --dup{syntax:\"*\";inherits:false}",
        "@property --dup { syntax: \"<length>\"; inherits: true; syntax: \
         \"*\"; inherits: false }" );
      ( "@font-face{font-weight:100 \
         900;font-display:swap;src:url(brand.woff2);font-family:Brand}",
        "@font-face { font-weight: 100 900; font-display: swap; src: \
         url(brand.woff2); font-family: Brand }" );
      ( "@page invoice:first{margin:1cm;size:A4;@top-left{content:\"Invoice\"}}",
        "@page invoice:first { margin: 1cm; size: A4; @top-left { content: \
         \"Invoice\" } }" );
      ( "@font-palette-values \
         --brand{font-family:Brand;base-palette:2;override-colors:0 red}",
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         base-palette: 2; override-colors: 0 red }" );
      ( "@view-transition{navigation:none}",
        "@view-transition { navigation: auto; navigation: none }" );
      ( "@position-try --below{top:anchor(bottom);left:anchor(center)}",
        "@position-try --below { left: anchor(center); top: anchor(bottom) }" );
      ( "@media screen and (width>=40em){.card{display:grid}}",
        "@media screen and (width >= 40em) { .card { display: grid } }" );
      ( "@supports((display:grid)and selector(:has(img))){.card{display:grid}}",
        "@supports ((display: grid) and selector(:has(img))) { .card { \
         display: grid } }" );
      ( "@container card style(--variant:featured){.card{color:red}}",
        "@container card style(--variant: featured) { .card { color: red } }" );
      ( "@scope(.card)to (.boundary){.title{color:red}}",
        "@scope (.card) to (.boundary) { .title { color: red } }" );
      ( "@starting-style{.dialog{opacity:0}}",
        "@starting-style { .dialog { opacity: 0 } }" );
    ];
  List.iter (neg_cursor read)
    [
      "@property --bad { syntax: \"<length>\"; inherits: false }";
      "@property --bad { syntax: \"<length>\"; inherits: false; initial-value: \
       red }";
      "@font-face { font-family: Brand; src: url(brand.woff2); @media screen { \
       .x { color: red } } }";
      "@font-palette-values --brand { font-family: Brand; @media screen { .x { \
       color: red } } }";
      "@view-transition { @media screen { .x { color: red } } }";
      "@position-try --below { @supports (display: grid) { .x { color: red } } \
       }";
      "@page { @top-center { @media screen { .x { color: red } } } }";
      "@keyframes fade { @media screen { opacity: 1 } }";
      "@media screen;";
      "@media screen and or (width) { .x { color: red } }";
      "@supports (display: grid) and (gap: 1rem) or (color: red) { .x { color: \
       red } }";
      "@container card style() { .x { color: red } }";
      "@scope () { .x { color: red } }";
      "@scope (.x) to () { .x { color: red } }";
      "@starting-style;";
    ]

let spec_at_rule_inventory_matrix () =
  let module A = Cascade_spec_inventory.At_rule_grammar in
  List.iter
    (fun (row : A.row) -> check_stylesheet ~expected:row.expected row.input)
    A.positive;
  List.iter (fun (row : A.invalid_row) -> neg_cursor read row.input) A.negative;
  let positive_features = A.features A.positive in
  let negative_features =
    A.negative
    |> List.map (fun (row : A.invalid_row) -> row.feature)
    |> List.sort_uniq String.compare
  in
  Alcotest.(check (list string))
    "at-rule inventory positive/negative feature parity" positive_features
    negative_features;
  List.iter
    (fun feature ->
      let positive_branches =
        List.filter (fun (row : A.row) -> row.feature = feature) A.positive
      in
      let negative_branches =
        List.filter
          (fun (row : A.invalid_row) -> row.feature = feature)
          A.negative
      in
      if positive_branches = [] then
        Alcotest.failf "%s has no positive at-rule inventory rows" feature;
      if negative_branches = [] then
        Alcotest.failf "%s has no negative at-rule inventory rows" feature)
    positive_features

(* ignore-test *)
let test_spec_snapshot_tracking_vectors () =
  (* Snapshot tracking vectors span stable cross-module syntax from recent CSS
     snapshots. The matrix tracks exact snapshot membership; these tests make
     historical snapshot coverage visible in the normal stylesheet suite. *)
  check_stylesheet
    ~expected:
      "@layer reset,base,components;@layer \
       components{.card{display:grid;gap:1rem}}"
    "@layer reset, base, components; @layer components { .card { display: \
     grid; gap: 1rem } }";
  check_stylesheet
    ~expected:
      "@container card (inline-size>30em){.card{grid-template-columns:subgrid}}"
    "@container card (inline-size > 30em) { .card { grid-template-columns: \
     subgrid } }";
  let oklch_support =
    "@supports (color: oklch(50% 0.1 20)) { .accent { color: oklch(50% 0.1 20) \
     } }"
  in
  (* The condition keeps [0.1] as written: it is the declaration the browser is
     asked to parse, not a value to re-spell. Its insignificant whitespace goes,
     as it does everywhere else in a prelude: [%] closes the percentage token,
     so [50%0.1] is the same two tokens. The guarded rule folds. *)
  check_stylesheet
    ~expected:
      "@supports(color:oklch(50%0.1 20)){.accent{color:oklch(50%.1 20)}}"
    oklch_support;
  assert_minify_and_optimize oklch_support
    ~minified:
      "@supports(color:oklch(50%0.1 20)){.accent{color:oklch(50%.1 20)}}"
    ~optimized:"@supports(color:oklch(50%0.1 20)){.accent{color:#944a4b}}";
  let nested_media =
    ".card { color: var(--fg); @media (prefers-color-scheme: dark) { & { \
     color: white } } }"
  in
  check_stylesheet
    ~expected:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:white}}}"
    nested_media;
  assert_minify_and_optimize nested_media
    ~minified:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:white}}}"
    ~optimized:
      ".card{color:var(--fg);@media(prefers-color-scheme:dark){&{color:#fff}}}";
  neg_cursor read "@layer reset,,base;";
  check_stylesheet ~expected:"@container card (){.card{color:red}}"
    "@container card () { .card { color: red } }";
  check_stylesheet ~expected:"@supports(){.accent{color:red}}"
    "@supports () { .accent { color: red } }"

(* ignore-test *)
let test_snapshot_membership_matrix () =
  let module S = Cascade_spec_inventory.Css_snapshot in
  let keys rows = List.map S.key rows |> List.sort String.compare in
  let snapshot_2026 =
    [
      "CSS Animations@1/2";
      "CSS Backgrounds and Borders@3";
      "CSS Box Alignment@3";
      "CSS Box Model@3";
      "CSS Cascading and Inheritance@5";
      "CSS Color@3";
      "CSS Color@4";
      "CSS Containment@3";
      "CSS Custom Properties@1";
      "CSS Display@3";
      "CSS Flexible Box Layout@1";
      "CSS Fonts@4";
      "CSS Grid Layout@1/2";
      "CSS Logical Properties@1";
      "CSS Overflow@3/4";
      "CSS Positioned Layout@3";
      "CSS Pseudo-Elements@4";
      "CSS Scroll Snap@1";
      "CSS Sizing@3";
      "CSS Syntax@3";
      "CSS Transforms@1/2";
      "CSS Transitions@1/2";
      "Conditional Rules@3";
      "Media Queries@4";
      "Selectors@4";
      "Values and Units@3";
    ]
  in
  let current_work =
    [
      "CSS Cascading and Inheritance@6";
      "CSS Fonts@5";
      "CSS Nesting@1";
      "CSS Properties and Values API@1";
      "Conditional Rules@4";
      "Media Queries@5";
      "Values and Units@4";
    ]
  in
  let experimental = [ "CSS Color@5"; "Values and Units@5" ] in
  Alcotest.(check (list string))
    "CSS Snapshot 2026 module membership" snapshot_2026
    (keys (S.by_baseline S.Snapshot_2026));
  Alcotest.(check (list string))
    "current-work module membership" current_work
    (keys (S.by_baseline S.Current_work));
  Alcotest.(check (list string))
    "experimental module membership" experimental
    (keys (S.by_baseline S.Experimental));
  List.iter
    (fun row ->
      if row.S.tests = [] then
        Alcotest.failf "%s has no deterministic test surface" (S.key row);
      if row.S.fuzzers = [] then
        Alcotest.failf "%s has no fuzz surface" (S.key row))
    S.rows

let suite =
  ( "stylesheet_at_rules",
    [
      ( "spec DOM selector matching boundary vectors",
        `Quick,
        dom_selector_boundary );
      ("spec fetch import and URL boundary vectors", `Quick, fetch_url_boundary);
      ( "spec environment query evaluation boundary vectors",
        `Quick,
        environment_query_boundary );
      ( "spec value resolution boundary vectors",
        `Quick,
        value_resolution_boundary );
      ( "spec custom property computed-time boundary vectors",
        `Quick,
        custom_property_boundary );
      ("spec current-work at-rules", `Quick, spec_current_at_rules);
      ("spec at-keyword prelude separator", `Quick, at_keyword_prelude_separator);
      ( "spec font-palette-values descriptor matrix",
        `Quick,
        font_palette_values_descriptor_matrix );
      ( "spec view-transition descriptor matrix",
        `Quick,
        spec_view_transition_descriptor_matrix );
      ( "spec position-try descriptor matrix",
        `Quick,
        spec_position_try_descriptor_matrix );
      ( "spec at-rule descriptor order duplicate matrix",
        `Quick,
        spec_at_rule_descriptor_matrix );
      ( "spec at-rule shared inventory matrix",
        `Quick,
        spec_at_rule_inventory_matrix );
      ( "spec snapshot tracking vectors",
        `Quick,
        test_spec_snapshot_tracking_vectors );
      ( "spec snapshot membership matrix",
        `Quick,
        test_snapshot_membership_matrix );
    ] )
