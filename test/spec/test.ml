(** CSS specification compliance tests.

    Test vectors are derived from W3C CSS specifications. The specs are the
    oracle: do not update expected results to match current implementation
    output. A failing spec-derived vector should remain visible until the
    implementation is fixed or the feature is explicitly out of scope. *)

open Cascade
open Css

(* {2 Helpers} *)

let roundtrip css expected =
  match of_string css with
  | Ok sheet ->
      let output = to_string ~minify:true ~newline:false sheet in
      Alcotest.(check string) css expected output
  | Error e -> Alcotest.fail (pp_parse_error e)

let roundtrip_identity css = roundtrip css css

let parses_valid css =
  match of_string css with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (pp_parse_error e)

let rejects_invalid css =
  match of_string css with
  | Error _ -> ()
  | Ok sheet ->
      Alcotest.failf "invalid CSS vector parsed: %s -> %s" css
        (to_string ~minify:true ~newline:false sheet)

let recover css expected min_warnings =
  let { stylesheet; warnings } = parse css in
  let output = to_string ~minify:true ~newline:false stylesheet in
  Alcotest.(check string) css expected output;
  Alcotest.(check bool)
    (css ^ " warnings") true
    (List.length warnings >= min_warnings)

(* {2 CSS Syntax Level 3} https://www.w3.org/TR/css-syntax-3/ *)

(* {2 CSS 2.x compatibility surface} *)

let css2_selectors_and_at_rules () =
  roundtrip "body { margin: 0; color: black }" "body{margin:0;color:black}";
  roundtrip "@charset \"UTF-8\";" "@charset \"UTF-8\";";
  roundtrip "@import 'legacy.css';" "@import 'legacy.css';";
  roundtrip "@media print { body { color: black } }"
    "@media print{body{color:black}}";
  roundtrip "@page :left { margin-left: 4cm; margin-right: 3cm }"
    "@page:left{margin-left:4cm;margin-right:3cm}";
  roundtrip "html > body p + p { text-indent: 1em }"
    "html>body p+p{text-indent:1em}";
  roundtrip "a:link { color: blue } a:visited { color: purple }"
    "a:link{color:blue}a:visited{color:purple}";
  roundtrip "li:first-child { list-style-type: none }"
    "li:first-child{list-style-type:none}"

let css2_pseudo_elements_aliases () =
  roundtrip "h1:first-letter { color: red }" "h1:first-letter{color:red}";
  roundtrip "p::first-line { color: blue }" "p:first-line{color:blue}";
  roundtrip "q:before { content: open-quote }" "q:before{content:open-quote}";
  roundtrip "q::after { content: close-quote }" "q:after{content:close-quote}";
  roundtrip "div { page-break-before: always }" "div{break-before:page}";
  roundtrip "div { page-break-after: avoid }" "div{break-after:avoid}";
  roundtrip "div { page-break-inside: avoid }" "div{break-inside:avoid}"

let css2_legacy_invalid_vectors () =
  rejects_invalid "@charset 'UTF-8';";
  rejects_invalid "@page :first:left { margin: 1cm }";
  rejects_invalid "div { page-break-before: always avoid }";
  rejects_invalid "div { page-break-inside: left }";
  rejects_invalid "h1::first-line::before { color: red }"

let css2_chapter_matrix () =
  (* CSS 2.x context-free grammar surface: selectors, paged media, visual
     formatting, generated content, lists/tables, and legacy recovery. *)
  List.iter
    (fun (input, expected) -> roundtrip input expected)
    [
      ( "html, body { display: block; min-height: 100% }",
        "html,body{display:block;min-height:100%}" );
      ( "body *[lang|=\"en\"] + p:first-line { text-transform: uppercase }",
        "body *[lang|=en]+p:first-line{text-transform:uppercase}" );
      ( "table > caption + colgroup col { visibility: collapse }",
        "table>caption+colgroup col{visibility:collapse}" );
      ( "ol li { list-style: decimal inside }",
        "ol li{list-style:decimal inside}" );
      ( "q:before { content: open-quote } q:after { content: close-quote }",
        "q:before{content:open-quote}q:after{content:close-quote}" );
      ( "pre { white-space: pre; tab-size: 4 }",
        "pre{white-space:pre;tab-size:4}" );
      ( "img { float: left; clear: both; vertical-align: middle }",
        "img{float:left;clear:both;vertical-align:middle}" );
      ( "@media print { h1 { page-break-before: always } }",
        "@media print{h1{break-before:page}}" );
      ( "@page chapter:right { margin: 2cm; size: A4 }",
        "@page chapter:right{margin:2cm;size:A4}" );
    ];
  List.iter rejects_invalid
    [
      "a + { color: red }";
      "table > > td { color: red }";
      "@page :left:right { margin: 1cm }";
      "ol { list-style-position: center }";
      "p { vertical-align: left right }";
      "q { content: open-quote none }";
    ]

(* SS 5.3 - Qualified rules: a prelude (selector) + block (declarations) *)
let syntax_qualified_rules () =
  (* Single rule with single declaration *)
  roundtrip "h1 { color: red }" "h1{color:red}";
  (* Multiple selectors in selector list *)
  roundtrip "h1, h2, h3 { margin: 0 }" "h1,h2,h3{margin:0}";
  (* Multiple declarations *)
  roundtrip "p { color: blue; font-size: 16px }" "p{color:blue;font-size:16px}";
  (* Multiple rules in sequence *)
  roundtrip "h1 { color: red } p { margin: 0 }" "h1{color:red}p{margin:0}"

(* SS 5.4 - At-rules: @media and @import *)
let syntax_at_rules () =
  (* @media at-rule *)
  roundtrip "@media screen { .btn { color: green } }"
    "@media screen{.btn{color:green}}";
  (* @import at-rule - trailing semicolon is preserved in output *)
  roundtrip "@import url(\"reset.css\");" "@import url(\"reset.css\");";
  (* @layer block *)
  roundtrip "@layer base { body { margin: 0 } }" "@layer base{body{margin:0}}"

(* SS 4.3.2 - Comments *)
let syntax_comments () =
  (* Comments are stripped during parsing *)
  roundtrip "/* This is a comment */ h1 { color: red }" "h1{color:red}";
  roundtrip "h1 { /* inline comment */ color: red }" "h1{color:red}";
  roundtrip "h1 { color: /* mid-value */ red }" "h1{color:red}"

(* SS 4.3.4 - Whitespace normalization *)
let syntax_whitespace () =
  (* Extra whitespace is normalized *)
  roundtrip "  h1  {  color  :  red  }  " "h1{color:red}";
  (* Tab and newline normalization *)
  roundtrip "h1\t{\n\tcolor:\tred\n}" "h1{color:red}"

(* SS 4.3.7 - Escape sequences in identifiers *)
let syntax_escapes () =
  (* Escaped class names roundtrip correctly *)
  roundtrip_identity ".sm\\:p-4{color:red}";
  roundtrip_identity ".w-1\\/2{width:50%}"

(* SS 5.3.7 / 5.4 - Parse errors recover locally *)
let syntax_recovery () =
  recover ".a { color: invalid; color: red }" ".a{color:red}" 1;
  recover ".a,:future-pseudo { color: red } .b { color: blue }" ".b{color:blue}"
    1;
  recover "@unknown { .a { color: red } } .b { color: blue }" ".b{color:blue}" 1;
  recover ".a { color: red" ".a{color:red}" 0

(* {2 CSS Selectors Level 4} https://www.w3.org/TR/selectors-4/ *)

(* SS 5.1 - Type selectors *)
let selectors_type () =
  roundtrip "h1 { color: red }" "h1{color:red}";
  roundtrip "div { display: block }" "div{display:block}";
  roundtrip "span { color: blue }" "span{color:blue}";
  roundtrip "article { margin: 0 }" "article{margin:0}"

(* SS 5.1 - Universal selector *)
let selectors_universal () =
  roundtrip "* { box-sizing: border-box }" "*{box-sizing:border-box}"

(* SS 6.1 - Class selectors *)
let selectors_class () =
  roundtrip ".warning { color: red }" ".warning{color:red}";
  roundtrip ".info { color: blue }" ".info{color:blue}"

(* SS 6.2 - ID selectors *)
let selectors_id () =
  roundtrip "#myid { color: red }" "#myid{color:red}";
  roundtrip "#main { display: flex }" "#main{display:flex}"

(* SS 7 - Attribute selectors *)
let selectors_attribute () =
  (* [att] - Presence *)
  roundtrip "[href] { color: blue }" "[href]{color:blue}";
  (* [att=val] - Exact match: simple ident values are unquoted in output *)
  roundtrip "[type=\"text\"] { border: 1px solid gray }"
    "[type=text]{border:1px solid gray}";
  (* [att~=val] - Whitespace-separated list *)
  roundtrip "[class~=\"warning\"] { color: red }" "[class~=warning]{color:red}";
  (* [att|=val] - Hyphen-separated list *)
  roundtrip "[lang|=\"en\"] { color: blue }" "[lang|=en]{color:blue}";
  (* [att^=val] - Prefix *)
  roundtrip "[href^=\"https\"] { color: green }" "[href^=https]{color:green}";
  (* [att$=val] - Suffix: non-ident values keep quotes *)
  roundtrip "[href$=\".pdf\"] { color: red }" "[href$=\".pdf\"]{color:red}";
  (* [att*=val] - Substring *)
  roundtrip "[title*=\"hello\"] { color: blue }" "[title*=hello]{color:blue}"

(* SS 8.1 - Pseudo-classes *)
let selectors_pseudo_classes () =
  roundtrip ":hover { color: red }" ":hover{color:red}";
  roundtrip ":first-child { color: red }" ":first-child{color:red}";
  roundtrip ":last-child { margin: 0 }" ":last-child{margin:0}";
  roundtrip ":nth-child(2n+1) { color: red }" ":nth-child(2n+1){color:red}";
  (* CSS Selectors 4 §6.6.1: [odd]/[even] are valid output forms in their own
     right; the printer preserves whatever the author wrote. *)
  roundtrip ":nth-child(even) { color: blue }" ":nth-child(even){color:blue}";
  roundtrip ":nth-child(odd) { color: red }" ":nth-child(odd){color:red}";
  roundtrip ":not(.foo) { color: red }" ":not(.foo){color:red}"

(* SS 8.2 - Pseudo-elements use the shortest valid legacy spelling in minified
   output for the four CSS1/CSS2 compatibility pseudo-elements. *)
let selectors_pseudo_elements () =
  roundtrip "::before { content: '' }" ":before{content:\"\"}";
  roundtrip "::after { content: '' }" ":after{content:\"\"}";
  roundtrip "::first-line { color: red }" ":first-line{color:red}"

(* SS 15 - Combinators *)
let selectors_combinators () =
  (* Descendant combinator (space) *)
  roundtrip "div p { color: red }" "div p{color:red}";
  (* Child combinator (>) *)
  roundtrip "div > p { color: red }" "div>p{color:red}";
  (* Adjacent sibling combinator (+) *)
  roundtrip "h1 + p { color: red }" "h1+p{color:red}";
  (* General sibling combinator (~) *)
  roundtrip "h1 ~ p { color: red }" "h1~p{color:red}"

(* SS 4 - Selector lists *)
let selectors_list () =
  roundtrip "h1, h2, h3 { margin: 0 }" "h1,h2,h3{margin:0}";
  roundtrip ".a, .b, .c { display: block }" ".a,.b,.c{display:block}"

(* SS 8.4.1 - :where() and :is() pseudo-classes *)
let selectors_where_is () =
  roundtrip ":where(.a, .b) { color: red }" ":where(.a,.b){color:red}";
  roundtrip ":is(.a, .b) { color: red }" ":is(.a,.b){color:red}";
  roundtrip ":is() { color: red }" ":is(){color:red}";
  roundtrip ":where() { color: red }" ":where(){color:red}";
  roundtrip ":is(:future-pseudo, .a) { color: red }" ":is(.a){color:red}";
  roundtrip ":where(:future-pseudo, .a) { color: red }" ":where(.a){color:red}"

(* {2 CSS Values and Units Level 4} https://www.w3.org/TR/css-values-4/ *)

(* SS 6.1 - Absolute lengths *)
let values_absolute_lengths () =
  roundtrip ".x { width: 100px }" ".x{width:100px}";
  roundtrip ".x { width: 10cm }" ".x{width:10cm}";
  roundtrip ".x { width: 10mm }" ".x{width:10mm}";
  roundtrip ".x { width: 1in }" ".x{width:1in}";
  roundtrip ".x { width: 12pt }" ".x{width:12pt}";
  roundtrip ".x { width: 1pc }" ".x{width:1pc}"

(* SS 6.2 - Relative lengths *)
let values_relative_lengths () =
  roundtrip ".x { font-size: 2em }" ".x{font-size:2em}";
  roundtrip ".x { font-size: 1.5rem }" ".x{font-size:1.5rem}";
  roundtrip ".x { width: 50vw }" ".x{width:50vw}";
  roundtrip ".x { height: 100vh }" ".x{height:100vh}";
  roundtrip ".x { width: 50% }" ".x{width:50%}"

(* SS 10.1 - calc() expressions *)
let values_calc () =
  roundtrip ".x { width: calc(100% - 2rem) }" ".x{width:calc(100% - 2rem)}";
  roundtrip ".x { width: calc(2 * 3rem) }" ".x{width:calc(2*3rem)}";
  roundtrip ".x { width: calc(100% - calc(2rem + 10px)) }"
    ".x{width:calc(100% - calc(2rem + 10px))}"

(* SS 6.3 - Angle units *)
let values_angles () =
  roundtrip ".x { transform: rotate(45deg) }" ".x{transform:rotate(45deg)}";
  roundtrip ".x { transform: rotate(1rad) }" ".x{transform:rotate(1rad)}";
  roundtrip ".x { transform: rotate(.5turn) }" ".x{transform:rotate(.5turn)}"

(* SS 6.4 - Duration units: ms values are normalized to s when shorter *)
let values_durations () =
  roundtrip ".x { transition-duration: 200ms }" ".x{transition-duration:.2s}";
  roundtrip ".x { transition-duration: 1s }" ".x{transition-duration:1s}";
  roundtrip ".x { transition-duration: 1500ms }" ".x{transition-duration:1.5s}"

(* {2 CSS Color Level 4} https://www.w3.org/TR/css-color-4/ *)

(* SS 6 - Named colors *)
let color_named () =
  roundtrip ".x { color: red }" ".x{color:red}";
  roundtrip ".x { color: blue }" ".x{color:blue}";
  (* SS 6.1 - rebeccapurple *)
  roundtrip ".x { color: rebeccapurple }" ".x{color:rebeccapurple}"

(* SS 5.1 - Hex notation *)
let color_hex () =
  (* 3-digit #rgb *)
  roundtrip_identity ".x{color:#f00}";
  (* 6-digit #rrggbb *)
  roundtrip_identity ".x{color:#ff0000}";
  (* 4-digit #rgba *)
  roundtrip_identity ".x{color:#f00f}";
  (* 8-digit #rrggbbaa *)
  roundtrip_identity ".x{color:#ff0000ff}"

(* SS 5.2.3 - rgb() function *)
let color_rgb () =
  (* Modern space-separated syntax *)
  roundtrip ".x { color: rgb(255 0 0) }" ".x{color:rgb(255 0 0)}";
  (* With alpha *)
  roundtrip ".x { color: rgb(255 0 0 / 50%) }" ".x{color:rgb(255 0 0/50%)}";
  (* Percentage form *)
  roundtrip ".x { color: rgb(100% 0% 0%) }" ".x{color:rgb(100% 0% 0%)}"

(* SS 5.2.4 - hsl() function *)
let color_hsl () =
  (* Modern space-separated syntax - hue in degrees (default unit, dropped) *)
  roundtrip ".x { color: hsl(120 100% 50%) }" ".x{color:hsl(120 100% 50%)}";
  (* With alpha *)
  roundtrip ".x { color: hsl(120 100% 50% / 50%) }"
    ".x{color:hsl(120 100% 50%/50%)}"

(* SS 5.2.5 - hwb() function *)
let color_hwb () =
  roundtrip ".x { color: hwb(90 10% 20%) }" ".x{color:hwb(90 10% 20%)}";
  roundtrip ".x { color: hwb(90 10% 20% / 0.25) }"
    ".x{color:hwb(90 10% 20%/.25)}"

(* SS 5.2.6 - oklch() and oklab() modern color functions *)
let color_oklch_oklab () =
  roundtrip ".x { color: oklch(50% 0.2 30) }" ".x{color:oklch(50% .2 30)}";
  roundtrip ".x { color: oklab(50% 0.1 -0.05) }" ".x{color:oklab(50% .1 -.05)}"

(* SS 5.2.7 - color-mix() function *)
let color_mix () =
  roundtrip ".x { color: color-mix(in srgb, red, blue) }"
    ".x{color:color-mix(in srgb,red,blue)}"

(* SS 5.3 - transparent and currentcolor keywords *)
let color_keywords () =
  roundtrip ".x { color: transparent }" ".x{color:transparent}";
  (* currentColor preserves its camelCase form *)
  roundtrip ".x { color: currentColor }" ".x{color:currentColor}"

(* {2 CSS Conditional Rules Level 3} https://www.w3.org/TR/css-conditional-3/ *)

(* SS 7.1 - @media with min-width/max-width *)
let conditional_media () =
  parses_valid "@media (min-width: 768px) { .btn { display: block } }";
  parses_valid "@media (max-width: 640px) { .btn { font-size: 14px } }";
  parses_valid
    "@media (prefers-color-scheme: dark) { body { background-color: black } }"

(* SS 8 - @supports with property checks *)
let conditional_supports () =
  roundtrip "@supports (display: grid) { .grid { display: grid } }"
    "@supports (display:grid){.grid{display:grid}}";
  roundtrip
    "@supports at-rule(@container) { .cq { container-type: inline-size } }"
    "@supports at-rule(@container){.cq{container-type:inline-size}}"

(* {2 CSS Syntax and Stylesheet At-rule Coverage}
   https://www.w3.org/TR/css-syntax-3/ SS 8 *)

let stylesheet_at_rules () =
  roundtrip "@charset \"UTF-8\";" "@charset \"UTF-8\";";
  roundtrip "@namespace url(http://www.w3.org/1999/xhtml);"
    "@namespace url(http://www.w3.org/1999/xhtml);";
  roundtrip "@namespace svg url(http://www.w3.org/2000/svg);"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  roundtrip "@page :left { margin-left: 4cm; margin-right: 3cm }"
    "@page:left{margin-left:4cm;margin-right:3cm}"

(* {2 CSS Cascade and Inheritance Level 4}
   https://www.w3.org/TR/css-cascade-4/ *)

(* SS 7.1 - CSS-wide keywords *)
let cascade_keywords () =
  roundtrip ".x { color: inherit }" ".x{color:inherit}";
  roundtrip ".x { color: initial }" ".x{color:initial}";
  roundtrip ".x { color: unset }" ".x{color:unset}";
  roundtrip ".x { color: revert }" ".x{color:revert}";
  roundtrip ".x { color: revert-layer }" ".x{color:revert-layer}"

(* SS 6.6 - @layer declarations and blocks *)
let cascade_layers () =
  (* Layer block with rules *)
  roundtrip "@layer base { body { margin: 0 } }" "@layer base{body{margin:0}}";
  (* Layer statement establishes layer order *)
  roundtrip "@layer reset, base, components;" "@layer reset,base,components;";
  (* Layer block with multiple rules *)
  roundtrip "@layer base { h1 { color: red } p { margin: 0 } }"
    "@layer base{h1{color:red}p{margin:0}}"

(* {2 CSS Cascade Level 5 / 6 at-rule syntax}
   https://www.w3.org/TR/css-cascade-5/ https://www.w3.org/TR/css-cascade-6/ *)

let cascade_current_at_rules () =
  parses_valid
    "@import url(\"theme.css\") layer(theme) supports(display: grid) screen;";
  roundtrip "@scope (.card) to (.footer) { .title { color: red } }"
    "@scope(.card) to (.footer){.title{color:red}}";
  roundtrip "@starting-style { .dialog { opacity: 0 } }"
    "@starting-style{.dialog{opacity:0}}"

(* {2 CSS Conditional Rules / Container Queries}
   https://www.w3.org/TR/css-conditional-3/
   https://www.w3.org/TR/css-contain-3/ *)

let conditional_container () =
  parses_valid
    "@container card (inline-size > 30em) { .item { display: grid } }";
  parses_valid "@container style(--variant: featured) { .card { color: red } }";
  parses_valid "@container scroll-state(stuck: top) { .card { color: red } }";
  rejects_invalid "@container style() { .card { color: red } }";
  rejects_invalid "@container scroll-state() { .card { color: red } }"

(* {2 CSS Nesting Level 1} https://www.w3.org/TR/css-nesting-1/ *)

let nesting_rules () =
  roundtrip ".card { color: red; & > img { display: block } }"
    ".card{color:red;&>img{display:block}}";
  parses_valid ".card { @media (width >= 40em) { & > img { display: block } } }"

(* {2 CSS Custom Properties for Cascading Variables Level 1}
   https://www.w3.org/TR/css-variables-1/ *)

(* SS 2 - Custom property definitions *)
let custom_properties () =
  roundtrip ":root { --primary-color: blue }" ":root{--primary-color:blue}";
  roundtrip ".x { color: var(--primary-color) }"
    ".x{color:var(--primary-color)}"

(* {2 CSS Fonts Level 4} https://www.w3.org/TR/css-fonts-4/ *)

(* SS 4.2 - @font-face rule *)
let font_face () =
  roundtrip "@font-face { font-family: MyFont; src: url(font.woff2); }"
    "@font-face {font-family:MyFont;src:url(font.woff2)}";
  parses_valid
    "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
     format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }"

(* {2 CSS Animations Level 1} https://www.w3.org/TR/css-animations-1/ *)

(* SS 7 - @keyframes rule *)
let keyframes () =
  roundtrip "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }"
    "@keyframes slide{0%{opacity:0}100%{opacity:1}}"

(* {2 Compound selectors and complex combinations}
   https://www.w3.org/TR/selectors-4/ SS 4 *)

let selectors_compound () =
  (* Element + class *)
  roundtrip "div.container { margin: auto }" "div.container{margin:auto}";
  (* Element + ID *)
  roundtrip "div#main { display: flex }" "div#main{display:flex}";
  (* Element + pseudo-class *)
  roundtrip "a:hover { color: red }" "a:hover{color:red}";
  (* Element + pseudo-element: minified output uses legacy single-colon form. *)
  roundtrip "p::first-line { color: blue }" "p:first-line{color:blue}";
  (* Multiple compound: element + class + pseudo *)
  roundtrip "a.link:hover { color: red }" "a.link:hover{color:red}"

(* {2 CSS Properties and Values API Level 1}
   https://www.w3.org/TR/css-properties-values-api-1/ *)

(* SS 3 - @property rule *)
let property_at_rule () =
  roundtrip
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: \
     red }"
    "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"

(* {2 Test suite registration} *)

let () =
  Alcotest.run "spec"
    [
      ( "spec",
        [
          Alcotest.test_case "css2: selectors and at-rules" `Quick
            css2_selectors_and_at_rules;
          Alcotest.test_case "css2: pseudo-elements and aliases" `Quick
            css2_pseudo_elements_aliases;
          Alcotest.test_case "css2: invalid legacy vectors" `Quick
            css2_legacy_invalid_vectors;
          Alcotest.test_case "css2: chapter matrix" `Quick css2_chapter_matrix;
          (* CSS Syntax Level 3 *)
          Alcotest.test_case "syntax: qualified rules" `Quick
            syntax_qualified_rules;
          Alcotest.test_case "syntax: at-rules" `Quick syntax_at_rules;
          Alcotest.test_case "syntax: comments" `Quick syntax_comments;
          Alcotest.test_case "syntax: whitespace" `Quick syntax_whitespace;
          Alcotest.test_case "syntax: escapes" `Quick syntax_escapes;
          Alcotest.test_case "syntax: recovery" `Quick syntax_recovery;
          (* CSS Selectors Level 4 *)
          Alcotest.test_case "selectors: type" `Quick selectors_type;
          Alcotest.test_case "selectors: universal" `Quick selectors_universal;
          Alcotest.test_case "selectors: class" `Quick selectors_class;
          Alcotest.test_case "selectors: id" `Quick selectors_id;
          Alcotest.test_case "selectors: attribute" `Quick selectors_attribute;
          Alcotest.test_case "selectors: pseudo-classes" `Quick
            selectors_pseudo_classes;
          Alcotest.test_case "selectors: pseudo-elements" `Quick
            selectors_pseudo_elements;
          Alcotest.test_case "selectors: combinators" `Quick
            selectors_combinators;
          Alcotest.test_case "selectors: list" `Quick selectors_list;
          Alcotest.test_case "selectors: :where() and :is()" `Quick
            selectors_where_is;
          Alcotest.test_case "selectors: compound" `Quick selectors_compound;
          (* CSS Values and Units Level 4 *)
          Alcotest.test_case "values: absolute lengths" `Quick
            values_absolute_lengths;
          Alcotest.test_case "values: relative lengths" `Quick
            values_relative_lengths;
          Alcotest.test_case "values: calc()" `Quick values_calc;
          Alcotest.test_case "values: angles" `Quick values_angles;
          Alcotest.test_case "values: durations" `Quick values_durations;
          (* CSS Color Level 4 *)
          Alcotest.test_case "color: named" `Quick color_named;
          Alcotest.test_case "color: hex notation" `Quick color_hex;
          Alcotest.test_case "color: rgb()" `Quick color_rgb;
          Alcotest.test_case "color: hsl()" `Quick color_hsl;
          Alcotest.test_case "color: hwb()" `Quick color_hwb;
          Alcotest.test_case "color: oklch() and oklab()" `Quick
            color_oklch_oklab;
          Alcotest.test_case "color: color-mix()" `Quick color_mix;
          Alcotest.test_case "color: transparent/currentcolor" `Quick
            color_keywords;
          (* CSS Conditional Rules Level 3 *)
          Alcotest.test_case "conditional: @media" `Quick conditional_media;
          Alcotest.test_case "conditional: @supports" `Quick
            conditional_supports;
          Alcotest.test_case "conditional: @container" `Quick
            conditional_container;
          Alcotest.test_case "stylesheet: at-rules" `Quick stylesheet_at_rules;
          (* CSS Cascade and Inheritance Level 4 *)
          Alcotest.test_case "cascade: CSS-wide keywords" `Quick
            cascade_keywords;
          Alcotest.test_case "cascade: @layer" `Quick cascade_layers;
          Alcotest.test_case "cascade: current at-rules" `Quick
            cascade_current_at_rules;
          (* CSS Nesting *)
          Alcotest.test_case "nesting: rules" `Quick nesting_rules;
          (* CSS Custom Properties *)
          Alcotest.test_case "variables: custom properties" `Quick
            custom_properties;
          (* CSS Fonts Level 4 *)
          Alcotest.test_case "fonts: @font-face" `Quick font_face;
          (* CSS Animations Level 1 *)
          Alcotest.test_case "animations: @keyframes" `Quick keyframes;
          (* CSS Properties and Values API *)
          Alcotest.test_case "properties: @property" `Quick property_at_rule;
        ] );
    ]
