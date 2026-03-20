(** CSS specification compliance tests.

    Test vectors derived from W3C CSS specifications to verify parsing
    and rendering conformance. *)

open Cascade
open Css

(** {2 Helpers} *)

let roundtrip css expected =
  match of_string css with
  | Ok sheet ->
    let output = to_string ~minify:true ~newline:false sheet in
    Alcotest.(check string) css expected output
  | Error e -> Alcotest.fail (pp_parse_error e)

let roundtrip_identity css = roundtrip css css

(** {2 CSS Syntax Level 3}
    https://www.w3.org/TR/css-syntax-3/ *)

(* SS 5.3 - Qualified rules: a prelude (selector) + block (declarations) *)
let test_syntax_qualified_rules () =
  (* Single rule with single declaration *)
  roundtrip
    "h1 { color: red }"
    "h1{color:red}";
  (* Multiple selectors in selector list *)
  roundtrip
    "h1, h2, h3 { margin: 0 }"
    "h1,h2,h3{margin:0}";
  (* Multiple declarations *)
  roundtrip
    "p { color: blue; font-size: 16px }"
    "p{color:blue;font-size:16px}";
  (* Multiple rules in sequence *)
  roundtrip
    "h1 { color: red } p { margin: 0 }"
    "h1{color:red}p{margin:0}"

(* SS 5.4 - At-rules: @media and @import *)
let test_syntax_at_rules () =
  (* @media at-rule *)
  roundtrip
    "@media screen { .btn { color: green } }"
    "@media screen{.btn{color:green}}";
  (* @import at-rule - trailing semicolon is preserved in output *)
  roundtrip
    "@import url(\"reset.css\");"
    "@import url(\"reset.css\");";
  (* @layer block *)
  roundtrip
    "@layer base { body { margin: 0 } }"
    "@layer base{body{margin:0}}"

(* SS 4.3.2 - Comments *)
let test_syntax_comments () =
  (* Comments are stripped during parsing *)
  roundtrip
    "/* This is a comment */ h1 { color: red }"
    "h1{color:red}";
  roundtrip
    "h1 { /* inline comment */ color: red }"
    "h1{color:red}";
  roundtrip
    "h1 { color: /* mid-value */ red }"
    "h1{color:red}"

(* SS 4.3.4 - Whitespace normalization *)
let test_syntax_whitespace () =
  (* Extra whitespace is normalized *)
  roundtrip
    "  h1  {  color  :  red  }  "
    "h1{color:red}";
  (* Tab and newline normalization *)
  roundtrip
    "h1\t{\n\tcolor:\tred\n}"
    "h1{color:red}"

(* SS 4.3.7 - Escape sequences in identifiers *)
let test_syntax_escapes () =
  (* Escaped class names roundtrip correctly *)
  roundtrip_identity
    ".sm\\:p-4{color:red}";
  roundtrip_identity
    ".w-1\\/2{width:50%}"

(** {2 CSS Selectors Level 4}
    https://www.w3.org/TR/selectors-4/ *)

(* SS 5.1 - Type selectors *)
let test_selectors_type () =
  roundtrip "h1 { color: red }" "h1{color:red}";
  roundtrip "div { display: block }" "div{display:block}";
  roundtrip "span { color: blue }" "span{color:blue}";
  roundtrip "article { margin: 0 }" "article{margin:0}"

(* SS 5.1 - Universal selector *)
let test_selectors_universal () =
  roundtrip "* { box-sizing: border-box }" "*{box-sizing:border-box}"

(* SS 6.1 - Class selectors *)
let test_selectors_class () =
  roundtrip ".warning { color: red }" ".warning{color:red}";
  roundtrip ".info { color: blue }" ".info{color:blue}"

(* SS 6.2 - ID selectors *)
let test_selectors_id () =
  roundtrip "#myid { color: red }" "#myid{color:red}";
  roundtrip "#main { display: flex }" "#main{display:flex}"

(* SS 7 - Attribute selectors *)
let test_selectors_attribute () =
  (* [att] - Presence *)
  roundtrip "[href] { color: blue }" "[href]{color:blue}";
  (* [att=val] - Exact match: simple ident values are unquoted in output *)
  roundtrip "[type=\"text\"] { border: 1px solid gray }"
    "[type=text]{border:1px solid gray}";
  (* [att~=val] - Whitespace-separated list *)
  roundtrip "[class~=\"warning\"] { color: red }"
    "[class~=warning]{color:red}";
  (* [att|=val] - Hyphen-separated list *)
  roundtrip "[lang|=\"en\"] { color: blue }"
    "[lang|=en]{color:blue}";
  (* [att^=val] - Prefix *)
  roundtrip "[href^=\"https\"] { color: green }"
    "[href^=https]{color:green}";
  (* [att$=val] - Suffix: non-ident values keep quotes *)
  roundtrip "[href$=\".pdf\"] { color: red }"
    "[href$=\".pdf\"]{color:red}";
  (* [att*=val] - Substring *)
  roundtrip "[title*=\"hello\"] { color: blue }"
    "[title*=hello]{color:blue}"

(* SS 8.1 - Pseudo-classes *)
let test_selectors_pseudo_classes () =
  roundtrip ":hover { color: red }" ":hover{color:red}";
  roundtrip ":first-child { color: red }" ":first-child{color:red}";
  roundtrip ":last-child { margin: 0 }" ":last-child{margin:0}";
  roundtrip ":nth-child(2n+1) { color: red }" ":nth-child(2n+1){color:red}";
  (* even normalizes to 2n, odd normalizes to 2n+1 in output *)
  roundtrip ":nth-child(even) { color: blue }" ":nth-child(2n){color:blue}";
  roundtrip ":nth-child(odd) { color: red }" ":nth-child(2n+1){color:red}";
  roundtrip ":not(.foo) { color: red }" ":not(.foo){color:red}"

(* SS 8.2 - Pseudo-elements
   The printer uses legacy single-colon form for backwards compatibility
   and normalizes single-quoted strings to double-quoted *)
let test_selectors_pseudo_elements () =
  roundtrip "::before { content: '' }" ":before{content:\"\"}";
  roundtrip "::after { content: '' }" ":after{content:\"\"}";
  roundtrip "::first-line { color: red }" ":first-line{color:red}"

(* SS 15 - Combinators *)
let test_selectors_combinators () =
  (* Descendant combinator (space) *)
  roundtrip "div p { color: red }" "div p{color:red}";
  (* Child combinator (>) *)
  roundtrip "div > p { color: red }" "div>p{color:red}";
  (* Adjacent sibling combinator (+) *)
  roundtrip "h1 + p { color: red }" "h1+p{color:red}";
  (* General sibling combinator (~) *)
  roundtrip "h1 ~ p { color: red }" "h1~p{color:red}"

(* SS 4 - Selector lists *)
let test_selectors_list () =
  roundtrip "h1, h2, h3 { margin: 0 }" "h1,h2,h3{margin:0}";
  roundtrip ".a, .b, .c { display: block }" ".a,.b,.c{display:block}"

(* SS 8.4.1 - :where() and :is() pseudo-classes *)
let test_selectors_where_is () =
  roundtrip ":where(.a, .b) { color: red }" ":where(.a,.b){color:red}";
  roundtrip ":is(.a, .b) { color: red }" ":is(.a,.b){color:red}"

(** {2 CSS Values and Units Level 4}
    https://www.w3.org/TR/css-values-4/ *)

(* SS 6.1 - Absolute lengths *)
let test_values_absolute_lengths () =
  roundtrip ".x { width: 100px }" ".x{width:100px}";
  roundtrip ".x { width: 10cm }" ".x{width:10cm}";
  roundtrip ".x { width: 10mm }" ".x{width:10mm}";
  roundtrip ".x { width: 1in }" ".x{width:1in}";
  roundtrip ".x { width: 12pt }" ".x{width:12pt}";
  roundtrip ".x { width: 1pc }" ".x{width:1pc}"

(* SS 6.2 - Relative lengths *)
let test_values_relative_lengths () =
  roundtrip ".x { font-size: 2em }" ".x{font-size:2em}";
  roundtrip ".x { font-size: 1.5rem }" ".x{font-size:1.5rem}";
  roundtrip ".x { width: 50vw }" ".x{width:50vw}";
  roundtrip ".x { height: 100vh }" ".x{height:100vh}";
  roundtrip ".x { width: 50% }" ".x{width:50%}"

(* SS 10.1 - calc() expressions *)
let test_values_calc () =
  roundtrip ".x { width: calc(100% - 2rem) }" ".x{width:calc(100% - 2rem)}";
  roundtrip ".x { width: calc(2 * 3rem) }" ".x{width:calc(2*3rem)}";
  roundtrip
    ".x { width: calc(100% - calc(2rem + 10px)) }"
    ".x{width:calc(100% - calc(2rem + 10px))}"

(* SS 6.3 - Angle units *)
let test_values_angles () =
  roundtrip ".x { transform: rotate(45deg) }" ".x{transform:rotate(45deg)}";
  roundtrip ".x { transform: rotate(1rad) }" ".x{transform:rotate(1rad)}";
  roundtrip ".x { transform: rotate(.5turn) }" ".x{transform:rotate(.5turn)}"

(* SS 6.4 - Duration units: ms values are normalized to s when shorter *)
let test_values_durations () =
  roundtrip ".x { transition-duration: 200ms }" ".x{transition-duration:.2s}";
  roundtrip ".x { transition-duration: 1s }" ".x{transition-duration:1s}";
  roundtrip ".x { transition-duration: 1500ms }" ".x{transition-duration:1.5s}"

(** {2 CSS Color Level 4}
    https://www.w3.org/TR/css-color-4/ *)

(* SS 6 - Named colors *)
let test_color_named () =
  roundtrip ".x { color: red }" ".x{color:red}";
  roundtrip ".x { color: blue }" ".x{color:blue}";
  (* SS 6.1 - rebeccapurple *)
  roundtrip ".x { color: rebeccapurple }" ".x{color:rebeccapurple}"

(* SS 5.1 - Hex notation *)
let test_color_hex () =
  (* 3-digit #rgb *)
  roundtrip_identity ".x{color:#f00}";
  (* 6-digit #rrggbb *)
  roundtrip_identity ".x{color:#ff0000}";
  (* 4-digit #rgba *)
  roundtrip_identity ".x{color:#f00f}";
  (* 8-digit #rrggbbaa *)
  roundtrip_identity ".x{color:#ff0000ff}"

(* SS 5.2.3 - rgb() function *)
let test_color_rgb () =
  (* Modern space-separated syntax *)
  roundtrip ".x { color: rgb(255 0 0) }" ".x{color:rgb(255 0 0)}";
  (* With alpha *)
  roundtrip ".x { color: rgb(255 0 0 / 50%) }" ".x{color:rgb(255 0 0/50%)}";
  (* Percentage form *)
  roundtrip ".x { color: rgb(100% 0% 0%) }" ".x{color:rgb(100% 0% 0%)}"

(* SS 5.2.4 - hsl() function *)
let test_color_hsl () =
  (* Modern space-separated syntax - hue in degrees (default unit, dropped) *)
  roundtrip ".x { color: hsl(120 100% 50%) }" ".x{color:hsl(120 100% 50%)}";
  (* With alpha *)
  roundtrip ".x { color: hsl(120 100% 50% / 50%) }"
    ".x{color:hsl(120 100% 50%/50%)}"

(* SS 5.2.5 - hwb() function *)
let test_color_hwb () =
  roundtrip ".x { color: hwb(90 10% 20%) }" ".x{color:hwb(90 10% 20%)}";
  roundtrip ".x { color: hwb(90 10% 20% / 0.25) }"
    ".x{color:hwb(90 10% 20%/.25)}"

(* SS 5.2.6 - oklch() and oklab() modern color functions *)
let test_color_oklch_oklab () =
  roundtrip ".x { color: oklch(50% 0.2 30) }" ".x{color:oklch(50% .2 30)}";
  roundtrip ".x { color: oklab(50% 0.1 -0.05) }" ".x{color:oklab(50% .1 -.05)}"

(* SS 5.2.7 - color-mix() function *)
let test_color_mix () =
  roundtrip ".x { color: color-mix(in srgb, red, blue) }"
    ".x{color:color-mix(in srgb,red,blue)}"

(* SS 5.3 - transparent and currentcolor keywords *)
let test_color_keywords () =
  roundtrip ".x { color: transparent }" ".x{color:transparent}";
  (* currentColor preserves its camelCase form *)
  roundtrip ".x { color: currentColor }" ".x{color:currentColor}"

(** {2 CSS Conditional Rules Level 3}
    https://www.w3.org/TR/css-conditional-3/ *)

(* SS 7.1 - @media with min-width/max-width *)
let test_conditional_media () =
  roundtrip
    "@media (min-width: 768px) { .btn { display: block } }"
    "@media (min-width: 768px){.btn{display:block}}";
  roundtrip
    "@media (max-width: 640px) { .btn { font-size: 14px } }"
    "@media (max-width: 640px){.btn{font-size:14px}}";
  roundtrip
    "@media (prefers-color-scheme: dark) { body { background-color: black } }"
    "@media (prefers-color-scheme: dark){body{background-color:black}}"

(* SS 8 - @supports with property checks *)
let test_conditional_supports () =
  roundtrip
    "@supports (display: grid) { .grid { display: grid } }"
    "@supports (display:grid){.grid{display:grid}}"

(** {2 CSS Cascade and Inheritance Level 4}
    https://www.w3.org/TR/css-cascade-4/ *)

(* SS 7.1 - CSS-wide keywords *)
let test_cascade_keywords () =
  roundtrip ".x { color: inherit }" ".x{color:inherit}";
  roundtrip ".x { color: initial }" ".x{color:initial}";
  roundtrip ".x { color: unset }" ".x{color:unset}";
  roundtrip ".x { color: revert }" ".x{color:revert}";
  roundtrip ".x { color: revert-layer }" ".x{color:revert-layer}"

(* SS 6.6 - @layer declarations and blocks *)
let test_cascade_layers () =
  (* Layer block with rules *)
  roundtrip
    "@layer base { body { margin: 0 } }"
    "@layer base{body{margin:0}}";
  (* Layer block with multiple rules *)
  roundtrip
    "@layer base { h1 { color: red } p { margin: 0 } }"
    "@layer base{h1{color:red}p{margin:0}}"

(** {2 CSS Custom Properties for Cascading Variables Level 1}
    https://www.w3.org/TR/css-variables-1/ *)

(* SS 2 - Custom property definitions *)
let test_custom_properties () =
  roundtrip
    ":root { --primary-color: blue }"
    ":root{--primary-color:blue}";
  roundtrip
    ".x { color: var(--primary-color) }"
    ".x{color:var(--primary-color)}"

(** {2 CSS Fonts Level 4}
    https://www.w3.org/TR/css-fonts-4/ *)

(* SS 4.2 - @font-face rule *)
let test_font_face () =
  roundtrip
    "@font-face { font-family: MyFont; src: url(font.woff2); }"
    "@font-face {font-family:MyFont;src:url(font.woff2)}"

(** {2 CSS Animations Level 1}
    https://www.w3.org/TR/css-animations-1/ *)

(* SS 7 - @keyframes rule *)
let test_keyframes () =
  roundtrip
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }"
    "@keyframes slide{0%{opacity:0}100%{opacity:1}}"

(** {2 Compound selectors and complex combinations}
    https://www.w3.org/TR/selectors-4/ SS 4 *)

let test_selectors_compound () =
  (* Element + class *)
  roundtrip "div.container { margin: auto }" "div.container{margin:auto}";
  (* Element + ID *)
  roundtrip "div#main { display: flex }" "div#main{display:flex}";
  (* Element + pseudo-class *)
  roundtrip "a:hover { color: red }" "a:hover{color:red}";
  (* Element + pseudo-element: uses legacy single-colon form *)
  roundtrip "p::first-line { color: blue }" "p:first-line{color:blue}";
  (* Multiple compound: element + class + pseudo *)
  roundtrip "a.link:hover { color: red }" "a.link:hover{color:red}"

(** {2 CSS Properties and Values API Level 1}
    https://www.w3.org/TR/css-properties-values-api-1/ *)

(* SS 3 - @property rule *)
let test_property_at_rule () =
  roundtrip
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: red }"
    "@property --color{syntax:\"<color>\";inherits:true;initial-value:red}"

(** {2 Test suite registration} *)

let suite =
  ( "spec",
    [
      (* CSS Syntax Level 3 *)
      Alcotest.test_case "syntax: qualified rules" `Quick
        test_syntax_qualified_rules;
      Alcotest.test_case "syntax: at-rules" `Quick test_syntax_at_rules;
      Alcotest.test_case "syntax: comments" `Quick test_syntax_comments;
      Alcotest.test_case "syntax: whitespace" `Quick test_syntax_whitespace;
      Alcotest.test_case "syntax: escapes" `Quick test_syntax_escapes;
      (* CSS Selectors Level 4 *)
      Alcotest.test_case "selectors: type" `Quick test_selectors_type;
      Alcotest.test_case "selectors: universal" `Quick test_selectors_universal;
      Alcotest.test_case "selectors: class" `Quick test_selectors_class;
      Alcotest.test_case "selectors: id" `Quick test_selectors_id;
      Alcotest.test_case "selectors: attribute" `Quick test_selectors_attribute;
      Alcotest.test_case "selectors: pseudo-classes" `Quick
        test_selectors_pseudo_classes;
      Alcotest.test_case "selectors: pseudo-elements" `Quick
        test_selectors_pseudo_elements;
      Alcotest.test_case "selectors: combinators" `Quick
        test_selectors_combinators;
      Alcotest.test_case "selectors: list" `Quick test_selectors_list;
      Alcotest.test_case "selectors: :where() and :is()" `Quick
        test_selectors_where_is;
      Alcotest.test_case "selectors: compound" `Quick test_selectors_compound;
      (* CSS Values and Units Level 4 *)
      Alcotest.test_case "values: absolute lengths" `Quick
        test_values_absolute_lengths;
      Alcotest.test_case "values: relative lengths" `Quick
        test_values_relative_lengths;
      Alcotest.test_case "values: calc()" `Quick test_values_calc;
      Alcotest.test_case "values: angles" `Quick test_values_angles;
      Alcotest.test_case "values: durations" `Quick test_values_durations;
      (* CSS Color Level 4 *)
      Alcotest.test_case "color: named" `Quick test_color_named;
      Alcotest.test_case "color: hex notation" `Quick test_color_hex;
      Alcotest.test_case "color: rgb()" `Quick test_color_rgb;
      Alcotest.test_case "color: hsl()" `Quick test_color_hsl;
      Alcotest.test_case "color: hwb()" `Quick test_color_hwb;
      Alcotest.test_case "color: oklch() and oklab()" `Quick
        test_color_oklch_oklab;
      Alcotest.test_case "color: color-mix()" `Quick test_color_mix;
      Alcotest.test_case "color: transparent/currentcolor" `Quick
        test_color_keywords;
      (* CSS Conditional Rules Level 3 *)
      Alcotest.test_case "conditional: @media" `Quick test_conditional_media;
      Alcotest.test_case "conditional: @supports" `Quick
        test_conditional_supports;
      (* CSS Cascade and Inheritance Level 4 *)
      Alcotest.test_case "cascade: CSS-wide keywords" `Quick
        test_cascade_keywords;
      Alcotest.test_case "cascade: @layer" `Quick test_cascade_layers;
      (* CSS Custom Properties *)
      Alcotest.test_case "variables: custom properties" `Quick
        test_custom_properties;
      (* CSS Fonts Level 4 *)
      Alcotest.test_case "fonts: @font-face" `Quick test_font_face;
      (* CSS Animations Level 1 *)
      Alcotest.test_case "animations: @keyframes" `Quick test_keyframes;
      (* CSS Properties and Values API *)
      Alcotest.test_case "properties: @property" `Quick test_property_at_rule;
    ] )
