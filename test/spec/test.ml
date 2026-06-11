(** CSS specification compliance tests.

    Test vectors are derived from W3C CSS specifications. The specs are the
    oracle: do not update expected results to match current implementation
    output. A failing spec-derived vector should remain visible until the
    implementation is fixed or the feature is explicitly out of scope. *)

open Cascade
open Css

(* {2 Helpers} *)

let roundtrip css expected =
  match of_string ~strict:true css with
  | Ok parsed ->
      let output =
        parsed.stylesheet
        |> optimize ~scope:`Stylesheet ~enforce_spec:true
        |> to_string ~minify:true
      in
      Alcotest.(check string) css expected output
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let roundtrip_identity css = roundtrip css css

let strip_ascii_ws s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ' ' | '\n' | '\r' | '\t' | '\012' -> () | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let contains_substring ~needle haystack =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if needle_len = 0 then true
    else if i + needle_len > hay_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  loop 0

let preserves_non_minified css fragments =
  match of_string ~strict:true css with
  | Ok parsed ->
      let output = to_string ~minify:false parsed.stylesheet in
      let compact_output = strip_ascii_ws output in
      List.iter
        (fun fragment ->
          let compact_fragment = strip_ascii_ws fragment in
          if not (contains_substring ~needle:compact_fragment compact_output)
          then
            Alcotest.failf
              "non-minified output for %S did not preserve %S\noutput: %S" css
              fragment output)
        fragments
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let preserves_non_minified_exact css fragments =
  match of_string ~strict:true css with
  | Ok parsed ->
      let output = to_string ~minify:false parsed.stylesheet in
      List.iter
        (fun fragment ->
          if not (contains_substring ~needle:fragment output) then
            Alcotest.failf
              "non-minified output for %S did not preserve exact fragment %S\n\
               output: %S"
              css fragment output)
        fragments
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let rejects_non_minified_fragments css fragments =
  match of_string ~strict:true css with
  | Ok parsed ->
      let output = to_string ~minify:false parsed.stylesheet in
      let compact_output = strip_ascii_ws output in
      List.iter
        (fun fragment ->
          let compact_fragment = strip_ascii_ws fragment in
          if contains_substring ~needle:compact_fragment compact_output then
            Alcotest.failf
              "non-minified output for %S should not contain minified-only %S\n\
               output: %S"
              css fragment output)
        fragments
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let rejects_non_minified_prefixes css prefixes =
  match of_string ~strict:true css with
  | Ok parsed ->
      let output = to_string ~minify:false parsed.stylesheet in
      let compact_output = strip_ascii_ws output in
      List.iter
        (fun prefix ->
          let compact_prefix = strip_ascii_ws prefix in
          if
            String.length compact_output >= String.length compact_prefix
            && String.sub compact_output 0 (String.length compact_prefix)
               = compact_prefix
          then
            Alcotest.failf
              "non-minified output for %S should not start with minified-only %S\n\
               output: %S"
              css prefix output)
        prefixes
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let parses_valid css =
  match of_string ~strict:true css with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)

let rejects_invalid css =
  match of_string ~strict:true css with
  | Error _ -> ()
  | Ok parsed ->
      Alcotest.failf "invalid CSS vector parsed: %s -> %s" css
        (to_string ~minify:true parsed.stylesheet)

let recover css expected min_warnings =
  let { stylesheet; warnings } =
    match of_string ~strict:false css with
    | Ok parsed -> parsed
    | Error e -> Alcotest.fail (Cascade.Error.to_string e)
  in
  let output =
    stylesheet
    |> optimize ~scope:`Stylesheet ~enforce_spec:true
    |> to_string ~minify:true
  in
  Alcotest.(check string) css expected output;
  Alcotest.(check bool)
    (css ^ " warnings") true
    (List.length warnings >= min_warnings)

(* Lenient [preserves_non_minified]: keeps [preserves], drops [drops], warns. *)
let recover_non_minified css ~preserves ~drops min_warnings =
  let { stylesheet; warnings } =
    match of_string ~strict:false css with
    | Ok parsed -> parsed
    | Error e -> Alcotest.fail (Cascade.Error.to_string e)
  in
  let output = to_string ~minify:false stylesheet in
  let compact_output = strip_ascii_ws output in
  List.iter
    (fun fragment ->
      let compact_fragment = strip_ascii_ws fragment in
      if not (contains_substring ~needle:compact_fragment compact_output) then
        Alcotest.failf
          "lenient non-minified output for %S did not preserve %S\noutput: %S"
          css fragment output)
    preserves;
  List.iter
    (fun fragment ->
      let compact_fragment = strip_ascii_ws fragment in
      if contains_substring ~needle:compact_fragment compact_output then
        Alcotest.failf
          "lenient non-minified output for %S should have dropped %S but kept \
           it: %S"
          css fragment output)
    drops;
  Alcotest.(check bool)
    (css ^ " warnings") true
    (List.length warnings >= min_warnings)

(* {2 CSS Syntax Level 3} https://www.w3.org/TR/css-syntax-3/ *)

(* {2 CSS 2.x compatibility surface} *)

let css2_selectors_and_at_rules () =
  roundtrip "body { margin: 0; color: black }" "body{margin:0;color:#000}";
  (* CSS Syntax 3 sec. 8.3: [@charset] is a byte-pattern encoding declaration
     consumed before tokenisation, not a stylesheet at-rule, so [@charset
     "UTF-8"] drops under minify (cascade always emits UTF-8). *)
  roundtrip "@charset \"UTF-8\";" "";
  roundtrip "@import 'legacy.css';" "@import\"legacy.css\";";
  roundtrip "@media print { body { color: black } }"
    "@media print{body{color:#000}}";
  roundtrip "@page :left { margin-left: 4cm; margin-right: 3cm }"
    "@page:left{margin-left:4cm;margin-right:3cm}";
  roundtrip "html > body p + p { text-indent: 1em }"
    "html>body p+p{text-indent:1em}";
  roundtrip "a:link { color: blue } a:visited { color: purple }"
    "a:link{color:#00f}a:visited{color:purple}";
  roundtrip "li:first-child { list-style-type: none }"
    "li:first-child{list-style-type:none}"

let css2_pseudo_elements_aliases () =
  roundtrip "h1:first-letter { color: red }" "h1:first-letter{color:red}";
  roundtrip "p::first-line { color: blue }" "p:first-line{color:#00f}";
  roundtrip "q:before { content: open-quote }" "q:before{content:open-quote}";
  roundtrip "q::after { content: close-quote }" "q:after{content:close-quote}";
  roundtrip "div { page-break-before: always }" "div{break-before:page}";
  roundtrip "div { page-break-after: avoid }" "div{break-after:avoid}";
  roundtrip "div { page-break-inside: avoid }" "div{break-inside:avoid}"

let css2_legacy_invalid_vectors () =
  rejects_invalid "@charset 'UTF-8';";
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
        "body,html{display:block;min-height:100%}" );
      (* CSS Selectors 4 section 3.5: [*] in a non-solitary compound is
         redundant, so [*[lang|=en]] serializes as [[lang|=en]]. *)
      ( "body *[lang|=\"en\"] + p:first-line { text-transform: uppercase }",
        "body [lang|=en]+p:first-line{text-transform:uppercase}" );
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
      "@page :unknown { margin: 1cm }";
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
  roundtrip "p { color: blue; font-size: 16px }" "p{color:#00f;font-size:16px}";
  (* Multiple rules in sequence *)
  roundtrip "h1 { color: red } p { margin: 0 }" "h1{color:red}p{margin:0}"

(* SS 5.4 - At-rules: @media and @import *)
let syntax_at_rules () =
  (* @media at-rule *)
  roundtrip "@media screen { .btn { color: green } }"
    "@media screen{.btn{color:green}}";
  (* @import at-rule: import URLs serialize to the shorter string form; trailing
     semicolon is preserved in output. *)
  roundtrip "@import url(\"reset.css\");" "@import\"reset.css\";";
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
  (* CSS Selectors 4 section 3.5: an unknown pseudo-class at an unforgiving site
     is a spec deviation. Lenient mode preserves it for forward compatibility
     and warns; strict mode escalates (pinned by [cross_mode_pinning]). *)
  recover ".a,:future-pseudo { color: red } .b { color: blue }" ".b{color:#00f}"
    1;
  recover "@unknown { .a { color: red } } .b { color: blue }" ".b{color:#00f}" 1;
  recover ".a { color: red" ".a{color:red}" 0

(* {2 CSS Selectors Level 4} https://www.w3.org/TR/selectors-4/ *)

(* SS 5.1 - Type selectors *)
let selectors_type () =
  roundtrip "h1 { color: red }" "h1{color:red}";
  roundtrip "div { display: block }" "div{display:block}";
  roundtrip "span { color: blue }" "span{color:#00f}";
  roundtrip "article { margin: 0 }" "article{margin:0}"

(* SS 5.1 - Universal selector *)
let selectors_universal () =
  roundtrip "* { box-sizing: border-box }" "*{box-sizing:border-box}"

(* SS 6.1 - Class selectors *)
let selectors_class () =
  roundtrip ".warning { color: red }" ".warning{color:red}";
  roundtrip ".info { color: blue }" ".info{color:#00f}"

(* SS 6.2 - ID selectors *)
let selectors_id () =
  roundtrip "#myid { color: red }" "#myid{color:red}";
  roundtrip "#main { display: flex }" "#main{display:flex}"

(* SS 7 - Attribute selectors *)
let selectors_attribute () =
  (* [att] - Presence *)
  roundtrip "[href] { color: blue }" "[href]{color:#00f}";
  (* [att=val] - Exact match: simple ident values are unquoted in output *)
  roundtrip "[type=\"text\"] { border: 1px solid gray }"
    "[type=text]{border:1px solid gray}";
  (* [att~=val] - Whitespace-separated list *)
  roundtrip "[class~=\"warning\"] { color: red }" "[class~=warning]{color:red}";
  (* [att|=val] - Hyphen-separated list *)
  roundtrip "[lang|=\"en\"] { color: blue }" "[lang|=en]{color:#00f}";
  (* [att^=val] - Prefix *)
  roundtrip "[href^=\"https\"] { color: green }" "[href^=https]{color:green}";
  (* [att$=val] - Suffix: non-ident values keep quotes *)
  roundtrip "[href$=\".pdf\"] { color: red }" "[href$=\".pdf\"]{color:red}";
  (* [att*=val] - Substring *)
  roundtrip "[title*=\"hello\"] { color: blue }" "[title*=hello]{color:#00f}"

(* SS 8.1 - Pseudo-classes *)
let selectors_pseudo_classes () =
  roundtrip ":hover { color: red }" ":hover{color:red}";
  roundtrip ":first-child { color: red }" ":first-child{color:red}";
  roundtrip ":last-child { margin: 0 }" ":last-child{margin:0}";
  (* CSS Selectors 4 section 14: the printer canonicalizes [:nth-child(<an+b>)]
     to the shortest spec-equivalent spelling. *)
  roundtrip ":nth-child(2n+1) { color: red }" ":nth-child(odd){color:red}";
  roundtrip ":nth-child(even) { color: blue }" ":nth-child(2n){color:#00f}";
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
  roundtrip ":is(.a, .b) { color: red }" ".a,.b{color:red}";
  (* Empty forgiving list: matches-nothing, dropped + warned in lenient mode;
     strict mode rejects (pinned by [cross_mode_pinning]). *)
  recover ":is() { color: red }" "" 1;
  recover ":where() { color: red }" "" 1;
  (* Forgiving-parse drops the invalid branch, leaving a single-argument
     [:is(.a)]. Per shortest-wins (Lightning CSS) the single-argument [:is()]
     unwraps to the bare selector, since [:is(.a)] is spec- equivalent to [.a]
     (same match set, same specificity per Selectors L4 §17). [:where()] cannot
     unwrap the same way because it contributes zero specificity. *)
  roundtrip ":is(:future-pseudo, .a) { color: red }" ".a{color:red}";
  roundtrip ":where(:future-pseudo, .a) { color: red }" ":where(.a){color:red}"

(* {2 CSS Values and Units Level 4} https://www.w3.org/TR/css-values-4/ *)

(* SS 6.1 - Absolute lengths *)
let values_absolute_lengths () =
  roundtrip ".x { width: 100px }" ".x{width:100px}";
  roundtrip ".x { width: 10cm }" ".x{width:10cm}";
  roundtrip ".x { width: 10mm }" ".x{width:10mm}";
  roundtrip ".x { width: 1in }" ".x{width:1in}";
  roundtrip ".x { width: 12pt }" ".x{width:12pt}";
  roundtrip ".x { width: 1pc }" ".x{width:1pc}";
  (* Values 4 SS 10.2 / Values 5 SS 6.5: unknown / future unit identifiers make
     the typed value invalid. Strict rejects (pinned by [cross_mode_pinning]);
     lenient recovers by dropping the declaration and surfacing the unknown unit
     as a warning. *)
  recover ".x { width: 1unknown; height: 10px }" ".x{height:10px}" 1;
  recover ".x { font-size: 16xyz }" "" 1

(* SS 6.2 - Relative lengths *)
let values_relative_lengths () =
  roundtrip ".x { font-size: 2em }" ".x{font-size:2em}";
  roundtrip ".x { font-size: 1.5rem }" ".x{font-size:1.5rem}";
  roundtrip ".x { width: 50vw }" ".x{width:50vw}";
  roundtrip ".x { height: 100vh }" ".x{height:100vh}";
  roundtrip ".x { width: 50% }" ".x{width:50%}"

(* SS 10.1 - calc() expressions. Per CSS Values 4 section 10.7 the printer
   simplifies all-constant calc subexpressions and reduces a single
   multiplicative operand to its product. Mixed-unit forms that cannot reduce at
   parse time still drop redundant nested calc() under minify when doing so is
   shorter and spec-equivalent. *)
let values_calc () =
  roundtrip ".x { width: calc(100% - 2rem) }" ".x{width:calc(100% - 2rem)}";
  roundtrip ".x { width: calc(2 * 3rem) }" ".x{width:6rem}";
  roundtrip ".x { width: calc(100% - calc(2rem + 10px)) }"
    ".x{width:calc(100% - 2rem - 10px)}"

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
  roundtrip ".x { color: blue }" ".x{color:#00f}";
  (* SS 6.1 - rebeccapurple *)
  roundtrip ".x { color: rebeccapurple }" ".x{color:#639}"

(* SS 5.1 - Hex notation. Per CSS Color 4 section 12.1, [#rrggbb] and the
   3-digit shorthand [#rgb] (and likewise [#rrggbbaa]/[#rgba]) denote the
   identical color, and a fully-opaque alpha channel ([f]/[ff]) is equivalent to
   omitting alpha. Under [~minify:true] the printer canonicalizes to the
   shortest equivalent form, including the CSS-named color when shorter (cssnano
   / Lightning CSS / clean-css conventions). *)
let color_hex () =
  (* All these forms denote pure red; the shortest spelling is the named color
     [red]. *)
  roundtrip ".x{color:red}" ".x{color:red}";
  roundtrip ".x{color:#ff0000}" ".x{color:red}";
  roundtrip ".x{color:#f00f}" ".x{color:red}";
  roundtrip ".x{color:#ff0000ff}" ".x{color:red}"

(* SS 5.2.3 - rgb() function *)
let color_rgb () =
  roundtrip ".x { color: rgb(255 0 0) }" ".x{color:red}";
  (* CSS Color 4 section 1.3: alpha as [<number>] in [\[0, 1\]] is
     spec-equivalent to [<percentage>] in [\[0%, 100%\]]; the printer
     canonicalizes to the [<number>] form per cssnano. *)
  roundtrip ".x { color: rgb(255 0 0 / 50%) }" ".x{color:#ff000080}";
  (* CSS Color 4 section 1.4: rgb() with all-percent channels denotes the same
     color as the equivalent named/hex form; rgb(100% 0% 0%) is red. *)
  roundtrip ".x { color: rgb(100% 0% 0%) }" ".x{color:red}"

(* SS 5.2.4 - hsl() function. Per CSS Color 4 section 1.4 the hsl() form denotes
   the same color as a named color or hex when applicable; minified output
   canonicalizes to the shortest equivalent spelling. Equal-length named/hex
   ties use the hex spelling, matching the Lightning CSS oracle in
   test/interop/lightning/traces/minify.pairs. *)
let color_hsl () =
  roundtrip ".x { color: hsl(120 100% 50%) }" ".x{color:#0f0}";
  roundtrip ".x { color: hsl(120 100% 50% / 50%) }" ".x{color:#00ff0080}"

(* SS 5.2.5 - hwb() function. Static HWB colors are equivalent to sRGB colors
   and minify to the shortest exact hex spelling, matching Lightning CSS. *)
let color_hwb () =
  roundtrip ".x { color: hwb(90 10% 20%) }" ".x{color:#73cc1a}";
  roundtrip ".x { color: hwb(90 10% 20% / 0.25) }" ".x{color:#73cc1a40}"

(* SS 5.2.6 - oklch() and oklab() modern color functions *)
let color_oklch_oklab () =
  roundtrip ".x { color: oklch(50% 0.2 30) }" ".x{color:#ba0d01}";
  roundtrip ".x { color: oklab(50% 0.1 -0.05) }" ".x{color:#88497e}"

(* SS 5.2.7 - color-mix() function *)
let color_mix () =
  roundtrip ".x { color: color-mix(in srgb, red, blue) }" ".x{color:purple}"

(* SS 5.3 - transparent and currentcolor keywords *)
let color_keywords () =
  (* Per CSS Color 4 section 6.4 the printer canonicalizes the fully-transparent
     color to the shortest equivalent spelling [#0000]. *)
  roundtrip ".x { color: transparent }" ".x{color:#0000}";
  (* currentColor preserves its camelCase form *)
  roundtrip ".x { color: currentColor }" ".x{color:currentColor}"

(* {2 CSS Conditional Rules Level 3} https://www.w3.org/TR/css-conditional-3/ *)

(* SS 7.1 - @media with min-width/max-width *)
let conditional_media () =
  roundtrip "@media (min-width: 768px) { .btn { display: block } }"
    "@media(min-width:768px){.btn{display:block}}";
  roundtrip "@media (max-width: 640px) { .btn { font-size: 14px } }"
    "@media(max-width:640px){.btn{font-size:14px}}";
  roundtrip
    "@media (prefers-color-scheme: dark) { body { background-color: black } }"
    "@media(prefers-color-scheme:dark){body{background-color:#000}}"

(* SS 8 - @supports with property checks *)
let conditional_supports () =
  roundtrip "@supports (display: grid) { .grid { display: grid } }"
    "@supports(display:grid){.grid{display:grid}}";
  roundtrip
    "@supports at-rule(@container) { .cq { container-type: inline-size } }"
    "@supports at-rule(@container){.cq{container-type:inline-size}}"

(* {2 CSS Syntax and Stylesheet At-rule Coverage}
   https://www.w3.org/TR/css-syntax-3/ SS 8 *)

let stylesheet_at_rules () =
  roundtrip "@charset \"UTF-8\";" "";
  roundtrip "@namespace url(http://www.w3.org/1999/xhtml);"
    "@namespace \"http://www.w3.org/1999/xhtml\";";
  roundtrip "@namespace svg url(http://www.w3.org/2000/svg);"
    "@namespace svg\"http://www.w3.org/2000/svg\";";
  roundtrip "@page :left { margin-left: 4cm; margin-right: 3cm }"
    "@page:left{margin-left:4cm;margin-right:3cm}";
  (* CSS Paged Media 3 section 4.3: [<page-selector>] permits zero or more page
     pseudo-classes after the optional page type. *)
  roundtrip "@page :first:left { margin: 1cm }" "@page:first:left{margin:1cm}";
  roundtrip "@page :blank:first { margin: .5cm }"
    "@page:blank:first{margin:.5cm}"

(* {2 CSS Cascade and Inheritance Level 4}
   https://www.w3.org/TR/css-cascade-4/ *)

(* SS 7.1 - CSS-wide keywords *)
let cascade_keywords () =
  roundtrip ".x { color: inherit }" ".x{color:inherit}";
  roundtrip ".x { color: initial }" ".x{color:initial}";
  roundtrip ".x { color: unset }" ".x{color:unset}";
  roundtrip ".x { color: revert }" ".x{color:revert}";
  roundtrip ".x { color: revert-layer }" ".x{color:revert-layer}";
  (* CSS Cascade L5 SS 7.3: CSS-wide keyword in a list is invalid; cascade drops
     the declaration like browsers do (Lightning/CSSO would preserve). Lenient
     mode here, since strict rejects per [cross_mode_pinning]. *)
  recover ".x { font-family: Arial, inherit }.y { color: red }" ".y{color:red}"
    1

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
  roundtrip
    "@import url(\"theme.css\") layer(theme) supports(display: grid) screen;"
    "@import\"theme.css\"layer(theme)supports(display:grid)screen;";
  roundtrip "@scope (.card) to (.footer) { .title { color: red } }"
    "@scope(.card)to (.footer){.title{color:red}}";
  roundtrip "@scope (.card) { .title { color: red } }"
    "@scope(.card){.title{color:red}}";
  roundtrip "@scope (:root) to (.stop, .end) { .title { color: blue } }"
    "@scope(:root)to (.end,.stop){.title{color:#00f}}";
  roundtrip "@starting-style { .dialog { opacity: 0 } }"
    "@starting-style{.dialog{opacity:0}}"

(* {2 CSS Conditional Rules / Container Queries}
   https://www.w3.org/TR/css-conditional-3/
   https://www.w3.org/TR/css-contain-3/ *)

let conditional_container () =
  roundtrip "@container card (inline-size > 30em) { .item { display: grid } }"
    "@container card (inline-size>30em){.item{display:grid}}";
  roundtrip "@container style(--variant: featured) { .card { color: red } }"
    "@container style(--variant:featured){.card{color:red}}";
  roundtrip "@container scroll-state(stuck: top) { .card { color: red } }"
    "@container scroll-state(stuck:top){.card{color:red}}";
  rejects_invalid "@container style() { .card { color: red } }";
  rejects_invalid "@container scroll-state() { .card { color: red } }"

(* {2 CSS Grid Layout Level 2}
   https://www.w3.org/TR/css-grid/#grid-template-areas-property *)

let grid_template_areas () =
  roundtrip ".x { grid-template-areas: \"nav  main\" \".    foot\" }"
    ".x{grid-template-areas:\"nav main\"\". foot\"}";
  roundtrip ".x { grid-template-areas: \".  .\" }"
    ".x{grid-template-areas:\". .\"}";
  roundtrip ".x { content: \"nav  main\" }" ".x{content:\"nav  main\"}";
  (* Grid 2 SS 7.3: each cell is `.` / `..` / <custom-ident>. `/` is a delim,
     not a name code point, so `nav/main` is not a single valid cell. *)
  rejects_invalid ".x { grid-template-areas: \"nav/main\" }";
  rejects_invalid ".x { grid-template-areas: \"nav main\" \"foot\" }";
  rejects_invalid ".x { grid-template-areas: \"a .\" \". a\" }"

(* {2 CSS Nesting Level 1} https://www.w3.org/TR/css-nesting-1/ *)

let nesting_rules () =
  roundtrip ".card { color: red; & > img { display: block } }"
    ".card{color:red;&>img{display:block}}";
  roundtrip ".card { @scope (&) to (.boundary) { & .title { color: blue } } }"
    ".card{@scope(&)to (.boundary){& .title{color:#00f}}}";
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
    "@font-face{font-family:MyFont;src:url(font.woff2)}";
  roundtrip
    "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
     format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }"
    "@font-face{font-family:Brand;src:url(brand.woff2)format(woff2);font-display:swap;unicode-range:U+25-FF}"

(* {2 CSS Animations Level 1} https://www.w3.org/TR/css-animations-1/ *)

(* SS 7 - @keyframes rule. CSS Animations 1 section 7.1: [from] / [to] are
   spec-equivalent to [0%] / [100%]. The printer canonicalizes to the shorter
   spelling per cssnano / Lightning CSS - [0%] beats [from], [to] beats
   [100%]. *)
let keyframes () =
  roundtrip "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }"
    "@keyframes slide{0%{opacity:0}to{opacity:1}}"

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
  roundtrip "p::first-line { color: blue }" "p:first-line{color:#00f}";
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

(* Minification-only canonicalizations above must not erase authored syntax
   forms in non-minified output. Whitespace is formatter-controlled, so these
   checks compare whitespace-stripped fragments. *)
let non_minified_preserves_css2_forms () =
  preserves_non_minified "@page :left { margin-left: 4cm; margin-right: 3cm }"
    [ "@page :left"; "margin-left: 4cm"; "margin-right: 3cm" ];
  preserves_non_minified "body *[lang|=\"en\"] + p:first-line { color: red }"
    [ "*[lang|=\"en\"]"; "p:first-line" ];
  preserves_non_minified "q:before { content: open-quote }"
    [ ":before"; "open-quote" ];
  preserves_non_minified "q::after { content: close-quote }"
    [ "::after"; "close-quote" ];
  preserves_non_minified "div { page-break-before: always }"
    [ "page-break-before: always" ];
  preserves_non_minified "div { page-break-after: avoid }"
    [ "page-break-after: avoid" ];
  preserves_non_minified "div { page-break-inside: avoid }"
    [ "page-break-inside: avoid" ];
  preserves_non_minified "ol li { list-style: decimal inside }"
    [ "ol li"; "list-style: decimal inside" ];
  preserves_non_minified
    "table > caption + colgroup col { visibility: collapse }"
    [ "table > caption + colgroup col"; "visibility: collapse" ]

let normal_keeps_at_rule_forms () =
  (* @import folds <url> and <string> into a bare double-quoted string per CSSOM
     serialization (the minified test on line 177 documents the same
     canonicalization). Fidelity preserves the URL, not the authored form. *)
  preserves_non_minified "@import 'legacy.css';" [ "@import \"legacy.css\"" ];
  rejects_non_minified_fragments "@import 'legacy.css';"
    [ "@import 'legacy.css'" ];
  preserves_non_minified "@import \"legacy.css\";" [ "@import \"legacy.css\"" ];
  preserves_non_minified "@import url(\"reset.css\");"
    [ "@import \"reset.css\"" ];
  rejects_non_minified_fragments "@import url(\"reset.css\");" [ "url(" ];
  preserves_non_minified "@import url(reset.css);" [ "@import \"reset.css\"" ];
  rejects_non_minified_fragments "@import url(reset.css);" [ "url(" ];
  preserves_non_minified "@namespace \"http://www.w3.org/1999/xhtml\";"
    [ "@namespace \"http://www.w3.org/1999/xhtml\"" ];
  preserves_non_minified "@namespace url(http://www.w3.org/1999/xhtml);"
    [ "@namespace url(http://www.w3.org/1999/xhtml)" ];
  rejects_non_minified_fragments "@namespace url(http://www.w3.org/1999/xhtml);"
    [ "@namespace \"http://www.w3.org/1999/xhtml\"" ];
  (* @namespace keeps the url() wrapper but strips quotes from inside it when
     the URL has no chars that would require quoting. Unlike @import, @namespace
     does not fold url() into a bare string. *)
  preserves_non_minified "@namespace url(\"http://www.w3.org/1999/xhtml\");"
    [ "@namespace url(http://www.w3.org/1999/xhtml)" ];
  rejects_non_minified_fragments
    "@namespace url(\"http://www.w3.org/1999/xhtml\");"
    [ "url(\""; "@namespace \"http://www.w3.org/1999/xhtml\"" ];
  preserves_non_minified "@namespace svg \"http://www.w3.org/2000/svg\";"
    [ "@namespace svg \"http://www.w3.org/2000/svg\"" ];
  preserves_non_minified "@namespace svg url(http://www.w3.org/2000/svg);"
    [ "@namespace svg url(http://www.w3.org/2000/svg)" ];
  rejects_non_minified_fragments
    "@namespace svg url(http://www.w3.org/2000/svg);"
    [ "@namespace svg \"http://www.w3.org/2000/svg\"" ];
  preserves_non_minified "@namespace svg url(\"http://www.w3.org/2000/svg\");"
    [ "@namespace svg url(http://www.w3.org/2000/svg)" ];
  rejects_non_minified_fragments
    "@namespace svg url(\"http://www.w3.org/2000/svg\");"
    [ "url(\""; "@namespace svg \"http://www.w3.org/2000/svg\"" ];
  preserves_non_minified "@charset \"UTF-8\";" [ "@charset \"UTF-8\"" ];
  preserves_non_minified "@media screen { .btn { color: green } }"
    [ "@media screen"; ".btn"; "color: green" ];
  preserves_non_minified "@layer base { body { margin: 0 } }"
    [ "@layer base"; "body"; "margin: 0" ];
  preserves_non_minified "@layer reset, base, components;"
    [ "@layer reset, base, components" ]

let non_minified_preserves_selector_forms () =
  preserves_non_minified ".sm\\:p-4{color:red}" [ ".sm\\:p-4" ];
  preserves_non_minified ".w-1\\/2{width:50%}" [ ".w-1\\/2" ];
  preserves_non_minified "html > body p + p { text-indent: 1em }"
    [ "html > body p + p" ];
  preserves_non_minified "h1 ~ p { color: red }" [ "h1 ~ p" ];
  preserves_non_minified "[type=\"text\"] { border: 1px solid gray }"
    [ "[type=\"text\"]"; "border: 1px solid gray" ];
  preserves_non_minified "[class~=\"warning\"] { color: red }"
    [ "[class~=\"warning\"]" ];
  preserves_non_minified "[lang|=\"en\"] { color: blue }" [ "[lang|=\"en\"]" ];
  preserves_non_minified "[href^=\"https\"] { color: green }"
    [ "[href^=\"https\"]" ];
  preserves_non_minified "[href$=\".pdf\"] { color: red }"
    [ "[href$=\".pdf\"]" ];
  preserves_non_minified "[title*=\"hello\"] { color: blue }"
    [ "[title*=\"hello\"]" ];
  preserves_non_minified ":nth-child(2n+1) { color: red }"
    [ ":nth-child(2n+1)" ];
  preserves_non_minified ":nth-child(even) { color: blue }"
    [ ":nth-child(even)" ];
  preserves_non_minified "::before { content: '' }"
    [ "::before"; "content: ''" ];
  rejects_non_minified_prefixes "::before { content: '' }" [ ":before" ];
  preserves_non_minified "::after { content: '' }" [ "::after"; "content: ''" ];
  rejects_non_minified_prefixes "::after { content: '' }" [ ":after" ];
  preserves_non_minified "::first-line { color: red }" [ "::first-line" ];
  rejects_non_minified_prefixes "::first-line { color: red }" [ ":first-line" ];
  preserves_non_minified "::first-letter { color: red }" [ "::first-letter" ];
  rejects_non_minified_prefixes "::first-letter { color: red }"
    [ ":first-letter" ];
  preserves_non_minified ":where(.a, .b) { color: red }" [ ":where(.a, .b)" ];
  preserves_non_minified ":is(.a, .b) { color: red }" [ ":is(.a, .b)" ];
  preserves_non_minified "div#main { display: flex }" [ "div#main" ];
  preserves_non_minified "a.link:hover { color: red }" [ "a.link:hover" ]

let non_minified_preserves_value_forms () =
  preserves_non_minified ".x { width: 100px }" [ "width: 100px" ];
  preserves_non_minified ".x { width: 10cm }" [ "width: 10cm" ];
  preserves_non_minified ".x { width: 1in }" [ "width: 1in" ];
  preserves_non_minified ".x { font-size: 1.5rem }" [ "font-size: 1.5rem" ];
  preserves_non_minified ".x { width: 50vw }" [ "width: 50vw" ];
  preserves_non_minified ".x { height: 100vh }" [ "height: 100vh" ];
  preserves_non_minified ".x { width: 50% }" [ "width: 50%" ];
  preserves_non_minified ".x { width: calc(100% - 2rem) }"
    [ "calc(100% - 2rem)" ];
  preserves_non_minified ".x { width: calc(2 * 3rem) }" [ "calc(2 * 3rem)" ];
  preserves_non_minified ".x { width: calc(100% - calc(2rem + 10px)) }"
    [ "calc(100% - calc(2rem + 10px))" ];
  preserves_non_minified ".x { transform: rotate(45deg) }" [ "rotate(45deg)" ];
  preserves_non_minified ".x { transform: rotate(1rad) }" [ "rotate(1rad)" ];
  preserves_non_minified ".x { transform: rotate(.5turn) }" [ "rotate(.5turn)" ];
  preserves_non_minified ".x { transition-duration: 200ms }"
    [ "transition-duration: 200ms" ];
  preserves_non_minified ".x { transition-duration: 1500ms }"
    [ "transition-duration: 1500ms" ];
  preserves_non_minified ".x { background-image: url(\"hero image.png\") }"
    [ "url(\"hero image.png\")" ];
  preserves_non_minified ".x { background-image: url(hero.png) }"
    [ "url(hero.png)" ];
  rejects_non_minified_fragments
    ".x { background-image: url(\"hero image.png\") }" [ "url(hero image.png)" ];
  rejects_non_minified_fragments ".x { background-image: url(hero.png) }"
    [ "url(\"hero.png\")" ];
  preserves_non_minified_exact ".x { content: \"nav  main\" }"
    [ "\"nav  main\"" ]

let non_minified_preserves_grid_forms () =
  preserves_non_minified_exact
    ".x { grid-template-areas: \"nav  main\" \".    foot\" }"
    [ "\"nav  main\""; "\".    foot\"" ]

let non_minified_preserves_color_forms () =
  (* CSSOM serialization and the minifier set canonicalize leading-zero decimals
     instead of preserving the original token spelling. Prettier is the pretty
     outlier, so use the shortest valid decimal spelling here too. *)
  preserves_non_minified ".x { color: rebeccapurple }" [ "rebeccapurple" ];
  preserves_non_minified ".x { color: #ff0000 }" [ "#ff0000" ];
  preserves_non_minified ".x { color: #f00f }" [ "#f00f" ];
  preserves_non_minified ".x { color: #ff0000ff }" [ "#ff0000ff" ];
  preserves_non_minified ".x { color: rgb(255 0 0 / 50%) }"
    [ "rgb(255 0 0 / 50%)" ];
  preserves_non_minified ".x { color: rgb(100% 0% 0%) }" [ "rgb(100% 0% 0%)" ];
  preserves_non_minified ".x { color: hsl(120 100% 50% / 50%) }"
    [ "hsl(120 100% 50% / 50%)" ];
  preserves_non_minified ".x { color: hwb(90 10% 20%) }" [ "hwb(90 10% 20%)" ];
  preserves_non_minified ".x { color: hwb(90 10% 20% / 0.25) }"
    [ "hwb(90 10% 20% / .25)" ];
  preserves_non_minified ".x { color: oklch(50% 0.2 30) }"
    [ "oklch(50% .2 30)" ];
  preserves_non_minified ".x { color: oklab(50% 0.1 -0.05) }"
    [ "oklab(50% .1 -.05)" ];
  preserves_non_minified ".x { color: color-mix(in srgb, red, blue) }"
    [ "color-mix(in srgb, red, blue)" ];
  preserves_non_minified ".x { color: attr(data-color type(<color>), red) }"
    [ "attr(data-color type(<color>), red)" ];
  preserves_non_minified ".x { width: attr(data-w px, 10px) }"
    [ "attr(data-w px, 10px)" ];
  preserves_non_minified ".x { width: attr(data-w px, calc(100% - 1rem)) }"
    [ "attr(data-w px, calc(100% - 1rem))" ];
  preserves_non_minified ".x { width: attr(data-w px, calc(10px + 0px)) }"
    [ "attr(data-w px, calc(10px + 0px))" ];
  preserves_non_minified ".x { width: attr(data-w px, var(--fallback, 10px)) }"
    [ "attr(data-w px, var(--fallback, 10px))" ];
  (* CSS Values 5 uses [raw-string] for string-valued attr(); [string] is a
     temporary Chromium compatibility alias. Keep this in a custom property so
     the token-stream fidelity is covered independently of property-specific
     support for attr() in [content]. *)
  preserves_non_minified ".x { --label: attr(data-label raw-string, \"x y\") }"
    [ "attr(data-label raw-string, \"x y\")" ];
  preserves_non_minified
    ".x { --label: attr(data-label raw-string, var(--label, \"x y\")) }"
    [ "attr(data-label raw-string, var(--label, \"x y\"))" ];
  preserves_non_minified ".x { color: transparent }" [ "transparent" ];
  preserves_non_minified ".x { color: currentColor }" [ "currentColor" ]

let fidelity_bad_css_wide_list () =
  (* Lenient recovery drops the invalid CSS-wide list decl; valid neighbors stay
     verbatim in non-minified output. *)
  recover_non_minified
    ".x { color: red; font-family: Arial, inherit; background: blue }"
    ~preserves:[ "color: red"; "background: blue" ]
    ~drops:[ "Arial, inherit" ] 1

(* Pin: spec-invalid input must Error in strict, Ok+warning in lenient. The fuzz
   suite (fuzz_declaration [assert_invalid_declaration_contract], fuzz_selector
   [assert_reject]) relies on this; collapsing the two modes broke 27 fuzz cases
   at once. Inputs are invalid per Cascade L5 SS 7.3 (CSS-wide keyword mixed
   with other values), matches-nothing forgiving lists, or Values 4 SS 10.2 /
   Values 5 SS 6.5 (unknown unit makes the dimension invalid). *)
let cross_mode_pinning () =
  let inputs =
    [
      ".x { font-family: Arial, inherit }";
      ".x { display: block revert }";
      ".x { margin: 1px inherit }";
      ":is(:future-pseudo) { color: red }";
      ":is() { color: red }";
      (* Unknown / future dimension units: Values 4 SS 10.2 and Values 5 SS 6.5
         say an unknown unit makes the typed value invalid. Strict rejects, but
         lenient keeps the token so the source isn't silently lost - the warning
         surfaces the unknown unit to the caller. *)
      ".x { width: 1unknown }";
      ".x { font-size: 16xyz }";
    ]
  in
  List.iter
    (fun css ->
      let strict = of_string ~strict:true css in
      let lenient = of_string ~strict:false css in
      (match strict with
      | Error _ -> ()
      | Ok parsed ->
          Alcotest.failf
            "strict mode accepted spec-invalid input %S -> %S. Route fidelity \
             tests through [of_string ~strict:false] + [recover] instead of \
             relaxing strict."
            css
            (to_string ~minify:true parsed.stylesheet));
      match lenient with
      | Error e ->
          Alcotest.failf "lenient mode failed to recover %S: %s" css
            (Cascade.Error.to_string e)
      | Ok { warnings = []; _ } ->
          Alcotest.failf
            "lenient mode swallowed spec-invalid input %S without warning" css
      | Ok _ -> ())
    inputs

(* Comment preservation policy. Per CSS Syntax 3 SS 4.3.2 ordinary comments are
   whitespace-equivalent tokens and can appear anywhere whitespace can, so
   serialization may drop them or replace them with whitespace. The minifier
   convention for bang comments [/*! ... */] is stronger: they are used for
   license / important headers and must survive serialization. *)
let comment_preservation_policy () =
  rejects_non_minified_fragments
    ".a { color: red } /* divider */ .b { color: blue }" [ "/* divider */" ];
  rejects_non_minified_fragments
    ".x { color: red; /* note */ background: blue }" [ "/* note */" ];
  roundtrip ".a { color: red } /* divider */ .b { color: blue }"
    ".a{color:red}.b{color:#00f}";
  roundtrip ".x { color: red; /* note */ background: blue }"
    ".x{color:red;background:#00f}";
  preserves_non_minified_exact
    ".a { color: red } /*! license */ .b { color: blue }" [ "/*! license */" ];
  (match
     of_string ~strict:true
       ".a { color: red } /*! license */ .b { color: blue }"
   with
  | Ok parsed ->
      let output =
        parsed.stylesheet
        |> optimize ~scope:`Stylesheet ~enforce_spec:true
        |> to_string ~minify:true
      in
      let find fragment = Astring.String.find_sub ~sub:fragment output in
      Alcotest.(check bool)
        "minified output keeps first rule" true
        (contains_substring ~needle:".a{color:red}" output);
      Alcotest.(check bool)
        "minified output keeps bang comment" true
        (contains_substring ~needle:"/*! license */" output);
      Alcotest.(check bool)
        "minified output keeps second rule" true
        (contains_substring ~needle:".b{color:#00f}" output);
      Alcotest.(check bool)
        "minified output keeps authored bang-comment order" true
        (match
           (find ".a{color:red}", find "/*! license */", find ".b{color:#00f}")
         with
        | Some a, Some c, Some b -> a < c && c < b
        | _ -> false)
  | Error e -> Alcotest.fail (Cascade.Error.to_string e));
  (* Comments inside selector lists or values are discarded (industry consensus
     - none of Lightning, cssnano, CSSO preserve these). *)
  roundtrip ".a /* between */ , .b { color: red }" ".a,.b{color:red}";
  roundtrip ".x { color: rgb(/* r */ 255 0 0) }" ".x{color:red}"

let non_minified_preserves_conditional_forms () =
  preserves_non_minified "@media (min-width: 768px) { .btn { display: block } }"
    [ "(min-width: 768px)" ];
  preserves_non_minified
    "@media (max-width: 640px) { .btn { font-size: 14px } }"
    [ "(max-width: 640px)" ];
  preserves_non_minified
    "@media (prefers-color-scheme: dark) { body { background-color: black } }"
    [ "(prefers-color-scheme: dark)" ];
  preserves_non_minified
    "@container card (inline-size > 30em) { .item { display: grid } }"
    [ "@container card (inline-size > 30em)" ];
  preserves_non_minified
    "@container style(--variant: featured) { .card { color: red } }"
    [ "style(--variant: featured)" ];
  preserves_non_minified
    "@container scroll-state(stuck: top) { .card { color: red } }"
    [ "scroll-state(stuck: top)" ];
  preserves_non_minified "@supports (display: grid) { .grid { display: grid } }"
    [ "@supports (display: grid)" ];
  preserves_non_minified
    "@supports at-rule(@container) { .cq { container-type: inline-size } }"
    [ "@supports at-rule(@container)"; "container-type: inline-size" ]

let non_minified_preserves_cascade_forms () =
  preserves_non_minified
    "@import url(\"theme.css\") layer(theme) supports(display: grid) screen;"
    [ "@import url(\"theme.css\")"; "supports(display: grid)" ];
  rejects_non_minified_fragments
    "@import url(\"theme.css\") layer(theme) supports(display: grid) screen;"
    [ "@import \"theme.css\" layer(theme) supports(display: grid) screen" ];
  preserves_non_minified
    "@import url(theme.css) layer(theme) supports(display: grid) screen;"
    [ "@import url(theme.css)"; "supports(display: grid)" ];
  rejects_non_minified_fragments
    "@import url(theme.css) layer(theme) supports(display: grid) screen;"
    [ "@import \"theme.css\" layer(theme) supports(display: grid) screen" ];
  preserves_non_minified
    "@import \"theme.css\" layer(theme) supports(display: grid) screen;"
    [ "@import \"theme.css\""; "supports(display: grid)" ];
  preserves_non_minified "@scope (.card) to (.footer) { .title { color: red } }"
    [ "@scope (.card) to (.footer)" ];
  preserves_non_minified "@scope (.card) { .title { color: red } }"
    [ "@scope (.card)" ];
  preserves_non_minified "@starting-style { .dialog { opacity: 0 } }"
    [ "@starting-style"; "opacity: 0" ];
  preserves_non_minified ".x { color: revert-layer }" [ "revert-layer" ]

let normal_keeps_nesting_vars () =
  preserves_non_minified ".card { color: red; & > img { display: block } }"
    [ "& > img" ];
  preserves_non_minified
    ".card { @scope (&) to (.boundary) { & .title { color: blue } } }"
    [ "@scope (&) to (.boundary)"; "& .title" ];
  preserves_non_minified
    ".card { @media (width >= 40em) { & > img { display: block } } }"
    [ "@media (width >= 40em)"; "& > img" ];
  preserves_non_minified ":root { --primary-color: blue }"
    [ "--primary-color: blue" ];
  preserves_non_minified ".x { color: var(--primary-color) }"
    [ "var(--primary-color)" ]

let normal_keeps_font_anim_forms () =
  preserves_non_minified
    "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
     format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }"
    [
      "@font-face";
      "font-family: Brand";
      "url(\"brand.woff2\")";
      "format(\"woff2\")";
      "font-display: swap";
      "U+0025-00FF";
    ];
  rejects_non_minified_fragments
    "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
     format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }"
    [ "url(brand.woff2)" ];
  preserves_non_minified
    "@font-face { font-family: Brand; src: url(brand.woff2) format(\"woff2\") }"
    [ "url(brand.woff2)"; "format(\"woff2\")" ];
  rejects_non_minified_fragments
    "@font-face { font-family: Brand; src: url(brand.woff2) format(\"woff2\") }"
    [ "url(\"brand.woff2\")" ];
  preserves_non_minified
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }"
    [ "@keyframes slide"; "0%"; "100%" ];
  preserves_non_minified
    "@property --color { syntax: \"<color>\"; inherits: true; initial-value: \
     red }"
    [ "@property --color"; "syntax: \"<color>\""; "inherits: true" ]

let serialization_idempotent ~minify css =
  match of_string ~strict:true css with
  | Error e -> Alcotest.fail (Cascade.Error.to_string e)
  | Ok parsed -> (
      let once = to_string ~minify parsed.stylesheet in
      match of_string ~strict:true once with
      | Error e ->
          Alcotest.failf "serialized CSS did not reparse: %S\n%s" once
            (Cascade.Error.to_string e)
      | Ok reparsed ->
          let twice = to_string ~minify reparsed.Css.stylesheet in
          Alcotest.(check string)
            (Fmt.str "idempotent minify=%b %s" minify css)
            once twice)

let serialization_invariants () =
  List.iter
    (fun css ->
      serialization_idempotent ~minify:true css;
      serialization_idempotent ~minify:false css)
    [
      "@charset \"UTF-8\"; @import url(theme.css) layer(theme) \
       supports(display: grid) screen; @namespace svg \
       url(http://www.w3.org/2000/svg); svg|circle { fill: red }";
      "@layer reset, theme; @layer theme { .btn::before { content: \"\"; \
       color: rgb(255 0 0 / 50%) } }";
      "@media (min-width: 30em) { @supports (display: grid) { @container card \
       (inline-size > 40em) { .x { display: grid } } } }";
      "@scope (.card) to (.footer) { .item { color: var(--brand, color-mix(in \
       srgb, red, blue)) } }";
      "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
       format(\"woff2\"); unicode-range: U+0025-00FF }";
      "@keyframes fade { from { opacity: 0 } to { opacity: 1 } }";
      "@property --gap { syntax: \"<length>\"; inherits: false; initial-value: \
       1rem }";
    ]

let minified_shortest_spec_edges () =
  List.iter
    (fun (input, expected) -> roundtrip input expected)
    [
      ("@import url(foo.css);", "@import\"foo.css\";");
      ("@import url(foo.css) print;", "@import\"foo.css\"print;");
      ( "@import url(foo.css) layer(theme) supports(display: flex) print;",
        "@import\"foo.css\"layer(theme)supports(display:flex)print;" );
      ( "@namespace url(http://www.w3.org/1999/xhtml);",
        "@namespace \"http://www.w3.org/1999/xhtml\";" );
      ( "@namespace svg url(http://www.w3.org/2000/svg);",
        "@namespace svg\"http://www.w3.org/2000/svg\";" );
      ( "@scope (.card) { .title { color: red } }",
        "@scope(.card){.title{color:red}}" );
      ( "@scope (.card) to (.footer, .aside) { .title { color: blue } }",
        "@scope(.card)to (.aside,.footer){.title{color:#00f}}" );
      ( ".card { @scope (&) to (.boundary) { & .title { color: blue } } }",
        ".card{@scope(&)to (.boundary){& .title{color:#00f}}}" );
      ("::before { content: '' }", ":before{content:\"\"}");
      ("::after { content: '' }", ":after{content:\"\"}");
      ("::first-line { color: blue }", ":first-line{color:#00f}");
      ("::first-letter { color: blue }", ":first-letter{color:#00f}");
      (".x { color: #ff0000ff }", ".x{color:red}");
      (".x { color: transparent }", ".x{color:#0000}");
      (".x { transition-duration: 500ms }", ".x{transition-duration:.5s}");
      ( "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
         format(\"woff2\"); unicode-range: U+0025-00FF }",
        "@font-face{font-family:Brand;src:url(brand.woff2)format(woff2);unicode-range:U+25-FF}"
      );
    ]

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
          Alcotest.test_case "grid: template areas" `Quick grid_template_areas;
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
          Alcotest.test_case "fidelity: CSS2 forms" `Quick
            non_minified_preserves_css2_forms;
          Alcotest.test_case "fidelity: syntax and at-rule forms" `Quick
            normal_keeps_at_rule_forms;
          Alcotest.test_case "fidelity: selector forms" `Quick
            non_minified_preserves_selector_forms;
          Alcotest.test_case "fidelity: value forms" `Quick
            non_minified_preserves_value_forms;
          Alcotest.test_case "fidelity: grid forms" `Quick
            non_minified_preserves_grid_forms;
          Alcotest.test_case "fidelity: color forms" `Quick
            non_minified_preserves_color_forms;
          Alcotest.test_case "fidelity: invalid CSS-wide list forms" `Quick
            fidelity_bad_css_wide_list;
          Alcotest.test_case
            "cross-mode: strict rejects, lenient warns on spec-invalid input"
            `Quick cross_mode_pinning;
          Alcotest.test_case "comments: serialization policy" `Quick
            comment_preservation_policy;
          Alcotest.test_case "fidelity: conditional forms" `Quick
            non_minified_preserves_conditional_forms;
          Alcotest.test_case "fidelity: cascade forms" `Quick
            non_minified_preserves_cascade_forms;
          Alcotest.test_case "fidelity: nesting and variable forms" `Quick
            normal_keeps_nesting_vars;
          Alcotest.test_case "fidelity: font animation property forms" `Quick
            normal_keeps_font_anim_forms;
          Alcotest.test_case "fidelity: serialization invariants" `Quick
            serialization_invariants;
          Alcotest.test_case "minify: shortest spec edges" `Quick
            minified_shortest_spec_edges;
        ] );
    ]
