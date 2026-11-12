(** Stylesheet tests: what strict parsing accepts and rejects. *)

open Cascade
module Selector = Css.Selector
open Stylesheet_helpers

let string_of_strict_error e = Cascade.Error.to_string e

let strict_accept name css =
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      let strict_output = minify parsed.stylesheet in
      let { Css.stylesheet; warnings; _ } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected valid %s: %s" name
              (Cascade.Error.to_string err)
      in
      Alcotest.(check int)
        ("lenient parse is warning-free for valid " ^ name)
        0 (List.length warnings);
      Alcotest.(check string)
        ("strict/lenient serialization agree for " ^ name)
        strict_output (minify stylesheet)
  | Error err ->
      Alcotest.failf "strict parser rejected valid %s: %s" name
        (string_of_strict_error err)

let strict_reject name css =
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      Alcotest.failf "strict parser accepted invalid %s: %S -> %S" name css
        (minify parsed.stylesheet)
  | Error err ->
      Alcotest.(check bool)
        ("strict error carries detail for " ^ name)
        true
        (String.length (string_of_strict_error err) > 0);
      let { Css.stylesheet; warnings; _ } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected invalid %s: %s" name
              (Cascade.Error.to_string err)
      in
      ignore (minify stylesheet : string);
      Alcotest.(check bool)
        ("lenient parse warns for " ^ name)
        true
        (List.length warnings > 0)

let font_family_descriptor_grammar () =
  strict_accept "single named @font-face family"
    "@font-face { font-family: Brand; src: url(brand.woff2) }";
  strict_accept "quoted generic word as @font-face family name"
    "@font-face { font-family: \"serif\"; src: url(serif.woff2) }";
  strict_accept "named @font-palette-values family list"
    "@font-palette-values --brand { font-family: Brand, \"Color Font\" }";
  strict_accept "quoted generic word as palette family name"
    "@font-palette-values --serif { font-family: \"serif\" }";
  strict_reject "empty @font-face font-family"
    "@font-face { font-family:; src: url(brand.woff2) }";
  strict_reject "multiple @font-face families"
    "@font-face { font-family: Brand, Other; src: url(brand.woff2) }";
  strict_reject "generic @font-face family"
    "@font-face { font-family: serif; src: url(serif.woff2) }";
  (* Sec. 2.1.1 turns the identifier away where it could be read as the generic
     keyword rather than as a name, which is where the name starts. After the
     first word it is an ordinary <custom-ident>, so [Brand serif] is a name and
     [serif Brand] is not; the font-family property already reads them that way
     and Chrome 153 reads both descriptors that way. *)
  strict_accept "generic word after the first in an unquoted @font-face family"
    "@font-face { font-family: Brand serif; src: url(brand.woff2) }";
  strict_reject "generic word heading an unquoted @font-face family"
    "@font-face { font-family: serif Brand; src: url(brand.woff2) }";
  strict_reject "CSS-wide @font-face family"
    "@font-face { font-family: inherit; src: url(brand.woff2) }";
  strict_reject "empty @font-palette-values font-family"
    "@font-palette-values --brand { font-family: }";
  strict_reject "generic @font-palette-values family"
    "@font-palette-values --serif { font-family: serif }";
  strict_reject "generic in an @font-palette-values family list"
    "@font-palette-values --brand { font-family: Brand, serif }";
  strict_accept "generic word after the first in an unquoted palette family"
    "@font-palette-values --brand { font-family: Brand serif }";
  strict_reject "generic word heading an unquoted palette family"
    "@font-palette-values --brand { font-family: serif Brand }";
  strict_reject "CSS-wide @font-palette-values family"
    "@font-palette-values --brand { font-family: inherit }"

(* CSS Properties and Values API 1 sec. 3 gives a syntax component either a
   [<syntax-type-name>] or an [<ident>] the value has to match literally, and
   sec. 5 parses the initial value against that syntax. The keyword arm is the
   one the reader answers by comparing text, and only its printing side was
   covered: a keyword syntax whose initial value matches, and one whose does
   not, are the two answers that comparison has. *)
let property_keyword_syntax_reads_its_own_value () =
  check_stylesheet
    ~expected:"@property --x{syntax:\"auto\";inherits:false;initial-value:auto}"
    "@property --x { syntax: \"auto\"; inherits: false; initial-value: auto }";
  check_stylesheet
    ~expected:
      "@property \
       --y{syntax:\"<length>|auto\";inherits:false;initial-value:auto}"
    "@property --y { syntax: \"<length> | auto\"; inherits: false; \
     initial-value: auto }";
  strict_reject "an initial value the keyword syntax does not name"
    "@property --w { syntax: \"auto\"; inherits: false; initial-value: none }"

(* CSS Conditional 3 sec. 6 writes [<supports-condition>] as [not
   <supports-in-parens> | <supports-in-parens> [ and <supports-in-parens> ]* |
   <supports-in-parens> [ or <supports-in-parens> ]*], so every operand of
   [and], [or] and [not] is parenthesised and a bare declaration is one nowhere.
   The reader carries a flag saying whether the operand it is about to read may
   be an unwrapped declaration, and only the outermost call sets it; nothing
   pinned that the operands do not. *)
let supports_operand_needs_its_parens () =
  strict_reject "unwrapped declaration on the right of and"
    "@supports (display:grid) and display:flex { a { color: red } }";
  strict_reject "unwrapped declaration on the right of or"
    "@supports (display:grid) or display:flex { a { color: red } }";
  strict_reject "unwrapped declaration under not"
    "@supports not display:grid { a { color: red } }";
  strict_reject "unwrapped declaration as the whole condition"
    "@supports display:grid { a { color: red } }";
  (* Control: the same condition with its parentheses reads. *)
  strict_accept "parenthesised operands on both sides of and"
    "@supports (display:grid) and (display:flex) { a { color: red } }"

(* A descriptor whose grammar the reader validates but whose text the AST keeps
   verbatim, so the space inside one [<integer> && <symbol>] pair survives while
   the space around the value does not. That outer white space is css-syntax-3
   declaration syntax rather than descriptor content, and only the reader's trim
   removes it: without it the pair prints back with a trailing space that no
   reader wrote and that a minified sheet has no reason to carry.
   [additive-symbols] is where it shows, since every other descriptor here is
   re-serialised from a parsed value and loses the spacing anyway. *)
let counter_style_descriptor_value_is_trimmed () =
  check_stylesheet
    ~expected:"@counter-style c{system:additive;additive-symbols:1 a}"
    "@counter-style c { system: additive; additive-symbols: 1 a }"

(* CSS Counter Styles 3 (ED) sec. 3.2 spells [<symbol>] as [<string> | <image> |
   <custom-ident>], and CSS Values 4 sec. 4.2 reserves [default] from every
   [<custom-ident>], so a symbol descriptor takes the string and refuses the
   bare ident. Blink 151 drops each of the rejections below. *)
let counter_style_symbol_reserved_default () =
  let body rest =
    "@counter-style c { system: cyclic; symbols: \"a\"; " ^ rest ^ " }"
  in
  strict_accept "quoted default as a counter-style prefix"
    (body "prefix: \"default\"");
  strict_accept "unreserved ident as a counter-style prefix" (body "prefix: a");
  strict_accept "quoted default as a counter-style symbol"
    "@counter-style c { system: cyclic; symbols: \"default\" }";
  strict_reject "reserved default as a counter-style prefix"
    (body "prefix: default");
  strict_reject "reserved default as a counter-style suffix"
    (body "suffix: default");
  strict_reject "reserved default as a counter-style negative"
    (body "negative: default");
  strict_reject "reserved default as the second counter-style negative"
    (body "negative: a default");
  strict_reject "reserved default as a counter-style pad symbol"
    (body "pad: 3 default");
  strict_reject "reserved default as a counter-style symbol"
    "@counter-style c { system: cyclic; symbols: default }";
  strict_reject "reserved default in counter-style additive-symbols"
    "@counter-style c { system: additive; additive-symbols: 3 default }"

(* CSS Animations 1 sec. 3: [<keyframes-name> = <custom-ident> | <string>], the
   two arms name the same animation, and only the ident arm excludes [none], the
   CSS-wide keywords and the [default] of CSS Values 4 sec. 4.2; the string arm
   takes all of those and excludes the empty string instead. The section
   serializes the name as an ident "unless it's a disallowed keyword, in which
   case it's serialized as a <string>", so a reserved name keeps its quotes:
   dropping them writes a rule Blink 153 refuses. Blink 153 answers every case
   below the same way. *)
let spec_keyframes_name_arms () =
  check_stylesheet "@keyframes \"default\"{0%{opacity:0}}";
  check_stylesheet "@keyframes \"none\"{0%{opacity:0}}";
  check_stylesheet "@keyframes \"initial\"{0%{opacity:0}}";
  check_stylesheet "@-webkit-keyframes \"default\"{0%{opacity:0}}";
  check_stylesheet "@-moz-keyframes \"revert-layer\"{0%{opacity:0}}";
  check_stylesheet "@keyframes slide{0%{opacity:0}}";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}}"
    "@keyframes \"slide\"{0%{opacity:0}}";
  strict_accept "quoted default as a keyframes name"
    "@keyframes \"default\" { from { opacity: 0 } }";
  strict_accept "quoted none as a keyframes name"
    "@keyframes \"none\" { from { opacity: 0 } }";
  strict_reject "reserved default as a keyframes name"
    "@keyframes default { from { opacity: 0 } }";
  strict_reject "folded reserved default as a keyframes name"
    "@keyframes Default { from { opacity: 0 } }";
  strict_reject "reserved default as a -webkit- keyframes name"
    "@-webkit-keyframes default { from { opacity: 0 } }";
  strict_reject "reserved default as a -moz- keyframes name"
    "@-moz-keyframes default { from { opacity: 0 } }";
  strict_reject "empty string as a keyframes name"
    "@keyframes \"\" { from { opacity: 0 } }";
  (* Sec. 3: "the following two @keyframes rules have the same name, so the
     first will be ignored". *)
  assert_minify_and_optimize
    "@keyframes foo { from { opacity: 0 } } @keyframes \"foo\" { from { \
     opacity: 1 } }"
    ~minified:"@keyframes foo{0%{opacity:0}}@keyframes foo{0%{opacity:1}}"
    ~optimized:"@keyframes foo{0%{opacity:1}}";
  (* An animation named through the string arm is the one [animation-name]
     reaches through the same arm. *)
  assert_minify_and_optimize
    "@keyframes \"default\" { from { opacity: 0 } } .a { animation-name: \
     \"default\" }"
    ~minified:
      "@keyframes \"default\"{0%{opacity:0}}.a{animation-name:\"default\"}"
    ~optimized:
      "@keyframes \"default\"{0%{opacity:0}}.a{animation-name:\"default\"}"

let spec_strict_accepts_valid_stylesheets () =
  List.iter
    (fun (name, css) -> strict_accept name css)
    [
      ("empty stylesheet", "");
      ("whitespace-only stylesheet", "   \n\t  ");
      ( "top-level CDO and CDC ignored",
        "<!-- .x { color: red } --> .y { color: blue }" );
      ("charset first", "@charset \"UTF-8\"; .x { color: red }");
      ("simple qualified rule", ".btn { color: red; }");
      ("empty qualified rule", ".empty { }");
      ("multiple qualified rules", ".btn { color: red } .card { margin: 10px }");
      ( "comments between declarations",
        ".x { color: red /* keep */ ; margin: 0 }" );
      ("four-value margin shorthand", ".x { margin: 1px 2px 3px 4px }");
      ( "font shorthand with line-height and family list",
        ".x { font: italic small-caps 700 1rem/1.5 \"Brand\", sans-serif }" );
      ("unknown vendor-prefixed declaration", ".x { -webkit-line-clamp: 2 }");
      ( "unknown future declaration",
        ".x { view-transition-class: card primary }" );
      ( "custom property token stream",
        ".x { --token-list: [a, b] (c) { d: e } }" );
      ("custom property digit dashed-ident", ".x { font-size: var(--1A202C) }");
      ("valid escape in string", ".x { content: '\\gggg' }");
      ("out-of-range rgb channels clamp", ".x { color: rgb(300, 300, 300) }");
      ("mixed numeric rgb channels", ".x { color: rgb(50%, 100, 50%) }");
      ( "transparent currentColor color-mix",
        ".x { color: color-mix(in srgb, currentColor 25%, transparent) }" );
      ( "hsl color-mix hue interpolation",
        ".x { color: color-mix(in hsl longer hue, red, blue) }" );
      ( "relative color syntax",
        ".x { color: rgb(from var(--brand) r g b / .5) }" );
      ( "typed attr fallback with calc",
        ".x { width: attr(data-w px, calc(10px + 0px)) }" );
      ( "rectangular grid template areas",
        ".x { grid-template-areas: \"head head\" \"nav main\" }" );
      ("escaped selector identifiers", ".\\31 0\\% { color: red }");
      ( "selector list with where and has",
        ".card:where(:has(> img), .featured) { color: red }" );
      ( "forgiving selector list keeps valid branch",
        ".x:is(.a,:future) { color: red }" );
      ( "not selector list",
        ".x:not(.disabled, [aria-disabled=\"true\"]) { color: red }" );
      ( "nth-child of selector list",
        ".x:nth-child(2n+1 of .item, :not(.skip)) { color: red }" );
      ("nested style rule", ".card { color: red; &:hover { color: blue } }");
      ( "nested conditional style rule",
        ".card { @media (width >= 30em) { color: red } }" );
      ( "combined supports and media import",
        "@import url(theme.css) supports(display: grid) screen;" );
      ( "anonymous import layer with supports and media",
        "@import url(theme.css) layer() supports((display: grid)) screen and \
         (color);" );
      ( "layer statement before import",
        "@layer reset, theme; @import url(theme.css) layer(theme);" );
      ("anonymous layer block", "@layer { .anonymous { color: red } }");
      ( "nested layer block",
        "@layer framework { @layer theme { .title { color: red } } }" );
      ( "charset import namespace prelude order",
        "@charset \"UTF-8\"; @import url(base.css); @namespace svg \
         url(http://www.w3.org/2000/svg); svg|a { fill: currentColor }" );
      ( "namespace before qualified rule",
        "@namespace svg url(http://www.w3.org/2000/svg); svg|a { color: red }"
      );
      ( "namespaced universal selector",
        "@namespace svg url(http://www.w3.org/2000/svg); *|a { color: red }" );
      ( "media not with feature",
        "@media not screen and (color) { .x { color: red } }" );
      ( "media range interval",
        "@media (400px <= width <= 1000px) { .x { color: red } }" );
      ( "media query list",
        "@media only screen and (hover: hover), print and (color) { .x { \
         color: red } }" );
      ( "supports font-tech",
        "@supports font-tech(variations) { .x { font-variation-settings: \
         \"wght\" 700 } }" );
      ( "supports grouped boolean logic",
        "@supports ((display: grid) or (display: flex)) and (not (display: \
         subgrid)) { .x { display: grid } }" );
      ( "container named style query",
        "@container card style(--variant: featured) { .x { display: grid } }" );
      ( "container range query",
        "@container sidebar (inline-size > 30em) { .x { display: grid } }" );
      (* Paged Media 3 SS 3.1 permits multiple page pseudo-classes; compound
         vectors are covered by the page descriptor matrix. *)
      ("empty page margin box", "@page { @top-center { } }");
      ( "scope with end boundary",
        "@scope (.card) to (.footer) { .title { color: red } }" );
      (* CSS Cascade 6 sec. 3.5.2 gives [@scope] a [<block-contents>] body, so a
         declaration written straight into it applies to the scoping root.
         Chrome 153 reports one as a [CSSNestedDeclarations] inside the
         [CSSScopeRule]; cascade used to read the body as rules only and drop
         the whole at-rule. *)
      ("scope with a bare declaration", "@scope (.s) { color: green }");
      ( "scope mixing a declaration and a rule",
        "@scope (.s) { color: green; .a { color: red } }" );
      ( "font-face wildcard unicode range",
        "@font-face { font-family: Icons; src: url(icons.woff2); \
         unicode-range: U+4?? }" );
      (* CSS Fonts 4 sec. 4.4 makes a descending descriptor range well defined
         rather than an error: the user agent swaps the two endpoints. Only
         [unicode-range] keeps an ordering rule. *)
      ( "font-face descending font-weight range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-weight: \
         900 100 }" );
      ( "font-face descending oblique angle range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-style: \
         oblique 20deg 10deg }" );
      ( "font-face descending font-stretch range",
        "@font-face { font-family: Brand; src: url(font.woff2); font-stretch: \
         200% 50% }" );
      ( "counter-style cyclic",
        "@counter-style thumbs { system: cyclic; symbols: \"*\" \"x\"; suffix: \
         \" \" }" );
      (* CSS Counter Styles 3 sec. 3.1.1: without the symbols a cyclic system
         "does not define a counter style (but is still a valid rule)", and sec.
         3 keeps a rule whose descriptors a UA does not implement, so a missing
         descriptor is not a parse error. *)
      ( "counter-style missing system",
        "@counter-style thumbs { symbols: \"*\" }" );
      ( "counter-style cyclic missing symbols",
        "@counter-style thumbs { system: cyclic }" );
      ( "counter-style additive missing additive-symbols",
        "@counter-style thumbs { system: additive }" );
    ]

let spec_strict_rejects_invalid_stylesheets () =
  List.iter
    (fun (name, css) -> strict_reject name css)
    [
      (* CSS Syntax and stylesheet grammar. *)
      ("top-level bare block", "{ color: red }");
      ("stray top-level right brace", ".x { color: red } }");
      ("unterminated block repaired at EOF", ".x { color: red");
      ( "malformed charset missing semicolon",
        "@charset \"UTF-8\" .x { color: red }" );
      ("charset must use string token", "@charset url(UTF-8);");
      ("late charset", ".x { color: red } @charset \"UTF-8\";");
      ( "late import after qualified rule",
        ".x { color: red } @import url(late.css);" );
      ( "namespace after qualified rule",
        ".x { color: red } @namespace svg url(http://www.w3.org/2000/svg);" );
      ( "namespace before import",
        "@namespace svg url(http://www.w3.org/2000/svg); @import url(late.css);"
      );
      ("import inside style rule", ".x { @import url(inner.css); color: red }");
      (* CSS Nesting 1 sec. 3.3: an at-rule whose body holds no style rule does
         not nest, so writing one inside a style rule is invalid. *)
      ( "font-face inside style rule",
        ".x { color: red; @font-face { font-family: F; src: url(f.woff2) } }" );
      ( "keyframes inside style rule",
        ".x { color: red; @keyframes k { to { opacity: 1 } } }" );
      ( "webkit keyframes inside style rule",
        ".x { color: red; @-webkit-keyframes k { to { opacity: 1 } } }" );
      ( "moz keyframes inside style rule",
        ".x { color: red; @-moz-keyframes k { to { opacity: 1 } } }" );
      ( "property inside style rule",
        ".x { color: red; @property --p { syntax: \"*\"; inherits: false } }" );
      ("page inside style rule", ".x { color: red; @page { margin: 1cm } }");
      ( "counter-style inside style rule",
        ".x { color: red; @counter-style c { system: cyclic; symbols: \"x\" } }"
      );
      ( "position-try inside style rule",
        ".x { color: red; @position-try --t { top: 1px } }" );
      ( "font-palette-values inside style rule",
        ".x { color: red; @font-palette-values --v { font-family: F; \
         base-palette: 0 } }" );
      ( "font-feature-values inside style rule",
        ".x { color: red; @font-feature-values F { @styleset { s: 1 } } }" );
      ( "viewport inside style rule",
        ".x { color: red; @viewport { width: 1px } }" );
      ( "ms-viewport inside style rule",
        ".x { color: red; @-ms-viewport { width: 1px } }" );
      ( "supports-condition inside style rule",
        ".x { color: red; @supports-condition (color: red) { color: green } }"
      );
      (* CSS Conditional 5 sec. 4: an @else needs a preceding @when here too. *)
      ( "orphan else inside style rule",
        ".x { color: red; @else { color: green } }" );
      ( "orphan else inside a nested group rule",
        ".x { @media screen { color: red; @else { color: green } } }" );
      ("import inside media rule", "@media screen { @import url(inner.css); }");
      ("import with block", "@import url(theme.css) { .x { color: red } }");
      ( "import layer after condition",
        "@import url(theme.css) screen layer(theme);" );
      ( "import duplicate layer condition",
        "@import url(theme.css) layer(theme) layer(base);" );
      ( "import supports after media condition",
        "@import url(theme.css) screen supports(display: grid);" );
      ("invalid layer list comma", "@layer reset,,base;");
      ("invalid css-wide layer name", "@layer initial { .x { color: red } }");
      ("qualified rule without block", ".x");
      ("declaration missing colon", ".x { color red }");
      ("declaration missing property", ".x { : red }");
      (* Selectors. *)
      ("empty class selector", ". { color: red }");
      ("empty id selector", "# { color: red }");
      ("invalid attribute operator", ".x[data-value ~~ test] { color: red }");
      ("empty is pseudo", ".x:is() { color: red }");
      ("empty has pseudo", ".x:has() { color: red }");
      ("invalid has relative selector", ".x:has(>) { color: red }");
      ("empty functional pseudo", ".x:not() { color: red }");
      ("invalid nth-child argument", ".x:nth-child(foo) { color: red }");
      ("invalid nth-child of selector list", ".x:nth-child(2 of) { color: red }");
      ("invalid nth-child An+B", ".x:nth-child(2n+ of .item) { color: red }");
      (* Cascade and declaration grammar. *)
      ("css-wide keyword mixed with ordinary value", ".x { color: inherit red }");
      ("all shorthand non-css-wide value", ".x { all: auto }");
      ( "duplicate important annotation",
        ".x { color: red !important !important }" );
      ("misspelled important annotation", ".x { color: red !importent }");
      ("single-hyphen custom property", ".x { -custom: value }");
      ("custom property empty name", ".x { --: value }");
      (* Values and property grammars. *)
      ("width missing unit", ".x { width: 10 }");
      ("unknown length unit", ".x { width: 10pp }");
      ("margin too many components", ".x { margin: 1px 2px 3px 4px 5px }");
      ("negative padding", ".x { padding: -1px }");
      ("negative line-height", ".x { line-height: -1 }");
      ("negative animation duration", ".x { animation-duration: -1s }");
      ("negative transition duration", ".x { transition-duration: -1s }");
      ("font-weight outside range", ".x { font-weight: 0 }");
      ("integer property rejects number", ".x { order: 1.5 }");
      ("font-family trailing comma", ".x { font-family: Brand, }");
      ("z-index rejects length", ".x { z-index: 1px }");
      ("invalid aspect-ratio", ".x { aspect-ratio: 16 / }");
      ("invalid calc operator", ".x { width: calc(1px + ) }");
      ( "invalid calc dimensional multiplication",
        ".x { width: calc(1px * 2px) }" );
      ("invalid min function empty", ".x { width: min() }");
      ("invalid clamp arity", ".x { width: clamp(1px, 2px) }");
      ("invalid short hex color", ".x { color: #12 }");
      ("invalid color function arity", ".x { color: rgb(255 0) }");
      ("mixed legacy and modern rgb syntax", ".x { color: rgb(255 0 0, .5) }");
      ("invalid hsl arity", ".x { color: hsl(0 100%) }");
      ("invalid lab channel", ".x { color: lab(50% 20) }");
      ( "invalid color-mix hue keyword",
        ".x { color: color-mix(in hsl specified hue, red, blue) }" );
      ("invalid transform angle", ".x { transform: rotate(45) }");
      ("invalid translate length", ".x { translate: 10 }");
      ("invalid scale arity", ".x { scale: 1 2 3 4 }");
      ("invalid filter function", ".x { filter: blur(red) }");
      ( "invalid grid area row shape",
        ".x { grid-template-areas: \"a a\" \"a b\" }" );
      ( "invalid grid area row width mismatch",
        ".x { grid-template-areas: \"a\" \"a b\" }" );
      (* At-rule descriptor grammars. *)
      ( "property missing syntax descriptor",
        "@property --x { inherits: false; initial-value: 0px }" );
      ( "property angle initial-value requires angle unit",
        "@property --x { syntax: \"<angle>\"; inherits: false; initial-value: \
         0 }" );
      ( "property angle-list initial-value requires angle unit",
        "@property --x { syntax: \"<angle>#\"; inherits: false; initial-value: \
         0 }" );
      ( "property invalid inherits descriptor",
        "@property --x { syntax: \"<length>\"; inherits: maybe; initial-value: \
         0px }" );
      ( "property initial-value rejects css-wide keyword",
        "@property --x { syntax: \"<length>\"; inherits: false; initial-value: \
         inherit }" );
      ( "font-face unicode-range out of range",
        "@font-face { font-family: Brand; src: url(font.woff2); unicode-range: \
         U+110000 }" );
      ( "font-face unicode-range descending range",
        "@font-face { font-family: Brand; src: url(font.woff2); unicode-range: \
         U+20-10 }" );
      ( "font-face invalid font-display list",
        "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
         block swap }" );
      ( "font-palette missing font-family",
        "@font-palette-values --brand { override-colors: 0 red }" );
      ("page margin invalid declaration", "@page { @top-left { display: 1px } }");
      ("keyframes invalid selector", "@keyframes fade { 50px { opacity: 0 } }");
      ("keyframes forbidden name none", "@keyframes none { to { opacity: 1 } }");
      ( "keyframes forbidden css-wide name",
        "@keyframes initial { to { opacity: 1 } }" );
      (* Conditional query grammars. *)
      (* CSS Cascade 6 sec. 3.5.2 gives each [@scope] bound a complex selector
         list, so a bound that does not parse leaves the prelude invalid. It
         used to become the [:invalid] pseudo-class, scoping the block to
         whatever form controls were in an invalid state. *)
      ("scope unparseable start bound", "@scope ) { .a { color: green } }");
      ("scope unparseable end bound", "@scope (.s) to ) { .a { color: green } }");
      (* The single-name shortcut in [Selector.of_string] read the whole bound
         as an element name. CSS Syntax 3 (ED) sec. 4.3.11 builds a name from
         ident code points, and a [}] is not one, so the bound names nothing. *)
      ("scope bound that is not a name", "@scope (}) { .a { color: green } }");
      ( "media ungrouped mixed boolean operators",
        "@media (width) and (height) or (color) { .x { color: red } }" );
      ("media dangling not", "@media not { .x { color: red } }");
      ("media dangling and", "@media screen and { .x { color: red } }");
      ("container empty style query", "@container style() { .x { color: red } }");
      ( "container empty scroll-state query",
        "@container scroll-state() { .x { color: red } }" );
      ("supports dangling not", "@supports not { .x { color: red } }");
      ( "supports dangling operator",
        "@supports (display: grid) and { .x { color: red } }" );
      ( "supports mixed operators without grouping",
        "@supports (display: grid) and (color: red) or (width: 1px) { .x { \
         color: red } }" );
      ("scope invalid empty root", "@scope () { .x { color: red } }");
      ( "scope invalid empty end boundary",
        "@scope (.x) to () { .x { color: red } }" );
    ]

let suite =
  ( "stylesheet_strict",
    [
      ("font-family descriptor grammar", `Quick, font_family_descriptor_grammar);
      ( "property keyword syntax reads its own value",
        `Quick,
        property_keyword_syntax_reads_its_own_value );
      ( "supports operand needs its parens",
        `Quick,
        supports_operand_needs_its_parens );
      ( "counter-style descriptor value is trimmed",
        `Quick,
        counter_style_descriptor_value_is_trimmed );
      ( "counter-style symbol reserves default",
        `Quick,
        counter_style_symbol_reserved_default );
      ("spec keyframes name arms", `Quick, spec_keyframes_name_arms);
      ( "spec strict accepts valid stylesheets",
        `Quick,
        spec_strict_accepts_valid_stylesheets );
      ( "spec strict rejects invalid stylesheets",
        `Quick,
        spec_strict_rejects_invalid_stylesheets );
    ] )
