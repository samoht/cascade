type row = {
  feature : string;
  branch : string;
  input : string;
  expected : string;
}

type invalid_row = { feature : string; branch : string; input : string }

let row feature branch expected input = { feature; branch; input; expected }
let invalid feature branch input = { feature; branch; input }

let positive =
  [
    row "supports" "general-enclosed-font-format"
      "@supports font-format(\"woff2\"){.x{color:red}}"
      "@supports font-format(\"woff2\") { .x { color: red } }";
    row "container" "general-enclosed-empty" "@container(){.x{color:red}}"
      "@container () { .x { color: red } }";
    row "media" "general-enclosed-interval"
      "@media(30em < width > 60em){.x{color:red}}"
      "@media (30em < width > 60em) { .x { color: red } }";
    row "property" "descriptor-order"
      "@property --accent{syntax:\"<color>\";inherits:true;initial-value:red}"
      "@property --accent { initial-value: red; inherits: true; syntax: \
       \"<color>\" }";
    row "property" "duplicate-descriptor"
      "@property --dup{syntax:\"*\";inherits:false}"
      "@property --dup { syntax: \"<length>\"; inherits: true; syntax: \"*\"; \
       inherits: false }";
    row "import" "layer-supports-media"
      "@import\"layout.css\"layer(framework.component)supports(display:grid)screen \
       and (width>=40em);"
      "@import url(layout.css) layer(framework.component) supports(display: \
       grid) screen and (width >= 40em);";
    row "namespace" "prefixed-url"
      "@namespace svg\"http://www.w3.org/2000/svg\";"
      "@namespace svg url(http://www.w3.org/2000/svg);";
    row "layer" "statement-order" "@layer reset,theme,components;"
      "@layer reset, theme, components;";
    row "layer" "anonymous-block" "@layer{.private{color:red}}"
      "@layer { .private { color: red } }";
    row "font-face" "descriptor-order"
      "@font-face{font-weight:100 \
       900;font-display:swap;src:url(brand.woff2);font-family:Brand}"
      "@font-face { font-weight: 100 900; font-display: swap; src: \
       url(brand.woff2); font-family: Brand }";
    row "font-face" "metric-overrides"
      "@font-face{font-family:Metrics;src:url(metrics.woff2);size-adjust:100%;ascent-override:normal;descent-override:20%;line-gap-override:0%}"
      "@font-face { font-family: Metrics; src: url(metrics.woff2); \
       size-adjust: 100%; ascent-override: normal; descent-override: 20%; \
       line-gap-override: 0%; }";
    row "page" "margin-rule"
      "@page invoice:first{margin:1cm;size:A4;@top-left{content:\"Invoice\"}}"
      "@page invoice:first { margin: 1cm; size: A4; @top-left { content: \
       \"Invoice\" } }";
    row "page" "page-margin-descriptor"
      "@page chapter:right{size:letter \
       landscape;margin:1in;@right-top{content:counter(page)}}"
      "@page chapter:right { size: letter landscape; margin: 1in; @right-top { \
       content: counter(page) } }";
    row "keyframes" "selector-list"
      "@keyframes fade{0%{opacity:0}50%,to{opacity:1}}"
      "@keyframes fade { from { opacity: 0 } 50%, 100% { opacity: 1 } }";
    row "keyframes" "out-of-range-block-dropped" "@keyframes bad{}"
      "@keyframes bad { -1% { opacity: 0 } 101% { opacity: 1 } }";
    row "keyframes" "invalid-selector-list-block-dropped" "@keyframes bad{}"
      "@keyframes bad { 50%, { opacity: 1 } from, 120% { opacity: 1 } 50px { \
       opacity: 1 } }";
    row "font-palette-values" "duplicate-descriptor"
      "@font-palette-values \
       --brand{font-family:Brand;base-palette:2;override-colors:0 red}"
      "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
       base-palette: 2; override-colors: 0 red }";
    row "view-transition" "duplicate-descriptor"
      "@view-transition{navigation:none}"
      "@view-transition { navigation: auto; navigation: none }";
    row "position-try" "descriptor-order"
      "@position-try --below{top:anchor(bottom);left:anchor(center)}"
      "@position-try --below { left: anchor(center); top: anchor(bottom) }";
    row "media" "nested-rule"
      "@media screen and (width>=40em){.card{display:grid}}"
      "@media screen and (width >= 40em) { .card { display: grid } }";
    row "supports" "nested-rule"
      "@supports((display:grid)and selector(:has(img))){.card{display:grid}}"
      "@supports ((display: grid) and selector(:has(img))) { .card { display: \
       grid } }";
    row "container" "style-query"
      "@container card style(--variant:featured){.card{color:red}}"
      "@container card style(--variant: featured) { .card { color: red } }";
    row "container" "scroll-state-query"
      "@container scroll-state(stuck:top){.card{color:red}}"
      "@container scroll-state(stuck: top) { .card { color: red } }";
    row "scope" "limit-selector"
      "@scope(.card)to (.boundary){.title{color:red}}"
      "@scope (.card) to (.boundary) { .title { color: red } }";
    row "starting-style" "nested-rule" "@starting-style{.dialog{opacity:0}}"
      "@starting-style { .dialog { opacity: 0 } }";
    row "when" "media-condition" "@when media(width>=40em){.card{display:grid}}"
      "@when media(width >= 40em) { .card { display: grid } }";
    row "when" "supports-condition"
      "@when supports(display:grid) and \
       media(pointer:fine){.card{display:grid}}"
      "@when supports(display: grid) and media(pointer: fine) { .card { \
       display: grid } }";
    row "else" "conditional-chain"
      "@when \
       supports(display:grid){.card{display:grid}}@else{.card{display:block}}"
      "@when supports(display: grid) { .card { display: grid } } @else { .card \
       { display: block } }";
    row "else" "conditional-branch"
      "@when media(width>=60em){.card{display:grid}}@else \
       supports(display:flex){.card{display:flex}}@else{.card{display:block}}"
      "@when media(width >= 60em) { .card { display: grid } } @else \
       supports(display: flex) { .card { display: flex } } @else { .card { \
       display: block } }";
    row "supports-condition" "named-query"
      "@supports-condition \
       --thicker-underlines{text-decoration-thickness:.2em;text-underline-offset:.3em}"
      "@supports-condition --thicker-underlines { text-decoration-thickness: \
       .2em; text-underline-offset: .3em }";
  ]

let negative =
  [
    invalid "property" "missing-initial"
      "@property --bad { syntax: \"<length>\"; inherits: false }";
    invalid "property" "invalid-initial"
      "@property --bad { syntax: \"<length>\"; inherits: false; initial-value: \
       red }";
    invalid "property" "bad-custom-property-name"
      "@property accent { syntax: \"<color>\"; inherits: true; initial-value: \
       red }";
    invalid "property" "bad-inherits-descriptor"
      "@property --bad { syntax: \"<color>\"; inherits: maybe; initial-value: \
       red }";
    invalid "import" "bad-order" "@import url(theme.css) screen layer(theme);";
    invalid "import" "missing-source" "@import layer(theme);";
    invalid "import" "duplicate-layer"
      "@import url(theme.css) layer(theme) layer(reset);";
    invalid "import" "bad-supports-condition"
      "@import url(theme.css) supports(display grid) screen;";
    invalid "namespace" "missing-url" "@namespace svg;";
    invalid "namespace" "block" "@namespace { url(http://example.test); }";
    invalid "layer" "empty-name-list" "@layer;";
    invalid "layer" "bad-name-list" "@layer theme,;";
    invalid "font-face" "nested-rule"
      "@font-face { font-family: Brand; src: url(brand.woff2); @media screen { \
       .x { color: red } } }";
    invalid "page" "invalid-pseudo" "@page :unknown { margin: 1cm }";
    invalid "page" "bad-margin-descriptor-value"
      "@page { @top-center { display: 1px } }";
    invalid "keyframes" "missing-block" "@keyframes missing-block";
    invalid "font-palette-values" "bad-name"
      "@font-palette-values brand { font-family: Brand; base-palette: 1 }";
    invalid "font-palette-values" "nested-rule"
      "@font-palette-values --brand { font-family: Brand; @media screen { .x { \
       color: red } } }";
    invalid "view-transition" "bad-descriptor"
      "@view-transition { navigation: always; }";
    invalid "view-transition" "prelude"
      "@view-transition page { navigation: auto; }";
    invalid "position-try" "bad-name" "@position-try default { top: 0; }";
    invalid "position-try" "nested-rule"
      "@position-try --fallback { @media screen { .x { color: red } } }";
    invalid "media" "bad-operator"
      "@media screen and or (width) { .x { color: red } }";
    invalid "supports" "mixed-operator"
      "@supports (display: grid) and (gap: 1rem) or (color: red) { .x { color: \
       red } }";
    invalid "container" "empty-style-query"
      "@container style() { .x { color: red } }";
    invalid "container" "bad-scroll-state-query"
      "@container scroll-state(stuck: diagonal) { .x { color: red } }";
    invalid "container" "mixed-operator"
      "@container (width) and (height) or (inline-size) { .x { color: red } }";
    invalid "scope" "empty-root" "@scope () { .x { color: red } }";
    invalid "scope" "empty-limit" "@scope (.x) to () { .x { color: red } }";
    invalid "starting-style" "missing-block" "@starting-style;";
    invalid "starting-style" "prelude"
      "@starting-style screen { .x { color: red } }";
    invalid "when" "empty-condition" "@when { .x { color: red } }";
    invalid "when" "missing-block" "@when media(width >= 40em);";
    invalid "when" "mixed-condition-operators"
      "@when media(width >= 40em) and supports(display: grid) or media(color) \
       { .x { color: red } }";
    invalid "else" "standalone" "@else { .x { color: red } }";
    invalid "else" "condition-without-previous"
      "@else supports(display: grid) { .x { color: red } }";
    invalid "else" "duplicate-condition"
      "@when media(width >= 40em) { .x { color: red } } @else \
       supports(display: grid) supports(color: red) { .x { color: blue } }";
    invalid "supports-condition" "bad-name"
      "@supports-condition thicker-underlines { color: red }";
    invalid "supports-condition" "missing-block"
      "@supports-condition --thicker-underlines;";
  ]

let features (rows : row list) =
  rows
  |> List.map (fun (row : row) -> row.feature)
  |> List.sort_uniq String.compare
