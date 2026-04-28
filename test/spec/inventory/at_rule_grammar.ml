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
    row "property" "descriptor-order"
      "@property --accent{syntax:\"<color>\";inherits:true;initial-value:red}"
      "@property --accent { initial-value: red; inherits: true; syntax: \
       \"<color>\" }";
    row "property" "duplicate-descriptor"
      "@property --dup{syntax:\"*\";inherits:false}"
      "@property --dup { syntax: \"<length>\"; inherits: true; syntax: \"*\"; \
       inherits: false }";
    row "font-face" "descriptor-order"
      "@font-face \
       {font-family:Brand;src:url(brand.woff2);font-display:swap;font-weight:100 \
       900}"
      "@font-face { font-weight: 100 900; font-display: swap; src: \
       url(brand.woff2); font-family: Brand }";
    row "font-face" "metric-overrides"
      "@font-face \
       {font-family:Metrics;src:url(metrics.woff2);size-adjust:100%;ascent-override:normal;descent-override:20%;line-gap-override:0%}"
      "@font-face { font-family: Metrics; src: url(metrics.woff2); \
       size-adjust: 100%; ascent-override: normal; descent-override: 20%; \
       line-gap-override: 0%; }";
    row "page" "margin-rule"
      "@page invoice:first{size:A4;margin:1cm;@top-left{content:\"Invoice\"}}"
      "@page invoice:first { margin: 1cm; size: A4; @top-left { content: \
       \"Invoice\" } }";
    row "page" "page-margin-descriptor"
      "@page chapter:right{size:letter \
       landscape;margin:1in;@right-top{content:counter(page)}}"
      "@page chapter:right { size: letter landscape; margin: 1in; @right-top { \
       content: counter(page) } }";
    row "keyframes" "selector-list"
      "@keyframes fade{from{opacity:0}50%,100%{opacity:1}}"
      "@keyframes fade { from { opacity: 0 } 50%, 100% { opacity: 1 } }";
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
      "@media screen and (width >= 40em){.card{display:grid}}"
      "@media screen and (width >= 40em) { .card { display: grid } }";
    row "supports" "nested-rule"
      "@supports ((display:grid) and selector(:has(img))){.card{display:grid}}"
      "@supports ((display: grid) and selector(:has(img))) { .card { display: \
       grid } }";
    row "container" "style-query"
      "@container card style(--variant: featured){.card{color:red}}"
      "@container card style(--variant: featured) { .card { color: red } }";
    row "container" "scroll-state-query"
      "@container scroll-state(stuck: top){.card{color:red}}"
      "@container scroll-state(stuck: top) { .card { color: red } }";
    row "scope" "limit-selector"
      "@scope (.card) to (.boundary){.title{color:red}}"
      "@scope (.card) to (.boundary) { .title { color: red } }";
    row "starting-style" "nested-rule" "@starting-style{.dialog{opacity:0}}"
      "@starting-style { .dialog { opacity: 0 } }";
  ]

let negative =
  [
    invalid "property" "missing-initial"
      "@property --bad { syntax: \"<length>\"; inherits: false }";
    invalid "property" "invalid-initial"
      "@property --bad { syntax: \"<length>\"; inherits: false; initial-value: \
       red }";
    invalid "font-face" "nested-rule"
      "@font-face { font-family: Brand; src: url(brand.woff2); @media screen { \
       .x { color: red } } }";
    invalid "font-face" "bad-range"
      "@font-face { font-family: Brand; src: url(brand.woff2); font-weight: \
       900 100 }";
    invalid "page" "invalid-pseudo" "@page :unknown { margin: 1cm }";
    invalid "page" "invalid-margin-descriptor"
      "@page { @top-center { display: block } }";
    invalid "keyframes" "bad-percentage" "@keyframes bad { -1% { opacity: 0 } }";
    invalid "keyframes" "bad-selector-list"
      "@keyframes bad { 50%, { opacity: 1 } }";
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
    invalid "container" "empty-query" "@container () { .x { color: red } }";
    invalid "container" "empty-style-query"
      "@container style() { .x { color: red } }";
    invalid "scope" "empty-root" "@scope () { .x { color: red } }";
    invalid "scope" "empty-limit" "@scope (.x) to () { .x { color: red } }";
    invalid "starting-style" "missing-block" "@starting-style;";
  ]

let features (rows : row list) =
  rows
  |> List.map (fun (row : row) -> row.feature)
  |> List.sort_uniq String.compare
