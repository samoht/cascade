type row = { branch : string; input : string; expected : string }
type invalid_row = { branch : string; input : string }

let row branch input expected = { branch; input; expected }
let invalid branch input = { branch; input }

let media_positive =
  [
    row "boolean width" "(width)" "(width)";
    row "boolean height" "(height)" "(height)";
    row "boolean color" "(color)" "(color)";
    row "boolean monochrome" "(monochrome)" "(monochrome)";
    row "min-width prefix" "(min-width: 40em)" "(min-width: 40em)";
    row "name-first greater-than" "(width > 40em)" "(width > 40em)";
    row "name-first greater-equal" "(width >= 40em)" "(width >= 40em)";
    row "value-first less-than" "(40em < width)" "(40em < width)";
    row "interval range" "(30em <= width < 60em)" "(30em <= width < 60em)";
    row "inclusive interval range" "(400px <= width <= 1200px)"
      "(400px <= width <= 1200px)";
    row "height dynamic viewport" "(height > 50dvh)" "(height > 50dvh)";
    row "aspect ratio" "(aspect-ratio > 16/9)" "(aspect-ratio > 16/9)";
    row "orientation" "(orientation: landscape)" "(orientation: landscape)";
    row "hover" "(hover: hover)" "(hover: hover)";
    row "any hover" "(any-hover: none)" "(any-hover: none)";
    row "pointer" "(pointer: coarse)" "(pointer: coarse)";
    row "any pointer" "(any-pointer: fine)" "(any-pointer: fine)";
    row "update" "(update: fast)" "(update: fast)";
    row "overflow block" "(overflow-block: scroll)" "(overflow-block: scroll)";
    row "overflow inline" "(overflow-inline: none)" "(overflow-inline: none)";
    row "color gamut" "(color-gamut: p3)" "(color-gamut: p3)";
    row "dynamic range" "(dynamic-range: high)" "(dynamic-range: high)";
    row "video dynamic range" "(video-dynamic-range: standard)"
      "(video-dynamic-range: standard)";
    row "prefers color scheme" "(prefers-color-scheme: dark)"
      "(prefers-color-scheme: dark)";
    row "prefers reduced motion" "(prefers-reduced-motion: reduce)"
      "(prefers-reduced-motion: reduce)";
    row "prefers reduced transparency" "(prefers-reduced-transparency: reduce)"
      "(prefers-reduced-transparency: reduce)";
    row "prefers contrast" "(prefers-contrast: more)" "(prefers-contrast: more)";
    row "prefers reduced data" "(prefers-reduced-data: reduce)"
      "(prefers-reduced-data: reduce)";
    row "forced colors" "(forced-colors: active)" "(forced-colors: active)";
    row "scripting" "(scripting: enabled)" "(scripting: enabled)";
    row "media type" "print" "print";
    row "not media type" "not print" "not print";
    row "only media type with feature" "only screen and (pointer: fine)"
      "only screen and (pointer: fine)";
    row "media type with disjunction"
      "screen and ((width >= 40em) or (orientation: landscape))"
      "screen and ((width >= 40em) or (orientation: landscape))";
    row "media query list" "screen and (width >= 40em), print and (color)"
      "screen and (width >= 40em), print and (color)";
  ]

let media_negative =
  [
    invalid "empty media query" "";
    invalid "empty media feature" "()";
    invalid "min-width missing value" "(min-width)";
    invalid "missing range value" "(width >=)";
    invalid "empty feature value" "(width:)";
    invalid "opposing interval operators" "(30em < width > 60em)";
    invalid "double name-first comparison" "(width = 40em = 50em)";
    invalid "incomplete interval" "(400px <= width <=)";
    invalid "bad aspect ratio" "(aspect-ratio > 16/)";
    invalid "bad orientation keyword" "(orientation: diagonal)";
    invalid "bad hover keyword" "(hover: sometimes)";
    invalid "bad any-pointer keyword" "(any-pointer: hover)";
    invalid "bad update keyword" "(update: instant)";
    invalid "bad overflow-block keyword" "(overflow-block: hidden)";
    invalid "bad color-gamut keyword" "(color-gamut: rgb)";
    invalid "bad dynamic-range keyword" "(dynamic-range: ultra)";
    invalid "bad color scheme keyword" "(prefers-color-scheme: sepia)";
    invalid "bad reduced motion keyword" "(prefers-reduced-motion: yes)";
    invalid "bad forced colors keyword" "(forced-colors: enabled)";
    invalid "missing right operand" "(width) and";
    invalid "ungrouped mixed operators" "(width) and (height) or (color)";
    invalid "missing and after media type" "screen (width)";
    invalid "double not" "not not screen";
    invalid "unclosed media feature" "(width >= 40em";
  ]

let container_positive =
  [
    row "boolean width" "(width)" "(width)";
    row "boolean height" "(height)" "(height)";
    row "boolean inline-size" "(inline-size)" "(inline-size)";
    row "inline-size lower bound" "(inline-size > 30em)" "(inline-size > 30em)";
    row "block-size range" "(block-size >= 20rem)" "(block-size >= 20rem)";
    row "width range" "(width >= 400px)" "(width >= 400px)";
    row "inclusive interval range" "(400px <= width <= 1200px)"
      "(400px <= width <= 1200px)";
    row "inline interval" "(30em <= inline-size < 60em)"
      "(30em <= inline-size < 60em)";
    row "orientation" "(orientation: portrait)" "(orientation: portrait)";
    row "aspect ratio" "(aspect-ratio > 16/9)" "(aspect-ratio > 16/9)";
    row "style declaration" "style(color: red)" "style(color: red)";
    row "style custom property boolean" "style(--theme)" "style(--theme)";
    row "style custom property value" "style(--theme: dark)"
      "style(--theme: dark)";
    row "style uppercase function" "STYLE(--theme: dark)" "STYLE(--theme: dark)";
    row "scroll-state stuck" "scroll-state(stuck: top)"
      "scroll-state(stuck: top)";
    row "scroll-state snapped" "scroll-state(snapped: block)"
      "scroll-state(snapped: block)";
    row "named size query" "card (inline-size > 30em)"
      "card (inline-size > 30em)";
    row "named style query" "card style(--variant: featured)"
      "card style(--variant: featured)";
    row "named scroll-state query" "card scroll-state(stuck: top)"
      "card scroll-state(stuck: top)";
  ]

let container_negative =
  [
    invalid "empty query" "";
    invalid "empty style query" "style()";
    invalid "unclosed style query" "style(--theme: dark";
    invalid "missing style declaration value" "style(color)";
    invalid "missing style property" "style(: red)";
    invalid "empty scroll-state query" "scroll-state()";
    invalid "unclosed scroll-state query" "scroll-state(stuck: top";
    invalid "missing scroll-state value" "scroll-state(stuck)";
    invalid "bad scroll-state value" "scroll-state(stuck: diagonal)";
    invalid "missing range value" "(width >)";
    invalid "opposing interval operators" "(30em < inline-size > 60em)";
  ]

let mutate_invalid (row : row) salt =
  match salt mod 5 with
  | 0 -> row.input ^ " and"
  | 1 -> "(" ^ row.input
  | 2 -> row.input ^ ")"
  | 3 -> row.input ^ " or"
  | _ -> "not not " ^ row.input
