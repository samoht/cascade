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
    row "hover none" "(hover: none)" "(hover: none)";
    row "any hover" "(any-hover: none)" "(any-hover: none)";
    row "any hover hover" "(any-hover: hover)" "(any-hover: hover)";
    row "pointer" "(pointer: coarse)" "(pointer: coarse)";
    row "pointer none" "(pointer: none)" "(pointer: none)";
    row "any pointer" "(any-pointer: fine)" "(any-pointer: fine)";
    row "any pointer coarse" "(any-pointer: coarse)" "(any-pointer: coarse)";
    row "update" "(update: fast)" "(update: fast)";
    row "update slow" "(update: slow)" "(update: slow)";
    row "update none" "(update: none)" "(update: none)";
    row "overflow block" "(overflow-block: scroll)" "(overflow-block: scroll)";
    row "overflow block paged" "(overflow-block: paged)"
      "(overflow-block: paged)";
    row "overflow inline" "(overflow-inline: none)" "(overflow-inline: none)";
    row "color gamut" "(color-gamut: p3)" "(color-gamut: p3)";
    row "color gamut srgb" "(color-gamut: srgb)" "(color-gamut: srgb)";
    row "dynamic range" "(dynamic-range: high)" "(dynamic-range: high)";
    row "video dynamic range" "(video-dynamic-range: standard)"
      "(video-dynamic-range: standard)";
    row "resolution range" "(resolution >= 2dppx)" "(resolution >= 2dppx)";
    row "scan interlace" "(scan: interlace)" "(scan: interlace)";
    row "grid device" "(grid: 0)" "(grid: 0)";
    row "inverted colors" "(inverted-colors: inverted)"
      "(inverted-colors: inverted)";
    row "display mode standalone" "(display-mode: standalone)"
      "(display-mode: standalone)";
    row "environment blending" "(environment-blending: additive)"
      "(environment-blending: additive)";
    row "video color gamut" "(video-color-gamut: rec2020)"
      "(video-color-gamut: rec2020)";
    row "horizontal viewport segments" "(horizontal-viewport-segments: 2)"
      "(horizontal-viewport-segments: 2)";
    row "vertical viewport segments range" "(vertical-viewport-segments >= 1)"
      "(vertical-viewport-segments >= 1)";
    row "prefers color scheme" "(prefers-color-scheme: dark)"
      "(prefers-color-scheme: dark)";
    row "prefers color scheme light" "(prefers-color-scheme: light)"
      "(prefers-color-scheme: light)";
    row "prefers reduced motion" "(prefers-reduced-motion: reduce)"
      "(prefers-reduced-motion: reduce)";
    row "prefers reduced motion no preference"
      "(prefers-reduced-motion: no-preference)"
      "(prefers-reduced-motion: no-preference)";
    row "prefers reduced transparency" "(prefers-reduced-transparency: reduce)"
      "(prefers-reduced-transparency: reduce)";
    row "prefers reduced transparency no preference"
      "(prefers-reduced-transparency: no-preference)"
      "(prefers-reduced-transparency: no-preference)";
    row "prefers contrast" "(prefers-contrast: more)" "(prefers-contrast: more)";
    row "prefers contrast less" "(prefers-contrast: less)"
      "(prefers-contrast: less)";
    row "prefers contrast forced" "(prefers-contrast: forced)"
      "(prefers-contrast: forced)";
    row "prefers contrast custom" "(prefers-contrast: custom)"
      "(prefers-contrast: custom)";
    row "prefers reduced data" "(prefers-reduced-data: reduce)"
      "(prefers-reduced-data: reduce)";
    row "prefers reduced data no preference"
      "(prefers-reduced-data: no-preference)"
      "(prefers-reduced-data: no-preference)";
    row "forced colors" "(forced-colors: active)" "(forced-colors: active)";
    row "forced colors none" "(forced-colors: none)" "(forced-colors: none)";
    row "scripting" "(scripting: enabled)" "(scripting: enabled)";
    row "scripting initial only" "(scripting: initial-only)"
      "(scripting: initial-only)";
    row "scripting none" "(scripting: none)" "(scripting: none)";
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
    invalid "bad any-hover keyword" "(any-hover: fine)";
    invalid "bad any-pointer keyword" "(any-pointer: hover)";
    invalid "bad update keyword" "(update: instant)";
    invalid "bad overflow-block keyword" "(overflow-block: hidden)";
    invalid "bad color-gamut keyword" "(color-gamut: rgb)";
    invalid "bad dynamic-range keyword" "(dynamic-range: ultra)";
    invalid "bad resolution unit" "(resolution >= 2px)";
    invalid "bad scan keyword" "(scan: fast)";
    invalid "bad grid keyword" "(grid: yes)";
    invalid "bad display-mode keyword" "(display-mode: popup)";
    invalid "bad environment-blending keyword" "(environment-blending: blend)";
    invalid "bad viewport segments value" "(horizontal-viewport-segments: -1)";
    invalid "bad color scheme keyword" "(prefers-color-scheme: sepia)";
    invalid "bad reduced motion keyword" "(prefers-reduced-motion: yes)";
    invalid "bad reduced transparency keyword"
      "(prefers-reduced-transparency: yes)";
    invalid "bad prefers contrast keyword" "(prefers-contrast: high)";
    invalid "bad reduced data keyword" "(prefers-reduced-data: yes)";
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
    row "style range" "style(10px <= --gap < 20px)"
      "style(10px <= --gap < 20px)";
    row "style uppercase function" "STYLE(--theme: dark)" "STYLE(--theme: dark)";
    row "scroll-state stuck" "scroll-state(stuck: top)"
      "scroll-state(stuck: top)";
    row "scroll-state snapped" "scroll-state(snapped: block)"
      "scroll-state(snapped: block)";
    row "scroll-state scrollable" "scroll-state(scrollable: inline)"
      "scroll-state(scrollable: inline)";
    row "scroll-state scrolled" "scroll-state(scrolled: block-start)"
      "scroll-state(scrolled: block-start)";
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
    invalid "bad scrollable value" "scroll-state(scrollable: diagonal)";
    invalid "bad scrolled value" "scroll-state(scrolled: diagonal)";
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
