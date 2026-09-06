type row = { branch : string; input : string; expected : string }
type invalid_row = { branch : string; input : string }

let row branch input expected = { branch; input; expected }
let invalid branch input = { branch; input }

let media_positive =
  [
    row "empty media query list" "" "";
    row "boolean width" "(width)" "(width)";
    row "boolean height" "(height)" "(height)";
    row "boolean color" "(color)" "(color)";
    row "color depth" "(color: 8)" "(color: 8)";
    row "color depth range" "(color >= 8)" "(color >= 8)";
    row "color negative range syntax" "(color < 0)" "(color < 0)";
    row "boolean color index" "(color-index)" "(color-index)";
    row "color index range" "(color-index >= 256)" "(color-index >= 256)";
    row "color index negative range syntax" "(color-index < 0)"
      "(color-index < 0)";
    row "boolean monochrome" "(monochrome)" "(monochrome)";
    row "monochrome depth" "(monochrome: 1)" "(monochrome: 1)";
    row "monochrome range" "(monochrome >= 2)" "(monochrome >= 2)";
    row "monochrome negative range syntax" "(monochrome < 0)" "(monochrome < 0)";
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
    row "overflow inline scroll" "(overflow-inline: scroll)"
      "(overflow-inline: scroll)";
    row "color gamut" "(color-gamut: p3)" "(color-gamut: p3)";
    row "color gamut srgb" "(color-gamut: srgb)" "(color-gamut: srgb)";
    row "dynamic range" "(dynamic-range: high)" "(dynamic-range: high)";
    row "video dynamic range" "(video-dynamic-range: standard)"
      "(video-dynamic-range: standard)";
    row "resolution range" "(resolution >= 2dppx)" "(resolution >= 2dppx)";
    row "resolution infinite" "(resolution: infinite)" "(resolution: infinite)";
    row "scan interlace" "(scan: interlace)" "(scan: interlace)";
    row "grid device" "(grid: 0)" "(grid: 0)";
    row "inverted colors" "(inverted-colors: inverted)"
      "(inverted-colors: inverted)";
    row "display mode standalone" "(display-mode: standalone)"
      "(display-mode: standalone)";
    row "display mode fullscreen" "(display-mode: fullscreen)"
      "(display-mode: fullscreen)";
    row "display mode minimal ui" "(display-mode: minimal-ui)"
      "(display-mode: minimal-ui)";
    row "display mode browser" "(display-mode: browser)"
      "(display-mode: browser)";
    row "display mode picture in picture" "(display-mode: picture-in-picture)"
      "(display-mode: picture-in-picture)";
    row "environment blending" "(environment-blending: additive)"
      "(environment-blending: additive)";
    row "environment blending opaque" "(environment-blending: opaque)"
      "(environment-blending: opaque)";
    row "environment blending subtractive" "(environment-blending: subtractive)"
      "(environment-blending: subtractive)";
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
    row "prefers contrast no preference" "(prefers-contrast: no-preference)"
      "(prefers-contrast: no-preference)";
    row "prefers contrast less" "(prefers-contrast: less)"
      "(prefers-contrast: less)";
    row "prefers contrast custom" "(prefers-contrast: custom)"
      "(prefers-contrast: custom)";
    row "prefers reduced data" "(prefers-reduced-data: reduce)"
      "(prefers-reduced-data: reduce)";
    row "prefers reduced data no preference"
      "(prefers-reduced-data: no-preference)"
      "(prefers-reduced-data: no-preference)";
    row "forced colors" "(forced-colors: active)" "(forced-colors: active)";
    row "forced colors none" "(forced-colors: none)" "(forced-colors: none)";
    row "nav controls" "(nav-controls: back)" "(nav-controls: back)";
    row "nav controls none" "(nav-controls: none)" "(nav-controls: none)";
    row "nav controls boolean" "(nav-controls)" "(nav-controls)";
    row "scripting" "(scripting: enabled)" "(scripting: enabled)";
    row "scripting initial only" "(scripting: initial-only)"
      "(scripting: initial-only)";
    row "scripting none" "(scripting: none)" "(scripting: none)";
    row "media type" "print" "print";
    row "all media type" "all" "all";
    row "screen media type" "screen" "screen";
    row "speech media type" "speech" "speech";
    row "unknown media type syntax" "unknown" "unknown";
    row "not media type" "not print" "not print";
    row "only media type with feature" "only screen and (pointer: fine)"
      "only screen and (pointer: fine)";
    row "media type with disjunction"
      "screen and ((width >= 40em) or (orientation: landscape))"
      "screen and ((width >= 40em) or (orientation: landscape))";
    row "media query list" "screen and (width >= 40em), print and (color)"
      "screen and (width >= 40em), print and (color)";
  ]

(* MQ4 section 3: these are general-enclosed, not malformed queries. *)
let media_general_enclosed =
  [
    row "empty media feature" "()" "()";
    row "min-width missing value" "(min-width)" "(min-width)";
    row "missing range value" "(width >=)" "(width >=)";
    row "empty feature value" "(width:)" "(width:)";
    row "opposing interval operators" "(30em < width > 60em)"
      "(30em < width > 60em)";
    row "double name-first comparison" "(width = 40em = 50em)"
      "(width = 40em = 50em)";
    row "incomplete interval" "(400px <= width <=)" "(400px <= width <=)";
    row "bad aspect ratio" "(aspect-ratio > 16/)" "(aspect-ratio > 16/)";
    row "bad orientation keyword" "(orientation: diagonal)"
      "(orientation: diagonal)";
    row "bad hover keyword" "(hover: sometimes)" "(hover: sometimes)";
    row "bad any-hover keyword" "(any-hover: fine)" "(any-hover: fine)";
    row "bad any-pointer keyword" "(any-pointer: hover)" "(any-pointer: hover)";
    row "bad update keyword" "(update: instant)" "(update: instant)";
    row "bad overflow-block keyword" "(overflow-block: hidden)"
      "(overflow-block: hidden)";
    row "retired overflow-block optional paged"
      "(overflow-block: optional-paged)" "(overflow-block: optional-paged)";
    row "bad overflow-inline paged" "(overflow-inline: paged)"
      "(overflow-inline: paged)";
    row "bad color-gamut keyword" "(color-gamut: rgb)" "(color-gamut: rgb)";
    row "bad dynamic-range keyword" "(dynamic-range: ultra)"
      "(dynamic-range: ultra)";
    row "bad resolution unit" "(resolution >= 2px)" "(resolution >= 2px)";
    row "bad scan keyword" "(scan: fast)" "(scan: fast)";
    row "bad grid keyword" "(grid: yes)" "(grid: yes)";
    row "bad display-mode keyword" "(display-mode: popup)"
      "(display-mode: popup)";
    row "bad environment-blending keyword" "(environment-blending: blend)"
      "(environment-blending: blend)";
    row "bad viewport segments value" "(horizontal-viewport-segments: -1)"
      "(horizontal-viewport-segments: -1)";
    row "bad color scheme keyword" "(prefers-color-scheme: sepia)"
      "(prefers-color-scheme: sepia)";
    row "bad reduced motion keyword" "(prefers-reduced-motion: yes)"
      "(prefers-reduced-motion: yes)";
    row "bad reduced transparency keyword" "(prefers-reduced-transparency: yes)"
      "(prefers-reduced-transparency: yes)";
    row "bad prefers contrast keyword" "(prefers-contrast: high)"
      "(prefers-contrast: high)";
    row "retired prefers contrast forced keyword" "(prefers-contrast: forced)"
      "(prefers-contrast: forced)";
    row "bad reduced data keyword" "(prefers-reduced-data: yes)"
      "(prefers-reduced-data: yes)";
    row "bad forced colors keyword" "(forced-colors: enabled)"
      "(forced-colors: enabled)";
    row "bad nav controls keyword" "(nav-controls: back-button)"
      "(nav-controls: back-button)";
    row "bad min prefix on discrete feature" "(min-orientation: portrait)"
      "(min-orientation: portrait)";
    row "bad color feature value" "(color: 20example)" "(color: 20example)";
  ]

let media_negative =
  [
    invalid "only before feature" "only (color)";
    invalid "missing query after not" "not";
    invalid "reserved or as media type" "or and (color)";
    invalid "bad media type condition join" "screen (width)";
    invalid "missing right operand" "(width) and";
    invalid "ungrouped mixed operators" "(width) and (height) or (color)";
    invalid "double not" "not not screen";
    invalid "unclosed media feature" "(width >= 40em";
  ]

let media_recovery =
  [
    row "unknown feature recovers inside list" "(max-weight: 3kg), (color)"
      "(max-weight: 3kg), (color)";
    row "bad bare query recovers inside list" "&test, speech" "not all, speech";
    row "trailing comma branch recovers" "(example, all,), speech"
      "(example, all,), speech";
    row "unknown max-weight recovers to next branch"
      "screen and (max-weight: 3kg) and (color), (color)"
      "screen and (max-weight: 3kg) and (color), (color)";
    row "invalid min-prefix recovers inside list"
      "(min-orientation: portrait), (width)"
      "(min-orientation: portrait), (width)";
  ]

let container_positive =
  [
    row "unknown range value" "(width >)" "(width >)";
    row "unknown interval" "(30em < inline-size > 60em)"
      "(30em < inline-size > 60em)";
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
    row "style range equality" "style(--gap = 10px)" "style(--gap = 10px)";
    row "style range name-first comparison" "style(--gap > 10px)"
      "style(--gap > 10px)";
    row "style range value-first comparison" "style(10px < --gap)"
      "style(10px < --gap)";
    row "style uppercase function" "STYLE(--theme: dark)" "STYLE(--theme: dark)";
    row "scroll-state stuck" "scroll-state(stuck: top)"
      "scroll-state(stuck: top)";
    row "scroll-state stuck logical" "scroll-state(stuck: inline-end)"
      "scroll-state(stuck: inline-end)";
    row "scroll-state stuck none" "scroll-state(stuck: none)"
      "scroll-state(stuck: none)";
    row "scroll-state snapped" "scroll-state(snapped: block)"
      "scroll-state(snapped: block)";
    row "scroll-state snapped both" "scroll-state(snapped: both)"
      "scroll-state(snapped: both)";
    row "scroll-state scrollable" "scroll-state(scrollable: inline)"
      "scroll-state(scrollable: inline)";
    row "scroll-state scrollable edge" "scroll-state(scrollable: bottom)"
      "scroll-state(scrollable: bottom)";
    row "scroll-state scrolled" "scroll-state(scrolled: block-start)"
      "scroll-state(scrolled: block-start)";
    row "scroll-state scrolled axis" "scroll-state(scrolled: y)"
      "scroll-state(scrolled: y)";
    row "scroll-state conjunction"
      "scroll-state((stuck: top) and (stuck: left))"
      "scroll-state((stuck: top) and (stuck: left))";
    row "scroll-state disjunction" "scroll-state((snapped: x) or (snapped: y))"
      "scroll-state((snapped: x) or (snapped: y))";
    row "scroll-state negation" "scroll-state(not (stuck: none))"
      "scroll-state(not (stuck: none))";
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
    invalid "bad stuck axis value" "scroll-state(stuck: x)";
    invalid "bad snapped edge value" "scroll-state(snapped: top)";
    invalid "bad scrollable value" "scroll-state(scrollable: diagonal)";
    invalid "bad scrolled value" "scroll-state(scrolled: diagonal)";
    invalid "ungrouped mixed query operators" "(width) and (height) or (color)";
    invalid "style query semicolon" "style(color: red;)";
    invalid "style query mixed operators" "style(--theme: dark) and or (width)";
    invalid "scroll-state mixed operators"
      "scroll-state((stuck: top) and (snapped: x) or (scrollable: y))";
    invalid "not not query" "not not (width)";
    invalid "equality bound in a style interval" "style(10px = --gap = 20px)";
    invalid "opposing style interval operators" "style(10px < --gap > 20px)";
  ]

let mutate_invalid (row : row) salt =
  match salt mod 5 with
  | 0 -> row.input ^ " and"
  | 1 -> "(" ^ row.input
  | 2 -> row.input ^ ")"
  | 3 -> row.input ^ " or"
  | _ -> "not not " ^ row.input
