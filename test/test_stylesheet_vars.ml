(** Stylesheet tests: custom properties, [var()] and theme inlining. *)

open Cascade
module Selector = Css.Selector
open Stylesheet_helpers

(* CSS Custom Properties L1 section 2 (var()): variable references are
   late-bound through the cascade and resolved at computed-value time. The
   optimizer cannot inline a variable without a context that resolves it (a
   [theme] map keyed on custom property names), so the [var()] reference and its
   [calc()] wrapper round-trip preserved. The value-independent CSS Values 4
   sec. 10.10.1 identities ([x * 1], [x + 0], ...) still fold: they hold for
   every possible substitution because the [var()] stays inside calc()'s
   grammar, so [calc(var(--x) * 1)] shortens to [calc(var(--x))] (not to bare
   [var(--x)], which sec. 10.10 forbids without knowing the value). *)
let customprops12_inlining () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x) preserved without context" ".x{color:var(--x)}"
    (normalize ".x { color: var(--x) }");
  Alcotest.(check string)
    "var(--x, red) fallback preserved" ".x{color:var(--x,red)}"
    (normalize ".x { color: var(--x, red) }");
  Alcotest.(check string)
    "calc with var preserved" ".x{width:calc(var(--gap)*2)}"
    (normalize ".x { width: calc(var(--gap) * 2) }");
  Alcotest.(check string)
    "single var inside calc is not equivalent to bare var"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing)) }");
  Alcotest.(check string)
    "calc var times one folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) * 1) }");
  Alcotest.(check string)
    "calc one times var folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(1 * var(--spacing)) }");
  Alcotest.(check string)
    "calc var divided by one folds, keeping the var reference"
    ".x{padding:calc(var(--spacing))}"
    (normalize ".x { padding: calc(var(--spacing) / 1) }");
  Alcotest.(check string)
    "calc var plus zero keeps the typed term"
    ".x{padding:calc(var(--spacing) + 0px)}"
    (normalize ".x { padding: calc(var(--spacing) + 0px) }");
  Alcotest.(check string)
    "calc zero plus var keeps the typed term"
    ".x{padding:calc(0px + var(--spacing))}"
    (normalize ".x { padding: calc(0px + var(--spacing)) }");
  Alcotest.(check string)
    "calc var minus zero keeps the typed term"
    ".x{padding:calc(var(--spacing) - 0px)}"
    (normalize ".x { padding: calc(var(--spacing) - 0px) }");
  Alcotest.(check string)
    "calc var-free left subtree may fold before var"
    ".x{padding:calc(3px + var(--spacing))}"
    (normalize ".x { padding: calc((1px + 2px) + var(--spacing)) }");
  Alcotest.(check string)
    "calc var-free right subtree may fold after var"
    ".x{padding:calc(var(--spacing)*6)}"
    (normalize ".x { padding: calc(var(--spacing) * (2 * 3)) }");
  Alcotest.(check string)
    "calc var-free percentage subtree may fold before var"
    ".x{padding:calc(50% - var(--spacing))}"
    (normalize ".x { padding: calc((100% / 2) - var(--spacing)) }");
  Alcotest.(check string)
    "rule defining the variable preserved alongside its use"
    ":root{--brand:red}.x{color:var(--brand)}"
    (normalize ":root { --brand: red } .x { color: var(--brand) }")

(* CSS Custom Properties L1 section 3 (Using Cascading Variables): normal CSS
   minifiers do not know the computed value of [var()] references. Lightning,
   cssnano, csso, clean-css, and esbuild all preserve the wrapper and only
   minify tokens that are still syntax-local, such as color fallbacks. *)
let customprops12_runtime_vars () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "font-family var survives normal minification"
    ".font-sans{font-family:var(--font-sans)}"
    (normalize ".font-sans { font-family: var(--font-sans) }");
  Alcotest.(check string)
    "authored fallback survives normal minification"
    ".font-sans{font-family:var(--font-sans,Arial,sans-serif)}"
    (normalize ".font-sans { font-family: var(--font-sans, Arial, sans-serif) }");
  Alcotest.(check string)
    "fallback tokens still canonicalize to shortest valid CSS"
    ".accent{color:var(--brand,red)}"
    (normalize ".accent { color: var(--brand, #ff0000) }");
  Alcotest.(check string)
    "custom property definition is not substituted into its use"
    ":root{--font-sans:ui-sans-serif,system-ui,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (normalize
       ":root { --font-sans: ui-sans-serif, system-ui, sans-serif } .font-sans \
        { font-family: var(--font-sans) }");
  Alcotest.(check bool)
    "runtime var is not rewritten to a static font stack" false
    (let out =
       normalize
         ":root { --font-sans: ui-sans-serif, system-ui, sans-serif } \
          .font-sans { font-family: var(--font-sans) }"
     in
     Astring.String.is_infix
       ~affix:".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}" out)

(* CSS Custom Properties L1 section 3 only substitutes actual [var()] function
   references. Text that merely contains the characters "var(...)" is ordinary
   string or URL payload and remains outside variable inlining. *)
let customprops12_text_payload () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.empty in
  let resolve = function "brand" -> Some "red" | _ -> None in
  Alcotest.(check bool)
    "string payload is not treated as a var() reference" true
    (let out =
       parse ".x { content: \"var(--brand)\" }"
       |> Css.resolve_theme ~theme ~theme_defaults:resolve
       |> Css.to_string ~minify:true
     in
     Astring.String.is_infix ~affix:"var(--brand)" out
     && not (Astring.String.is_infix ~affix:"red" out))

(* CSS Custom Properties L1 section 3 (Resolving Dependency Cycles): a variable
   that references itself directly or indirectly is invalid at computed time. At
   syntax time the chain is preserved - the cycle detection happens in the
   cascade. *)
let customprops15_cycle () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "self-referential var preserved at syntax layer" true
    (let out = normalize ":root { --x: var(--x) }" in
     Astring.String.is_infix ~affix:"--x:var(--x)" out)

let fidelity_var_preserved () =
  pretty_preserves ".x { color: var(--brand) }" [ "var(--brand)" ];
  pretty_preserves ".x { color: var(--x, red) }" [ "var(--x, red)" ];
  pretty_preserves ".x { width: calc(var(--gap) * 2) }" [ "var(--gap)" ]

(* {2 var() fallback edges (CSS Custom Properties L1)} *)

let customprops12_empty_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x,) (empty fallback) preserved" ".x{color:var(--x,)}"
    (normalize ".x { color: var(--x,) }");
  Alcotest.(check bool)
    "var(--x, ) (whitespace fallback) parses" true
    (match Css.of_string ~strict:false ".x { color: var(--x, ) }" with
    | Ok _ -> true
    | _ -> false);
  Alcotest.(check string)
    "var(--x) (no fallback) preserved" ".x{color:var(--x)}"
    (normalize ".x { color: var(--x) }")

let customprops12_color_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, rgb(255, 0, 0)) -> var(--x, red)" ".x{color:var(--x,red)}"
    (normalize ".x { color: var(--x, rgb(255, 0, 0)) }");
  Alcotest.(check string)
    "var(--x, rgb(0, 0, 0)) -> var(--x, #000)" ".x{color:var(--x,#000)}"
    (normalize ".x { color: var(--x, rgb(0, 0, 0)) }");
  Alcotest.(check string)
    "var(--x, transparent) -> var(--x, #0000)" ".x{color:var(--x,#0000)}"
    (normalize ".x { color: var(--x, transparent) }")

let customprops12_nested_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, var(--y, red)) preserved" ".x{color:var(--x,var(--y,red))}"
    (normalize ".x { color: var(--x, var(--y, red)) }");
  Alcotest.(check string)
    "three-level var chain preserved" ".x{color:var(--x,var(--y,var(--z)))}"
    (normalize ".x { color: var(--x, var(--y, var(--z))) }")

let customprops12_calc_fallback () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var(--x, calc(1px + 2px)) holds the calc (typed boundary)"
    ".x{width:var(--x,calc(1px + 2px))}"
    (normalize ".x { width: var(--x, calc(1px + 2px)) }")

let customprops12_multi_comma () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check bool)
    "var(--x, red, blue) keeps both fallback tokens" true
    (let out = normalize ".x { color: var(--x, red, blue) }" in
     Astring.String.is_infix ~affix:"red" out
     && Astring.String.is_infix ~affix:"blue" out);
  Alcotest.(check bool)
    "font-family fallback list preserved" true
    (let out =
       normalize
         ".x { font-family: var(--font, \"Helvetica Neue\", sans-serif) }"
     in
     Astring.String.is_infix ~affix:"Helvetica Neue" out
     && Astring.String.is_infix ~affix:"sans-serif" out)

let customprops12_whitespace () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var( --x , red ) drops surrounding whitespace" ".x{color:var(--x,red)}"
    (normalize ".x { color: var( --x , red ) }")

let customprops12_case_sensitive () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--My-Var preserved with original case" ".x{color:var(--My-Var,red)}"
    (normalize ".x { color: var(--My-Var, red) }")

let customprops13_declaration () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--x: 10px preserved" ".x{--x:10px}"
    (normalize ".x { --x: 10px }");
  Alcotest.(check string)
    "--x: red blue preserved" ".x{--x:red blue}"
    (normalize ".x { --x: red blue }");
  (* CSS Syntax 3 (ED) sec. 5.5.6 discards whitespace before a declaration's
     value and again from its end, and CSS Custom Properties 1 (ED) sec. 2
     writes the [--*] family's value [<declaration-value>?], so what is left of
     a whitespace-only value is the empty value sec. 2.2 calls valid. *)
  Alcotest.(check string)
    "--x: before block close is the empty value" ".x{--x:}"
    (normalize ".x { --x: }");
  Alcotest.(check string)
    "--x: ; is the empty value" ".x{--x:}"
    (normalize ".x { --x: ; }")

let customprops13_color_keyword_case_fold () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "--c: RED folds to the canonical lower-case colour keyword" ".x{--c:red}"
    (normalize ".x { --c: RED }");
  Alcotest.(check string)
    "--c: ToMaTo folds to the canonical named colour" ".x{--c:tomato}"
    (normalize ".x { --c: ToMaTo }");
  Alcotest.(check string)
    "named colour folds inside a larger token stream"
    ".x{--c:1px solid rebeccapurple}"
    (normalize ".x { --c: 1px solid rebeccaPurple }");
  Alcotest.(check string)
    "--c: AUTO folds like the other colour-position keywords" ".x{--c:auto}"
    (normalize ".x { --c: AUTO }");
  Alcotest.(check string)
    "unrecognised idents keep their source case" ".x{--c:MyToken}"
    (normalize ".x { --c: MyToken }");
  Alcotest.(check string)
    "system colour keywords keep their canonical mixed case"
    ".x{--c:ButtonText}"
    (normalize ".x { --c: ButtonText }")

let normalize_minified css =
  match Css.of_string ~strict:false css with
  | Ok parsed -> minify parsed.stylesheet |> String.trim
  | Error _ -> Alcotest.failf "failed to parse: %s" css

(* CSS Custom Properties L1 section 2 preserves unregistered custom-property
   declarations as opaque token streams. CSS Properties and Values API 1 section
   2 lifts a custom property into a typed value only after an @property
   registration for that name. *)
(* CSS Scroll Snap 1 section 5.2: [scroll-snap-align] takes one or two
   keywords; the reader must stop at the end of the value, not consume the
   declaration's trailing semicolon as a missing second keyword. *)
let scroll_snap_align_trailing_semicolon () =
  Alcotest.(check string)
    "scroll-snap-align value followed by a semicolon parses"
    ".x{scroll-snap-align:none}"
    (normalize_minified ".x { scroll-snap-align: none; }");
  Alcotest.(check string)
    "scroll-snap-align pair followed by a semicolon parses"
    ".x{scroll-snap-align:start end}"
    (normalize_minified ".x { scroll-snap-align: start end; }")

let customprops13_unregistered_font_stack () =
  Alcotest.(check string)
    "unregistered font-stack custom property keeps string tokens"
    ".x{--font-sans:ui-sans-serif,system-ui,sans-serif,\"Segoe UI \
     Symbol\",\"Noto Color Emoji\"}"
    (normalize_minified
       ".x { --font-sans: ui-sans-serif, system-ui, sans-serif, \"Segoe UI \
        Symbol\", \"Noto Color Emoji\" }")

let customprops13_unregistered_theme_tokens () =
  Alcotest.(check string)
    "unregistered theme custom properties keep font strings and calc tokens"
    "@layer theme{:host,:root{--font-mono:ui-monospace,SFMono-Regular,\"Roboto \
     Mono\",\"Courier \
     New\",monospace;--font-sans:ui-sans-serif,system-ui,sans-serif,\"Segoe UI \
     Symbol\",\"Noto Color \
     Emoji\";--text-lg--line-height:calc(1.75/1.125);--text-sm--line-height:calc(1.25/.875)}}"
    (normalize_minified
       "@layer theme { :host, :root { --font-sans: ui-sans-serif, system-ui, \
        sans-serif, \"Segoe UI Symbol\", \"Noto Color Emoji\"; --font-mono: \
        ui-monospace, SFMono-Regular, \"Roboto Mono\", \"Courier New\", \
        monospace; --text-sm--line-height: calc(1.25 / .875); \
        --text-lg--line-height: calc(1.75 / 1.125) } }")

let customprops13_registered_numeric_calc () =
  Alcotest.(check string)
    "unregistered numeric custom property keeps token stream"
    ".x{--text-sm--line-height:calc(1.25/.875)}"
    (normalize_minified ".x { --text-sm--line-height: calc(1.25 / .875) }");
  Alcotest.(check string)
    "registered numeric custom property reduces to typed number"
    "@property \
     --text-sm--line-height{syntax:\"<number>\";inherits:true;initial-value:1}.x{--text-sm--line-height:1.42857}"
    (normalize_minified
       "@property --text-sm--line-height { syntax: \"<number>\"; inherits: \
        true; initial-value: 1 } .x { --text-sm--line-height: calc(1.25 / \
        .875) }")

let customprops13_registered_oklch_chroma () =
  Alcotest.(check string)
    "unregistered OKLCH custom property folds the colour function"
    ".x{--color-zinc-500:#71717b}"
    (normalize_minified ".x { --color-zinc-500: oklch(55.2% .016 285.938) }");
  Alcotest.(check string)
    "registered OKLCH custom property uses typed color minification"
    "@property \
     --color-zinc-500{syntax:\"<color>\";inherits:true;initial-value:#000}.x{--color-zinc-500:#71717b}"
    (normalize_minified
       "@property --color-zinc-500 { syntax: \"<color>\"; inherits: true; \
        initial-value: black } .x { --color-zinc-500: oklch(55.2% .016 \
        285.938) }");
  (* A [@property] registration is document-global regardless of source order
     (CSS Properties and Values API 1 SS 2), so a usage inside [@layer] that
     precedes its [@property] rule (the Tailwind output shape) is still promoted
     and colour-canonicalised. *)
  Alcotest.(check string)
    "registration applies to a prior usage nested in a layer"
    "@layer utilities{.x{--color-zinc-500:#71717b}}@property \
     --color-zinc-500{syntax:\"<color>\";inherits:true;initial-value:#000}"
    (normalize_minified
       "@layer utilities { .x { --color-zinc-500: oklch(55.2% .016 285.938) } \
        } @property --color-zinc-500 { syntax: \"<color>\"; inherits: true; \
        initial-value: black }")

let customprops13_registered_percent_calc () =
  Alcotest.(check string)
    "unregistered percent calc custom property keeps token stream"
    ".x{--tw-translate-x:calc(1/2*100%)}"
    (normalize_minified ".x { --tw-translate-x: calc(1 / 2 * 100%) }");
  Alcotest.(check string)
    "registered percent custom property uses typed calc minification"
    "@property \
     --tw-translate-x{syntax:\"<percentage>\";inherits:false;initial-value:0%}.x{--tw-translate-x:50%}"
    (normalize_minified
       "@property --tw-translate-x { syntax: \"<percentage>\"; inherits: \
        false; initial-value: 0% } .x { --tw-translate-x: calc(1 / 2 * 100%) }")

let customprops13_registered_negative_dimension_calc () =
  Alcotest.(check string)
    "unregistered dimension calc custom property keeps token stream"
    ".x{--tw-tracking:calc(.05em*-1)}"
    (normalize_minified ".x { --tw-tracking: calc(.05em * -1) }");
  Alcotest.(check string)
    "registered length custom property reduces to dimension"
    "@property \
     --tw-tracking{syntax:\"<length>\";inherits:false;initial-value:0}.x{--tw-tracking:-.05em}"
    (normalize_minified
       "@property --tw-tracking { syntax: \"<length>\"; inherits: false; \
        initial-value: 0px } .x { --tw-tracking: calc(.05em * -1) }")

let customprops13_registered_angle_time_calc () =
  (* <angle>/<time> custom properties fold their calc() whether or not they are
     registered: the unit unambiguously fixes the type, so a complete math
     function reduces to its dimension in every var() site (like a complete
     colour). The registered path uses the typed kind; the unregistered path
     folds the opaque substream. *)
  Alcotest.(check string)
    "unregistered angle calc custom property folds to an angle"
    ".x{--tw-rotate:0deg}"
    (normalize_minified ".x { --tw-rotate: calc(1deg * 0) }");
  Alcotest.(check string)
    "unregistered time calc custom property folds to a duration"
    ".x{--tw-delay:2s}"
    (normalize_minified ".x { --tw-delay: calc(1s * 2) }");
  Alcotest.(check string)
    "registered angle custom property reduces to an angle"
    "@property \
     --tw-rotate{syntax:\"<angle>\";inherits:false;initial-value:0deg}.x{--tw-rotate:0deg}"
    (normalize_minified
       "@property --tw-rotate { syntax: \"<angle>\"; inherits: false; \
        initial-value: 0deg } .x { --tw-rotate: calc(1deg * 0) }");
  Alcotest.(check string)
    "registered time custom property reduces to a duration"
    "@property \
     --tw-delay{syntax:\"<time>\";inherits:false;initial-value:0s}.x{--tw-delay:2s}"
    (normalize_minified
       "@property --tw-delay { syntax: \"<time>\"; inherits: false; \
        initial-value: 0s } .x { --tw-delay: calc(1s * 2) }");
  (* A percentage is ambiguous (length vs number percentage) so an unregistered
     percentage calc stays an opaque token stream. *)
  Alcotest.(check string)
    "unregistered percentage calc custom property stays opaque"
    ".x{--a:calc(50%*2)}"
    (normalize_minified ".x { --a: calc(50% * 2) }")

let customprops13_trigonometric_calc () =
  (* CSS Values 4 sec. 10.4 puts the trigonometric family among the math
     functions, so a complete one reduces in a custom-property stream the way
     the exponential family already does. The inverse functions resolve to an
     <angle>, which is unambiguous in every var() site. *)
  Alcotest.(check string)
    "unregistered arc tangent custom property folds to an angle" ".x{--v:45deg}"
    (normalize_minified ".x { --v: atan2(1, 1) }");
  Alcotest.(check string)
    "unregistered arc cosine custom property folds to an angle" ".x{--v:90deg}"
    (normalize_minified ".x { --v: acos(0) }");
  Alcotest.(check string)
    "unregistered arc sine custom property folds to an angle" ".x{--v:90deg}"
    (normalize_minified ".x { --v: asin(1) }")

let customprops13_shortest_unresolved_calc_spacing () =
  Alcotest.(check string)
    "unregistered unresolved calc custom property keeps token stream"
    ".x{--tw-border-spacing-x:calc(var(--spacing)*4)}"
    (normalize_minified ".x { --tw-border-spacing-x: calc(var(--spacing) * 4) }")

let customprops13_box_shadow_zero_spread () =
  Alcotest.(check string)
    "unregistered box-shadow custom property keeps token stream"
    ".shadow-sm{--tw-shadow:0 1px 3px 0 var(--tw-shadow-color,#0000001a)}"
    (normalize_minified
       ".shadow-sm { --tw-shadow: 0 1px 3px 0 var(--tw-shadow-color, \
        #0000001a) }")

let customprops13_hex_color_folds () =
  Alcotest.(check string)
    "unregistered hex custom property folds to the shortest hex" ".x{--c:#fff}"
    (normalize_minified ".x { --c: #ffffff }");
  (* An opaque alpha hex with no shorter form stays verbatim. *)
  Alcotest.(check string)
    "8-digit hex without a shorter form is unchanged" ".x{--c:#abcdef12}"
    (normalize_minified ".x { --c: #abcdef12 }");
  (* A bare colour keyword is also a valid <custom-ident> in a non-colour
     substitution site, so the fold must never produce a name: red folds to the
     hex form, not to [red], even though [red] is one byte shorter. *)
  Alcotest.(check string)
    "hex folds to hex, never to a named colour" ".x{--c:#f00}"
    (normalize_minified ".x { --c: #ff0000 }");
  Alcotest.(check string)
    "an rgb() function also folds to hex, not a name" ".x{--c:#f00}"
    (normalize_minified ".x { --c: rgb(255 0 0) }");
  (* A hex inside an <image> function (itself a clearly typed substream) folds
     through the recursion. *)
  Alcotest.(check string)
    "hex nested in a gradient folds" ".x{--g:linear-gradient(#f00,#00f)}"
    (normalize_minified ".x { --g: linear-gradient(#ff0000, #0000ff) }")

let customprops13_shortest_oklab_sign_boundaries () =
  Alcotest.(check string)
    "unregistered OKLab custom property folds the colour function"
    ".prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       ".prose { --tw-prose-kbd-shadows: oklab(21% -.003 -.034 / .1) }");
  Alcotest.(check string)
    "unregistered OKLab custom property folds regardless of source sign spacing"
    ".prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       ".prose { --tw-prose-kbd-shadows: oklab(21% -.003-.034 / .1) }");
  Alcotest.(check string)
    "registered OKLab custom property uses typed color minification"
    "@property \
     --tw-prose-kbd-shadows{syntax:\"<color>\";inherits:true;initial-value:#000}.prose{--tw-prose-kbd-shadows:#1118281a}"
    (normalize_minified
       "@property --tw-prose-kbd-shadows { syntax: \"<color>\"; inherits: \
        true; initial-value: black } .prose { --tw-prose-kbd-shadows: \
        oklab(21% -.003 -.034 / .1) }")

let customprops12_invalid_var () =
  Alcotest.(check bool)
    "var() with no arguments is rejected" true
    (match Css.of_string ~strict:true ".x { color: var() }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "var(red) (non-dashed) is rejected" true
    (match Css.of_string ~strict:true ".x { color: var(red) }" with
    | Error _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "var(-x, red) (single-dash prefix) is rejected" true
    (match Css.of_string ~strict:true ".x { color: var(-x, red) }" with
    | Error _ -> true
    | _ -> false)

let customprops12_shorthand_calc () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() in shorthand value preserved" ".x{border:var(--border)}"
    (normalize ".x { border: var(--border) }");
  Alcotest.(check string)
    "var() inside calc() preserved" ".x{width:calc(var(--x) + 10px)}"
    (normalize ".x { width: calc(var(--x) + 10px) }");
  Alcotest.(check string)
    "multiple var() in one value preserved" ".x{padding:var(--top)var(--right)}"
    (normalize ".x { padding: var(--top) var(--right) }")

let fidelity_var_fallback_preserved () =
  pretty_preserves ".x { color: var(--x,) }" [ "var(--x,)" ];
  pretty_preserves ".x { color: var(--x, red) }" [ "var(--x, red)" ];
  pretty_preserves ".x { color: var(--x, var(--y, red)) }"
    [ "var(--x, var(--y, red))" ]

let fidelity_calc_fallback () =
  pretty_preserves ".x { width: var(--x, calc(1px + 2px)) }"
    [ "var(--x, calc(1px + 2px))" ]

let fidelity_multi_comma () =
  pretty_preserves ".x { color: var(--x, red, blue) }" [ "var(--x, red, blue)" ];
  pretty_preserves
    ".x { font-family: var(--font, \"Helvetica Neue\", sans-serif) }"
    [ "Helvetica Neue"; "sans-serif" ]

let fidelity_var_name_case_preserved () =
  pretty_preserves ".x { color: var(--My-Var, red) }" [ "--My-Var" ];
  pretty_preserves ".x { color: var(--snake_case_var) }" [ "--snake_case_var" ]

let fidelity_custom_property_decl_preserved () =
  pretty_preserves ":root { --brand: red }" [ "--brand" ];
  pretty_preserves ".x { --x: 10px }" [ "--x" ];
  pretty_preserves ".x { --x: red blue }" [ "red blue" ]

(* {2 Variable inlining via theme + theme_defaults} *)

(* CSS Custom Properties L1: cascade exposes a print-time inlining API via an
   explicit caller-provided context. The [theme] set is a protection set:
   variables listed there stay as [var(--name)]. For variables outside that set,
   only a concrete [theme_defaults] result is positive evidence for inlining;
   [None] means "unknown", not "undefined". *)
let custom_props1_theme_inlining () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_theme = Css.Pp.String_set.empty in
  let resolve_brand = function "brand" -> Some "red" | _ -> None in
  let resolve_gap = function "gap" -> Some "16px" | _ -> None in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--brand) inlines to red when not in theme" ".x{color:red}"
    (render_theme ~theme:no_theme ~theme_defaults:resolve_brand
       (parse ".x { color: var(--brand) }"));
  Alcotest.(check string)
    "var(--gap) inlines to 16px when not in theme" ".x{margin:16px}"
    (render_theme ~theme:no_theme ~theme_defaults:resolve_gap
       (parse ".x { margin: var(--gap) }"));
  Alcotest.(check bool)
    "var() in theme keeps the var() reference" true
    (let theme = Css.Pp.String_set.add "brand" no_theme in
     let out =
       render_theme ~theme ~theme_defaults:resolve_brand
         (parse ".x { color: var(--brand) }")
     in
     Astring.String.is_infix ~affix:"var(--brand)" out)

(* CSS Custom Properties L1 section 2: a fallback is used at computed-value time
   when the referenced custom property is invalid or missing. The print-time
   renderer should not infer that from [theme_defaults] returning [None]; it has
   only learned that the caller did not provide a concrete replacement. *)
let customprops1_unresolved_fallback () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_resolve _ = (None : string option) in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--undef, red) is preserved when resolver has no answer"
    ".x{color:var(--undef,red)}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
       (parse ".x { color: var(--undef, red) }"));
  Alcotest.(check string)
    "nested var() chain is preserved when resolver has no answers"
    ".x{color:var(--a,var(--b,blue))}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
       (parse ".x { color: var(--a, var(--b, blue)) }"));
  Alcotest.(check string)
    "optimize+minify preserves unresolved nested var fallback"
    ".x{color:var(--a,var(--b,#00f))}"
    (normalize_minified ".x { color: var(--a, var(--b, blue)) }")

(* CSS Custom Properties L1: a theme var reachable only through another var()'s
   *typed* fallback ([transition-timing-function: var(--tw-ease, var(--theme))])
   still resolves transitively. Theme resolution is seeded by a structural value
   scan, so the nested var is found even though the typed AST spells that
   fallback as a value rather than a [Var_fallback]; the outer runtime var stays
   live. *)
let customprops1_transitive_fallback () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "default-transition-timing-function" -> Some "ease"
    | _ -> None
  in
  let theme = Css.Pp.String_set.of_list [ "tw-ease" ] in
  Alcotest.(check string)
    "nested theme var in a typed fallback inlines, runtime var stays live"
    ".t{transition-timing-function:var(--tw-ease,ease)}"
    (parse
       ".t { transition-timing-function: var(--tw-ease, \
        var(--default-transition-timing-function)) }"
    |> Css.resolve_theme ~theme ~theme_defaults:resolve
    |> Css.to_string ~minify:true)

(* CSS Properties & Values API: resolve_theme is a partial inline - it
   substitutes resolved theme vars but must leave unrelated [@property]
   registrations untouched. Inlining one theme token previously dropped every
   [@property] in the stylesheet. *)
let customprops1_resolve_keeps_property () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "inlining a theme var keeps an unrelated @property registration"
    "@property --tw-foo{syntax:\"*\";inherits:false}.x{width:24px}"
    (parse
       "@property --tw-foo{syntax:\"*\";inherits:false}.x{width:var(--blur-xl)}"
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:(function
         | "blur-xl" -> Some "24px"
         | _ -> None)
    |> Css.to_string ~minify:true)

(* CSS Custom Properties L1: a theme token already declared in the input on a
   root-scope block ([:root,:host], as Tailwind emits) and resolvable through
   [theme_defaults] inlines from that declaration and is dropped - the existing
   binding must not survive, be duplicated, or block the substitution. The
   declared value wins over the theme default (cascade). *)
let customprops1_inline_declared_root_token () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let blur = function "blur-xl" -> Some "24px" | _ -> None in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:blur
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "a declared :root,:host theme token inlines and its block is dropped"
    ".x{filter:blur(24px)}"
    (render ":root,:host{--blur-xl:24px}.x{filter:blur(var(--blur-xl))}");
  Alcotest.(check string)
    "the declared value wins over the theme default" ".x{filter:blur(9px)}"
    (render ":root{--blur-xl:9px}.x{filter:blur(var(--blur-xl))}")

(* CSS Custom Properties L1 section 2: inlining a [var()] inside a [calc()]
   resolves the variable, and the resulting all-constant calc reduces under
   minify. *)
let customprops1_calc_inline () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function "gap" -> Some "5px" | _ -> None in
  Alcotest.(check string)
    "calc(var(--gap) + 10px) inlines and reduces to 15px" ".x{width:15px}"
    (parse ".x { width: calc(var(--gap) + 10px) }"
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true)

(* CSS Custom Properties L1: a theme var referenced anywhere - inside another
   var's opaque value or inside a utility rule - and resolvable through
   [theme_defaults] is emitted at root scope, the global token's cascade scope.
   It merges into an existing :root,:host block (no spurious extra block), is
   never injected into the element-scoped rule that references it, and an
   unrelated @supports polyfill default is not pulled in. *)
let customprops1_transitive_merge () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.of_list [ "shadow" ] in
  let spacing = function "spacing" -> Some ".25rem" | _ -> None in
  let render ?(theme_defaults = spacing) css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "transitive theme default is folded into the kept variable"
    ":root,:host{--shadow:0 0 .25rem black}"
    (render ":root,:host{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "a theme default referenced from a utility is folded into it"
    ".shadow{--shadow:0 0 .25rem black}"
    (render ".shadow{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "an unrelated @supports polyfill default is not pulled in"
    "@supports(color:lab(0 0 0)){:root{--tw-x:initial}}:root,:host{--shadow:0 \
     0 .25rem black}"
    (render
       "@supports (color: lab(0 0 0)){:root{--tw-x:initial}} \
        :root,:host{--shadow:0 0 var(--spacing) black}");
  Alcotest.(check string)
    "an unresolved reference is kept live, not materialised"
    ":root,:host{--shadow:0 0 var(--spacing) black}"
    (render
       ~theme_defaults:(fun _ -> None)
       ":root,:host{--shadow:0 0 var(--spacing) black}")

(* CSS Custom Properties L1 sec. 2.1: a custom property's value is a
   [<declaration-value>], and CSS Syntax 3 sec. 8.2 bars an unmatched [}] / [)],
   a top-level [;], a [<bad-string-token>] and an unterminated function from
   one. [theme_defaults] is caller-supplied CSS text, so an answer that breaks
   those rules is not a value the name resolves to: the reference stays live,
   the sibling default still binds, and no rule, at-rule or second declaration
   reaches the output. *)
let theme_defaults_reject_escaping_value () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let src = ".a{color:var(--brand);background-color:var(--ok)}" in
  let defaults brand = function
    | "brand" -> Some brand
    | "ok" -> Some "blue"
    | _ -> None
  in
  let emit brand =
    parse src
    |> Css.resolve_theme ~theme_defaults:(defaults brand)
    |> Css.to_string ~minify:true
  in
  let inline brand =
    parse src
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:(defaults brand)
    |> Css.to_string ~minify:true
  in
  let bound =
    ":root{--ok:blue}.a{color:var(--brand);background-color:var(--ok)}"
  in
  let inlined = ".a{color:var(--brand);background-color:blue}" in
  List.iter
    (fun (what, brand) ->
      Alcotest.(check string)
        (what ^ " binds nothing and leaves the sibling default")
        bound (emit brand);
      Alcotest.(check string)
        (what ^ " stays a live reference while the rest inlines")
        inlined (inline brand))
    [
      ("a value closing the block", "red}");
      ("a value starting a sibling rule", "red}.evil{color:red");
      ("a value starting a rule after a [;]", "red};.evil{color:red");
      ("a value starting an at-rule", "red}@media print{.x{color:red}");
      ("a value carrying a second declaration", "red;--evil:lime");
      ("a value with an unmatched [)]", "red)");
      ("an unterminated function", "rgb(1,2,3");
      ("an empty value", "");
    ];
  (* A comment is consumed by tokenization (CSS Syntax 3 sec. 4.3.2), so
     ["red/*"] is the one-token value ["red"] and does bind. sec. 4.3.5 ends a
     string at EOF as the string it read, so that one binds as well. *)
  Alcotest.(check string)
    "an unterminated comment leaves a value that binds"
    ":root{--ok:blue;--brand:red}.a{color:var(--brand);background-color:var(--ok)}"
    (emit "red/*");
  Alcotest.(check string)
    "a value the input ended inside a string binds"
    ":root{--ok:blue;--brand:\"abc\"}.a{color:var(--brand);background-color:var(--ok)}"
    (emit "\"abc")

(* CSS Syntax 3 sec. 4.3.7: a [\X] escape carries any code point into an ident,
   so [var(--x\3b y)] references the theme name ["x;y"]. The default binds under
   that name, written back with the escapes that read it - the one spelling that
   makes the declaration the name it references rather than some other
   declaration ([y:red]) at root scope. *)
let theme_defaults_escaping_name () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  List.iter
    (fun (what, src, name, bound, inlined) ->
      let resolve ?theme () =
        parse src
        |> Css.resolve_theme ?theme ~theme_defaults:(fun n ->
            if n = name then Some "red" else None)
        |> Css.to_string ~minify:true
      in
      Alcotest.(check string)
        (what ^ ": binds under the name the reference spells")
        bound (resolve ());
      Alcotest.(check string)
        (what ^ ": resolves the reference it binds")
        inlined
        (resolve ~theme:Css.Pp.String_set.empty ());
      Alcotest.(check string)
        (what ^ ": the binding reads back as itself")
        bound
        (Css.to_string ~minify:true (parse bound)))
    [
      ( "a name carrying a [;]",
        ".a{color:var(--x\\3b y)}",
        "x;y",
        ":root{--x\\;y:red}.a{color:var(--x\\;y)}",
        ".a{color:red}" );
      ( "a name carrying a [}]",
        ".a{color:var(--x\\7d y)}",
        "x}y",
        ":root{--x\\}y:red}.a{color:var(--x\\}y)}",
        ".a{color:red}" );
    ]

(* CSS Custom Properties L1 section 2: a [var()] used inside a fallback list
   ([var(--font, "Helvetica", sans-serif)]) inlines if the variable resolves,
   otherwise the fallback list is kept intact. *)
let customprops1_fallback_list () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve_font = function "font" -> Some "Arial" | _ -> None in
  let no_resolve _ = (None : string option) in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--font, fallback) inlines to Arial when resolved"
    ".x{font-family:Arial}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve_font
       (parse ".x { font-family: var(--font, sans-serif) }"));
  Alcotest.(check bool)
    "var() with multi-comma fallback list keeps fallback when unresolved" true
    (let out =
       render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:no_resolve
         (parse ".x { font-family: var(--font, \"Helvetica\", sans-serif) }")
     in
     Astring.String.is_infix ~affix:"Helvetica" out
     && Astring.String.is_infix ~affix:"sans-serif" out)

(* CSS Custom Properties L1 section 2: a custom-property declaration may itself
   contain [var()] references. Inlining a chain [--accent: var(--brand)]
   requires resolving the inner variable first. *)
let customprops1_value_position () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "brand" -> Some "red"
    | "gap" -> Some "16px"
    | _ -> None
  in
  Alcotest.(check bool)
    "multiple var() in one value all inline" true
    (let out =
       parse ".x { padding: var(--gap) var(--gap) }"
       |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
            ~theme_defaults:resolve
       |> Css.to_string ~minify:true
     in
     Astring.String.is_infix ~affix:"16px 16px" out
     || Astring.String.is_infix ~affix:"16px" out)

(* CSS Custom Properties L1 section 2: explicit theme resolution is a transform
   phase, not merely a print-time spelling hook. If a resolver replacement is
   another [var()] reference, the transform should keep resolving until it
   reaches a concrete value, then normal minified serialization applies to that
   concrete value. *)
let theme_chain_resolution () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "accent" -> Some "var(--brand)"
    | "brand" -> Some "color(srgb 1 0 0)"
    | "gap" -> Some "var(--space)"
    | "space" -> Some "calc(1px + 2px)"
    | "nested-gap" -> Some "var(--nested-space)"
    | "nested-space" -> Some "calc(1px + var(--foo))"
    | "foo" -> Some "2px"
    | "runtime-gap" -> Some "var(--runtime-space)"
    | "runtime-space" -> Some "calc(1px + var(--runtime-foo))"
    | "fallback-gap" -> Some "var(--maybe, var(--space))"
    | "deep-fallback" -> Some "var(--unknown, calc(1px + var(--foo)))"
    | "two-axis" -> Some "var(--x) var(--y)"
    | "x" -> Some "1px"
    | "y" -> Some "calc(1px + 1px)"
    | "shadow-color" -> Some "var(--brand)"
    | _ -> None
  in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  let render_optimized css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.optimize |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "var replacement chains resolve to an authored color function"
    ".x{color:color(srgb 1 0 0)}"
    (render ".x { color: var(--accent) }");
  Alcotest.(check string)
    "resolved color function optimizes to the shortest color" ".x{color:red}"
    (render_optimized ".x { color: var(--accent) }");
  Alcotest.(check string)
    "var replacement chains resolve to calc, then reduce" ".x{width:3px}"
    (render ".x { width: var(--gap) }");
  Alcotest.(check string)
    "nested var inside replacement resolves before calc reduction"
    ".x{width:3px}"
    (render ".x { width: var(--nested-gap) }");
  Alcotest.(check string)
    "unresolved nested var inside replacement stays runtime-dynamic"
    ".x{width:calc(1px + var(--runtime-foo))}"
    (render ".x { width: var(--runtime-gap) }");
  Alcotest.(check string)
    "fallback arm inside replacement resolves when primary stays unknown"
    ".x{width:3px}"
    (render ".x { width: var(--fallback-gap) }");
  Alcotest.(check string)
    "var inside calculated fallback arm resolves, then calc reduces"
    ".x{width:3px}"
    (render ".x { width: var(--deep-fallback) }");
  Alcotest.(check string)
    "replacement with multiple vars resolves every slot" ".x{margin:1px 2px}"
    (render ".x { margin: var(--two-axis) }");
  Alcotest.(check string)
    "nested color replacement resolves through another replacement"
    ".x{text-shadow:0 0 2px color(srgb 1 0 0)}"
    (render ".x { text-shadow: 0 0 2px var(--shadow-color) }");
  Alcotest.(check string)
    "nested color replacement optimizes after resolution"
    ".x{text-shadow:0 0 2px red}"
    (render_optimized ".x { text-shadow: 0 0 2px var(--shadow-color) }")

(* CSS Custom Properties L1 section 3: a real per-element dependency cycle makes
   the variables in that cycle invalid at computed-value time. A caller-provided
   theme resolver is not that full computed-value graph, so a cycle in resolver
   output should stop resolution and preserve the authored runtime expression at
   the boundary instead of looping or guessing that the fallback is safe to
   select statically. *)
let theme_cycle_preserved () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "a" -> Some "var(--b)"
    | "b" -> Some "var(--a)"
    | "outer" -> Some "calc(1px + var(--loop-a))"
    | "loop-a" -> Some "var(--loop-b)"
    | "loop-b" -> Some "var(--loop-a)"
    | "fallback-loop" -> Some "var(--missing, var(--loop-a))"
    | "deep-fallback-loop" -> Some "var(--missing, calc(1px + var(--loop-a)))"
    | _ -> None
  in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "cyclic var replacement preserves the authored fallback boundary"
    ".x{color:var(--a,red)}"
    (render ".x { color: var(--a, red) }");
  Alcotest.(check string)
    "cycle nested inside calc preserves the authored outer expression"
    ".x{width:var(--outer)}"
    (render ".x { width: var(--outer) }");
  Alcotest.(check string)
    "cycle reached through fallback preserves the authored outer expression"
    ".x{color:var(--fallback-loop)}"
    (render ".x { color: var(--fallback-loop) }");
  Alcotest.(check string)
    "cycle inside calculated fallback preserves the authored outer expression"
    ".x{width:var(--deep-fallback-loop)}"
    (render ".x { width: var(--deep-fallback-loop) }")

(* CSS Custom Properties L1 section 2: when the variable's resolved value is a
   color, the color canonicalization rules (hex shorten, named conversion) apply
   to the inlined value as well. *)
let custom_props1_inlined_color_canonicalizes () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let resolve = function
    | "brand" -> Some "#ff0000"
    | "bg" -> Some "rgb(0, 0, 0)"
    | _ -> None
  in
  let render_theme ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.to_string ~minify:true
  in
  let render_optimized ?theme ?theme_defaults sheet =
    sheet
    |> Css.resolve_theme ?theme ?theme_defaults
    |> Css.optimize |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "var(--brand)=#ff0000 canonicalizes to shortest color after inlining"
    ".x{color:#f00}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { color: var(--brand) }"));
  Alcotest.(check string)
    "var(--bg)=rgb(0,0,0) preserves authored function after inlining"
    ".x{background-color:rgb(0 0 0)}"
    (render_theme ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { background-color: var(--bg) }"));
  Alcotest.(check string)
    "var(--bg)=rgb(0,0,0) optimizes to #000 after inlining"
    ".x{background-color:#000}"
    (render_optimized ~theme:Css.Pp.String_set.empty ~theme_defaults:resolve
       (parse ".x { background-color: var(--bg) }"))

(* {2 Fidelity tests for theme inlining} *)

(* Without explicit theme/theme_defaults configuration the printer preserves the
   var() reference unchanged. *)
let fidelity_no_inlining_without_context () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  Alcotest.(check string)
    "var() without theme keeps the reference under non-minified"
    ".x {\n  color: var(--brand);\n}"
    (Css.to_string (parse ".x { color: var(--brand) }"));
  Alcotest.(check bool)
    "var() with fallback under non-minified preserves both" true
    (let out = Css.to_string (parse ".x { color: var(--brand, red) }") in
     Astring.String.is_infix ~affix:"var(--brand" out
     && Astring.String.is_infix ~affix:"red" out)

(* CSS Custom Properties L1 section 2 (Variable Substitution): a [var()]
   fallback is part of the source value until the caller proves a concrete
   replacement. An empty [theme] with [theme_defaults] returning [None] should
   preserve the [var()] wrapper and fallback syntax. The fallback's own tokens
   may still be canonicalized locally. *)
let custom_props1_fallback_resolution_mode () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let no_resolve _ = (None : string option) in
  let inlined css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
         ~theme_defaults:no_resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "var(--undef, red) remains a runtime fallback" ".x{color:var(--undef,red)}"
    (inlined ".x { color: var(--undef, red) }");
  Alcotest.(check string)
    "var(--undef, #ff0000) fallback canonicalizes inside var()"
    ".x{color:var(--undef,#f00)}"
    (inlined ".x { color: var(--undef, #ff0000) }");
  Alcotest.(check string)
    "calc fallback is held inside var() (typed boundary)"
    ".x{width:var(--undef,calc(1px + 2px))}"
    (inlined ".x { width: var(--undef, calc(1px + 2px)) }");
  Alcotest.(check string)
    "nested var() chain remains runtime fallback"
    ".x{color:var(--a,var(--b,var(--c,blue)))}"
    (inlined ".x { color: var(--a, var(--b, var(--c, blue))) }");
  Alcotest.(check string)
    "empty fallback preserved as empty value (cascade-time invalid)"
    ".x{color:var(--undef,)}"
    (inlined ".x { color: var(--undef,) }")

(* CSS Custom Properties L1 section 2: when the variable IS in the theme set,
   the [var()] reference is preserved even when a fallback is available - the
   theme protects the variable from inlining. *)
let custom_props1_theme_protects_var () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.add "brand" Css.Pp.String_set.empty in
  let no_resolve _ = (None : string option) in
  let inlined css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults:no_resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check bool)
    "var(--brand, red) keeps reference when --brand is in theme" true
    (let out = inlined ".x { color: var(--brand, red) }" in
     Astring.String.is_infix ~affix:"var(--brand" out)

(* UX contract for print-time theme substitution: a supplied [theme] set should
   not make every non-theme variable look statically undefined. The set only
   protects names that must stay dynamic; inlining still requires a concrete
   resolver answer. *)
let theme_set_not_undefined () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.empty |> Css.Pp.String_set.add "brand" in
  let no_resolve _ = (None : string option) in
  let resolve_accent = function "accent" -> Some "#ff0000" | _ -> None in
  let render ?(resolve = no_resolve) css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults:resolve
    |> Css.to_string ~minify:true |> String.trim
  in
  Alcotest.(check string)
    "non-theme var without resolver answer is preserved"
    ".x{color:var(--accent)}"
    (render ".x { color: var(--accent) }");
  Alcotest.(check string)
    "non-theme var fallback without resolver answer is preserved"
    ".x{color:var(--accent,red)}"
    (render ".x { color: var(--accent, red) }");
  Alcotest.(check string)
    "non-theme var with resolver answer inlines" ".x{color:#f00}"
    (render ~resolve:resolve_accent ".x { color: var(--accent) }");
  Alcotest.(check string)
    "non-theme var fallback with resolver answer inlines" ".x{color:#f00}"
    (render ~resolve:resolve_accent ".x { color: var(--accent, blue) }")

let typed_var_default_fidelity () =
  let dx, x =
    Css.var
      ~default:(Css.Zero : Css.length)
      "tw-translate-x" Css.Length (Css.Px 123.)
  in
  let dy, y =
    Css.var
      ~default:(Css.Zero : Css.length)
      "tw-translate-y" Css.Length (Css.Px 456.)
  in
  let stylesheet =
    [
      Css.rule ~selector:(Selector.class_ "x")
        [
          dx;
          dy;
          Css.translate
            (Css.XY ((Css.Var x : Css.length), (Css.Var y : Css.length)));
        ];
    ]
  in
  Alcotest.(check string)
    "typed defaults do not inline under normal stylesheet rendering"
    ".x{--tw-translate-x:123px;--tw-translate-y:456px;translate:var(--tw-translate-x)var(--tw-translate-y)}"
    (minify stylesheet)

let suite =
  ( "stylesheet_vars",
    [
      ( "spec custom-properties 1 2 var inlining preserved",
        `Quick,
        customprops12_inlining );
      ( "spec custom-properties 1 2 normal minify keeps runtime vars",
        `Quick,
        customprops12_runtime_vars );
      ( "spec custom-properties 1 2 var text payload not inlined",
        `Quick,
        customprops12_text_payload );
      ( "spec custom-properties 1 5 var cycle preserved",
        `Quick,
        customprops15_cycle );
      ("fidelity var preserved", `Quick, fidelity_var_preserved);
      ( "spec custom-properties 1 2 empty fallback preserved",
        `Quick,
        customprops12_empty_fallback );
      ( "spec custom-properties 1 2 color in fallback canonicalizes",
        `Quick,
        customprops12_color_fallback );
      ( "spec custom-properties 1 2 nested var fallback preserved",
        `Quick,
        customprops12_nested_fallback );
      ( "spec custom-properties 1 2 calc in fallback",
        `Quick,
        customprops12_calc_fallback );
      ( "spec custom-properties 1 2 multi-comma fallback preserved",
        `Quick,
        customprops12_multi_comma );
      ( "spec custom-properties 1 2 var whitespace dropped",
        `Quick,
        customprops12_whitespace );
      ( "spec custom-properties 1 2 name case sensitive",
        `Quick,
        customprops12_case_sensitive );
      ( "spec custom-properties 1 3 custom property declaration",
        `Quick,
        customprops13_declaration );
      ( "spec scroll-snap 6.2 trailing semicolon",
        `Quick,
        scroll_snap_align_trailing_semicolon );
      ( "spec custom-properties 1 3 colour keyword case fold",
        `Quick,
        customprops13_color_keyword_case_fold );
      ( "spec custom-properties 1 3 unregistered font stack",
        `Quick,
        customprops13_unregistered_font_stack );
      ( "spec custom-properties 1 3 unregistered theme tokens",
        `Quick,
        customprops13_unregistered_theme_tokens );
      ( "spec custom-properties 1 3 registered numeric calc",
        `Quick,
        customprops13_registered_numeric_calc );
      ( "spec custom-properties 1 3 registered oklch chroma",
        `Quick,
        customprops13_registered_oklch_chroma );
      ( "spec custom-properties 1 3 registered percent calc",
        `Quick,
        customprops13_registered_percent_calc );
      ( "spec custom-properties 1 3 registered negative dimension calc",
        `Quick,
        customprops13_registered_negative_dimension_calc );
      ( "spec custom-properties 1 3 registered angle and time calc",
        `Quick,
        customprops13_registered_angle_time_calc );
      ( "spec custom-properties 1 3 trigonometric calc",
        `Quick,
        customprops13_trigonometric_calc );
      ( "spec custom-properties 1 3 unregistered unresolved calc spacing",
        `Quick,
        customprops13_shortest_unresolved_calc_spacing );
      ( "spec custom-properties 1 3 unregistered box-shadow token stream",
        `Quick,
        customprops13_box_shadow_zero_spread );
      ( "spec custom-properties 1 3 hex colour folds",
        `Quick,
        customprops13_hex_color_folds );
      ( "spec custom-properties 1 3 registered oklab sign boundaries",
        `Quick,
        customprops13_shortest_oklab_sign_boundaries );
      ( "spec custom-properties 1 2 invalid var rejected (negative)",
        `Quick,
        customprops12_invalid_var );
      ( "spec custom-properties 1 2 var in shorthand and calc",
        `Quick,
        customprops12_shorthand_calc );
      ( "fidelity var fallback preserved",
        `Quick,
        fidelity_var_fallback_preserved );
      ("fidelity var calc in fallback preserved", `Quick, fidelity_calc_fallback);
      ( "fidelity var multi-comma fallback preserved",
        `Quick,
        fidelity_multi_comma );
      ( "fidelity var name case preserved",
        `Quick,
        fidelity_var_name_case_preserved );
      ( "fidelity custom property declaration preserved",
        `Quick,
        fidelity_custom_property_decl_preserved );
      ( "spec custom-properties 1 theme inlining",
        `Quick,
        custom_props1_theme_inlining );
      ( "spec custom-properties 1 fallback preserved when unknown",
        `Quick,
        customprops1_unresolved_fallback );
      ( "spec custom-properties 1 nested theme var in typed fallback resolves",
        `Quick,
        customprops1_transitive_fallback );
      ( "spec custom-properties 1 resolve keeps unrelated @property",
        `Quick,
        customprops1_resolve_keeps_property );
      ( "spec custom-properties 1 inline declared root-scope token",
        `Quick,
        customprops1_inline_declared_root_token );
      ( "spec custom-properties 1 inlined var in calc simplifies",
        `Quick,
        customprops1_calc_inline );
      ( "spec custom-properties 1 transitive theme var merges in place",
        `Quick,
        customprops1_transitive_merge );
      ( "theme defaults reject a value that escapes its declaration",
        `Quick,
        theme_defaults_reject_escaping_value );
      ( "theme defaults bind a name that needs escaping",
        `Quick,
        theme_defaults_escaping_name );
      ( "spec custom-properties 1 fallback list with inlining",
        `Quick,
        customprops1_fallback_list );
      ( "spec custom-properties 1 var in value position",
        `Quick,
        customprops1_value_position );
      ( "spec custom-properties 1 theme chain resolution",
        `Quick,
        theme_chain_resolution );
      ( "spec custom-properties 1 theme cycle preserved",
        `Quick,
        theme_cycle_preserved );
      ( "spec custom-properties 1 inlined color canonicalizes",
        `Quick,
        custom_props1_inlined_color_canonicalizes );
      ( "fidelity no inlining without context",
        `Quick,
        fidelity_no_inlining_without_context );
      ( "spec custom-properties 1 fallback resolution mode",
        `Quick,
        custom_props1_fallback_resolution_mode );
      ( "spec custom-properties 1 theme protects var",
        `Quick,
        custom_props1_theme_protects_var );
      ( "spec custom-properties 1 theme set is not undefined set",
        `Quick,
        theme_set_not_undefined );
      ( "fidelity typed var default not inlined in stylesheet mode",
        `Quick,
        typed_var_default_fidelity );
    ] )
