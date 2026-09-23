# Cascade reference

This page holds the precise behaviour behind the [README](../README.md): every
option, what each rewrite assumes, and the exact rules of the comparison. Read
the README first; come here to check a detail.

## Minification

### Options of `cascade fmt`

| Flag | Purpose |
|---|---|
| `-m, --minify` | Minify the output. Local linear rewrites always run; the expensive global factoring fixpoint runs only when its preflight predicts useful savings. The top-level pipeline re-runs until the AST stops changing, since rule-order canonicalisation can expose a merge a single pass would miss: up to five times for a sheet of at most 128 rules, once above that. |
| `--objective=transfer\|raw` | Size metric `--minify` optimises for. `transfer` (default) keeps a global-factoring result only when it also shrinks the estimated gzip (DEFLATE) size of the output, since repeated declaration text is nearly free once compressed. `raw` keeps every raw-byte win and drives the factoring fixpoint to convergence, the right objective when the output ships uncompressed (inline style attributes, email HTML), at a large multiple of the default's wall clock. Has no effect without `--minify`. |
| `--lossless` | Disable bounded approximation under `--minify`. Exact rewrites still run, but repeating static numeric arithmetic stays as `calc()`, and static modern colour-space values and `color-mix()` stay functional. Otherwise-independent declarations retain their authored order instead of being sorted for compression. Has no effect without `--minify`. |
| `--enforce-spec` | Drop the evergreen-browser baseline target. Cascade still serialises to the shortest form the CSS text and the specs prove on their own, so it keeps every vendor-prefixed declaration, the `min-`/`max-` spelling of a media or container feature, the `&` prefix on a nested selector, the author's `:not(:dir(ltr))` and `input:not(:enabled)`, the number form of an `oklab`/`oklch` axis, and the quotes around a multi-word font family. It also holds the parser to the ident code points CSS Syntax 3 lists, which is the one part that acts without `--minify`. |
| `--scope=fragment\|stylesheet` | How much surrounding CSS context to assume. `fragment` (default) treats the input as an excerpt; `stylesheet` asserts the input is the whole author CSS graph, allowing Cascade to synthesise shorthands that reset omitted longhands because there are no unseen author declarations to overwrite. |
| `--closed-world` | Assume you know the exact HTML and that no element ever matches two clashing selectors, so the optimiser may merge rules it would otherwise keep apart. Unsafe: the page can render wrong if such an element appears, including one a script adds at runtime. See [Scope](#scope). Has no effect without `--minify`. |
| `--flatten-nesting` | Desugar nested rules into flat top-level rules for browsers that pre-date CSS Nesting. By default cascade preserves nesting since modern browsers parse it natively and it is usually shorter. |
| `--inline-imports` | Resolve `@import` against files relative to the input. Without `--import-root`, this may read any local path available to the process and is unsafe for untrusted CSS. |
| `--import-root=DIR` | Restrict `--inline-imports` to the canonical root and its descendants. Lexical `..` and symlink escapes are rejected; relative roots are resolved from the current working directory. |
| `--inline-vars` | Substitute `var(--name)` references with their declared values, then drop unused custom properties. Closed-world: assumes no runtime mutation of the variables it inlines. |
| `--keep-vars=NAMES` | Comma-separated custom-property names to preserve under `--inline-vars`. |
| `--profile` | Print the optimiser's global factoring fixpoint to stderr after the run: one row per iteration with the rules, bytes, and time on each side, then the committed savings and the preflight's own counters. Has no effect without `--minify`. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

Cascade parses the stylesheet into an AST and prints it again. Comments are
discarded during parsing, and empty rules and invalid declarations are dropped
in both pretty and minified output.

### Policy

Cascade picks the shortest spelling that renders the same at every choice
point. Where the CSS specifications and the browsers' error recovery allow
several valid serialisations, it chooses the shortest.

Rendering is what a browser paints. The CSSOM is not part of it: a rewrite may
change both the declaration text a script reads back through
`getPropertyValue` and the computed value it reads through `getComputedStyle`,
provided every rendered result is identical. `background:none` becomes
`background:0 0`, one byte shorter and painting the same in every case, while
`getComputedStyle` reports `background-position` as `0% 0%` for the input and
`0px 0px` for the output. A page that reads its own computed styles back sees
the minified spelling.

### What runs

Value-level rewrites:

- **Colours:** hex when no longer than the name (`black` -> `#000`,
  `blue` -> `#00f`; `red` stays a name). Modern colour functions
  (`lab`/`lch`/`oklab`/`oklch`/`color()`) fold to shorter sRGB only within the
  ΔE<sub>OK</sub> budget below.
- **Numbers and lengths:** drop leading and trailing zeros (`0.5` -> `.5`);
  convert compatible units only when shorter (`12pt` stays `12pt`).
- **Math:** `calc()`, `hypot()` and the rest fold constant subexpressions only
  when the serialised result is exact (`calc(100%/3)` stays `calc(100%/3)`).
  `calc(100/4)` becomes `25`, while `calc(1.75/1.125)` stays as written because
  14/9 has no finite decimal form. Multiplication folds unconditionally, since
  it is closed over finite decimals; division folds only when the quotient is
  exact. Default minification also folds all-static unitless
  `line-height:calc()` arithmetic to six significant figures; `--lossless`
  disables that fold.
- **Whitespace:** elided at safe token boundaries (`100% 0` -> `100%0`).

Selector-level rewrites:

- Branches sorted into cascade's canonical order
  (`div,.class,#id` -> `#id,.class,div`).
- Pseudo-elements in legacy single-colon form (`::before` -> `:before`).

Rule-level rewrites:

- Adjacent same-selector merging and identical-body combining across
  non-overlapping intermediates, with specificity and importance reasoning.
- Shared declarations extracted into comma-list rules when that is safe for
  the cascade and smaller.
- Shorthands with unordered components serialised in a canonical order
  (`animation:1s slide` -> `animation:slide 1s`).
- Dead-rule elimination, `@layer` consolidation, and the merging of `@media`,
  `@supports` and `@container` blocks that carry the same condition.
- A nested `@supports` condition decided against the conditions enclosing it:
  a guard they already answer yes loses its wrapper, a guard they contradict
  takes its block with it, and anything else narrows to what they leave to
  ask.
- Media Queries 4 range syntax when shorter (`(min-width:48px)` ->
  `(width>=48px)`).

These rules apply wherever cascade has a typed CSS value. An unregistered
custom-property value stays an opaque token stream, with one exception: a
substream whose type is fixed by its own syntax. A complete colour function
(`oklab(...)`, `color-mix(...)`, `rgb(...)`) or a hex colour (`#abc`) is a
colour in every `var()` substitution site, so it folds to its shortest
spelling. The same holds for a constant math function whose units fix its
dimension: a `calc()` reducing to an `<angle>` or `<time>` (`calc(1deg * 0)`
-> `0deg`) folds, while a `<percentage>` or a `calc()` that still references a
`var()` stays verbatim. The colour fold never produces a bare colour keyword,
because a name like `red` is also a valid `<custom-ident>`.

Whitespace inside an opaque value is folded only where it is insignificant. A
`)` closing a non-substitution function or a block is a hard token boundary,
so the space after it is dropped (`drop-shadow(a) drop-shadow(b)` ->
`drop-shadow(a)drop-shadow(b)`). The space after a `var()`, `env()` or
`attr()` stays, since the substituted value could otherwise merge with its
neighbour.

The value inside an `@supports` condition is also left alone. CSS Conditional
Rules 3 section 6.1 answers a declaration feature by running that exact
declaration through the browser's parser, so the text is the question:
`@supports (color: rgba(0,0,0,.5))` reaches the output as written. The
whitespace around the condition's own tokens is still elided, so
`@supports (display: grid)` prints as `@supports(display:grid)`.

### How rule merging scales

The rule-level rewrites run on a graph of cascade dependencies rather than on
repeated scans of the file. An edge joins two rules only when their order
matters: same-origin rules whose equal-specificity selectors may match the
same element and whose equal-importance declarations overlap once shorthands
are expanded. Independent rules stay unordered.

Candidate rewrites are scheduled through a priority queue, largest saving
first. Applying one updates the graph and re-enumerates only the affected
neighbourhood; a full enumeration is kept as the fallback when the queue
drains. The output is a topological order of the final graph, with the first
source appearance as the tie-break, so rules keep their source order wherever
the cascade does not force another.

### Approximation and lossless mode

Cascade folds colours only within 0.002 ΔE<sub>OK</sub> (the CSS Color 4
difference metric for Oklab and OkLCh). Functional alpha rounds to three
decimals (a tolerance of 0.0005); the 8-bit alpha of a hex fold is its
canonical spelling and is not gated by that tolerance.

`--lossless` keeps colour values exact: hex and named canonicalisation and
modern-syntax rewrites still run, but channel rounding, within-budget
modern-space folds and static `color-mix()` resolution are disabled.
Independent declarations keep their authored order rather than being sorted
for compression. A repeating quotient remains a `calc()`. `--enforce-spec`
does not change either precision policy.

### Scope

`--minify` assumes it sees all of the CSS text but nothing of the page at
runtime. It uses source order, the cascade, dependencies and dead-code
reasoning, but does not assume the DOM shape, writing mode, computed
direction, user styles or runtime changes to custom properties. The output
stays correct when the stylesheet is embedded in a larger page.

`--scope=stylesheet` asserts the input is the whole author CSS (after
`@import` resolution). The optimiser can then use a shorthand that resets
longhands the input never sets, because no other author rule can set them.

`--closed-world` is about the HTML rather than the CSS: it asserts that no
element ever matches two conflicting selectors, so rules can be grouped that
would otherwise stay apart. `.a{color:red}.c{color:blue}.b{color:red}` stays
three rules by default, since a `.b.c` element would take the wrong colour;
under the flag it becomes `.a,.b{color:red}.c{color:#00f}`.

### Target browsers

The default targets are Chrome 111, Firefox 128, Safari 16.4 and iOS Safari
16.4 rendering an HTML document. The record is public as
`Css.Optimize.evergreen_targets`, and library callers can pass their own
`Css.Optimize.targets`. Under these targets:

- `:not(:dir(ltr))` becomes `:dir(rtl)`, since every HTML element is `ltr` or
  `rtl`, and `input:not(:enabled)` becomes `input:disabled`. Each rewrite runs
  only where the selector proves its subject is an element the pair applies
  to, so `.c:not(:enabled)` and `input:not(:required)` stay as written.
- A vendor-prefixed declaration (`-moz-box-sizing`) is dropped when its
  unprefixed twin is present. WebKit fallbacks are added beside `user-select`,
  `backdrop-filter`, `hyphens` and `mask` where Safari 16.4 or Chrome 111 still
  need them. The prefixed `mask-mode` and `mask-composite` are not generated,
  because their legacy grammars differ.
- A `min-`/`max-` media or container feature becomes the range syntax,
  `(min-width: 700px)` to `(width >= 700px)`.
- A nested selector loses its `&` prefix, `& div` to `div`.
- An `oklab` or `oklch` axis takes the percentage spelling where it is
  shorter, `oklch(.7 .304 20)` to `oklch(.7 76% 20)`.

`--enforce-spec` removes all of these. It also keeps the quotes around a
multi-word font family, and it holds the parser to the ident code points CSS
Syntax 3 lists rather than accepting anything above U+007F; that last part
applies even without `--minify`.

An `@supports` condition is never assumed true or false, because support
depends on the rendering browser, down to its experimental-feature settings.
When a target requires a prefixed spelling, cascade asks the equivalent
disjunction, for example `(-webkit-backdrop-filter: ...) or
(backdrop-filter: ...)`.

## Comparing stylesheets

### Options of `cascade diff`

| Flag | Purpose |
|---|---|
| `--diff=MODE` | What counts as a difference: `auto` (default), `tree`, `string` or `canonical`. |
| `--limit=auto\|none\|N` | How many top-level differences to print. `auto` (default) prints them all while the report stays short, then keeps as many as fit; `none` prints every one; an integer prints exactly that many. |
| `--lossless` | Disable colour and numeric approximation in the `canonical` comparison. |
| `--enforce-spec` | Assume no browser in the `canonical` comparison: every `@supports` guard, vendor prefix and colour fallback is kept, so two sheets that differ on one are reported. |
| `--prune-unused-custom-props` | Drop custom properties nothing references, on both sides, before a `canonical` comparison. The comparison is then blind to differences in unused custom properties. |
| `--browser --html FILE` | Render both files over `FILE` in headless Chromium and compare the screenshots instead of the CSS. |
| `--json` | Write the comparison as one JSON document. The exit status is unchanged. |
| `--color=WHEN` | `auto` (default), `always` or `never`. `CASCADE_COLOR` sets the same thing; `NO_COLOR` overrides both. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

### The canonical comparison

`--diff=canonical` parses and minifies each input independently with the same
settings, then compares the two results byte for byte. `--lossless` applies to
both sides; the comparison itself adds no tolerance. Before comparing, it
treats as equal the changes that cannot alter a computed value:

- declarations or rules that set different properties may swap freely;
- a rule inside a `@media`, `@supports` or `@container` block holding only
  plain rules may move past statements it cannot conflict with, so how rules
  are grouped into such blocks does not matter;
- distinct custom properties may swap within a rule;
- different factorings of the same content compare equal, such as a
  declaration hoisted into a shared selector list versus written inline;
- where a layer block stands among unlayered rules, and whether layer order is
  declared up front or implied by block order, does not matter, since the
  cascade sorts by layer first.

Order that the cascade depends on stays significant: two writes of the same
property, a shorthand and its longhand, a vendor-prefixed fallback that
matters, and two `@layer` blocks of one layer.

A prefixed declaration that the
[WHATWG Compatibility Standard](https://compat.spec.whatwg.org/#css-legacy-name-aliases)
section 3.4.1 lists as a legacy alias is the same property as its unprefixed
twin, so `-webkit-transform` beside an identical `transform` normalises to
`transform` alone. A prefix the list does not name, such as the `-moz-`
spellings or `-webkit-user-select`, is a property of its own.
`-webkit-text-decoration-color` is not on the list, but every engine that
ships it treats it as an alias, and so does cascade. Equivalent shorthand
decompositions are not modelled.

Numbers follow the same precision as minification. An exact quotient such as
`calc(28/14)` equals `2` in either mode. By default `calc(28/18)` equals
cascade's six-figure `1.55556`; under `--lossless` it equals no finite decimal.

For browsers, the comparison assumes the targets `--minify` uses: a
`@supports` guard every target satisfies is unwrapped, a prefix a target
needs is added on both sides and one no target needs is dropped from both, and
a colour fallback every target parses past is dead. `--enforce-spec` turns
these off. An `@supports` guard that no browser can answer yes to, such as one
over a `<general-enclosed>` term that CSS Conditional 3 section 6.1 makes
false, selects nothing on either side, and its block is dropped from both.

`canonical` reports a difference only when some element could compute a
different value. Two files that render the same compare equal even when one
contains something the other lacks, an empty rule for example. Use `tree` or
`string` to catch that.

### Unreadable input and exit codes

A difference cascade can see exits 1, whether or not it read everything. When
it finds no difference but could not read a rule in one of the files, it
compares the text each side dropped. If that text matches byte for byte, it
cannot hide a difference and the exit is 0. If it differs, the exit is 2,
because one side dropped a rule the other did not.

A declaration cascade cannot read is handled differently. Its reader follows
the browser's accept set (`test/spec/browser/accept_set`), so a declaration it
refuses is one the browser would also drop. It is dropped on that side, a
parse warning is printed, and the verdict is taken over what remains. The
report and the `--json` document count the unreadable declarations and rules
on each side.

### Browser comparison

`--browser --html FILE` removes the document's own `<style>` elements and
stylesheet links but keeps inline `style` attributes. It loads the page afresh
under each file, at every viewport width a media condition in either file
names and in every interaction state either file names. A state is applied to
every element at once, animations are held at their start and transitions
finished, and the whole page is captured and compared pixel for pixel. Where a
capture differs, the computed styles of the elements under the differing
pixels are read under both files and the differences listed. The report
records the browser version, viewports, states and number of renders.

It needs node and a headless Chromium (`NODE`, `CHROME`, or the usual places)
and exits 2 without them, on a driver failure, or when nothing was rendered.

## Inlining: `cascade apply`

A declaration moves onto an element only when nothing left in CSS can
override it. A rule with no inline form (`:hover`, a `@media` block, a
pseudo-element, `@keyframes`) stays in a single `<style>` block, together with
every declaration it competes with, since an inline style beats every
selector.

A `<style>` block whose rules were all inlined is emptied rather than removed,
because a `<style>` element is a sibling like any other, and removing it would
change what a selector such as `.navbox + style + .portal-bar` matches. A
block the parser could not use keeps its text.

The exit code is 0 on success, including a parse that recovered part of its
input, and 1 when a `<style>` block or the extra stylesheet parsed to nothing.
The page is still written in that case, without those styles.

## Pruning: `cascade prune`

A rule is removed only when the matcher can evaluate its selector and every
element fails to match it. Treating "cannot evaluate" as "does not match" is
how a dead-rule check deletes a live rule, so the two are kept apart.

- A selector the matcher cannot evaluate (`:hover`, a pseudo-element,
  `:lang()`) is kept, and so is a selector list containing one. Every branch of
  a list the matcher can evaluate is judged on its own, so `.card, .gone`
  keeps `.card` and drops `.gone`.
- `@media`, `@supports` and `@container` conditions are not evaluated, since a
  document says nothing about the device or the browser. The rules inside are
  judged by their selectors.
- Statements that name no element (`@keyframes`, `@font-face`, `@property`,
  `@import`, `@layer`, `@charset`) are kept.
- Nesting is flattened first. Pipe the result through `cascade --minify` to
  nest it again.

`--dry-run` prints a ranking instead: rules that matched nothing first, then
the rest by increasing number of matched elements. Rules kept because their
selector could not be evaluated are counted separately, since that number
measures what the analysis could not see.

## The library

### Parsing modes

`Css.of_string ~strict:false s` returns `Ok { stylesheet; warnings }`, with
`warnings` listing recovered syntax and declaration issues. `~strict:true`
fails where the lenient parse would have warned. When both succeed, their
minified outputs are identical.

`Css.of_string ~preserve_source:true` also returns `source = Some snapshot`.
`Css.Source.contents` is the caller's exact string; `preprocessed`, `comments`
and `rules` expose the CSS Syntax view, and `original_loc` and `span` map
locations back to byte offsets and line and column positions. Each top-level
rule owns the bytes from the previous rule's end through its own end, and
`trailing_loc` owns the rest. An ordinary parse returns `source = None` and
keeps none of this.

The snapshot describes the parse, not later transforms. Optimising or
flattening the stylesheet leaves it unchanged, and a transform that needs
positions for nodes it splits, merges or creates must record them itself.

### Theme resolution

`Css.resolve_theme ?theme ?theme_defaults` is the library form of
`--inline-vars --keep-vars`:

- `theme` is the set of variable names whose `var()` references stay live.
  References to any other name are inlined to the value `theme_defaults`
  gives.
- `theme_defaults` maps a custom-property name to its value. Every `var()`
  reference that the stylesheet does not define and `theme_defaults` can
  resolve is emitted as a definition in the root theme block: an existing
  `:root` or `:host` rule, or a new `:root`. Resolution is transitive, and a
  chain that cycles or reaches an unknown name is dropped. A name for which
  `theme_defaults` returns `None`, such as a runtime `--tw-*` variable, is left
  alone.

Definitions go on `:root` because custom properties inherit: a theme token
defined there reaches every element and stays overridable, while one defined
on the rule that happens to use it would be confined to that rule.

### Structure

`Css.optimize ?targets ?judge ?flatten_nesting ?aggressive ?lossless
?enforce_spec ?scope` is the main entry point. Structural transforms (`fold`,
`map`, `sort`, `flatten_nesting`) and `Css.inline_imports` work on the same
AST. `?judge` names the browsers for which a run decides `@supports` guards and
colour fallbacks, which the minifier never does on its own. The diff is in the
`cascade.diff` sub-library (`Cascade_diff.Css_compare`,
`Cascade_diff.Tree_diff`, `Cascade_diff.String_diff`).

## CSS coverage

Cascade covers parsing, printing, transforms, comparison and optimisation for
these modules. It is not a browser engine.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting, specificity |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | About 30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colours, 15 colour spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (a nested condition that fails to parse recovers as `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimiser |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries](https://www.w3.org/TR/css-conditional-5/#container-queries) | `@container` with size queries and typed `style()` / `scroll-state()` queries, including range operators |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing and printing, typed fallbacks, theme substitution |
| [Properties and Values API Level 1](https://www.w3.org/TR/css-properties-values-api-1/) | `@property` with `syntax`, `inherits` and `initial-value` |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Typed properties cover the box model, flexbox, grid, logical properties,
typography, borders, backgrounds, gradients, transforms, transitions,
animations, filters, masks, anchor positioning, view transitions and
vendor-prefixed longhands.

## Test corpora

The browser tests drive a headless Chromium. CI installs one and sets
`CHROME`; locally the harness uses `CHROME` or the newest build in the
puppeteer or playwright cache, so an old cache tests an old browser. Install a
current one with `npx playwright install chromium-headless-shell`.

CI runs four jobs: build and test on Linux and macOS, a build against the
oldest dependency versions that solve, a check that `.ml`, `.mli`, `.mll`,
`.mly` and `dune` files are ASCII ([scripts/check_ascii.sh](../scripts/check_ascii.sh)),
and lint (ocamlformat and merlint). `merlint` comes from the
[samoht opam overlay](https://tangled.org/gazagnaire.org/opam-overlay.git).

Four committed corpora cover parser conformance, malformed input and
minification. Each directory records its upstream revision, licence and
regeneration command.

- **Web Platform Tests.** The `css/css-syntax/` subset of the
  [Web Platform Tests](https://github.com/web-platform-tests/wpt) is vendored
  under [test/interop/wpt/traces/css-syntax/](../test/interop/wpt/traces/css-syntax/)
  and replayed by [test/interop/wpt/test.ml](../test/interop/wpt/test.ml).
  Every CSS fragment goes through `Css.of_string`, and a test fails when
  cascade rejects what browsers accept or accepts what they reject; there is
  no skip list. Refresh with `REGEN=1 dune build @test/interop/wpt/regen-traces`.
- **Lightning CSS minifier tests.** Cascade's output is compared with cached
  answers from esbuild, clean-css, CSSO, cssnano and Lightning CSS over the
  Lightning CSS test inputs
  ([trace](../test/interop/lightning/traces/minify.pairs)). A case passes when
  cascade's output is no longer than the shortest valid cached answer; answers
  that crashed, do not round-trip, or change the parsed shape are excluded and
  logged. Refresh with `REGEN=1 dune build @test/interop/lightning/regen-traces`.
- **css-minify-tests.** A hand-curated set of
  [source and expected pairs](https://github.com/keithamus/css-minify-tests)
  across 29 feature categories. Each output must equal `expected.css` after
  cascade's documented normalisations. Refresh with
  `REGEN=1 dune build @test/interop/css-minify-tests/regen-traces`.
- **Markus Kuhn's UTF-8 stress test.** The parser reads the malformed-byte
  corpus whole and line by line. The source is pinned by SHA-256:
  `REGEN=1 dune build @test/interop/utf8/regen-traces`.

The [SatCSS benchmark](../bench/satcss/) is regenerated locally and not part
of the normal tests, because its upstream repository carries no licence for
redistributing the website stylesheets.
