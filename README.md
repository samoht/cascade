# Cascade

A command-line tool for **formatting**, **minifying**, **inlining**,
**structurally diffing**, and **pruning** CSS. Ships one binary (`cascade`)
and, for OCaml users, the library it is built on.

<!-- $MDX skip -->
```bash
cascade --minify style.css > style.min.css
cascade diff a.css b.css
```

Default minification is practical, not byte-for-byte preservation: it targets
maintained evergreen browsers and may approximate colours within `0.002`
Delta-EOK. Parsing and serialisation also normalise raw token spelling,
including CSSOM-observable custom-property strings. For a more conservative
contract, disable approximation and browser-baseline assumptions:

<!-- $MDX skip -->
```bash
opam exec -- cascade --minify --lossless --enforce-spec style.css
```

That combination retains authored declaration order, but it still does not
preserve comments, whitespace, exact source spelling, or every CSSOM-visible
string.

Cascade works from a typed CSS AST rather than the raw text, so every command
emits valid CSS by construction and reasons about the cascade instead of
guessing from bytes. `cascade fmt --minify` uses cascade analysis for structural
transforms (deduplication, rule merging, and selector grouping), canonicalises
colours and values under the contract above, and optimises for the bytes that
actually ship: compressed transfer size rather than raw length. `cascade diff`
compares the parsed structure, so a refactor that reorders rules or regroups
declarations without changing what they compute reads as no difference rather
than a wall of red and green. The same engine backs the `cascade` OCaml library.

On the SatCSS corpus of real-world stylesheets, cascade is competitive on both
size and speed; the head-to-head numbers are in [BENCHMARKS.md](BENCHMARKS.md).

## Install

On macOS, via the Homebrew tap:

<!-- $MDX skip -->
```bash
brew install samoht/tap/cascade
```

For OCaml/opam users (installs the CLI *and* the `cascade` library):

<!-- $MDX skip -->
```bash
opam install cascade
```

From source (OCaml 5.2+, opam, dune):

<!-- $MDX skip -->
```bash
git clone https://github.com/samoht/cascade.git
cd cascade
opam install . --with-test
cascade --help
```

## `cascade fmt`: format and minify

```text
cascade fmt [OPTIONS] [FILE]
```

`fmt` is the default subcommand, so `cascade FILE` and `cascade fmt FILE` do the
same thing. It reads a CSS file (or stdin when no file or `-` is given), parses
it into a structured CSS model, and writes formatted CSS to stdout.

Without flags it pretty-prints. With `--minify` it runs the standard safe
transforms (deduplication, rule merging, selector grouping, empty-rule
elimination, nested-rule flattening, shorthand composition, colour
canonicalisation) and emits minified output.

Exact `calc()` arithmetic folds in every precision mode: `calc(100/4)` becomes
`25`, while `calc(1.75/1.125)` stays as written because 14/9 has no finite
decimal form. Multiplication folds unconditionally, being closed over finite
decimals; division folds only when the quotient is exact. Default minification
additionally folds all-static unitless `line-height:calc()` arithmetic to six
significant figures; `--lossless` disables that approximate fold. The exact-only
rule also governs a `calc()` inside a custom-property value, whose token stream
is otherwise left opaque.

Cascade parses the stylesheet into an AST and prints it again. Comments are
discarded during parsing, and empty rules and invalid declarations are dropped
in both pretty and minified output.

### Common recipes

<!-- $MDX skip -->
```bash
cascade style.css > style.formatted.css                            # pretty-print
cascade --minify style.css > style.min.css                         # minify
cascade --minify --objective=raw style.css > style.min.css         # smallest uncompressed
cascade --inline-imports --inline-vars --minify style.css > out.css # bundle + minify
cascade --inline-vars --keep-vars=theme,brand style.css > themed.css
cat style.css | cascade -                                          # read stdin
```

### Flags

| Flag | Purpose |
|---|---|
| `-m, --minify` | Minify the output. Local linear rewrites always run; the expensive global factoring fixpoint runs only when its preflight predicts useful savings. The top-level pipeline re-runs until the AST stops changing, since rule-order canonicalisation can expose a merge a single pass would miss: up to five times for a sheet of at most 128 rules, once above that. |
| `--objective=transfer\|raw` | Size metric `--minify` optimises for. `transfer` (default) keeps a global-factoring result only when it also shrinks the estimated gzip (DEFLATE) size of the output, since repeated declaration text is nearly free once compressed. `raw` keeps every raw-byte win and drives the factoring fixpoint to convergence, the right objective when the output ships uncompressed (inline style attributes, email HTML), at a large multiple of the default's wall clock. Has no effect without `--minify`. |
| `--lossless` | Disable bounded approximation under `--minify`. Exact rewrites still run, but repeating static numeric arithmetic stays as `calc()`, and static modern colour-space values and `color-mix()` stay functional. Otherwise-independent declarations retain their authored order instead of being sorted for compression. Has no effect without `--minify`. |
| `--enforce-spec` | Drop the evergreen-browser baseline target. Cascade still serialises to the shortest form the CSS text and the specs prove on their own, so it keeps every vendor-prefixed declaration, the `min-`/`max-` spelling of a media or container feature, the `&` prefix on a nested selector, the author's `:not(:dir(ltr))` and `input:not(:enabled)`, the number form of an `oklab`/`oklch` axis, and the quotes around a multi-word font family. It also holds the parser to the ident code points CSS Syntax 3 lists, which is the one part that acts without `--minify`. |
| `--scope=fragment\|stylesheet` | How much surrounding CSS context to assume. `fragment` (default) treats the input as an excerpt; `stylesheet` asserts the input is the whole author CSS graph, allowing Cascade to synthesise shorthands that reset omitted longhands because there are no unseen author declarations to overwrite. |
| `--closed-world` | Assume you know the exact HTML and that no element ever matches two clashing selectors, so the optimiser may merge rules it would otherwise keep apart. Unsafe: the page can render wrong if such an element appears, including one a script adds at runtime. This is about the HTML, where `--scope` is about how much of the CSS you control; see [Scope](#scope). Has no effect without `--minify`. |
| `--flatten-nesting` | Desugar nested rules into flat top-level rules for browsers that pre-date CSS Nesting. By default cascade preserves nesting since modern browsers parse it natively and it is usually shorter. |
| `--inline-imports` | Resolve `@import` against files relative to the input. Without `--import-root`, this may read any local path available to the process and is unsafe for untrusted CSS. |
| `--import-root=DIR` | Restrict `--inline-imports` to the canonical root and its descendants. Lexical `..` and symlink escapes are rejected; relative roots are resolved from the current working directory. |
| `--inline-vars` | Substitute `var(--name)` references with their declared values, then drop unused custom properties. Closed-world: assumes no runtime mutation of the variables it inlines. |
| `--keep-vars=NAMES` | Comma-separated custom-property names to preserve under `--inline-vars`. |
| `--profile` | Print the optimiser's global factoring fixpoint to stderr after the run: one row per iteration with the rules, bytes, and time on each side, then the committed savings and the preflight's own counters. Useful to triage a slow input. Has no effect without `--minify`. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

### Size

`--minify` optimises compressed transfer size by default; `--objective=raw`
optimises uncompressed bytes instead, for output that ships uncompressed
(inline style attributes, email HTML). Head-to-head sizes and timings against
other minifiers on the SatCSS corpus are in [BENCHMARKS.md](BENCHMARKS.md).

## `cascade diff`: structural CSS diff

```text
cascade diff [OPTIONS] FILE1 FILE2
```

Compares two CSS files through the parsed CSS structure rather than
character-by-character: added, removed, modified, and reordered rules are
detected structurally, and property value changes are reported in terms of CSS
values. Identical files exit 0; differences exit 1, making `cascade diff`
usable as a CI check. Under `--diff=canonical`, exit 0 asserts that no element
computes a value differing beyond the approximation budget below, not that the
two files hold the same rules. A comparison that finds no difference but had to
drop a declaration or a rule it could not read exits 2 instead: what it dropped
reached neither side, so identity is not a verdict the comparison can give. The
report and the `--json` document count declarations and rules per side.

`--diff=MODE` controls what counts as "no difference":

- `auto` (default): structural diff; falls back to a string diff when the
  parsed structures match but the strings don't, so formatting differences
  (whitespace, comment position) still surface.
- `tree`: structural diff only; formatting-only differences collapse to
  "identical".
- `string`: character-level comparison.
- `canonical`: independently parses and optimises each input with the same
  canonical settings, serialises each result as minified CSS, then compares the
  two canonical representations byte for byte. `--lossless`, when present, is
  applied to both projections; the comparison step itself performs no further
  value interpretation or tolerance. Declarations or
  rules whose footprints are disjoint (they write different properties) may
  swap freely, a `@media`/`@supports`/`@container` block containing only
  plain rules moves as a unit past statements its rules cannot conflict with,
  distinct custom properties may swap within any rule, and different
  factorings of the same content (a declaration hoisted into a shared
  selector-list group vs written inline, split vs grouped selector lists)
  compare equal, since none of those moves can change a computed value.
  Cascade-significant order is kept distinct (two writes of the same
  property, a shorthand and its longhand, a load-bearing vendor-prefixed
  fallback, `@layer` blocks). An identical
  `-webkit-text-decoration-color`/`text-decoration-color` twin is normalized
  away; a differing or prefixed-only declaration remains distinct. Equivalent
  shorthand decompositions are still not modelled.
  Numeric arithmetic follows the same precision mode as minification: an exact
  quotient such as `calc(28/14)` compares equal to `2` in either mode. By
  default, `calc(28/18)` compares equal to Cascade's six-significant-figure
  output `1.55556`; under `--lossless` it remains distinct from every finite
  decimal spelling.

`canonical` ignores what cannot change a computed value, so surplus output that
renders identically, an empty rule for instance, does not appear in its report.
A check that must catch surplus output needs `tree` or `string`; a CI gate that
cares about both runs both.

| Flag | Purpose |
|---|---|
| `--diff=MODE` | What counts as "no difference": `auto` (default), `tree`, `string` or `canonical`, as above. |
| `--limit=auto\|none\|N` | How many top-level differences to print. `auto` (default) prints them all while the report stays short, then keeps as many as fit, each one whole and never fewer than one; `none` prints every one; an integer prints exactly that many. A shortened report ends with the number left out. |
| `--lossless` | Disable bounded colour and numeric approximation in the `--diff=canonical` canonicalisation, so two sheets that differ only by a fold within an approximation budget report as different rather than equal. Has no effect outside `--diff=canonical`. |
| `--prune-unused-custom-props` | Drop the custom-property bindings nothing references, on both sides, before comparing under `--diff=canonical`, so two sheets that differ only by a dead binding compare equal. The comparison is then blind to dead-custom-property divergences. Has no effect outside `--diff=canonical`. |
| `--color=WHEN` | `auto` (default), `always` or `never`. `CASCADE_COLOR` sets the same thing; `NO_COLOR` overrides both. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

<!-- $MDX skip -->
```bash
cascade diff reference.css output.css
cascade diff --diff=tree reference.css output.css
cascade diff --diff=canonical reference.css output.css
NO_COLOR=1 cascade diff reference.css output.css
```

## `cascade apply`: resolve a stylesheet into inline styles

```text
cascade apply [--minimal] PAGE.html [EXTRA.css]
```

Resolves the cascade against every element of `PAGE.html` and writes each
element's winning declarations into its `style` attribute, printing the page to
stdout. Selector matching, specificity, `!important` and inline-style priority
decide which declaration wins, the way a browser decides it. The page's own
`<style>` blocks supply the CSS, and `EXTRA.css` is applied on top of them.

A declaration moves onto an element only when nothing left in CSS can overwrite
it. A rule with no inline form (`:hover`, a `@media` block, a pseudo-element,
`@keyframes`) cannot be projected onto an element at all, so it is kept in a
single `<style>` block, and every declaration a kept rule competes for is kept
beside it. A style attribute outranks every selector, so inlining a `.btn`
colour past a `.btn:hover` colour would win a fight the browser gives to the
hover rule:

```html
<html><body><p class="btn">Send</p></body></html>
```

```css
.btn { color: #fff; padding: .5rem 1rem }
.btn:hover { color: #eee }
```

`cascade apply page.html theme.css` writes the padding onto the element and
leaves both colours in CSS, since `:hover` still has to be able to win:

```html
<html><head><style>.btn{color:#fff}.btn:hover{color:#eee}</style></head><body><p style="padding:.5rem 1rem" class="btn">Send</p></body></html>
```

A projected `<style>` block is emptied rather than removed. A `<style>` element
is a sibling like any other, so unlinking it would stop a kept rule such as
`.navbox + style + .portal-bar` from matching what it matches in the browser. A
block the parser could not use keeps its text instead of being emptied with the
rest: emptying it would ship a page with neither the inline styles it should
have had nor the CSS a browser might still make something of.

| Flag | Purpose |
|---|---|
| `--minimal` | Drop an inherited declaration that only restates the value the element already inherits from its ancestors, for the smallest styled page. |

The exit code is 0 on success, including a parse that recovered part of its
input, and 1 when a `<style>` block or `EXTRA.css` parsed to nothing. The page
is still written in that case, without those styles, so a build can gate on the
status rather than on the output.

<!-- $MDX skip -->
```bash
cascade apply page.html > inlined.html
cascade apply page.html theme.css > inlined.html
cascade apply --minimal page.html theme.css > inlined.html
```

## `cascade prune`: remove the rules a page cannot use

```text
cascade prune [--dry-run] PAGE.html... STYLE.css
```

Matches every rule of `STYLE.css` against every element of the given documents
and writes the stylesheet back without the rules that matched nothing. The
transform is the output, as it is under `--minify`; `--dry-run` reports instead.

A rule is removed only when the matcher has a model for its selector *and*
every element answers that it does not match. Keeping those two negatives apart
is the whole of the analysis: reading "no model" as "does not match" is how a
dead-rule check deletes a live rule.

- A selector outside what the matcher models (`:hover`, a pseudo-element,
  `:lang()`) is kept and never counted as unused, and so is a selector list
  holding one such branch. Every branch of a fully modelled list is judged on
  its own, so `.card, .gone` keeps `.card` and drops `.gone`.
- A `@media`, `@supports` or `@container` condition is never evaluated. It asks
  about a device, a user agent or a layout container, none of which a document
  carries, so the rules inside are judged by their own selectors alone.
- A statement that names no element (`@keyframes`, `@font-face`, `@property`,
  `@import`, `@layer`, `@charset`) is kept: a document says nothing about
  whether it is reached.
- Nesting is flattened before anything is judged, since a nested selector is
  written against its parent. Pipe the result through `cascade --minify` to
  nest it again.

**The documents are the whole of what the analysis sees.** A class a script
adds at runtime is in none of them, so a rule waiting for one is removed. That
is the limit of an AST-level dead-rule check: [CILLA](#references) runs the page
and reads matching off the live DOM instead, which is what answering for a class
that appears only after a click takes. Check what your scripts add before
shipping a pruned stylesheet.

`--dry-run` writes the ranking instead. Rules that matched nothing come first,
then the survivors by ascending matched-element count. The rules kept for want
of a model are listed and counted apart, since that number is the measure of
what the analysis could not see.

| Flag | Purpose |
|---|---|
| `--dry-run` | Write the ranked report instead of the pruned stylesheet. Rules that matched nothing come first, then the survivors by ascending matched-element count; the rules kept because the matcher has no model for their selector are counted apart. |

<!-- $MDX skip -->
```bash
cascade prune src/*.html style.css > style.pruned.css
cascade prune --dry-run src/*.html style.css
cascade prune src/*.html style.css | cascade --minify - > style.min.css
```

## In a build, CI, or pre-commit hook

A common shape: minify on build, check formatting in CI, and diff structurally
in pre-commit hooks.

<!-- $MDX skip -->
```bash
# build step
cascade --minify --inline-vars src/style.css > dist/style.min.css

# CI: fail when the committed file is not the formatted version
cascade src/style.css > /tmp/fmt.css
cascade diff --diff=tree src/style.css /tmp/fmt.css

# pre-commit: catch changes beyond formatting
cascade diff --diff=canonical origin/main:src/style.css src/style.css
```

The exit code is 0 when the inputs are identical under the chosen mode, 1 when
they differ, and 2 when the comparison finds no difference but had to drop a
declaration or a rule it could not read, so cascade slots into any tool that
branches on exit codes (`git` hooks, `make`, GitHub Actions, ...). The
`--minify` pipeline is fast enough that a 200 KB stylesheet costs well under
100 ms on the SatCSS corpus; `--objective=raw` trades roughly an order of
magnitude of wall clock for the last few percent of uncompressed bytes and fits
a release build rather than a watcher loop.

## `--minify` policy

Cascade picks the shortest behaviour-preserving spelling at every choice point.
Where the CSS spec and browser-compatible recovery rules permit several valid
serialisations, cascade chooses the shortest valid one.

Behaviour is what a browser renders. The CSSOM is not part of it: a rewrite may
change both the declaration text a script reads back through `getPropertyValue`
and the computed value it reads through `getComputedStyle`, provided every
rendered result is identical. `background:none` becomes `background:0 0`, one
byte shorter and painting the same in every case, while `getComputedStyle`
reports `background-position` as `0% 0%` for the input and `0px 0px` for the
output. A page that reads its own computed styles back sees the minified
spelling.

### What runs

Value-level rewrites:

- **Colours:** hex when no longer than the name (`black` -> `#000`,
  `blue` -> `#00f`; `red` stays a name). Modern colour functions
  (`lab`/`lch`/`oklab`/`oklch`/`color()`) fold to shorter sRGB only within the
  ΔE<sub>OK</sub> budget below.
- **Numbers and lengths:** drop leading/trailing zeros (`0.5` -> `.5`); convert
  compatible units only when shorter (`12pt` stays `12pt`).
- **Math:** `calc()`, `hypot()`, etc. fold constant subexpressions only when
  the serialised result is exact (`calc(100%/3)` stays `calc(100%/3)`).
- **Whitespace:** elided at safe token boundaries (`100% 0` -> `100%0`).

Selector-level rewrites:

- Branches sorted into cascade's canonical order
  (`div,.class,#id` -> `#id,.class,div`).
- Pseudo-elements in legacy single-colon form (`::before` -> `:before`).

Rule-level rewrites:

- Adjacent same-selector merging and identical-body combining across
  non-overlapping intermediates, with specificity and importance reasoning.
- The DAG optimiser extracts shared declarations into comma-list rules when
  cascade-safe and net smaller.
- Rule ordering is stable and deterministic: the optimiser builds a
  cascade-dependency DAG, then emits a topological order whose tie-break is the
  first source appearance of each node. If two rules are not order-constrained,
  cascade does not invent a lexicographic CSS order for them; it keeps the
  source-order key.
- Shorthands with unordered components serialise in cascade's canonical order
  (`animation:1s slide` -> `animation:slide 1s`).
- Dead-rule elimination, `@layer` consolidation, and the merging of `@media`,
  `@supports` and `@container` blocks that carry the same condition.
- A nested `@supports` condition is decided against the conditions enclosing it:
  a guard they already answer yes loses its wrapper, a guard they contradict
  takes its block with it, and anything else narrows to what they leave to ask.
- MQ4 range syntax when shorter (`(min-width:48px)` -> `(width>=48px)`).

These rules compose wherever cascade has a typed CSS value. An unregistered
custom-property value stays an opaque token stream, with one exception: a
substream whose type is fixed by its own syntax. A complete colour function
(`oklab(...)`, `color-mix(...)`, `rgb(...)`, ...) or a hex colour (`#abc`) is
unconditionally a colour in every `var()` substitution site, so it folds to its
shortest spelling and the fold preserves every rendered result. The same holds
for a complete math function whose units fix its dimension unambiguously: a
constant `calc()` reducing to an `<angle>` or `<time>` (`calc(1deg * 0)` ->
`0deg`) folds, while a `<percentage>` (ambiguous: length vs number percentage)
or a `calc()` that still references a `var()` stays verbatim. The colour fold
never produces a bare colour keyword: a name like `red` is also a valid
`<custom-ident>`, so it stays distinct from `#f00` even though it is shorter,
and hex stays hex. The fold changes the exact token string a script reads back
via `getPropertyValue`, which the policy above puts outside what cascade
preserves.

Whitespace inside an opaque value is likewise folded only where it is
insignificant: a `)` closing a non-substitution function or a block is a hard
token boundary, so the space after it is dropped (`drop-shadow(a) drop-shadow(b)`
-> `drop-shadow(a)drop-shadow(b)`). The space after a `var()` / `env()` / `attr()`
stays, since the substituted value could otherwise merge with its neighbour.

The value inside an `@supports` condition is opaque too, for a different
reason. CSS Conditional Rules 3 section 6.1 answers a declaration feature by
running that exact declaration through the rendering browser's parser, so what
stands there is the question and not a value to shorten:
`@supports (color: rgba(0,0,0,.5))` and `@supports (width: 10.0px)` reach the
output as written, where the same text in a rule body folds. The whitespace
around the condition's own tokens is still elided, so
`@supports (display: grid)` prints as `@supports(display:grid)`.

### How rule merging scales

The rule-level rewrites run on a cascade-dependency DAG, not on repeated linear
file scans. Graph edges represent only order-sensitive cascade dependencies:
same-origin rules whose equal-specificity selector branches may match the same
element and whose equal-importance declarations overlap after
shorthand/longhand expansion. Disjoint rules remain unordered in the graph.

Candidate rewrites are scheduled through an incremental priority queue,
largest byte-saving first. Applying a rewrite updates the graph and
re-enumerates only the affected neighbourhood; a full enumeration is kept as the
fallback when the queue drains. The final output is a deterministic
topological projection of the live graph, with first source appearance as the
stable tie-break key. Produced group/residual nodes inherit the earliest source
slot they represent, so the optimiser is source-stable whenever the cascade
does not force another order.

### Approximation and lossless mode

Cascade folds colours only within `0.002` ΔE<sub>OK</sub> (the CSS Color 4
Delta-E metric for Oklab/OkLCh). Alpha is separate: functional alpha rounds
to three decimals (`0.0005` tolerance); the 8-bit alpha of a hex fold is
its canonical spelling and is not gated by that tolerance.

Pass `--lossless` to keep colour values exact: hex/named canonicalisation and
modern-syntax rewrites still run, but channel rounding, within-budget
modern-space folds, and static `color-mix()` resolution are disabled.
Otherwise-independent declarations retain their authored order rather than
being sorted for compression, preserving their order in stylesheet text and
CSSOM enumeration.

The default optimiser also evaluates all-static unitless `line-height:calc()`
arithmetic and writes a non-terminating result to six significant figures.
Under `--lossless`, exact results still fold but a repeating quotient remains a
`calc()`. `--enforce-spec` does not change either precision policy; it controls
browser-target assumptions instead.

### Scope

`--minify` is closed over the CSS text but open over runtime layout state.
Cascade uses source order, the cascade, dependencies, and dead-code reasoning,
but does not assume DOM shape, writing mode, computed direction, user styles,
or runtime custom-property mutation. The output stays sound when the minified
stylesheet is embedded in a larger page.

`--scope=stylesheet` asserts the input is the whole author CSS graph (after
`@import` resolution). The optimiser can then synthesise a partial-coverage
shorthand whose omitted longhand resets are proved not to disturb a prior
write the optimiser can't see.

`--closed-world` is the other axis, and it is about the HTML rather than the
CSS: it asserts you know the exact document and that no element ever matches
two clashing selectors, so the optimiser may group rules the open world keeps
apart. `.a{color:red}.c{color:blue}.b{color:red}` stays three rules by default,
since a `.b.c` element would take the wrong colour, and becomes
`.a,.b{color:red}.c{color:#00f}` under the flag. It is unsafe for any page such
an element can appear on, including one a script builds at runtime.

### Target browsers

The default minify targets Chrome 111, Firefox 128, Safari 16.4 and iOS Safari
16.4 rendering an HTML document. The same record is public as
`Css.Optimize.evergreen_targets`; library callers can pass a different
`Css.Optimize.targets` record. The HTML direction model, where every element
is either `ltr` or `rtl`, shortens `:not(:dir(ltr))` to
`:dir(rtl)`. The HTML form-control model, which says an `input` is either
`:enabled` or `:disabled`, shortens `input:not(:enabled)` to `input:disabled`.
A vendor-prefixed declaration (`-moz-box-sizing`) whose unprefixed twin is
present is dropped, since evergreen browsers understand the unprefixed form.
Conversely, the target adds WebKit fallbacks beside `user-select`,
`backdrop-filter`, `hyphens`, `mask` and its compatible layer longhands where
Safari 16.4 or Chrome 111 still needs them. The prefixed `mask-mode` and
`mask-composite` properties are not generated because their legacy grammars
differ from the standard properties. A `min-`/`max-` media or container feature
becomes the Media Queries 4 range grammar, `(min-width: 700px)` to
`(width >= 700px)`. A nested selector loses its `&` prefix, `& div` to `div`,
which the relaxed nesting syntax reads the same way. An `oklab` or `oklch` axis
takes the percentage spelling wherever it is shorter, `oklch(.7 .304 20)` to
`oklch(.7 76% 20)`.

Each of those state pseudo-class pairs partitions a different set of elements,
and outside its own set an element matches neither half, so the rewrite runs
only where the compound proves its subject is in the set. `.c:not(:enabled)`
and `input:not(:required)` both stay as the author wrote them.

`--enforce-spec` drops those facts. Cascade still serialises to the shortest
CSS form it knows, but neither the direction nor the form-control model is
assumed, every vendor prefix is kept, a media or container feature keeps the
`min-`/`max-` spelling the author wrote, a nested selector keeps its `&`, and a
colour axis keeps its number form. It also keeps the quotes around a multi-word
font-family name, whose unquoted form is shorter but is not the CSSOM-canonical
serialisation, and holds the parser to the ident code points CSS Syntax 3 lists
rather than reading anything above U+007F. That last one is the only part of the
flag that acts without `--minify`.

An `@supports` condition is never assumed true or false. CSS Conditional Rules
3 section 6.1 defines support as the rendering browser accepting the
declaration, down to a per-installation experimental-feature preference, so
the author's guard remains a question for that browser. When a target requires
a prefixed spelling, Cascade asks the equivalent disjunction, for example
`(-webkit-backdrop-filter: ...) or (backdrop-filter: ...)`.

## CSS specification coverage

Cascade targets selected **CSS Level 3, Level 4, and Level 5** modules. Its
conformance target is CSS parsing, ASTs, printing, transforms, diffs, and
optimisation; it is not a complete web-platform runtime.

| Specification | Coverage |
|---|---|
| [Selectors Level 4](https://www.w3.org/TR/selectors-4/) | Class, ID, element, universal, attribute, pseudo-classes (`:hover`, `:nth-child()`, `:where()`, `:not()`, `:is()`, `:has()`), pseudo-elements, combinators, `&` nesting, specificity |
| [Values and Units Level 4](https://www.w3.org/TR/css-values-4/) | ~30 length units, `calc()`, `clamp()`, `min()`, `max()`, `minmax()`, angles, durations |
| [Color Level 4](https://www.w3.org/TR/css-color-4/) | Hex, `rgb()`, `hsl()`, `hwb()`, `oklch()`, `oklab()`, `color-mix()`, 148 named colours, 15 colour spaces |
| [Conditional Rules Level 5](https://www.w3.org/TR/css-conditional-5/) | `@media` (a nested condition that fails to parse recovering as `not all`), `@supports` property and selector checks, `@when` / `@else`, `@supports-condition` |
| [Cascade Level 5](https://www.w3.org/TR/css-cascade-5/) | `@layer` declarations and blocks, CSS-wide keywords, `all` reset semantics in the optimiser |
| [Nesting Module](https://www.w3.org/TR/css-nesting-1/) | Nested rules with `&`, nested `@media` and `@supports` |
| [Container Queries Level 5](https://www.w3.org/TR/css-conditional-5/#container-queries) | `@container` with size queries and typed `style()` / `scroll-state()` queries, including range operators |
| [Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/) | `var()` parsing/printing, typed fallbacks, theme/default substitution, `@property` registration |
| [Fonts Level 4](https://www.w3.org/TR/css-fonts-4/) | `@font-face` descriptors |
| [Animations Level 1](https://www.w3.org/TR/css-animations-1/) | `@keyframes`, `@starting-style` |

Typed CSS properties cover the box model, flexbox, grid, logical properties,
typography, borders, backgrounds, gradients, transforms, transitions,
animations, filters, masks, anchor positioning, view transitions, and
vendor-prefixed longhands. Together these cover the stylesheet surface
typically emitted by CSS generators, component libraries, and utility
frameworks.

## Limitations

- **UTF-8 input only.** Cascade parses already-decoded UTF-8 text. BOM
  handling, HTTP charset fallback, and `@charset "...";` byte sniffing are
  the caller's job; legacy encodings (`Shift_JIS`, UTF-16, ...) must be
  decoded upstream.
- **No runtime subsystems.** No implicit DOM, CSSOM, network loader, layout
  tree, renderer, or computed-style engine. CSS syntax for those features
  parses and prints; analyses that need runtime data take an explicit closed
  context.
- **Comments and source positions** are not attached to the typed AST or
  reproduced by the parser/printer round trip. Library callers that need
  authored syntax can pass `~preserve_source:true` to `Css.of_string`: the
  resulting separate, immutable `Css.Source` snapshot retains exact input
  bytes, comments, located syntax rules, deterministic trivia ownership, and
  original-byte/line-column mappings. A later AST transform does not invent
  provenance for nodes it splits, merges, drops, or creates.
- **`cascade prune` sees only the documents it is given.** A rule whose class
  a script adds at runtime matches nothing in the parsed HTML, so it is
  removed.
- **Unregistered custom properties** stay opaque token streams to the
  optimiser, apart from substreams whose type their own syntax fixes
  unambiguously (complete colour functions, and constant math functions
  reducing to an `<angle>` or `<time>`), which fold to their shortest spelling.

## Using cascade as a library

The CLI is a thin wrapper over the public OCaml API exposed by the `cascade`
opam package.

```ocaml
# open Cascade.Css;;
# let button =
    rule ~selector:(Selector.class_ "btn")
      [ display Inline_block
      ; background_color (hex "#3b82f6")
      ; color (hex "#ffffff")
      ; padding [ Rem 0.5 ]
      ; border_radius (radius (Rem 0.375))
      ]
  in to_string (v [ button ]);;
- : string =
".btn {\n  display: inline-block;\n  background-color: #3b82f6;\n  color: #ffffff;\n  padding: .5rem;\n  border-radius: .375rem;\n}"
```

Output:

```css
.btn {
  display: inline-block;
  background-color: #3b82f6;
  color: #ffffff;
  padding: .5rem;
  border-radius: .375rem;
}
```

Properties, values, and selectors are sealed OCaml ADTs, so invalid
constructions are caught at compile time. Structural transforms (`fold`,
`map`, `sort`, `flatten_nesting`), `Css.inline_imports`, and
`Css.optimize ?targets ?flatten_nesting ?aggressive ?lossless ?enforce_spec
?scope` is the main entry point for AST-level work. Transforms that need
information beyond CSS text take an explicit context rather than reading
ambient runtime state; `Css.Optimize.evergreen_targets` names the default
browser contract.

Structural diff lives in the separate `cascade.diff` sub-library
(`Cascade_diff.Css_compare`, `Cascade_diff.Tree_diff`,
`Cascade_diff.String_diff`); it is what `cascade diff` is built on.

### Theme resolution

`Css.resolve_theme ?theme ?theme_defaults` is the AST-level form of the
`--inline-vars --keep-vars` recipe above: it resolves design-token variables
against caller-supplied data.

- `theme` is the set of variable names whose `var()` references stay live. When
  `theme` is given, references to any other name are inlined to the value
  `theme_defaults` resolves for it.
- `theme_defaults` maps a custom-property name to its value and supplies the
  global token definitions. Every `var()` reference that is undefined in the
  stylesheet and resolvable through `theme_defaults` is emitted as a definition
  in the root-scope theme block: an existing `:root` / `:host` rule, or a fresh
  `:root`. Resolution is transitive, and a chain that cycles or hits a dead end
  is dropped. A name `theme_defaults` returns `None` on (for example a runtime
  `--tw-*` variable) is left free.

The definition lands at root scope by design. Custom properties are inherited
and resolved per element
([Custom Properties Level 1](https://www.w3.org/TR/css-variables-1/)), so
`var(--x)` needs `--x` defined on the element or an ancestor. A theme token is
global: defining it on `:root` / `:host` makes it inherit to every element and
stay globally overridable, whereas defining it on the element-scoped rule that
happens to reference it would confine and shadow it.

### Parsing modes

`Css.of_string ~strict:false s` returns `Ok { stylesheet; warnings }`, with
`warnings` listing recovered syntax and declaration issues. `~strict:true`
errors when the lenient parse would have warned. When both succeed, their
minified outputs are identical.

`Css.of_string ~preserve_source:true` has the same strictness and diagnostics
but returns `source = Some snapshot`. `Css.Source.contents` is the caller's
exact string; `preprocessed`, `comments`, and `rules` expose the CSS Syntax
view, and `original_loc` / `span` map its locations back to original byte
offsets and one-based line/column positions. Each top-level syntax rule owns the
bytes from the previous rule's end through its own end, and `trailing_loc` owns
the rest, so whitespace and recovered material have an unambiguous home. The
opt-in snapshot retains the original buffer, a second buffer only when CSS
preprocessing changes it, the component tree, comments, a line index, and an
offset map when needed; an ordinary parse returns `source = None` and pays none
of that retention cost.

The snapshot describes the authored parse, not a mutable source map. Optimising
or flattening the typed stylesheet leaves it unchanged. A transform that needs
mappings for split, merged, or synthetic nodes must record those mappings while
performing the transform instead of receiving guessed locations from Cascade.

### Small runtime footprint

The core `cascade` library links `uri`, `psq`, `logs` and
[mtime](https://erratique.ch/software/mtime), which supplies the monotonic
clock `--profile` measures factoring iterations against; it does not pull
`fmt` or `unix`, so js_of_ocaml embedders stay lean. A local jsoo build that
parses and minifies one stylesheet compresses to under 200 KiB
(`--opt 3 --no-source-map`, size-oriented runtime flags).

## Development and testing

<!-- $MDX skip -->
```bash
dune build          # the library and the cascade binary
dune test           # the whole suite: unit, cram, fuzz, interop, render
dune fmt            # ocamlformat
merlint             # lint the OCaml sources
```

`merlint` comes from the
[samoht opam overlay](https://tangled.org/gazagnaire.org/opam-overlay.git);
add the repository before `opam install merlint`.

CI runs four jobs, and a change passing `dune build` and `dune test` can still
fail two of them: `Build and test` on Linux and macOS, `Lower bounds` against
the oldest dependency versions that solve, `ASCII source`
([scripts/check_ascii.sh](scripts/check_ascii.sh) over the `.ml`, `.mli`,
`.mll`, `.mly` and `dune` files), and `Lint` (ocamlformat plus merlint).
Prose files carry their non-ASCII content.

Every user-visible change gets an entry in the top version block of
[CHANGES.md](CHANGES.md), naming the impact rather than the diff.

Four committed oracle corpora cover parser conformance, malformed UTF-8 input,
and minified-output behaviour. Each directory records its pinned upstream,
license notice, and exact regeneration command.

- **WPT parser conformance.** The `css/css-syntax/` subset of the [Web
  Platform Tests](https://github.com/web-platform-tests/wpt) is vendored
  under [test/interop/wpt/traces/css-syntax/](test/interop/wpt/traces/css-syntax/)
  and replayed by [test/interop/wpt/test.ml](test/interop/wpt/test.ml). Every
  CSS fragment in every `<style>`, `style="..."`, `support/*.css`, and
  `parseRule(\`...\`)` site goes through `Css.of_string`. A test fails when
  cascade rejects what browsers accept or accepts what browsers reject; there
  is no skip list. Refresh with
  `REGEN=1 dune build @test/interop/wpt/regen-traces`.
- **Lightning CSS minify oracle.** Cascade's `--minify` output is compared
  with cached answers from `esbuild`, `cleancss`, `csso`, `cssnano`, and
  `lightningcss-cli` over the Lightning CSS test inputs
  ([trace](test/interop/lightning/traces/minify.pairs), regenerated via
  `REGEN=1 dune build @test/interop/lightning/regen-traces`). The upstream
  revision and every oracle CLI are package-lock pinned. Each record is treated
  as the complete stylesheet (`scope: Stylesheet`). A case passes when
  cascade's output is no longer than the shortest *valid* cached answer;
  oracle answers that crashed, fail to round-trip, or change the parsed shape
  are excluded and logged.
- **keithamus/css-minify-tests.** A vendor-neutral hand-curated set of
  `source.css`/`expected.css` pairs covering 29 CSS feature categories. Each
  pair must equal the upstream `expected.css` after cascade's documented
  normalisations. Refresh with
  `REGEN=1 dune build @test/interop/css-minify-tests/regen-traces`.
- **Markus Kuhn's UTF-8 stress test.** The parser consumes the committed
  malformed-byte corpus whole and line by line. The floating source URL is
  pinned by SHA-256 and a refresh fails on drift:
  `REGEN=1 dune build @test/interop/utf8/regen-traces`.

The [SatCSS benchmark](bench/satcss/) (Hague-Lin-Hong's CSS minification
corpus) is regenerated locally and not part of normal tests: the upstream
repository carries no licence for redistributing the website CSS snapshots.

## References

**Other CSS tooling.** [Lightning CSS](https://github.com/parcel-bundler/lightningcss)
(Rust), [esbuild](https://github.com/evanw/esbuild) (Go), and the JS
optimisers [CSSO](https://github.com/css/csso),
[cssnano](https://github.com/cssnano/cssnano), and
[clean-css](https://github.com/clean-css/clean-css) all serve as cached
minifier oracles in the test suite.
[PostCSS](https://github.com/postcss/postcss) and
[CSSTree](https://github.com/csstree/csstree) are the broader JS
parser/AST projects worth comparing against. Earlier OCaml CSS work:
[css-parser](https://github.com/astrada/ocaml-css-parser) and
[OCaml-css](https://zoggy.frama.io/ocaml-css/).

**Optimisation research.**
[Hague, Lin, Hong (2018)](https://arxiv.org/abs/1812.02989) formalize rule
merging as a CSS-graph problem: a merge is legal only when selector
intersection and the intervening cascade dependencies preserve semantics.
[Visscher, Punt, Zaytsev (2016)](https://grammarware.net/text/2016/aba-css.pdf)
catalogue A-B*-A patterns (a property set, overridden, then restored), useful
adversarial input for optimisers since source order, specificity,
inheritance, and implicit defaults all affect whether a rewrite is sound.
[CILLA](https://github.com/saltlab/cilla) (Mesbah, Mirshokraie) analyses
runtime DOM-CSS matching to flag dead selectors at the layout level, a useful
reference for what an AST-level dead-rule check can and cannot claim.

**Specifications cascade implements:**
[Syntax 3](https://www.w3.org/TR/css-syntax-3/),
[Selectors 4](https://www.w3.org/TR/selectors-4/),
[Values 4](https://www.w3.org/TR/css-values-4/),
[Color 4](https://www.w3.org/TR/css-color-4/),
[Cascade 5](https://www.w3.org/TR/css-cascade-5/),
[Conditional 5](https://www.w3.org/TR/css-conditional-5/), and
[Nesting 1](https://www.w3.org/TR/css-nesting-1/).

## Licence

[ISC](LICENSE)
