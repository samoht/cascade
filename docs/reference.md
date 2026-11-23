# Cascade reference

This page holds the precise behaviour behind the [README](../README.md): every
option, what each rewrite assumes, and the exact rules of the comparison. Read
the README first; come here to check a detail.

## Minification

### Options of `cascade fmt`

| Flag | Purpose |
|---|---|
| `-m, --minify` | Minify the output. Local linear rewrites always run; the expensive global factoring fixpoint runs only when its preflight predicts useful savings. The top-level pipeline makes one pass at every size, and each step finishes in it what it starts, so minifying the output again returns it. |
| `--objective=transfer\|raw` | Size metric `--minify` optimises for. `transfer` (default) weighs each rewrite by an estimate of the compressed bytes it saves (see [How rule merging scales](#how-rule-merging-scales)), and merges rules with one body only where they print within 128 rules of each other, because a selector moved away from its neighbours stops compressing against them; `raw` weighs rewrites by their raw bytes and merges at any distance. `raw` also drives the factoring fixpoint to convergence, the right objective when the output ships uncompressed (inline style attributes, email HTML), at a large multiple of the default's wall clock. Has no effect without `--minify`. |
| `--lossless` | Disable bounded approximation under `--minify`. Exact rewrites still run, but repeating static numeric arithmetic stays as `calc()`, and static modern colour-space values and `color-mix()` stay functional. Otherwise-independent declarations retain their authored order instead of being sorted for compression. Has no effect without `--minify`. |
| `--enforce-spec` | Drop the evergreen-browser baseline target. Cascade still serialises to the shortest form the CSS text and the specs prove on their own, so it keeps every vendor-prefixed declaration, the `min-`/`max-` spelling of a media or container feature, the author's `:not(:dir(ltr))` and `input:not(:enabled)`, the number form of an `oklab`/`oklch` axis, and the quotes around a multi-word font family. It also has the parser keep a rule over a pseudo-class no target reads, and holds it to the ident code points CSS Syntax 3 lists; those parts act without `--minify` too. |
| `--scope=fragment\|stylesheet` | How much surrounding CSS context to assume. `fragment` (default) treats the input as an excerpt; `stylesheet` asserts the input is the whole author CSS graph, allowing Cascade to synthesise shorthands that reset omitted longhands because there are no unseen author declarations to overwrite. |
| `--closed-world` | Assume you know the exact HTML and that no element ever matches two clashing selectors, so the optimiser may merge rules it would otherwise keep apart. Unsafe: the page can render wrong if such an element appears, including one a script adds at runtime. See [Scope](#scope). Has no effect without `--minify`. |
| `--flatten-nesting` | Desugar nested rules into flat top-level rules. `--minify` already does for the default targets and under `--enforce-spec`, since Chrome 111 and Safari 16.4 read no nesting; the flag also flattens for library callers whose targets parse it. Has no effect without `--minify`. |
| `--inline-imports` | Resolve `@import` against files relative to the input. Without `--import-root`, this may read any local path available to the process and is unsafe for untrusted CSS. |
| `--import-root=DIR` | Restrict `--inline-imports` to the canonical root and its descendants. Lexical `..` and symlink escapes are rejected; relative roots are resolved from the current working directory. |
| `--inline-vars` | Substitute `var(--name)` references with their declared values, then drop unused custom properties. Closed-world: assumes no runtime mutation of the variables it inlines. |
| `--keep-vars=NAMES` | Comma-separated custom-property names to preserve under `--inline-vars`. |
| `--profile` | Print to stderr, after the run, the time spent reading, optimising and printing. A table of the global factoring fixpoint follows, one row per iteration with the rules, bytes and time on each side. The committed savings, in estimated compressed bytes under `--objective=transfer`, and the preflight's own counters close it. Has no effect without `--minify`. |
| `-q, --quiet` | Print errors only: no parse warning and no note. |
| `-v, --verbose` | Print every parse warning in full; a warning whose message was already printed for the file is otherwise counted under the first. Twice (`-vv`) also prints the optimiser's step tracing. |

Cascade parses the stylesheet into an AST and prints it again. Comments are
discarded during parsing, and empty rules and invalid declarations are dropped
in both pretty and minified output.

### Policy

Cascade picks the shortest spelling that renders the same at every choice
point. Where the CSS specifications and the browsers' error recovery allow
several valid serialisations, it chooses the shortest.

What `--minify` preserves is observational equality over the target browsers,
defined in [invariants.md](invariants.md#definitions): in every document and
surrounding context, every element gets the same used values and the same
transitions and animations. The CSSOM text is not part of it: a rewrite may
change the declaration text a script reads back through `getPropertyValue` and
the computed value it reads through `getComputedStyle`. `background:none`
becomes `background:0 0`, one byte shorter and painting the same in every
case, while `getComputedStyle` reports `background-position` as `0% 0%` for
the input and `0px 0px` for the output. A custom property is the exception:
`var()` substitutes its tokens, so its value is kept as written, apart from
whitespace.

### What runs

Value-level rewrites:

- **Colours:** hex when no longer than the name (`black` -> `#000`,
  `blue` -> `#00f`; `red` stays a name). A legacy sRGB colour (hex, named,
  `rgb()`, `hsl()`, `hwb()`) folds to hex or a name, and any other colour
  (`lab`/`lch`/`oklab`/`oklch`/`color()`) keeps its notation, its coefficients
  rounded within the ΔE<sub>OK</sub> budget below: CSS Color 4 sec. 13.3
  interpolates the two kinds in different spaces, so `color(srgb 1 0 0)` is not
  `red` in a gradient or a transition. A static `color-mix()` folds to a colour
  in its interpolation space (`color-mix(in srgb,red,blue)` ->
  `color(srgb .5 0 .5)`), and a `none` component is kept.
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
first. Under `--objective=raw` a saving is the bytes the rewrite removes. Under
`--objective=transfer` it is an estimate of the compressed bytes, in the way
LZ77 reads text: each selector branch and each declaration is a token, and the
consumed rules, in the order they print, are costed against the produced
rules. A token new to the list costs 2.42 bytes and 0.061 per byte of its
text. A token printed earlier in the list starts a match, 1.32 bytes and
0.005 per byte for a declaration, 1.47 and 0.029 per byte for a selector
branch, and the tokens after it extend the match for their per-byte part
alone while they repeat, in order, what followed its earlier copy. These are
least-squares fits of what gzip `-9` saves when one token or one match is
deleted from the corpus output. So lifting declarations out of rules that
repeat them saves a match each, and the leftover each member keeps costs a
match for its selector printed a second time; twenty sprite rules sharing
three declarations stay as written. The estimate reads the candidate's rules
alone: a token the rewrite neither creates nor deletes costs the same before
and after it, wherever else the sheet prints it.

Applying a candidate updates the graph; when the queue drains, the candidates
of the neighbourhood the commits touched are worked out again, and every
candidate whose refusal no longer stands is queued again, at every size. The output is a topological order of the final graph, with the first
source appearance as the tie-break, so rules keep their source order wherever
the cascade does not force another.

The graph keeps that order as rewrites land, in an order-maintenance list
(Dietz and Sleator; the list labelling of Bender et al.) that tells which of
two rules prints first in constant time, rather than working it out at the
end. A merged rule's first source appearance is the earliest of the rules it
replaces that share a selector branch with it, or of all of them when none
does. It prints right after the last rule it must follow, as soon as no rule
free to come first appears earlier in the source; the replaced rules stay in
the order as relays, so every order they imposed still holds. When that last
rule prints after the merged rule's first appearance, the rules the merged
rule must precede that printed before it wait for it, and come after it
interleaved by first appearance with the rules that follow; every other rule
keeps its place. Each rule's slot is the largest first source position up to
it in that order: where it prints, on the source scale.

The candidate set is bounded and deterministic, and reads the rules a run
prints and their order, never where they were written. Rules sharing a
declaration, or writing a property once, form a bucket, and the factoring
candidates are runs of up to 24 neighbouring members of a bucket, in the order
they print, wherever that is; a bucket with more than 256 such runs offers
none. One budget of 256 subsets per kind is spent on the buckets in the order
their first members print, then by size, then by where the key's declaration
stands in that first member. Under `--objective=raw` a bucket of up to 12 members
is tried whole, less one member and as runs of neighbours, a larger one as up
to 32 runs, and no budget is shared. Subsets that factor the same declarations
out of the same rules are one candidate. The test for a rule standing between the members of a
merge reads the printed order, and the provider of a default is the member
printed first. Equal savings rank by rewrite kind, then by the produced rules
one by one, shortest first and then by their text, then by the slots of the
consumed rules when the candidate was queued, earliest first; never by a hash
or the order of a table.

Rules with one body form a bucket too. Under `--objective=transfer` its
members merge in runs, read in the order they print, since the estimate sees a
selector's text but not its neighbours: a selector moved far from the rules
it shares text with stops compressing against them, and without the bound the
corpus output grows by 1.1% under gzip. A run starts at the first
member no earlier run holds. It takes every member printed within 128 rules of
that one, counting only the rules that print. A commit anywhere changes how many
rules print between two members, so every time the queue drains each bucket of
two or more members is split again, and offered again where its runs changed.
While at most 129 rules print, no run can be split. Under `--objective=raw` a
bucket merges whole.

These bounds read the rules a run prints and their order, and a rewrite is
accepted or refused on the live rules and where they print, so a second
`--minify`, which builds its graph afresh from the output, offers the same
candidates and refuses them all. One question still reads the rules earlier
rewrites consumed: whether a member overriding a default precedes its provider,
which drops the entry from the group. The run is factored once, and nothing is
checked or factored again to confirm it.

The pipeline makes one pass, at every size, and nothing runs to confirm it.
Each step finishes what it starts in that pass instead: a block merge
optimizes again the rule runs it joins, a hoist joins the blocks it leaves
adjacent, a dead nested rule takes the blocks it empties, a shorthand
composition deduplicates what it overrides, and the compatibility prefixes are
the ones a second pass would settle on, and a rule run that nesting changes is
optimized again. The corpus tests check that optimizing the output again
returns it. Of the 504 SatCSS sheets and Tailwind's two, none changes on a
second `--minify`.

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
- Each target stands for its version and every later one, up to the newest
  release in the compatibility data cascade ships. An earlier declaration of
  a property is dropped only when every such browser that reads it reads the
  later one, so `a{width:1px;width:stretch}` keeps `width:1px` for Chrome 111
  to 137, Firefox and Safari, which drop `stretch`. This holds under `--enforce-spec` too.
- A `min-`/`max-` media or container feature becomes the range syntax,
  `(min-width: 700px)` to `(width >= 700px)`.
- Nesting is flattened, `.a{color:red;.b{color:blue}}` to
  `.a{color:red}.a .b{color:#00f}`, and none is synthesised. Chrome 111 and
  Safari 16.4 read no nested rule, and Chrome 112 to 119 and Safari 16.5 to
  17.1 refuse one whose selector starts with an identifier (MDN's
  browser-compat-data, `css.selectors.nesting`). Under targets from Chrome
  120, Firefox 117 and Safari 17.2 an author's nesting stays, a nested
  selector loses its `&` prefix (`& div` to `div`), and adjacent rules may
  nest. A rule holding a declaration after a nested rule, or one directly in a
  nested group rule (a nested declarations rule, CSS Nesting 1 sec. 3.4 and
  5), stays from Chrome 130, Firefox 132 and Safari 18.2
  (`api.CSSNestedDeclarations`) and is flattened under older targets, since
  releases before nested declarations rules read it in another order.
- An `oklab` or `oklch` axis takes the percentage spelling where it is
  shorter, `oklch(.7 .304 20)` to `oklch(.7 76% 20)`.
- A rule whose selector holds, outside `:is()` and `:where()`, a pseudo-class
  no target reads is dropped by the reader, with or without `--minify`, and a
  warning names it, since every target drops the whole rule (Selectors 4
  section 3.9). The list comes from MDN's browser-compat-data, Firefox's
  selector parser and Chrome and WebKit: `:-o-prefocus`,
  `:-moz-submit-invalid`, `:selected`, `:-moz-any-link`, `:-ms-fullscreen`,
  `:-ms-full-screen`, `:-o-full-screen`, `:full-screen`, and the
  pseudo-element names written with one colon, such as
  `:-webkit-input-placeholder` and `:selection`, that only the CSS 2.1 four
  (`:before`, `:after`, `:first-line`, `:first-letter`) may take.
  `:-moz-placeholder` is not one: Firefox still reads it.

`--enforce-spec` removes all of these, and the reader keeps such a rule as
written, apart from every other selector. It also keeps the quotes around a
multi-word font family, and it holds the parser to the ident code points CSS
Syntax 3 lists rather than accepting anything above U+007F; the two reader
parts apply even without `--minify`.

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
| `--enforce-spec` | Judge the `canonical` comparison for every conforming implementation rather than the targets: every `@supports` guard, vendor prefix, colour fallback and rule over a pseudo-class no target reads is kept, so two sheets that differ on one are reported. |
| `--prune-unused-custom-props` | Drop custom properties nothing references, on both sides, before a `canonical` comparison. The comparison is then blind to differences in unused custom properties. |
| `--browser --html FILE` | Render both files over `FILE` in headless Chromium and compare the screenshots instead of the CSS. |
| `--json` | Write the comparison as one JSON document. The exit status is unchanged. |
| `--color=WHEN` | `auto` (default), `always` or `never`. `CASCADE_COLOR` sets the same thing; `NO_COLOR` overrides both. |
| `-q, --quiet` / `-v, --verbose` | Standard verbosity controls. |

### The canonical comparison

`--diff=canonical` parses and minifies each input independently with the same
settings, then compares the two results byte for byte. `--lossless` applies to
both sides, and the comparison applies the minifier's approximation bounds and
none looser. Before comparing, it treats as equal the changes that cannot
alter a used value:

- declarations or rules that set different properties may swap freely;
- a rule inside a `@media`, `@supports` or `@container` block holding only
  plain rules may move past statements it cannot conflict with, so how rules
  are grouped into such blocks does not matter;
- distinct custom properties may swap within a rule;
- different factorings of the same content compare equal, such as a
  declaration hoisted into a shared selector list versus written inline;
- a declaration may stand in any occurrence of its selector that no
  conflicting write separates it from, so how a selector's declarations are
  split into rules, and which of its rules were merged, does not matter;
- where a layer block stands among unlayered rules, and whether layer order is
  declared up front or implied by block order, does not matter, since the
  cascade sorts by layer first;
- the order of a compound selector's simple selectors does not matter
  (Selectors 4 sec. 3.1), so `.a.b` equals `.b.a`, while a type selector
  stays first and a pseudo-element last;
- where a `@keyframes` or `@font-face` rule stands among style rules does not
  matter, while two of one name or font family keep their order;
- nested `@media` blocks equal the one block of their conjunction:
  `@media screen{@media (min-width:1px){...}}` equals
  `@media screen and (min-width:1px){...}`, while
  `@media print{@media screen{...}}` matches no medium.

Order that the cascade depends on stays significant: two writes of the same
property, a shorthand and its longhand, a vendor-prefixed fallback that
matters, and two `@layer` blocks of one layer.

A prefixed declaration that the
[WHATWG Compatibility Standard](https://compat.spec.whatwg.org/#css-legacy-name-aliases)
section 3.4.1 lists as a legacy alias is the same property as its unprefixed
twin, so `-webkit-transform` beside an identical `transform` normalises to
`transform` alone. A prefix the list does not name, such as the `-moz-`
spellings or `-webkit-user-select`, is a property of its own, and so is a
listed alias whose unprefixed twin some target browser does not read: no
Chromium or Firefox release reads `mask-border`, so the list's
`-webkit-mask-box-image` set stays distinct from it. A value is dated per
name, too: Chrome 111 to 119 parse `text` under `-webkit-background-clip` and
not under `background-clip`, so `-webkit-background-clip:text` and
`background-clip:text` compare different, while `content-box`, which both
names parse in every target, compares as one property.
`-webkit-text-decoration-color` is not on the list, but every engine that
ships it treats it as an alias, and so does cascade. Equivalent shorthand
decompositions are not modelled. A shorthand and the parts it is composed of
stay different where it resets a slot they leave out: `border` resets
`border-image`, which a sheet before or after the input may set, so it
differs from `border-width`, `border-style` and `border-color` and from its
four sides.

Numbers follow the same precision as minification. An exact quotient such as
`calc(28/14)` equals `2` in either mode. By default `calc(28/18)` equals
cascade's six-figure `1.55556`; under `--lossless` it equals no finite decimal.

For browsers, the comparison assumes the targets `--minify` uses: a
`@supports` guard every target satisfies is unwrapped, a prefix a target
needs is added on both sides and one no target needs is dropped from both, a
colour fallback every target reads past is dead, a `-ms-` or `-o-`
declaration, which no target reads, is dropped from both, and so is a rule
whose selector no target reads ([Target browsers](#target-browsers)).
Adding a prefix a target needs is the comparator's side of the one rendering
change `--minify` makes on purpose ([M1 and M2](invariants.md#master-guarantees)):
`.a{user-select:none}` compares equal to its minified
`.a{-webkit-user-select:none;user-select:none}`, though Safari applies only
the second.
Each target stands for its version and every later one in the data, so a
`@supports` guard no target satisfies stays: `@supports (field-sizing:content)` fails in
Chrome 111 and holds from Chrome 123.
`--enforce-spec` turns these off. An `@supports` guard that no browser can answer yes to, such as one
over a `<general-enclosed>` term that CSS Conditional 3 section 6.1 makes
false, selects nothing on either side, and its block is dropped from both.

`canonical` exits 0 only when every target browser renders the two files
alike, in the sense of [invariants.md](invariants.md#master-guarantees). It
may report a difference that paints nothing. Content that changes nothing an
element renders, an empty rule for example, is no difference; use `tree` or
`string` to catch that.

### Unreadable input and exit codes

A difference cascade can see exits 1, whether or not it read everything. When
it finds no difference but could not read a rule in one of the files, it
compares the text each side dropped. If that text matches byte for byte, it
cannot hide a difference and the exit is 0. If it differs, the exit is 2,
because one side dropped a rule the other did not.

A rule dropped because its selector holds a pseudo-class no target reads is
not counted against the verdict in `canonical` mode: every target drops it too,
and the comparison judges for those targets. Under `--enforce-spec` the reader
keeps that rule on both sides, so a sheet that lost it differs and the exit is
1. A pseudo-class cascade does not know at all is not on that list, and a rule
lost to it still withholds the verdict.

A declaration cascade cannot read is handled differently. The verdict takes
one the reader refuses as one every target drops; a refused declaration that
a target keeps is a reader bug, which the harnesses below look for. It is dropped on that side,
a parse warning is printed, and the verdict is taken over what remains. The
report and the `--json` document count the unreadable declarations and rules
on each side.

Two browser harnesses test the reader against Chromium in both directions.
`test/spec/browser/accept_set` asks the browser about values generated for
every property the reader dispatches on, and reports a value the browser keeps
that the reader drops as well as the reverse. `test/render/parse_recovery`
checks that, over malformed CSS, the reader keeps the rules and declarations
the browser's CSSOM keeps.

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
- Nesting is flattened first.

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

## Release criteria

For each of the 504 SatCSS sheets and Tailwind's output, `cascade diff
--diff=canonical SHEET <(cascade fmt --minify -q SHEET)` exits 0 under the
default and under `--objective=raw`; an exit 1 or 2 blocks the release until
fixed. Canonical diff reads both sides through one normaliser, so each minimal
reduction of a change is also rendered against its source with `cascade diff
--browser`.
