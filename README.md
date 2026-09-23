# Cascade

Cascade is a CSS toolkit written in OCaml. One command-line tool, `cascade`,
formats and minifies stylesheets, compares two stylesheets by what they do
rather than how they are written, inlines a stylesheet into an HTML page, and
removes the rules a set of pages never uses. The OCaml library it is built on
is available too.

Cascade parses CSS into a typed syntax tree and works on that tree rather than
on the text. The minifier can therefore reason about the cascade (which rule
wins, and whether moving a rule changes anything) and always prints valid CSS.
The diff reports changes by rule and declaration, so a refactor that only
reorders or regroups rules shows up as no difference. Correctness is checked
against Chromium: the test suite renders stylesheets and their minified forms
and compares the pages pixel for pixel.

<!-- $MDX skip -->
```sh
cascade --minify style.css > style.min.css
cascade diff --diff=canonical style.css style.min.css
```

The minified output renders the same, but it is not byte-for-byte what you
wrote. By default it targets maintained evergreen browsers (Chrome 111,
Firefox 128, Safari 16.4), may round a colour by up to 0.002 in the Oklab
colour space, and normalises spellings, including custom-property strings a
script could read back. For a more conservative result, turn off the rounding
and the browser assumptions:

<!-- $MDX skip -->
```bash
cascade --minify --lossless --enforce-spec style.css
```

Comments, whitespace and the exact source spelling are still not kept.

On the SatCSS corpus of real-world stylesheets, the minifier is competitive
with esbuild, Lightning CSS, CSSO and cssnano on both size and speed;
[BENCHMARKS.md](BENCHMARKS.md) has the numbers.

## Install

With Homebrew:

<!-- $MDX skip -->
```sh
brew install samoht/tap/cascade
```

With opam, which also installs the `cascade` library (OCaml 5.2 or later):

<!-- $MDX skip -->
```sh
opam install cascade
```

## Formatting and minifying

`cascade fmt` is the default command, so `cascade style.css` and
`cascade fmt style.css` are the same. Without options it pretty-prints; with
`--minify` it produces the smallest CSS it can prove renders the same.

<!-- $MDX skip -->
```sh
cascade style.css > style.formatted.css
cascade --minify style.css > style.min.css
cat style.css | cascade --minify -
```

The two assumptions described at the top each have a switch:

- `--enforce-spec` stops targeting current browsers. Cascade then keeps
  vendor prefixes and older syntax, and relies only on what the CSS
  specifications guarantee.
- `--lossless` stops rounding. Colours and repeating decimals from `calc()`
  stay exact, and declarations keep the author's order.

By default the minifier optimises for the size after gzip compression,
because servers usually send CSS compressed. `--objective=raw` optimises the uncompressed size
instead, for CSS that ships uncompressed such as email HTML; it is noticeably
slower. On the SatCSS corpus, `--minify` takes about 2.2 ms per KB of input, so
a 200 KB stylesheet takes a few hundred milliseconds.

A few options trade safety for size, and are off by default:

- `--inline-imports` replaces `@import` with the imported file.
  `--import-root=DIR` limits which files it may read, which matters for
  untrusted CSS.
- `--inline-vars` replaces `var(--name)` with its value, assuming no script
  changes the variables at runtime. `--keep-vars=theme,brand` keeps some of
  them live.
- `--scope=stylesheet` tells the minifier it sees all the CSS of the page, so
  it can use shorthands that would otherwise reset properties set elsewhere.
- `--closed-world` tells it that no element ever matches two conflicting
  selectors, which allows more merging. The page renders wrongly if one does,
  including an element a script adds later.

`cascade fmt --help` lists every option.
[docs/reference.md](docs/reference.md#minification) describes exactly which
rewrites run and why each one preserves the rendering.

## Comparing stylesheets

`cascade diff` parses both files and reports added, removed and changed rules
and declarations. Here `a.css` is `.ocaml{all:unset}.ocaml{visibility:hidden}`
and `b.css` has the same two rules in the other order, so the element ends up
visible:

<!-- $MDX skip -->
```sh
cascade diff --diff=canonical a.css b.css
```

```text
CSS: 54 chars vs 54 chars (0.0% diff)
Changes: 1 modified rule

--- a.css
+++ b.css
└─ .ocaml
      - visibility: hidden
```

`--diff` chooses what counts as a difference:

- `auto`, the default, compares the structure, then falls back to comparing
  the text so that formatting changes still show.
- `tree` compares only the structure, so formatting changes do not count.
- `string` compares the text.
- `canonical` minifies both sides the same way and compares the results. Two
  stylesheets are the same when every element would get the same computed
  values, however the rules are written, grouped or ordered. Like the
  minifier, it assumes current browsers and allows the same rounding;
  `--enforce-spec` and `--lossless` turn those off.

`canonical` is the right mode for checking that a build step, a minifier or a
refactor did not change the page. It can over-report: a difference it lists is
a candidate, and some of them paint nothing. The exact rules are in
[docs/reference.md](docs/reference.md#comparing-stylesheets).

To settle a doubt, `--browser` renders a page under both stylesheets in
headless Chromium and compares the screenshots, at every viewport width and
interaction state the stylesheets mention. Where pixels differ, it lists the
computed values that changed:

<!-- $MDX skip -->
```sh
cascade diff --browser --html page.html old.css new.css
```

The page's own `<style>` elements and stylesheet links are removed first, so
each file is judged alone. This needs node and a headless Chromium (set `NODE`
and `CHROME`, or install one with `npx playwright install
chromium-headless-shell`).

`cascade diff` exits with 0 when the files are the same, 1 when they differ,
and 2 when it could not decide: a browser was missing, or a file contained a
rule cascade could not read and the two files dropped different text. A script
should treat 2 as a failure.

## Inlining styles into a page

`cascade apply` moves styles from a page's `<style>` blocks (and an optional
extra stylesheet) into each element's `style` attribute, as email HTML needs:

<!-- $MDX skip -->
```sh
cascade apply page.html theme.css > inlined.html
```

Selector matching, specificity and `!important` decide which declaration wins,
as in a browser. Rules that cannot be inlined, such as `:hover`, `@media` or
pseudo-elements, stay in a `<style>` block, along with every declaration they
compete with. For example, with

```css
.btn { color: #fff; padding: .5rem 1rem }
.btn:hover { color: #eee }
```

the padding moves onto the element but both colours stay in CSS, because an
inline colour would always beat the `:hover` rule. `--minimal` also drops
declarations that only repeat an inherited value.

## Removing unused rules

`cascade prune` removes the rules that match no element of the given pages:

<!-- $MDX skip -->
```sh
cascade prune --dry-run src/*.html style.css      # report what would go
cascade prune src/*.html style.css > style.pruned.css
```

A rule is removed only when cascade can evaluate its selector and no element
matches it. Selectors it cannot evaluate statically (`:hover`, pseudo-elements,
`:lang()`) are kept, and so are `@keyframes`, `@font-face` and the like.
`@media` conditions are not evaluated.

**Pruning only sees the pages you give it.** A rule for a class that a script
adds at runtime will be removed, so check what your scripts add before
shipping a pruned stylesheet.

## Using cascade in a build

A typical setup minifies during the build and uses `diff` as a check:

<!-- $MDX skip -->
```sh
# build
cascade --minify src/style.css > dist/style.min.css

# CI: fail if the committed file is not formatted
cascade src/style.css > formatted.css
cascade diff --diff=tree src/style.css formatted.css

# before committing: fail if a change alters what the CSS does
git show HEAD:src/style.css > previous.css
cascade diff --diff=canonical previous.css src/style.css
```

## Using the library

The command-line tool is a thin wrapper around the `cascade` library. CSS is
built from typed values, so an invalid property or value is a compile error:

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

`Css.of_string` parses a stylesheet, `Css.optimize` minifies it, and
`Css.inline_imports` and `Css.resolve_theme` do what the matching command-line
options do. The diff lives in the `cascade.diff` sub-library. The core library
depends only on `uri`, `psq`, `logs` and `mtime`, and compiles to JavaScript
with js_of_ocaml in under 200 KiB compressed.
[docs/reference.md](docs/reference.md#the-library) covers parsing modes,
source positions and theme resolution.

## Limitations

- Cascade reads UTF-8 only. Other encodings must be converted first.
- It has no layout engine or DOM. Analyses that depend on the page, such as
  `apply` and `prune`, work from the HTML they are given.
- Comments and source positions are not kept through formatting. Library
  callers who need them can parse with `~preserve_source:true`.
- The values of custom properties are mostly left as written, since their
  meaning depends on where they are used. Complete colour values, and
  constant `calc()` expressions that reduce to an angle or a time, are still
  shortened.

The CSS modules cascade supports are listed in
[docs/reference.md](docs/reference.md#css-coverage).

## Development

<!-- $MDX skip -->
```sh
dune build
dune test      # includes the examples in this README
dune fmt
```

The browser tests need a headless Chromium; without one they are skipped. The
test suite also replays the Web Platform Tests for CSS syntax, the Lightning
CSS minifier tests with the answers of five other minifiers, and
[css-minify-tests](https://github.com/keithamus/css-minify-tests).
[docs/reference.md](docs/reference.md#test-corpora) explains each corpus and
how to regenerate it. Every user-visible change gets an entry in
[CHANGES.md](CHANGES.md).

## Related work

[Lightning CSS](https://github.com/parcel-bundler/lightningcss),
[esbuild](https://github.com/evanw/esbuild),
[CSSO](https://github.com/css/csso), [cssnano](https://github.com/cssnano/cssnano)
and [clean-css](https://github.com/clean-css/clean-css) are the minifiers
cascade is tested against. The rule-merging approach follows
[Hague, Lin and Hong (2018)](https://arxiv.org/abs/1812.02989), who treat
merging as a graph problem over the cascade's ordering constraints.
[Visscher, Punt and Zaytsev (2016)](https://grammarware.net/text/2016/aba-css.pdf)
catalogue patterns where a property is set, overridden and restored, which
make good test input. [CILLA](https://github.com/saltlab/cilla) finds unused
selectors by running the page, which catches the runtime classes that
`cascade prune` cannot see.

## Licence

ISC. See [LICENSE](LICENSE).
