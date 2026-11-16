# Cascade

A CSS generation and manipulation library and CLI: a typed CSS AST, parser,
pretty-printer, structural diff and optimizer, with no Tailwind-specific code.
`README.md` covers what the tool does, how it is built and tested, and the
oracle corpora the suite replays.

## Scope

Cascade is scoped to CSS text and CSS ASTs: parse, print, minify, diff,
fold/map/sort, and safe AST transforms. A context-supplied evaluation is in
scope when the caller passes the data in through an explicit closed context
record; `Css.Context.t` is that type for property-value transforms, and
theme-based `var()` output is the worked example. Tailwind-specific code, and
Cascade APIs that exist only to serve Tailwind, belong in the `tw` project.
A tw parity difference that comes from tw binding a typed node into a custom
property, or from an unreferenced custom-property binding, is fixed in tw's
codegen, not by weakening cascade's minifier or its comparator.

`lib/` is flat and large. Read the interface next to a module rather than
guessing from its name, and prefer extending an existing pass to adding one.
CSS semantics (matching, the cascade, specificity, nesting) live in the
library (`Resolve`, `Flatten`); a `bin/` command is a thin caller.

## Working on it

- Start a behavioural change with a test that fails on current `main`. A test
  that passes before the fix is pinning something else.
- Tests encode the spec. When a test oracle contradicts the code, derive the
  expectation from `README.md` and the fetched spec text: the shortest
  spelling that is correct under the spec and renders the same in browsers.
  Fix whichever side departs from it, cite the section by title and document
  (the Editor's Draft and the TR snapshot number the same algorithms
  differently, and the code follows the Editor's Draft), and land an oracle
  change as its own commit. Ask only when the spec is silent or the spec and
  shipping browsers disagree in a way users see; a ruling on one at-rule or
  property is not precedent for another.
- Never rewrite an expected output to match what the code prints, never set
  an expectation by calling the function under test, and never read `lib/` to
  decide what an expectation should be. The oracles are the spec, the other
  minifiers (lightningcss, esbuild, csso, clean-css, cssnano) and the browser.
- For static colour folding the imported css-minify-tests fixtures are
  authoritative; for syntactic validity and shorthand canonicalisation cascade
  stays spec-faithful and a spec-loose fixture gets an override in
  `test/interop/css-minify-tests/test.ml`.
- Sealed ADTs over open extension: nothing outside this repo adds a variant, so
  a new case should stop every site that decides about it from compiling.
- Thread configuration (`scope`, `enforce_spec`, targets) as arguments; no
  module-level `ref` cells, for configuration or as forward declarations.
- The optimizer matches the typed property GADT, never property-name strings.
  A longhand that parses as `Unknown_property` is typed first.
- Build a string with `Buffer`, the internal `Pp`, or `String.concat ""`, not
  with `^`, `Printf.sprintf` or `Fmt.str`. Tests and fuzzers use `Fmt`.
- Source files are 7-bit ASCII (`scripts/check_ascii.sh`, run by the
  pre-commit hook): write "sec. 4.3.5", `--`, `->`, and put a glyph a string
  must emit behind a `\u{XXXX}` escape.

## Invariants

The printer, optimizer and canonical-diff invariants are in
[docs/invariants.md](docs/invariants.md); read it before changing `pp`, the
normalize pass, factoring or `rule_order.ml`.

## Building and testing

- Build through the project switch: `scripts/with_switch.sh dune build @check`.
  Bare `dune` can pick another compiler and leave `_build` with cmt files of
  two magics, which merlint reports as unchecked.
- Cram (`test/cli/*.t`) and mdx failures print a diff, never `[FAIL]`, so a
  grep for a failure marker misses them; check the exit status as well.
- Run merlint after `dune build @check @default`, once, over every touched file
  together; it applies fewer rules to a single file. Its exit status is 0 with
  findings, so read the output and the unchecked-file count. CI's merlint can
  be newer than the local one (it catches polymorphic comparison, E106).
- To ask a browser, run headless Chrome with `--dump-dom` over a page that
  writes its answer into the DOM or `document.title` (`el.style.<prop>`,
  `document.styleSheets[0].cssRules`, `getComputedStyle`), with every
  candidate in one stylesheet. When CI and a clean local run disagree, check
  which Chromium the harness found
  ([docs/reference.md](docs/reference.md#test-corpora)) before suspecting the
  code.
- Validate an optimizer change on the full SatCSS corpus with
  `bench/corpus_sweep.sh -b <baseline>` (CPU time, interleaved, byte identity),
  not on a pair of files. Measure allocation and CPU together and vary the
  input's shape as well as its size. Rebuild the baseline after every rebase.
- Work from worktrees outside the checkout, never under it (the pre-commit
  hook's `dune fmt` refuses a nested one), and never `git stash` in the
  maintainer's checkout.

## Pull requests

- One independent change, one PR, one `CHANGES.md` entry ending in `(#N)`,
  backfilled after the PR exists.
- All open PRs form one linear `gh stack`. A new PR goes on the tip, and
  `gh stack link <stack> <pr>` registers it, since `--base` alone does not. A
  stack holds 100 PRs; the next one is a second stack rooted on the first's
  top branch with `gh stack init --base <top> ...`.
- `gh stack link` moves the base graph, not commits, and `MERGEABLE` is not a
  check. After moving any branch, walk the whole chain with
  `git merge-base --is-ancestor origin/<base> origin/<branch>` and the commit
  count, and restack bottom to top with `rebase --onto` from each branch's
  recorded old head. A branch amends itself once when it backfills its PR
  number, so anything rebased onto it earlier is stale.
