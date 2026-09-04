# Differential render tests

Two transforms must leave the rendered page unchanged: for every element, the
**complete `getComputedStyle`** in a real headless browser must be identical
before and after.

- **`cascade apply`** resolves the stylesheet into each element's `style`
  attribute and empties the `<style>` blocks, in both the default (full) and
  `--minimal` modes.
- **`cascade --minify`** rewrites each `<style>` block in place (via
  `minify_page.js`), on its own and again with `--inline-vars`. Idempotence,
  size and reparse checks all pass on output that renders differently; only
  this catches a rewrite that changes the computed style.

The browser and page-rewrite primitives are also the render adapter for
[`bench/differential/run.sh`](../../bench/differential/run.sh). That suite sets
`MINIFIER_CMD` to compare an external minifier without maintaining a second
copy of either JavaScript driver. `REPORT_FORMAT=tsv`, `RAW_OUT` and `FILT_OUT`
expose the same comparison as reproducible investigation artifacts; the normal
test output is unchanged.

`dune runtest` runs the comparison over `fixtures/`. Run it by hand to include
the pages `fetch.sh` downloads:

```sh
sh test/inline/run.sh
```

The binary under test is the working tree's: `run.sh` builds `bin/main.exe`
through the checkout's own opam switch and heads its output with the path and
version it resolved. `CASCADE` overrides it with a path, to measure an
installed release on purpose. There is no fallback to a `cascade` on `PATH`,
which would report on whichever release is installed while reading as a result
about the branch.

It looks for a headless Chrome in `$CHROME`, on `PATH`, under the puppeteer and
playwright caches (highest version, ordered numerically), then in the macOS
application bundle, and skips cleanly if none is found, so it is a no-op in CI.
`CASCADE_NO_BROWSER` is not that: silencing a gate is not a pass, so it exits
non-zero for every value but `unchecked`, which exits 0 and says the run checked
nothing. `xtest.js` appends an extractor script, runs the browser with
`--dump-dom`, and diffs the computed styles element by element.

## Reproducibility

A difference list is a measurement, so it has to repeat: run the same
comparison twice and diff the two outputs, and they match byte for byte.

That takes both halves of the harness. `fetch.sh` freezes each downloaded page
through `freeze_page.js`, which removes everything that resolves at a moment
nobody controls: scripts, `@font-face` rules whose relative `.woff2` cannot be
read under `file://`, and images with an unreachable `src`. `xtest.js` then
pins the browser, which otherwise brings its own variables (window size,
device scale factor, scrollbars, name resolution). Without them, four runs
over one unchanged pair of pages reported 14, 410, 14 and 14 differences.

Freezing happens at fetch time, before `cascade apply` ever sees the page, so
the document cascade resolves is the document the browser lays out. Pages live
in the gitignored `pages/`; re-run `fetch.sh` to rebuild them.

Freezing and pinning are what make a page reproducible; neither shows that it
is. So `run.sh` renders each fetched page twice with nothing transforming it in
between (`xtest.js --self`) before it measures anything, and unless the two
renders agree it reports the page as unusable and skips its transform legs. A
page that differs from itself yields a difference list about nothing, and the
run would otherwise hand that list to whichever transform it was measuring.
Fixtures are exempt: they are committed and change only under review, where a
page arrives off the network and goes stale on its own.

Two renders catch a page that is unstable on those two, which is not the same
as catching an unstable page: a `tailwind.html` frozen before the freezer
learned to strip `@font-face` differed from itself on three self-checks in
eight. So the run also re-freezes each page and requires that to be a no-op.
`freeze_page.js` is idempotent, so a page it still changes was frozen by an
older one and is stale whatever it renders today. That question is
deterministic and costs milliseconds, so it is asked first, and `fetch.sh` is
the answer to it.

## What a count is comparable to

Repeating on one machine is half of it. A count also has to say what produced
it, and two of its three inputs sit outside the repository.

The **browser** is one: Chrome versions disagree about computed styles, so a
count is comparable only against another from the same engine. `run.sh` heads
its output with the version it used. Set `CHROME_VERSION` to a substring of the
expected version and the run stops unless the browser matches, which is how a
comparison crosses machines; leaving it unset reports the version without
demanding one, so a machine with a different build still runs.

The **pages** are the other: `fetch.sh` downloads them live, and a CDN serves
whatever it serves that day. Committing them would pin the bytes at the cost of
about a megabyte of third-party CSS in every clone, kept fresh by hand, so the
harness records them instead of pinning them. `fetch.sh` writes `pages/MANIFEST`
with a sha256 per stage (the page off the wire, the CSS the CDN served, the
frozen page), and `run.sh` prints the frozen page's hash in each real-page
label. A count that moved because the site moved shows up as a changed hash
next to it.

The **canonical filter** is the third input, and it lives here. `canon_filter.exe`
compares values the way cascade does, so `0%` against `0px`, or `red` against
`rgb(255, 0, 0)`, is not reported. Comparing raw strings instead counts every
such pair, which inflates the total into a number that reads like a result and
is not one. `run.sh` therefore builds the filter through the checkout's own
opam switch and proves it filters, on two pairs that must be dropped and one
that must survive, before measuring anything; a filter that is missing or wrong
stops the run rather than downgrading it. One dropped pair is vendor-prefixed,
because cascade folds a colour only for a property it types, and a real page's
prefixed colours outnumber all its other differences.

## Coverage

`fixtures/` is committed and always runs. `pages/` holds real sites downloaded
by `fetch.sh`, and they gate the run in the same way: a surviving difference is
a defect in a transform, whichever page found it. A failure that starts the day
a site is redesigned is still a defect, but re-run `fetch.sh` before reading it
as a regression in the working tree.

Every page runs both transforms. `apply` and `--minify` fail differently, and a
real page carries selectors, feature queries and at-rules no fixture does, so a
minify defect only a real page reaches stays unmeasured until the leg exists.
The four pages cost about 25 seconds between them.

Positions are how the two element lists line up, so the comparison runs only as
far as the first index whose tags disagree. Past a dropped or added element
every later index compares two different elements, and one structural change
would report as thousands: `xtest.js` names the index instead and says how many
elements it did not compare.
