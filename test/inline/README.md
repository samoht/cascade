# Differential render tests

Two transforms must leave the rendered page unchanged: for every element, the
**complete `getComputedStyle`** in a real headless browser must be identical
before and after.

- **`cascade apply`** resolves the stylesheet into each element's `style`
  attribute and empties the `<style>` blocks, in both the default (full) and
  `--minimal` modes.
- **`cascade --minify`** rewrites each `<style>` block in place (via
  `minify_page.js`). Idempotence, size and reparse checks all pass on output
  that renders differently; only this catches a minification that changes the
  computed style.

```sh
CASCADE=_build/default/bin/main.exe sh test/inline/run.sh
```

It looks for a headless Chrome on `PATH`, in `$CHROME`, or under the puppeteer
cache, and skips cleanly if none is found (so it is a no-op in CI). `xtest.js`
appends an extractor script, runs the browser with `--dump-dom`, and diffs the
computed styles element by element.

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
