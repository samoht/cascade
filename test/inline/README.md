# Differential render tests

Two transforms must leave the rendered page unchanged: for every element, the
**complete `getComputedStyle`** in a real headless browser must be identical
before and after.

- **`cascade apply`** resolves the stylesheet into each element's `style`
  attribute and drops the `<style>` blocks, in both the default (full) and
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
injects an extractor script, runs the browser with `--dump-dom`, and diffs the
computed styles element by element.
