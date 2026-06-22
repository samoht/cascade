# `cascade apply` differential test

`cascade apply` resolves a stylesheet into each element's `style` attribute and
drops the `<style>` blocks. This checks that the result is equivalent to the
original: for every element, the **complete `getComputedStyle`** in a real
headless browser must be identical between the original page and the inlined
output, in both the default (full) and `--minimal` modes.

```sh
CASCADE=_build/default/bin/main.exe sh test/inline/run.sh
```

It looks for a headless Chrome on `PATH`, in `$CHROME`, or under the puppeteer
cache, and skips cleanly if none is found (so it is a no-op in CI). `xtest.js`
injects an extractor script, runs the browser with `--dump-dom`, and diffs the
computed styles element by element.
