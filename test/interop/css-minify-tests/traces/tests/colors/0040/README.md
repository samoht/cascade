# Inner colors minified inside color-mix()

`color-mix(in oklch, rgba(255, 255, 255, 1), currentcolor)` — the color-mix
cannot be statically resolved because `currentcolor` is dynamic, but the inner
`rgba(255, 255, 255, 1)` can be shortened to `#fff`. Minifiers should optimize
individual color arguments even when the overall mix must be preserved.
