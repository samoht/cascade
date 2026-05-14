# rgb() none keyword resolves to 0 outside interpolation

`none` in `rgb(none 128 0)` means "missing channel". Outside an interpolation
context (like `color-mix`), missing channels resolve to `0`. So this is
`rgb(0 128 0)` = `#008000` = `green`.
