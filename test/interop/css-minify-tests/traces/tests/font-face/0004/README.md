# format() string to keyword conversion

The `format()` function in `@font-face` `src` accepts both string and keyword
syntax per CSS Fonts 4. `format("woff2")` and `format(woff2)` are equivalent,
so quotes can be safely stripped. Whitespace between `url()` and `format()` can
also be removed.
