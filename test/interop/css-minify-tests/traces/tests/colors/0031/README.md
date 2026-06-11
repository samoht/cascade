# color-mix with var(): minify literal color argument

`rgba(255, 255, 255, 1.0)` inside a `color-mix()` with a `var()` second
argument can still be shortened to `#fff`. The var() prevents resolving the
mix, but the literal color is independently minifiable.
