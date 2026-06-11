# color-mix with var(): minify hsl to named color

`hsl(0, 100%, 50%)` inside a `color-mix()` with a `var()` second argument can
still be shortened to `red`. The var() prevents resolving the mix, but each
literal color argument is independently minifiable.
