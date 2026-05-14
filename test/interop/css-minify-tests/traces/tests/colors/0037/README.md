# color-mix() with 0% first color eliminates to the second color

`color-mix(in srgb, red 0%, blue)` means 0% of the first color and 100% of
the second. The result is just `blue`, whose shortest form is `#00f`. Minifiers
should detect this and replace the entire expression with the second color.
