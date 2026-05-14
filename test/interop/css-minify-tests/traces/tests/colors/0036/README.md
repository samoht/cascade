# color-mix() with 100% first color eliminates to that color

`color-mix(in srgb, red 100%, blue)` means 100% of the first color and 0% of
the second. The result is just `red`. Minifiers should detect this and replace
the entire expression with the shortest form of the first color.
