# color-mix() with 0% second color eliminates to the first color

`color-mix(in srgb, red, blue 0%)` means the second color contributes nothing.
The result is just `red`. Minifiers should detect this and replace the entire
expression with the shortest form of the first color.
