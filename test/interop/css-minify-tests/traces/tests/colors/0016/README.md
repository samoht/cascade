# Modern rgb() with alpha 1 to named color

The modern space-separated `rgb(255 0 0 / 1)` syntax with full opacity should
minify to the shortest equivalent named color `red`, dropping the alpha channel.
