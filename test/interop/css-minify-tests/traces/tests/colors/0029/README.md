# color-mix in oklch resolved to oklch value

`color-mix(in oklch, red 50%, blue 50%)` can be resolved at build time to
`oklch(.54 .285 326.6)`. A minifier that resolves the mix must do so in oklch
space -- mixing in srgb produces a different color. Keeping the result in oklch
avoids lossy gamut mapping to sRGB hex. Precision is rounded to 3dp (L/C) and
1dp (hue) -- well below JND. Lightness uses the number form (`.54`) rather than
percent (`54%`) -- same length but consistent with always preferring numbers
over percentages since percentages are longer for other components.

For a lot more detail on rounding, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
