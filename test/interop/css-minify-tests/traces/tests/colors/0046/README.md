# oklch/oklab excess precision rounds to 3 decimal places

oklch and oklab use a 0-1 lightness scale and small chroma/a/b ranges. dEOk JND
is 0.02, so 3dp (0.001) on L/C/a/b gives two orders of magnitude headroom.
Hue at 1dp (0.1 deg) contributes ~0.0003 dEOk - effectively zero. A minifier
can round e.g. `oklch(0.55432 0.12276 180.4567 / 0.74567)` to
`oklch(.554 .123 180.5/.746)` with no perceptible change. Alpha is 0-1, so 3dp
applies there too.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
