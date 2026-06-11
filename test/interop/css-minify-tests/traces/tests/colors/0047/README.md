# lch/lab excess precision rounds to 1 decimal place

CIE LCH and Lab use a 0-100 lightness scale, chroma up to ~150, and a/b axes
of roughly -128 to 128. dE00 JND is 2.0. At 0dp (integers) the worst-case
error is already sub-JND for L/C/a/b, but lch chroma at 0dp can hit ~0.5 dE00.
1dp gives two orders of magnitude headroom (worst-case ~0.05 dE00 for all
channels). A minifier can round e.g. `lch(54.321 43.765 274.456 / 0.74567)` to
`lch(54.3 43.8 274.5/.746)`. Alpha is 0-1, so 3dp applies there.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
