# color(display-p3) excess precision rounds to 3 decimal places

Display P3 channels are in the 0–1 range. 3dp is well below the perceptual JND
and provides sufficient headroom for chained colour operations. Alpha is 0–1, so
3dp applies there too.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
