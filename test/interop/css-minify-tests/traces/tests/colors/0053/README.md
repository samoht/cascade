# color(xyz-d65) excess precision rounds to 4 decimal places

XYZ D65 channels in CSS `color()` cover a range beyond 0–1 (D65 white point
channels can exceed 1.0 for wide-gamut spaces). Like `srgb-linear`, this wider
range means 3dp can introduce visible rounding errors, so 4dp is required.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
