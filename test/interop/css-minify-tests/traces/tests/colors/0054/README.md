# color(xyz-d50) excess precision rounds to 4 decimal places

XYZ D50 channels in CSS `color()` can exceed the 0–1 range (D50 white point
values go above 1.0 for some channels). Like `srgb-linear` and `xyz-d65`, the
wider numeric range means 3dp can introduce visible rounding errors, so 4dp is
required.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
