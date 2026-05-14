# oklab() rounding: out-of-gamut colours stay in native space

`oklab()` values with excess precision are rounded to 3 decimal places.
Out-of-gamut oklab colours (chroma too high to fit in sRGB) stay in oklab
space; they cannot be safely represented as a hex value.

In-gamut oklab colours may be minified to hex if the hex form is shorter.
