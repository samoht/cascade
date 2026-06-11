# color(srgb-linear) excess precision rounds to 4 decimal places

sRGB-linear channels are in the 0–1 range. Unlike other 0–1 colour spaces which
use 3dp, srgb-linear needs 4dp because its linear transfer function amplifies
rounding errors for near-black values at 3dp. Alpha is 0–1, so 3dp applies.

Both colours here are out-of-gamut for sRGB, so they stay in their native
colour space. In-gamut colours may be minified to hex.

For a lot more detail on this, read [Too Much Color](https://keithcirkel.co.uk/too-much-color).
