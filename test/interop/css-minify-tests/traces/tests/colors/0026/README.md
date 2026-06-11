# oklch() numeric minification

`oklch()` values can have their numbers minified (leading zero removal) but
must not be converted to sRGB hex since the oklch gamut exceeds sRGB.
