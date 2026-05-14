# display-p3 color must not be converted to sRGB

`color(display-p3 1 0 0)` is a P3 red that cannot be represented in sRGB.
Converting to hex or rgb would clamp the value and lose the wider gamut intent.
The color function and space must be preserved.
