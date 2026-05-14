# color-mix() in oklch resolved to oklch() when out of sRGB gamut

`color-mix(in oklch, lime, blue)` produces a high-chroma teal in oklch space
that is outside the sRGB gamut. A minifier must not convert this to a hex color
(which would clamp and change the color). Instead, the mix should be resolved
to oklch functional notation: `oklch(.659 .304 203.3)`, which is both shorter
than the color-mix and preserves the intended color. L/C at 3dp and hue at 1dp
- well below dEOk JND of 0.02.
