# In-gamut oklab colour is minified to hex

`oklab(0.5 -0.1 0.1)` is within the sRGB gamut. A minifier should convert it
to the shortest sRGB representation (`#3c740a`) rather than keeping the
longer oklab form.
