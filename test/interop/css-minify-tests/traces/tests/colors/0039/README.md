# Redundant "shorter hue" removed from color-mix()

`color-mix(in oklch shorter hue, currentcolor, red)` — `shorter` is the default
hue interpolation method for polar color spaces, so `shorter hue` is redundant
and can be removed. The result is `color-mix(in oklch, currentcolor, red)`.
