# Redundant 50% percentages removed from color-mix()

`color-mix(in srgb, currentcolor 50%, red 50%)` — 50%/50% is the default mix
ratio, so both percentage arguments are redundant and can be removed. The result
is `color-mix(in srgb, currentcolor, red)`.
