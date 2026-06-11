# color-mix() with known colors flattened to named color

`color-mix(in srgb, red 50%, blue 50%)` can be resolved at build time. The
50/50 sRGB mix of red (255,0,0) and blue (0,0,255) is (128,0,128) = `purple`.
Minifiers should flatten color-mix() with static arguments to the shortest form.
