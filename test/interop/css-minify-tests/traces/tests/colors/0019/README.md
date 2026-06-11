# color-mix() default percentages flattened to named color

When `color-mix()` omits percentages (defaulting to 50%/50%) with known color
arguments, minifiers can resolve the mix at build time. `color-mix(in srgb, red,
blue)` is the same 50/50 mix as the explicit form, yielding (128,0,128) = `purple`.
