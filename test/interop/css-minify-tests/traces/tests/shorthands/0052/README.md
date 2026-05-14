# font shorthand must not clobber font-variation-settings (reorder required)

font-variation-settings is declared before font longhands. A minifier collapsing
the longhands to `font` must reorder it before `font-variation-settings`, since
`font` resets font-variation-settings to its initial value (`normal`).
