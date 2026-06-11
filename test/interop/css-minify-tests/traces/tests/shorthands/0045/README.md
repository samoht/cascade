# font shorthand must not clobber font-variant-ligatures (reorder required)

font-variant-ligatures is declared before font longhands. A minifier collapsing
the longhands to `font` must reorder it before `font-variant-ligatures`, since
`font` resets font-variant-ligatures to its initial value (`normal`).
