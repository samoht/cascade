# mask shorthand must not clobber mask-border (reorder required)

mask-border is declared before mask longhands. A minifier collapsing the
longhands to `mask` must reorder it before `mask-border`, since `mask` resets
mask-border to its initial value.
