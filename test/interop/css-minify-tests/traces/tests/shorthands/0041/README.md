# border shorthand must not clobber border-image (reorder required)

border-image is declared before border longhands. A minifier collapsing the
longhands to `border` must reorder it before `border-image`, since `border`
resets border-image to its initial value.
