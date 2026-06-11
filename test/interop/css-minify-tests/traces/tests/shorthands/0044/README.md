# border longhands with border-image longhands (collapse both)

Both border and border-image expressed as longhands. Minifier should collapse
each group into its respective shorthand, with `border` ordered before
`border-image` to avoid the reset clobbering border-image.
