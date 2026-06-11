# Transition shorthand default duration removal

`transition: all 0s` can be shortened to `transition: all` because `0s` is the
default value for `transition-duration`. The `all` keyword alone is a valid
single-transition value with all other components taking their initial values.
