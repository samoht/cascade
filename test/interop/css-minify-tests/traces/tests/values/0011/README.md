# Smart separator preservation in calc()

Spaces around `+` and `-` operators inside `calc()` must be preserved. Removing
them would cause `50vh+10px` to be parsed as a single dimension token, or
`100%-20px` to change meaning. This tests that minifiers do not over-strip.
