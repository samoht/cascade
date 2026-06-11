# calc() space around + operator required

Spaces around `+` inside `calc()` are required by the CSS spec, just like `-`.
Without the space, `+20px` is tokenized as a single positive dimension token,
leaving two values with no operator, which is invalid and causes the declaration
to be silently dropped.
