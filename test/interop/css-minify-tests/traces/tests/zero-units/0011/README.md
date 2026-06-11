# Preserve unit on zero-percentage flex-basis

`flex-basis: 0%` must NOT be shortened to `flex-basis: 0`. When the flex
container has no definite size, `0%` resolves to `auto`, whereas `0` resolves
to zero length. Since `0%` is shorter than `auto`, it is already optimal.
