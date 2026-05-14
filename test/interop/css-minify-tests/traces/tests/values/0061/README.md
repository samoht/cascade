# Empty content string must not be removed

`content: ""` generates a pseudo-element with no text. Removing the declaration
or changing the value to `none`/`normal` would prevent the pseudo-element from
being generated, which is a semantic change.
