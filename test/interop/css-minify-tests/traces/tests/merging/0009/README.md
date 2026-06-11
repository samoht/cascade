# Merging rules is unsafe with all:initial

Merging `.foo{color:red}` and `.bar{all:initial;color:red}` into
`.bar,.foo{color:red}.bar{all:initial}` is unsafe because `all:initial` resets
`color` to its initial value, so the merged color declaration would be
overridden.
