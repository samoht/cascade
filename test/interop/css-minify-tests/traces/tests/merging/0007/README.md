# Merging rules is unsafe with the all property

Merging `.foo{color:red}` and `.bar{all:unset;color:red}` into
`.bar,.foo{color:red}.bar{all:unset}` is unsafe because `all:unset` resets
`color`, so the merged color declaration would be overridden.
