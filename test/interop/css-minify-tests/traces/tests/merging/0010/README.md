# Merging rules is unsafe with all:revert-layer

Merging `.foo{color:red}` and `.bar{all:revert-layer;color:red}` into
`.bar,.foo{color:red}.bar{all:revert-layer}` is unsafe because
`all:revert-layer` rolls back `color` to the previous cascade layer, so the
merged color declaration would be overridden.
