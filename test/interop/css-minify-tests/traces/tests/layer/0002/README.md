# Non-adjacent @layer blocks can merge

Layer ordering is determined by first appearance of the layer name, not by
physical position of rule blocks. Two `@layer base` blocks separated by
`@layer utilities` can be merged because layer precedence is unchanged.
