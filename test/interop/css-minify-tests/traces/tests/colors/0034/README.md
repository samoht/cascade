# Named color to hex enables space elision

`blue` (4 chars) converts to `#00f` (4 chars) which looks like a no-op, but
`#` is an unambiguous token start so the preceding space can be dropped:
`1px solid#00f` saves 1 byte over `1px solid blue`.
