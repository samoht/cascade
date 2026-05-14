# unicode-range wildcard compression

`U+0000-00FF` can be shortened to `U+??` using wildcard notation. The `?`
replaces trailing hex digits that cover the full 0-F range, and leading zeros
in the prefix can be dropped.
