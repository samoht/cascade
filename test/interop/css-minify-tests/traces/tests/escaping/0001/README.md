# Escape sequence resolution in identifiers

`\66 oo` is a CSS escape sequence for `foo`. Minifiers should resolve escape
sequences in identifiers to their plain UTF-8 equivalents when safe.
