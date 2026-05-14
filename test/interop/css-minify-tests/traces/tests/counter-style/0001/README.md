# @counter-style whitespace removal

Basic whitespace minification inside a `@counter-style` rule. The space between
the at-keyword and the name must be preserved (unlike `@page`), but all other
internal whitespace is removable. The default encoding is UTF-8, therefore the
\1F44D codepoint can be replaced with 👍.
