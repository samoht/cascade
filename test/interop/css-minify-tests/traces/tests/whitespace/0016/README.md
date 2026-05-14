# Descendant combinator -- space is the selector itself

In `div p`, the space IS the descendant combinator. Removing it produces `divp`,
a single type selector for an element named `divp` which matches nothing. The
space between simple selectors forming a descendant relationship must always be
preserved.
