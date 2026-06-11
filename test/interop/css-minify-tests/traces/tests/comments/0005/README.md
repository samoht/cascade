# Comment in @container style query custom property must be preserved

`@container style(--bar: a/**/b)` uses a comment as a zero-width token
separator. Replacing it with a space or removing it entirely changes the token
sequence the query matches against, breaking the correspondence with the
declaration `--bar: a/**/b`.
