# transform -- no space needed between adjacent functions

In `transform: rotate(45deg) scale(2)`, the space between `)` and `scale(`
can be removed. The `)` token is always its own token and cannot merge with the
following function token, so `rotate(45deg)scale(2)` parses identically.
