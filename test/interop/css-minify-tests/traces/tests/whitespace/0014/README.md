# background -- no space needed between ) and keyword

In `background: url(bg.png) no-repeat`, the space between `)` and `no-repeat`
can be removed. The `)` token always terminates cleanly and cannot merge with
the following ident token, so `url(bg.png)no-repeat` parses identically.
