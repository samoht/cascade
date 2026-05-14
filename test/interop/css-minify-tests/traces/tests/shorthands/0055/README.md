# Border longhand merge unsafe with var()

Merging `border-width`, `border-style`, `border-color` into `border` shorthand
is unsafe when values use `var()`. The variables may expand to multi-value
strings (e.g. `--border-width: 0 0 0 1px`) which are valid in longhands but
not in the shorthand form.
