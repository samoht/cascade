# Absolute unit conversion to px

`12pt` converts to `16px`. Absolute units (pt, pc, cm, mm, in, Q) have fixed
ratios to px. Normalizing to px also improves gzip compression by reducing the
number of distinct unit strings in the output.
