# Calc division with irrational result must keep calc

`calc(100% / 3)` cannot be statically resolved to a finite decimal without
precision loss. Any truncation (e.g. `33.3333%`) introduces measurable rounding
error. The calc wrapper must be preserved; only whitespace is removable.
