# Padding longhand merge unsafe when var() fallback is unresolvable

When `var()` fallbacks are themselves `var()` references, the final value cannot
be statically determined. Merging into shorthand is unsafe because the nested
variable may resolve to an invalid or multi-value result.
