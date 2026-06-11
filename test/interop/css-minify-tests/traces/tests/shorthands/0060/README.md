# Border longhand merge unsafe when any var() fallback is unresolvable

If even one `var()` has an unresolvable fallback (nested `var()`, no fallback,
etc.), the longhands cannot be safely merged into `border` shorthand because
the runtime value may expand to something invalid in shorthand context.
