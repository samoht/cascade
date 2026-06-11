# Unresolved position-try-fallbacks can be removed

`position-try-fallbacks: --top, --undefined` can drop `--undefined` when no
matching `@position-try --undefined` exists in the stylesheet. Unresolved
dashed-ident fallbacks are skipped at runtime and have no effect.
