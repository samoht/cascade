# Duplicate @charset removal

Only the first @charset declaration is meaningful; subsequent @charset rules are dropped. Uses two non-UTF-8 charsets to avoid special-casing UTF-8 removal.
