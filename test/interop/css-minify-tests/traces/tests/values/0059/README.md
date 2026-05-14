# Unrecognized properties must not be removed

A rule with unrecognized property names is syntactically valid CSS. Minifiers
must not discard it -- unknown properties may be meaningful to other tools or
future CSS specifications.
