# :where() whitespace removal

Whitespace inside `:where()` around the selector list commas can be removed.
`:where()` itself must not be replaced with `:is()` as they differ in
specificity (`:where()` is zero-specificity).
