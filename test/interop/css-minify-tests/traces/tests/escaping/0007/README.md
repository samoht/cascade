# Newline escape in string must be preserved

`"\a"` is a CSS escape for a newline character (U+000A). It cannot be replaced
with a literal newline inside the string, as that would terminate the string.
