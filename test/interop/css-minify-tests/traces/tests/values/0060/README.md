# url() quotes required when URL contains parentheses

Removing quotes from `url("image (1).png")` would produce `url(image (1).png)`
which is a parse error -- the unquoted form cannot contain `(` or `)`.
