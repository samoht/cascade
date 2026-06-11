# url() quote removal in @font-face src

Quotes inside `url()` can be removed when the URL contains no special characters
(whitespace, parentheses, or single/double quotes). `url("font.woff2")` becomes
`url(font.woff2)`.
