# 8-digit hex with non-repeating alpha must not shorten

`#ff00007a` has alpha `7a` (not `77`), so it cannot be shortened to 4-digit
hex `#f007`. A minifier that naively pairs digits would produce the wrong alpha.
