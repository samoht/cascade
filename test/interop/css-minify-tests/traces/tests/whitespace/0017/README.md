# Space before hash token is removable

`#` unambiguously starts a hash token in CSS, so no whitespace is needed to
separate it from a preceding ident or keyword. `solid #fff` becomes `solid#fff`.
