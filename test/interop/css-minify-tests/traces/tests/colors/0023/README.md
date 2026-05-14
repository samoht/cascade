# Named color shorter than any hex equivalent

`purple` is `#800080` -- 6 chars vs 7 for hex, and the digits don't pair so it
can't shorten to 3-digit hex either. A minifier must not convert to hex here.
