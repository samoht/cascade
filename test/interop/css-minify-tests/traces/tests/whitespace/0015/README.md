# font shorthand -- no space needed around /

In `font: 16px / 1.5 sans-serif`, the spaces around `/` separating font-size
from line-height can be removed. The `/` is a delim token that always stands
alone, so `16px/1.5` parses identically. Note the space before `sans-serif`
must remain because `1.5sans-serif` would become a single dimension token.
