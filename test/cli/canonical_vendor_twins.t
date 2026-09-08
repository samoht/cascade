CLI: cascade diff --diff=canonical - a sheet against its own minified form.

--minify synthesises a vendor-prefixed twin where a declared target reads only
the prefixed spelling, and drops an authored one the unprefixed property has
superseded. Both rewrites leave a prefixed declaration whose unprefixed twin
carries the same value and importance, which the canonical projection reads as
the twin alone. So every sheet below compares equal to its own minified output,
in either direction and whatever target settled the prefix.

  $ for css in \
  > 'a{-webkit-transform:none;transform:none}' \
  > 'a{-webkit-user-select:none;user-select:none}' \
  > 'a{-webkit-box-shadow:1px 1px red;box-shadow:1px 1px red}' \
  > 'a{user-select:var(--x,)}' \
  > 'a{-webkit-transition:all 1s;transition:all 1s}' \
  > 'a{-webkit-animation-delay:1s;animation-delay:1s}' \
  > 'a{-webkit-flex-wrap:wrap;flex-wrap:wrap}' \
  > 'a{-moz-box-sizing:border-box;box-sizing:border-box}' \
  > 'a{-webkit-line-clamp:2;line-clamp:2}' \
  > 'a{backdrop-filter:blur(1px)}' \
  > 'a{mask-size:cover}' \
  > 'a{mask:url(a.png)}' \
  > ; do
  >   printf '%s' "$css" > src.css
  >   cascade fmt --minify src.css > min.css
  >   if cascade diff --diff=canonical src.css min.css > /dev/null; then
  >     echo "same: $css"
  >   else
  >     echo "DIFFERS: $css -> $(cat min.css)"
  >   fi
  > done
  same: a{-webkit-transform:none;transform:none}
  same: a{-webkit-user-select:none;user-select:none}
  same: a{-webkit-box-shadow:1px 1px red;box-shadow:1px 1px red}
  same: a{user-select:var(--x,)}
  same: a{-webkit-transition:all 1s;transition:all 1s}
  same: a{-webkit-animation-delay:1s;animation-delay:1s}
  same: a{-webkit-flex-wrap:wrap;flex-wrap:wrap}
  same: a{-moz-box-sizing:border-box;box-sizing:border-box}
  same: a{-webkit-line-clamp:2;line-clamp:2}
  same: a{backdrop-filter:blur(1px)}
  same: a{mask-size:cover}
  same: a{mask:url(a.png)}

--enforce-spec keeps every prefix, so the same sheets meet their minified form
from the other side of the target gate too.

  $ for css in \
  > 'a{-webkit-transform:none;transform:none}' \
  > 'a{-webkit-transition:all 1s;transition:all 1s}' \
  > ; do
  >   printf '%s' "$css" > src.css
  >   cascade fmt --minify --enforce-spec src.css > min.css
  >   if cascade diff --diff=canonical src.css min.css > /dev/null; then
  >     echo "same: $css"
  >   else
  >     echo "DIFFERS: $css -> $(cat min.css)"
  >   fi
  > done
  same: a{-webkit-transform:none;transform:none}
  same: a{-webkit-transition:all 1s;transition:all 1s}

A prefixed declaration with no twin beside it is the only spelling an engine
that needs the prefix reads, so it is kept and a differing value still differs.

  $ cat > prefixed-none.css <<CSS
  > a{-webkit-transform:none}
  > CSS
  $ cat > standard-rotate.css <<CSS
  > a{transform:rotate(1deg)}
  > CSS
  $ cascade diff --diff=canonical prefixed-none.css standard-rotate.css > /dev/null
  [1]

  $ cat > prefixed-rotate.css <<CSS
  > a{-webkit-transform:rotate(1deg)}
  > CSS
  $ cat > standard-none.css <<CSS
  > a{transform:none}
  > CSS
  $ cascade diff --diff=canonical prefixed-rotate.css standard-none.css > /dev/null
  [1]

  $ cat > red.css <<CSS
  > a{color:red}
  > CSS
  $ cat > blue.css <<CSS
  > a{color:blue}
  > CSS
  $ cascade diff --diff=canonical red.css blue.css > /dev/null
  [1]

  $ cat > mask-none.css <<CSS
  > a{-webkit-mask:none}
  > CSS
  $ cat > mask-url.css <<CSS
  > a{mask:url(a.png)}
  > CSS
  $ cascade diff --diff=canonical mask-none.css mask-url.css > /dev/null
  [1]

A twin whose value differs from its unprefixed neighbour is a real fallback and
stays visible on both sides.

  $ cat > fallback.css <<CSS
  > a{-webkit-transform:none;transform:rotate(1deg)}
  > CSS
  $ cat > standard-only.css <<CSS
  > a{transform:rotate(1deg)}
  > CSS
  $ cascade diff --diff=canonical fallback.css standard-only.css > /dev/null
  [1]
