CLI: an invalid declaration is dropped, a valid untyped one is kept.

README: "empty rules and invalid declarations are dropped in both pretty and
minified output". A value a browser rejects is an invalid declaration, so it
goes, whatever shape its token stream has. The opaque fallback is for a value
that is valid CSS the typed reader cannot represent, which is a different
thing and is pinned below.

Each of these has a value no browser accepts. The declaration goes and the
rule with it.

  $ cat > invalid.css <<EOF
  > a{width:calc(100%- 10px)}
  > b{transform:translateX(10px red)}
  > c{display:list-item table}
  > d{animation-timing-function:steps(1.5)}
  > e{grid-column-start:0}
  > f{grid-template-columns:1px [a] [b] 2px}
  > g{position-area:left block-start}
  > h{transform-origin:left 10px top 20px}
  > i{clip-path:inset(1px round auto)}
  > j{offset:total nonsense here}
  > k{grid-column-start:span -1}
  > l{border-width:max(1px,red)}
  > EOF
  $ cascade fmt --minify invalid.css 2>/dev/null


The same holds without --minify, since the contract names both.

  $ cascade fmt invalid.css 2>/dev/null


Each one warns rather than failing the run.

  $ cascade fmt --minify invalid.css 2>&1 >/dev/null | grep -c "bad value\|invalid"
  12

What the opaque fallback is for, and must keep doing: an unknown property name,
a value carrying a runtime substitution, and a colour fallback. None of these
is a value the typed reader rejects as invalid.

  $ cat > opaque.css <<EOF
  > a{-vendor-thing:whatever it likes}
  > b{width:var(--w)}
  > c{color:color(display-p3 1 0 0)}
  > EOF
  $ cascade fmt --minify opaque.css
  a{-vendor-thing:whatever it likes}b{width:var(--w)}c{color:color(display-p3 1 0 0)}
