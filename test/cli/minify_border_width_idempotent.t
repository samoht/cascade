CLI: --minify is idempotent on a border-width min()/max()/clamp() fold.

lib/declaration.ml requires that two declarations minifying to the same text
always share one value, since rule merging keys on that hash. min(2cqi,3cqi)
and 2cqi are the same <line-width> (CSS Values 4 sec. 10.2 keeps the smaller
of two same-unit arguments), so a first --minify pass must fold the min()
into one AST node before hashing, not just into matching text. A second pass
over already-minified output must be a no-op.

  $ cat > border_width.css <<CSS
  > a { border-width: min(2cqi,3cqi) }
  > b { border-width: 2cqi }
  > CSS
  $ cascade --minify border_width.css
  a,b{border-width:2cqi}
  $ cascade --minify border_width.css | cascade --minify -
  a,b{border-width:2cqi}

A plain fmt keeps the authored spelling: folding min()/max()/clamp() is a
node-changing rewrite, so it belongs in the optimizer, not in fmt's pure
serialiser.

  $ cat > authored.css <<CSS
  > a { border-width: max(2cqi, 3cqi) }
  > CSS
  $ cascade authored.css
  a {
    border-width: max(2cqi, 3cqi);
  }
