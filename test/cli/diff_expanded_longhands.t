CLI: cascade diff - a shorthand against the longhands it expands to.

CSS Backgrounds 3 sec. 3.4: [border] writes border-width, border-style and
border-color, sets each one it omits to its initial value, and resets
border-image to none. Written out in full, those longhands are the
shorthand: the tree and the canonical diff find no difference, and the
default mode reports only that the text differs.

  $ cat > a.css <<'EOF'
  > .a{border:1px solid red}
  > EOF
  $ cat > b.css <<'EOF'
  > .a{border-width:1px;border-style:solid;border-color:red;border-image:none}
  > EOF
  $ cascade diff --diff=tree a.css b.css
  CSS files are identical
  $ cascade diff --diff=canonical a.css b.css
  CSS files are identical
  $ NO_COLOR=1 cascade diff a.css b.css | sed -n 2p
  Changes: no structural change; the text differs

A slot the shorthand omits reads as its initial value, here sec. 3.1's
currentcolor.

  $ cat > a.css <<'EOF'
  > .a{border:1px solid}
  > EOF
  $ cat > b.css <<'EOF'
  > .a{border-width:1px;border-style:solid;border-color:currentcolor;border-image:none}
  > EOF
  $ cascade diff --diff=tree a.css b.css
  CSS files are identical
  $ cascade diff --diff=canonical a.css b.css
  CSS files are identical

A longhand that writes another value there is still a difference.

  $ cat > b.css <<'EOF'
  > .a{border-width:1px;border-style:solid;border-color:red;border-image:none}
  > EOF
  $ cascade diff --diff=tree a.css b.css > /dev/null
  [1]
  $ cascade diff --diff=canonical a.css b.css > /dev/null
  [1]
