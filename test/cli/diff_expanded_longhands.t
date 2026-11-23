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

CSS Flexible Box 1 sec. 7.1: a factor [flex] leaves out is 1, so a lone
basis is [1 1 <basis>], and [flex:1 10px] is [flex:10px]. --minify writes
the shorter one.

  $ printf '.a{flex:10px}' > a.css
  $ printf '.a{flex:1 10px}' > b.css
  $ cascade diff --diff=canonical a.css b.css
  CSS files are identical
  $ cascade --minify b.css
  .a{flex:10px}

CSS Backgrounds 3 sec. 2.10: [background] sets [background-color] to the
initial value sec. 2.2 gives it, [transparent], when it names no colour, so
the longhands written out in full say what the shorthand says.

  $ cat > a.css <<'EOF2'
  > .a{background:url(a.png) no-repeat content-box}
  > EOF2
  $ cat > b.css <<'EOF2'
  > .a{background-image:url(a.png);background-position:0% 0%;background-size:auto;background-repeat:no-repeat;background-attachment:scroll;background-origin:content-box;background-clip:content-box;background-color:transparent}
  > EOF2
  $ cascade diff --diff=canonical a.css b.css
  CSS files are identical
  $ cascade --minify b.css
  .a{background:url(a.png)no-repeat content-box}
