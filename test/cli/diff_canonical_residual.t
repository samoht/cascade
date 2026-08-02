CLI: cascade diff - canonical mode's byte verdict.

The canonical minified form is what canonical mode compares, so two sheets
that reach different bytes differ even when the tree diff walked past the
divergence. Here an empty layer-order pin the projection does not fold
away: the report shows the two canonical forms and the exit code is 1.

  $ cat > pinned.css <<EOF
  > @layer a;@layer a{x{top:0}}
  > EOF
  $ cat > unpinned.css <<EOF
  > @layer a{x{top:0}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical pinned.css unpinned.css
  CSS: 28 chars vs 19 chars (32.1% diff)
  Changes: none classified structurally (see report below)
  Canonical forms differ:
  
  Strings differ at position 8 (line 0, col 8)
  
  --- pinned.css
  +++ unpinned.css
  @@ position 8 @@
  -@layer a;@layer a{x{top:0}}
  +@layer a{x{top:0}}
           ^
  
  [1]

  $ cascade diff --diff=canonical pinned.css unpinned.css > /dev/null; echo $?
  1

Two sheets that reach the same canonical form are equal whatever they were
spelled like.

  $ cat > same-a.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > same-b.css <<EOF
  > .x{color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css same-b.css
  CSS files are identical
