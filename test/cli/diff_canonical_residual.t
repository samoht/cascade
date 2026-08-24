CLI: cascade diff - canonical mode's byte verdict.

The canonical minified form is what canonical mode compares, so two sheets
that reach different bytes differ even when the tree diff walked past the
divergence. Here a layer-order pin that is the only declaration of its layer:
it puts `a` before `b`, the tree diff sees a statement carrying no rules, and
the report shows the two canonical forms with exit code 1.

  $ cat > pinned.css <<EOF
  > @layer a;@layer b{x{top:0}}
  > EOF
  $ cat > unpinned.css <<EOF
  > @layer b{x{top:0}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=canonical pinned.css unpinned.css
  CSS: 28 chars vs 19 chars (32.1% diff)
  Changes: none classified structurally (see report below)
  Canonical forms differ:
  
  Strings differ at position 7 (line 0, col 7)
  
  --- pinned.css
  +++ unpinned.css
  @@ position 7 @@
  -@layer a;@layer b{x{top:0}}
  +@layer b{x{top:0}}
          ^
  
  [1]

  $ cascade diff --diff=canonical pinned.css unpinned.css > /dev/null; echo $?
  1

  $ NO_COLOR=1 cascade diff --diff=tree pinned.css unpinned.css
  CSS files are identical

Two sheets that reach the same canonical form are equal whatever they were
spelled like. A pin the very next block repeats gives its layer the position
that block gives it anyway, so the projection folds it and the two spellings
are one sheet.

  $ cat > repeated.css <<EOF
  > @layer a;@layer a{x{top:0}}
  > EOF
  $ cat > plain.css <<EOF
  > @layer a{x{top:0}}
  > EOF
  $ cascade diff --diff=canonical repeated.css plain.css
  CSS files are identical

  $ cat > same-a.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > same-b.css <<EOF
  > .x{color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css same-b.css
  CSS files are identical
