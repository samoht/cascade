CLI: cascade diff - canonical mode's residual byte difference.

Two sheets the structural comparator calls equivalent can still
canonicalise to different bytes: here an empty layer-order pin the
projection does not fold away. Tree-diff is the authoritative answer, so
the verdict stays equivalence and the exit code stays 0, but the report
names the divergence rather than dropping it.

  $ cat > pinned.css <<EOF
  > @layer a;@layer a{x{top:0}}
  > EOF
  $ cat > unpinned.css <<EOF
  > @layer a{x{top:0}}
  > EOF
  $ cascade diff --diff=canonical pinned.css unpinned.css
  CSS files are equivalent
  Canonical minified forms still differ (cosmetic; --depth=max shows it)

  $ cascade diff --diff=canonical pinned.css unpinned.css > /dev/null; echo $?
  0

Under --depth=max the two canonical forms are shown.

  $ NO_COLOR=1 cascade diff --diff=canonical --depth=max pinned.css unpinned.css
  CSS files are equivalent
  Canonical minified forms still differ:

  Strings differ at position 8 (line 0, col 8)

  --- pinned.css
  +++ unpinned.css
  @@ position 8 @@
  -@layer a;@layer a{x{top:0}}
  +@layer a{x{top:0}}
           ^

A canonical comparison whose bytes do match keeps the plain verdict.

  $ cat > same-a.css <<EOF
  > .x { color: red }
  > EOF
  $ cat > same-b.css <<EOF
  > .x{color:red}
  > EOF
  $ cascade diff --diff=canonical same-a.css same-b.css
  CSS files are identical
