CLI: what may stand before an @import.

CSS Cascade 5 sec. 2 puts an @import before every other valid at-rule
and style rule, except @charset and @layer statements, or the browser
ignores it. A comment is not a rule (CSS Syntax 3 sec. 4.3.2 consumes it
without producing a token), so a preserved license comment before an
@import leaves the import in place, and the browser loads it.

  $ cat > license.css <<CSS
  > /*! license */ @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify license.css
  /*! license */@import"a.css";.a{color:red}
  $ cascade fmt license.css
  /*! license */
  @import "a.css";
  .a {
    color: red;
  }

An at-rule cascade has no grammar for may be one the reader ignores,
which the spec says leaves the import valid, so the import stays. The
warning is the one for the unknown at-rule, not one for the import.

  $ cat > unknown.css <<CSS
  > @foo bar; @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify unknown.css 2> err.txt
  @foo bar;@import"a.css";.a{color:red}
  $ grep -c '^warning' err.txt
  1

A rule before the import still makes it invalid: it is reported and
dropped.

  $ cat > late.css <<CSS
  > .a{color:red} @import url(a.css);
  > CSS
  $ cascade fmt --minify late.css 2> err.txt
  .a{color:red}
  $ grep -c 'must precede' err.txt
  1

An @namespace ends the imports (CSS Namespaces 3 sec. 3), so an
@import after one is invalid. It is reported, and the minified output
drops it as it drops an @import after a rule.

  $ cat > namespace.css <<CSS
  > @namespace svg url(x); @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify namespace.css 2> err.txt
  @namespace svg"x";.a{color:red}
  $ grep -c 'must precede' err.txt
  1

An @namespace after a style rule is ignored by the browser in the same
way, and goes from the minified output too.

  $ cat > late-namespace.css <<CSS
  > .a{color:red} @namespace svg url(x);
  > CSS
  $ cascade fmt --minify late-namespace.css 2> err.txt
  .a{color:red}
  $ grep -c 'must precede' err.txt
  1
