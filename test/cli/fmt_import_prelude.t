CLI: what may stand before an @import.

CSS Cascade 5 sec. 2: "Any @import rules must precede all other valid
at-rules and style rules in a style sheet (ignoring @charset and empty
@layer definitions) and must not have any other valid at-rules or style
rules between it and previous @import rules, or else the @import rule
is invalid." Only valid rules count. Each placement below is measured
in Chrome by a curated case of test/render/parse_recovery.ml, named
beside it; the CSSOM shows a CSSImportRule whether or not the imported
sheet loads, so the case shows whether the import survived.

A comment is no rule at all (CSS Syntax 3 sec. 4.3.2 consumes it
without producing a token), so a preserved license comment leaves the
import valid: Chrome keeps the CSSImportRule (comment_before_import).

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

The same holds between two imports: Chrome keeps both
(comment_between_imports).

  $ cat > between.css <<CSS
  > @import url(a.css); /*! c */ @import url(b.css); .a{color:red}
  > CSS
  $ cascade fmt --minify between.css
  @import"a.css";/*! c */@import"b.css";.a{color:red}

An at-rule with no grammar behind it is invalid and ignored (CSS
Syntax 3 sec. 8, CSS 2.1 sec. 4.2), so it is not a valid at-rule
before the import, which stays: Chrome keeps the CSSImportRule
(unknown_at_rule_before_import). The one warning is the one for the
unknown at-rule, not one for the import.

  $ cat > unknown.css <<CSS
  > @foo bar; @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify unknown.css 2> err.txt
  @foo bar;@import"a.css";.a{color:red}
  $ grep -c '^warning' err.txt
  1

A style rule whose selector is invalid is dropped, so it is not a
valid rule either: Chrome keeps the import (invalid_rule_before_import).

  $ cat > invalid-rule.css <<CSS
  > ..b{color:red} @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify invalid-rule.css 2> err.txt
  @import"a.css";.a{color:red}
  $ grep -c 'must precede' err.txt
  0
  [1]

A valid rule before the import makes it invalid: it is reported and
dropped.

  $ cat > late.css <<CSS
  > .a{color:red} @import url(a.css);
  > CSS
  $ cascade fmt --minify late.css 2> err.txt
  .a{color:red}
  $ grep -c 'must precede' err.txt
  1

An @layer block is a valid at-rule, not an empty @layer definition, so
an import after one is invalid: Chrome drops it
(layer_block_before_import).

  $ cat > layer-block.css <<CSS
  > @layer x{.b{color:red}} @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify layer-block.css 2> err.txt
  @layer x{.b{color:red}}.a{color:red}
  $ grep -c 'must precede' err.txt
  1

An @media whose query does not parse is still a valid @media rule: Media
Queries 4 sec. 3.2 reads the bad query as "not all". An import after it
is invalid: Chrome keeps the rule under "not all" and drops the import
(bad_media_before_import).

  $ cat > bad-media.css <<CSS
  > @media screen screen{.b{color:red}} @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify bad-media.css 2> err.txt
  @media not all{.b{color:red}}.a{color:red}
  $ grep -c 'must precede' err.txt
  1

CSS Namespaces 3 sec. 3.1: "Any @namespace rules must follow all
@charset and @import rules and precede all other non-ignored at-rules
and style rules in a style sheet", and "A syntactically invalid
@namespace rule (whether malformed or misplaced) must be ignored." So an
@import after an @namespace is invalid by Cascade 5 sec. 2: Chrome drops
it (namespace_before_import). It is reported, and the minified output
drops it as it drops an @import after a rule.

  $ cat > namespace.css <<CSS
  > @namespace svg url(x); @import url(a.css); .a{color:red}
  > CSS
  $ cascade fmt --minify namespace.css 2> err.txt
  @namespace svg"x";.a{color:red}
  $ grep -c 'must precede' err.txt
  1

By the same section an @namespace after a style rule is ignored, and
goes from the minified output too (late_namespace).

  $ cat > late-namespace.css <<CSS
  > .a{color:red} @namespace svg url(x);
  > CSS
  $ cascade fmt --minify late-namespace.css 2> err.txt
  .a{color:red}
  $ grep -c 'must precede' err.txt
  1

An @layer statement after the imports is a non-ignored at-rule, so an
@namespace after it is ignored: Chrome drops it
(namespace_after_layer_statement).

  $ cat > layer-namespace.css <<CSS
  > @import url(a.css); @layer x; @namespace svg url(y); .a{color:red}
  > CSS
  $ cascade fmt --minify layer-namespace.css 2> err.txt
  @import"a.css";@layer x;.a{color:red}
  $ grep -c 'must precede' err.txt
  1

CSS Cascade 5 sec. 6.4.4.2 allows the empty @layer statements "before
@import and @namespace rules (after the @charset rule, if any)", so one
that opens the sheet leaves the @namespace after it valid: Chrome keeps
both (namespace_after_leading_layer_statement).

  $ cat > leading-layer.css <<CSS
  > @layer x; @namespace svg url(y); .a{color:red}
  > CSS
  $ cascade fmt --minify leading-layer.css 2> err.txt
  @layer x;@namespace svg"y";.a{color:red}
  $ cat err.txt
