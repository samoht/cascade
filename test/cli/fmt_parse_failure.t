CLI: `fmt` refuses to report success when it produced nothing.

`cascade fmt src.css > dist.css` is a build step. When the input parses
to an empty stylesheet the redirect writes a 0-byte file, and exiting 0
there hands CI a green build and a site with no CSS. The warnings on
stderr are easy to miss and impossible to gate on.

An entirely unparseable stylesheet: every rule is dropped, so the run is
an error, stdout stays empty and the exit status is non-zero.

  $ cat > broken.css <<EOF
  > @@@@ }}} {{{ !!! ;;;
  > EOF
  $ cascade fmt broken.css > out.css 2> err.txt
  [1]
  $ wc -c < out.css | tr -d ' '
  0
  $ grep '^Error' err.txt
  Error: broken.css: parse dropped every rule; refusing to write an empty stylesheet

The warnings that explain the drop are still reported.

  $ grep -c '^warning' err.txt
  4

A single rule whose value does not validate is the same case: the one
rule is dropped and nothing is left to write.

  $ cat > one-bad-rule.css <<EOF
  > .b { color: notacolor }
  > EOF
  $ cascade fmt one-bad-rule.css > out2.css 2> err2.txt
  [1]
  $ wc -c < out2.css | tr -d ' '
  0

Reading from stdin reports the same way.

  $ cascade fmt - < broken.css > out3.css 2> err3.txt
  [1]
  $ grep '^Error' err3.txt
  Error: <stdin>: parse dropped every rule; refusing to write an empty stylesheet

Partial recovery is not an error: the rules that survive are written and
the exit status stays 0.

  $ cat > partial.css <<EOF
  > .b { color: notacolor }
  > .ok { color: blue }
  > EOF
  $ cascade fmt --minify partial.css 2> /dev/null
  .ok{color:#00f}

An input with no rules to begin with is legitimately empty: nothing was
dropped, so there is nothing to report.

  $ cat > comment-only.css <<EOF
  > /* nothing but a comment */
  > EOF
  $ cascade fmt comment-only.css
