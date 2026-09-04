CLI: what `fmt` reports through its exit status.

`cascade fmt src.css > dist.css` is a build step, and the warnings on
stderr are easy to miss and impossible to gate on, so the exit status
carries the one thing the redirect cannot show: whether the parser could
use the input at all. It is non-zero when a source that had something to
drop left no statement behind, which is the question `cascade apply` asks
of each `<style>` block. Everything else is a run that did its job.

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
  8

Reading from stdin reports the same way.

  $ cascade fmt - < broken.css > out3.css 2> err3.txt
  [1]
  $ grep '^Error' err3.txt
  Error: <stdin>: parse dropped every rule; refusing to write an empty stylesheet

A rule whose only declaration does not validate is a partial loss, not a
total one. CSS Syntax 3 sec. 5.4.4 discards the declaration and returns
the rule that held it, so the parse leaves a statement; that statement
has nothing to print, so the output is empty all the same.

  $ cat > one-bad-rule.css <<EOF
  > .b { color: notacolor }
  > EOF
  $ cascade fmt one-bad-rule.css > out2.css 2> err2.txt
  $ wc -c < out2.css | tr -d ' '
  0
  $ grep '^Error' err2.txt
  [1]

The declaration that went is still reported.

  $ grep -c 'notacolor' err2.txt
  2

Partial recovery is not an error: the rules that survive are written and
the exit status stays 0.

  $ cat > partial.css <<EOF
  > .b { color: notacolor }
  > .ok { color: blue }
  > EOF
  $ cascade fmt --minify partial.css 2> /dev/null
  .ok{color:#00f}

Whatever the parser lost. A selector it cannot read takes the whole rule
that carried it, and the rules either side are written.

  $ cat > bad-selector.css <<EOF
  > .ok { color: red }
  > :: { color: blue }
  > .ok2 { color: green }
  > EOF
  $ cascade fmt --minify bad-selector.css 2> /dev/null
  .ok{color:red}.ok2{color:green}

An at-rule prelude it cannot read is the same shape: the block goes, its
neighbours stay.

  $ cat > bad-prelude.css <<EOF
  > .ok { color: red }
  > @media ^^^ { .x { color: blue } }
  > .ok2 { color: green }
  > EOF
  $ cascade fmt --minify bad-prelude.css 2> /dev/null
  .ok{color:red}.ok2{color:green}

An input with no rules to begin with is legitimately empty: nothing was
dropped, so there is nothing to report.

  $ cat > comment-only.css <<EOF
  > /* nothing but a comment */
  > EOF
  $ cascade fmt comment-only.css

An empty output is not the question the status answers, and cannot be:
`--minify` deletes CSS no browser would act on, and deleting it is the
tool working rather than the parser losing. CSS Syntax 3 sec. 8.3 makes
`@charset` a decoder hint rather than a rule, and UTF-8 is what the
serialiser writes anyway, so a redundant one goes and the output is
empty. Both were parsed and both were kept.

  $ cat > charset.css <<EOF
  > @charset "UTF-8";@charset "UTF-8";
  > EOF
  $ cascade fmt --minify charset.css > out4.css 2> err4.txt
  $ wc -c < out4.css | tr -d ' '
  0
  $ grep '^Error' err4.txt
  [1]

The second one is out of place, which is reported, and neither that nor
the deletion is a rule the parse dropped.

  $ grep -c '^warning' err4.txt
  1

A `@font-face` with no `src` is the same shape. CSS Fonts 4 sec. 4.3
gives it no font to load, so `--minify` removes it, but the rule parsed
and the statement was there.

  $ cat > face.css <<EOF
  > @font-face{font-family:x}
  > EOF
  $ cascade fmt --minify face.css > out5.css 2> err5.txt
  $ wc -c < out5.css | tr -d ' '
  0
  $ grep '^Error' err5.txt
  [1]

A warning is not a loss either way round: an at-rule this library does
not recognise is reported and kept whole, so a stylesheet that is nothing
but one formats to itself.

  $ cat > kept.css <<EOF
  > @foo bar;
  > EOF
  $ cascade fmt --minify kept.css 2> err6.txt
  @foo bar;
  $ grep -c '^warning' err6.txt
  1
