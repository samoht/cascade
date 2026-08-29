CLI: --enforce-spec's parse effect does not need --minify.

lib/lexer.ml gates non-ASCII identifiers on `~enforce_spec` at parse time
(CSS Syntax 3 sec. 4.2's ident code points), independent of `--minify`. A
raw non-ASCII identifier this parser reads by default then fails that
narrower check under `--enforce-spec`, taking its rule with it - a real
effect the CLI's "has no effect without --minify" warning must not deny.

  $ cat > arrow.css <<EOF
  > .text-↗ { color: red }
  > EOF
  $ cascade fmt arrow.css
  .text-\2197  {
    color: red;
  }

`--enforce-spec` alone drops the rule, so the flag did something: no
warning may say it did not.

  $ cascade fmt --enforce-spec arrow.css 2>&1 | grep -v "warning"
  Error: arrow.css: parse dropped every rule; refusing to write an empty stylesheet

Adding `--minify` changes nothing about that outcome - the drop already
happened at parse time.

  $ cascade fmt --enforce-spec --minify arrow.css 2>&1 | grep -v "warning"
  Error: arrow.css: parse dropped every rule; refusing to write an empty stylesheet

An input `--enforce-spec` leaves alone stays silent too: no warning fires
just because `--minify` is absent.

  $ cat > plain.css <<EOF
  > .a { color: red }
  > EOF
  $ cascade fmt --enforce-spec plain.css 2>&1 | grep -v "warning"
  .a {
    color: red;
  }
