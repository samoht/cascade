CLI: an at-rule cascade does not recognise still reaches the output.

CSS Syntax 3 sec. 5.4.2 consumes an at-rule whatever its name, block and
all. Discarding an unrecognised one is a user-agent step (CSS 2.1 sec.
4.2), and `cascade fmt` is not the user agent: it cannot know which agent
reads what it writes. Lightning CSS, esbuild, csso, clean-css and cssnano
all keep the at-rule and its block.

The body of an unrecognised at-rule has no known grammar, so it is
carried as source text; every input below is written tight so the text
carried is the text shown.

A statement-form at-rule keeps its prelude and its neighbour.

  $ cat > stmt.css <<CSS
  > @foo bar;
  > a { color: red }
  > CSS
  $ cascade fmt --minify stmt.css 2> err.txt
  @foo bar;a{color:red}
  $ grep -c '^warning' err.txt
  1

A block-form at-rule keeps the rules it contains. Deleting the wrapper
deletes them too, which is the same silent loss with more of the
stylesheet in it.

  $ cat > block.css <<CSS
  > @future x{a{color:red}}
  > b { color: blue }
  > CSS
  $ cascade fmt --minify block.css 2> /dev/null
  @future x{a{color:red}}b{color:#00f}

The same at any block depth.

  $ cat > nested.css <<CSS
  > @media screen { @foo bar; d { color: red } }
  > CSS
  $ cascade fmt --minify nested.css 2> /dev/null
  @media screen{@foo bar;d{color:red}}

tw hands cascade `--input-css` written in Tailwind's authoring
vocabulary, none of which cascade recognises.

  $ cat > tw.css <<CSS
  > @theme{--color-a:red}
  > @utility btn{color:red}
  > .x { color: red }
  > CSS
  $ cascade fmt --minify tw.css 2> /dev/null
  @theme{--color-a:red}@utility btn{color:red}.x{color:red}

An at-rule that is the whole stylesheet is the whole output, so nothing
was dropped to report and the exit status stays 0.

  $ printf '@foo bar;' | cascade fmt --minify - 2> /dev/null
  @foo bar;

Pretty output round-trips: re-formatting it is a fixed point, and
minifying it lands back on the minified form.

  $ cascade fmt block.css > pretty.css 2> /dev/null
  $ cascade fmt pretty.css 2> /dev/null > pretty2.css
  $ diff pretty.css pretty2.css && echo identical
  identical
  $ cascade fmt --minify pretty.css 2> /dev/null
  @future x{a{color:red}}b{color:#00f}
