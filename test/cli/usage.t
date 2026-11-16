CLI: the help pages and the usage errors a new user meets first.

`cascade FILE` is `cascade fmt FILE`, but a word that names no file and
looks like no path is a mistyped command, and is reported as one, with the
command it is closest to.

  $ printf '.a{color:red}' > a.css
  $ cascade fmtt a.css > /dev/null 2> err.txt
  [124]
  $ grep -c 'Did you mean' err.txt
  1
  $ cascade a.css
  .a {
    color: red;
  }

A missing stylesheet is still a stylesheet, not a command.

  $ cascade missing.css
  cascade: error: missing.css: No such file or directory
  [1]

A refused option value is one error line and the exit status, nothing
else.

  $ cascade --inline-vars --keep-vars='*' a.css
  cascade: error: --keep-vars does not accept the wildcard "*"; list names explicitly
  [1]

An option that does nothing without another says so, and `-q` drops the
note. `--flatten-nesting` is one: it acts on the minifier's output.

  $ cascade --flatten-nesting a.css > /dev/null
  cascade: warning: --flatten-nesting has no effect without --minify
  $ cascade -q --flatten-nesting a.css > /dev/null

Every command's page has examples, and the options are grouped by what
they act on.

  $ for c in fmt diff apply prune; do
  >   cascade $c --help=plain | grep -c '^EXAMPLES$' | sed "s/^/$c /"
  > done
  fmt 1
  diff 1
  apply 1
  prune 1
  $ cascade fmt --help=plain | grep -E '^[A-Z ]*OPTIONS$'
  MINIFY OPTIONS
  INLINING OPTIONS
  COMMON OPTIONS
  $ cascade diff --help=plain | grep -E '^[A-Z ]*OPTIONS$'
  OPTIONS
  CANONICAL OPTIONS
  BROWSER OPTIONS
  COMMON OPTIONS

The pages name code as code: no odoc brackets such as `[@import]` reach
the rendered text.

  $ for c in fmt diff apply prune; do cascade $c --help=plain; done \
  > | grep -E '\[(@[a-z]|var\(|--\]|\.\.\])' || echo none
  none
