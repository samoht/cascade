CLI: cascade writes the bytes it prints, so a line ends in LF on every
platform. [cat -e] marks each line end with $ and shows a carriage return
as ^M.

The formatted sheet on standard output:

  $ printf '.a { color: red }\n.b { color: blue }\n' > two.css
  $ cascade fmt two.css | cat -e
  .a {$
    color: red;$
  }$
  .b {$
    color: blue;$
  }$

A warning on standard error:

  $ printf '.a { color: red }\n.b { color: }\n' > bad.css
  $ cascade fmt bad.css 2>&1 >/dev/null | cat -e
  warning: bad.css: read_declaration: bad value for : expected a value at [31-31] (in component)$
  warning: .a { color: red }$
  warning: .b { color: }$
  warning:              ^$
  warning: $

The help page:

  $ cascade fmt --help=plain | grep -c "$(printf '\r')"
  0
  [1]
