CLI: cascade writes the bytes it prints, so a line ends in LF on every
platform. This is a platform test: it can only fail on Windows, where a
text-mode channel writes each LF as CRLF. CSS Syntax 3 sec. 3.3 (Preprocessing
the input stream) replaces each CR, FF and CRLF by one LF before tokenizing, so
CRLF output would still be valid CSS that reads back the same; the test pins
byte reproducibility, the same bytes on every platform. [cat -e] marks each
line end with $ and shows a carriage return as ^M.

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
  warning: read_declaration: bad value for : expected a value at bad.css:2:14 (in component)$
  warning: .a { color: red }$
  warning: .b { color: }$
  warning:              ^$
  warning: $

The help page:

  $ cascade fmt --help=plain | grep -c "$(printf '\r')"
  0
  [1]

Sec. 3.3 turns a CRLF, a lone CR and an FF each into one LF, so a file with
those line breaks formats to the bytes of its LF twin above.

  $ cascade fmt two.css > two.out
  $ printf '.a { color: red }\r\n.b { color: blue }\r\n' > crlf.css
  $ cascade fmt crlf.css | cat -e
  .a {$
    color: red;$
  }$
  .b {$
    color: blue;$
  }$
  $ cascade fmt crlf.css | cmp - two.out
  $ printf '.a { color: red }\r.b { color: blue }\r' > cr.css
  $ cascade fmt cr.css | cmp - two.out
  $ printf '.a { color: red }\f.b { color: blue }\f' > ff.css
  $ cascade fmt ff.css | cmp - two.out

Standard input is read as a file is:

  $ cat crlf.css | cascade fmt - | cmp - two.out

A preserved comment keeps its text, which sec. 3.3 has already rewritten with
LF:

  $ printf '/*! a\r\nb */\n.a { color: red }\n' > bang.css
  $ cascade fmt bang.css | cat -e
  /*! a$
  b */$
  .a {$
    color: red;$
  }$

A custom property value spanning a CRLF formats as its LF twin:

  $ printf '.a { --x: a\r\n  b }\n' > custom-crlf.css
  $ printf '.a { --x: a\n  b }\n' > custom-lf.css
  $ cascade fmt custom-lf.css > custom-lf.out
  $ cascade fmt custom-crlf.css | cat -e
  .a {$
    --x: a$
    b;$
  }$
  $ cascade fmt custom-crlf.css | cmp - custom-lf.out

In a string, a backslash before a newline is a line continuation: consume a
string token (sec. 4.3.5) consumes both and appends nothing, so each line
break gives the string "ab":

  $ printf '.a { content: "a\\\r\nb" }\n' > continuation-crlf.css
  $ printf '.a { content: "a\\\rb" }\n' > continuation-cr.css
  $ printf '.a { content: "a\\\fb" }\n' > continuation-ff.css
  $ cascade fmt --minify continuation-crlf.css | cat -e
  .a{content:"ab"}$
  $ cascade fmt --minify continuation-cr.css | cat -e
  .a{content:"ab"}$
  $ cascade fmt --minify continuation-ff.css | cat -e
  .a{content:"ab"}$

cascade writes no file itself; a redirect hands it one as standard output,
which on Windows is a file handle rather than a pipe:

  $ cascade fmt crlf.css > out.css
  $ cat -e out.css
  .a {$
    color: red;$
  }$
  .b {$
    color: blue;$
  }$
