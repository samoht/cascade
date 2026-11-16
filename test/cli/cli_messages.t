CLI: the messages cascade writes itself read like its parse warnings.

A compiler writes `file: error: message`, naming the program where no file
is at fault, and an editor or a log filter reads that form. Every message
cascade writes on standard error takes it, so one pattern finds them all.

A file the parser could use none of is the file's error.

  $ printf '@@@@ }}} {{{' > broken.css
  $ cascade broken.css 2>&1 | grep ': error: '
  broken.css: error: parse dropped every rule; refusing to write an empty stylesheet

An option the run refuses is the program's.

  $ printf '.a{color:red}' > a.css
  $ cascade --inline-vars --keep-vars='*' a.css
  cascade: error: --keep-vars does not accept the wildcard "*"; list names explicitly
  [1]
  $ cascade --inline-imports - < a.css
  cascade: error: --inline-imports needs a file path, since relative URLs cannot be resolved from standard input
  [1]

So is a flag that does nothing, as a warning.

  $ cascade --lossless a.css > /dev/null
  cascade: warning: --lossless has no effect without --minify

An import that cannot be inlined is a warning about the file it names.

  $ printf '@import "gone.css";\n.a{color:red}\n' > imports.css
  $ cascade --minify --inline-imports imports.css 2>&1 >/dev/null
  gone.css: warning: cannot read: No such file or directory

`prune` with documents that hold nothing to match is the program's error.

  $ printf '' > empty.html
  $ cascade prune empty.html a.css
  cascade: error: the documents hold no element, so every rule would look unused
  [1]
