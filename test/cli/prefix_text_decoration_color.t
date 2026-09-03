CLI: --minify mirrors an unresolved text-decoration-color under the WebKit name.

Safari and iOS Safari answer text-decoration-color under the legacy
-webkit- spelling as well as the standard one, and the compatibility data
every browser-targeting minifier reads reports the standard longhand as
covered only from 26.2. The default target is Safari and iOS Safari 16.4, so
a value whose colour is not settled at parse time is mirrored under both
names, standard last so it wins wherever it is understood.

  $ cat > unresolved.css <<CSS
  > .a { text-decoration-color: var(--c) }
  > CSS
  $ cascade --minify unresolved.css
  .a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}

A value that reads as a colour is served by the standard longhand on every
declared target, so it carries no alias.

  $ cat > literal.css <<CSS
  > .a { text-decoration-color: blue }
  > CSS
  $ cascade --minify literal.css
  .a{text-decoration-color:#00f}

An authored prefixed declaration owns the compatibility spelling: it keeps its
own value and is not supplemented.

  $ cat > authored.css <<CSS
  > .a { -webkit-text-decoration-color: red; text-decoration-color: var(--c) }
  > CSS
  $ cascade --minify authored.css
  .a{-webkit-text-decoration-color:red;text-decoration-color:var(--c)}

--enforce-spec drops the target facts, leaving the authored declaration alone.

  $ cascade --minify --enforce-spec unresolved.css
  .a{text-decoration-color:var(--c)}
