CLI: --inline-imports path resolution.

A relative path resolves against the entry file's directory.

  $ mkdir -p sub
  $ cat > sub/inner.css <<EOF
  > .inner { color: blue }
  > EOF
  $ cat > entry.css <<EOF
  > @import url("sub/inner.css");
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports entry.css
  .inner{color:#00f}.e{color:red}

A relative path inside an imported file resolves against the importing
file's directory, not the entry's. A nested @import in [sub/middle.css]
referencing [inner.css] (sibling) works.

  $ cat > sub/middle.css <<EOF
  > @import url("inner.css");
  > .middle { color: green }
  > EOF
  $ cat > entry-rel.css <<EOF
  > @import url("sub/middle.css");
  > .e { color: red }
  > EOF
  $ cascade --minify --inline-imports entry-rel.css
  .inner{color:#00f}.middle{color:green}.e{color:red}

A multi-directory chain: imports traverse correctly through nested
directories.

  $ mkdir -p deep/nested
  $ cat > deep/nested/leaf.css <<EOF
  > .leaf { color: red }
  > EOF
  $ cat > deep/middle.css <<EOF
  > @import url("nested/leaf.css");
  > .middle { color: blue }
  > EOF
  $ cat > entry-deep.css <<EOF
  > @import url("deep/middle.css");
  > .e { color: green }
  > EOF
  $ cascade --minify --inline-imports entry-deep.css
  .leaf{color:red}.middle{color:#00f}.e{color:green}

A path with parent traversal ([../foo.css]) resolves up the directory
tree.

  $ mkdir -p sibling
  $ cat > sibling/dep.css <<EOF
  > .dep { color: red }
  > EOF
  $ cat > sibling/needs-parent.css <<EOF
  > @import url("../entry-parent.css");
  > .child { color: blue }
  > EOF
  $ cat > entry-parent.css <<EOF
  > .parent { color: green }
  > EOF
  $ cat > entry-traverse.css <<EOF
  > @import url("sibling/needs-parent.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-traverse.css
  .parent{color:green}.child{color:#00f}.e{padding:0}

A redundant traversal ([./foo.css]) normalises to the same target as
[foo.css].

  $ cat > simple.css <<EOF
  > .s { color: red }
  > EOF
  $ cat > entry-norm.css <<EOF
  > @import url("./simple.css");
  > EOF
  $ cascade --minify --inline-imports entry-norm.css
  .s{color:red}

A URL with a query string is stripped at file lookup time.

  $ cat > base-q.css <<EOF
  > .b { color: blue }
  > EOF
  $ cat > entry-query.css <<EOF
  > @import url("base-q.css?v=1");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-query.css
  .b{color:#00f}.e{padding:0}

A URL with a fragment is stripped at file lookup time.

  $ cat > entry-frag.css <<EOF
  > @import url("base-q.css#section");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-frag.css
  .b{color:#00f}.e{padding:0}

A path with internal whitespace requires quoting; the parser only
accepts the quoted form.

  $ cat > "with space.css" <<EOF
  > .ws { color: red }
  > EOF
  $ cat > entry-ws.css <<EOF
  > @import url("with space.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-ws.css
  .ws{color:red}.e{padding:0}

A path with backslash-escaped characters resolves to the unescaped path
(per CSS Syntax L3 §4.3.7 escapes are decoded at tokenisation).

  $ cat > "weird name.css" <<EOF
  > .w { color: red }
  > EOF
  $ cat > entry-esc.css <<EOF
  > @import url("weird\ name.css");
  > .e { padding: 0 }
  > EOF
  $ cascade --minify --inline-imports entry-esc.css
  .w{color:red}.e{padding:0}

An import root rejects a lexical parent traversal that leaves the root. The
unresolved import stays in the output, as it does for any unreadable import.

  $ mkdir -p import-boundary/root
  $ cat > import-boundary/outside.css <<EOF
  > .outside { color: red }
  > EOF
  $ cat > import-boundary/root/entry-parent.css <<EOF
  > @import url("../outside.css");
  > .entry { color: blue }
  > EOF
  $ cascade --minify --inline-imports --import-root=import-boundary/root import-boundary/root/entry-parent.css 2>&1 | grep -Fq 'outside --import-root'
  $ cascade --minify --inline-imports --import-root=import-boundary/root import-boundary/root/entry-parent.css 2>/dev/null
  @import"../outside.css";.entry{color:#00f}

An absolute path under the canonical root is accepted.

  $ import_root="$PWD/import-boundary/root"
  $ cat > import-boundary/root/absolute.css <<EOF
  > .absolute { color: green }
  > EOF
  $ cat > import-boundary/root/entry-absolute.css <<EOF
  > @import url("$import_root/absolute.css");
  > .entry { color: blue }
  > EOF
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-absolute.css
  .absolute{color:green}.entry{color:#00f}

An absolute path outside the canonical root is rejected too.

  $ outside="$PWD/import-boundary/outside.css"
  $ cat > import-boundary/root/entry-absolute-out.css <<EOF
  > @import url("$outside");
  > .entry { color: blue }
  > EOF
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-absolute-out.css 2>&1 | grep -Fq 'outside --import-root'
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-absolute-out.css 2>/dev/null | grep -Fq '@import'

A symlink is accepted when its canonical target remains under the root.

  $ ln -s absolute.css import-boundary/root/inside-link.css
  $ cat > import-boundary/root/entry-inside-link.css <<EOF
  > @import url("inside-link.css");
  > .entry { color: blue }
  > EOF
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-inside-link.css
  .absolute{color:green}.entry{color:#00f}

A symlink whose canonical target leaves the root is rejected.

  $ ln -s ../outside.css import-boundary/root/outside-link.css
  $ cat > import-boundary/root/entry-outside-link.css <<EOF
  > @import url("outside-link.css");
  > .entry { color: blue }
  > EOF
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-outside-link.css 2>&1 | grep -Fq 'outside --import-root'
  $ cascade --minify --inline-imports --import-root="$import_root" import-boundary/root/entry-outside-link.css 2>/dev/null
  @import"outside-link.css";.entry{color:#00f}
