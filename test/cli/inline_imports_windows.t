CLI: --inline-imports reads a Windows drive path as a local file. WHATWG URL
reads C: as a scheme (sec. 4.4 scheme state); a drive is the Windows
filesystem convention --inline-imports follows. The test runs only on Windows
(see dune), where cygpath -m writes the drive form C:/...

  $ mkdir -p root
  $ cat > root/a.css <<EOF
  > .a { color: red }
  > EOF
  $ root=$(cygpath -m "$PWD/root")

A drive-absolute path with forward slashes:

  $ cat > entry.css <<EOF
  > @import url("$root/a.css");
  > .e { color: blue }
  > EOF
  $ cascade --minify --inline-imports entry.css
  .a{color:red}.e{color:#00f}

With backslashes, each doubled in the CSS string, where a backslash opens an
escape (CSS Syntax 3 sec. 4.3.7):

  $ win_root=$(cygpath -w "$PWD/root" | sed 's/\\/\\\\/g')
  $ cat > entry-backslash.css <<EOF
  > @import url("$win_root\\\\a.css");
  > .e { color: blue }
  > EOF
  $ cascade --minify --inline-imports entry-backslash.css
  .a{color:red}.e{color:#00f}

A drive-relative path, which Windows resolves against the current directory
of that drive:

  $ drive=$(printf '%s' "$root" | cut -c1-2)
  $ cat > entry-relative.css <<EOF
  > @import url("${drive}root/a.css");
  > .e { color: blue }
  > EOF
  $ cascade --minify --inline-imports entry-relative.css
  .a{color:red}.e{color:#00f}

A drive path is checked against --import-root with its drive kept:

  $ cascade --minify --inline-imports --import-root="$root" entry.css
  .a{color:red}.e{color:#00f}
  $ mkdir -p other
  $ cascade --minify --inline-imports --import-root="$(cygpath -m "$PWD/other")" entry.css 2>&1 | grep -Fq 'outside --import-root'
