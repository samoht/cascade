scripts/check_ascii.sh --staged holds the staged content to 7-bit ASCII.
Run by hand, with no names on its standard input, it collects the staged
names itself, so it cannot pass because it was given nothing to check.
[ascii] masks the offending bytes so this transcript stays ASCII.

  $ check=$PWD/../../../scripts/check_ascii.sh
  $ ascii () { { "$@"; echo "exit $?"; } 2>&1 | LC_ALL=C tr '\200-\377' '?'; }
  $ git init -q repo
  $ cd repo
  $ printf 'let x = "\303\251"\n' > bad.ml
  $ printf 'let y = 1\n' > good.ml
  $ git add bad.ml good.ml

With nothing on standard input it still finds the staged byte:

  $ ascii $check --staged < /dev/null
  error: non-ASCII byte in source:
    bad.ml:1:let x = "??"
  use ASCII in comments and \u{XXXX} escapes in string literals.
  exit 1

Names given as arguments narrow the check to those files:

  $ ascii $check --staged good.ml
  exit 0

The staged content is what counts, not the worktree:

  $ printf 'let x = 1\n' > bad.ml
  $ ascii $check --staged < /dev/null
  error: non-ASCII byte in source:
    bad.ml:1:let x = "??"
  use ASCII in comments and \u{XXXX} escapes in string literals.
  exit 1
