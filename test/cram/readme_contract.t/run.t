The first example carries the minifier's practical contract instead of making
readers discover its observable trade-offs much later in the manual.

  $ readme="$PWD/../../../README.md"
  $ opening=$(sed -n '1,36p' "$readme")
  $ printf '%s\n' "$opening" | grep -Fq '0.002'
  $ printf '%s\n' "$opening" | grep -Fq 'maintained evergreen'
  $ printf '%s\n' "$opening" | grep -Fq 'custom-property strings'
  $ printf '%s\n' "$opening" | grep -Fq 'not byte-for-byte'

The conservative command remains a skipped shell example: neither MDX nor a
reader should mistake it for an exact-source-preservation promise.

  $ awk '
  >   /<!-- \$MDX skip -->/ { skip = NR }
  >   /^```bash$/ { in_shell = 1; marked = (NR == skip + 1); next }
  >   /^```$/ { in_shell = 0 }
  >   in_shell && marked && /--minify --lossless --enforce-spec/ { found = 1 }
  >   END { exit !found }
  > ' "$readme"
