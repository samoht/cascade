Cascade keeps its supported CSS modules public, including the facade aliases,
the stylesheet traversal API and the documented parser roots.

  $ ocamlfind ocamlc -package cascade -c intended.ml

The library's own alias module names every module it holds, public or not, so
the build says what there is to check. A module is reachable when an external
client can include it, which needs its compiled interface on the include path:

  $ printf 'include Cascade\n' > aliases.ml
  $ ocamlfind ocamlc -package cascade -i aliases.ml |
  >   sed -n 's/^module \([A-Za-z0-9_]*\) = Cascade\..*/\1/p' > modules
  $ while IFS= read -r module; do
  >   printf 'module Probe = struct include Cascade.%s end\n' "$module" > probe.ml
  >   if ocamlfind ocamlc -package cascade -c probe.ml >/dev/null 2>&1; then
  >     echo "$module"
  >   fi
  > done < modules > reachable

These are the modules an external client can use, and the one list a change to
the public API has to update:

  $ cat reachable
  Apply
  Aria
  Color_space
  Component
  Container
  Context
  Css
  Cursor
  Declaration
  Error
  Font_face
  Keyframe
  Lexer
  Loc
  Media
  Nest
  Optimize
  Parser
  Pp
  Properties
  Prune
  Reader
  Resolve
  Selector
  Selector_summary
  Sort
  Source_index
  Stats
  Stylesheet
  Support
  Supports
  Syntax
  Token
  Values
  Variables

Every module lib/dune lists under private_modules is an implementation detail,
and none of them is reachable:

  $ awk '/\(private_modules/ { listing = 1; next }
  >      listing { name = $1; sub(/\).*/, "", name)
  >                print toupper(substr(name, 1, 1)) substr(name, 2)
  >                if ($0 ~ /\)/) listing = 0 }' "$PWD/../../../lib/dune" |
  >   sort > private
  $ comm -12 private reachable

Every module is one or the other, so the private list above was read whole:

  $ sort private reachable | comm -3 - modules
