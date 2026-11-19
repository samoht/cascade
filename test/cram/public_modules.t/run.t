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
  Baseline
  Block
  Color_space
  Common
  Commute
  Component
  Conflict_index
  Container
  Context
  Cover
  Css
  Ctx
  Cursor
  Declaration
  Edge
  Error
  Factor
  Flatten
  Font_face
  Keyframe
  Lexer
  Line_col
  Loc
  Media
  Merge
  Nest
  Optimize
  Parser
  Pp
  Preflight
  Properties
  Prune
  Reach
  Reader
  Resolve
  Rule
  Rule_candidate
  Rule_graph
  Rule_index
  Rule_order
  Rule_order_layer
  Rule_rewrite
  Rule_scheduler
  Selector
  Selector_summary
  Shorthand
  Size
  Sort
  Source_index
  Stats
  Stylesheet
  Subsets
  Support
  Supports
  Syntax
  Token
  Values
  Variables
  Webkit_fallback

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
