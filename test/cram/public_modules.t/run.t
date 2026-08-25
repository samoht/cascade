Cascade keeps its supported CSS modules public, including the facade aliases,
the stylesheet traversal API and the documented parser roots.

  $ ocamlfind ocamlc -package cascade -c intended.ml

Optimizer implementation modules are not part of the installed API.  Report
any one that an external client can still compile against.

  $ for module in Rule_graph Rule_scheduler Pool Order_maintenance Preflight Ctx Cover Edge Loop Rule_candidate Rule_rewrite Factor_safe Gzip_size Index Declaration_intf Properties_intf Selector_intf Stylesheet_intf Values_intf Variables_intf Baseline Block Common Factor Flatten Inline Merge Rule Rule_index Rule_order Shorthand Size Summary; do
  >   printf 'module Internal = struct include Cascade.%s end\n' "$module" > accidental.ml
  >   if ocamlfind ocamlc -package cascade -c accidental.ml >/dev/null 2>&1; then
  >     echo "EXPOSED $module"
  >   fi
  > done
