The statement-merging passes are reachable on their own: a caller that runs the
optimizer under its own flag can still collapse a run of adjacent blocks in a
sheet it only minifies.

  $ ocamlfind ocamlc -package cascade -linkpkg consumer.ml -o consumer
  $ ./consumer
  media: @media print{.a{opacity:0}.b{opacity:1}}
  supports: @supports(display:grid){.a{opacity:0}.b{opacity:1}}
  starting-style: @starting-style{.a{opacity:0}.b{opacity:1}}
