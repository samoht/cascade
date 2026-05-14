(lang dune 3.21)
(using mdx 0.4)
(name cascade)
(generate_opam_files true)
(source (github samoht/cascade))
(license ISC)
(maintainers "Thomas Gazagnaire")
(authors "Thomas Gazagnaire")

(package
 (name cascade)
 (synopsis "CSS generation and manipulation library for OCaml")
 (description
  "Cascade provides a typed CSS AST, parser, pretty-printer, and optimizer.
   Supports selectors, properties, values, media queries, container queries,
   @supports, @property, @layer, @keyframes, @font-face, and CSS variables.")
 (depends
  (ocaml (>= 5.2))
  dune
  dune-build-info
  (alcotest :with-test)
  (alcobar :with-test)
  (astring :with-test)
  (lambdasoup :with-test)
  (mdx :with-test)
  (re :with-test)
  fmt
  uutf
  cmdliner))
