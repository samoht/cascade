(lang dune 3.0)
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
  (ocaml (>= 4.14))
  dune
  dune-build-info
  (alcotest :with-test)
  (alcobar :with-test)
  (astring :with-test)
  fmt
  cmdliner))
