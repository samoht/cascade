Cascade links a fixed set of runtime dependencies.  The list is part of what
the library promises: an embedder targeting js_of_ocaml pays for every entry.
Alphabetical, and mtime comes along because mtime.clock exports it.

  $ ocamlfind query -format '%(requires)' cascade
  logs mtime mtime.clock psq uri uutf

Measuring how long a factoring iteration takes needs a monotonic clock, so the
library reads one from mtime rather than the wall clock in unix.  Nothing else
in the library wants an operating system, and unix has no js_of_ocaml
implementation, so it must stay out of the closure.

  $ ocamlfind query -recursive -p-format cascade | grep -Fx unix || echo "unix: not linked"
  unix: not linked
