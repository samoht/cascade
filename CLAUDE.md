# Cascade

A CSS generation and manipulation library and CLI: a typed CSS AST, parser,
pretty-printer, structural diff and optimizer, with no Tailwind-specific code.
`README.md` covers what the tool does, how it is built and tested, and the
oracle corpora the suite replays.

## Scope

Cascade is scoped to CSS text and CSS ASTs: parse, print, minify, diff,
fold/map/sort, and safe AST transforms. A context-supplied evaluation is in
scope when the caller passes the data in through an explicit closed context
record; `Css.Context.t` is that type for property-value transforms, and
theme-based `var()` output is the worked example. Tailwind-specific code, and
Cascade APIs that exist only to serve Tailwind, belong in the `tw` project.

`lib/` is flat and large. Read the interface next to a module rather than
guessing from its name, and prefer extending an existing pass to adding one.

## Working on it

- Start a behavioural change with a test that fails on current `main`. A test
  that passes before the fix is pinning something else.
- When a test oracle contradicts the code, surface the contradiction and ask
  which side is right. Never rewrite an expected output to match what the code
  prints, and never set an expectation by calling the function under test.
- Sealed ADTs over open extension: nothing outside this repo adds a variant, so
  a new case should stop every site that decides about it from compiling.
- Build a string with `Buffer`, the internal `Pp`, or `String.concat ""`, not
  with `^`, `Printf.sprintf` or `Fmt.str`.
