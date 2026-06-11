(** Closed-world inlining transforms.

    Lives one layer above {!Context}: drives the typed-evaluator primitives
    ({!Context.eval}, custom-property lookup) and threads scope through the AST.
    Both transforms assume a closed world (no runtime mutation, full file
    resolution). *)

val vars : ?keep_vars:string list -> Stylesheet.t -> Stylesheet.t
(** [vars ?keep_vars stylesheet] substitutes [var(--name)] references with the
    value of the corresponding [--name] declaration in [stylesheet] and drops
    the now-unused custom-property definitions. Names listed in [keep_vars]
    (with or without the leading [--]) keep their definitions and remain as live
    [var()] references in the output. *)

val decode_import_url : string -> string
(** [decode_import_url s] strips the [url(...)] wrapper and any surrounding
    quotes from an [@import] URL string as held in
    {!Stylesheet.import_rule.url}. The parser preserves the verbatim source form
    there for round-tripping; this helper recovers the bare URL. *)

val imports :
  ?query:Context.query ->
  ?layer_order:string list ->
  Context.loader ->
  Stylesheet.t ->
  Stylesheet.t
(** [imports ?query ?layer_order loader stylesheet] replaces every [@import]
    rule in [stylesheet] with the body of the imported stylesheet looked up
    through [loader]. Imports the loader cannot resolve are left in place; a
    cyclic [@import] is broken by dropping the second visit. The walk descends
    into nested at-rules and rule bodies, so imports declared inside them are
    inlined too; the caller is responsible for preloading [loader.imports] with
    every transitively-referenced stylesheet body. *)
