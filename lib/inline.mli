(** Closed-world inlining transforms.

    Lives one layer above {!Context}: drives the typed-evaluator primitives
    ({!Context.eval}, custom-property lookup) and threads scope through the AST.
    Both transforms assume a closed world (no undeclared runtime mutation, full
    file resolution). *)

val vars :
  ?keep_vars:string list ->
  ?warn:(string -> unit) ->
  Stylesheet.t ->
  Stylesheet.t
(** [vars ?keep_vars ?warn stylesheet] substitutes [var(--name)] references with
    the value of the corresponding [--name] declaration and deletes the
    definition, but only for a variable with a single definition (its value is
    then unambiguous). A variable in [keep_vars], or one redefined in a
    different scope (a real cascade override such as dark mode), keeps its
    definition and stays a live [var()] reference. [warn] is called with each
    name (leading [--]) that could not be inlined because it is redefined in a
    different scope. *)

val mentioned_custom_names : Stylesheet.t -> string list
(** [mentioned_custom_names stylesheet] is every custom-property name (leading
    [--]) the stylesheet still mentions: declared by a declaration, referenced
    by a [var()] in a declaration or in an at-rule condition (fallbacks
    included), or queried by a [style()] container query. A [@property] body is
    not a mention, so a registration never keeps itself. *)

val layer_order : Stylesheet.t -> string list option
(** [layer_order stylesheet] is the cascade layer order [stylesheet] declares,
    weakest first, as one dotted path per layer, or [None] when the order
    depends on something static analysis cannot settle: a layer first named
    inside a conditional group is introduced there only when the condition
    holds, and one named inside an {!Stylesheet.Origin} block belongs to that
    origin's own stack. This is the order {!vars} resolves a custom property
    defined across several layers against, and [None] is the answer that stops
    it. *)

val flattening_layers_is_safe : Stylesheet.t -> bool
(** [flattening_layers_is_safe stylesheet] is [true] when dropping every
    [@layer] wrapper in [stylesheet] leaves the same declaration winning each
    cascade slot. Unwrapping a layer replays the stack as document order and
    lets specificity speak again, which only holds where the two already agree
    on every slot two layers write. [false] also answers for a sheet where that
    is out of reach: an order {!layer_order} cannot rank, an anonymous layer,
    another origin's stack, a [@scope] block, a nested rule, or a declaration
    broad enough to write any slot. *)

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
