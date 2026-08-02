(** Canonical cascade-safe rule ordering.

    Models the cascade as the CSS-graph dependency of Hague, Lin and Hong ("CSS
    Minification via Constraint Solving", TOPLAS 2019): two flat style rules are
    order-constrained only when they overlap in {e both} dimensions - their
    selectors may match a common element {e and} they write a common property.
    Disjoint in either dimension means reordering them is cascade-neutral.

    Within a maximal run of consecutive reorderable rules, the dependency forms
    a DAG (an edge from the earlier to the later rule of every conflicting
    pair). [canonicalize] emits the canonical linear extension of that DAG: a
    topological order that, among the rules currently free to come next, always
    takes the one smallest by [(selector, body)]. Selector-first keeps a nesting
    chain's prefixes contiguous and parent-before-child, so downstream nesting
    synthesis still fires. The result is invariant under any cascade-neutral
    reordering of the input, which is what lets the canonical diff treat such
    reorderings as no-ops. *)

val canonical_declarations :
  Declaration.declaration list -> Declaration.declaration list
(** [canonical_declarations decls] reorders a rule's declarations into a
    deterministic content order for cross-rule gzip alignment, keeping the
    relative order of any two whose footprints overlap (same property, or a
    shorthand and a longhand) since that is cascade-significant. Physically
    unchanged when already canonical. *)

val canonicalize : Stylesheet.statement list -> Stylesheet.statement list
(** [canonicalize stmts] reorders each maximal run of consecutive reorderable
    style rules into the canonical order described above. At-rules, nested
    rules, and custom-property rules are barriers: they keep their position and
    split runs.

    A run of consecutive [@property] rules is the one at-rule exception: it
    sorts by name, keeping the last registration of each. CSS Properties and
    Values API 1 sec. 2 makes registrations for different names
    order-independent and gives a name its last registration, so the emission
    order carries no meaning. Every block body is its own run context.

    A [@media] query of the Level 3 form [not all and (X)] is rewritten to the
    Level 4 [not (X)] it is equal to under Media Queries 4 sec. 2.1, since [all]
    is the identity media type. That direction loses support in a Level 3
    parser, so it belongs to the projection rather than to emission.

    A custom property holding a font stack has each quoted multi-word family
    name rewritten as the [<ident>] sequence it unquotes to, which CSS Fonts 4
    sec. 15.3 makes the same family name. Emission keeps whichever spelling the
    author wrote, since unquoting an opaque token stream could corrupt a
    [content] use, so again only the projection can bring the two together.

    A [color(srgb ...)] whose channels all land on a whole byte is rewritten to
    the [rgb()] / hex / named spelling of the same colour, so that
    [color(srgb 1 0 0)] and [rgb(255 0 0)] do not read as a difference under
    [--lossless], which otherwise keeps whichever function was written. Only
    that fold applies: a declaration is kept exactly as it came in unless the
    colour moved. Emission cannot make the same rewrite, since [color()] needs a
    browser that parses it. *)
