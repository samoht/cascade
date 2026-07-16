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
    split runs. Returns the input list physically unchanged when no run
    reorders. *)
