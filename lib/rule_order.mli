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

val canonicalize :
  ?lossless:bool -> Stylesheet.statement list -> Stylesheet.statement list
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
    Level 4 [not (X)] it is equal to under Media Queries 4 sec. 2.3, since [all]
    is the identity media type. That direction loses support in a Level 3
    parser, so it belongs to the projection rather than to emission.

    A bare generic family in a custom property's token stream proves the stream
    is a font stack, and each quoted family name in it is then rewritten as the
    [<ident>] sequence it unquotes to, one word or several, which CSS Fonts 4
    sec. 2.1.1 makes the same family name. Emission keeps whichever spelling the
    author wrote, since unquoting an opaque token stream could corrupt a
    [content] use, so again only the projection can bring the two together.

    A [color(srgb ...)] whose channels all land on a whole byte is rewritten to
    the [rgb()] / hex / named spelling of the same colour, so that
    [color(srgb 1 0 0)] and [rgb(255 0 0)] do not read as a difference under
    [--lossless], which otherwise keeps whichever function was written. Only
    that fold applies: a declaration is kept exactly as it came in unless the
    colour moved. Emission cannot make the same rewrite, since [color()] needs a
    browser that parses it.

    A [none] channel of a Lab-family colour standing as a whole colour-longhand
    value is read as the zero CSS Color 4 sec. 4.4 says a missing component
    behaves as, so [oklab(0% none none / .5)] folds like [oklab(0% 0 0 / .5)]
    and meets the hex a minifier writes for it. Sec. 13.3 keeps that off every
    position the sheet interpolates: a gradient stop, a [color-mix()] operand, a
    shadow colour and a custom-property token stream keep their [none], the pass
    does not enter [@keyframes] or [@starting-style], and a colour whose own
    rule transitions the property it writes keeps its [none]. [lossless] is
    passed through to the fold so the resolved colour respells no further than
    the caller's precision mode allows. Emission keeps [none], which is what the
    value says.

    A top-level [@layer] statement loses every name whose removal leaves the
    sheet's layer order alone, and goes away once it has none left. CSS Cascade
    5 sec. 6.4.3 sorts layers by the order in which they first are declared, so
    a pin the following block repeats declares nothing the block does not, while
    one that is the only or the earliest declaration of its layer stays. A
    position the projection cannot read - an anonymous layer, the layers an
    [@import] carries in, or a layer declared inside a conditional group rule,
    which sec. 6.4.3 has contribute only when the condition holds - blocks the
    fold across it. Emission keeps every pin, since the statement is visible
    through the CSSOM. *)
