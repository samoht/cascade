(** CSS optimization utilities *)

open Declaration
open Stylesheet

(** {1 Scope Assumption} *)

type scope = [ `Fragment | `Stylesheet ]
(** The [scope] knob tells the optimizer how much surrounding CSS context the
    input might be embedded in.

    [`Fragment] (the default) treats the input as an excerpt that may be
    concatenated with arbitrary other author CSS - earlier [<link>], later
    [<style>], bundler concatenation, layer statements outside the file,
    caller-side composition. Only semantics-preserving rewrites under any
    surrounding CSS are allowed; resetful shorthands are synthesised only when
    the local longhand run is {e reset-closed} (every absent longhand the
    shorthand would reset is present in the run), so the shorthand cannot shadow
    a prior cascade write the optimizer cannot see.

    [`Stylesheet] asserts the caller controls the whole author stylesheet graph
    (after [@import] resolution). The optimizer may then synthesise a
    partial-coverage shorthand because the omitted longhand resets are
    guaranteed not to disturb any prior write. *)

(** {1 Declaration Optimization} *)

val duplicate_buggy_properties : declaration list -> declaration list
(** [duplicate_buggy_properties decls] duplicates known buggy properties for
    browser compatibility. Some WebKit properties need to be duplicated for
    older Safari versions. See: https://bugs.webkit.org/show_bug.cgi?id=101180.
*)

val deduplicate_declarations :
  ?scope:scope -> declaration list -> declaration list
(** [deduplicate_declarations ?scope decls] removes overridden declarations
    following CSS cascade rules: !important wins over normal, and among same
    importance the last one wins. [scope] (default [`Fragment]) gates
    partial-coverage shorthand synthesis; see the {!scope} doc. *)

type ctx
(** Optimization context: the {!scope} together with the registered-custom-
    property predicate and the [lossless], [aggressive], [extend_lists], and
    [closed_world] knobs. The shorthand composers take it; build one with
    {!ctx_of_scope}. *)

val ctx_of_scope :
  ?lossless:bool ->
  ?aggressive:bool ->
  ?regroup:bool ->
  ?extend_lists:bool ->
  ?closed_world:bool ->
  ?objective:Ctx.objective ->
  ?enforce_spec:bool ->
  ?stats:Stats.t ->
  scope option ->
  ctx
(** [ctx_of_scope ?lossless ?aggressive ?extend_lists ?closed_world scope]
    builds the context the composers take; [None] is [`Fragment]. [aggressive]
    forces the expensive global factoring fixpoint to run even when its
    preflight predicts low gain. [extend_lists] is for direct DAG-scheduler
    experiments; the main stylesheet optimizer enables guarded selector-list
    extension internally. [closed_world] asserts the caller knows the exact HTML
    and that no element matches two clashing selectors, so the optimizer may
    merge rules it would otherwise keep apart; unsafe for an unknown DOM. *)

val compose_shorthands :
  ctx:ctx -> (int * declaration) list -> (int * declaration) list
(** [compose_shorthands ~ctx decls] runs the shorthand-composition pipeline over
    index-tagged declarations: longhands fold into shorthands, resets reorder,
    and shadowed longhands drop. Each declaration it leaves unchanged is
    returned with its physical identity preserved, so a no-op shares every
    element with the input. *)

val merge_box_shorthand_longhands :
  (int * declaration) list ->
  (int * declaration) list ->
  (int * declaration) list
(** [merge_box_shorthand_longhands source decls] folds box-shorthand longhands
    that follow a matching box shorthand back into it. A declaration that
    absorbs nothing is returned unchanged by physical identity. *)

val merge_overflow_longhands :
  (int * declaration) list -> (int * declaration) list
(** [merge_overflow_longhands decls] folds [overflow-x] and [overflow-y] into
    the [overflow] shorthand when both appear with matching importance. A
    declaration left unmerged keeps its physical identity. *)

val drop_invalid : t -> t
(** [drop_invalid ss] removes every declaration whose typed value contains an
    [Invalid] arm cascade detected at parse time (CSS spec violations that
    cascade preserved verbatim for round-trip). Run as part of minify-time
    spec-based optimization. *)

val drop_unknown_at_rules : t -> t
(** [drop_unknown_at_rules ss] removes every [Unknown_at_rule] statement at any
    block depth. CSS Syntax 3 sec. 5.4.1 says an unknown at-rule is discarded;
    the parser preserves them in the AST for fidelity, and minify-time
    canonicalization then drops them. *)

val drop_empty_rules : t -> t
(** [drop_empty_rules ss] removes top-level rules and at-rule frames whose body
    is empty (no declarations and no nested rules). *)

(** {1 Edge Model} *)

(** Existential wrapper that hides the value type of a typed property tag, so
    properties of different value types can live in the same edge list. *)
type packed_property = Packed : 'a Properties.property -> packed_property

type edge = {
  summary : Selector_summary.t;
  property : packed_property;
  important : bool;
}
(** A single property write in the CSS graph: a selector's subject summary
    paired with the typed property it writes. This is the (selector, property)
    edge from the CSS-graph model of Hague-Lin-Hong (TOPLAS 2019), modulo the
    cheap subject-summary fingerprint used in place of full selector
    intersection. *)

val edges_of_rule : rule -> edge list
(** [edges_of_rule r] enumerates the property writes in [r]. If [r.selector] is
    a comma-separated list, one edge is emitted per (subject summary, property)
    pair; otherwise one edge per declaration. Useful for asserting no-new-edges
    invariants on rule rewrites and for the fuzz harness's selector-intersection
    and biclique vectors. *)

(** {1 Rule Optimization} *)

val single_rule : ?scope:scope -> rule -> rule
(** [single_rule ?scope rule] deduplicates declarations in one rule. *)

val rules : ?scope:scope -> rule list -> rule list
(** [rules ?scope rs] optimizes a list of flat rules. *)

(** {1 Nested Structure Optimization} *)

(** {1 Stylesheet Optimization} *)

val apply_property_duplication : t -> t
(** [apply_property_duplication ss] applies only property duplication for
    browser compatibility without other optimizations. *)

val stylesheet :
  ?scope:scope ->
  ?flatten_nesting:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?aggressive:bool ->
  ?regroup:bool ->
  ?closed_world:bool ->
  ?objective:Ctx.objective ->
  ?prune_unused_custom_props:bool ->
  ?stats:Stats.t ->
  t ->
  t
(** [stylesheet ?scope ?flatten_nesting ?lossless ?enforce_spec ss] optimizes an
    entire stylesheet while preserving cascade semantics for any DOM (with
    [closed_world] off, the default). When [@supports] blocks are present
    alongside top-level rules, optimization is limited because the stylesheet
    structure separates rules from [@supports] blocks, losing their relative
    ordering.

    When [flatten_nesting] is [true] (default [false]) nested rules are
    desugared into flat rules: child selectors with [&] have the parent selector
    substituted in, child selectors without [&] are joined to the parent with
    the descendant combinator, and at-rules nested inside a rule are emitted at
    the top level with the parent selector applied to their inner rules.

    [scope] (default [`Fragment]) gates partial-coverage shorthand synthesis;
    see the {!scope} doc.

    [lossless] disables colour approximation while keeping exact colour
    canonicalisation.

    When [enforce_spec] is [false] (default) the optimizer may treat baseline
    feature queries as known facts and elide [@supports] guards whose condition
    is satisfied in maintained evergreen browsers; [true] keeps every feature
    query and applies only CSS-text-and-spec-provable rewrites.

    When [closed_world] is [true] (default [false]) the optimizer assumes the
    caller knows the exact HTML and that no element ever matches two clashing
    selectors, so it may merge rules it would otherwise keep apart. Unsafe: the
    page can render wrong if such an element appears, including one a script
    adds at runtime. This is about the HTML, separate from [scope] (how much of
    the CSS you control). The default is safe for any page; see
    {!Ctx.closed_world}.

    When [prune_unused_custom_props] is [true] (default [false]) custom-property
    bindings referenced by no [var()] anywhere are dropped. This is opt-in
    because it assumes a complete stylesheet with no out-of-band reader (another
    stylesheet, or [getComputedStyle]) - the same closed-world assumption as
    {!Css.inline_vars}.

    [stats] records what this run did; read it back with {!Stats.snapshot}.
    Without it the run counts into a recorder of its own that nobody reads. *)

val flatten_nesting : t -> t
(** [flatten_nesting ss] returns [ss] with every nested rule flattened into a
    top-level rule. Equivalent to the [~flatten_nesting:true] mode of
    {!stylesheet} but without the deduplication / merge passes. *)
