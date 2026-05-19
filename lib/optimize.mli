(** CSS optimization utilities *)

open Declaration
open Stylesheet

(** {1 World Assumption} *)

(** The [world] knob tells the optimizer whether the stylesheet is the whole
    relevant author stylesheet graph or only a fragment.

    {b Open_world} (the default) makes no assumption about surrounding CSS:
    earlier [<link>], later [<style>], bundler concatenation, layer
    statements outside the file, or caller-side composition may all be in
    play. Only semantics-preserving rewrites under arbitrary surrounding CSS
    are allowed; in particular, resetful shorthands are only synthesized when
    the local longhand run is {e reset-closed} -- every absent longhand the
    shorthand would reset is present in the run, so the shorthand cannot
    shadow a prior cascade write the optimizer cannot see.

    {b Closed_world} asserts the caller controls the whole author stylesheet
    graph (after [@import] resolution). The optimizer may then synthesize a
    partial-coverage shorthand because the omitted longhand resets are
    guaranteed not to disturb any prior write. *)
type world = Open_world | Closed_world

(** {1 Declaration Optimization} *)

val duplicate_buggy_properties : declaration list -> declaration list
(** [duplicate_buggy_properties decls] duplicates known buggy properties for
    browser compatibility. Some WebKit properties need to be duplicated for
    older Safari versions. See: https://bugs.webkit.org/show_bug.cgi?id=101180.
*)

val deduplicate_declarations : ?world:world -> declaration list -> declaration list
(** [deduplicate_declarations ?world decls] removes overridden declarations
    following CSS cascade rules: !important wins over normal, and among same
    importance the last one wins. [world] (default {!Open_world}) gates
    partial-coverage shorthand synthesis; see the {!world} doc. *)

val drop_invalid : t -> t
(** [drop_invalid ss] removes every declaration whose typed value contains an
    [Invalid] arm cascade detected at parse time (CSS spec violations that
    cascade preserved verbatim for round-trip). Run as part of minify-time
    spec-based optimization. *)

val drop_unknown_at_rules : t -> t
(** [drop_unknown_at_rules ss] removes every [Unknown_at_rule] statement at any
    block depth. CSS Syntax 3 §5.4.1 says an unknown at-rule is discarded; the
    parser preserves them in the AST for fidelity, and minify-time
    canonicalization then drops them. *)

val drop_empty_rules : t -> t
(** [drop_empty_rules ss] removes top-level rules and at-rule frames whose body
    is empty (no declarations and no nested rules). *)

(** {1 Edge Model} *)

type packed_property =
  | Packed : 'a Properties.property -> packed_property
      (** Existential wrapper that hides the value type of a typed property tag,
          so properties of different value types can live in the same edge list.
      *)

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

val single_rule : ?world:world -> rule -> rule
(** [single_rule ?world rule] deduplicates declarations in one rule. *)

val merge_rules : rule list -> rule list
(** [merge_rules rules] merges adjacent rules with identical selectors while
    preserving cascade order. *)

val combine_identical_rules : rule list -> rule list
(** [combine_identical_rules rules] combines consecutive rules with identical
    declarations into comma-separated selectors. *)

val rules : ?world:world -> rule list -> rule list
(** [rules ?world rs] optimizes a list of flat rules. *)

(** {1 Nested Structure Optimization} *)

(** {1 Stylesheet Optimization} *)

val apply_property_duplication : t -> t
(** [apply_property_duplication ss] applies only property duplication for
    browser compatibility without other optimizations. *)

val stylesheet : ?world:world -> ?flatten_nesting:bool -> t -> t
(** [stylesheet ?world ?flatten_nesting ss] optimizes an entire stylesheet
    while preserving cascade semantics. When [@supports] blocks are present
    alongside top-level rules, optimization is limited because the stylesheet
    structure separates rules from [@supports] blocks, losing their relative
    ordering.

    When [flatten_nesting] is [true] (default [false]) nested rules are
    desugared into flat rules: child selectors with [&] have the parent
    selector substituted in, child selectors without [&] are joined to the
    parent with the descendant combinator, and at-rules nested inside a rule
    are emitted at the top level with the parent selector applied to their
    inner rules.

    [world] (default {!Open_world}) gates partial-coverage shorthand
    synthesis; see the {!world} doc. *)

val flatten_nesting : t -> t
(** [flatten_nesting ss] returns [ss] with every nested rule flattened into a
    top-level rule. Equivalent to the [~flatten_nesting:true] mode of
    {!stylesheet} but without the deduplication / merge passes. *)
