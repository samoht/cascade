(** Compare two CSS stylesheets and report their differences.

    The standard entry points are {!diff} (returns a structured {!t}) and
    {!equal} (the boolean wrapper). Both accept an optional {!mode} that selects
    how a non-equal pair is reported.

    Cascade does not implement a general CSS semantic-equivalence rewriter. The
    closest the library comes is mode [`Canonical], which compares the inputs by
    their optimized canonical minified serialization through
    {!Cascade.Css.to_string}. That collapses whitespace, color spellings,
    leading-/trailing-zero normalisations, optimizer-preserved shorthand
    choices, and other choices the optimizer and pretty-printer make; it does
    {b not} reason about browser computed values or cascade-affecting rule
    reorderings.

    Those bytes are the verdict in mode [`Canonical]. Canonical means equivalent
    inputs project to one form, so two canonical forms that differ are either
    two different stylesheets or one projection missing a normalisation key -
    and the comparison reports a difference either way. The tree diff explains a
    difference (which rule, which declaration, which value); it does not
    overrule the bytes. A byte difference it walked past comes back as a
    {!constructor-String_diff} of the two canonical forms.

    Both causes are findings, not tolerances. A missing key is fixed by adding
    the key to the projection (its normalisations are listed under
    {!Cascade.Css.canonicalize_rule_order}); a difference the tree diff cannot
    see is fixed in {!module-Tree_diff}. *)

open Cascade

(** {1:diffs Difference types}

    The detailed tree-diff vocabulary lives in the {!module-Tree_diff} module;
    this module wraps it with parse-error handling and a string-diff fallback.
*)

type result =
  | Tree_diff of Tree_diff.t  (** Structural AST differences. *)
  | String_diff of String_diff.t
      (** Strings differ but no structural change was detected. *)
  | No_diff  (** No difference under the selected {!mode}. *)
  | Both_errors of Error.t * Error.t
  | Expected_error of Error.t
  | Actual_error of Error.t

type t = {
  result : result;
  expected_warnings : Error.t list;
  actual_warnings : Error.t list;
}
(** A comparison outcome plus the parse warnings each side accumulated. A
    declaration the parser rejects is dropped from that side's AST, so without
    the warnings a structural diff would read as a phantom addition on the side
    that parsed (or as no difference at all when both sides collapse to the same
    AST). The warnings are empty in mode [`String], which never parses, and when
    the header-stripped inputs are bytewise equal. *)

type mode = [ `Auto | `Tree | `String | `Canonical ]
(** CSS comparison mode.

    - [`Auto] (default) -- tree diff when the ASTs differ, string diff
      otherwise.
    - [`Tree] -- structural diff only; formatting-only differences collapse to
      {!No_diff}.
    - [`String] -- character-level diff; the inputs are not parsed.
    - [`Canonical] -- parse both stylesheets, serialize optimized minified
      outputs, and compare those outputs. This includes value spellings that
      Cascade canonicalizes as equivalent, such as [transparent] and [#0000] in
      color positions. Equal outputs are {!No_diff}; differing outputs are a
      difference, reported as a tree diff of the two when the walk reaches it
      and as a string diff of them when it does not.

    The projection runs no rewrite whose applicability depends on the order the
    input happens to put its rules in. Factoring shared declarations into a
    selector list and synthesising nesting from a run of rules both fire only
    where the rules are already adjacent, so one stylesheet written two ways
    reaches two different forms and the comparison reports a difference that is
    not there. A canonical form cannot depend on the spelling it exists to see
    past, so [Css.optimize] runs these under [~regroup:true] and the projection
    does not. Adding a rewrite here means checking it against that: if
    reordering the input changes whether it applies, it belongs behind
    [regroup]. *)

val diff :
  ?mode:mode ->
  ?lossless:bool ->
  ?prune_unused_custom_props:bool ->
  string ->
  string ->
  t
(** [diff ?mode expected actual] returns the diff between two CSS strings. A
    leading [/*! ... */] tool banner on either side is stripped before
    comparison. Parsing failures surface as [_error] variants. [lossless]
    preserves exact color channels during canonical comparison.

    [prune_unused_custom_props] (default [false], [`Canonical] mode only) drops
    custom-property bindings referenced by nothing on both sides before
    comparing, so two stylesheets that differ only by a dead binding compare
    equal. Opt-in: it makes the comparator blind to dead-custom-property
    divergences, so enable it only when that render-no-op difference is
    immaterial (e.g. a parity harness against output that omits the binding). *)

val equal :
  ?mode:mode ->
  ?lossless:bool ->
  ?prune_unused_custom_props:bool ->
  string ->
  string ->
  bool
(** [equal ?mode a b] is [true] iff [diff ?mode a b] is {!No_diff}. Under
    [`Canonical] that is exactly byte equality of the two canonical forms. *)

val as_tree_diff : t -> Tree_diff.t option
(** [as_tree_diff result] returns the underlying [Tree_diff.t] when [result] is
    a {!constructor-Tree_diff}; [None] otherwise. *)

val pp :
  ?expected:string ->
  ?actual:string ->
  ?color:bool ->
  ?depth:int ->
  Buffer.t ->
  t ->
  unit
(** [pp ?expected ?actual ?color ?depth buf result] renders each side's parse
    warnings into [buf], then formats [result] below them. Warnings lead because
    a declaration the parser dropped qualifies every difference that follows.
    The [expected]/[actual] labels are used in the rendered header and warning
    lines (defaults: ["Expected"], ["Actual"]). [color] (default [false]) wraps
    diff markers in ANSI escapes; the caller decides whether the destination
    supports colour. [depth] bounds the rendered tree levels as in
    {!Tree_diff.pp} (default: unbounded).

    {!pp_warnings} and {!pp_diff} are the two halves, for callers that need to
    size or bound the sections independently. *)

val pp_warnings :
  ?expected:string -> ?actual:string -> ?max:int -> Buffer.t -> t -> unit
(** [pp_warnings ?expected ?actual ?max buf result] renders only the parse
    warnings each side accumulated. [max] caps how many are printed per side
    (default: all); the remainder is reported as a count, so a stylesheet that
    trips the same unsupported syntax hundreds of times cannot bury the diff
    those warnings qualify. *)

val pp_diff :
  ?expected:string ->
  ?actual:string ->
  ?color:bool ->
  ?depth:int ->
  Buffer.t ->
  t ->
  unit
(** [pp_diff ?expected ?actual ?color ?depth buf result] renders only the
    difference report, without the parse warnings. *)

val has_warnings : t -> bool
(** [has_warnings result] is [true] when either side accumulated a parse
    warning. *)

(** {1:stats Statistics} *)

type stats = {
  expected : string;
  actual : string;
  expected_chars : int;
  actual_chars : int;
  added_rules : int;
  removed_rules : int;
  modified_rules : int;
  reordered_rules : int;
  rearranged_rules : int;
  regrouped_rules : int;
  container_changes : int;
  layer_order_swaps : int;
}
(** Summary of differences extracted from a {!t}. [layer_order_swaps] counts the
    pairs of cascade layers the two sheets declare in the opposite relative
    order, as {!Tree_diff.type-layer_order_diff} reports them. *)

val stats : expected_str:string -> actual_str:string -> t -> stats
(** [stats ~expected_str ~actual_str result] computes a {!type-stats} record
    from a diff [result]. *)

val pp_stats : Buffer.t -> stats -> unit
(** [pp_stats buf stats] formats a stats record into [buf]. *)

(** {1:property_values Property-scoped value comparison} *)

val equivalent_value :
  ?lossless:bool -> property:string -> string -> string -> bool
(** [equivalent_value ~property a b] returns [true] when [a] and [b], parsed as
    the right-hand side of a [property:] declaration, share the same canonical
    declaration serialization through {!Cascade.Css.to_string}. This is the
    value-level analogue of mode [`Canonical] in {!diff}; it does not model
    browser computed-value semantics.

    Example: [equivalent_value ~property:"color" "transparent" "#0000"] is
    [true], because the two forms minify to the same canonical color in a value
    position. *)

(** {1:tool_headers Tool-banner interop}

    Minifier output frequently starts with a [/*! ... */] banner identifying the
    tool. These helpers normalise that banner away so two outputs can be
    compared on their CSS content. {!diff} and {!equal} already strip the banner
    internally -- these are exposed only for callers that want to do their own
    pre-processing. *)

val strip_tool_header : string -> string
(** [strip_tool_header css] removes a leading [/*! ... */] banner comment
    emitted by CSS tools, then trims surrounding whitespace. Regular [/* ... */]
    comments are preserved because they may be part of the CSS being compared.
*)
