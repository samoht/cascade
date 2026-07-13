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
    reorderings. *)

open Cascade

(** {1:diffs Difference types}

    The detailed tree-diff vocabulary lives in the {!module-Tree_diff} module;
    this module wraps it with parse-error handling and a string-diff fallback.
*)

type result =
  | Tree_diff of Tree_diff.t  (** Structural AST differences. *)
  | String_diff of String_diff.t
      (** Strings differ but no structural change was detected. *)
  | No_diff of { canonical_byte_diff : (string * string) option }
      (** Structurally equivalent under the selected {!mode}.
          [canonical_byte_diff = None] means the inputs were bytewise equal
          (after header strip / canonical minify). [Some (expected, actual)]
          appears under [`Canonical] when the structural comparator found no
          difference but the canonical-minified bytes still differed - i.e.
          cascade's canonical pass hasn't (yet) collapsed those textual
          variants. The two strings let maintainers inspect which gap to chip
          away at; callers matching [No_diff _] get the right equality answer
          either way. *)
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

    - [`Auto] (default) — tree diff when the ASTs differ, string diff otherwise.
    - [`Tree] — structural diff only; formatting-only differences collapse to
      {!No_diff}.
    - [`String] — character-level diff; the inputs are not parsed.
    - [`Canonical] — parse both stylesheets, serialize optimized minified
      outputs, and compare those outputs. This includes value spellings that
      Cascade canonicalizes as equivalent, such as [transparent] and [#0000] in
      color positions. If the normalized forms differ, the returned diff is
      reported from those normalized outputs. *)

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
(** [equal ?mode a b] is [true] iff [diff ?mode a b] is {!No_diff}. *)

val as_tree_diff : t -> Tree_diff.t option
(** [as_tree_diff result] returns the underlying [Tree_diff.t] when [result] is
    a {!constructor-Tree_diff}; [None] otherwise. *)

val pp :
  ?expected:string -> ?actual:string -> ?color:bool -> Buffer.t -> t -> unit
(** [pp ?expected ?actual ?color buf result] formats [result] into [buf], then
    renders each side's parse warnings. The [expected]/[actual] labels are used
    in the rendered header and warning lines (defaults: ["Expected"],
    ["Actual"]). [color] (default [false]) wraps diff markers in ANSI escapes;
    the caller decides whether the destination supports colour. *)

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
  regrouped_rules : int;
  container_changes : int;
}
(** Summary of differences extracted from a {!t}. *)

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
    internally — these are exposed only for callers that want to do their own
    pre-processing. *)

val strip_tool_header : string -> string
(** [strip_tool_header css] removes a leading [/*! ... */] banner comment
    emitted by CSS tools, then trims surrounding whitespace. Regular [/* ... */]
    comments are preserved because they may be part of the CSS being compared.
*)
