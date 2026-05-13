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

type t =
  | Tree_diff of Tree_diff.t  (** Structural AST differences. *)
  | String_diff of String_diff.t
      (** Strings differ but no structural change was detected. *)
  | No_diff
      (** No differences were reported under the selected {!mode}. Depending on
          the mode this means byte-identical inputs (after tool-header
          stripping), equal ASTs, or equal canonical minified forms. *)
  | Both_errors of Error.t * Error.t
  | Expected_error of Error.t
  | Actual_error of Error.t

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

val diff : ?mode:mode -> string -> string -> t
(** [diff ?mode expected actual] returns the diff between two CSS strings. A
    leading [/*! ... */] tool banner on either side is stripped before
    comparison. Parsing failures surface as [_error] variants. *)

val equal : ?mode:mode -> string -> string -> bool
(** [equal ?mode a b] is [true] iff [diff ?mode a b] is {!No_diff}. *)

val as_tree_diff : t -> Tree_diff.t option
(** [as_tree_diff result] returns the underlying [Tree_diff.t] when [result] is
    a {!Tree_diff}; [None] otherwise. *)

val pp : ?expected:string -> ?actual:string -> Buffer.t -> t -> unit
(** [pp ?expected ?actual buf result] formats [result] into [buf]. The
    [expected]/[actual] labels are used in the rendered header (defaults:
    ["Expected"], ["Actual"]). *)

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
  container_changes : int;
}
(** Summary of differences extracted from a {!t}. *)

val stats : expected_str:string -> actual_str:string -> t -> stats
(** [stats ~expected_str ~actual_str result] computes a {!stats} record from a
    diff [result]. *)

val pp_stats : Buffer.t -> stats -> unit
(** [pp_stats buf stats] formats a stats record into [buf]. *)

(** {1:property_values Property-scoped value comparison} *)

val equivalent_value : property:string -> string -> string -> bool
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
