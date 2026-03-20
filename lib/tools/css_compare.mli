(** CSS comparison utilities for testing using the proper CSS parser.

    {1:overview Overview}

    This module provides tools for comparing CSS stylesheets and detecting
    differences at the structural level. It parses CSS using the proper CSS
    parser and reports differences in a structured format suitable for testing
    and debugging.

    The comparison results are represented as structured differences that
    distinguish between different types of changes: additions, removals, content
    modifications, and reorderings. *)

(** {1:diffs Difference types}

    Tree difference types are defined in the {!Tree_diff} module. *)

val strip_header : string -> string
(** [strip_header css] removes a leading header comment if present. *)

val compare : string -> string -> bool
(** [compare a b] returns [true] when [a] and [b] are structurally equivalent
    CSS ASTs.

    If either CSS string fails to parse, falls back to string equality. This
    ensures the function never fails but may give false negatives for equivalent
    CSS with different formatting when parsing fails. *)

type t =
  | Tree_diff of Tree_diff.t  (** CSS AST differences found *)
  | String_diff of String_diff.t  (** No structural diff but strings differ *)
  | No_diff  (** Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error
      (** Result of diffing two CSS strings - either a diff or parse errors *)

val diff : expected:string -> actual:string -> t
(** [diff ~expected ~actual] parses both CSS strings and returns their diff or
    parse errors if parsing fails. Uses smart detection to choose between tree
    and string diff. *)

val diff_with_mode :
  mode:[ `Auto | `Tree | `String ] -> expected:string -> actual:string -> t
(** [diff_with_mode ~mode ~expected ~actual] compares CSS with explicit diff
    mode selection.
    @param mode
      [`Auto] (default behavior): use tree diff for structural changes, string
      diff otherwise. [`Tree]: force structural tree-based diff. [`String]:
      force string diff, skip parsing. *)

val as_tree_diff : t -> Tree_diff.t option
(** [as_tree_diff result] extracts [Tree_diff.t] from a diff result, returning
    [None] for other result types. *)

val pp : ?expected:string -> ?actual:string -> t Fmt.t
(** [pp ?expected ?actual] formats a diff_result with optional labels.
    @param expected Label for expected CSS (default: "Expected").
    @param actual Label for actual CSS (default: "Actual"). *)

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
(** Statistics about CSS differences *)

val stats : expected_str:string -> actual_str:string -> t -> stats
(** [stats ~expected_str ~actual_str result] computes statistics from a diff
    result (alias for [compute_stats]). *)

val pp_stats : stats Fmt.t
(** [pp_stats] formats a stats record. *)
