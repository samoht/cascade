(** What a [cascade diff] report names and asserts, and the shape of the printed
    report. *)

val vacuous_entries : Cascade_diff.Tree_diff.t -> string list
(** [vacuous_entries d] is every entry of [d] that names a rule and shows no
    change of it. *)

val named_selectors : Cascade_diff.Tree_diff.t -> string list
(** [named_selectors d] is the selectors the entries of [d] that assert
    something name, sorted and unique. *)

val named_containers : Cascade_diff.Tree_diff.t -> string list
(** [named_containers d] is the container conditions [d] names, sorted and
    unique. *)

val swap_partners : Cascade_diff.Tree_diff.t -> string list
(** [swap_partners d] is every selector [d] reports as reordered because a swap
    moved it. *)

type claim = { text : string; ok : bool }
(** An assertion a report entry makes, and whether it holds. *)

val claim : string -> bool -> claim
(** [claim text ok] is the claim [text], holding iff [ok]. *)

val rule_claims :
  expected:Diff_report_index.sheet_index ->
  actual:Diff_report_index.sheet_index ->
  Cascade_diff.Tree_diff.rule_diff ->
  claim list
(** [rule_claims ~expected ~actual r] is what the rule entry [r] asserts,
    checked against the two indexed sheets. *)

val claims :
  expected:Diff_report_index.sheet_index ->
  actual:Diff_report_index.sheet_index ->
  Cascade_diff.Tree_diff.t ->
  claim list
(** [claims ~expected ~actual d] is what every entry of [d] asserts, checked
    against the two indexed sheets. *)

val render_report : ?entries:int -> Cascade_diff.Css_compare.t -> string
(** [render_report ?entries result] is the uncoloured report of [result]. *)

val render_summary :
  expected_str:string ->
  actual_str:string ->
  Cascade_diff.Css_compare.t ->
  string
(** [render_summary ~expected_str ~actual_str result] is the summary line of
    [result]. *)

val top_level_lines : string -> string list
(** [top_level_lines body] is the lines of [body] that open a top-level entry.
*)

val elided : string -> int
(** [elided body] is the count a shortened report says it left out, or [0]. *)

val stats_total : Cascade_diff.Css_compare.stats -> int
(** [stats_total s] is the sum of the change counters of [s]. *)
