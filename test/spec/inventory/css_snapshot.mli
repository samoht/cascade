(** CSS snapshot/module membership tracked by Cascade's test scope. *)

type baseline =
  | Snapshot_2024
  | Snapshot_2025
  | Snapshot_2026
  | Current_work
  | Experimental
  | Legacy
  | External

type row = {
  module_name : string;
  level : string;
  baseline : baseline;
  css_text_scope : bool;
  tests : string list;
  fuzzers : string list;
}

val rows : row list
(** In-scope CSS modules and their deterministic/fuzz coverage surfaces. *)

val key : row -> string
(** Stable membership key used by exact snapshot matrix tests. *)

val by_baseline : baseline -> row list
(** Rows with the given maturity or snapshot baseline. *)
