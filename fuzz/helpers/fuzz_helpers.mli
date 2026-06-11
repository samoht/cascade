(** Shared assertion helpers used across the fuzz entry points. *)

val assert_invalid_declaration_contract : string -> string -> unit
(** [assert_invalid_declaration_contract label input] asserts that [input] is
    rejected as a declaration under [~strict:true] and recovers (with a warning)
    under [~strict:false]. Used by fuzz tests to pin the strict-rejects /
    lenient-recovers contract for invalid declarations. *)

val shapes_with_rule_runs :
  boundary_shape:(Cascade.Css.Stylesheet.statement -> string list) ->
  Cascade.Css.Stylesheet.statement list ->
  string list
(** [shapes_with_rule_runs ~boundary_shape ss] maps each statement of [ss] with
    [boundary_shape] and collapses every contiguous run of [Rule]s into a single
    ["rules"] token, so a boundary-shape invariant tracks the at-rule skeleton
    without forcing the optimizer to keep every individual rule. Each fuzz entry
    point supplies its own [boundary_shape] (they differ in baseline-[@supports]
    handling). *)
