(** CSS tree difference analysis for structural comparison. *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}
(** Declaration diff information. *)

(** Individual rule changes. *)
type rule_diff =
  | Rule_added of { selector : string; declarations : Css.declaration list }
  | Rule_removed of { selector : string; declarations : Css.declaration list }
  | Rule_content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
      added_properties : string list;
      removed_properties : string list;
    }
  | Rule_selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }
  | Rule_reordered of {
      selector : string;
      expected_pos : int;
      actual_pos : int;
      swapped_with : string option;
          (** When only declaration order changed inside the rule, positions may
              be irrelevant; in that case [old_declarations]/[new_declarations]
              carry the before/after declarations to allow detailed
              pretty-printing. *)
      old_declarations : Css.declaration list option;
      new_declarations : Css.declaration list option;
    }

type container_info = {
  container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
  condition : string;
  rules : Css.statement list; (* Rules within this container *)
}
(** Container rule information. *)

(** Container changes. *)
type container_diff =
  | Container_added of container_info
  | Container_removed of container_info
  | Container_modified of {
      info : container_info; (* expected *)
      actual_rules : Css.statement list; (* actual *)
      rule_changes : rule_diff list;
      container_changes : container_diff list; (* Nested container changes *)
    }
  | Container_reordered of {
      info : container_info;
      expected_pos : int;
      actual_pos : int;
    }
  | Container_block_structure_changed of {
      container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
      condition : string;
      expected_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in expected *)
      actual_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in actual *)
    }

type t = { rules : rule_diff list; containers : container_diff list }
(** Structured CSS differences. *)

val is_empty : t -> bool
(** [is_empty d] returns [true] if [d] contains no differences. *)

val diff : expected:Css.t -> actual:Css.t -> t
(** [diff ~expected ~actual] computes structural differences between two CSS
    ASTs. *)

val pp : ?expected:string -> ?actual:string -> Format.formatter -> t -> unit
(** [pp ?expected ?actual fmt t] pretty-prints a tree diff with optional labels.
    Default labels are "Expected" and "Actual". *)

val pp_rule_diff_simple : Format.formatter -> rule_diff -> unit
(** [pp_rule_diff_simple fmt rule] pretty-prints a rule diff in a simple format
    suitable for tests. *)

(** {1 Query functions} *)

val single_rule_diff : t -> rule_diff option
(** [single_rule_diff diff] returns [Some rule] if [diff] contains exactly one
    rule change, [None] otherwise. *)

val count_containers_by_type :
  [ `Container | `Layer | `Media | `Property | `Supports ] -> t -> int
(** [count_containers_by_type container_type diff] counts containers of the
    given type in [diff]. *)

val has_container_added_of_type :
  [ `Container | `Layer | `Media | `Property | `Supports ] -> t -> bool
(** [has_container_added_of_type container_type diff] returns [true] if [diff]
    contains added containers of the given type. *)

val has_container_removed_of_type :
  [ `Container | `Layer | `Media | `Property | `Supports ] -> t -> bool
(** [has_container_removed_of_type container_type diff] returns [true] if [diff]
    contains removed containers of the given type. *)
