(** CSS tree difference analysis for structural comparison. *)

open Cascade

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}
(** Declaration diff information. *)

(** Individual rule changes. *)
type rule_diff =
  | Added of { selector : string; declarations : Css.declaration list }
  | Removed of { selector : string; declarations : Css.declaration list }
  | Content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
      added_properties : string list;
      removed_properties : string list;
    }
  | Selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }
  | Reordered of {
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
  | Rearranged of { selector : string; declarations : Css.declaration list }
      (** Every declaration the selector carries survives on both sides, spread
          differently over the rules that write it. An element that also matches
          an overlapping selector can resolve differently, since which rule
          carries a declaration decides where it sits relative to the other
          selector's rules. *)
  | Regrouped of { from_selectors : string list; to_selectors : string list }
      (** A comma group merged or split across rules with identical
          declarations: the same selectors survive, only the grouping differs.
          [from_selectors]/[to_selectors] are the rule selectors in expected and
          actual. *)

type container_info = {
  container_type :
    [ `Media
    | `Layer
    | `Supports
    | `Container
    | `Property
    | `Nesting
    | `At_rule ];
  condition : string;
  rules : Css.statement list; (* Rules within this container *)
}
(** Container rule information. [`At_rule] covers the at-rules that carry no
    selector and no condition of their own ([@page], [@font-face],
    [@counter-style], [@scope], [@starting-style], ...); their [condition] is
    the at-rule text up to the block. *)

(** Container changes. *)
type container_diff =
  | Added of container_info
  | Removed of container_info
  | Modified of {
      info : container_info; (* expected *)
      actual_rules : Css.statement list; (* actual *)
      rule_changes : rule_diff list;
      container_changes : container_diff list; (* Nested container changes *)
    }
  | Reordered of { info : container_info; expected_pos : int; actual_pos : int }
  | Block_structure_changed of {
      container_type :
        [ `Media
        | `Layer
        | `Supports
        | `Container
        | `Property
        | `Nesting
        | `At_rule ];
      condition : string;
      expected_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in expected *)
      actual_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in actual *)
    }

type layer_order_diff = {
  expected_order : string list;
  actual_order : string list;
  swapped : (string * string) list;
}
(** The cascade layer order the two sheets declare, when they declare it
    differently. Layers are named by the dotted paths of
    {!Cascade.Resolve.layer_order}, weakest first, restricted to the layers both
    sheets declare: one side declaring a layer the other does not is reported as
    the [@layer] block that came or went, not as an order change.

    [swapped] holds the pairs [(weaker, stronger)] that [expected_order]
    declares in that order and [actual_order] the other way round - the layers
    whose conflicts the two sheets resolve differently. It is never empty.

    The order compared is the sheet's own, so a layer declared inside a
    conditional group is not part of it, as in {!Cascade.Resolve.layer_order}.
*)

type t = {
  rules : rule_diff list;
  containers : container_diff list;
  layer_order : layer_order_diff option;
}
(** Structured CSS differences. *)

val is_empty : t -> bool
(** [is_empty d] returns [true] if [d] contains no differences. *)

val reorder_is_significant :
  Css.declaration list -> Css.declaration list -> bool
(** [reorder_is_significant d1 d2] is [true] when reordering the declarations
    changes the cascade, i.e. two overlapping declarations swap relative order.
    A reorder of disjoint declarations is no difference. *)

val diff : expected:Css.t -> actual:Css.t -> t
(** [diff ~expected ~actual] computes structural differences between two CSS
    ASTs. *)

val pp :
  ?expected:string ->
  ?actual:string ->
  ?color:bool ->
  ?depth:int ->
  Buffer.t ->
  t ->
  unit
(** [pp ?expected ?actual ?color ?depth buf t] pretty-prints a tree diff with
    optional labels. Default labels are "Expected" and "Actual". [color]
    (default [false]) wraps diff markers in ANSI escapes; the printer writes
    into a buffer, so the caller decides whether the destination supports
    colour.

    [depth] bounds how many tree levels are rendered, the top-level entries
    being level 1 (default: unbounded). A node whose children are cut off is
    followed by a ["... N more lines"] marker, so the report never reads as if
    the elided subtree were empty. *)

val pp_rule_diff_simple : Buffer.t -> rule_diff -> unit
(** [pp_rule_diff_simple buf rule] pretty-prints a rule diff in a simple format
    suitable for tests. *)

(** {1 Query functions} *)

val single_rule_diff : t -> rule_diff option
(** [single_rule_diff diff] returns [Some rule] if [diff] contains exactly one
    rule change, [None] otherwise. *)

val count_containers_by_type :
  [ `At_rule | `Container | `Layer | `Media | `Nesting | `Property | `Supports ] ->
  t ->
  int
(** [count_containers_by_type container_type diff] counts containers of the
    given type in [diff]. *)

val has_container_added_of_type :
  [ `At_rule | `Container | `Layer | `Media | `Nesting | `Property | `Supports ] ->
  t ->
  bool
(** [has_container_added_of_type container_type diff] returns [true] if [diff]
    contains added containers of the given type. *)

val has_container_removed_of_type :
  [ `At_rule | `Container | `Layer | `Media | `Nesting | `Property | `Supports ] ->
  t ->
  bool
(** [has_container_removed_of_type container_type diff] returns [true] if [diff]
    contains removed containers of the given type. *)
