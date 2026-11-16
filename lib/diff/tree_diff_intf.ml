(* Types shared between the tree diff's computation, its report and the public
   [Tree_diff] interface. *)

open Cascade

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

type rule_diff =
  | Added of { selector : string; declarations : Css.declaration list }
  | Removed of { selector : string; declarations : Css.declaration list }
  | Content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
      added_properties : (string * string) list;
      removed_properties : (string * string) list;
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
      swapped_with : string option; (* Selector that moved to old position *)
      (* When only declarations order changed within the rule, we carry the
         before/after declarations to pretty-print a property reorder
         summary. *)
      old_declarations : Css.declaration list option;
      new_declarations : Css.declaration list option;
    }
  | Rearranged of { selector : string; declarations : Css.declaration list }
    (* Every declaration the selector carries survives on both sides, spread
       differently over the rules that write it. An element that also matches an
       overlapping selector can resolve differently. *)
  | Regrouped of {
      from_selectors : string list; (* rule selectors in expected *)
      to_selectors : string list; (* rule selectors in actual *)
    }
(* A comma group merged or split across rules with identical declarations: the
   same selectors survive, only the grouping differs. *)

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
  rules : Css.statement list;
}

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

type t = {
  rules : rule_diff list;
  containers : container_diff list;
  layer_order : layer_order_diff option;
}
