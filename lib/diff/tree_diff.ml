(** CSS tree difference analysis for structural comparison. *)

open Cascade
include Tree_diff_intf

let is_empty d =
  d.rules = [] && d.containers = [] && Option.is_none d.layer_order

let reorder_is_significant = Tree_diff_rules.reorder_is_significant
let pp = Tree_diff_render.pp
let pp_rule_diff_simple = Tree_diff_render.pp_rule_diff_simple

(** Query functions *)
let single_rule_diff (diff : t) =
  match diff.rules with [ rule ] -> Some rule | _ -> None

let rec count_containers_in_list container_type containers =
  List.fold_left
    (fun count cont ->
      let this_count =
        match cont with
        | Added { container_type = ct; _ }
        | Removed { container_type = ct; _ }
        | Reordered { info = { container_type = ct; _ }; _ }
        | Block_structure_changed { container_type = ct; _ } ->
            if ct = container_type then 1 else 0
        | Modified { info = { container_type = ct; _ }; container_changes; _ }
          ->
            let nested_count =
              count_containers_in_list container_type container_changes
            in
            (if ct = container_type then 1 else 0) + nested_count
      in
      count + this_count)
    0 containers

let count_containers_by_type container_type (diff : t) =
  count_containers_in_list container_type diff.containers

(* [Modified] holds the containers that came and went inside a container that
   survived, so an existence query reads the same tree
   [count_containers_in_list] counts: a flat [List.exists] would answer no about
   a container the count reports, which is an answer about the shape of the diff
   rather than about the stylesheets. *)
let rec exists_container here containers =
  List.exists
    (fun cont ->
      here cont
      ||
      match cont with
      | Modified { container_changes; _ } ->
          exists_container here container_changes
      | Added _ | Removed _ | Reordered _ | Block_structure_changed _ -> false)
    containers

let has_container_added_of_type container_type (diff : t) =
  exists_container
    (function
      | Added { container_type = ct; _ } -> ct = container_type
      | Removed _ | Modified _ | Reordered _ | Block_structure_changed _ ->
          false)
    diff.containers

let has_container_removed_of_type container_type (diff : t) =
  exists_container
    (function
      | Removed { container_type = ct; _ } -> ct = container_type
      | Added _ | Modified _ | Reordered _ | Block_structure_changed _ -> false)
    diff.containers

open Tree_diff_rules
open Tree_diff_containers

let diff ~(expected : Css.t) ~(actual : Css.t) : t =
  let all1 = Css.statements expected in
  let all2 = Css.statements actual in
  (* Imports are diffed separately ([process_imports]); excluding them here
     keeps [rule_diffs] from matching every import on the universal key. *)
  let rules1 = List.filter (fun s -> Css.as_import s = None) all1 in
  let rules2 = List.filter (fun s -> Css.as_import s = None) all2 in
  (* Same assembly as inside a container, so a difference reports the same way
     at either depth. *)
  let rule_changes =
    to_rule_changes rules1 rules2 @ process_imports all1 all2
  in

  (* Delegate all container and nested-container diffs to the generic walker *)
  let containers = nested_differences all1 all2 in
  { rules = rule_changes; containers; layer_order = layer_order_diff all1 all2 }
