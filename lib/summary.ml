module Props = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Map = Map.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Prop_key_tbl = Hashtbl.Make (struct
  type t = Declaration.prop_key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type ids = int array
type bloom = int

type t = {
  rule : Stylesheet.rule;
  size : int;
  selector_size : int;
  decl_sizes : int list;
  decl_pp_size : int;
  decl_count : int;
  prop_set : Props.t;
  prop_ids : ids;
  decl_prop_ids : ids;
  decl_map : Declaration.t Map.t;
  decl_size_map : int Map.t;
  decl_bloom : bloom;
  selector_summary : Selector_summary.t Lazy.t;
}

let prop_ids : int Prop_key_tbl.t = Prop_key_tbl.create 512
let next_prop_id = ref 0

let reset () =
  Prop_key_tbl.reset prop_ids;
  next_prop_id := 0

let prop d = Declaration.property_key d

let prop_id prop =
  match Prop_key_tbl.find_opt prop_ids prop with
  | Some id -> id
  | None ->
      let id = !next_prop_id in
      incr next_prop_id;
      Prop_key_tbl.add prop_ids prop id;
      id

let ids_of_set props =
  let ids = Props.to_seq props |> Array.of_seq |> Array.map prop_id in
  Array.sort compare ids;
  ids

let ids_of_decls decls =
  decls |> List.map (fun d -> prop_id (prop d)) |> Array.of_list

let ids_empty ids = Array.length ids = 0

let ids_mem id ids =
  let rec search lo hi =
    if lo > hi then false
    else
      let mid = (lo + hi) lsr 1 in
      let value = ids.(mid) in
      if id = value then true
      else if id < value then search lo (mid - 1)
      else search (mid + 1) hi
  in
  search 0 (Array.length ids - 1)

let ids_disjoint a b =
  let rec loop i j =
    if i >= Array.length a || j >= Array.length b then true
    else
      let x = a.(i) and y = b.(j) in
      if x = y then false else if x < y then loop (i + 1) j else loop i (j + 1)
  in
  loop 0 0

let ids_subset a b =
  let rec loop i j =
    if i >= Array.length a then true
    else if j >= Array.length b then false
    else
      let x = a.(i) and y = b.(j) in
      if x = y then loop (i + 1) (j + 1)
      else if x < y then false
      else loop i (j + 1)
  in
  loop 0 0

let ids_inter a b =
  let acc = ref [] in
  let rec loop i j =
    if i < Array.length a && j < Array.length b then
      let x = a.(i) and y = b.(j) in
      if x = y then (
        acc := x :: !acc;
        loop (i + 1) (j + 1))
      else if x < y then loop (i + 1) j
      else loop i (j + 1)
  in
  loop 0 0;
  Array.of_list (List.rev !acc)

let bloom_mask (h : int) = (1 lsl (h land 63)) lor (1 lsl ((h lsr 8) land 63))
let bloom_add b h = b lor bloom_mask h

let bloom_might_contain b h =
  let m = bloom_mask h in
  b land m = m

let bloom_of_decls decls =
  List.fold_left (fun b d -> bloom_add b (Declaration.hash d)) 0 decls

let decl_maps decls sizes =
  let rec loop set decl_map size_map decls sizes =
    match (decls, sizes) with
    | [], [] -> (set, decl_map, size_map)
    | decl :: decls, size :: sizes ->
        let prop = prop decl in
        let decl_map, size_map =
          if Map.mem prop decl_map then (decl_map, size_map)
          else (Map.add prop decl decl_map, Map.add prop size size_map)
        in
        loop (Props.add prop set) decl_map size_map decls sizes
    | _ -> assert false
  in
  loop Props.empty Map.empty Map.empty decls sizes

let v ~rule_size ~decl_size ~selector_size (rule : Stylesheet.rule) =
  let decls = rule.Stylesheet_intf.declarations in
  let decl_sizes = List.map decl_size decls in
  let prop_set, decl_map, decl_size_map = decl_maps decls decl_sizes in
  {
    rule;
    size = rule_size rule;
    selector_size = selector_size rule.Stylesheet_intf.selector;
    decl_sizes;
    decl_pp_size = List.fold_left ( + ) 0 decl_sizes;
    decl_count = List.length decls;
    prop_set;
    prop_ids = ids_of_set prop_set;
    decl_prop_ids = ids_of_decls decls;
    decl_map;
    decl_size_map;
    decl_bloom = bloom_of_decls decls;
    selector_summary =
      lazy (Selector_summary.of_selector rule.Stylesheet_intf.selector);
  }

let rule t = t.rule
let size t = t.size
let selector_size t = t.selector_size
let decl_sizes t = t.decl_sizes
let decl_pp_size t = t.decl_pp_size
let decl_count t = t.decl_count
let prop_set t = t.prop_set
let prop_ids t = t.prop_ids
let decl_prop_ids t = t.decl_prop_ids
let selector_summary t = t.selector_summary
let same_bloom a b = a.decl_bloom = b.decl_bloom
let bloom t = t.decl_bloom
let may_share_bloom t bloom = t.decl_bloom land bloom <> 0
let may_share_decl_hash a b = a.decl_bloom land b.decl_bloom <> 0

let declares_all t props =
  List.for_all (fun prop -> Props.mem prop t.prop_set) props

let declares_ids t props = ids_subset props t.prop_ids
let decl_for_prop t prop = Map.find_opt prop t.decl_map
let decl_size_for_prop t prop = Map.find_opt prop t.decl_size_map

let contains ~same t decl =
  bloom_might_contain t.decl_bloom (Declaration.hash decl)
  && List.exists
       (fun candidate -> same decl candidate)
       t.rule.Stylesheet_intf.declarations
