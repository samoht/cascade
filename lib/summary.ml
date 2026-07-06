module Props = Set.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

module Map = Map.Make (struct
  type t = Declaration.prop_key

  let compare = Stdlib.compare
end)

type bloom = int

type t = {
  rule : Stylesheet.rule;
  size : int;
  selector_size : int;
  decl_sizes : int list;
  decl_pp_size : int;
  decl_count : int;
  prop_set : Props.t;
  decl_map : Declaration.t Map.t;
  decl_size_map : int Map.t;
  decl_bloom : bloom;
  selector_summary : Selector_summary.t Lazy.t;
}

let prop d = Declaration.property_key d

(* A 63-bit OCaml int has bit positions 0..62; [1 lsl 63] is 0. Reduce each tap
   into 0..62 so it never selects the out-of-range bit 63. *)
let bloom_tap h =
  let positive = h land max_int in
  1 lsl (positive mod 63)

let bloom_mask (h : int) = bloom_tap h lor bloom_tap (h lsr 8)
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
let selector_summary t = t.selector_summary
let same_bloom a b = a.decl_bloom = b.decl_bloom
let bloom t = t.decl_bloom
let may_share_bloom t bloom = t.decl_bloom land bloom <> 0
let may_share_decl_hash a b = a.decl_bloom land b.decl_bloom <> 0

let declares_all t props =
  List.for_all (fun prop -> Props.mem prop t.prop_set) props

let decl_for_prop t prop = Map.find_opt prop t.decl_map
let decl_size_for_prop t prop = Map.find_opt prop t.decl_size_map

let contains ~same t decl =
  bloom_might_contain t.decl_bloom (Declaration.hash decl)
  && List.exists
       (fun candidate -> same decl candidate)
       t.rule.Stylesheet_intf.declarations
