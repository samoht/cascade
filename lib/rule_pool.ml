(* A pool node bundles an order-maintenance handle (cascade position) with a
   union-find element carrying the rule. Combining two nodes unions their
   elements (so a stale handle still resolves to the merged rule via [find]) and
   drops the merged-away node from the order. *)

module UF = UnionFind

type node = Stylesheet.rule UF.elem Order_maintenance.node
type t = Stylesheet.rule UF.elem Order_maintenance.t

let of_rules rs =
  let t = Order_maintenance.create () in
  List.iter (fun r -> ignore (Order_maintenance.add_last t (UF.make r))) rs;
  t

let elem n = Order_maintenance.data n
let rule n = UF.get (UF.find (elem n))
let set n r = UF.set (UF.find (elem n)) r
let length t = Order_maintenance.length t
let nodes t = Order_maintenance.nodes t
let before a b = Order_maintenance.compare a b < 0
let next n = Order_maintenance.next n
let prev n = Order_maintenance.prev n
let to_rules t = List.map rule (nodes t)

let combine t a b f =
  ignore (UF.merge f (elem a) (elem b));
  Order_maintenance.remove t b;
  a

let insert_after t n r = Order_maintenance.insert_after t n (UF.make r)
let remove t n = Order_maintenance.remove t n
