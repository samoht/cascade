(* A pool node bundles an order-maintenance handle (cascade position) with a
   union-find element carrying the rule. Combining two nodes unions their
   elements (so a stale handle still resolves to the merged rule via [find]) and
   drops the merged-away node from the order. *)

(* Union-find with path compression and union by rank, carrying a value at each
   class root. The merge callback combines the two class values. *)
module UF = struct
  type 'a elem = 'a node ref

  and 'a node =
    | Root of { mutable value : 'a; mutable rank : int }
    | Link of 'a elem

  let v value = ref (Root { value; rank = 0 })

  let rec find e =
    match !e with
    | Root _ -> e
    | Link parent ->
        let root = find parent in
        e := Link root;
        root

  let get e = match !(find e) with Root r -> r.value | Link _ -> assert false

  let set e value =
    match !(find e) with Root r -> r.value <- value | Link _ -> assert false

  let merge f a b =
    let ra = find a and rb = find b in
    if ra == rb then ra
    else
      match (!ra, !rb) with
      | Root da, Root db ->
          let value = f da.value db.value in
          if da.rank < db.rank then (
            db.value <- value;
            ra := Link rb;
            rb)
          else (
            da.value <- value;
            if da.rank = db.rank then da.rank <- da.rank + 1;
            rb := Link ra;
            ra)
      | _ -> assert false
end

type node = Stylesheet.rule UF.elem Order_maintenance.node
type t = Stylesheet.rule UF.elem Order_maintenance.t

let of_rules rs =
  let t = Order_maintenance.v () in
  List.iter (fun r -> ignore (Order_maintenance.add_last t (UF.v r))) rs;
  t

let elem n = Order_maintenance.data n
let rule n = UF.get (UF.find (elem n))
let is_live n = Order_maintenance.is_live n
let id n = Order_maintenance.id n
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

let insert_after t n r = Order_maintenance.insert_after t n (UF.v r)
let insert_before t n r = Order_maintenance.insert_before t n (UF.v r)
let remove t n = Order_maintenance.remove t n
