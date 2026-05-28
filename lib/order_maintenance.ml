(* Doubly-linked nodes carrying integer tags drawn from [0, universe]. The tags
   are kept strictly increasing along the list, so [compare] is a tag
   comparison. Insertion picks the midpoint tag between neighbours; when two
   neighbours are adjacent there is no midpoint, so the whole list is relabelled
   with evenly-spaced tags and the insertion retried. With a 2^62 universe the
   gaps are large, so relabels are rare in practice. *)

(* Well within [max_int] (2^62 - 1 on 64-bit OCaml) so midpoints and the
   relabel's [i * step] never overflow; 2^60 still leaves ~2.9e15 of gap for a
   few hundred entries. *)
let universe = 1 lsl 60

type 'a node = {
  data : 'a;
  id : int;  (** stable identity, unlike [tag] which a relabel reassigns *)
  mutable tag : int;
  mutable prev : 'a node option;
  mutable next : 'a node option;
  mutable live : bool;
}

type 'a t = {
  mutable head : 'a node option;
  mutable tail : 'a node option;
  mutable count : int;
  mutable next_id : int;
}

let v () = { head = None; tail = None; count = 0; next_id = 0 }

let fresh_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id

let is_empty t = t.count = 0
let length t = t.count
let data n = n.data
let is_live n = n.live

let relabel t =
  let step = universe / (t.count + 1) in
  let rec loop i = function
    | None -> ()
    | Some n ->
        n.tag <- i * step;
        loop (i + 1) n.next
  in
  loop 1 t.head

let rec insert_after t n x =
  if not n.live then invalid_arg "Order_maintenance.insert_after: stale node";
  let succ = n.next in
  let lo = n.tag in
  let hi = match succ with Some s -> s.tag | None -> universe in
  if hi - lo <= 1 then (
    relabel t;
    insert_after t n x)
  else
    let node =
      {
        data = x;
        id = fresh_id t;
        tag = lo + ((hi - lo) / 2);
        prev = Some n;
        next = succ;
        live = true;
      }
    in
    n.next <- Some node;
    (match succ with
    | Some s -> s.prev <- Some node
    | None -> t.tail <- Some node);
    t.count <- t.count + 1;
    node

let rec insert_before t n x =
  if not n.live then invalid_arg "Order_maintenance.insert_before: stale node";
  match n.prev with
  | Some p -> insert_after t p x
  | None ->
      (* [n] is the head: pick a tag between 0 and [n]'s tag. *)
      let lo = 0 and hi = n.tag in
      if hi - lo <= 1 then (
        relabel t;
        insert_before t n x)
      else
        let node =
          {
            data = x;
            id = fresh_id t;
            tag = lo + ((hi - lo) / 2);
            prev = None;
            next = Some n;
            live = true;
          }
        in
        n.prev <- Some node;
        t.head <- Some node;
        t.count <- t.count + 1;
        node

let add_last t x =
  match t.tail with
  | Some tail -> insert_after t tail x
  | None ->
      let node =
        {
          data = x;
          id = fresh_id t;
          tag = universe / 2;
          prev = None;
          next = None;
          live = true;
        }
      in
      t.head <- Some node;
      t.tail <- Some node;
      t.count <- 1;
      node

let remove t n =
  if not n.live then invalid_arg "Order_maintenance.remove: stale node";
  (match n.prev with Some p -> p.next <- n.next | None -> t.head <- n.next);
  (match n.next with Some s -> s.prev <- n.prev | None -> t.tail <- n.prev);
  n.live <- false;
  n.prev <- None;
  n.next <- None;
  t.count <- t.count - 1

let compare a b = Int.compare a.tag b.tag

let to_list t =
  let rec loop acc = function
    | None -> List.rev acc
    | Some n -> loop (n.data :: acc) n.next
  in
  loop [] t.head

let nodes t =
  let rec loop acc = function
    | None -> List.rev acc
    | Some n -> loop (n :: acc) n.next
  in
  loop [] t.head

let next n = n.next
let prev n = n.prev
let id n = n.id
