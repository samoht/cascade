(* Adding node [p] leaves the order of the nodes Kahn reaches before [p]'s last
   predecessor [a], less [p]'s descendants among them, as it was: none of them
   waits for [p], and taking some out of a run of choices leaves the others in
   their order. Those descendants wait for [p]. From [a] on, the old order is
   merged with the waiting nodes: the next node of the old order is the least
   free one of the old order, as it was when Kahn took it, unless a waiting node
   is its predecessor, in which case it waits too; otherwise the smaller of it
   and the least free waiting node comes next. Once nothing waits, every node
   placed so far is one the old order had placed by then, and the rest of the
   old order follows unchanged. *)

module Int_table = Common.Table.Make (Int)

type 'edge nodes = {
  order : Order_maintenance.t;
  origin : int array;
  slot : int array;
  succ : (int * 'edge) list array;
  death : int array;
  generation : int;
}

let live nodes k = nodes.death.(k) > nodes.generation

let successors nodes k =
  List.filter_map
    (fun (j, _) -> if Order_maintenance.mem nodes.order j then Some j else None)
    nodes.succ.(k)

(* A fresh visit set: [true] the first time it is asked about a node. *)
let fresh_visit () =
  let seen = Int_table.create 16 in
  fun k ->
    (not (Int_table.mem seen k))
    &&
    (Int_table.replace seen k ();
     true)

let rank_less nodes a b =
  match Int.compare nodes.origin.(a) nodes.origin.(b) with
  | 0 -> a < b
  | order -> order < 0

module Rank_set = Set.Make (struct
  type t = int * int

  let compare (o, a) (o', b) =
    match Int.compare o o' with 0 -> Int.compare a b | order -> order
end)

let earliest order = function
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun first k ->
             if Order_maintenance.compare order k first < 0 then k else first)
           first rest)

let latest order = function
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun last k ->
             if Order_maintenance.compare order k last > 0 then k else last)
           first rest)

(* Recompute [slot] from the node after [from] (the first node when [None])
   until past [until] and up to the first node it leaves unchanged. *)
let reslot nodes ~moved ~from ~until =
  let rec loop running passed = function
    | None -> ()
    | Some k ->
        let slot = Int.max running nodes.origin.(k) in
        let passed = passed || k = until in
        let next = Order_maintenance.next nodes.order k in
        if nodes.slot.(k) <> slot then begin
          nodes.slot.(k) <- slot;
          if live nodes k then moved k;
          loop slot passed next
        end
        else if not passed then loop slot passed next
  in
  match from with
  | None -> loop min_int false (Order_maintenance.first nodes.order)
  | Some k -> loop nodes.slot.(k) false (Order_maintenance.next nodes.order k)

let place nodes prev k =
  match prev with
  | Some prev -> Order_maintenance.insert_after nodes.order prev k
  | None -> Order_maintenance.insert_first nodes.order k

(* [p]'s descendants Kahn reached before [a]. *)
let waiting nodes p a =
  let visit = fresh_visit () in
  let rec ahead acc = function
    | [] -> acc
    | k :: rest ->
        if Order_maintenance.compare nodes.order k a < 0 && visit k then
          ahead (k :: acc) (List.rev_append (successors nodes k) rest)
        else ahead acc rest
  in
  ahead [] (successors nodes p)

(* Almost always nothing waits for [p] and [p] precedes none of the nodes that
   take their turn before it, which then keep their places: [p] goes before the
   first node that takes its turn after it. [None] when [p] precedes one of
   them; otherwise the node [p] comes right after. *)
let alone nodes p ~after next =
  let unmarked = fresh_visit () in
  List.iter (fun j -> ignore (unmarked j)) (successors nodes p);
  let rec find prev = function
    | Some y when rank_less nodes y p ->
        if unmarked y then find (Some y) (Order_maintenance.next nodes.order y)
        else None
    | _ -> Some prev
  in
  find after next

(* A waiting node is out of [order] until it is placed again, and still counts
   as a successor. *)
let merge_successors nodes deferred k =
  List.filter_map
    (fun (j, _) ->
      if Order_maintenance.mem nodes.order j || Int_table.mem deferred j then
        Some j
      else None)
    nodes.succ.(k)

(* The merge: [pending] counts each node's predecessors still waiting, and
   [free] holds the waiting nodes none of whose predecessors waits. *)
let merge nodes ~moved ~waiting p ~after next =
  let pending = Int_table.create 16 in
  let deferred = Int_table.create 16 in
  let count k = Option.value ~default:0 (Int_table.find_opt pending k) in
  let successors = merge_successors nodes deferred in
  let wait k =
    List.iter
      (fun j -> Int_table.replace pending j (count j + 1))
      (successors k)
  in
  let take_out k =
    Int_table.replace deferred k ();
    Order_maintenance.remove nodes.order k;
    if live nodes k then moved k
  in
  List.iter take_out waiting;
  List.iter wait (p :: waiting);
  let release free m =
    List.fold_left
      (fun free j ->
        let left = count j - 1 in
        Int_table.replace pending j left;
        if left = 0 && Int_table.mem deferred j then
          Rank_set.add (nodes.origin.(j), j) free
        else free)
      free (successors m)
  in
  let rec loop prev next free =
    match Rank_set.min_elt_opt free with
    | None -> prev
    | Some ((_, m) as least) -> (
        match next with
        | Some y when count y > 0 ->
            let after = Order_maintenance.next nodes.order y in
            take_out y;
            wait y;
            loop prev after free
        | Some y when rank_less nodes y m ->
            loop (Some y) (Order_maintenance.next nodes.order y) free
        | _ ->
            place nodes prev m;
            loop (Some m) next (release (Rank_set.remove least free) m))
  in
  loop after next (Rank_set.singleton (nodes.origin.(p), p))

let add nodes ~preds ~moved p =
  let order = nodes.order in
  let last = latest order preds in
  let waiting = match last with None -> [] | Some a -> waiting nodes p a in
  let from =
    match earliest order waiting with
    | None -> last
    | Some first -> Order_maintenance.prev order first
  in
  let next =
    match last with
    | Some a -> Order_maintenance.next order a
    | None -> Order_maintenance.first order
  in
  let until =
    match waiting with
    | [] -> (
        match alone nodes p ~after:last next with
        | Some prev ->
            place nodes prev p;
            Some p
        | None -> merge nodes ~moved ~waiting p ~after:last next)
    | _ :: _ -> merge nodes ~moved ~waiting p ~after:last next
  in
  Option.iter (fun until -> reslot nodes ~moved ~from ~until) until
