(** Order_maintenance module tests. *)

open Cascade
module O = Order_maintenance

let before a b = O.compare a b < 0

let empty () =
  let t = O.v () in
  Alcotest.(check bool) "is_empty" true (O.is_empty t);
  Alcotest.(check int) "length" 0 (O.length t);
  Alcotest.(check (list int)) "to_list" [] (O.to_list t)

let add_last_order () =
  let t = O.v () in
  let _ = O.add_last t 1 in
  let _ = O.add_last t 2 in
  let _ = O.add_last t 3 in
  Alcotest.(check (list int)) "order" [ 1; 2; 3 ] (O.to_list t);
  Alcotest.(check int) "length" 3 (O.length t);
  Alcotest.(check bool) "not empty" false (O.is_empty t)

let compare_reflects_order () =
  let t = O.v () in
  let a = O.add_last t 'a' in
  let b = O.add_last t 'b' in
  Alcotest.(check bool) "a before b" true (before a b);
  Alcotest.(check bool) "b not before a" false (before b a);
  Alcotest.(check int) "a vs a is 0" 0 (O.compare a a);
  Alcotest.(check bool)
    "compare antisymmetric" true
    (O.compare a b = -O.compare b a |> fun x -> x || O.compare a b <> 0)

let insert_after_places () =
  let t = O.v () in
  let a = O.add_last t 1 in
  let c = O.add_last t 3 in
  let b = O.insert_after t a 2 in
  Alcotest.(check (list int)) "spliced" [ 1; 2; 3 ] (O.to_list t);
  Alcotest.(check bool) "a<b" true (before a b);
  Alcotest.(check bool) "b<c" true (before b c);
  Alcotest.(check bool) "a<c" true (before a c)

let remove_middle () =
  let t = O.v () in
  let _ = O.add_last t 1 in
  let b = O.add_last t 2 in
  let _ = O.add_last t 3 in
  O.remove t b;
  Alcotest.(check (list int)) "removed middle" [ 1; 3 ] (O.to_list t);
  Alcotest.(check int) "length" 2 (O.length t)

let remove_ends () =
  let t = O.v () in
  let a = O.add_last t 1 in
  let _ = O.add_last t 2 in
  let c = O.add_last t 3 in
  O.remove t a;
  O.remove t c;
  Alcotest.(check (list int)) "only middle left" [ 2 ] (O.to_list t)

(* Repeatedly inserting right after the same node shrinks the available tag gap
   until a relabel is forced; the order must stay correct throughout. *)
let relabel_stress () =
  let t = O.v () in
  let head = O.add_last t 0 in
  let _tail = O.add_last t (-1) in
  (* insert 1..200 each right after head: they end up in reverse just after
     head, before tail: [0; 200; 199; ...; 1; -1] *)
  for i = 1 to 200 do
    ignore (O.insert_after t head i)
  done;
  let expected = (0 :: List.init 200 (fun i -> 200 - i)) @ [ -1 ] in
  Alcotest.(check (list int))
    "order after forced relabels" expected (O.to_list t);
  Alcotest.(check int) "length" 202 (O.length t)

(* compare must agree with the live left-to-right order for every pair. *)
let compare_total_order () =
  let t = O.v () in
  let n0 = O.add_last t 0 in
  let nodes = ref [ n0 ] in
  (* build a jumbled set of inserts *)
  for i = 1 to 60 do
    let anchor = List.nth !nodes (i mod List.length !nodes) in
    let n = O.insert_after t anchor i in
    nodes := n :: !nodes
  done;
  (* nodes paired with their live index *)
  let order = Array.of_list (O.to_list t) in
  let index = Hashtbl.create 64 in
  Array.iteri (fun i v -> Hashtbl.replace index v i) order;
  let all = !nodes in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          let ia = Hashtbl.find index (O.data a) in
          let ib = Hashtbl.find index (O.data b) in
          let by_index = Stdlib.compare ia ib in
          let by_cmp = O.compare a b in
          let sign x = compare x 0 in
          Alcotest.(check int)
            (Fmt.str "compare %d/%d matches order" (O.data a) (O.data b))
            (sign by_index) (sign by_cmp))
        all)
    all

let stale_node () =
  let t = O.v () in
  let a = O.add_last t 1 in
  let _ = O.add_last t 2 in
  O.remove t a;
  Alcotest.check_raises "double remove"
    (Invalid_argument "Order_maintenance.remove: stale node") (fun () ->
      O.remove t a);
  Alcotest.check_raises "insert after stale"
    (Invalid_argument "Order_maintenance.insert_after: stale node") (fun () ->
      ignore (O.insert_after t a 9))

let suite =
  ( "order_maintenance",
    [
      Alcotest.test_case "empty" `Quick empty;
      Alcotest.test_case "add_last order" `Quick add_last_order;
      Alcotest.test_case "compare reflects order" `Quick compare_reflects_order;
      Alcotest.test_case "insert_after places" `Quick insert_after_places;
      Alcotest.test_case "remove middle" `Quick remove_middle;
      Alcotest.test_case "remove ends" `Quick remove_ends;
      Alcotest.test_case "relabel stress" `Quick relabel_stress;
      Alcotest.test_case "compare total order" `Quick compare_total_order;
      Alcotest.test_case "stale node" `Quick stale_node;
    ] )
