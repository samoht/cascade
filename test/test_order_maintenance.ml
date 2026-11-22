(** Order_maintenance tests: the order answers like the list it stands for. *)

open Cascade
module O = Order_maintenance

let to_list t =
  let rec loop acc = function
    | None -> List.rev acc
    | Some x -> loop (x :: acc) (O.next t x)
  in
  loop [] (O.first t)

(* Walking the order gives [expected], and each element compares before the
   next, which the comparison, an order on tags, carries to every pair. *)
let check_order msg t expected =
  Alcotest.(check (list int)) (msg ^ ": walk") expected (to_list t);
  let rec pairs = function
    | x :: (y :: _ as rest) ->
        if not (O.compare t x y < 0 && O.compare t y x > 0) then
          Alcotest.failf "%s: %d should come before %d" msg x y;
        pairs rest
    | _ -> ()
  in
  pairs expected

let initial_order () =
  let t = O.v 5 in
  check_order "v 5" t [ 0; 1; 2; 3; 4 ];
  Alcotest.(check bool) "0 has a place" true (O.mem t 0);
  Alcotest.(check bool) "5 has none" false (O.mem t 5);
  Alcotest.(check int) "a node equals itself" 0 (O.compare t 3 3)

let insertions_place () =
  let t = O.v 3 in
  O.insert_after t 0 3;
  O.insert_before t 0 4;
  O.insert_after t 2 5;
  O.insert_first t 6;
  check_order "inserted" t [ 6; 4; 0; 3; 1; 2; 5 ]

let removal_and_reinsertion () =
  let t = O.v 4 in
  O.remove t 1;
  Alcotest.(check bool) "removed" false (O.mem t 1);
  check_order "without 1" t [ 0; 2; 3 ];
  O.insert_after t 3 1;
  check_order "1 moved last" t [ 0; 2; 3; 1 ];
  O.remove t 0;
  O.insert_after t 2 0;
  check_order "0 moved after 2" t [ 2; 0; 3; 1 ]

(* Inserting again and again at one point halves the gap each time, so the tags
   run out and the order relabels the dense range around it. The answers stay
   those of the list, and the relabelling stays local: retagging the whole order
   at every collision would retag about [n^2 / 100] elements here (a collision
   every 50 or so insertions, half the order each time), against [O(n log n)]
   for ranges sized by density. *)
let dense_insertions_relabel () =
  let n = 20_000 in
  let t = O.v 2 in
  for x = 2 to n + 1 do
    O.insert_after t 0 x
  done;
  let middle = List.init n (fun i -> n + 1 - i) in
  check_order "after 0, newest first" t ((0 :: middle) @ [ 1 ]);
  Alcotest.(check bool) "relabelling happened" true (O.relabels t > 0);
  Alcotest.(check bool)
    (Fmt.str "relabelling is local (%d retags)" (O.relabels t))
    true
    (O.relabels t < n * n / 400)

(* The same, alternating ends of a growing run, against a reference list. *)
let interleaved_insertions () =
  let t = O.v 1 in
  let reference = ref [ 0 ] in
  let last = ref 0 in
  for x = 1 to 3000 do
    if x mod 2 = 0 then begin
      O.insert_after t !last x;
      let rec after = function
        | [] -> [ x ]
        | y :: rest -> if y = !last then y :: x :: rest else y :: after rest
      in
      reference := after !reference
    end
    else begin
      O.insert_before t !last x;
      let rec before = function
        | [] -> [ x ]
        | y :: rest -> if y = !last then x :: y :: rest else y :: before rest
      in
      reference := before !reference
    end;
    if x mod 7 = 0 then last := x
  done;
  check_order "interleaved" t !reference

let exists_between_walks_the_gap () =
  let t = O.v 6 in
  let seen = ref [] in
  let stop_at = 4 in
  let found =
    O.exists_between t 1 5 (fun x ->
        seen := x :: !seen;
        x = stop_at)
  in
  Alcotest.(check bool) "found" true found;
  Alcotest.(check (list int)) "asked in order" [ 2; 3; 4 ] (List.rev !seen);
  Alcotest.(check bool)
    "nothing strictly between neighbours" false
    (O.exists_between t 2 3 (fun _ -> true))

let suite =
  ( "order_maintenance",
    [
      Alcotest.test_case "v n is 0 to n - 1" `Quick initial_order;
      Alcotest.test_case "insertions take their place" `Quick insertions_place;
      Alcotest.test_case "a removed element can come back elsewhere" `Quick
        removal_and_reinsertion;
      Alcotest.test_case "dense insertions relabel locally" `Quick
        dense_insertions_relabel;
      Alcotest.test_case "interleaved insertions keep the list" `Quick
        interleaved_insertions;
      Alcotest.test_case "exists_between walks the gap" `Quick
        exists_between_walks_the_gap;
    ] )
