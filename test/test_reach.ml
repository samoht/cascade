(* The reachable sets Rule_graph closes, checked against a list model. *)

open Cascade

let set_of count nodes =
  let set = Reach.v count in
  List.iter (Reach.add set) nodes;
  set

let members count set = List.filter (Reach.mem set) (List.init count Fun.id)

let add_and_mem () =
  let set = set_of 70 [ 0; 7; 8; 63; 64; 69 ] in
  Alcotest.(check (list int))
    "the nodes added, across byte and word boundaries" [ 0; 7; 8; 63; 64; 69 ]
    (members 70 set)

let union_and_meets () =
  let count = 130 in
  let a = set_of count [ 1; 64; 129 ] and b = set_of count [ 2; 65 ] in
  Alcotest.(check bool) "disjoint sets do not meet" false (Reach.meets a b);
  Reach.union ~into:a b;
  Alcotest.(check (list int))
    "the union holds both" [ 1; 2; 64; 65; 129 ] (members count a);
  Alcotest.(check bool) "and meets what it took" true (Reach.meets a b)

let missing_nodes () =
  let count = 20 in
  let a = set_of count [ 3; 9; 17 ] and b = set_of count [ 9 ] in
  Alcotest.(check (option (list int)))
    "what the other lacks"
    (Some [ 3; 17 ])
    (Option.map (List.sort Int.compare) (Reach.missing a b));
  let wide = 1000 in
  let full = set_of wide (List.init wide Fun.id) in
  Alcotest.(check (option (list int)))
    "more nodes than words is left to a union" None
    (Reach.missing full (Reach.v wide))

let grow_keeps_sets () =
  let sets = Reach.table 10 in
  let set = Reach.row sets in
  Reach.add set 5;
  sets.(5) <- set;
  let grown = Reach.grow sets 500 in
  Alcotest.(check bool) "room for the new count" true (Array.length grown >= 500);
  Alcotest.(check bool)
    "an unset entry stays unset" false
    (Reach.is_set grown.(0));
  Alcotest.(check bool)
    "a built set keeps its node" true
    (Reach.mem grown.(5) 5);
  Alcotest.(check int)
    "at the new width"
    (Bytes.length (Reach.row grown))
    (Bytes.length grown.(5));
  Alcotest.(check bool)
    "a table with room is kept" true
    (Reach.grow grown 20 == grown)

let unequal_widths () =
  Alcotest.check_raises "two widths" (Invalid_argument "Reach: unequal widths")
    (fun () -> ignore (Reach.meets (Reach.v 8) (Reach.v 80)))

let suite =
  ( "reach",
    [
      Alcotest.test_case "add and mem" `Quick add_and_mem;
      Alcotest.test_case "union and meets" `Quick union_and_meets;
      Alcotest.test_case "missing" `Quick missing_nodes;
      Alcotest.test_case "grow keeps sets" `Quick grow_keeps_sets;
      Alcotest.test_case "unequal widths" `Quick unequal_widths;
    ] )
