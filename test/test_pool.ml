(** Pool module tests. *)

open Cascade
module P = Pool

let mk name : Stylesheet.rule =
  {
    selector = Selector.class_ name;
    declarations = [];
    nested = [];
    merge_key = None;
  }

let sel (r : Stylesheet.rule) = Selector.to_string ~minify:true r.selector
let names p = List.map sel (P.to_rules p)

let union_selectors (a : Stylesheet.rule) (b : Stylesheet.rule) :
    Stylesheet.rule =
  { a with selector = Selector.list [ a.selector; b.selector ] }

let rule_name i = Fmt.str "r%03d" i
let pair_selector i = Fmt.str ".%s,.%s" (rule_name i) (rule_name (i + 1))

let roundtrip () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  Alcotest.(check (list string)) "order" [ ".a"; ".b"; ".c" ] (names p);
  Alcotest.(check int) "length" 3 (P.length p)

let nodes_in_order () =
  let p = P.of_rules [ mk "a"; mk "b" ] in
  match P.nodes p with
  | [ na; nb ] ->
      Alcotest.(check string) "first" ".a" (sel (P.rule na));
      Alcotest.(check string) "second" ".b" (sel (P.rule nb));
      Alcotest.(check bool) "a before b" true (P.before na nb)
  | _ -> Alcotest.fail "expected 2 nodes"

let combine_unions_and_drops () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match P.nodes p with
  | [ na; nb; nc ] ->
      let s = P.combine p na nb union_selectors in
      (* surviving node carries the merged rule, [b] is gone, order kept *)
      Alcotest.(check string) "merged selector" ".a,.b" (sel (P.rule s));
      Alcotest.(check (list string)) "live rules" [ ".a,.b"; ".c" ] (names p);
      Alcotest.(check int) "length" 2 (P.length p);
      Alcotest.(check bool) "survivor before c" true (P.before s nc)
  | _ -> Alcotest.fail "expected 3 nodes"

let insert_after_places () =
  let p = P.of_rules [ mk "a"; mk "c" ] in
  match P.nodes p with
  | [ na; _ ] ->
      let nb = P.insert_after p na (mk "b") in
      Alcotest.(check (list string)) "inserted" [ ".a"; ".b"; ".c" ] (names p);
      Alcotest.(check string) "new node rule" ".b" (sel (P.rule nb))
  | _ -> Alcotest.fail "expected 2 nodes"

let remove_node () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match P.nodes p with
  | [ _; nb; _ ] ->
      P.remove p nb;
      Alcotest.(check (list string)) "removed middle" [ ".a"; ".c" ] (names p)
  | _ -> Alcotest.fail "expected 3 nodes"

let set_rule () =
  let p = P.of_rules [ mk "a" ] in
  match P.nodes p with
  | [ na ] ->
      P.set na (mk "z");
      Alcotest.(check (list string)) "in place" [ ".z" ] (names p)
  | _ -> Alcotest.fail "expected 1 node"

let combine_stale_handles_resolve_to_survivor () =
  let p = P.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match P.nodes p with
  | [ na; nb; nc ] ->
      let id = P.id na in
      let survivor = P.combine p na nb union_selectors in
      Alcotest.(check int) "survivor keeps identity" id (P.id survivor);
      Alcotest.(check bool) "survivor live" true (P.is_live survivor);
      Alcotest.(check bool) "merged-away node dead" false (P.is_live nb);
      Alcotest.(check string)
        "stale handle resolves merged rule" ".a,.b"
        (sel (P.rule nb));
      Alcotest.(check bool)
        "survivor still before next live rule" true (P.before survivor nc);
      Alcotest.(check (list string))
        "only live nodes emitted" [ ".a,.b"; ".c" ] (names p)
  | _ -> Alcotest.fail "expected 3 nodes"

let insert_before_places_and_links_neighbors () =
  let p = P.of_rules [ mk "b"; mk "c" ] in
  match P.nodes p with
  | [ nb; _ ] ->
      let na = P.insert_before p nb (mk "a") in
      Alcotest.(check (list string))
        "inserted before" [ ".a"; ".b"; ".c" ] (names p);
      Alcotest.(check bool) "new node before old head" true (P.before na nb);
      Alcotest.(check (option string))
        "old head prev" (Some ".a")
        (Option.map (fun n -> sel (P.rule n)) (P.prev nb));
      Alcotest.(check (option string))
        "new node next" (Some ".b")
        (Option.map (fun n -> sel (P.rule n)) (P.next na))
  | _ -> Alcotest.fail "expected 2 nodes"

let large_pairwise_combines_keep_order_and_stale_rules () =
  let count = 300 in
  let p = P.of_rules (List.init count (fun i -> mk (rule_name i))) in
  let nodes = Array.of_list (P.nodes p) in
  for i = 0 to (count / 2) - 1 do
    ignore (P.combine p nodes.(i * 2) nodes.((i * 2) + 1) union_selectors)
  done;
  let expected = List.init (count / 2) (fun i -> pair_selector (i * 2)) in
  Alcotest.(check int) "pool shrinks to merged pairs" (count / 2) (P.length p);
  Alcotest.(check (list string)) "merged pair order" expected (names p);
  for i = 0 to (count / 2) - 1 do
    let survivor = nodes.(i * 2) in
    let stale = nodes.((i * 2) + 1) in
    let expected_rule = pair_selector (i * 2) in
    Alcotest.(check bool) "survivor remains live" true (P.is_live survivor);
    Alcotest.(check bool) "stale pair node removed" false (P.is_live stale);
    Alcotest.(check string)
      "stale pair rule resolves" expected_rule
      (sel (P.rule stale))
  done

let verify_pool p expected =
  let nodes = P.nodes p in
  Alcotest.(check int)
    "length matches model" (List.length expected) (P.length p);
  Alcotest.(check (list string)) "rules match model" expected (names p);
  Alcotest.(check int)
    "node count matches model" (List.length expected) (List.length nodes);
  List.iteri
    (fun i node ->
      let prev = if i = 0 then None else Some (List.nth nodes (i - 1)) in
      let next =
        if i + 1 = List.length nodes then None
        else Some (List.nth nodes (i + 1))
      in
      Alcotest.(check bool)
        "prev link" true
        (match (P.prev node, prev) with
        | None, None -> true
        | Some a, Some b -> a == b
        | _ -> false);
      Alcotest.(check bool)
        "next link" true
        (match (P.next node, next) with
        | None, None -> true
        | Some a, Some b -> a == b
        | _ -> false);
      for j = i + 1 to List.length nodes - 1 do
        Alcotest.(check bool)
          "before order" true
          (P.before node (List.nth nodes j))
      done)
    nodes

let replace_at i value xs = List.mapi (fun j x -> if i = j then value else x) xs

let insert_at i value xs =
  let rec aux j = function
    | [] -> [ value ]
    | x :: rest as all -> if i = j then value :: all else x :: aux (j + 1) rest
  in
  aux 0 xs

let remove_at i xs =
  xs
  |> List.mapi (fun j x -> (j, x))
  |> List.filter_map (fun (j, x) -> if i = j then None else Some x)

let test_fuzz_mutation_stream_preserves_order_and_links () =
  for seed = 0 to 59 do
    let rng = Random.State.make [| 0x50; seed |] in
    let p =
      P.of_rules (List.init 12 (fun i -> Fmt.kstr mk "s%d_%03d" seed i))
    in
    let model = ref (List.init 12 (fun i -> Fmt.str ".s%d_%03d" seed i)) in
    let next_name = ref 0 in
    verify_pool p !model;
    for _step = 0 to 119 do
      let nodes = P.nodes p in
      let len = List.length nodes in
      let op = Random.State.int rng 5 in
      (if len > 0 then
         match op with
         | 0 ->
             let i = Random.State.int rng len in
             let node = List.nth nodes i in
             let name = Fmt.str "s%d_b_%03d" seed !next_name in
             incr next_name;
             ignore (P.insert_before p node (mk name));
             model := insert_at i ("." ^ name) !model
         | 1 ->
             let i = Random.State.int rng len in
             let node = List.nth nodes i in
             let name = Fmt.str "s%d_a_%03d" seed !next_name in
             incr next_name;
             ignore (P.insert_after p node (mk name));
             model := insert_at (i + 1) ("." ^ name) !model
         | 2 when len > 1 ->
             let i = Random.State.int rng len in
             P.remove p (List.nth nodes i);
             model := remove_at i !model
         | 3 ->
             let i = Random.State.int rng len in
             let name = Fmt.str "s%d_set_%03d" seed !next_name in
             incr next_name;
             P.set (List.nth nodes i) (mk name);
             model := replace_at i ("." ^ name) !model
         | _ when len > 1 ->
             let i = Random.State.int rng (len - 1) in
             let survivor =
               P.combine p (List.nth nodes i)
                 (List.nth nodes (i + 1))
                 union_selectors
             in
             let merged = sel (P.rule survivor) in
             model := remove_at (i + 1) (replace_at i merged !model)
         | _ -> ());
      verify_pool p !model
    done
  done

let suite =
  ( "pool",
    [
      Alcotest.test_case "roundtrip" `Quick roundtrip;
      Alcotest.test_case "nodes in order" `Quick nodes_in_order;
      Alcotest.test_case "combine unions and drops" `Quick
        combine_unions_and_drops;
      Alcotest.test_case "insert_after places" `Quick insert_after_places;
      Alcotest.test_case "remove node" `Quick remove_node;
      Alcotest.test_case "set rule" `Quick set_rule;
      Alcotest.test_case "combine stale handles resolve to survivor" `Quick
        combine_stale_handles_resolve_to_survivor;
      Alcotest.test_case "insert_before places and links neighbors" `Quick
        insert_before_places_and_links_neighbors;
      Alcotest.test_case "large pairwise combines keep order and stale rules"
        `Quick large_pairwise_combines_keep_order_and_stale_rules;
      Alcotest.test_case "fuzz mutation stream preserves order and links" `Quick
        test_fuzz_mutation_stream_preserves_order_and_links;
    ] )
