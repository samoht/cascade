open Cascade

let mk name : Stylesheet.rule =
  {
    selector = Selector.class_ name;
    declarations = [];
    nested = [];
    merge_key = None;
  }

let sel (r : Stylesheet.rule) = Selector.to_string ~minify:true r.selector
let node_sel node = sel (Pool.rule node)
let names pool = List.map sel (Pool.to_rules pool)
let rule_name i = Fmt.str "r%03d" i
let merged_name i = Fmt.str "m%03d" i

let index_of_node node =
  let s = node_sel node in
  int_of_string (String.sub s 2 3)

let index_of_original_node node =
  let s = node_sel node in
  if String.length s = 5 && s.[1] = 'r' then
    Some (int_of_string (String.sub s 2 3))
  else None

let test_applies_replacement_and_reports_saving () =
  let pool = Pool.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match Pool.nodes pool with
  | [ a; b; _ ] ->
      let score node =
        if node == a then
          Some
            (Loop.action
               ~replacement:[ mk "x"; mk "y" ]
               ~consumed:[ b ] ~saving:7)
        else None
      in
      let applied_savings = ref [] in
      let applied =
        Loop.run
          ~on_apply:(fun saving ->
            applied_savings := saving :: !applied_savings)
          (Loop.v pool score)
      in
      Alcotest.(check int) "one rewrite applied" 1 applied;
      Alcotest.(check (list int))
        "saving callback" [ 7 ]
        (List.rev !applied_savings);
      Alcotest.(check (list string))
        "replacement inserted before anchor" [ ".x"; ".y"; ".c" ] (names pool)
  | _ -> Alcotest.fail "expected 3 nodes"

let test_ignores_none_zero_and_negative_scores () =
  let pool = Pool.of_rules [ mk "a"; mk "b"; mk "c" ] in
  let score node =
    match node_sel node with
    | ".a" -> Some (Loop.action ~replacement:[ mk "x" ] ~consumed:[] ~saving:0)
    | ".b" ->
        Some (Loop.action ~replacement:[ mk "y" ] ~consumed:[] ~saving:(-1))
    | _ -> None
  in
  Alcotest.(check int) "nothing applied" 0 (Loop.run (Loop.v pool score));
  Alcotest.(check (list string))
    "pool unchanged" [ ".a"; ".b"; ".c" ] (names pool)

let test_best_first_rescores_stale_candidate () =
  let pool = Pool.of_rules [ mk "a"; mk "b"; mk "c" ] in
  let score node =
    match node_sel node with
    | ".a" -> (
        match Pool.next node with
        | Some next when String.equal (node_sel next) ".b" ->
            Some
              (Loop.action
                 ~replacement:[ mk "x" ]
                 ~consumed:[ next ] ~saving:10)
        | Some next when String.equal (node_sel next) ".y" ->
            Some
              (Loop.action ~replacement:[ mk "z" ] ~consumed:[ next ] ~saving:5)
        | _ -> None)
    | ".b" -> (
        match Pool.next node with
        | Some next when String.equal (node_sel next) ".c" ->
            Some
              (Loop.action
                 ~replacement:[ mk "y" ]
                 ~consumed:[ next ] ~saving:20)
        | _ -> None)
    | _ -> None
  in
  let applied_savings = ref [] in
  let applied =
    Loop.run
      ~on_apply:(fun saving -> applied_savings := saving :: !applied_savings)
      (Loop.v pool score)
  in
  Alcotest.(check int) "two rewrites applied" 2 applied;
  Alcotest.(check (list int))
    "best candidate applied first" [ 20; 5 ]
    (List.rev !applied_savings);
  Alcotest.(check (list string))
    "stale anchor was rescored" [ ".z" ] (names pool)

let test_rescores_non_adjacent_consumed_nodes_away () =
  let pool = Pool.of_rules [ mk "a"; mk "b"; mk "c" ] in
  match Pool.nodes pool with
  | [ a; _; c ] ->
      let calls = ref 0 in
      let score node =
        incr calls;
        if node == a && !calls = 1 then
          Some (Loop.action ~replacement:[ mk "x" ] ~consumed:[ c ] ~saving:5)
        else None
      in
      Alcotest.(check int)
        "non-adjacent action is rescored away" 0
        (Loop.run (Loop.v pool score));
      Alcotest.(check (list string))
        "pool unchanged" [ ".a"; ".b"; ".c" ] (names pool)
  | _ -> Alcotest.fail "expected 3 nodes"

let test_large_non_overlapping_rewrite_queue () =
  let count = 200 in
  let pool = Pool.of_rules (List.init count (fun i -> mk (rule_name i))) in
  let score node =
    let i = index_of_node node in
    if i mod 2 = 0 then
      match Pool.next node with
      | Some next when index_of_node next = i + 1 ->
          Some
            (Loop.action
               ~replacement:[ mk (merged_name i) ]
               ~consumed:[ next ] ~saving:(count - i))
      | _ -> None
    else None
  in
  let applied = Loop.run (Loop.v pool score) in
  let expected = List.init (count / 2) (fun i -> "." ^ merged_name (i * 2)) in
  Alcotest.(check int) "one rewrite per pair" (count / 2) applied;
  Alcotest.(check int)
    "pool shrinks to one rule per pair" (count / 2) (Pool.length pool);
  Alcotest.(check (list string))
    "rewritten order is stable" expected (names pool)

type oracle_slot = Original of int | Replaced of int | Removed

let greedy_overlapping_oracle savings =
  let count = Array.length savings + 1 in
  let slots = Array.init count (fun i -> Original i) in
  let candidates =
    List.init (count - 1) Fun.id
    |> List.sort (fun a b -> compare savings.(b) savings.(a))
  in
  let applied = ref 0 in
  List.iter
    (fun i ->
      match (slots.(i), slots.(i + 1)) with
      | Original a, Original b when a = i && b = i + 1 ->
          incr applied;
          slots.(i) <- Replaced i;
          slots.(i + 1) <- Removed
      | _ -> ())
    candidates;
  let final =
    Array.to_list slots
    |> List.filter_map (function
      | Original i -> Some ("." ^ rule_name i)
      | Replaced i -> Some ("." ^ merged_name i)
      | Removed -> None)
  in
  (!applied, final)

let test_fuzz_overlapping_rewrite_queue_matches_greedy_oracle () =
  for seed = 0 to 119 do
    let rng = Random.State.make [| 0x100; seed |] in
    let count = 8 + Random.State.int rng 48 in
    let savings =
      Array.init (count - 1) (fun i ->
          ((1 + Random.State.int rng 1_000) * count) + (count - i))
    in
    let pool = Pool.of_rules (List.init count (fun i -> mk (rule_name i))) in
    let score node =
      match index_of_original_node node with
      | Some i when i + 1 < count -> (
          match Pool.next node with
          | Some next -> (
              match index_of_original_node next with
              | Some j when j = i + 1 ->
                  Some
                    (Loop.action
                       ~replacement:[ mk (merged_name i) ]
                       ~consumed:[ next ] ~saving:savings.(i))
              | _ -> None)
          | None -> None)
      | _ -> None
    in
    let expected_applied, expected_names = greedy_overlapping_oracle savings in
    let applied = Loop.run (Loop.v pool score) in
    Alcotest.(check int) "applied rewrite count" expected_applied applied;
    Alcotest.(check (list string))
      "final rule order" expected_names (names pool)
  done

(* Scalability. The loop re-scores only the anchors a merge actually touches and
   keeps its frontier in a priority-search queue, so draining N adjacent merges
   is O(N log N) work. A batch fixpoint (re-scan every rule each pass until
   nothing changes) would instead do O(passes x rules); on a chain that merges
   pairwise it degrades towards quadratic. We measure allocation (a
   deterministic proxy for work, unlike wall-clock) at growing N and assert each
   doubling stays well under the 4x a quadratic would show. *)
let drain_pairwise count =
  let pool = Pool.of_rules (List.init count (fun _ -> mk "r")) in
  (* Track the original nodes by stable id (selector-name parsing caps at three
     digits); a merge replaces its anchor with a non-original ["m"] node, so
     only original-original adjacencies stay mergeable -- a maximal matching of
     [count / 2] merges down the chain. *)
  let originals = Hashtbl.create count in
  List.iter
    (fun n -> Hashtbl.replace originals (Pool.id n) ())
    (Pool.nodes pool);
  let is_original node = Hashtbl.mem originals (Pool.id node) in
  let score node =
    if is_original node then
      match Pool.next node with
      | Some next when is_original next ->
          Some
            (Loop.action ~replacement:[ mk "m" ] ~consumed:[ next ] ~saving:1)
      | _ -> None
    else None
  in
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let t0 = Unix.gettimeofday () in
  let applied = Loop.run (Loop.v pool score) in
  let ms = (Unix.gettimeofday () -. t0) *. 1000. in
  (applied, Gc.minor_words () -. w0, ms)

let test_scales_subquadratically () =
  let sizes = [ 1000; 2000; 4000; 8000 ] in
  let rows = List.map (fun n -> (n, drain_pairwise n)) sizes in
  List.iter
    (fun (n, (applied, words, ms)) ->
      Alcotest.(check int)
        (Fmt.str "N=%d merges every adjacent pair" n)
        (n / 2) applied;
      Fmt.epr "  N=%-5d  words/rule=%-6.1f  %.2f ms@." n (words /. float n) ms)
    rows;
  (* Each doubling of N must keep allocation growth well below the 4x a
     quadratic shows; O(N log N) lands around 2.1-2.2x. *)
  let words = List.map (fun (_, (_, w, _)) -> w) rows in
  let rec doublings = function
    | a :: (b :: _ as rest) -> (b /. a) :: doublings rest
    | _ -> []
  in
  List.iter
    (fun r ->
      Alcotest.(check bool)
        (Fmt.str "per-doubling work ratio %.2f stays sub-quadratic (< 2.6)" r)
        true (r < 2.6))
    (doublings words)

let suite =
  ( "loop",
    [
      Alcotest.test_case "applies replacement and reports saving" `Quick
        test_applies_replacement_and_reports_saving;
      Alcotest.test_case "ignores none zero and negative scores" `Quick
        test_ignores_none_zero_and_negative_scores;
      Alcotest.test_case "best first rescores stale candidate" `Quick
        test_best_first_rescores_stale_candidate;
      Alcotest.test_case "rescores non-adjacent consumed nodes away" `Quick
        test_rescores_non_adjacent_consumed_nodes_away;
      Alcotest.test_case "large non-overlapping rewrite queue" `Quick
        test_large_non_overlapping_rewrite_queue;
      Alcotest.test_case "fuzz overlapping rewrite queue matches greedy oracle"
        `Quick test_fuzz_overlapping_rewrite_queue_matches_greedy_oracle;
      Alcotest.test_case "scales sub-quadratically with rule count" `Quick
        test_scales_subquadratically;
    ] )
