open Cascade

let ids n = List.init n Rule_graph.Node_id.of_int_exn
let ints subsets = List.map (List.map Rule_graph.Node_id.to_int) subsets
let at_index id = Rule_graph.Node_id.to_int id
let sorted subsets = List.sort (List.compare Int.compare) (ints subsets)

let rec contiguous = function
  | a :: (b :: _ as rest) -> b = a + 1 && contiguous rest
  | [ _ ] | [] -> true

let small_bucket_is_tried_whole_and_less_one () =
  let budget = Subsets.exact ~aggressive:false 50 in
  Alcotest.(check (list (list int)))
    "the whole bucket, then each member left out"
    [ [ 0; 1; 2 ]; [ 1; 2 ]; [ 0; 2 ]; [ 0; 1 ] ]
    (ints (Subsets.of_bucket budget ~origin:at_index (ids 3)))

let a_lone_member_is_no_subset () =
  let small = Subsets.exact ~aggressive:false 50 in
  let large = Subsets.exact ~aggressive:false 500 in
  List.iter
    (fun (label, budget) ->
      Alcotest.(check (list (list int)))
        (label ^ ": one member") []
        (ints (Subsets.of_bucket budget ~origin:at_index (ids 1)));
      Alcotest.(check (list (list int)))
        (label ^ ": no member") []
        (ints (Subsets.of_bucket budget ~origin:at_index [])))
    [ ("small run", small); ("large run", large) ]

(* Past 128 rules a bucket offers every run of neighbours: ten members make the
   45 runs of two to ten, each of consecutive members. *)
let a_large_run_tries_runs_of_neighbours () =
  let budget = Subsets.exact ~aggressive:false 500 in
  let subsets = Subsets.of_bucket budget ~origin:at_index (ids 10) in
  Alcotest.(check int) "every run of two or more" 45 (List.length subsets);
  Alcotest.(check bool)
    "each is a run of neighbours" true
    (List.for_all contiguous (ints subsets))

(* A run reaches at most 24 members: thirty in a row would offer more than the
   256 subsets a bucket may take, so the bucket offers none. *)
let a_bucket_past_its_bound_offers_nothing () =
  let budget = Subsets.default ~aggressive:false 500 in
  Alcotest.(check (list (list int)))
    "thirty neighbours" []
    (ints (Subsets.of_bucket budget ~origin:at_index (ids 30)))

(* A run spans at most 128 source positions, so members far apart in the source
   never share one. *)
let a_run_spans_at_most_128_positions () =
  let budget = Subsets.exact ~aggressive:false 500 in
  let origin id = 100 * Rule_graph.Node_id.to_int id in
  Alcotest.(check (list (list int)))
    "members 100 positions apart pair only with their neighbour"
    [ [ 0; 1 ]; [ 1; 2 ] ]
    (sorted (Subsets.of_bucket budget ~origin (ids 3)))

let one_budget_is_shared_past_128_rules () =
  Alcotest.(check (option int))
    "small run" None
    (Subsets.shared (Subsets.exact ~aggressive:false 128));
  Alcotest.(check (option int))
    "large run" (Some 256)
    (Subsets.shared (Subsets.exact ~aggressive:false 129));
  Alcotest.(check (option int))
    "aggressive" None
    (Subsets.shared (Subsets.default ~aggressive:true 5000))

(* A bucket too large to try whole in a small run is tried as runs of
   neighbours, widest first: up to 32 for exact sharing, one for default
   factoring. *)
let a_large_bucket_in_a_small_run_is_tried_as_runs () =
  let exact = Subsets.exact ~aggressive:false 100 in
  let default = Subsets.default ~aggressive:false 100 in
  let exact_subsets = Subsets.of_bucket exact ~origin:at_index (ids 20) in
  Alcotest.(check int) "exact" 32 (List.length exact_subsets);
  Alcotest.(check bool)
    "exact runs are runs of neighbours" true
    (List.for_all contiguous (ints exact_subsets));
  Alcotest.(check (list (list int)))
    "default takes the whole bucket as one run"
    [ List.init 20 Fun.id ]
    (ints (Subsets.of_bucket default ~origin:at_index (ids 20)))

let suite =
  ( "subsets",
    [
      Alcotest.test_case "small bucket is tried whole and less one" `Quick
        small_bucket_is_tried_whole_and_less_one;
      Alcotest.test_case "a lone member is no subset" `Quick
        a_lone_member_is_no_subset;
      Alcotest.test_case "a large run tries runs of neighbours" `Quick
        a_large_run_tries_runs_of_neighbours;
      Alcotest.test_case "a bucket past its bound offers nothing" `Quick
        a_bucket_past_its_bound_offers_nothing;
      Alcotest.test_case "a run spans at most 128 positions" `Quick
        a_run_spans_at_most_128_positions;
      Alcotest.test_case "one budget is shared past 128 rules" `Quick
        one_budget_is_shared_past_128_rules;
      Alcotest.test_case "a large bucket in a small run is tried as runs" `Quick
        a_large_bucket_in_a_small_run_is_tried_as_runs;
    ] )
