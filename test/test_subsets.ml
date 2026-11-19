open Cascade

let ids n = List.init n Rule_graph.Node_id.of_int_exn
let ints subsets = List.map (List.map Rule_graph.Node_id.to_int) subsets
let sorted subsets = List.sort (List.compare Int.compare) (ints subsets)

let rec contiguous = function
  | a :: (b :: _ as rest) -> b = a + 1 && contiguous rest
  | [ _ ] | [] -> true

let normal = Subsets.v ~aggressive:false
let aggressive = Subsets.v ~aggressive:true

(* Under --aggressive a small bucket is tried whole, then less each member. *)
let small_bucket_is_tried_whole_and_less_one () =
  Alcotest.(check (list (list int)))
    "the whole bucket, then each member left out"
    [ [ 0; 1; 2 ]; [ 1; 2 ]; [ 0; 2 ]; [ 0; 1 ] ]
    (ints (Subsets.of_bucket aggressive (ids 3)))

let a_lone_member_is_no_subset () =
  List.iter
    (fun (label, budget) ->
      Alcotest.(check (list (list int)))
        (label ^ ": one member") []
        (ints (Subsets.of_bucket budget (ids 1)));
      Alcotest.(check (list (list int)))
        (label ^ ": no member") []
        (ints (Subsets.of_bucket budget [])))
    [ ("normal", normal); ("aggressive", aggressive) ]

(* A bucket offers every run of neighbours: ten members make the 45 runs of two
   to ten, each of consecutive members. *)
let a_bucket_tries_runs_of_neighbours () =
  let subsets = Subsets.of_bucket normal (ids 10) in
  Alcotest.(check int) "every run of two or more" 45 (List.length subsets);
  Alcotest.(check bool)
    "each is a run of neighbours" true
    (List.for_all contiguous (ints subsets))

(* A run reaches at most 24 members: thirty in a row would offer more than the
   256 subsets a bucket may take, so the bucket offers none. *)
let a_bucket_past_its_bound_offers_nothing () =
  Alcotest.(check (list (list int)))
    "thirty neighbours" []
    (ints (Subsets.of_bucket normal (ids 30)))

(* A run is of neighbours in the bucket, wherever they print: three members are
   the whole bucket and its two pairs of neighbours. *)
let a_run_reads_neighbours_alone () =
  Alcotest.(check (list (list int)))
    "every run of neighbours"
    [ [ 0; 1 ]; [ 0; 1; 2 ]; [ 1; 2 ] ]
    (sorted (Subsets.of_bucket normal (ids 3)))

let one_budget_is_shared_outside_aggressive () =
  Alcotest.(check (option int)) "normal" (Some 256) (Subsets.shared normal);
  Alcotest.(check (option int)) "aggressive" None (Subsets.shared aggressive)

(* Under --aggressive a bucket too large to try whole is tried as up to 32 runs
   of neighbours, widest first. *)
let a_large_bucket_is_tried_as_runs_under_aggressive () =
  let subsets = Subsets.of_bucket aggressive (ids 20) in
  Alcotest.(check int) "runs" 32 (List.length subsets);
  Alcotest.(check bool)
    "runs of neighbours" true
    (List.for_all contiguous (ints subsets));
  Alcotest.(check (list int))
    "the widest first" (List.init 20 Fun.id)
    (List.hd (ints subsets))

let suite =
  ( "subsets",
    [
      Alcotest.test_case "small bucket is tried whole and less one" `Quick
        small_bucket_is_tried_whole_and_less_one;
      Alcotest.test_case "a lone member is no subset" `Quick
        a_lone_member_is_no_subset;
      Alcotest.test_case "a bucket tries runs of neighbours" `Quick
        a_bucket_tries_runs_of_neighbours;
      Alcotest.test_case "a bucket past its bound offers nothing" `Quick
        a_bucket_past_its_bound_offers_nothing;
      Alcotest.test_case "a run reads neighbours alone" `Quick
        a_run_reads_neighbours_alone;
      Alcotest.test_case "one budget is shared outside aggressive" `Quick
        one_budget_is_shared_outside_aggressive;
      Alcotest.test_case "a large bucket is tried as runs under aggressive"
        `Quick a_large_bucket_is_tried_as_runs_under_aggressive;
    ] )
