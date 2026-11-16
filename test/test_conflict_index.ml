open Cascade

let key decl =
  match Shorthand.declaration_overlap_keys decl with
  | [ key ] -> key
  | _ -> Alcotest.fail "not one key"

let color = key (Declaration.v Properties.Color (Current : Values.color))

let width =
  key
    (Declaration.v Properties.Outline_width (Px 1.0 : Properties.border_width))

(* Values at positions 0 to 3, the first at specificity 1 under [color], then
   specificity 2 under [color], specificity 1 under [width] and specificity 1
   under [color]. Asks which of them after [after] a rule of specificity 1 and
   [keys] meets, recording each value the query tests. *)
let tested ?(found = fun _ -> false) ~after keys =
  let index = Conflict_index.positioned () in
  List.iteri
    (fun at (spec, key) ->
      Conflict_index.add_at index ~at ~specs:[ spec ] ~keys:[ key ] at)
    [ (1, color); (2, color); (1, width); (1, color) ];
  let seen = ref [] in
  let answer =
    Conflict_index.exists_after index ~after ~specs:[ 1 ] ~keys (fun at ->
        seen := at :: !seen;
        found at)
  in
  (answer, List.sort Int.compare !seen)

let check label expected got =
  Alcotest.(check (pair bool (list int))) label expected got

(* Only a value after the position that shares a specificity and a key is
   tested, and each once. *)
let a_query_reads_its_buckets_after_the_position () =
  check "one key" (false, [ 3 ]) (tested ~after:0 [ color ]);
  check "two keys" (false, [ 2; 3 ]) (tested ~after:1 [ color; width ]);
  check "nothing after the last" (false, []) (tested ~after:3 [ color; width ])

(* A broad footprint meets every key, so a query with one reads every value of
   its specificity, and a value under the broad key meets every query. *)
let a_broad_footprint_meets_every_key () =
  check "a broad query"
    (false, [ 2; 3 ])
    (tested ~after:0 [ Shorthand.broad_overlap_key ]);
  let index = Conflict_index.positioned () in
  Conflict_index.add_at index ~at:0 ~specs:[ 1 ]
    ~keys:[ Shorthand.broad_overlap_key ]
    ();
  Alcotest.(check bool)
    "a broad value" true
    (Conflict_index.exists_after index ~after:(-1) ~specs:[ 1 ] ~keys:[ width ]
       (fun () -> true))

let the_answer_is_the_test () =
  check "stops at the first found" (true, [ 3 ])
    (tested ~found:(fun _ -> true) ~after:0 [ color ])

let suite =
  ( "conflict_index",
    [
      Alcotest.test_case "a query reads its buckets after the position" `Quick
        a_query_reads_its_buckets_after_the_position;
      Alcotest.test_case "a broad footprint meets every key" `Quick
        a_broad_footprint_meets_every_key;
      Alcotest.test_case "the answer is the test" `Quick the_answer_is_the_test;
    ] )
