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

let rule css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule r ] -> r
      | _ -> Alcotest.failf "not one rule: %s" css)
  | Error _ -> Alcotest.failf "parse failed: %s" css

let facts css =
  let r = rule css in
  let decls =
    List.map
      (fun decl ->
        {
          Conflict_index.decl;
          footprint = Shorthand.declaration_overlap_keys decl;
        })
      r.declarations
  in
  let keys =
    List.sort_uniq Shorthand.overlap_key_compare
      (List.concat_map
         (fun (d : Conflict_index.decl_overlap) -> d.footprint)
         decls)
  in
  let branches = Edge.selectors r.selector in
  ( keys,
    decls,
    branches,
    List.map Selector_summary.of_selector branches,
    List.map Selector.specificity branches )

(* Inserts each rule as the node numbered by its position, and asks about one
   more rule numbered after them. *)
let candidates ?(relays = fun _ -> true) ~relay rules css =
  let index = Conflict_index.v (List.length rules + 1) in
  List.iteri
    (fun i css ->
      let _, decls, branches, summaries, specs = facts css in
      Conflict_index.insert index ~relay:(relays i) decls branches summaries
        specs i)
    rules;
  let keys, decls, branches, summaries, specs = facts css in
  Conflict_index.candidates index ~relay ~keys decls branches summaries specs
    (List.length rules)
  |> List.sort Int.compare

let check_nodes label expected got =
  Alcotest.(check (list int)) label expected got

(* Only a node that can meet the query on every dimension is a candidate: a
   specificity that cannot tie, another property and a subject element that
   cannot be the same element each rule a node out. *)
let candidates_meet_on_every_dimension () =
  check_nodes "same property, same specificity, same element" [ 0 ]
    (candidates ~relay:false
       [
         "div.a{color:red}";
         "span.b{color:red}";
         "div.c{margin:0}";
         "div#d{color:red}";
       ]
       "div.q{color:green}")

(* A simple writer of one declaration is answered through the two newest runs of
   writers of one declaration, which every older writer reaches. Asked by a
   writer of the newest run's declaration, the index answers with the run before
   alone. *)
let a_relay_reads_the_run_before_the_newest () =
  let rules =
    [ ".a{color:red}"; ".b{color:blue}"; ".c{color:green}"; ".d{color:blue}" ]
  in
  check_nodes "the two newest runs" [ 2; 3 ]
    (candidates ~relay:true rules ".q{color:red}");
  check_nodes "the run before, when the newest writes the same" [ 2 ]
    (candidates ~relay:true rules ".q{color:blue}");
  check_nodes "every node, for a node that may fold" [ 0; 1; 2; 3 ]
    (candidates ~relay:false rules ".q{color:blue}")

(* A node that may fold carries no order for another, so it never joins a run
   and every later query sees it. *)
let a_folding_node_stays_visible () =
  check_nodes "the folding node and the two newest runs" [ 0; 2; 3 ]
    (candidates
       ~relays:(fun i -> i <> 0)
       ~relay:true
       [
         ".a{color:red}"; ".b{color:blue}"; ".c{color:green}"; ".d{color:blue}";
       ]
       ".q{color:yellow}")

let suite =
  ( "conflict_index",
    [
      Alcotest.test_case "a query reads its buckets after the position" `Quick
        a_query_reads_its_buckets_after_the_position;
      Alcotest.test_case "a broad footprint meets every key" `Quick
        a_broad_footprint_meets_every_key;
      Alcotest.test_case "the answer is the test" `Quick the_answer_is_the_test;
      Alcotest.test_case "candidates meet on every dimension" `Quick
        candidates_meet_on_every_dimension;
      Alcotest.test_case "a relay reads the run before the newest" `Quick
        a_relay_reads_the_run_before_the_newest;
      Alcotest.test_case "a folding node stays visible" `Quick
        a_folding_node_stays_visible;
    ] )
