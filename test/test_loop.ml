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
      Alcotest.(check int) "anchor was scored once and rescored once" 4 !calls;
      Alcotest.(check (list string))
        "pool unchanged" [ ".a"; ".b"; ".c" ] (names pool)
  | _ -> Alcotest.fail "expected 3 nodes"

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
    ] )
