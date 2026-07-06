open Cascade

let rules css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let nid i = Rule_graph.Node_id.of_int_exn i
let nids ids = List.map nid ids

let positive_candidate_uses_final_size () =
  let graph =
    Rule_graph.of_rules (rules ".a{color:red;margin:0}.b{color:red;margin:0}")
  in
  match
    Rule_rewrite.v ~kind:Identical_body ~finalize:Fun.id graph
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules ".a,.b{color:red;margin:0}")
  with
  | None -> Alcotest.fail "grouped identical body should save bytes"
  | Some candidate ->
      Alcotest.(check bool) "saving is positive" true (candidate.saving > 0);
      Alcotest.(check int)
        "generation is captured"
        (Rule_graph.generation graph)
        candidate.generation

let rejects_non_saving_rewrite () =
  let graph = Rule_graph.of_rules (rules ".a{color:red}.b{color:blue}") in
  match
    Rule_rewrite.v ~kind:Same_selector ~finalize:Fun.id graph
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules ".a{color:red}.b{color:blue}")
  with
  | None -> ()
  | Some _ -> Alcotest.fail "non-saving rewrite must not become a candidate"

let rejects_empty_finalized_output () =
  let graph = Rule_graph.of_rules (rules ".a{color:red}.b{color:red}") in
  let finalize (rule : Stylesheet.rule) =
    { rule with Stylesheet.declarations = [] }
  in
  match
    Rule_rewrite.v ~kind:Identical_body ~finalize graph
      ~consume:(nids [ 0; 1 ])
      ~produce:(rules ".a,.b{color:red}")
  with
  | None -> ()
  | Some _ -> Alcotest.fail "empty finalized output must be rejected"

let suite =
  ( "rule_rewrite",
    [
      Alcotest.test_case "positive candidate uses finalized size" `Quick
        positive_candidate_uses_final_size;
      Alcotest.test_case "rejects non-saving rewrite" `Quick
        rejects_non_saving_rewrite;
      Alcotest.test_case "rejects empty finalized output" `Quick
        rejects_empty_finalized_output;
    ] )
