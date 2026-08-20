open Cascade

let rules css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render rules =
  String.concat ""
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules)

let candidates css =
  css |> rules |> Rule_graph.of_rules
  |> Rule_candidate.enumerate ~ctx:Ctx.fragment ~finalize:Fun.id

let candidates_with_ctx ctx css =
  css |> rules |> Rule_graph.of_rules
  |> Rule_candidate.enumerate ~ctx ~finalize:Fun.id

let large_normal_css prefix =
  let fillers =
    List.init 129 (fun i -> Fmt.str ".f%d{left:%dpx;top:%dpx}" i i i)
    |> String.concat ""
  in
  prefix ^ fillers

let has_candidate ~kind ~produce candidates =
  List.exists
    (fun candidate ->
      Rule_rewrite.equal_kind candidate.Rule_rewrite.kind kind
      && String.equal produce (render candidate.produce))
    candidates

let default_factoring_uses_first_value_with_overrides () =
  let candidates =
    candidates
      ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}"
  in
  Alcotest.(check bool)
    "default factoring candidate keeps first value and emits overrides" true
    (has_candidate ~kind:Default_factoring
       ~produce:".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
       candidates)

let large_normal_mode_keeps_local_exact_factoring () =
  let candidates =
    candidates
      (large_normal_css ".a{display:flex;color:red}.b{display:flex;color:blue}")
  in
  Alcotest.(check bool)
    "large normal mode keeps local exact shared-declaration candidates" true
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a,.b{display:flex}.a{color:red}.b{color:blue}" candidates)

let large_normal_mode_keeps_local_default_factoring () =
  let candidates =
    candidates
      (large_normal_css
         ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}")
  in
  Alcotest.(check bool)
    "large normal mode keeps local default-value candidates" true
    (has_candidate ~kind:Default_factoring
       ~produce:".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
       candidates)

let exact_shared_declarations_prune_long_selector () =
  let candidates =
    candidates
      "#f{top:0;position:absolute;width:100%}.b{padding:16px;position:absolute;z-index:1}.b-arrow{position:absolute}.very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
  in
  Alcotest.(check bool)
    "long selector is not pulled into the shared position:absolute group" false
    (List.exists
       (fun candidate ->
         candidate.Rule_rewrite.kind = Exact_shared_declarations
         && Astring.String.is_infix ~affix:".very-long-closebtn-selector"
              (render candidate.produce))
       candidates)

let same_selector_candidate_orders_commuting_blocks_canonically () =
  let candidates = candidates ".x{margin:0}.x{color:red}" in
  Alcotest.(check bool)
    "same-selector candidate canonicalizes commuting declaration blocks" true
    (has_candidate ~kind:Same_selector ~produce:".x{color:red;margin:0}"
       candidates)

let selector_list_extension_keeps_strict_alternative () =
  let css = ".a,.b{color:red}.c{color:red}.d{color:red}" in
  let strict = candidates css in
  Alcotest.(check bool)
    "plain context ignores the existing selector list" true
    (has_candidate ~kind:Identical_body ~produce:".c,.d{color:red}" strict);
  Alcotest.(check bool)
    "plain context does not extend the existing selector list" false
    (has_candidate ~kind:Identical_body ~produce:".a,.b,.c,.d{color:red}" strict);
  let extended =
    candidates_with_ctx (Ctx.with_extend_lists true Ctx.fragment) css
  in
  Alcotest.(check bool)
    "extended context still exposes the strict alternative" true
    (has_candidate ~kind:Identical_body ~produce:".c,.d{color:red}" extended);
  Alcotest.(check bool)
    "extended context exposes the locally better selector-list extension" true
    (has_candidate ~kind:Identical_body ~produce:".a,.b,.c,.d{color:red}"
       extended)

let exact_factoring_keeps_later_shorthand_overlap () =
  let candidates =
    candidates
      ".c{border-color:green}.a.c{border-top:3px solid red;border-color:green}"
  in
  Alcotest.(check bool)
    "border-color must not be lifted before an earlier border-top leftover"
    false
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a.c,.c{border-color:green}.a.c{border-top:3px solid red}"
       candidates)

let exact_factoring_keeps_prior_member_leftover_overlap () =
  let candidates =
    candidates
      ".b{border-color:green;border-top-color:red}.a{border-color:green}.a.b.c{border-color:green}"
  in
  Alcotest.(check bool)
    "border-color must not be lifted away from a prior overlapping leftover"
    false
    (has_candidate ~kind:Exact_shared_declarations
       ~produce:".a,.a.b.c,.b{border-color:green}.b{border-top-color:red}"
       candidates)

let default_factoring_keeps_prior_member_leftover_overlap () =
  let candidates =
    candidates
      ".b{border-color:green;border-top-color:red}.a{border-color:green;margin-top:7px}.a.b.c{border-color:green}"
  in
  Alcotest.(check bool)
    "default group must not lift a later default before a prior overlap" false
    (has_candidate ~kind:Default_factoring
       ~produce:
         ".a,.a.b.c,.b{border-color:green}.b{border-top-color:red}.a{margin-top:7px}"
       candidates)

let suite =
  ( "rule_candidate",
    [
      Alcotest.test_case "default factoring uses first value with overrides"
        `Quick default_factoring_uses_first_value_with_overrides;
      Alcotest.test_case "large normal mode keeps local exact factoring" `Quick
        large_normal_mode_keeps_local_exact_factoring;
      Alcotest.test_case "large normal mode keeps local default factoring"
        `Quick large_normal_mode_keeps_local_default_factoring;
      Alcotest.test_case "exact shared declarations prune long selector" `Quick
        exact_shared_declarations_prune_long_selector;
      Alcotest.test_case
        "same selector candidate orders commuting blocks canonically" `Quick
        same_selector_candidate_orders_commuting_blocks_canonically;
      Alcotest.test_case "selector-list extension keeps strict alternative"
        `Quick selector_list_extension_keeps_strict_alternative;
      Alcotest.test_case "exact factoring keeps later shorthand overlap" `Quick
        exact_factoring_keeps_later_shorthand_overlap;
      Alcotest.test_case "exact factoring keeps prior member leftover overlap"
        `Quick exact_factoring_keeps_prior_member_leftover_overlap;
      Alcotest.test_case "default factoring keeps prior member leftover overlap"
        `Quick default_factoring_keeps_prior_member_leftover_overlap;
    ] )
