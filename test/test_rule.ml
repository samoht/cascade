open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)

let rule_strings rules =
  List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules

let test_single_deduplicates_without_box_composition () =
  let optimized =
    Rule.single ~ctx:Ctx.fragment
      (rule
         ".a{color:red;color:blue;margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}")
  in
  Alcotest.(check string)
    "single rule cleanup skips box composition"
    ".a{color:#00f;margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule optimized)

let test_finalize_composes_and_canonicalizes_selector () =
  let optimized =
    Rule.finalize ~ctx:Ctx.fragment
      (rule
         ".b,.a{margin-top:1px;margin-right:2px;margin-bottom:1px;margin-left:2px}")
  in
  Alcotest.(check string)
    "finalize composes declarations and canonicalizes selector"
    ".a,.b{margin:1px 2px}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule optimized)

let test_drop_shadowed_declarations_keeps_live_properties () =
  let optimized =
    Rule.drop_shadowed (rules ".a{color:red;width:1px}.a{color:blue}")
  in
  Alcotest.(check (list string))
    "only fully shadowed declaration is removed"
    [ ".a{width:1px}"; ".a{color:blue}" ]
    (rule_strings optimized)

let test_drop_shadowed_selector_list_requires_all_branches () =
  let optimized =
    Rule.drop_shadowed (rules ".a,.b{color:red}.a{color:blue}.b{color:green}")
  in
  Alcotest.(check (list string))
    "list declaration drops when every branch is shadowed"
    [ ".a{color:blue}"; ".b{color:green}" ]
    (rule_strings optimized)

let extend_ctx = Ctx.with_extend_lists true Ctx.fragment

let test_merge_adjacent_identical_bodies () =
  let merged =
    Rule.merge_adjacent_identical ~ctx:extend_ctx
      (rules ".flex-grow{flex-grow:1}.grow{flex-grow:1}.basis-0{flex-basis:0}")
  in
  Alcotest.(check (list string))
    "adjacent identical bodies merge into a selector list"
    [ ".flex-grow,.grow{flex-grow:1}"; ".basis-0{flex-basis:0}" ]
    (rule_strings merged)

let test_merge_adjacent_skips_intervening_rule () =
  let input = rules ".a{color:red}.mid{margin:0}.b{color:red}" in
  let merged = Rule.merge_adjacent_identical ~ctx:extend_ctx input in
  Alcotest.(check bool)
    "non-adjacent identical bodies stay split, input physically unchanged" true
    (merged == input)

let test_merge_adjacent_skips_vendor_pseudo () =
  (* A selector list with an unsupported vendor branch invalidates the whole
     rule in other engines, so vendor pseudo-elements never share a list. *)
  let input =
    rules "::-webkit-scrollbar{display:none}::-moz-focus-inner{display:none}"
  in
  let merged = Rule.merge_adjacent_identical ~ctx:extend_ctx input in
  Alcotest.(check bool)
    "vendor pseudo-element selectors never share a list" true (merged == input)

let suite =
  ( "rule",
    [
      Alcotest.test_case "single deduplicates without box composition" `Quick
        test_single_deduplicates_without_box_composition;
      Alcotest.test_case "finalize composes and canonicalizes selector" `Quick
        test_finalize_composes_and_canonicalizes_selector;
      Alcotest.test_case "drop shadowed declarations keeps live properties"
        `Quick test_drop_shadowed_declarations_keeps_live_properties;
      Alcotest.test_case "drop shadowed selector list requires all branches"
        `Quick test_drop_shadowed_selector_list_requires_all_branches;
      Alcotest.test_case "merge adjacent identical bodies" `Quick
        test_merge_adjacent_identical_bodies;
      Alcotest.test_case "merge adjacent skips intervening rule" `Quick
        test_merge_adjacent_skips_intervening_rule;
      Alcotest.test_case "merge adjacent skips vendor pseudo" `Quick
        test_merge_adjacent_skips_vendor_pseudo;
    ] )
