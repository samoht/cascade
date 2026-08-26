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

let disjoint_property_run n =
  let buffer = Buffer.create (n * 16) in
  let formatter = Fmt.with_buffer buffer in
  for i = 0 to n - 1 do
    Fmt.pf formatter ".a{p%d:0}" i
  done;
  rules (Buffer.contents buffer)

let measure f =
  Gc.full_major ();
  let w0 = Gc.minor_words () in
  let r = f () in
  ignore (Sys.opaque_identity r);
  Gc.minor_words () -. w0

(* A rule's property should be looked up in the later writes for its selector,
   not compared with every declaration written by every later rule. Counting
   allocated words makes the complexity bound independent of scheduler load and
   CPU frequency. *)
let test_drop_shadowed_disjoint_properties_is_subquadratic () =
  let small = disjoint_property_run 2_000 in
  let large = disjoint_property_run 4_000 in
  let small_words = measure (fun () -> Rule.drop_shadowed small) in
  let large_words = measure (fun () -> Rule.drop_shadowed large) in
  let ratio = large_words /. small_words in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx for 2x N)" small_words large_words ratio)
    true (ratio < 3.)

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

(* --- complexity guard --- *)

(* [n] adjacent rules sharing one body and no two selectors alike, so the whole
   run is one group and [take] grows it to [n]. The [:where(.sidebar)] prefix is
   what makes the cost readable: the group- and peer-marker probes each take a
   [String.sub] of a class name longer than the marker they look for, so a walk
   of one of these selectors allocates and minor words count walks. *)
let same_body_run n =
  let b = Buffer.create (n * 48) in
  let out = Fmt.with_buffer b in
  for i = 0 to n - 1 do
    Fmt.pf out ":where(.sidebar) .s%d{color:red}" i
  done;
  rules (Buffer.contents b)

(* Whether a run may share one selector list turns on two facts read off each
   selector alone, so the group decision costs one walk per member. Asking it of
   every PAIR costs [n(n-1)] walks, and a run of same-body rules is exactly what
   a utility-class sheet emits. Doubling the run must about double the words. *)
let test_merge_run_reads_each_selector_once () =
  (* The ratio below is readable only while the marker probe allocates. Pin that
     first, so a probe that stops allocating fails here rather than leaving the
     guard passing on a flat line. *)
  let sel = Selector.of_string ":where(.sidebar) .s0" in
  Alcotest.(check bool)
    "marker probe allocates, so words count walks" true
    (measure (fun () -> Selector.has_is_where_pattern sel) > 0.);
  let in1 = same_body_run 200 and in2 = same_body_run 400 in
  let merge input () = Rule.merge_adjacent_identical ~ctx:extend_ctx input in
  Alcotest.(check int)
    "the whole run merges into one rule" 1
    (List.length (merge in2 ()));
  let a1 = measure (merge in1) in
  let a2 = measure (merge in2) in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.1fx for 2x N)" a1 a2 (a2 /. a1))
    true
    (a2 < a1 *. 3.)

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
      Alcotest.test_case "drop shadowed disjoint properties is subquadratic"
        `Quick test_drop_shadowed_disjoint_properties_is_subquadratic;
      Alcotest.test_case "merge adjacent identical bodies" `Quick
        test_merge_adjacent_identical_bodies;
      Alcotest.test_case "merge adjacent skips intervening rule" `Quick
        test_merge_adjacent_skips_intervening_rule;
      Alcotest.test_case "merge adjacent skips vendor pseudo" `Quick
        test_merge_adjacent_skips_vendor_pseudo;
      Alcotest.test_case "merge run reads each selector once" `Quick
        test_merge_run_reads_each_selector_once;
    ] )
