open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        (Css.statements stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render rules =
  String.concat ""
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) rules)

let test_common_hoists_profitable_adjacent_run () =
  Factor.clear ();
  let optimized =
    Factor.common
      (rules
         ".a{display:flex;color:red}.b{display:flex;color:blue}.c{display:flex;color:green}")
  in
  Alcotest.(check string)
    "shared declaration is hoisted"
    ".a,.b,.c{display:flex;color:red}.b{color:blue}.c{color:green}"
    (render optimized)

let test_common_rejects_duplicate_property_member () =
  Factor.clear ();
  let css = ".a{display:flex;display:block}.b{display:flex}" in
  Alcotest.(check string)
    "unsafe duplicate property run is untouched" css
    (Factor.common (rules css) |> render)

let test_anchor_hoists_across_unrelated_gap () =
  Factor.clear ();
  let optimized =
    Factor.anchor
      (rules
         ".a{background-color:red;border-color:red}.x{width:1px}.b{background-color:red;border-color:blue}.c{background-color:red;border-color:green}")
  in
  Alcotest.(check string)
    "shared declaration crosses unrelated rule"
    ".a,.b,.c{background-color:red}.a{border-color:red}.x{width:1px}.b{border-color:blue}.c{border-color:green}"
    (render optimized)

let test_anchor_respects_tied_overlap () =
  Factor.clear ();
  let css = ".a{color:red}.x.a{color:green}.a{color:blue}" in
  Alcotest.(check string)
    "intervening same-specificity overlap blocks merge" css
    (Factor.anchor (rules css) |> render)

let test_run_reaches_fixpoint_with_finalizer () =
  Factor.clear ();
  let optimized =
    Factor.run ~ctx:Ctx.fragment
      ~finalize:(Rule.finalize ~ctx:Ctx.fragment)
      (rules
         ".a{display:flex;margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}.b{display:flex;margin-top:2px;margin-right:2px;margin-bottom:2px;margin-left:2px}.c{display:flex;margin-top:3px;margin-right:3px;margin-bottom:3px;margin-left:3px}")
  in
  Alcotest.(check string)
    "run factors and finalizes leftovers"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (render optimized)

let suite =
  ( "factor",
    [
      Alcotest.test_case "common hoists profitable adjacent run" `Quick
        test_common_hoists_profitable_adjacent_run;
      Alcotest.test_case "common rejects duplicate property member" `Quick
        test_common_rejects_duplicate_property_member;
      Alcotest.test_case "anchor hoists across unrelated gap" `Quick
        test_anchor_hoists_across_unrelated_gap;
      Alcotest.test_case "anchor respects tied overlap" `Quick
        test_anchor_respects_tied_overlap;
      Alcotest.test_case "run reaches fixpoint with finalizer" `Quick
        test_run_reaches_fixpoint_with_finalizer;
    ] )
