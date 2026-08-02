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

let optimize_rules css =
  rules css |> Rule_graph.of_rules
  |> Rule_scheduler.run ~ctx:Ctx.fragment ~finalize:Fun.id
  |> Rule_graph.to_rules |> render

(* Cascade-equivalence oracle for the safety tests: resolve every element the
   class alphabet can describe against the input and against the optimized
   output, and require an identical computed style. This judges soundness from
   the cascade itself, independent of how (or whether) a rule fired. *)
let parse css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

type node = { classes : string list }

module Node = struct
  type t = node

  let equal = ( = )
  let name _ = None
  let id _ = None
  let classes t = t.classes
  let attribute _ _ = None
  let parent _ = None
  let children _ = []
  let text_children _ = []
end

module R = Resolve.Make (Node)

let rec subsets = function
  | [] -> [ [] ]
  | x :: xs ->
      let rest = subsets xs in
      rest @ List.map (fun tail -> x :: tail) rest

let canon d = Pp.to_string ~minify:true Declaration.pp_declaration d

let computed sheet classes =
  R.resolve sheet { classes } |> List.map canon |> List.sort String.compare

let assert_cascade_preserved ~universe css =
  let before = parse css and after = parse (optimize_rules css) in
  List.iter
    (fun classes ->
      Alcotest.(check (list string))
        ("computed style for ." ^ String.concat "." classes)
        (computed before classes) (computed after classes))
    (subsets universe)

(* --- one positive case per rule kind --- *)

let merges_identical_body () =
  Alcotest.(check string)
    "same-body rules group under a combined selector list"
    ".a,.b{color:red;margin:0}"
    (optimize_rules ".a{color:red;margin:0}.b{color:red;margin:0}")

let merges_same_selector_canonically () =
  Alcotest.(check string)
    "same-selector rules concatenate, commuting blocks ordered canonically"
    ".x{color:red;margin:0}"
    (optimize_rules ".x{margin:0}.x{color:red}")

let inlines_long_selector_branch () =
  (* The [.aaaaaaaa] branch costs more than inlining [left:0] into its own rule,
     so the branch is pulled out and the residual group keeps only [.b]. *)
  Alcotest.(check string)
    "long selector branch is inlined into its receiver"
    ".aaaaaaaa{top:0;left:0}.b{left:0}"
    (optimize_rules ".aaaaaaaa{top:0}.b,.aaaaaaaa{left:0}")

(* --- each kind must refuse a merge that would reorder a real conflict --- *)

let refuses_identical_body_across_conflict () =
  (* [.a] and [.c] share a body but a conflicting [.b] sits between them;
     merging would reorder the cascade for [.a.b] / [.b.c]. *)
  assert_cascade_preserved ~universe:[ "a"; "b"; "c" ]
    ".a{color:red}.b{color:green}.c{color:red}"

let refuses_same_selector_across_conflict () =
  (* Two [.x] rules cannot fuse across a [.y] that ties them on [color]. *)
  assert_cascade_preserved ~universe:[ "x"; "y" ]
    ".x{color:red}.y{color:green}.x{color:blue}"

let refuses_default_factoring_across_override () =
  (* The default-value group would hoist [z-index:1] (the first value) and leave
     [.c] re-asserting the default, but [.b] overrides it in between on
     overlapping selectors, so the hoist is unsafe and must not happen. *)
  assert_cascade_preserved ~universe:[ "a"; "b"; "c" ]
    ".a{z-index:1}.b{z-index:2}.c{z-index:1}"

let factors_shared_declarations () =
  Alcotest.(check string)
    "shared exact declaration is factored"
    ".a,.b{display:flex;color:red}.b{color:blue}"
    (optimize_rules ".a{display:flex;color:red}.b{display:flex;color:blue}")

let factors_default_value_group () =
  Alcotest.(check string)
    "common property uses first cascade value and leaves overrides"
    ".a,.b,.c{display:flex;margin:1px}.b{margin:2px}.c{margin:3px}"
    (optimize_rules
       ".a{display:flex;margin:1px}.b{display:flex;margin:2px}.c{display:flex;margin:3px}")

let suite =
  ( "rule_scheduler",
    [
      Alcotest.test_case "merges identical bodies" `Quick merges_identical_body;
      Alcotest.test_case "merges same selector canonically" `Quick
        merges_same_selector_canonically;
      Alcotest.test_case "inlines a long selector branch" `Quick
        inlines_long_selector_branch;
      Alcotest.test_case "refuses identical-body merge across a conflict" `Quick
        refuses_identical_body_across_conflict;
      Alcotest.test_case "refuses same-selector merge across a conflict" `Quick
        refuses_same_selector_across_conflict;
      Alcotest.test_case "refuses default factoring across an override" `Quick
        refuses_default_factoring_across_override;
      Alcotest.test_case "factors shared declarations" `Quick
        factors_shared_declarations;
      Alcotest.test_case "factors default-value group" `Quick
        factors_default_value_group;
    ] )
