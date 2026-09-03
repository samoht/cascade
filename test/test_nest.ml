open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)

let rule_strings rules =
  List.map (fun r -> Pp.to_string ~minify:true Stylesheet.pp_rule r) rules

let sel_string sel = Selector.to_string ~minify:true sel

let test_contains_and_substitute () =
  let parent = Selector.class_ "card" in
  let nested = Selector.of_string "&:hover" in
  Alcotest.(check bool) "contains nesting" true (Nest.contains nested);
  Alcotest.(check bool)
    "plain selector" false
    (Nest.contains (Selector.class_ "plain"));
  Alcotest.(check string)
    "substitute parent" ".card:hover"
    (sel_string (Nest.substitute ~parent nested))

let test_combine_relative_nested_and_descendant () =
  let parent = Selector.class_ "card" in
  Alcotest.(check string)
    "descendant for plain child" ".card .title"
    (sel_string (Nest.combine parent (Selector.class_ "title")));
  Alcotest.(check string)
    "nesting replacement" ".card:hover"
    (sel_string (Nest.combine parent (Selector.of_string "&:hover")));
  Alcotest.(check string)
    "relative child" ".card>.title"
    (sel_string
       (Nest.combine parent
          (Selector.Relative (Selector.Child, Selector.class_ "title"))))

let test_merge_lone_wrapper () =
  let merged = Nest.merge_lone (rule ".card{.title{color:red}}") in
  Alcotest.(check string)
    "pure wrapper merged" ".card .title{color:red}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule merged);
  let with_decl = Nest.merge_lone (rule ".card{color:red;.title{width:1px}}") in
  Alcotest.(check string)
    "wrapper with declarations preserved" ".card{color:red;.title{width:1px}}"
    (Pp.to_string ~minify:true Stylesheet.pp_rule with_decl)

(* CSS Selectors 4 sec. 3.6.5 makes a combinator after a pseudo-element invalid,
   and the selector reader refuses [.a::before .b] when it is authored directly.
   Merging a lone wrapper into its sole nested rule reaches that selector from a
   valid parent and a valid child, so it applies the rule too, as
   [Flatten.block] does for the same input: the branches that follow the
   pseudo-element go, and a wrapper left with nothing goes with the empty
   rules. *)
let test_merge_lone_pseudo_element_parent () =
  let merged css =
    Pp.to_string ~minify:true Stylesheet.pp_rule (Nest.merge_lone (rule css))
  in
  Alcotest.(check string)
    "a descendant under a pseudo-element parent goes" ".a:before{}"
    (merged ".a::before{.b{color:red}}");
  Alcotest.(check string)
    "a child combinator goes the same way" ".a:before{}"
    (merged ".a::before{>.b{color:red}}");
  Alcotest.(check string)
    "a pseudo-class ::before does not take goes too" ".a:before{}"
    (merged ".a::before{.b,&:hover{color:red}}");
  (* Control: no pseudo-element, so the merge is valid and still happens. *)
  Alcotest.(check string)
    "a plain parent still merges" ".a .b{color:red}"
    (merged ".a{.b{color:red}}")

(* CSS Nesting 1 sec. 3.4 holds a declaration written after a nested statement
   behind it. Moving it back is cascade-neutral exactly when it commutes with
   what it crosses, so the hoist is decided per declaration and per property. *)
let test_hoist_declaration_runs () =
  let hoisted css =
    Pp.to_string ~minify:true Stylesheet.pp_rule
      (Nest.hoist_declaration_runs (rule css))
  in
  Alcotest.(check string)
    "a disjoint run rejoins the rule's own declarations"
    ".a{color:red;& b{width:1px}}"
    (hoisted ".a{& b{width:1px}color:red}");
  Alcotest.(check string)
    "a clashing run keeps its place" ".a{& b{color:blue}color:red}"
    (hoisted ".a{& b{color:blue}color:red}");
  Alcotest.(check string)
    "a crossed shorthand blocks its longhand"
    ".a{& b{margin:1px}margin-top:2px}"
    (hoisted ".a{& b{margin:1px}margin-top:2px}");
  Alcotest.(check string)
    "only the clashing declaration stays"
    ".a{width:2px;& b{color:blue}color:red}"
    (hoisted ".a{& b{color:blue}color:red;width:2px}");
  (* [!important] wins wherever it sits, so it never has to wait. *)
  Alcotest.(check string)
    "a differing importance is not a clash"
    ".a{color:red!important;& b{color:blue}}"
    (hoisted ".a{& b{color:blue}color:red!important}");
  (* An unknown at-rule is raw text, so nothing may be assumed about it. *)
  Alcotest.(check string)
    "an unknown at-rule blocks everything" ".a{@wat foo{q:1}color:red}"
    (hoisted ".a{@wat foo{q:1}color:red}")

let test_rules_synthesizes_isolated_chain () =
  let nested = Nest.rules (rules ".card{color:red}.card .title{width:1px}") in
  Alcotest.(check (list string))
    "flat chain nests when shorter"
    [ ".card{color:red;.title{width:1px}}" ]
    (rule_strings nested)

let test_rules_preserves_competing_outside_selector () =
  let original =
    rules ".card{color:red}.card .title{width:1px}.title{font-weight:bold}"
  in
  Alcotest.(check (list string))
    "outside competitor prevents nesting" (rule_strings original)
    (rule_strings (Nest.rules original))

let suite =
  ( "nest",
    [
      Alcotest.test_case "contains and substitute" `Quick
        test_contains_and_substitute;
      Alcotest.test_case "combine relative nested and descendant" `Quick
        test_combine_relative_nested_and_descendant;
      Alcotest.test_case "merge_lone wrapper" `Quick test_merge_lone_wrapper;
      Alcotest.test_case "merge_lone pseudo-element parent" `Quick
        test_merge_lone_pseudo_element_parent;
      Alcotest.test_case "hoist declaration runs" `Quick
        test_hoist_declaration_runs;
      Alcotest.test_case "rules synthesizes isolated chain" `Quick
        test_rules_synthesizes_isolated_chain;
      Alcotest.test_case "rules preserves competing outside selector" `Quick
        test_rules_preserves_competing_outside_selector;
    ] )
