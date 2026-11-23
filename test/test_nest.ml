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
  let nested = Selector.v "&:hover" in
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
    (sel_string (Nest.combine parent (Selector.v "&:hover")));
  Alcotest.(check string)
    "relative child" ".card>.title"
    (sel_string
       (Nest.combine parent
          (Selector.Relative (Selector.Child, Selector.class_ "title"))))

(* pp is a serializer (docs/invariants.md), so a selector nesting composes has
   to be the one reading its print gives back. The reader takes a compound as
   one flat run of simple selectors and nests a combinator chain to the right;
   splicing a parent in verbatim would leave a compound inside a compound, or a
   chain on the left of a combinator, which prints the same and reads apart. *)
let test_combine_reads_back () =
  let check parent child expected =
    let combined = Nest.combine parent child in
    let printed = sel_string combined in
    Alcotest.(check string) expected expected printed;
    Alcotest.(check bool)
      (String.concat "" [ expected; " reads back as built" ])
      true
      (Selector.equal combined (Selector.v printed))
  in
  check
    (Selector.v "&:where(.s,.s *)")
    (Selector.v "&:hover") "&:where(.s,.s *):hover";
  check (Selector.v ".a:hover") (Selector.v "&.b") ".a:hover.b";
  check (Selector.v ".a.b") (Selector.v ".x&") ".x.a.b";
  check (Selector.v ".l a") (Selector.v "&:hover") ".l a:hover";
  check (Selector.v ".a .b") (Selector.v "&>.c") ".a .b>.c";
  check (Selector.v ".a .b") (Selector.v ".c") ".a .b .c";
  check (Selector.v ".a>.b")
    (Selector.Relative (Selector.Next_sibling, Selector.class_ "c"))
    ".a>.b+.c"

(* The same composition reached through the optimizer: merging a [&:where(...)]
   wrapper into its lone [&:hover] child builds the selector that nesting
   synthesis then compares with its neighbour's. Built in the reader's shape,
   the pair nests on the first pass instead of only once the printed text is
   read back, so minifying the output again changes nothing. The innermost body
   is a nested rule, not a nested declarations rule, which would be flattened
   under any targets (CSS Nesting 1 sec. 5). *)
let test_merged_selector_nests_on_first_pass () =
  let minify css =
    match Css.of_string css with
    | Ok { stylesheet; _ } ->
        Css.to_string ~minify:true
          (Css.optimize ~targets:Optimize_helpers.nesting_targets stylesheet)
    | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
  in
  let once =
    minify
      ".b{color:red;@media (prefers-color-scheme:dark){&:where(.s,.s \
       *){color:blue}}@media (prefers-color-scheme:dark){&:where(.s,.s \
       *){&:hover{@media (hover:hover){&{color:green}}}}}}"
  in
  Alcotest.(check string)
    "nested on the first pass"
    ".b{color:red;@media(prefers-color-scheme:dark){&:where(.s,.s \
     *){color:#00f;&:hover{@media(hover:hover){&{color:green}}}}}}"
    once;
  Alcotest.(check string) "a fixed point" once (minify once)

let nesting_targets = Optimize_helpers.nesting_targets

let minify_for targets css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      Css.to_string ~minify:true (Css.optimize ~targets stylesheet)
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* CSS Cascade 6 sec. 3.5.3 implies the scoping root in front of a scoped rule's
   selector. Merging [.a{.x &{...}}] into [.x .a] there moves that root in front
   of [.x], so an [.x] outside the scope stops counting; Chromium 153 styles
   different elements under the two. A child that starts with its parent merges
   as before. *)
let test_merge_lone_in_scope () =
  Alcotest.(check string)
    "a child naming its parent late stays nested"
    "@scope(.c){.a{.x &{color:red}}}"
    (minify_for nesting_targets "@scope (.c){.a{.x &{color:red}}}");
  Alcotest.(check string)
    "a child starting with its parent merges" "@scope(.c){.a .b{color:red}}"
    (minify_for nesting_targets "@scope (.c){.a{.b{color:red}}}")

(* A form is emitted only if every browser the run serves parses it. MDN's
   browser-compat-data records CSS Nesting ([css.selectors.nesting]) in full
   from Chrome 120, Firefox 117 and Safari 17.2; Chrome 111 and Safari 16.4, the
   default floors, read none of it (CSS Syntax 3 throws a nested rule away with
   the declaration it ends up in), and Chrome 112 to 119 and Safari 16.5 to 17.1
   refuse a nested selector that starts with an identifier. So under the default
   floors nesting is neither synthesised nor kept: an author's nesting is
   flattened, which CSS Nesting 1 sec. 4 and 5 make exact. Under
   [--enforce-spec] the browsers are every conforming implementation, which
   includes one that predates CSS Nesting. *)
let test_nesting_follows_the_floors () =
  let minify ?(enforce_spec = false) ?targets css =
    match Css.of_string css with
    | Ok { stylesheet; _ } ->
        Css.to_string ~minify:true
          (Css.optimize ~enforce_spec ?targets stylesheet)
    | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
  in
  Alcotest.(check string)
    "no nesting is synthesised for the default floors"
    ".a{color:red}.a .b{color:#00f}"
    (minify ".a{color:red}.a .b{color:blue}");
  Alcotest.(check string)
    "nor under --enforce-spec" ".a{color:red}.a .b{color:#00f}"
    (minify ~enforce_spec:true ".a{color:red}.a .b{color:blue}");
  Alcotest.(check string)
    "an author's nesting is flattened for the default floors"
    ".a{color:red}.a .b{color:#00f}"
    (minify ".a{color:red;.b{color:blue}}");
  Alcotest.(check string)
    "and under --enforce-spec" ".a{color:red}.a .b{color:#00f}"
    (minify ~enforce_spec:true ".a{color:red;.b{color:blue}}");
  Alcotest.(check string)
    "no relaxed selector either" ".a{color:#00f}.a div{color:red}"
    (minify ".a{color:blue;& div{color:red}}");
  (* [Css.Optimize.rules] keeps an author's nesting, so the [&] prefix stays
     there too: [div] alone starts with an identifier. *)
  Alcotest.(check (list string))
    "nor through the rule-list entry point"
    [ ".a{color:red;& div{top:0}}" ]
    (rule_strings (Css.Optimize.rules [ rule ".a{color:red;& div{top:0}}" ]));
  (* Floors that parse nesting keep and synthesise it. *)
  Alcotest.(check string)
    "nesting is synthesised where every target parses it"
    ".a{color:red;.b{color:#00f}}"
    (minify ~targets:nesting_targets ".a{color:red}.a .b{color:blue}");
  Alcotest.(check string)
    "and an author's nesting stays" ".a{color:red;.b{color:#00f}}"
    (minify ~targets:nesting_targets ".a{color:red;.b{color:blue}}")

(* CSS Nesting 1 sec. 3.4 and 5 keep a declaration written after a nested rule
   in place, as a nested declarations rule, where Chrome 120 to 129, Firefox 117
   to 131 and Safari 17.2 to 18.1 moved it ahead of every nested rule. So
   [.a{&{color:red}color:blue}] is blue in one and red in the other, and the
   compatibility data cascade ships names no release that reads it in place. A
   rule holding one is flattened under any floors, into the order the
   specification gives. *)
let test_nested_declarations_are_flattened () =
  let minify = minify_for in
  Alcotest.(check string)
    "a declaration after a nested rule" ".a{color:#00f}"
    (minify nesting_targets ".a{&{color:red}color:blue}");
  Alcotest.(check string)
    "a declaration in a nested @media" "@media print{.a{color:#00f}}"
    (minify nesting_targets ".a{@media print{color:blue}}");
  (* Control: a rule with no nested declarations stays nested. *)
  Alcotest.(check string)
    "nested rules alone stay" ".a{color:red;&:hover{color:#00f}}"
    (minify nesting_targets ".a{color:red;&:hover{color:blue}}")

(* Outside a style rule [&] is [:scope] (CSS Nesting 1 sec. 4), and nested it is
   the parent, so a flat rule whose selector holds [&] cannot be re-parented:
   [.a &] matches an [.a]'s descendant scoping root, and nested under [.a] it
   would be [.a .a]. The same holds one level down, where [&] is the enclosing
   rule. Chromium 153 styles 279 elements of a test tree under
   [.c{.a{color:red}.a &{color:blue}}] and 171 under [.c
   .a{color:red;&{color:blue}}]. *)
let test_nesting_selector_is_not_reparented () =
  let same targets css =
    Alcotest.(check string) css css (minify_for targets css)
  in
  List.iter
    (same Css.Optimize.evergreen_targets)
    [
      ".a{overflow-x:scroll}.a &{width:20px}";
      "div{opacity:.5}div>&{flex-basis:10px}";
    ];
  List.iter (same nesting_targets)
    [
      ".a{overflow-x:scroll}.a &{width:20px}";
      "div{opacity:.5}div>&{flex-basis:10px}";
      ".c{.a{color:red}.a &{color:#00f}}";
      ":has(.d){.b{column-gap:3px}.b~&{overflow:hidden}}";
      ".c~.c{.b{--x:2px}.b~&{top:2px}}";
    ];
  Alcotest.(check string)
    "flattened for the default floors"
    ":has(.d) .b{column-gap:3px}.b~:has(.d){overflow:hidden}"
    (minify_for Css.Optimize.evergreen_targets
       ":has(.d){.b{column-gap:3px}.b~&{overflow:hidden}}")

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
  (* CSS Nesting 1 sec. 4: [&] cannot represent a pseudo-element, so [&] alone
     under one matches nothing, though [.a::before] written flat would. *)
  Alcotest.(check string)
    "[&] alone under a pseudo-element parent goes" ".a:before{}"
    (merged ".a::before{&{color:red}}");
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

(* CSS Nesting 1 sec. 4: [&] cannot represent a pseudo-element, so nesting
   [.a::part(p):hover] under [.a::part(p)] as [&:hover] would match nothing
   where the flat rule matches the hovered part. *)
let test_rules_keep_a_pseudo_element_parent_flat () =
  let original = rules ".a::part(p){color:red}.a::part(p):hover{color:blue}" in
  Alcotest.(check (list string))
    "no nesting under a pseudo-element" (rule_strings original)
    (rule_strings (Nest.rules original))

let test_rules_preserves_competing_outside_selector () =
  let original =
    rules ".card{color:red}.card .title{width:1px}.title{font-weight:bold}"
  in
  Alcotest.(check (list string))
    "outside competitor prevents nesting" (rule_strings original)
    (rule_strings (Nest.rules original))

(* The same rule read by [drop_dead_nested], which keeps the rule and edits its
   selector list where [merge_lone] above folds the whole thing away. A branch
   composing to a combinator after the pseudo-element goes ([& b] under
   [a::before] is [a::before b]), and so does one putting the combinator before
   it: CSS Nesting 1 sec. 4 makes [&] match what [:is()] of the parent matches,
   and [:is()] matches no pseudo-element, so [.x &] is [.x :is(a::before)] and
   Chromium 153 leaves the pseudo-element unstyled. A branch that does not go
   through the pseudo-element stays. Dropping the nested rule outright would
   take a live branch with it, and keeping it whole would leave a branch no
   engine matches. *)
let test_drop_dead_nested_keeps_live_branches () =
  let dropped css =
    Pp.to_string ~minify:true Stylesheet.pp_rule
      (Nest.drop_dead_nested (rule css))
  in
  Alcotest.(check string)
    "[&] names no pseudo-element, so both branches are dead"
    {|a:before{content:""}|}
    (dropped {|a::before{content:"";.x &,& b{color:red}}|});

  Alcotest.(check string)
    "a nested rule whose every branch is dead goes with its body"
    {|a:before{content:""}|}
    (dropped {|a::before{content:"";& b,& i{color:red}}|});
  (* Control: no pseudo-element, so no branch is dead and both stay. *)
  Alcotest.(check string)
    "a plain parent keeps both branches" ".a{color:red;.x &,& b{width:1px}}"
    (dropped ".a{color:red;.x &,& b{width:1px}}")

let suite =
  ( "nest",
    [
      Alcotest.test_case "contains and substitute" `Quick
        test_contains_and_substitute;
      Alcotest.test_case "combine relative nested and descendant" `Quick
        test_combine_relative_nested_and_descendant;
      Alcotest.test_case "combine reads back" `Quick test_combine_reads_back;
      Alcotest.test_case "merged selector nests on first pass" `Quick
        test_merged_selector_nests_on_first_pass;
      Alcotest.test_case "merge_lone wrapper" `Quick test_merge_lone_wrapper;
      Alcotest.test_case "merge_lone in scope" `Quick test_merge_lone_in_scope;
      Alcotest.test_case "nesting follows the floors" `Quick
        test_nesting_follows_the_floors;
      Alcotest.test_case "nested declarations are flattened" `Quick
        test_nested_declarations_are_flattened;
      Alcotest.test_case "nesting selector is not re-parented" `Quick
        test_nesting_selector_is_not_reparented;
      Alcotest.test_case "merge_lone pseudo-element parent" `Quick
        test_merge_lone_pseudo_element_parent;
      Alcotest.test_case "drop_dead_nested keeps live branches" `Quick
        test_drop_dead_nested_keeps_live_branches;
      Alcotest.test_case "hoist declaration runs" `Quick
        test_hoist_declaration_runs;
      Alcotest.test_case "rules synthesizes isolated chain" `Quick
        test_rules_synthesizes_isolated_chain;
      Alcotest.test_case "rules keep a pseudo-element parent flat" `Quick
        test_rules_keep_a_pseudo_element_parent_flat;
      Alcotest.test_case "rules preserves competing outside selector" `Quick
        test_rules_preserves_competing_outside_selector;
    ] )
