open Cascade

let statements css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> Css.statements stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Pp.to_string ~minify:true Stylesheet.pp_stylesheet stmts
let canonical css = statements css |> Rule_order.canonicalize |> render

let sorts_independent_rules_into_content_order () =
  Alcotest.(check string)
    "independent rules sort by serialized content" ".a{margin:0}.b{color:red}"
    (canonical ".b{color:red}.a{margin:0}")

let keeps_conflicting_rules_in_source_order () =
  (* Same-selector rules coalesce and the merged declarations reduce with the
     cascade dedup: the last write of a property wins and the dead earlier one
     is dropped, so the two source orders keep their opposite winners and stay
     distinct. *)
  Alcotest.(check string)
    "same selector and property keeps the last winner" ".a{color:blue}"
    (canonical ".a{color:red}.a{color:blue}");
  Alcotest.(check string)
    "opposite source order keeps the opposite winner" ".a{color:red}"
    (canonical ".a{color:blue}.a{color:red}")

let custom_property_position_converges () =
  (* A custom-property definition and a [var()] user are cascade-independent, so
     both source orderings reach one canonical form. *)
  let want = ".x{filter:blur(var(--b))}:root{--b:1}" in
  Alcotest.(check string)
    "definition after the user" want
    (canonical ".x{filter:blur(var(--b))}:root{--b:1}");
  Alcotest.(check string)
    "definition before the user" want
    (canonical ":root{--b:1}.x{filter:blur(var(--b))}")

let custom_property_importance_survives () =
  (* Cascade 5 sec. 6.2: an important declaration beats a normal one whatever
     the order, so [!important] on a custom property is cascade-significant and
     the projection has to carry it over. *)
  Alcotest.(check string)
    "important is kept" ":root{--x:red!important}"
    (canonical ":root{--x:red !important}");
  Alcotest.(check bool)
    "an important definition does not project like a normal one" true
    (canonical ":root{--x:red !important}" <> canonical ":root{--x:red}")

let custom_property_layer_and_meta_survive () =
  (* A custom declaration also carries the cascade layer it was written in and
     its caller metadata. The projection rebuilds the declaration from its
     value, so both have to be carried over with it. *)
  let inject, project = Css.meta () in
  let decl, _ =
    Css.var ~layer:"theme" ~meta:(inject "tag") "x" Css.Length (Css.Rem 1.)
  in
  let projected =
    Rule_order.canonicalize
      [ Css.rule ~selector:(Selector.of_string ":root") [ decl ] ]
  in
  match List.filter_map Css.statement_declarations projected with
  | [ [ d ] ] ->
      Alcotest.(check (option string))
        "layer survives" (Some "theme")
        (Declaration.custom_declaration_layer d);
      Alcotest.(check (option string))
        "meta survives" (Some "tag")
        (Option.bind (Declaration.meta_of_declaration d) project)
  | ds ->
      Alcotest.failf "expected one rule holding one declaration, got %d rules"
        (List.length ds)

let custom_property_font_name_quoting_converges () =
  (* CSS Fonts 4 sec. 15.3: a multi-word family name spells the same family
     quoted or as the ident sequence it unquotes to, and a custom property
     holding a font stack substitutes either form identically into
     [font-family]. Emission keeps whichever the author wrote, so only the
     projection can bring the two spellings together. *)
  let quoted =
    {|:root{--font-sans:ui-sans-serif,system-ui,"Noto Color Emoji"}|}
  in
  let unquoted =
    ":root{--font-sans:ui-sans-serif,system-ui,Noto Color Emoji}"
  in
  Alcotest.(check string)
    "the quoted spelling folds onto the ident sequence" (canonical unquoted)
    (canonical quoted);
  (* Gated on a generic family being present: without one the property is
     type-unknown, and a single-word string could be a [<custom-ident>] some
     other substitution context reads as a string. *)
  Alcotest.(check bool)
    "a single-word string stays opaque" true
    (canonical {|.a{--x:"foo"}|} <> canonical ".a{--x:foo}")

let media_and_independent_rule_converge () =
  (* A conditional block whose rules cannot conflict with a neighbouring rule
     reorders with it, so both source orderings reach one canonical form. *)
  let grouped = ".a{display:block}@media (min-width:48rem){.b{flex-grow:1}}" in
  let split = "@media (min-width:48rem){.b{flex-grow:1}}.a{display:block}" in
  Alcotest.(check string)
    "both orders converge" (canonical grouped) (canonical split);
  Alcotest.(check bool)
    "rule sorts before the block" true
    (String.length (canonical split) > 2
    && String.sub (canonical split) 0 2 = ".a")

let media_conflict_keeps_source_order () =
  (* The media rule writes the same property on the same selector, so the
     relative order is cascade-significant and both spellings stay put. *)
  let media_first = "@media print{.a{display:flex}}.a{display:block}" in
  let rule_first = ".a{display:block}@media print{.a{display:flex}}" in
  Alcotest.(check string)
    "media first stays"
    (render (statements media_first))
    (canonical media_first);
  Alcotest.(check string)
    "rule first stays"
    (render (statements rule_first))
    (canonical rule_first)

let layer_blocks_stay_put () =
  (* [@layer] order pins cascade-layer priority at first occurrence; layer
     blocks are barriers. *)
  let css = "@layer b{.y{color:blue}}@layer a{.x{color:red}}" in
  Alcotest.(check string)
    "layer blocks keep source order"
    (render (statements css))
    (canonical css)

let block_with_layer_content_is_barrier () =
  (* A conditional block is only reorderable when its transitive content is
     plain rules; a nested [@layer] pins it in place. *)
  let css = "@media print{@layer a{.x{color:red}}}.b{margin:0}" in
  Alcotest.(check string)
    "media wrapping a layer stays put"
    (render (statements css))
    (canonical css)

let hoisted_group_converges_with_inline () =
  (* A shared declaration hoisted into a selector-list group is the same
     declaration written inline, so both factorings project to one form. *)
  Alcotest.(check string)
    "hoisted and inline forms converge"
    (canonical
       ".absolute{position:absolute}.sr-only{position:absolute;white-space:nowrap}")
    (canonical
       ".absolute,.sr-only{position:absolute}.sr-only{white-space:nowrap}")

let selector_list_expands_without_coalesce () =
  (* Every selector-list rule expands onto its branches, whether or not a branch
     occurs elsewhere. [.a,.b{margin:0}] and [.a{margin:0}.b{margin:0}] are the
     same stylesheet, so they have to project to the same form; leaving a list
     with no shared branch grouped made the projection depend on how the input
     happened to be written. *)
  Alcotest.(check string)
    "a list rule expands onto its branches" ".a{margin:0}.b{margin:0}"
    (canonical ".a,.b{margin:0}")

let selector_list_expands_when_a_branch_is_shared () =
  (* When a branch also appears in another rule, the list is expanded so the
     coalesce can fold that branch; the non-shared branch keeps its
     declarations. *)
  Alcotest.(check string)
    "shared branch expands and coalesces" ".a{margin:0}.b{color:red;margin:0}"
    (canonical ".a,.b{margin:0}.b{color:red}")

let coalesce_blocked_by_intervening_conflict () =
  (* [.b] can match the same element as [.a] at equal specificity and writes the
     same property, so folding the two [.a] occurrences together past it is not
     observable-free: they stay split, in source order. The first [color] is
     dropped all the same - a later rule with the *identical* selector writes
     it, so it never wins for any element. *)
  Alcotest.(check string)
    "conflicting write between occurrences keeps them split"
    ".b{color:blue}.a{color:green}"
    (canonical ".a{color:red}.b{color:blue}.a{color:green}");
  (* what the rule leaves behind still coalesces *)
  Alcotest.(check string)
    "a declaration the later occurrence does not write survives"
    ".b{color:blue}.a{color:green;margin:0}"
    (canonical ".a{color:red;margin:0}.b{color:blue}.a{color:green}");
  (* [!important] changes the winner, so nothing is dead *)
  Alcotest.(check string)
    "an important declaration is never shadowed" ".a{color:red!important}"
    (canonical ".a{color:red!important}.a{color:green}")

let coalesce_drops_exact_duplicates () =
  Alcotest.(check string)
    "duplicate declaration collapses to the later occurrence"
    ".a{color:red;margin:0}"
    (canonical ".a{color:red}.a{color:red;margin:0}")

let coalesce_drops_dead_override () =
  (* When optimize factors a shared default into a group and specialises one
     branch ([.a,.b{color:red}] then [.b{color:blue}]), expanding and coalescing
     [.b] must drop the dead default so the coalesced rule holds the same
     declarations a sheet that folded [.b] directly would - otherwise the two
     project to different declaration sets and the structural comparison
     desynchronises (the tw-parity shadow/scrollbar regression). *)
  Alcotest.(check string)
    "coalesced branch drops the overridden default"
    ".a{color:red}.b{color:blue}"
    (canonical ".a,.b{color:red}.b{color:blue}")

let commuting_declarations_converge () =
  (* Declarations that write disjoint cascade slots sort into one canonical
     order, so two rules holding the same set in a different commuting order
     converge (the .sr-only phantom: every declaration writes a distinct
     property). *)
  let a =
    ".x{position:absolute;clip-path:inset(50%);width:1px;overflow:hidden}"
  in
  let b =
    ".x{width:1px;overflow:hidden;position:absolute;clip-path:inset(50%)}"
  in
  Alcotest.(check string)
    "both commuting orders converge" (canonical a) (canonical b)

let overlapping_declarations_keep_order () =
  (* A shorthand and its longhand write a common slot, so their relative order
     is cascade-significant and the two source orders must not converge. *)
  Alcotest.(check bool)
    "shorthand/longhand order stays distinct" true
    (canonical ".x{margin:0;margin-top:5px}"
    <> canonical ".x{margin-top:5px;margin:0}")

let nested_conditionals_participate () =
  let css =
    "@media print{@supports (display:flex){.z{color:red}}}.b{margin:0}"
  in
  Alcotest.(check bool)
    "independent rule sorts before the nested block" true
    (String.length (canonical css) > 2 && String.sub (canonical css) 0 2 = ".b")

let same_condition_blocks_fold_together () =
  (* Two blocks under one condition write one cascade slot, so the projection
     folds them the way it folds two occurrences of a selector. A sheet whose
     source kept them apart converges with one that wrote them together. *)
  let split =
    "@media print{.a{color:red}}.zzz{order:1}@media print{.b{color:blue}}"
  in
  let together = "@media print{.a{color:red}.b{color:blue}}.zzz{order:1}" in
  Alcotest.(check string)
    "separated blocks fold together" (canonical together) (canonical split);
  Alcotest.(check bool)
    "one block survives" true
    (String.length (canonical split)
     - String.length
         (String.concat "" (String.split_on_char '@' (canonical split)))
    = 1)

let conflicting_block_fold_is_refused () =
  (* Folding either way moves a conditional write of [color] on [.a] past an
     unconditional write of [color] on [.a], which changes the winner when the
     query matches, so both blocks stay. *)
  let css =
    "@media print{.a{color:red}}.a{color:green}@media print{.a{color:blue}}"
  in
  Alcotest.(check string)
    "both blocks stay"
    (render (statements css))
    (canonical css)

let suite =
  ( "rule_order",
    [
      Alcotest.test_case "sorts independent rules" `Quick
        sorts_independent_rules_into_content_order;
      Alcotest.test_case "keeps conflicting order" `Quick
        keeps_conflicting_rules_in_source_order;
      Alcotest.test_case "custom-property position converges" `Quick
        custom_property_position_converges;
      Alcotest.test_case "custom-property importance survives" `Quick
        custom_property_importance_survives;
      Alcotest.test_case "custom-property layer and meta survive" `Quick
        custom_property_layer_and_meta_survive;
      Alcotest.test_case "custom-property font-name quoting converges" `Quick
        custom_property_font_name_quoting_converges;
      Alcotest.test_case "media and independent rule converge" `Quick
        media_and_independent_rule_converge;
      Alcotest.test_case "media conflict keeps source order" `Quick
        media_conflict_keeps_source_order;
      Alcotest.test_case "layer blocks stay put" `Quick layer_blocks_stay_put;
      Alcotest.test_case "hoisted group converges with inline" `Quick
        hoisted_group_converges_with_inline;
      Alcotest.test_case "selector list expands without coalesce" `Quick
        selector_list_expands_without_coalesce;
      Alcotest.test_case "selector list expands when a branch is shared" `Quick
        selector_list_expands_when_a_branch_is_shared;
      Alcotest.test_case "coalesce blocked by intervening conflict" `Quick
        coalesce_blocked_by_intervening_conflict;
      Alcotest.test_case "coalesce drops exact duplicates" `Quick
        coalesce_drops_exact_duplicates;
      Alcotest.test_case "coalesce drops dead override" `Quick
        coalesce_drops_dead_override;
      Alcotest.test_case "commuting declarations converge" `Quick
        commuting_declarations_converge;
      Alcotest.test_case "overlapping declarations keep order" `Quick
        overlapping_declarations_keep_order;
      Alcotest.test_case "block with layer content is barrier" `Quick
        block_with_layer_content_is_barrier;
      Alcotest.test_case "nested conditionals participate" `Quick
        nested_conditionals_participate;
      Alcotest.test_case "same-condition blocks fold together" `Quick
        same_condition_blocks_fold_together;
      Alcotest.test_case "conflicting block fold is refused" `Quick
        conflicting_block_fold_is_refused;
    ] )
