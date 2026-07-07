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
  (* Same-selector rules coalesce (the merged declaration order resolves
     identically), but the write order inside stays cascade-significant: the two
     source orders must not converge. *)
  Alcotest.(check string)
    "same selector and property remains ordered" ".a{color:red;color:blue}"
    (canonical ".a{color:red}.a{color:blue}");
  Alcotest.(check string)
    "opposite source order keeps the opposite winner" ".a{color:blue;color:red}"
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

let selector_list_expands_to_branches () =
  Alcotest.(check string)
    "list rule projects onto its branches" ".a{margin:0}.b{margin:0}"
    (canonical ".a,.b{margin:0}")

let coalesce_blocked_by_intervening_conflict () =
  (* [.b] can match the same element as [.a] at equal specificity and writes the
     same property as both [.a] occurrences, so neither reordering nor folding
     past it is observable-free; the occurrences stay split in source order. *)
  Alcotest.(check string)
    "conflicting write between occurrences keeps them split"
    ".a{color:red}.b{color:blue}.a{color:green}"
    (canonical ".a{color:red}.b{color:blue}.a{color:green}")

let coalesce_drops_exact_duplicates () =
  Alcotest.(check string)
    "duplicate declaration collapses to the later occurrence"
    ".a{color:red;margin:0}"
    (canonical ".a{color:red}.a{color:red;margin:0}")

let nested_conditionals_participate () =
  let css =
    "@media print{@supports (display:flex){.z{color:red}}}.b{margin:0}"
  in
  Alcotest.(check bool)
    "independent rule sorts before the nested block" true
    (String.length (canonical css) > 2 && String.sub (canonical css) 0 2 = ".b")

let suite =
  ( "rule_order",
    [
      Alcotest.test_case "sorts independent rules" `Quick
        sorts_independent_rules_into_content_order;
      Alcotest.test_case "keeps conflicting order" `Quick
        keeps_conflicting_rules_in_source_order;
      Alcotest.test_case "custom-property position converges" `Quick
        custom_property_position_converges;
      Alcotest.test_case "media and independent rule converge" `Quick
        media_and_independent_rule_converge;
      Alcotest.test_case "media conflict keeps source order" `Quick
        media_conflict_keeps_source_order;
      Alcotest.test_case "layer blocks stay put" `Quick layer_blocks_stay_put;
      Alcotest.test_case "hoisted group converges with inline" `Quick
        hoisted_group_converges_with_inline;
      Alcotest.test_case "selector list expands to branches" `Quick
        selector_list_expands_to_branches;
      Alcotest.test_case "coalesce blocked by intervening conflict" `Quick
        coalesce_blocked_by_intervening_conflict;
      Alcotest.test_case "coalesce drops exact duplicates" `Quick
        coalesce_drops_exact_duplicates;
      Alcotest.test_case "block with layer content is barrier" `Quick
        block_with_layer_content_is_barrier;
      Alcotest.test_case "nested conditionals participate" `Quick
        nested_conditionals_participate;
    ] )
