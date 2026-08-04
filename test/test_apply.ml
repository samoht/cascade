open Cascade

type node = {
  name : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  mutable parent : node option;
  children : node list;
}

module Node = struct
  type t = node

  let equal = ( == )
  let name n = Some n.name
  let id n = n.id
  let classes n = n.classes
  let attribute n key = List.assoc_opt key n.attrs
  let parent n = n.parent
  let children n = n.children
  let text_children _ = []
end

module A = Apply.Make (Node)

let node ?id ?(classes = []) ?(attrs = []) ?(children = []) name =
  let n = { name; id; classes; attrs; parent = None; children } in
  List.iter (fun c -> c.parent <- Some n) children;
  n

let inline_style decls =
  Stylesheet.inline_style_of_declarations ~minify:true ~mode:Variables decls

(* [A.compute] takes a parsed sheet, so the fixtures parse here. They are all
   meant to parse; one that does not is a broken test, not a case under test. *)
let parse css =
  match Css.of_string css with
  | Ok p -> p.Css.stylesheet
  | Error e -> Alcotest.failf "fixture did not parse: %s" (Error.to_string e)

let projects_static_rule_to_inline_style () =
  let n = node ~classes:[ "card" ] "div" in
  let result = A.compute ~sheet:(parse ".card{color:red;margin:0}") [ n ] in
  match result.styles with
  | [ (node, decls) ] ->
      Alcotest.(check bool) "same node" true (Node.equal node n);
      Alcotest.(check string)
        "inline declarations" "color:red;margin:0" (inline_style decls);
      Alcotest.(check string) "no kept css" "" result.keep_css;
      Alcotest.(check int) "kept count" 0 result.kept
  | _ -> Alcotest.fail "expected one inline assignment"

let keeps_stateful_rule_in_css () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute ~sheet:(parse ".card{margin:0}.card:hover{color:blue}") [ n ]
  in
  Alcotest.(check string)
    "inline static rule" "margin:0"
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string)
    "kept dynamic rule" ".card:hover{color:blue}" result.keep_css;
  Alcotest.(check int) "kept count" 1 result.kept

(* A @layer block applies unconditionally, so its rules project onto elements
   just like top-level ones instead of being kept wholesale in a <style>. *)
let projects_layered_rule_to_inline_style () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute ~sheet:(parse "@layer u{.card{color:red;margin:0}}") [ n ]
  in
  match result.styles with
  | [ (node, decls) ] ->
      Alcotest.(check bool) "same node" true (Node.equal node n);
      Alcotest.(check string)
        "inline declarations" "color:red;margin:0" (inline_style decls);
      Alcotest.(check string) "no kept css" "" result.keep_css;
      Alcotest.(check int) "kept count" 0 result.kept
  | _ -> Alcotest.fail "expected one inline assignment"

(* Inside a layer the static/dynamic split still holds: the plain rule inlines,
   the stateful one stays in the kept <style>, and it stays in its layer because
   that is what orders it against the other kept rules. *)
let keeps_stateful_rule_inside_layer_in_css () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute
      ~sheet:(parse "@layer u{.card{margin:0}.card:hover{color:blue}}")
      [ n ]
  in
  Alcotest.(check string)
    "inline static rule" "margin:0"
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string)
    "kept dynamic rule" "@layer u{.card:hover{color:blue}}" result.keep_css;
  Alcotest.(check int) "kept count" 1 result.kept

(* The inline style [css] projects onto a single [p.x], as a minified
   declaration list. *)
let projected css =
  let n = node ~classes:[ "x" ] "p" in
  let result = A.compute ~sheet:(parse css) [ n ] in
  match result.styles with
  | [ (_, decls) ] -> inline_style decls
  | _ -> Alcotest.fail "expected one inline assignment"

(* css-cascade-5 sec. 6.4.4: among normal declarations an unlayered one beats
   every layer, whatever the source order. *)
let unlayered_beats_layer () =
  Alcotest.(check string)
    "unlayered beats the layer" "color:red"
    (projected ".x{color:red}@layer u{.x{color:blue}}");
  Alcotest.(check string)
    "and still beats it when the layer comes last" "color:red"
    (projected "@layer u{.x{color:blue}}.x{color:red}")

(* The criterion reverses for important declarations: a layered important beats
   an unlayered one, again whatever the source order. *)
let layer_beats_unlayered_important () =
  Alcotest.(check string)
    "layered important wins" "color:blue!important"
    (projected ".x{color:red!important}@layer u{.x{color:blue!important}}");
  Alcotest.(check string)
    "and still wins when it comes first" "color:blue!important"
    (projected "@layer u{.x{color:blue!important}}.x{color:red!important}")

(* Between two layers the last declared wins for normal declarations and the
   first for important ones. *)
let later_layer_wins_normal_earlier_wins_important () =
  Alcotest.(check string)
    "later layer wins" "color:blue"
    (projected "@layer a{.x{color:red}}@layer b{.x{color:blue}}");
  Alcotest.(check string)
    "earlier layer wins for important" "color:red!important"
    (projected
       "@layer a{.x{color:red!important}}@layer b{.x{color:blue!important}}")

(* An [@layer b, a;] statement fixes the order before any block appears, so the
   blocks that follow do not order themselves. *)
let layer_statement_orders_the_blocks () =
  Alcotest.(check string)
    "the statement decides which layer is last" "color:red"
    (projected "@layer b,a;@layer a{.x{color:red}}@layer b{.x{color:blue}}");
  Alcotest.(check string)
    "and the important order follows it" "color:blue!important"
    (projected
       "@layer b,a;@layer a{.x{color:red!important}}@layer \
        b{.x{color:blue!important}}")

(* Layers outrank specificity in the cascade sorting order (css-cascade-5 sec.
   6), so the plain class in the later layer beats the compound selector in the
   earlier one. *)
let layer_outranks_specificity () =
  Alcotest.(check string)
    "later layer beats higher specificity" "color:blue"
    (projected "@layer a{p.x{color:red}}@layer b{.x{color:blue}}")

(* A control: layers that set different properties do not compete, so both
   declarations project as they did before. *)
let non_competing_layers_both_project () =
  Alcotest.(check string)
    "both layers contribute" "color:red;margin:0"
    (projected "@layer a{.x{color:red}}@layer b{.x{margin:0}}")

(* The whole split of [css] over [roots]: the style attribute written onto [n],
   the <style> body kept beside it, and the count of kept rules. *)
let check_split name ?roots ~css ~inline ~keep ~kept n =
  let result =
    A.compute ~sheet:(parse css) (Option.value roots ~default:[ n ])
  in
  let decls =
    match List.find_opt (fun (m, _) -> Node.equal m n) result.styles with
    | Some (_, decls) -> decls
    | None -> Alcotest.failf "%s: no assignment for the node" name
  in
  Alcotest.(check string) (name ^ ": inline style") inline (inline_style decls);
  Alcotest.(check string) (name ^ ": kept css") keep result.keep_css;
  Alcotest.(check int) (name ^ ": kept rules") kept result.kept

(* A [@scope] block gates its rules on where the element sits in the tree, so it
   has no inline form and stays in the sheet. The properties it sets therefore
   count as dynamic: moving the competing declaration into the style attribute
   would place it above the kept rule, which is not where the author put it. *)
let keeps_property_a_scoped_rule_sets () =
  check_split "scoped" ~css:".x{color:red}@scope(.root){.x{color:blue}}"
    ~inline:"" ~keep:".x{color:red}@scope(.root){.x{color:blue}}" ~kept:2
    (node ~classes:[ "x" ] "p")

(* Same for [@starting-style]: its declarations apply for the frame in which the
   element is inserted, which no style attribute can express. *)
let keeps_property_a_starting_style_rule_sets () =
  check_split "starting-style"
    ~css:".x{color:red}@starting-style{.x{color:blue}}" ~inline:""
    ~keep:".x{color:red}@starting-style{.x{color:blue}}" ~kept:2
    (node ~classes:[ "x" ] "p")

(* A control for both: a conditional block that sets another property competes
   with nothing, so the split is unchanged. *)
let non_competing_scoped_rule_still_inlines () =
  check_split "scoped, other property"
    ~css:".x{color:red}@scope(.root){.x{margin:0}}" ~inline:"color:red"
    ~keep:"@scope(.root){.x{margin:0}}" ~kept:1
    (node ~classes:[ "x" ] "p")

(* The matcher compares attribute values verbatim, so it cannot represent the
   [i] case flag. A selector it cannot represent has to stay in the sheet:
   inlining it drops it from the sheet, and it then matches nobody, so the
   declaration is lost. *)
let keeps_rule_with_attribute_case_flag () =
  check_split "case-insensitive attribute" ~css:"p[data-k=\"X\" i]{color:red}"
    ~inline:"" ~keep:"p[data-k=X i]{color:red}" ~kept:1
    (node ~attrs:[ ("data-k", "x") ] "p")

(* The control: without the flag the selector is representable, so it projects
   onto the element and leaves nothing behind. *)
let projects_attribute_rule_to_inline_style () =
  check_split "attribute" ~css:"p[data-k=\"X\"]{color:red}" ~inline:"color:red"
    ~keep:"" ~kept:0
    (node ~attrs:[ ("data-k", "X") ] "p")

(* Namespaces are not modelled either, so the matcher would answer for [svg|p]
   what it answers for [p] and paint an HTML paragraph. *)
let keeps_rule_with_namespaced_element () =
  check_split "namespaced element"
    ~css:"@namespace svg url(http://www.w3.org/2000/svg);svg|p{color:red}"
    ~inline:""
      (* The minified printer writes the namespace URL as a string, and the
         [@namespace] statement counts towards the kept statements. *)
    ~keep:"@namespace svg\"http://www.w3.org/2000/svg\";svg|p{color:red}"
    ~kept:2 (node "p")

(* [>>>] is a legacy shadow-piercing combinator with no tree relation behind it,
   so the matcher has no answer for it and the rule stays in the sheet. *)
let keeps_rule_with_shadow_piercing_combinator () =
  let span = node "span" in
  let root = node ~children:[ node ~children:[ span ] "div" ] "section" in
  check_split "shadow-piercing" ~roots:[ root ] ~css:"div>>>span{color:red}"
    ~inline:"" ~keep:"div>>>span{color:red}" ~kept:1 span

(* The control: the descendant combinator is modelled, so the same shape of rule
   projects onto the span. *)
let projects_descendant_combinator_rule () =
  let span = node "span" in
  let root = node ~children:[ node ~children:[ span ] "div" ] "section" in
  check_split "descendant" ~roots:[ root ] ~css:"div span{color:red}"
    ~inline:"color:red" ~keep:"" ~kept:0 span

(* [kept] is reported to the user as a number of rules, so a grouping at-rule
   contributes the rules inside it rather than counting once for its wrapper: a
   @media block holding three rules keeps three rules out of the inline
   projection. *)
let kept_counts_the_rules_a_media_block_holds () =
  let n = node ~classes:[ "a" ] "p" in
  let result =
    A.compute
      ~sheet:
        (parse
           "@media(min-width:10px){.a{color:red}.b{color:blue}.c{color:green}}")
      [ n ]
  in
  Alcotest.(check int) "kept rules" 3 result.kept

(* An at-rule that holds no rules of its own is itself the one thing kept, so it
   counts once. *)
let kept_counts_a_rule_less_at_rule_once () =
  let n = node ~classes:[ "a" ] "p" in
  let result =
    A.compute ~sheet:(parse "@font-face{font-family:x;src:url(a.woff2)}") [ n ]
  in
  Alcotest.(check int) "kept rules" 1 result.kept

(* A stylesheet the parser had to recover and an empty one project onto exactly
   the same nothing, so the projection cannot be what tells them apart. Taking a
   parsed sheet leaves that to {!Css.of_string}, whose warnings the caller reads
   before any of this runs; parsing the CSS text here would swallow them and
   report both as the same empty result. ["a{"] is neither case: CSS Syntax 3
   sec. 5.4 closes the block at EOF, so it is a valid rule with no
   declarations. *)
let invalid_css_is_not_empty_css () =
  let warnings css =
    match Css.of_string css with
    | Ok p -> p.Css.warnings
    | Error e ->
        Alcotest.failf "unexpected fatal parse error: %s" (Error.to_string e)
  in
  Alcotest.(check bool)
    "recovered CSS carries diagnostics" true
    (warnings "@@@@ }}} {{{ !!! ;;;" <> []);
  Alcotest.(check bool) "empty CSS carries none" true (warnings "" = []);
  Alcotest.(check bool)
    "a block closed at EOF carries none either" true
    (warnings "a{" = []);
  List.iter
    (fun css ->
      let result = A.compute ~sheet:(parse css) [ node "div" ] in
      Alcotest.(check string)
        ("inline declarations: " ^ css)
        ""
        (match result.styles with
        | [ (_, decls) ] -> inline_style decls
        | _ -> Alcotest.fail "expected one inline assignment");
      Alcotest.(check string) ("kept css: " ^ css) "" result.keep_css;
      Alcotest.(check int) ("kept count: " ^ css) 0 result.kept)
    [ "@@@@ }}} {{{ !!! ;;;"; ""; "a{" ]

let suite =
  ( "apply",
    [
      Alcotest.test_case "projects static rule to inline style" `Quick
        projects_static_rule_to_inline_style;
      Alcotest.test_case "keeps stateful rule in css" `Quick
        keeps_stateful_rule_in_css;
      Alcotest.test_case "projects layered rule to inline style" `Quick
        projects_layered_rule_to_inline_style;
      Alcotest.test_case "keeps stateful rule inside layer in css" `Quick
        keeps_stateful_rule_inside_layer_in_css;
      Alcotest.test_case "unlayered beats layer" `Quick unlayered_beats_layer;
      Alcotest.test_case "layer beats unlayered for important" `Quick
        layer_beats_unlayered_important;
      Alcotest.test_case "later layer wins normal, earlier wins important"
        `Quick later_layer_wins_normal_earlier_wins_important;
      Alcotest.test_case "layer statement orders the blocks" `Quick
        layer_statement_orders_the_blocks;
      Alcotest.test_case "layer outranks specificity" `Quick
        layer_outranks_specificity;
      Alcotest.test_case "non-competing layers both project" `Quick
        non_competing_layers_both_project;
      Alcotest.test_case "keeps the property a scoped rule sets" `Quick
        keeps_property_a_scoped_rule_sets;
      Alcotest.test_case "keeps the property a starting-style rule sets" `Quick
        keeps_property_a_starting_style_rule_sets;
      Alcotest.test_case "non-competing scoped rule still inlines" `Quick
        non_competing_scoped_rule_still_inlines;
      Alcotest.test_case "keeps a rule with an attribute case flag" `Quick
        keeps_rule_with_attribute_case_flag;
      Alcotest.test_case "projects an attribute rule to inline style" `Quick
        projects_attribute_rule_to_inline_style;
      Alcotest.test_case "keeps a rule with a namespaced element" `Quick
        keeps_rule_with_namespaced_element;
      Alcotest.test_case "keeps a rule with a shadow-piercing combinator" `Quick
        keeps_rule_with_shadow_piercing_combinator;
      Alcotest.test_case "projects a descendant combinator rule" `Quick
        projects_descendant_combinator_rule;
      Alcotest.test_case "kept counts the rules a media block holds" `Quick
        kept_counts_the_rules_a_media_block_holds;
      Alcotest.test_case "kept counts a rule-less at-rule once" `Quick
        kept_counts_a_rule_less_at_rule_once;
      Alcotest.test_case "invalid css is not empty css" `Quick
        invalid_css_is_not_empty_css;
    ] )
