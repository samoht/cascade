open Cascade

type node = {
  name : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  children : node list;
}

module Node = struct
  type t = node

  let equal = ( == )
  let name n = Some n.name
  let id n = n.id
  let classes n = n.classes
  let attribute n key = List.assoc_opt key n.attrs
  let parent _ = None
  let children n = n.children
end

module A = Apply.Make (Node)

let node ?id ?(classes = []) ?(attrs = []) ?(children = []) name =
  { name; id; classes; attrs; children }

let inline_style decls =
  Stylesheet.inline_style_of_declarations ~minify:true ~mode:Variables decls

let projects_static_rule_to_inline_style () =
  let n = node ~classes:[ "card" ] "div" in
  let result = A.compute ~css:".card{color:red;margin:0}" [ n ] in
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
  let result = A.compute ~css:".card{margin:0}.card:hover{color:blue}" [ n ] in
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
  let result = A.compute ~css:"@layer u{.card{color:red;margin:0}}" [ n ] in
  match result.styles with
  | [ (node, decls) ] ->
      Alcotest.(check bool) "same node" true (Node.equal node n);
      Alcotest.(check string)
        "inline declarations" "color:red;margin:0" (inline_style decls);
      Alcotest.(check string) "no kept css" "" result.keep_css;
      Alcotest.(check int) "kept count" 0 result.kept
  | _ -> Alcotest.fail "expected one inline assignment"

(* Inside a layer the static/dynamic split still holds: the plain rule inlines,
   the stateful one stays in the kept <style>. *)
let keeps_stateful_rule_inside_layer_in_css () =
  let n = node ~classes:[ "card" ] "div" in
  let result =
    A.compute ~css:"@layer u{.card{margin:0}.card:hover{color:blue}}" [ n ]
  in
  Alcotest.(check string)
    "inline static rule" "margin:0"
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string)
    "kept dynamic rule" ".card:hover{color:blue}" result.keep_css;
  Alcotest.(check int) "kept count" 1 result.kept

(* The inline style [css] projects onto a single [p.x], as a minified
   declaration list. *)
let projected css =
  let n = node ~classes:[ "x" ] "p" in
  let result = A.compute ~css [ n ] in
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

let invalid_css_is_empty () =
  let result = A.compute ~css:"a{" [ node "div" ] in
  Alcotest.(check string)
    "inline declarations" ""
    (match result.styles with
    | [ (_, decls) ] -> inline_style decls
    | _ -> Alcotest.fail "expected one inline assignment");
  Alcotest.(check string) "kept css" "" result.keep_css;
  Alcotest.(check int) "kept count" 0 result.kept

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
      Alcotest.test_case "invalid css is empty" `Quick invalid_css_is_empty;
    ] )
