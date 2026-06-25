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
      Alcotest.test_case "invalid css is empty" `Quick invalid_css_is_empty;
    ] )
