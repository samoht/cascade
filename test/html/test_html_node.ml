(* [Cascade_html.Html_node] is what the resolver walks, so selectors read
   through it see the page's structure. *)

module Html = Cascade_html.Html
module Node = Cascade_html.Html_node

let page =
  Html.parse "<div id=\"d\" class=\"box\"><p>a</p>text<span>b</span></div>"

let root () =
  match Html.roots page with
  | [ d ] -> d
  | _ -> Alcotest.fail "expected one root"

let test_element_facts () =
  let d = root () in
  Alcotest.(check (option string)) "name" (Some "div") (Node.name d);
  Alcotest.(check (option string)) "id" (Some "d") (Node.id d);
  Alcotest.(check (list string)) "classes" [ "box" ] (Node.classes d)

(* Children are the elements alone, in order, and each points back at its
   parent; the text between them is reported apart. *)
let test_structure () =
  let d = root () in
  let children = Node.children d in
  Alcotest.(check (list (option string)))
    "element children" [ Some "p"; Some "span" ]
    (List.map Node.name children);
  Alcotest.(check bool)
    "parent link" true
    (List.for_all
       (fun c ->
         match Node.parent c with Some p -> Node.equal p d | None -> false)
       children);
  Alcotest.(check (list string))
    "text children" [ "text" ] (Node.text_children d)

let suite =
  ( "html_node",
    [
      Alcotest.test_case "element facts" `Quick test_element_facts;
      Alcotest.test_case "structure" `Quick test_structure;
    ] )
