open Cascade

(* A minimal in-memory element tree to exercise {!Resolve.Make} without a
   DOM. *)
type tree = {
  tname : string;
  tid : string option;
  tclasses : string list;
  mutable tparent : tree option;
  tchildren : tree list;
}

let elt ?id ?(classes = []) name children =
  let t =
    {
      tname = name;
      tid = id;
      tclasses = classes;
      tparent = None;
      tchildren = children;
    }
  in
  List.iter (fun c -> c.tparent <- Some t) children;
  t

module Node = struct
  type t = tree

  let equal = ( == )
  let name t = Some t.tname
  let id t = t.tid
  let classes t = t.tclasses
  let attribute _ _ = None
  let parent t = t.tparent
  let children t = t.tchildren
end

module R = Resolve.Make (Node)

let sel = Selector.of_string

(* section div (#a) span (#s1) div (#b) span (#s2) *)
let s1 = elt ~id:"s1" "span" []
let s2 = elt ~id:"s2" "span" []
let a = elt ~id:"a" "div" [ s1 ]
let b = elt ~id:"b" "div" [ s2 ]
let section = elt "section" [ a; b ]
let yes name s n = Alcotest.(check bool) name true (R.matches (sel s) n)
let no name s n = Alcotest.(check bool) name false (R.matches (sel s) n)

let test_simple () =
  yes "element" "span" s1;
  yes "id" "#s2" s2;
  no "wrong id" "#s1" s2;
  yes "compound element+id" "div#a" a;
  no "compound mismatch" "div#b" a

let test_single_combinator () =
  yes "descendant" "section span" s2;
  yes "child" "div>span" s2;
  no "child needs direct parent" "section>span" s2;
  yes "next-sibling on div" "div+div" b;
  no "next-sibling: first has no predecessor" "div+div" a;
  yes "subsequent-sibling" "div~div" b

(* The regression: a sibling combinator followed by a descendant/child
   combinator. The subject ([span]) sits past two combinators, so the matcher
   must thread the anchor: [div+div] applies between the two divs, not at the
   span. *)
let test_sibling_then_descendant () =
  yes "sibling then child" "div+div>span" s2;
  no "first branch has no preceding sibling" "div+div>span" s1;
  yes "sibling then descendant" "div+div span" s2;
  no "descendant of first div" "div+div span" s1;
  yes "subsequent-sibling then child" "div~div>span" s2

let test_resolve_cascade () =
  let sheet =
    match
      Css.of_string
        "span{color:red}#s2{color:#0f0}div+div>span{font-weight:700}"
    with
    | Ok { stylesheet; _ } -> stylesheet
    | Error e -> Alcotest.failf "parse: %s" (Error.to_string e)
  in
  let decls = R.resolve sheet s2 in
  let value p =
    List.find_map
      (fun d ->
        if Declaration.property_name d = p then
          Some (Declaration.string_of_declaration ~minify:true d)
        else None)
      decls
  in
  (* higher specificity #s2 beats the element rule *)
  Alcotest.(check (option string))
    "id wins over element" (Some "color:#0f0") (value "color");
  (* the sibling+child rule that the old matcher missed now contributes *)
  Alcotest.(check (option string))
    "sibling-combined rule applies" (Some "font-weight:700")
    (value "font-weight")

(* {!Apply.Make} reuses the same {!Node} adapter: a static rule projects onto
   the element, a rule with no inline form ([:hover]) stays in a <style>
   block. *)
let test_apply_compute () =
  let module A = Apply.Make (Node) in
  let result : tree Apply.result =
    A.compute ~css:"#s2{font-weight:700}#s2:hover{color:#00f}" [ section ]
  in
  let s2_decls =
    List.find_map
      (fun (n, decls) -> if Node.equal n s2 then Some decls else None)
      result.styles
    |> Option.value ~default:[]
  in
  Alcotest.(check bool)
    "static rule projected onto s2" true
    (List.exists
       (fun d -> Declaration.property_name d = "font-weight")
       s2_decls);
  Alcotest.(check bool)
    "dynamic property not projected" false
    (List.exists (fun d -> Declaration.property_name d = "color") s2_decls);
  Alcotest.(check int) "the :hover rule is kept in a <style>" 1 result.kept;
  Alcotest.(check bool) "kept css is non-empty" true (result.keep_css <> "")

let suite =
  ( "resolve",
    [
      Alcotest.test_case "simple selectors" `Quick test_simple;
      Alcotest.test_case "single combinator" `Quick test_single_combinator;
      Alcotest.test_case "sibling then descendant/child" `Quick
        test_sibling_then_descendant;
      Alcotest.test_case "resolve applies the cascade" `Quick
        test_resolve_cascade;
      Alcotest.test_case "apply projects a static rule, keeps a dynamic one"
        `Quick test_apply_compute;
    ] )
