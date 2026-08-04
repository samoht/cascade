open Cascade

(* A minimal in-memory element tree to exercise {!Resolve.Make} without a
   DOM. *)
type tree = {
  tname : string;
  tid : string option;
  tclasses : string list;
  ttext : string list;
  mutable tparent : tree option;
  tchildren : tree list;
}

let elt ?id ?(classes = []) ?(text = []) name children =
  let t =
    {
      tname = name;
      tid = id;
      tclasses = classes;
      ttext = text;
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
  let text_children t = t.ttext
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

let match_result =
  Alcotest.testable
    (fun ppf -> function
      | Resolve.Matches -> Fmt.string ppf "Matches"
      | Resolve.No_match -> Fmt.string ppf "No_match"
      | Resolve.Unsupported -> Fmt.string ppf "Unsupported")
    ( = )

let answers name expected s n =
  Alcotest.check match_result name expected (R.match_selector (sel s) n)

(* A selector the matcher has no model for is not a selector that fails to
   match: the caller has to be able to tell the two apart, or it drops a rule it
   only failed to understand. *)
let test_unsupported_is_not_no_match () =
  answers "a modelled hit" Resolve.Matches "span" s2;
  answers "a modelled miss" Resolve.No_match "div" s2;
  answers "stateful" Resolve.Unsupported "span:hover" s2;
  answers "pseudo-element" Resolve.Unsupported "span::before" s2;
  answers "attribute case flag" Resolve.Unsupported "[id=\"S2\" i]" s2;
  answers "namespaced type" Resolve.Unsupported "*|span" s2;
  answers "shadow-piercing combinator" Resolve.Unsupported "div>>>span" s2;
  (* One unsupported part carries the whole selector, whichever side of the
     compound, list or negation it sits on, and whether or not the rest
     matches. *)
  answers "compound" Resolve.Unsupported "span:hover" s2;
  answers "list" Resolve.Unsupported "span,div:hover" s2;
  answers "negation" Resolve.Unsupported ":not(:hover)" s2;
  answers "left of a combinator" Resolve.Unsupported "div:hover span" s2;
  answers "left of a combinator that missed" Resolve.Unsupported "p:hover span"
    s2

(* {!Resolve.supported} is the same answer without a node, which is what lets
   {!Apply} decide whether a rule may leave the stylesheet. *)
let test_supported_needs_no_node () =
  let check name expected s =
    Alcotest.(check bool) name expected (Resolve.supported (sel s))
  in
  check "class" true ".c";
  check "attribute" true "[data-k=\"X\"]";
  check "structural" true "p:empty";
  check "descendant" true "div span";
  check "attribute case flag" false "[data-k=\"X\" i]";
  check "namespaced attribute" false "[svg|href]";
  check "shadow-piercing combinator" false "div>>>span";
  check "stateful" false ":hover";
  check "one bad branch spoils the list" false "span,div:hover"

(* selectors-4 sec. 13.2: [:empty] is "an element that has no children except,
   optionally, document white space characters". White space alone leaves an
   element empty, any other text does not, and U+00A0 is not document white
   space (css-text-4 sec. 4.3) - the spec lists [<div>&nbsp;</div>] among the
   elements [div:empty] does not represent. *)
let test_empty_counts_text_children () =
  yes "no children at all" ":empty" (elt "p" []);
  yes "spaces, tabs and newlines" ":empty" (elt ~text:[ " \t\n" ] "p" []);
  no "a text child" ":empty" (elt ~text:[ "text" ] "p" []);
  no "a text child beside white space" ":empty"
    (elt ~text:[ " "; "text" ] "p" []);
  no "a no-break space" ":empty" (elt ~text:[ "\u{00a0}" ] "p" []);
  no "an element child" ":empty" (elt "p" [ elt "span" [] ])

let sheet_of css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse: %s" (Error.to_string e)

let test_resolve_cascade () =
  let sheet =
    sheet_of "span{color:red}#s2{color:#0f0}div+div>span{font-weight:700}"
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

let resolved_color css =
  List.find_map
    (fun d ->
      if Declaration.property_name d = "color" then
        Some (Declaration.string_of_declaration ~minify:true d)
      else None)
    (R.resolve (sheet_of css) s2)

(* Layer names form a tree (css-cascade-5 sec. 6.4.2): [@layer a.b] creates [a]
   first and nests [b] inside it, and a later [@layer a.d] is ordered inside [a]
   too, so it stays before a top-level layer declared in between. *)
let test_resolve_nested_layers () =
  Alcotest.(check (option string))
    "a sublayer comes after its parent" (Some "color:red")
    (resolved_color "@layer a.b{span{color:red}}@layer a{span{color:blue}}");
  Alcotest.(check (option string))
    "a top-level layer sorts after the whole subtree" (Some "color:green")
    (resolved_color
       "@layer a.b{span{color:red}}@layer c{span{color:green}}@layer \
        a.d{span{color:blue}}")

(* Each unnamed [@layer { }] block is a layer of its own, so two of them order
   like any other pair: the last wins for normal declarations, the first for
   important ones. *)
let test_resolve_anonymous_layers () =
  Alcotest.(check (option string))
    "the last anonymous layer wins" (Some "color:blue")
    (resolved_color "@layer{span{color:red}}@layer{span{color:blue}}");
  Alcotest.(check (option string))
    "the first wins for important" (Some "color:red!important")
    (resolved_color
       "@layer{span{color:red!important}}@layer{span{color:blue!important}}")

(* {!Apply.Make} reuses the same {!Node} adapter: a static rule projects onto
   the element, a rule with no inline form ([:hover]) stays in a <style>
   block. *)
let test_apply_compute () =
  let module A = Apply.Make (Node) in
  let sheet =
    match Css.of_string "#s2{font-weight:700}#s2:hover{color:#00f}" with
    | Ok p -> p.Css.stylesheet
    | Error e -> Alcotest.failf "fixture did not parse: %s" (Error.to_string e)
  in
  let result : tree Apply.result = A.compute ~sheet [ section ] in
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
      Alcotest.test_case "unsupported is not no-match" `Quick
        test_unsupported_is_not_no_match;
      Alcotest.test_case "supported needs no node" `Quick
        test_supported_needs_no_node;
      Alcotest.test_case "empty counts text children" `Quick
        test_empty_counts_text_children;
      Alcotest.test_case "resolve applies the cascade" `Quick
        test_resolve_cascade;
      Alcotest.test_case "nested layer names order as a tree" `Quick
        test_resolve_nested_layers;
      Alcotest.test_case "anonymous layers are distinct" `Quick
        test_resolve_anonymous_layers;
      Alcotest.test_case "apply projects a static rule, keeps a dynamic one"
        `Quick test_apply_compute;
    ] )
