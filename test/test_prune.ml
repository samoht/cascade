open Cascade

type node = {
  name : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  texts : string list;
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

  (* Only text counts, as {!Resolve.NODE} states: a comment is not a text node,
     so an element holding one alone is still [:empty]. The fixtures below give
     [texts] and children apart for exactly that reason. *)
  let text_children n = n.texts
end

module P = Prune.Make (Node)

let node ?id ?(classes = []) ?(attrs = []) ?(texts = []) ?(children = []) name =
  let n = { name; id; classes; attrs; texts; parent = None; children } in
  List.iter (fun c -> c.parent <- Some n) children;
  n

(* The fixtures are all meant to parse; one that does not is a broken test, not
   a case under test. *)
let parse css =
  match Css.of_string css with
  | Ok p -> p.Css.stylesheet
  | Error e -> Alcotest.failf "fixture did not parse: %s" (Error.to_string e)

let verdict =
  let pp ppf = function
    | Prune.Unused -> Fmt.string ppf "unused"
    | Prune.Used n -> Fmt.pf ppf "used %d" n
    | Prune.Unmodelled -> Fmt.string ppf "unmodelled"
  in
  let equal a b =
    match (a, b) with
    | Prune.Unused, Prune.Unused | Prune.Unmodelled, Prune.Unmodelled -> true
    | Prune.Used a, Prune.Used b -> Int.equal a b
    | (Prune.Unused | Prune.Used _ | Prune.Unmodelled), _ -> false
  in
  Alcotest.testable pp equal

(* A page with one [.card] holding a paragraph, plus a bare paragraph. *)
let page () =
  node "html"
    ~children:
      [
        node "body"
          ~children:
            [
              node "div" ~classes:[ "card" ]
                ~children:[ node "p" ~texts:[ "one" ] ];
              node "p" ~texts:[ "two" ];
            ];
      ]

let analyse css = P.analyse ~sheet:(parse css) [ page () ]
let css result = Css.to_string ~minify:true result.Prune.sheet
let verdicts result = List.map (fun e -> e.Prune.verdict) result.Prune.entries

let check_verdicts msg expected result =
  Alcotest.(check (list verdict)) msg expected (verdicts result)

let removes_a_rule_no_element_matches () =
  let r = analyse ".card{color:red}.gone{color:blue}" in
  check_verdicts "verdicts" [ Prune.Used 1; Prune.Unused ] r;
  Alcotest.(check string) "sheet" ".card{color:red}" (css r)

let counts_every_matched_element () =
  let r = analyse "p{margin:0}" in
  check_verdicts "verdicts" [ Prune.Used 2 ] r;
  Alcotest.(check int) "elements" 5 r.Prune.elements

(* [Unsupported] is not a No_match: the matcher has no model for [:hover], so no
   element ruled the rule out and it stays. *)
let keeps_a_selector_the_matcher_cannot_decide () =
  let r = analyse ".gone:hover{color:red}" in
  check_verdicts "verdicts" [ Prune.Unmodelled ] r;
  Alcotest.(check string) "sheet" ".gone:hover{color:red}" (css r)

(* Selectors 4 sec. 8.1: [:root] is the document's root element. *)
let root_matches_the_root_element () =
  let r = analyse ":root{--brand:red}" in
  check_verdicts "verdicts" [ Prune.Used 1 ] r;
  Alcotest.(check string) "sheet" ":root{--brand:red}" (css r)

(* Selectors 4 sec. 13.2 counts element nodes and non-empty text nodes, and
   matches an element holding nothing but document white space - the Level 4
   change no engine has taken. Pruning on that reading would delete a rule the
   browser still applies, so the matcher declines it and the rule stays without
   a verdict. *)
let keeps_empty_for_want_of_a_shipped_model () =
  let tree =
    node "html"
      ~children:
        [
          node "div" ~children:[];
          node "div" ~texts:[ "x" ];
          node "div" ~children:[ node "span" ];
        ]
  in
  let r = P.analyse ~sheet:(parse "div:empty{color:red}") [ tree ] in
  check_verdicts "verdicts" [ Prune.Unmodelled ] r;
  Alcotest.(check string) "sheet" "div:empty{color:red}" (css r)

(* A condition is a question about the device, not about the document, so the
   rules inside a group block are judged by their own selectors. *)
let a_condition_is_not_a_selector () =
  let r = analyse "@media print{.card{color:red}}" in
  check_verdicts "verdicts" [ Prune.Used 1 ] r;
  Alcotest.(check string) "sheet" "@media print{.card{color:red}}" (css r)

let drops_a_group_block_left_with_nothing () =
  let r = analyse "@supports (display:grid){.gone{display:grid}}p{margin:0}" in
  check_verdicts "verdicts" [ Prune.Unused; Prune.Used 2 ] r;
  Alcotest.(check string) "sheet" "p{margin:0}" (css r)

(* A layer left with no rule still declares the layer the cascade orders by
   (css-cascade-5 sec. 6.4.4.2), so it survives its rules; minified, the
   statement form is how that declaration is spelled. *)
let keeps_a_layer_block_left_with_nothing () =
  let r = analyse "@layer base{.gone{color:red}}" in
  check_verdicts "verdicts" [ Prune.Unused ] r;
  Alcotest.(check string) "sheet" "@layer base;" (css r)

let keeps_a_statement_with_no_selector () =
  let source =
    "@font-face{font-family:Brand;src:url(b.woff2)}@keyframes \
     fade{0%{opacity:0}}@property \
     --gap{syntax:\"<length>\";inherits:false;initial-value:1rem}"
  in
  let r = analyse source in
  check_verdicts "no rule was judged" [] r;
  Alcotest.(check string) "sheet" source (css r)

(* A list is only as modelled as its least modelled branch, so one unmodelled
   branch leaves the whole rule without a verdict. Dropping [.gone] here would
   rest on a No_match the matcher never gave for the list it belongs to. *)
let keeps_a_list_holding_an_unmodelled_branch () =
  let r = analyse ".gone,.card:hover{color:red}" in
  check_verdicts "verdicts" [ Prune.Unmodelled ] r;
  Alcotest.(check string) "sheet" ".gone,.card:hover{color:red}" (css r)

(* Each branch of a modelled list matches on its own and carries its own
   specificity (Selectors 4 sec. 3.3, sec. 17), so the unused one goes. *)
let drops_the_unused_branch_of_a_modelled_list () =
  let r = analyse ".card,.gone{color:red}" in
  check_verdicts "verdicts" [ Prune.Used 1 ] r;
  Alcotest.(check string) "sheet" ".card{color:red}" (css r)

(* A custom property reaches an element through a rule that matched it, or by
   inheritance from an ancestor a rule matched. [.gone] matched nothing, so
   nothing here ever carried [--bg] and [var(--bg)] already resolved to its
   guaranteed-invalid value. *)
let removes_an_unused_rule_that_declares_a_custom_property () =
  let r = analyse ".gone{--bg:black}p{background:var(--bg)}" in
  check_verdicts "verdicts" [ Prune.Unused; Prune.Used 2 ] r;
  Alcotest.(check string) "sheet" "p{background:var(--bg)}" (css r)

(* Nesting is flattened first, so the nested rule is judged on the selector it
   resolves to rather than on the [&] it was written with. *)
let judges_a_nested_rule_on_its_flattened_selector () =
  let r = analyse ".card{color:red;& span{color:blue}}" in
  check_verdicts "verdicts" [ Prune.Used 1; Prune.Unused ] r;
  Alcotest.(check string) "sheet" ".card{color:red}" (css r)

(* Documents with no element between them make every rule look unused. The count
   is what tells a caller that is an answer about the input. *)
let no_element_leaves_every_rule_unused () =
  let r = P.analyse ~sheet:(parse ".card{color:red}") [] in
  check_verdicts "verdicts" [ Prune.Unused ] r;
  Alcotest.(check int) "elements" 0 r.Prune.elements

let suite =
  ( "prune",
    [
      Alcotest.test_case "removes a rule no element matches" `Quick
        removes_a_rule_no_element_matches;
      Alcotest.test_case "counts every matched element" `Quick
        counts_every_matched_element;
      Alcotest.test_case "keeps a selector the matcher cannot decide" `Quick
        keeps_a_selector_the_matcher_cannot_decide;
      Alcotest.test_case "root matches the root element" `Quick
        root_matches_the_root_element;
      Alcotest.test_case "keeps empty for want of a shipped model" `Quick
        keeps_empty_for_want_of_a_shipped_model;
      Alcotest.test_case "a condition is not a selector" `Quick
        a_condition_is_not_a_selector;
      Alcotest.test_case "drops a group block left with nothing" `Quick
        drops_a_group_block_left_with_nothing;
      Alcotest.test_case "keeps a layer block left with nothing" `Quick
        keeps_a_layer_block_left_with_nothing;
      Alcotest.test_case "keeps a statement with no selector" `Quick
        keeps_a_statement_with_no_selector;
      Alcotest.test_case "keeps a list holding an unmodelled branch" `Quick
        keeps_a_list_holding_an_unmodelled_branch;
      Alcotest.test_case "drops the unused branch of a modelled list" `Quick
        drops_the_unused_branch_of_a_modelled_list;
      Alcotest.test_case
        "removes an unused rule that declares a custom property" `Quick
        removes_an_unused_rule_that_declares_a_custom_property;
      Alcotest.test_case "judges a nested rule on its flattened selector" `Quick
        judges_a_nested_rule_on_its_flattened_selector;
      Alcotest.test_case "no element leaves every rule unused" `Quick
        no_element_leaves_every_rule_unused;
    ] )
