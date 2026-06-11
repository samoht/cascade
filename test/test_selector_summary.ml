(** Tests for {!Css.Selector_summary}.

    The summary is intentionally conservative: [may_overlap = false] is a hard
    disjointness claim used by optimizer dependency checks, while
    [may_overlap = true] only means "not proven disjoint". *)

open Cascade
module S = Css.Selector
module Summary = Css.Selector_summary

let selector input = S.of_string input
let summary input = Summary.of_selector (selector input)

let check_summary ?(ids = []) ?(classes = []) ?element ?(complex = false) name
    input =
  let actual = summary input in
  Alcotest.(check (list string)) (name ^ " ids") ids (Summary.ids actual);
  Alcotest.(check (list string))
    (name ^ " classes") classes (Summary.classes actual);
  Alcotest.(check (option string))
    (name ^ " element") element (Summary.element actual);
  Alcotest.(check bool) (name ^ " complex") complex (Summary.is_complex actual)

let check_overlap name expected a b =
  let got = Summary.may_overlap (summary a) (summary b) in
  Alcotest.(check bool) name expected got

let facts () =
  check_summary "universal" "*" ~ids:[] ~classes:[];
  check_summary "class" ".button" ~classes:[ "button" ];
  check_summary "id" "#submit" ~ids:[ "submit" ];
  check_summary "element" "button" ~element:"button";
  check_summary "compound sorted facts" "button#submit.primary.secondary"
    ~ids:[ "submit" ] ~classes:[ "primary"; "secondary" ] ~element:"button";
  check_summary "simple pseudo-class is complex" ".button:hover"
    ~classes:[ "button" ] ~complex:true;
  check_summary "attribute selector is complex" ".button[data-active]"
    ~classes:[ "button" ] ~complex:true

let subject_compound () =
  check_summary "descendant uses rightmost subject" "main article.card"
    ~classes:[ "card" ] ~element:"article";
  check_summary "child uses rightmost subject" ".layout > button.primary"
    ~classes:[ "primary" ] ~element:"button";
  check_summary "sibling uses rightmost subject" ".label + input#email"
    ~ids:[ "email" ] ~element:"input";
  check_summary "single-item selector list is transparent"
    (S.to_string (S.List [ S.class_ "item" ]))
    ~classes:[ "item" ];
  check_summary "multi selector list is complex" ".a,.b" ~complex:true

let disjointness () =
  check_overlap "same class may overlap" true ".button" ".button";
  check_overlap "different classes may overlap" true ".button" ".primary";
  check_overlap "class and universal may overlap" true ".button" "*";
  check_overlap "same id may overlap" true "#main" "#main";
  check_overlap "different ids are disjoint" false "#main" "#other";
  check_overlap "id only disjoint when both sides have ids" true "#main" ".card";
  check_overlap "same element may overlap" true "button.primary"
    "button.secondary";
  check_overlap "different elements are disjoint" false "button" "a";
  check_overlap "element disjoint wins with shared class" false "button.x" "a.x";
  check_overlap "complex summary keeps subject facts" false "a:hover" "button";
  check_overlap "complex summary is conservative without conflict" true
    ".a:hover" "button";
  check_overlap "selector list is conservative" true ".a,.b" ".c"

let pseudo_elements () =
  check_summary "before pseudo-element keeps originating element"
    "button::before" ~element:"button";
  check_overlap "same pseudo-element may overlap" true ".x::before" ".y::before";
  check_overlap "different pseudo-elements are disjoint" false ".x::before"
    ".x::after";
  check_overlap "pseudo-element and originating element are disjoint" false
    ".x::before" ".x";
  check_overlap "legacy and modern before spelling may overlap" true ".x:before"
    ".x::before";
  check_overlap "legacy and modern after spelling may overlap" true ".x:after"
    ".x::after"

let attributes () =
  check_overlap "different exact attribute values are disjoint" false
    "[type=button]" "[type=text]";
  check_overlap "same exact attribute value may overlap" true "[type=button]"
    "[type=button]";
  check_overlap "presence and exact attribute may overlap" true "[type]"
    "[type=button]";
  check_overlap "case-insensitive exact attributes may overlap by case" true
    "[type=\"BUTTON\" i]" "[type=button]";
  check_overlap "default attribute case stays conservative" true
    "[type=\"BUTTON\"]" "[type=button]";
  check_overlap "case-insensitive exact attributes can prove disjoint" false
    "[type=\"button\" i]" "[type=text]";
  check_overlap "different attributes stay conservative" true "[type=button]"
    "[role=textbox]"

let child_context () =
  check_overlap "different immediate parent ids are disjoint" false "#a > .item"
    "#b > .item";
  check_overlap "same immediate parent id may overlap" true "#a > .item"
    "#a > .item";
  check_overlap "different descendant ancestors stay conservative" true
    "#a .item" "#b .item";
  check_overlap "child parent disjointness combines with complex subjects" false
    "#a > .item:hover" "#b > .item:focus"

let negation () =
  check_overlap "required class conflicts with not class" false ".item.active"
    ".item:not(.active)";
  check_overlap "required id conflicts with not id" false "#save" ":not(#save)";
  check_overlap "required element conflicts with not element" false "button"
    ":not(button)";
  check_overlap "compound not stays conservative" true ".active"
    ":not(.active.disabled)";
  check_overlap "required exact attr conflicts with not exact attr" false
    "[data-state=open]" ":not([data-state=open])";
  check_overlap "different exact attr does not conflict with not exact attr"
    true "[data-state=closed]" ":not([data-state=open])"

let child_positions () =
  check_overlap "first child and even nth child are disjoint" false
    ":first-child" ":nth-child(even)";
  check_overlap "first child and odd nth child may overlap" true ":first-child"
    ":nth-child(odd)";
  check_overlap "different nth indexes are disjoint" false ":nth-child(2)"
    ":nth-child(3)";
  check_overlap "odd and even nth child are disjoint" false ":nth-child(odd)"
    ":nth-child(even)";
  check_overlap "first-child can overlap last-child on an only child" true
    ":first-child" ":last-child";
  check_overlap "nth fact conflicts with matching not nth" false ":nth-child(2)"
    ":not(:nth-child(even))";
  check_overlap "broad nth overlap with narrow not nth stays conservative" true
    ":nth-child(even)" ":not(:nth-child(2))"

let optimizer_dependency_examples () =
  (* These are the real shapes the optimizer cares about when approximating the
     CSS graph from Hague/Lin/Hong. A [false] result lets a rewrite cross a
     dependency; every [true] here is deliberately conservative. *)
  check_overlap "SMT A-B-A compounds may all hit one element" true ".a.x" ".b.x";
  check_overlap "different required ids prove disjoint" false "#a.x" "#b.x";
  check_overlap "descendant context is ignored for subject overlap" true
    ".theme .item" ".dialog .item";
  check_overlap "rightmost ids still prove descendant selectors disjoint" false
    ".theme #save" ".dialog #cancel";
  check_overlap "complex selectors keep id disjointness" false "#a:hover"
    "#b:focus";
  check_overlap "exact subject attributes prove disjoint" false
    "[data-state=open]" "[data-state=closed]";
  check_overlap "simple :not required token proves disjoint" false
    ".item.active" ".item:not(.active)";
  check_overlap "nth-child parity proves disjoint" false ".row:nth-child(odd)"
    ".row:nth-child(even)";
  check_overlap "child combinator parent conflicts prove disjoint" false
    "#toolbar > .item" "#dialog > .item";
  check_overlap "Tailwind prose selector stays conservative" true
    ".prose \
     :where(kbd):not(:where([class~=\"not-prose\"],[class~=\"not-prose\"] *))"
    ".prose kbd";
  check_overlap "selector list stays conservative for grouping safety" true
    ".a,.b" ".c"

let suite =
  ( "selector_summary",
    [
      Alcotest.test_case "facts" `Quick facts;
      Alcotest.test_case "subject compound" `Quick subject_compound;
      Alcotest.test_case "disjointness" `Quick disjointness;
      Alcotest.test_case "pseudo-elements" `Quick pseudo_elements;
      Alcotest.test_case "attributes" `Quick attributes;
      Alcotest.test_case "child context" `Quick child_context;
      Alcotest.test_case "negation" `Quick negation;
      Alcotest.test_case "child positions" `Quick child_positions;
      Alcotest.test_case "optimizer dependency examples" `Quick
        optimizer_dependency_examples;
    ] )
