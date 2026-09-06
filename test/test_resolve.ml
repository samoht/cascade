open Cascade
open Css_test_helpers

(* A minimal in-memory element tree to exercise {!Resolve.Make} without a
   DOM. *)
type tree = {
  tname : string;
  tid : string option;
  tclasses : string list;
  tattrs : (string * string) list;
  ttext : string list;
  mutable tparent : tree option;
  tchildren : tree list;
}

let elt ?id ?(classes = []) ?(attrs = []) ?(text = []) name children =
  let t =
    {
      tname = name;
      tid = id;
      tclasses = classes;
      tattrs = attrs;
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
  let attribute t k = List.assoc_opt k t.tattrs
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

let yes ?reading name s n =
  Alcotest.(check bool) name true (R.matches ?reading (sel s) n)

let no ?reading name s n =
  Alcotest.(check bool) name false (R.matches ?reading (sel s) n)

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

let answers ?reading name expected s n =
  Alcotest.check match_result name expected
    (R.match_selector ?reading (sel s) n)

(* A selector the matcher has no model for is not a selector that fails to
   match: the caller has to be able to tell the two apart, or it drops a rule it
   only failed to understand. *)
let test_unsupported_is_not_no_match () =
  answers "a modelled hit" Resolve.Matches "span" s2;
  answers "a modelled miss" Resolve.No_match "div" s2;
  answers "stateful" Resolve.Unsupported "span:hover" s2;
  answers "pseudo-element" Resolve.Unsupported "span::before" s2;
  answers "a case flag on a presence test" Resolve.Unsupported "[id i]" s2;
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
  check "structural" true "p:first-child";
  check "descendant" true "div span";
  check "attribute case flag" true "[data-k=\"X\" i]";
  check "child-indexed" true ":nth-child(2n+1)";
  check "child-indexed over an of S" true ":nth-child(2 of .a)";
  check "typed child-indexed" true ":nth-of-type(2)";
  check "relational" true ":has(> .a)";
  check "the reference element" true ":scope";
  check "a case flag on a presence test" false "[data-k i]";
  check "an of S on :nth-of-type" false ":nth-of-type(1 of p)";
  check "the content language" false ":lang(en)";
  check "namespaced attribute" false "[svg|href]";
  check "shadow-piercing combinator" false "div>>>span";
  check "stateful" false ":hover";
  check "one bad branch spoils the list" false "span,div:hover"

let titled = elt ~attrs:[ ("title", "x") ] "div" []

(* Two forms selectors-4 defines that no engine implements as defined. Sec. 6.3
   gives the [s] flag "identical to" semantics; every engine refuses the
   selector instead, and the rule carrying it with it. Sec. 13.2 matches an
   element whose only content is document white space, and its own note records
   that Level 2 and Level 3 did not - the change engines have not taken. The
   answer to either is a fact about the specification and not about how the page
   renders, and {!Apply} inlines and {!Prune} deletes, so the matcher declines
   the form rather than rewrite a page against its own browser.

   The [s] flag is refused whatever the node: sec. 6.3 is about the selector,
   and an engine that refuses it refuses the rule. Sec. 13.2 is about the
   element, so the node decides that one - {!test_empty_is_node_dependent}. *)
let test_declines_what_no_engine_implements () =
  answers "the case-sensitive attribute flag" Resolve.Unsupported "[title=x s]"
    titled;
  answers "on a value the flag rules out" Resolve.Unsupported "[title=X s]"
    titled;
  answers "the flag under a negation" Resolve.Unsupported ":not([title=x s])"
    titled;
  answers "the flag in a relational argument" Resolve.Unsupported
    ":has([title=x s])" titled;
  let declines name s =
    Alcotest.(check bool) name false (Resolve.supported (sel s))
  in
  declines "supported settles it without a node" ":empty";
  declines "and for the case-sensitive flag" "[title=x s]";
  (* The model is still there: the spec reading answers both. *)
  answers ~reading:Resolve.Spec "the flag reads the value as written"
    Resolve.Matches "[title=x s]" titled;
  answers ~reading:Resolve.Spec "and rules the other case out" Resolve.No_match
    "[title=X s]" titled;
  Alcotest.(check bool)
    "the spec reading models :empty" true
    (Resolve.supported ~reading:Resolve.Spec (sel ":empty"))

(* Sec. 13.2 represents "an element that has no children except, optionally,
   document white space characters", and its note records that Level 2 and Level
   3 did not match the white-space one. So the two readings part over that
   element and no other: an element with no children at all is [:empty] to both,
   and one holding an element or text of its own is [:empty] to neither. The
   decline belongs to the element, not to the selector, and an element the two
   levels agree on gets the answer they agree on.

   {!Resolve.supported} stays conservative through all of it. It is asked before
   any node is walked, so it cannot know which element it will be given, and
   {!Apply} and {!Prune} decide per rule off that answer. *)
let test_empty_is_node_dependent () =
  answers "no children at all" Resolve.Matches ":empty" (elt "p" []);
  answers "an element child" Resolve.No_match ":empty" (elt "p" [ elt "b" [] ]);
  answers "a text child" Resolve.No_match ":empty" (elt ~text:[ "text" ] "p" []);
  answers "a no-break space is not document white space" Resolve.No_match
    ":empty"
    (elt ~text:[ "\u{00a0}" ] "p" []);
  (* The one element Level 3 and Level 4 answer differently. *)
  answers "white space alone" Resolve.Unsupported ":empty"
    (elt ~text:[ " \t\n" ] "p" []);
  answers "white space beside text is text to both" Resolve.No_match ":empty"
    (elt ~text:[ " "; "text" ] "p" []);
  (* The combining forms carry the node's answer, decline and all. *)
  answers "in a compound" Resolve.Matches "p:empty" (elt "p" []);
  answers "under a negation" Resolve.No_match ":not(:empty)" (elt "p" []);
  answers "as one branch of a list" Resolve.Matches "span,:empty" (elt "p" []);
  answers "a compound over the declining element" Resolve.Unsupported "p:empty"
    (elt ~text:[ " " ] "p" []);
  (* No ancestor of an element holds it and nothing else, so an ancestor is
     never the element the two levels part over. *)
  answers "left of a descendant combinator" Resolve.No_match ":empty span" s2;
  answers "a relational argument the subtree decides" Resolve.Matches
    ":has(:empty)" a;
  let declines name s =
    Alcotest.(check bool) name false (Resolve.supported (sel s))
  in
  declines "the rule-level answer stays conservative" ":empty";
  declines "wherever :empty sits in the selector" "p:empty";
  declines "including inside a relational argument" ":has(:empty)";
  declines "and inside an of S" ":nth-child(1 of :empty)"

(* A sibling or a descendant can be the element the two levels part over, and
   the answer about it has to reach the subject rather than be read as a miss.
   Folding the decline into "does not match" here would hand a caller a definite
   answer off an element whose own answer cascade does not have. *)
let test_empty_declines_through_the_tree () =
  let ws () = elt ~text:[ " " ] "i" [] in
  let target = elt "p" [] in
  let _ = elt "div" [ ws (); target ] in
  answers "a declining preceding sibling" Resolve.Unsupported ":empty+p" target;
  let host = elt "div" [ ws () ] in
  answers "a declining descendant" Resolve.Unsupported ":has(:empty)" host;
  let indexed = elt "p" [] in
  let _ = elt "div" [ ws (); indexed ] in
  answers "a declining sibling in an of S" Resolve.Unsupported
    ":nth-child(2 of :empty)" indexed

(* selectors-4 sec. 13.2: [:empty] is "an element that has no children except,
   optionally, document white space characters". White space alone leaves an
   element empty, any other text does not, and U+00A0 is not document white
   space (css-text-4 sec. 4.3) - the spec lists [<div>&nbsp;</div>] among the
   elements [div:empty] does not represent. This is the reading engines have not
   taken, so it is the one {!Resolve.constructor-Spec} answers. *)
let test_empty_counts_text_children () =
  let yes = yes ~reading:Resolve.Spec and no = no ~reading:Resolve.Spec in
  yes "no children at all" ":empty" (elt "p" []);
  yes "spaces, tabs and newlines" ":empty" (elt ~text:[ " \t\n" ] "p" []);
  no "a text child" ":empty" (elt ~text:[ "text" ] "p" []);
  no "a text child beside white space" ":empty"
    (elt ~text:[ " "; "text" ] "p" []);
  no "a no-break space" ":empty" (elt ~text:[ "\u{00a0}" ] "p" []);
  no "an element child" ":empty" (elt "p" [ elt "span" [] ])

(* A spec sentence about a child-indexed pseudo-class names a set of positions
   ("the 2nd, 4th, 6th, etc elements"), so pin the whole set rather than probe
   it: [indices s children] is the 1-based positions of [children] that [s]
   matches. *)
let indices s children =
  List.mapi (fun i n -> (i + 1, n)) children
  |> List.filter_map (fun (i, n) ->
      if R.matches (sel s) n then Some i else None)

let of_ten = List.init 10 (fun i -> i + 1)
let items = List.map (fun _ -> elt "li" []) of_ten
let _ : tree = elt "ol" items
let item i = List.nth items (i - 1)

(* selectors-4 sec. 13.3.1: [:nth-child(An+B)] represents the elements that are
   among the An+Bth of their inclusive siblings, where An+B "represents any
   index i = An + B for any non-negative integer n" and the list is 1-indexed
   (sec. 13). The sets below are the spec's own: [2n+1] takes the first child
   "because when n=0 the expression evaluates to 1", [even] "represents the 2nd,
   4th, 6th, etc elements", [10n-1] "the 9th, 19th, 29th, etc elements", and
   [10n+9] is the "Same". [odd] is [2n+1] and [even] is [2n] per the An+B
   microsyntax (css-syntax-3 sec. 6). *)
let test_nth_child_indices () =
  let check name expected s =
    Alcotest.(check (list int)) name expected (indices s items)
  in
  check "the index is 1-based" [ 1 ] ":nth-child(1)";
  check "2n+1 takes the first child" [ 1; 3; 5; 7; 9 ] ":nth-child(2n+1)";
  check "odd is 2n+1" [ 1; 3; 5; 7; 9 ] ":nth-child(odd)";
  check "even is the 2nd, 4th, 6th" [ 2; 4; 6; 8; 10 ] ":nth-child(even)";
  check "2n is even" [ 2; 4; 6; 8; 10 ] ":nth-child(2n)";
  (* n=0 gives -1, which is no index at all, so 10n-1 starts at the 9th *)
  check "a negative B skips the indices below 1" [ 9 ] ":nth-child(10n-1)";
  check "10n+9 names the same set" [ 9 ] ":nth-child(10n+9)";
  check "A=0 is the single index B" [ 3 ] ":nth-child(0n+3)";
  check "B alone spells it" [ 3 ] ":nth-child(3)";
  check "index 0 is no index" [] ":nth-child(0)";
  check "a negative A counts down to 1" [ 1; 2; 3 ] ":nth-child(-n+3)";
  check "n alone takes every index" of_ten ":nth-child(n)";
  check "n+4 takes the rest" [ 4; 5; 6; 7; 8; 9; 10 ] ":nth-child(n+4)"

(* selectors-4 sec. 13.3.2: the same list "counting backwards from the end". The
   spec's [tr:nth-last-child(-n+2)] "represents the two last rows of an HTML
   table". *)
let test_nth_last_child_indices () =
  let check name expected s =
    Alcotest.(check (list int)) name expected (indices s items)
  in
  check "the last one" [ 10 ] ":nth-last-child(1)";
  check "the two last ones" [ 9; 10 ] ":nth-last-child(-n+2)";
  check "odd from the end" [ 2; 4; 6; 8; 10 ] ":nth-last-child(odd)";
  check "10n-1 from the end" [ 2 ] ":nth-last-child(10n-1)"

(* selectors-4 sec. 13.3: these pseudo-classes "have been rephrased to refer to
   an element's relative index amongst its siblings", since there was "no reason
   to exclude them from matching elements without parents". A parentless element
   is the whole list, so it is both the first and the last of it. *)
let test_child_index_without_a_parent () =
  let orphan = elt "p" [] in
  yes "the first of its inclusive siblings" ":nth-child(1)" orphan;
  yes "and the last" ":nth-last-child(1)" orphan;
  yes "so the only one" ":only-child" orphan;
  no "not the second" ":nth-child(2)" orphan;
  yes "a fixture root indexes the same way" ":nth-child(1)" section

(* selectors-4 sec. 13: "Standalone text and other non-element nodes are not
   counted when calculating the position of an element in the list of children
   of its parent." A {!NODE} keeps text out of [children], so text around an
   element leaves its index alone. *)
let test_child_index_counts_elements_only () =
  let bold = elt "b" [] and italic = elt "i" [] in
  let _ : tree =
    elt ~text:[ "lead "; " middle "; " tail" ] "p" [ bold; italic ]
  in
  yes "the first element child" ":nth-child(1)" bold;
  yes "the second element child" ":nth-child(2)" italic

(* selectors-4 sec. 13.3.1: with [of S] the list is "composed of their inclusive
   siblings that match the selector list S", so [:nth-child(-n+3 of
   li.important)] "matches the first three 'important' list items". The spec
   contrasts it with moving the selector out of the function: [li.important:
   nth-child(-n+3)] "instead just selects the first three children if they also
   happen to be 'important' list items". *)
let important = List.init 4 (fun _ -> elt ~classes:[ "important" ] "li" [])
let plain = List.init 2 (fun _ -> elt "li" [])

let important_items =
  match (important, plain) with
  | [ i2; i4; i5; i6 ], [ p1; p3 ] -> [ p1; i2; p3; i4; i5; i6 ]
  | _ -> assert false

let _ : tree = elt "ol" important_items

(* The [hidden] rows of the zebra-striping example in the same section:
   [tr:nth-child(even of :not([hidden]))] keeps "a proper alternating background
   regardless of which rows are hidden". *)
let rows =
  List.mapi
    (fun i _ ->
      let attrs = if i = 2 then [ ("hidden", "") ] else [] in
      elt ~attrs "tr" [])
    [ 1; 2; 3; 4; 5; 6 ]

let _ : tree = elt "tbody" rows

let test_nth_child_of_selector () =
  let check name expected s children =
    Alcotest.(check (list int)) name expected (indices s children)
  in
  check "the first three important items" [ 2; 4; 5 ]
    ":nth-child(-n+3 of li.important)" important_items;
  check "the important ones among the first three" [ 2 ]
    "li.important:nth-child(-n+3)" important_items;
  check "even among the rows that are not hidden" [ 2; 5 ]
    "tr:nth-child(even of :not([hidden]))" rows;
  check "even among all the rows" [ 2; 4; 6 ] "tr:nth-child(even)" rows;
  check "counted backwards over the same list" [ 5; 6 ]
    ":nth-last-child(-n+2 of li.important)" important_items

(* selectors-4 sec. 13.4: the typed child-indexed pseudo-classes "resolve based
   on an element's index among elements of the same type (tag name) in their
   sibling list". Sec. 13.4.3 makes [:first-of-type] the same element as
   [:nth-of-type(1)], sec. 13.4.4 makes [:last-of-type] the same as
   [:nth-last-of-type(1)], and sec. 13.4.5 makes [:only-of-type] the same as
   [:first-of-type:last-of-type]. *)
let typed_children =
  [
    elt "h2" []; elt "p" []; elt "h2" []; elt "span" []; elt "h2" []; elt "p" [];
  ]

let _ : tree = elt "div" typed_children

let test_of_type_indices () =
  let check name expected s =
    Alcotest.(check (list int)) name expected (indices s typed_children)
  in
  check "the first h2" [ 1 ] "h2:first-of-type";
  check "the last h2" [ 5 ] "h2:last-of-type";
  check "the second h2" [ 3 ] "h2:nth-of-type(2)";
  check "the second h2 from the end" [ 3 ] "h2:nth-last-of-type(2)";
  check "the span is the only one of its type" [ 4 ] ":only-of-type";
  check "no h2 is" [] "h2:only-of-type";
  (* sec. 13.4.1: "img:nth-of-type(2) is equivalent to *:nth-child(2 of img)" *)
  check "the same element as :nth-child(2 of h2)" [ 3 ] ":nth-child(2 of h2)";
  (* sec. 13.4.2 gives both spellings of "all h2 children except the first and
     last" *)
  check "every h2 but the first and the last" [ 3 ]
    "h2:nth-of-type(n+2):nth-last-of-type(n+2)";
  check "the same set through :not" [ 3 ]
    "h2:not(:first-of-type):not(:last-of-type)"

(* An element with no parent is the whole list of its inclusive siblings here
   too, so it is the only one of its type. *)
let test_of_type_without_a_parent () =
  let orphan = elt "p" [] in
  yes "first of its type" ":first-of-type" orphan;
  yes "last of its type" ":last-of-type" orphan;
  yes "only of its type" ":only-of-type" orphan

(* The type comparison is the type selector's own, which reads a name ASCII
   case-insensitively, so sec. 13.4.1's equivalence between [:nth-of-type()] and
   [:nth-child(An+B of <type>)] holds whichever case the tree spells the tag
   in. *)
let test_of_type_reads_the_name_case_insensitively () =
  let upper = elt "DIV" [] and lower = elt "div" [] in
  let _ : tree = elt "section" [ upper; lower ] in
  yes "the first of the two" ":first-of-type" upper;
  no "so the second is not" ":first-of-type" lower;
  no "and neither is alone" ":only-of-type" upper;
  yes "as :nth-child(2 of div) agrees" ":nth-child(2 of div)" lower

(* selectors-4 sec. 4.5: [:has()] "represents an element if any of the relative
   selectors would match at least one element when anchored against this
   element". A relative selector "begins with a combinator, with a selector
   representing the anchor element implied at the start"; with no combinator the
   descendant combinator is implied (sec. 3.4). *)
let nested_img = elt "img" []
let link_direct = elt "a" [ elt "img" [] ]
let link_nested = elt "a" [ elt "span" [ nested_img ] ]
let link_none = elt "a" [ elt "span" [] ]
let links = elt "nav" [ link_direct; link_nested; link_none ]

let test_has_descendant_and_child () =
  (* sec. 4.5: "a:has(> img)" "matches only <a> elements that contain an <img>
     child" *)
  yes "an img child" "a:has(> img)" link_direct;
  no "a grandchild is not a child" "a:has(> img)" link_nested;
  no "no img at all" "a:has(> img)" link_none;
  (* with no combinator the descendant combinator is implied *)
  yes "an img child is a descendant" "a:has(img)" link_direct;
  yes "so is a grandchild" "a:has(img)" link_nested;
  no "still no img" "a:has(img)" link_none;
  (* the argument is a complex selector, anchored at the subject *)
  yes "a child span holding an img" "a:has(> span img)" link_nested;
  no "the img is not under a span" "a:has(> span img)" link_direct;
  (* the anchor is the subject, not an ancestor of it *)
  no "an ancestor of the match is not the anchor" "nav:has(> img)" links

let dt_first = elt "dt" []
let dt_second = elt "dt" []
let dd_last = elt "dd" []
let _ : tree = elt "dl" [ dt_first; dt_second; dd_last ]

let test_has_sibling_combinators () =
  (* sec. 4.5: "dt:has(+ dt)" "matches a <dt> element immediately followed by
     another <dt> element" *)
  yes "followed immediately by a dt" "dt:has(+ dt)" dt_first;
  no "followed immediately by a dd" "dt:has(+ dt)" dt_second;
  yes "a dd follows somewhere" "dt:has(~ dd)" dt_first;
  yes "and follows this one too" "dt:has(~ dd)" dt_second;
  yes "a dt follows this one" "dt:has(~ dt)" dt_first;
  no "nothing follows the last dt" "dt:has(~ dt)" dt_second;
  no "the dd has no dt after it" "dd:has(~ dt)" dd_last

(* sec. 4.5 spells out that the order of [:not()] and [:has()] matters:
   "section:not(:has(h1, h2, h3, h4, h5, h6))" "matches <section> elements that
   don't contain any heading elements", while swapping the nesting "would result
   in matching any <section> element which contains anything that's not a
   heading element". *)
let sec_bare = elt "section" []
let sec_heading = elt "section" [ elt "h1" [] ]
let sec_para = elt "section" [ elt "p" [] ]
let _ : tree = elt "main" [ sec_bare; sec_heading; sec_para ]
let headings = "h1, h2, h3, h4, h5, h6"

let test_has_ordering_against_not () =
  let holds_no_heading =
    String.concat "" [ "section:not(:has("; headings; "))" ]
  in
  let holds_a_non_heading =
    String.concat "" [ "section:has(:not("; headings; "))" ]
  in
  yes "an empty section holds no heading" holds_no_heading sec_bare;
  no "this one holds one" holds_no_heading sec_heading;
  yes "a paragraph is not a heading" holds_no_heading sec_para;
  no "an empty section holds nothing at all" holds_a_non_heading sec_bare;
  no "this one holds only a heading" holds_a_non_heading sec_heading;
  yes "this one holds a paragraph" holds_a_non_heading sec_para

(* selectors-4 sec. 6.3: the [i] identifier makes a UA "match the attribute's
   value ASCII case-insensitively", and [s] makes it match "case-sensitively,
   with 'identical to' semantics". With no flag the case-sensitivity "depends on
   the document language", which a {!NODE} does not carry, so the value is read
   as written. *)
let framed =
  elt
    ~attrs:[ ("frame", "HSIDES"); ("data-k", "GR\u{00dc}N"); ("lang", "EN-GB") ]
    "table" []

let type_lower = elt ~attrs:[ ("type", "a") ] "ol" []
let type_upper = elt ~attrs:[ ("type", "A") ] "ol" []

let test_attribute_case_flags () =
  (* sec. 6.3's own example: [frame=hsides i] styles the attribute "whether that
     value is represented as hsides, HSIDES, hSides, etc." *)
  yes "i folds the case" "[frame=hsides i]" framed;
  (* HTML sec. 4.16.2 lists [frame] among the attributes an HTML document
     compares ASCII case-insensitively however the selector is written, and the
     engines apply it, so the browser reading folds without a flag. *)
  yes "an HTML-folded name folds without a flag" "[frame=hsides]" framed;
  no ~reading:Resolve.Spec "the specification alone reads it as written"
    "[frame=hsides]" framed;
  (* [data-k] is not on that list, so its value is read as written. *)
  no "a name off the list is read as written" "[data-k=\"gr\u{00dc}n\"]" framed;
  (* The name part is converted to ASCII lowercase before it is compared. *)
  yes "the name folds" "[FRAME=hsides]" framed;
  no ~reading:Resolve.Spec "s is identical-to" "[frame=hsides s]" framed;
  yes ~reading:Resolve.Spec "and the written value is identical to itself"
    "[frame=HSIDES s]" framed;
  (* "ASCII case insensitivity allows green to match GREEN. However, gruen [with
     an umlaut] would not match GRUEN [with an umlaut]." *)
  no "i folds ASCII only" "[data-k=\"gr\u{00fc}n\" i]" framed;
  (* the flag applies to every matcher, not just [=] *)
  yes "a prefix" "[frame^=hs i]" framed;
  yes "a suffix" "[frame$=des i]" framed;
  yes "a substring" "[frame*=sid i]" framed;
  yes "a hyphen list" "[lang|=en i]" framed;
  (* sec. 6.3's second example: [type="a" s] and [type="A" s] are two rules *)
  yes ~reading:Resolve.Spec "s keeps the two apart" "[type=\"a\" s]" type_lower;
  no ~reading:Resolve.Spec "the other way round" "[type=\"a\" s]" type_upper;
  yes ~reading:Resolve.Spec "and back" "[type=\"A\" s]" type_upper;
  yes "i puts them together" "[type=\"a\" i]" type_lower;
  yes "both ways" "[type=\"a\" i]" type_upper

(* selectors-4 sec. 8.4: ":scope represents this scoping root", and "if there is
   no scoping root then :scope represents the root of the tree the element is in
   ... or :root otherwise". Nothing hands this matcher a scoping root and it has
   no shadow tree, so [:scope] is [:root]. *)
let test_scope_is_the_root () =
  yes "the root of the tree" ":scope" section;
  no "not an element inside it" ":scope" s1;
  yes "and combines like :root" ":scope > div" a;
  no "a child, not a descendant" ":scope > span" s1

(* [Unsupported] beats every other answer in every combining form, so a caller
   never reads a gap in the model as a rule that does not apply. Each selector
   here is asked against a node the supported part of it misses, so a matcher
   that settled on the first failing part would answer [No_match] instead. *)
let test_unsupported_beats_the_structural_forms () =
  let leaf = elt "p" [] in
  answers "a stateful :has argument" Resolve.Unsupported ":has(:hover)"
    link_direct;
  answers "a stateful relative :has argument" Resolve.Unsupported
    ":has(> :hover)" link_direct;
  answers "no candidate to test it against" Resolve.Unsupported ":has(:hover)"
    leaf;
  answers "inside :not" Resolve.Unsupported ":not(:has(:hover))" link_direct;
  answers "inside :is" Resolve.Unsupported ":is(:has(:hover))" link_direct;
  answers "beside a type that already missed" Resolve.Unsupported
    "p:has(:hover)" link_direct;
  answers "a branch of a list" Resolve.Unsupported ".x, :has(:hover)"
    link_direct;
  answers "a stateful of S" Resolve.Unsupported ":nth-child(1 of :hover)"
    (item 1);
  answers "counting backwards" Resolve.Unsupported
    ":nth-last-child(1 of :hover)" (item 1);
  answers "one stateful branch of an of S" Resolve.Unsupported
    ":nth-child(1 of .x, :hover)" (item 1);
  answers "a namespace in an of S" Resolve.Unsupported ":nth-child(1 of *|li)"
    (item 1);
  answers "a namespace in a :has argument" Resolve.Unsupported ":has(*|img)"
    link_direct;
  (* sec. 13.4.1 gives :nth-of-type() an An+B and nothing else *)
  answers "an of S on :nth-of-type" Resolve.Unsupported ":nth-of-type(1 of p)"
    (item 1);
  (* sec. 16: the modifier follows a matcher and a value *)
  answers "a case flag on a presence test" Resolve.Unsupported "[frame i]"
    framed;
  (* the content language is the document's to define, not the tree's *)
  answers ":lang" Resolve.Unsupported ":lang(en)" framed

(* The other half of the same property: a form the matcher does model answers
   [No_match] rather than hedging, even where it has nothing to look at. *)
let test_the_new_forms_answer_no_match () =
  let leaf = elt "p" [] in
  answers "an empty subtree is a miss" Resolve.No_match ":has(> img)" leaf;
  answers "so is one with no sibling after it" Resolve.No_match ":has(+ p)" leaf;
  answers "an index nothing reaches" Resolve.No_match ":nth-child(0)" (item 1);
  answers "an of S the node itself misses" Resolve.No_match
    ":nth-child(1 of .absent)" (item 1);
  answers "a type with no second of its kind" Resolve.No_match
    "span:nth-of-type(2)"
    (List.nth typed_children 3)

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
          Some (Declaration.to_string ~minify:true d)
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
        Some (Declaration.to_string ~minify:true d)
      else None)
    (R.resolve (sheet_of css) s2)

(* Layer names form a tree (css-cascade-5 sec. 6.4.2): [@layer a.b] creates [a]
   first and nests [b] inside it, and a later [@layer a.d] is ordered inside [a]
   too, so it stays before a top-level layer declared in between. Sec. 6.4.3
   puts the parent's own rules at the end of that subtree: "Unlayered rules are
   sorted later than any layered rules within the same parent layer (if any)."
   So [a] outranks [a.b], however the two were written. *)
let test_resolve_nested_layers () =
  Alcotest.(check (option string))
    "a sublayer comes before its parent's own rules" (Some "color:blue")
    (resolved_color "@layer a.b{span{color:red}}@layer a{span{color:blue}}");
  Alcotest.(check (option string))
    "a top-level layer sorts after the whole subtree" (Some "color:green")
    (resolved_color
       "@layer a.b{span{color:red}}@layer c{span{color:green}}@layer \
        a.d{span{color:blue}}")

(* The same sec. 6.4.3 rule over every way of writing the pair. A parent's own
   rules sit in an implicit sub-layer that closes its subtree, so among normal
   declarations the parent beats each of its sublayers and among important ones
   every sublayer beats it. Neither answer depends on which was declared first,
   nor on whether the sublayer is named, nested as a block, anonymous, or only
   pre-declared by an [@layer a, b;] statement. *)
let test_parent_layer_closes_its_subtree () =
  Alcotest.(check (option string))
    "the parent wins when it is declared second" (Some "color:blue")
    (resolved_color "@layer a.b{span{color:red}}@layer a{span{color:blue}}");
  Alcotest.(check (option string))
    "and when it is declared first" (Some "color:red")
    (resolved_color "@layer a{span{color:red}}@layer a.b{span{color:blue}}");
  Alcotest.(check (option string))
    "the sublayer wins for important, sublayer declared first"
    (Some "color:red!important")
    (resolved_color
       "@layer a.b{span{color:red!important}}@layer \
        a{span{color:blue!important}}");
  Alcotest.(check (option string))
    "the sublayer wins for important, parent declared first"
    (Some "color:blue!important")
    (resolved_color
       "@layer a{span{color:red!important}}@layer \
        a.b{span{color:blue!important}}");
  Alcotest.(check (option string))
    "a sublayer block nested in its parent orders the same way"
    (Some "color:blue")
    (resolved_color "@layer a{@layer b{span{color:red}}span{color:blue}}");
  Alcotest.(check (option string))
    "an anonymous block nested in a parent is one of its sublayers"
    (Some "color:blue")
    (resolved_color "@layer a{@layer{span{color:red}}span{color:blue}}");
  Alcotest.(check (option string))
    "a statement declaring the parent first does not move its own rules"
    (Some "color:blue")
    (resolved_color
       "@layer a;@layer a.b{span{color:red}}@layer a{span{color:blue}}");
  Alcotest.(check (option string))
    "a statement declaring the sublayer first does not either"
    (Some "color:blue")
    (resolved_color
       "@layer a.b;@layer a{span{color:blue}}@layer a.b{span{color:red}}");
  Alcotest.(check (list string))
    "the parent closes the run its sublayers open" [ "a.b"; "a.d"; "a"; "c" ]
    (Resolve.layer_order
       (sheet_of "@layer a.b{span{color:red}}@layer c{}@layer a.d{}"))

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

(* Every block at-rule {!Flatten.block} leaves standing around a rule, as the
   CSS text that opens and closes it. [@else] binds to the [@when] before it
   (css-conditional-5 sec. 3), so its opener carries an antecedent that cannot
   match [s2]. *)
let wrappers_resolve_skips =
  [
    ("@media", "@media (width>0px){", "}");
    ("@supports", "@supports (color:red){", "}");
    ("@container", "@container (width>0px){", "}");
    ("@-moz-document", "@-moz-document url-prefix(){", "}");
    ("@starting-style", "@starting-style{", "}");
    ("@when", "@when media(width>0px){", "}");
    ("@else", "@when media(width<0px){p{color:blue}}@else{", "}");
    ("@scope", "@scope (.card){", "}");
  ]

(* [resolve] answers for the ordinary cascade of an element in a document it
   cannot see: it has no viewport, no UA feature table, no container layout, no
   document URL, and no scoping root, and [@starting-style] declares a
   before-change style rather than an ordinary one. So a rule inside any of
   these contributes nothing, and only [@layer], which gates nothing, does. This
   is what {!Apply.Make} relies on: it keeps each of these blocks in the
   stylesheet and inlines only what [resolve] returns, so a declaration counted
   in both places would be applied twice. *)
let test_resolve_skips_conditional_and_scoped_blocks () =
  Alcotest.(check (option string))
    "a bare rule resolves" (Some "color:red")
    (resolved_color "span{color:red}");
  Alcotest.(check (option string))
    "a rule in @layer resolves" (Some "color:red")
    (resolved_color "@layer L{span{color:red}}");
  List.iter
    (fun (label, before, after) ->
      Alcotest.(check (option string))
        (String.concat " " [ "a rule in"; label; "does not resolve" ])
        None
        (resolved_color (String.concat "" [ before; "span{color:red}"; after ])))
    wrappers_resolve_skips;
  Alcotest.(check (option string))
    "a rule in an origin wrapper does not resolve" None
    (List.find_map
       (fun d ->
         if Declaration.property_name d = "color" then
           Some (Declaration.to_string ~minify:true d)
         else None)
       (R.resolve [ Stylesheet.Origin (Author, sheet_of "span{color:red}") ] s2))

(* [layer_order] is the order [resolve] ranks against, so it counts a layer in
   exactly the blocks [resolve] walks. A layer named inside one of the others is
   not part of that order. *)
let test_layer_order_counts_the_blocks_resolve_walks () =
  Alcotest.(check (list string))
    "a nested @layer block is counted" [ "a.b"; "a" ]
    (Resolve.layer_order (sheet_of "@layer a{@layer b{span{color:red}}}"));
  List.iter
    (fun (label, before, after) ->
      Alcotest.(check (list string))
        (String.concat " " [ "a layer named inside"; label; "is not counted" ])
        []
        (Resolve.layer_order
           (sheet_of
              (String.concat "" [ before; "@layer a{span{color:red}}"; after ]))))
    wrappers_resolve_skips

(* The declarations [resolve] leaves out are not lost: {!Apply.Make} keeps the
   block whole in the stylesheet it emits, so the browser applies it there. *)
let test_apply_keeps_the_blocks_resolve_skips () =
  let module A = Apply.Make (Node) in
  let result : tree Apply.result =
    A.compute ~sheet:(sheet_of "@scope (.card){span{color:red}}") [ section ]
  in
  Alcotest.(check bool)
    "nothing is projected onto an element" true
    (List.for_all (fun (_, decls) -> decls = []) result.styles);
  Alcotest.(check bool)
    "the @scope block is kept in the stylesheet" true
    (String.length result.keep_css > 0)

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*], so
   [@layer a\2e b] names one layer whose single ident holds a dot, and [@layer
   a.b] names the sublayer [b] of [a]. They are two layers, each ordered by its
   own first declaration (sec. 6.4.2), and [a] is the parent of the second only.
   Chrome answers each of these the same way. *)
let test_resolve_escaped_dot_layers () =
  Alcotest.(check (option string))
    "the layer named a.b is declared after the sublayer" (Some "color:red")
    (resolved_color
       "@layer a.b,a\\2e b;@layer a\\2e b{span{color:red}}@layer \
        a.b{span{color:blue}}");
  Alcotest.(check (option string))
    "the sublayer is declared after the layer named a.b" (Some "color:blue")
    (resolved_color
       "@layer a\\2e b,a.b;@layer a\\2e b{span{color:red}}@layer \
        a.b{span{color:blue}}");
  Alcotest.(check (option string))
    "a.b joins a's subtree, which precedes z" (Some "color:green")
    (resolved_color
       "@layer a,z;@layer z{span{color:green}}@layer a.b{span{color:blue}}");
  Alcotest.(check (option string))
    "the layer named a.b joins no subtree, so it follows z" (Some "color:blue")
    (resolved_color
       "@layer a,z;@layer z{span{color:green}}@layer a\\2e b{span{color:blue}}");
  Alcotest.(check (option string))
    "two spellings of one ident are one layer" (Some "color:green")
    (resolved_color
       "@layer a\\2e b,w;@layer w{span{color:green}}@layer \
        a\\.b{span{color:blue}}")

(* The same two names in {!Resolve.layer_order}: each part of a path is written
   with the escapes that read it back (CSS Syntax 3 sec. 2.1), so the dot inside
   an ident cannot pass for the separator between two. *)
let test_layer_order_escaped_dot () =
  Alcotest.(check (list string))
    "a dot inside an ident is not a path separator" [ "a.b"; "a"; "a\\.b" ]
    (Resolve.layer_order (sheet_of "@layer a.b;@layer a\\2e b;"));
  Alcotest.(check (list string))
    "a sublayer of the layer named a.b" [ "a\\.b.c"; "a\\.b" ]
    (Resolve.layer_order (sheet_of "@layer a\\2e b.c;"))

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

(* --- allocation / complexity guard --- *)

let sheet_of_size n =
  let b = Buffer.create ((n * 20) + 32) in
  let out = Fmt.with_buffer b in
  for i = 1 to n do
    Fmt.pf out ".c%d{--p%d:%d}" i i i
  done;
  Buffer.add_string b "#s1{color:red}";
  match Css.of_string (Buffer.contents b) with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* Flattening the sheet and bucketing its rules by layer depend on the sheet
   alone, so a caller walking a document should pay for them once. Ten queries
   against one prepared sheet cost close to one preparation; ten calls of
   [resolve] pay ten times. *)
let test_resolve_prepared_shares_the_sheet () =
  let sheet = sheet_of_size 2_000 in
  let queries = 10 in
  let repeated =
    measure (fun () ->
        let last = ref [] in
        for _ = 1 to queries do
          last := R.resolve sheet s1
        done;
        !last)
  in
  let shared =
    measure (fun () ->
        let prepared = Resolve.prepare sheet in
        let last = ref [] in
        for _ = 1 to queries do
          last := R.resolve_prepared prepared s1
        done;
        !last)
  in
  let ratio = repeated /. shared in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f repeated vs %.0f shared (%.2fx for %d queries)"
       repeated shared ratio queries)
    true (ratio > 3.)

(* [resolve] is [prepare] followed by [resolve_prepared], so the two answer
   alike on every node. *)
let test_resolve_prepared_agrees () =
  let sheet = sheet_of_size 32 in
  let prepared = Resolve.prepare sheet in
  List.iter
    (fun node ->
      let direct = List.map Declaration.to_string (R.resolve sheet node) in
      let via =
        List.map Declaration.to_string (R.resolve_prepared prepared node)
      in
      Alcotest.(check (list string)) "prepared resolve agrees" direct via)
    [ s1; s2; a; b; section ]

let suite =
  ( "resolve",
    [
      Alcotest.test_case "simple selectors" `Quick test_simple;
      Alcotest.test_case "prepared resolve shares the sheet" `Quick
        test_resolve_prepared_shares_the_sheet;
      Alcotest.test_case "prepared resolve agrees with resolve" `Quick
        test_resolve_prepared_agrees;
      Alcotest.test_case "single combinator" `Quick test_single_combinator;
      Alcotest.test_case "sibling then descendant/child" `Quick
        test_sibling_then_descendant;
      Alcotest.test_case "unsupported is not no-match" `Quick
        test_unsupported_is_not_no_match;
      Alcotest.test_case "supported needs no node" `Quick
        test_supported_needs_no_node;
      Alcotest.test_case "declines what no engine implements" `Quick
        test_declines_what_no_engine_implements;
      Alcotest.test_case "empty is the node's question" `Quick
        test_empty_is_node_dependent;
      Alcotest.test_case "empty declines through the tree" `Quick
        test_empty_declines_through_the_tree;
      Alcotest.test_case "empty counts text children" `Quick
        test_empty_counts_text_children;
      Alcotest.test_case "nth-child indexes from one" `Quick
        test_nth_child_indices;
      Alcotest.test_case "nth-last-child counts from the end" `Quick
        test_nth_last_child_indices;
      Alcotest.test_case "a child index needs no parent" `Quick
        test_child_index_without_a_parent;
      Alcotest.test_case "a child index counts elements only" `Quick
        test_child_index_counts_elements_only;
      Alcotest.test_case "of S filters the sibling list" `Quick
        test_nth_child_of_selector;
      Alcotest.test_case "of-type indexes within the type" `Quick
        test_of_type_indices;
      Alcotest.test_case "an of-type index needs no parent" `Quick
        test_of_type_without_a_parent;
      Alcotest.test_case "of-type reads the name case-insensitively" `Quick
        test_of_type_reads_the_name_case_insensitively;
      Alcotest.test_case "has over descendants and children" `Quick
        test_has_descendant_and_child;
      Alcotest.test_case "has over siblings" `Quick test_has_sibling_combinators;
      Alcotest.test_case "has and not do not commute" `Quick
        test_has_ordering_against_not;
      Alcotest.test_case "attribute case flags" `Quick test_attribute_case_flags;
      Alcotest.test_case "scope is the root" `Quick test_scope_is_the_root;
      Alcotest.test_case "unsupported beats the structural forms" `Quick
        test_unsupported_beats_the_structural_forms;
      Alcotest.test_case "the structural forms answer no-match" `Quick
        test_the_new_forms_answer_no_match;
      Alcotest.test_case "resolve applies the cascade" `Quick
        test_resolve_cascade;
      Alcotest.test_case "nested layer names order as a tree" `Quick
        test_resolve_nested_layers;
      Alcotest.test_case "a parent layer closes its own subtree" `Quick
        test_parent_layer_closes_its_subtree;
      Alcotest.test_case "anonymous layers are distinct" `Quick
        test_resolve_anonymous_layers;
      Alcotest.test_case "resolve skips conditional and scoped blocks" `Quick
        test_resolve_skips_conditional_and_scoped_blocks;
      Alcotest.test_case "layer order counts the blocks resolve walks" `Quick
        test_layer_order_counts_the_blocks_resolve_walks;
      Alcotest.test_case "apply keeps the blocks resolve skips" `Quick
        test_apply_keeps_the_blocks_resolve_skips;
      Alcotest.test_case "an escaped dot is not a layer separator" `Quick
        test_resolve_escaped_dot_layers;
      Alcotest.test_case "layer_order keeps the two apart" `Quick
        test_layer_order_escaped_dot;
      Alcotest.test_case "apply projects a static rule, keeps a dynamic one"
        `Quick test_apply_compute;
    ] )
