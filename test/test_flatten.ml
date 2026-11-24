open Cascade

let block css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Css.to_string ~minify:true stmts

let test_no_nesting_is_identity () =
  let stmts = block ".a{color:red}.b{width:1px}" in
  let flattened = Flatten.block stmts in
  Alcotest.(check string)
    "no nesting input round-trips minified" ".a{color:red}.b{width:1px}"
    (render flattened)

let test_flattens_descendant_nesting () =
  let stmts = block ".a{color:red;.b{width:1px}}" in
  let flattened = Flatten.block stmts in
  let out = render flattened in
  Alcotest.(check bool)
    ("nested rule lifts to top level: " ^ out)
    true
    (String.length out > 0
    && not (String.contains out '{' && String.contains out '}' = false));
  Alcotest.(check bool)
    "result contains the lifted selector .a .b" true
    (let len = String.length ".a .b" in
     let pat = ".a .b" in
     let found = ref false in
     for i = 0 to String.length out - len do
       if String.sub out i len = pat then found := true
     done;
     !found)

let test_empty_block_is_empty () =
  let stmts = block "" in
  Alcotest.(check int)
    "empty input yields empty output" 0
    (List.length (Flatten.block stmts))

(* CSS Selectors 4 sec. 3.6.5 makes a combinator after a pseudo-element invalid,
   and the selector reader refuses [.a::before .b] when it is authored directly.
   Flattening reaches the same selector from a valid parent and a valid nested
   selector, so it has to apply the rule too: Chrome 151 and WebKit 26.5 keep
   such a nested rule in cssRules but it matches nothing, so the rule carries no
   style and emitting it only produces CSS cascade would refuse to read back.

   The parent's own declarations are unaffected, and a nested selector that
   extends the compound rather than following it is still valid. *)
let test_pseudo_element_parent_drops_invalid_nesting () =
  let dropped name css expected =
    Alcotest.(check string) name expected (render (Flatten.block (block css)))
  in
  dropped "a descendant under a pseudo-element parent goes"
    ".a::before{.b{color:red}}" "";
  dropped "the parent keeps its own declarations"
    ".a::before{color:red;.b{color:blue}}" ".a:before{color:red}";
  dropped "a child combinator goes the same way" ".a::before{>.b{color:red}}" "";
  (* Chrome 146 drops [a::before:hover] from cssRules, and the reader refuses
     it, so composing it through [&] is dead too. *)
  dropped "a pseudo-class ::before does not take goes with the rest"
    ".a::before{.b,&:hover{color:red}}" "";
  (* Control: no pseudo-element, so nesting is valid and lifts as before. *)
  dropped "a plain parent still flattens" ".a{.b{color:red}}" ".a .b{color:red}"

(* CSS Selectors 4 sec. 3.6.3 and sec. 3.6.4 bound what a compound may carry
   after a pseudo-element, and CSS Nesting 1 sec. 4 says [&] cannot represent
   one at all, as [:is()] cannot: [&:hover] under [a::part(p)] is
   [:is(a::part(p)):hover], which matches nothing, not [a::part(p):hover].
   Chromium 153 leaves [#h::part(p){&{...}}] and [p::before{&{...}}] unstyled,
   while the flat [#h::part(p)] rule after it applies. So every composition
   through [&] under a pseudo-element parent goes, whichever pseudo-class or
   sub-pseudo-element the pseudo-element would take written flat. *)
let test_pseudo_element_parent_drops_invalid_compound () =
  let check name css expected =
    Alcotest.(check string) name expected (render (Flatten.block (block css)))
  in
  check "a pseudo-class the pseudo-element refuses goes"
    ".a::before{&:hover{color:red}}" "";
  check "so does one after ::selection" ".a::selection{&:hover{color:red}}" "";
  check "and a second refused user action" ".a::before{&:focus{color:red}}" "";
  check "a class after the pseudo-element goes" ".a::before{&.b{color:red}}" "";
  check "so does a sub-pseudo-element it does not define"
    ".a::before{&::after{color:red}}" "";
  check "[&] names no ::file-selector-button"
    ".a::file-selector-button{&:hover{color:red}}" "";
  check "[&] names no ::part()" ".a::part(p){&:hover{color:red}}" "";
  check "[&] names no ::cue" ".a::cue{&:hover{color:red}}" "";
  check "[&] names no ::before, even to reach its ::marker"
    ".a::before{&::marker{color:red}}" ""

(* CSS Nesting 1 sec. 4: [&] matches what [:is(<parent list>)] matches, and
   [:is()] matches no pseudo-element, so [&] alone, or after a combinator, names
   nothing under a pseudo-element parent. Chromium 153 leaves [p::before] red
   under [p::before{color:red;&{color:blue}}] and under [.x &] with [.x] on the
   body, and colours [.a] but not [.a::before] under [.a,.a::before{&{...}}].
   CSS Nesting 1 sec. 5 makes a nested declarations rule the exception: it
   matches the parent's pseudo-elements too, and Chromium 153 applies one. *)
let test_nesting_selector_names_no_pseudo_element () =
  let check name css expected =
    Alcotest.(check string) name expected (render (Flatten.block (block css)))
  in
  check "[&] alone under a pseudo-element parent goes"
    ".a::before{&{color:red}}" "";
  check "the parent keeps its own declarations"
    ".a::before{color:red;&{color:blue}}" ".a:before{color:red}";
  check "[&] after a combinator goes too" ".a::before{.x &{color:red}}" "";
  check "::part() is a pseudo-element like any other"
    "#h::part(p){color:red;&{color:blue}}" "#h::part(p){color:red}";
  check "a list parent's pseudo-element branch gives [&] nothing"
    ".a,.a::before{&{color:red}}" ".a{color:red}";
  check "nor in a compound" ".a,.a::before{&.x{color:red}}" ".a.x{color:red}";
  check "nor before a descendant" ".a,.a::before{.b{color:red}}"
    ".a .b{color:red}";
  (* Control: a nested declarations rule is no [&] rule. *)
  check "nested declarations reach the pseudo-element"
    ".a::before{@media screen{color:red}}" "@media screen{.a:before{color:red}}"

(* CSS Nesting 1 sec. 3 lets a nested rule start with an identifier, so its
   prelude is ambiguous with a declaration until the block appears:
   [h2:where(...)] reads as the property [h2] with value [where(...)]. Parsing
   has to fall back to a rule, or the whole nested block is dropped. *)
let test_nested_rule_starting_with_ident () =
  let check src expected =
    Alcotest.(check string) src expected (render (block src))
  in
  (* the shape that motivated this: element + pseudo-class *)
  check ".a{color:red;h2:where(:not(.x)){font-size:1px}}"
    ".a{color:red;h2:where(:not(.x)){font-size:1px}}";
  (* a bare element with a pseudo-class, and a pseudo-element *)
  check ".a{p:hover{color:red}}" ".a{p:hover{color:red}}";
  (* minify canonicalises the legacy one-colon form, as it does at top level *)
  check ".a{li::before{color:red}}" ".a{li:before{color:red}}";
  (* several in a row, after and between declarations: CSS Nesting 1 sec. 3.4
     keeps [height] where it was written, behind both nested rules *)
  check ".a{color:red;h2:hover{width:1px}h3:hover{width:2px};height:2px}"
    ".a{color:red;h2:hover{width:1px}h3:hover{width:2px}height:2px}";
  (* the shapes that already worked must keep working *)
  check ".a{&:hover{color:red}}" ".a{&:hover{color:red}}";
  check ".a{.b{color:red}}" ".a{.b{color:red}}";
  check ".a{h2+h3{color:red}}" ".a{h2+h3{color:red}}"

(* A declaration that is merely invalid has no block, so it still reports as a
   bad declaration rather than being retried as a selector. *)
let test_bad_declaration_still_a_declaration () =
  let stmts = block ".a{color:;width:1px}" in
  Alcotest.(check string)
    "the invalid declaration drops, the good one stays" ".a{width:1px}"
    (render stmts)

(* CSS Nesting 1 sec. 4: [&] stands for [:is(<parent selector list>)]. The
   wrapper is load-bearing whenever the parent carries a combinator and [&] does
   not head the selector, since the parent's own structure would otherwise
   escape into the surrounding context. *)
let test_nesting_wraps_complex_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  (* without the wrapper this reads as ".dark .a .b", which additionally demands
     that ".a" sit inside ".dark" *)
  check ".a .b{.dark &{color:red}}" ".dark :is(.a .b){color:red}";
  (* [&] in the middle, and [&] appended to a compound, escape the same way *)
  check ".a .b{.x & .y{color:red}}" ".x :is(.a .b) .y{color:red}";
  check ".a .b{.foo&{color:red}}" ".foo:is(.a .b){color:red}";
  (* a parent with no combinator matches and scores the same either way, so it
     goes in verbatim *)
  check ".a{.dark &{color:red}}" ".dark .a{color:red}";
  (* nothing can escape to the left of a leading [&], so no wrapper there *)
  check ".a .b{&:hover{color:red}}" ".a .b:hover{color:red}";
  check ".a .b{& .c{color:red}}" ".a .b .c{color:red}"

(* Selectors 4 sec. 4.1 puts the type or universal selector first in a compound,
   so a parent that starts with one cannot be spliced in after another simple
   selector: [.x&] under [a] would read back as a single class named [xa]. CSS
   Nesting 1 sec. 4 keeps the [:is()] wrapper for that case. *)
let test_nesting_wraps_type_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check "a{.x&{color:red}}" ".x:is(a){color:red}";
  check "a.c{.x&{color:red}}" ".x:is(a.c){color:red}";
  check "*{.x&{color:red}}" ".x:is(*){color:red}";
  (* [&] at the head of every compound leaves the type selector where it may
     stand, so the parent goes in bare *)
  check "a{&.x{color:red}}" "a.x{color:red}";
  check "a{&:hover{color:red}}" "a:hover{color:red}";
  check "a{.x &{color:red}}" ".x a{color:red}";
  (* a parent with no type selector to lead needs no wrapper either way *)
  check ".c{.x&{color:red}}" ".x.c{color:red}"

(* CSS Nesting 1 sec. 3: a nested selector list is relative to the parent branch
   by branch. Combining the parent with the list as a whole put the combinator
   on the first branch only, and every later branch escaped as a top-level
   selector -- [.p { a, b { ... } }] matched every [b] on the page. *)
let test_nested_selector_list_keeps_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check ".p{a,b{color:red}}" ".p a,.p b{color:red}";
  check ".p{a::before,a::after{color:red}}" ".p a:before,.p a:after{color:red}";
  check ".p{& a,& b{color:red}}" ".p a,.p b{color:red}"

(* CSS Nesting 1 sec. 4 desugars [&] to the parent rule's selector wrapped in
   [:is()], and reads a nested selector with no [&] as if it had a leading one.
   A parent selector list spliced in bare loses that grouping: [.a, .b] under
   [&:hover] reads back as the two branches [.a] and [.b:hover], so the
   pseudo-class binds to the last branch alone and the first matches unguarded.
   Selectors 4 sec. 4.2 weighs [:is()] as its most specific argument, so the
   wrapper may go only where the whole selector is the parent list and its
   branches already agree on one specificity. *)
let test_list_parent_keeps_its_branches () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check ".a,.b{&:hover{color:red}}" ":is(.a,.b):hover{color:red}";
  check ".a,.b{.c{color:red}}" ":is(.a,.b) .c{color:red}";
  check ".a,.b{>.c{color:red}}" ":is(.a,.b)>.c{color:red}";
  (* every branch of a nested list carries the whole parent *)
  check ".a,.b{.c,&:hover{color:red}}"
    ":is(.a,.b) .c,:is(.a,.b):hover{color:red}";
  (* a second level nests inside the wrapper the first one built *)
  check ".a,.b{&:hover{&:focus{color:red}}}" ":is(.a,.b):hover:focus{color:red}";
  (* [&] alone spells the parent list itself, at the weight of each branch *)
  check ".a,.b{&{color:red}}" ".a,.b{color:red}";
  check ".a,#b{&{color:red}}" ":is(.a,#b){color:red}"

(* [@-moz-document] groups rules like any other conditional at-rule, so nesting
   inside one flattens and a rule wrapping one keeps its selector: emitting the
   at-rule verbatim from inside a rule drops the parent it was written under. *)
let test_flattens_inside_moz_document () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check "@-moz-document url-prefix(){.a{.b{color:red}}}"
    "@-moz-document url-prefix(){.a .b{color:red}}";
  check ".a{@-moz-document url-prefix(){.b{color:red}}}"
    "@-moz-document url-prefix(){.a .b{color:red}}"

(* CSS Nesting 1 sec. 3.3: any at-rule whose body carries style rules nests
   inside a style rule, so flattening one has to carry the parent selector into
   its declaration run. A group rule flatten does not know leaves the run bare
   at the top of the block, which no reader takes back. *)
let test_nested_group_rules_keep_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check ".a{@starting-style{color:red}}" "@starting-style{.a{color:red}}";
  check ".a{@-moz-document url-prefix(){color:red}}"
    "@-moz-document url-prefix(){.a{color:red}}";
  check ".a{@when media(width>0px){color:red}}"
    "@when media(width>0px){.a{color:red}}";
  check ".a{@layer n{@media screen{color:red}}}"
    "@layer n{@media screen{.a{color:red}}}"

(* CSS Cascade 6 sec. 3.5.3: a style rule directly in an [@scope] block whose
   selector holds neither [&] nor [:scope] is relative, with the scoping root
   and a descendant combinator implied at its start, and sec. 3.5.1 makes [&]
   there [:where(:scope)]. So the parent [&] stands for in a rule nested under
   it is [& <parent>], and a flat selector that does not start with the parent
   has to say so, or the implied root lands in front of what comes before it.
   Chromium 153 styles an [.a4] inside [.c4] inside [.a4] under [@scope
   (.c4){:is(.a4,.b4){.a4 &{...}}}], and the same element under [@scope
   (.c6){.a6 :is(& :is(.k6)){...}}]. *)
let test_scope_keeps_the_implied_root () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check "@scope (.c){:is(.a,.b){.a &{grid-row:3}}}"
    "@scope(.c){.a :is(& :is(.a,.b)){grid-row:3}}";
  check "@scope (.c){.a{.x &{color:red}}}" "@scope(.c){.x :is(& .a){color:red}}";
  (* A selector that starts with its parent keeps the root implied. *)
  check "@scope (.c){.a{.b{color:red}}}" "@scope(.c){.a .b{color:red}}";
  check "@scope (.c){.a{&:hover{color:red}}}" "@scope(.c){.a:hover{color:red}}";
  check "@scope (.c){.a{>.b{color:red}}}" "@scope(.c){.a>.b{color:red}}"

(* Chromium 153 reads the prelude of an [@scope] nested in a style rule as a
   nested selector: [.a{@scope (.b){...}}] roots at a [.b] inside an [.a], not
   at an element that is both. Its block stays an [@scope] block: declarations
   in it match the scoping root (CSS Cascade 6 sec. 3.5.6), and Chromium colours
   that [.b], and its rules are relative to the root, not to [.a]. *)
let test_nested_scope_roots_under_the_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check ".a{@scope (.b){color:red}}" "@scope(.a .b){color:red}";
  check ".a{@scope (.b){.x{color:red}}}" "@scope(.a .b){.x{color:red}}";
  check ".a{@scope (& > .b){.x{color:red}}}" "@scope(.a>.b){.x{color:red}}"

(* Selectors 4 sec. 3.9 drops a whole selector list over one branch a browser
   does not parse, and with the parent rule go the rules nested in it. The
   [:is()] that composes a nested selector (CSS Nesting 1 sec. 3) is forgiving
   (Selectors 4 sec. 4.2): Chromium 153 drops [.c,[x=a s]{&:hover{..}}] whole,
   and keeps [:is(.c,[x=a s]):hover] and colours [.c:hover]. So each branch some
   target drops is listed again beside the nested selector, matching nothing
   under a [:not()] of the universal selector: a browser that drops the branch
   drops the rule, as it dropped its parent, and one that reads it reads the
   [:is()] at its own weight. A parent every target reads takes no extra
   branch. *)
let test_list_parent_some_target_drops () =
  let check name css expected =
    Alcotest.(check string) name expected (render (Flatten.block (block css)))
  in
  check "a branch only Firefox reads"
    ".c,[x=a s]{color:red;&:hover{color:blue}}"
    ".c,[x=a s]{color:red}:is(.c,[x=a s]):hover,[x=a s]:not(*){color:blue}";
  check "branches of different weights" "#a,[x=a s]{&:hover{color:blue}}"
    ":is(#a,[x=a s]):hover,[x=a s]:not(*){color:blue}";
  check "a descendant" ".c,:-moz-focusring{.d{color:blue}}"
    ":is(.c,:-moz-focusring) .d,:-moz-focusring:not(*){color:blue}";
  check "a parent every target reads" ".c,[x=a i]{&:hover{color:blue}}"
    ":is(.c,[x=a i]):hover{color:blue}"

let suite =
  ( "flatten",
    [
      Alcotest.test_case "no nesting is identity" `Quick
        test_no_nesting_is_identity;
      Alcotest.test_case "flattens descendant nesting" `Quick
        test_flattens_descendant_nesting;
      Alcotest.test_case "empty block stays empty" `Quick
        test_empty_block_is_empty;
      Alcotest.test_case "pseudo-element parent drops invalid nesting" `Quick
        test_pseudo_element_parent_drops_invalid_nesting;
      Alcotest.test_case "pseudo-element parent drops an invalid compound"
        `Quick test_pseudo_element_parent_drops_invalid_compound;
      Alcotest.test_case "nesting selector names no pseudo-element" `Quick
        test_nesting_selector_names_no_pseudo_element;
      Alcotest.test_case "scope keeps the implied root" `Quick
        test_scope_keeps_the_implied_root;
      Alcotest.test_case "nested scope roots under the parent" `Quick
        test_nested_scope_roots_under_the_parent;
      Alcotest.test_case "nested rule starting with an ident" `Quick
        test_nested_rule_starting_with_ident;
      Alcotest.test_case "bad declaration stays a declaration" `Quick
        test_bad_declaration_still_a_declaration;
      Alcotest.test_case "nesting wraps a complex parent" `Quick
        test_nesting_wraps_complex_parent;
      Alcotest.test_case "nesting wraps a type parent" `Quick
        test_nesting_wraps_type_parent;
      Alcotest.test_case "nested selector list keeps the parent" `Quick
        test_nested_selector_list_keeps_parent;
      Alcotest.test_case "list parent keeps its branches" `Quick
        test_list_parent_keeps_its_branches;
      Alcotest.test_case "flattens inside @-moz-document" `Quick
        test_flattens_inside_moz_document;
      Alcotest.test_case "nested group rules keep the parent" `Quick
        test_nested_group_rules_keep_parent;
      Alcotest.test_case "list parent some target drops" `Quick
        test_list_parent_some_target_drops;
    ] )
