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
   after a pseudo-element, and nesting composes that compound out of a valid
   parent and a valid child just as it composes a combinator. Chrome 146 drops
   [a::before:hover], [a::selection:hover] and [a::before:focus] from cssRules
   and keeps [a::file-selector-button:hover], [a::part(p):hover] and
   [a::cue:hover]; cascade's reader draws the same line, so the flattener has to
   draw it too. *)
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
  (* The pseudo-elements that do take a user action keep it. *)
  check "::file-selector-button takes :hover"
    ".a::file-selector-button{&:hover{color:red}}"
    ".a::file-selector-button:hover{color:red}";
  check "::part() takes :hover" ".a::part(p){&:hover{color:red}}"
    ".a::part(p):hover{color:red}";
  check "::cue takes :hover" ".a::cue{&:hover{color:red}}"
    ".a::cue:hover{color:red}";
  (* CSS Pseudo-Elements 4 sec. 4.2 defines the ::marker of a ::before. *)
  check "::before takes ::marker" ".a::before{&::marker{color:red}}"
    ".a:before::marker{color:red}"

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
      Alcotest.test_case "nested rule starting with an ident" `Quick
        test_nested_rule_starting_with_ident;
      Alcotest.test_case "bad declaration stays a declaration" `Quick
        test_bad_declaration_still_a_declaration;
      Alcotest.test_case "nesting wraps a complex parent" `Quick
        test_nesting_wraps_complex_parent;
      Alcotest.test_case "nested selector list keeps the parent" `Quick
        test_nested_selector_list_keeps_parent;
      Alcotest.test_case "list parent keeps its branches" `Quick
        test_list_parent_keeps_its_branches;
      Alcotest.test_case "flattens inside @-moz-document" `Quick
        test_flattens_inside_moz_document;
      Alcotest.test_case "nested group rules keep the parent" `Quick
        test_nested_group_rules_keep_parent;
    ] )
