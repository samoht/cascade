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

(* CSS Nesting 1 sec. 2 lets a nested rule start with an identifier, so its
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
  (* several in a row, after and between declarations *)
  check ".a{color:red;h2:hover{width:1px}h3:hover{width:2px};height:2px}"
    ".a{color:red;height:2px;h2:hover{width:1px}h3:hover{width:2px}}";
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

(* CSS Nesting 1 sec. 2.1: [&] stands for [:is(<parent selector list>)]. The
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

(* CSS Nesting 1 §2: a nested selector list is relative to the parent branch by
   branch. Combining the parent with the list as a whole put the combinator on
   the first branch only, and every later branch escaped as a top-level selector
   — [.p { a, b { ... } }] matched every [b] on the page. *)
let test_nested_selector_list_keeps_parent () =
  let check src expected =
    Alcotest.(check string) src expected (render (Flatten.block (block src)))
  in
  check ".p{a,b{color:red}}" ".p a,.p b{color:red}";
  check ".p{a::before,a::after{color:red}}" ".p a:before,.p a:after{color:red}";
  check ".p{& a,& b{color:red}}" ".p a,.p b{color:red}"

let suite =
  ( "flatten",
    [
      Alcotest.test_case "no nesting is identity" `Quick
        test_no_nesting_is_identity;
      Alcotest.test_case "flattens descendant nesting" `Quick
        test_flattens_descendant_nesting;
      Alcotest.test_case "empty block stays empty" `Quick
        test_empty_block_is_empty;
      Alcotest.test_case "nested rule starting with an ident" `Quick
        test_nested_rule_starting_with_ident;
      Alcotest.test_case "bad declaration stays a declaration" `Quick
        test_bad_declaration_still_a_declaration;
      Alcotest.test_case "nesting wraps a complex parent" `Quick
        test_nesting_wraps_complex_parent;
      Alcotest.test_case "nested selector list keeps the parent" `Quick
        test_nested_selector_list_keeps_parent;
    ] )
