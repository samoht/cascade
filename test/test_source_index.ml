(* The index exists so a rewriter can cut the author's own bytes, so every test
   here checks an offset against the source it came from rather than against a
   reprint. The cases that matter are the ones a brace count gets wrong. *)
module Index = Cascade.Source_index

let at_rule name css i =
  match Index.at_rule (Index.v css) ~name i with
  | Some h -> h
  | None -> Alcotest.failf "no %s at %d in %S" name i css

let test_at_rule_block () =
  let css = "@media print { .a { color: red } }" in
  let h = at_rule "@media" css 0 in
  Alcotest.(check string) "prelude" "print" h.prelude;
  Alcotest.(check string) "body" " .a { color: red } " h.block.body;
  Alcotest.(check int) "next is past the close" (String.length css) h.block.next

(* A [{] inside a string is not a block opener. Counting braces reads the body
   as ending at the [}] in the string; the parser does not. *)
let test_brace_in_string () =
  let css = "@media print { .a { content: \"{\" } }" in
  let h = at_rule "@media" css 0 in
  Alcotest.(check string)
    "body keeps the whole rule" " .a { content: \"{\" } " h.block.body

let test_brace_in_comment () =
  let css = "@media print { /* } */ .a { color: red } }" in
  let h = at_rule "@media" css 0 in
  Alcotest.(check string)
    "body keeps the whole rule" " /* } */ .a { color: red } " h.block.body

(* A [;] inside a group does not end the at-rule, so the prelude runs to the
   brace rather than to the first semicolon. *)
let test_semicolon_in_prelude_group () =
  let css = "@supports (a: b; c) { .a { color: red } }" in
  let h = at_rule "@supports" css 0 in
  Alcotest.(check string) "prelude" "(a: b; c)" h.prelude

let test_blockless_statement () =
  let css = "@import \"x\" theme(static) source(none);\n.a{}" in
  match Index.at_statement (Index.v css) ~name:"@import" 0 with
  | None -> Alcotest.fail "no @import statement at 0"
  | Some s ->
      Alcotest.(check string)
        "prelude" " \"x\" theme(static) source(none)" s.prelude;
      Alcotest.(check int) "next is past the semicolon" 39 s.next

(* A blockless at-rule that a closing brace terminates reports that brace, so a
   splice does not swallow it. *)
let test_blockless_closed_by_brace () =
  let css = "@media print { @layer a }" in
  match Index.at_statement (Index.v css) ~name:"@layer" 15 with
  | None -> Alcotest.fail "no @layer statement at 15"
  | Some s ->
      Alcotest.(check string) "prelude" " a " s.prelude;
      Alcotest.(check char) "next points at the brace" '}' css.[s.next]

let test_call () =
  let css = ".a { width: theme(--spacing) }" in
  match Index.call (Index.v css) ~name:"theme" 12 with
  | None -> Alcotest.fail "no theme() call at 12"
  | Some b ->
      Alcotest.(check string) "arguments" "--spacing" b.body;
      Alcotest.(check char) "next is past the close" ' ' css.[b.next]

let test_call_without_arguments () =
  let css = ".a { width: f() }" in
  match Index.call (Index.v css) ~name:"f" 12 with
  | None -> Alcotest.fail "no f() call at 12"
  | Some b -> Alcotest.(check string) "empty arguments" "" b.body

let test_calls_and_statements_collect () =
  let css = "@import \"a\";@import \"b\";.x{width:f(1);height:f(2)}" in
  let index = Index.v css in
  Alcotest.(check int)
    "two imports" 2
    (List.length (Index.at_statements index ~name:"@import"));
  Alcotest.(check int) "two calls" 2 (List.length (Index.calls index ~name:"f"));
  Alcotest.(check int)
    "no @media" 0
    (List.length (Index.at_statements index ~name:"@media"))

(* An unterminated group ends where the source does, which is where the parser
   ends it too. *)
let test_unclosed_block () =
  let css = "@media print { .a { color: red }" in
  let h = at_rule "@media" css 0 in
  Alcotest.(check string)
    "body runs to the end" " .a { color: red }" h.block.body

let test_name_must_match () =
  let css = "@media print { }" in
  let index = Index.v css in
  Alcotest.(check bool)
    "wrong name" true
    (Index.at_rule index ~name:"@supports" 0 = None);
  Alcotest.(check bool)
    "a blockful at-rule is not a statement" true
    (Index.at_statement index ~name:"@media" 0 = None);
  Alcotest.(check bool)
    "nothing starts at 1" true
    (Index.at_rule index ~name:"@media" 1 = None)

let suite =
  ( "source_index",
    [
      Alcotest.test_case "at-rule block" `Quick test_at_rule_block;
      Alcotest.test_case "brace inside a string" `Quick test_brace_in_string;
      Alcotest.test_case "brace inside a comment" `Quick test_brace_in_comment;
      Alcotest.test_case "semicolon inside a prelude group" `Quick
        test_semicolon_in_prelude_group;
      Alcotest.test_case "blockless statement" `Quick test_blockless_statement;
      Alcotest.test_case "blockless closed by a brace" `Quick
        test_blockless_closed_by_brace;
      Alcotest.test_case "call arguments" `Quick test_call;
      Alcotest.test_case "call without arguments" `Quick
        test_call_without_arguments;
      Alcotest.test_case "collecting calls and statements" `Quick
        test_calls_and_statements_collect;
      Alcotest.test_case "unclosed block" `Quick test_unclosed_block;
      Alcotest.test_case "name and offset must both match" `Quick
        test_name_must_match;
    ] )
