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

(* The reader rewrites the input before parsing (CSS Syntax 3 sec. 3.3
   "Preprocessing the input stream"): a CRLF becomes one LF, a leading BOM goes
   and a NUL becomes the three bytes of U+FFFD. The offsets must still index the
   caller's string, not the rewritten one. *)

let statements name css =
  Index.at_statements (Index.v css) ~name
  |> List.map (fun (i, (s : Index.statement)) -> (i, s.prelude, s.next))
  |> List.sort (fun (i, _, _) (j, _, _) -> Int.compare i j)

let calls name css =
  Index.calls (Index.v css) ~name
  |> List.map (fun (i, (b : Index.block)) -> (i, b.body, b.next))
  |> List.sort (fun (i, _, _) (j, _, _) -> Int.compare i j)

let found = Alcotest.(list (triple int string int))

let check_header css ~at ~prelude ~brace ~body ~next =
  let h = at_rule "@media" css at in
  Alcotest.(check string) "prelude" prelude h.prelude;
  Alcotest.(check int) "brace" brace h.brace;
  Alcotest.(check string) "body" body h.block.body;
  Alcotest.(check int) "next" next h.block.next

(* @import 0..33, ';' 34, CR 35, LF 36, @source 37, inline 45, its ')' 63, ';'
   64. *)
let test_crlf_statement () =
  let css =
    "@import \"tailwindcss\" source(none);\r\n@source inline(\"underline\");\n"
  in
  Alcotest.check found "@import"
    [ (0, " \"tailwindcss\" source(none)", 35) ]
    (statements "@import" css);
  Alcotest.check found "@source"
    [ (37, " inline(\"underline\")", 65) ]
    (statements "@source" css);
  Alcotest.check found "source()" [ (22, "none", 34) ] (calls "source" css);
  Alcotest.check found "inline()"
    [ (45, "\"underline\"", 64) ]
    (calls "inline" css)

(* @media 6, '{' 19, ' .b { color: red } ' 20..38, '}' 39. *)
let test_crlf_block () =
  check_header ".a{}\r\n@media print { .b { color: red } }" ~at:6
    ~prelude:"print" ~brace:19 ~body:" .b { color: red } " ~next:40

(* @media 6, '{' 19, body 20..36 with a CRLF at each end, '}' 37, CRLF
   38..39. *)
let test_crlf_inside_block () =
  check_header ".a{}\r\n@media print {\r\n.b{color:red}\r\n}\r\n" ~at:6
    ~prelude:"print" ~brace:19 ~body:"\r\n.b{color:red}\r\n" ~next:38

(* theme 11, '(' 16, '--spacing' 17..25, ')' 26. *)
let test_crlf_call () =
  Alcotest.check found "theme()"
    [ (11, "--spacing", 27) ]
    (calls "theme" ".a{\r\nwidth:theme(--spacing)}")

(* BOM 0..2, @import 3, ';' 14, LF 15, @media 16, '{' 29, '}' 36. *)
let test_bom () =
  let css = "\xEF\xBB\xBF@import \"x\";\n@media print { .b{} }" in
  Alcotest.check found "@import"
    [ (3, " \"x\"", 15) ]
    (statements "@import" css);
  check_header css ~at:16 ~prelude:"print" ~brace:29 ~body:" .b{} " ~next:37

(* A NUL grows by two bytes in the rewrite. @import 0, NUL 9, ';' 11, LF 12,
   @layer 13, ';' 21. *)
let test_nul () =
  let css = "@import \"\x00\";\n@layer a;" in
  Alcotest.check found "@import"
    [ (0, " \"\x00\"", 12) ]
    (statements "@import" css);
  Alcotest.check found "@layer" [ (13, " a", 22) ] (statements "@layer" css)

(* The same sources with LF only, and with a lone CR or FF, which the rewrite
   maps one byte to one byte: nothing shifts. *)
let test_lf_control () =
  let css =
    "@import \"tailwindcss\" source(none);\n@source inline(\"underline\");\n"
  in
  Alcotest.check found "@import"
    [ (0, " \"tailwindcss\" source(none)", 35) ]
    (statements "@import" css);
  Alcotest.check found "@source"
    [ (36, " inline(\"underline\")", 64) ]
    (statements "@source" css);
  Alcotest.check found "inline()"
    [ (44, "\"underline\"", 63) ]
    (calls "inline" css);
  check_header ".a{}\n@media print { .b { color: red } }" ~at:5 ~prelude:"print"
    ~brace:18 ~body:" .b { color: red } " ~next:39;
  check_header ".a{}\r@media print {\x0C.b{}\r}" ~at:5 ~prelude:"print"
    ~brace:18 ~body:"\x0C.b{}\r" ~next:26

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
      Alcotest.test_case "CRLF before a statement" `Quick test_crlf_statement;
      Alcotest.test_case "CRLF before a block" `Quick test_crlf_block;
      Alcotest.test_case "CRLF inside a block" `Quick test_crlf_inside_block;
      Alcotest.test_case "CRLF before a call" `Quick test_crlf_call;
      Alcotest.test_case "leading BOM" `Quick test_bom;
      Alcotest.test_case "NUL in a string" `Quick test_nul;
      Alcotest.test_case "LF, lone CR and FF keep offsets" `Quick
        test_lf_control;
    ] )
