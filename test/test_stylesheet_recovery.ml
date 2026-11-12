(** Stylesheet tests: lenient parsing recovery and its warnings. *)

open Cascade
open Css.Stylesheet
open Stylesheet_helpers

let spec_lenient_recovery_stylesheets () =
  lenient_recover "bad declaration then good declaration"
    ".a { color: invalid-color; color: red }" ".a{color:red}" 1;
  lenient_recover "bad declaration keeps sibling rule"
    ".a { color: rgb(300); } .b { color: red }" ".b{color:red}" 1;
  (* CSS Syntax 3 sec. 5.4.2 consumes the at-rule and its block whatever the
     name, so what recovers here is the rule after it, not the at-rule. *)
  lenient_recover "unknown at-rule keeps its neighbour"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    "@unknown-rule{.bad{color:red}}.ok{color:#00f}" 1;
  lenient_recover "bad selector list drops rule only"
    ".ok { color: green } .bad:not() { color: red } .next { color: blue }"
    ".ok{color:green}.next{color:#00f}" 1;
  lenient_recover "unclosed block auto-closes" ".btn { color: red;"
    ".btn{color:red}" 0;
  lenient_recover "descriptor at-rule in a style rule keeps its neighbours"
    ".a { color: red; @font-face { font-family: F; src: url(f.woff2) } \
     background: blue }"
    ".a{color:red;background:#00f}" 1;
  lenient_recover "orphan else in a style rule keeps its neighbours"
    ".a { color: red; @else { color: green } background: blue }"
    ".a{color:red;background:#00f}" 1

(* CSS Syntax 3 sec. 5.4.4 "consume a style block's contents" ends an invalid
   declaration at the next top-level [;], and a [{}] met on the way is one
   component value of the value being skipped, not a stopping point. A nested
   at-rule's body is <block-contents> just like a style rule's (CSS Nesting 1
   sec. 3.3), so the same recovery applies inside it: Blink 146 drops the one
   declaration and keeps every neighbour, the enclosing group rule and the rest
   of the sheet. *)
let spec_lenient_recovery_nested_at_rule_declarations () =
  let recovered = ".a{@media screen{color:red;background:#00f}}" in
  lenient_recover "bad declaration first in a nested at-rule"
    ".a { @media screen { width: 10; color: red; background: blue } }" recovered
    1;
  lenient_recover "bad declaration mid-run in a nested at-rule"
    ".a { @media screen { color: red; width: 10; background: blue } }" recovered
    1;
  lenient_recover "bad declaration last in a nested at-rule"
    ".a { @media screen { color: red; background: blue; width: 10 } }" recovered
    1;
  lenient_recover "sole declaration of a nested at-rule dropped"
    ".a { @media screen { width: 10 } }" "" 1;
  lenient_recover "bad declaration in a nested style rule"
    ".a { & b { color: red; width: 10; margin: 0 } }" ".a b{color:red;margin:0}"
    1;
  lenient_recover "bad declaration in a style rule under a nested at-rule"
    ".a { @media screen { & b { color: red; width: 10; margin: 0 } } }"
    ".a{@media screen{& b{color:red;margin:0}}}" 1;
  lenient_recover "bad declaration in a doubly nested at-rule"
    ".a { @media screen { @container (width > 0px) { color: red; width: 10; \
     background: blue } } }"
    ".a{@media screen{@container(width>0px){color:red;background:#00f}}}" 1;
  lenient_recover "nested at-rule keeps recovery in @layer"
    ".a { @layer x { color: red; width: 10; background: blue } }"
    ".a{@layer x{color:red;background:#00f}}" 1;
  lenient_recover "nested at-rule keeps recovery in @scope"
    ".a { @scope (.b) { color: red; width: 10; background: blue } }"
    ".a{@scope(.b){color:red;background:#00f}}" 1;
  lenient_recover "nested at-rule keeps recovery in @starting-style"
    ".a { @starting-style { color: red; width: 10; background: blue } }"
    ".a{@starting-style{color:red;background:#00f}}" 1;
  (* A [{}] in the dropped value is consumed with it; the run resumes at the [;]
     after it. An unclosed function has no such [;] - the tokenizer closes it at
     the end of the block, so it swallows the rest, as Blink does. *)
  lenient_recover "curly block inside a dropped value is skipped with it"
    ".a { @media screen { color: red; width: {1}; background: blue } }"
    recovered 1;
  lenient_recover "unclosed function swallows the rest of the block"
    ".a { @media screen { color: red; width: calc(1px; background: blue } }"
    ".a{@media screen{color:red}}" 1;
  lenient_recover "a run of stray tokens is dropped like a bad declaration"
    ".a { @media screen { color: red; !!!; background: blue } }" recovered 1

(* A dropped declaration leaves no gap: the run written around it is one run, as
   it is in Blink 146, the same way a dropped at-rule leaves one. *)
let spec_lenient_recovery_nested_declaration_run () =
  lenient_recover "bad declaration immediately before a nested rule"
    ".a { @media screen { width: 10; & b { color: red } } }"
    ".a{@media screen{& b{color:red}}}" 1;
  lenient_recover "bad declaration immediately after a nested rule"
    ".a { @media screen { & b { color: red } width: 10; background: blue } }"
    ".a{@media screen{& b{color:red}background:#00f}}" 1;
  lenient_recover "dropped declaration does not seal the run before a rule"
    ".a { @media screen { color: red; width: 10; background: blue; & b { \
     margin: 0 } } }"
    ".a{@media screen{color:red;background:#00f;& b{margin:0}}}" 1;
  lenient_recover "dropped declaration does not seal the run it opened"
    ".a { @media screen { width: 10; color: red; & b { margin: 0 } background: \
     blue } }"
    ".a{@media screen{color:red;& b{margin:0}background:#00f}}" 1

(* [css] reports exactly [expected] warnings leniently, and [~strict:true]
   rejects it exactly when the lenient parse warned. *)
let warns_exactly css expected =
  let warnings =
    match Css.of_string ~strict:false css with
    | Ok { Css.warnings; _ } -> warnings
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" css
          (Cascade.Error.to_string err)
  in
  Alcotest.(check int) ("warnings for " ^ css) expected (List.length warnings);
  match (Css.of_string ~strict:true css, expected) with
  | Ok _, 0 -> ()
  | Error _, 0 -> Alcotest.failf "strict parse rejected warning-free %S" css
  | Ok _, _ -> Alcotest.failf "strict parse accepted warned-about %S" css
  | Error _, _ -> ()

(* Each recovered declaration reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. *)
let spec_recovery_warns_once_per_declaration () =
  let check = warns_exactly in
  check ".a { @media screen { color: red; width: 10; background: blue } }" 1;
  check ".a { @media screen { width: 10; height: 20; color: red } }" 2;
  check
    ".a { @media screen { @container (width > 0px) { width: 10; color: red } } \
     }"
    1;
  check ".a { @media screen { color: red; background: blue } }" 0

(* CSS Paged Media 3 sec. 4.1: "If an error is encountered during the processing
   of a declaration block within a page or a margin context, the Rules for
   handling parsing errors apply; that is, valid declarations within the block
   are applied." One bad descriptor therefore costs that descriptor, not the
   [@page] holding it and not the sheet holding that. The discard follows CSS
   Syntax 3 sec. 5.4.4 for a declaration - up to the next top-level [;], a [{}]
   met on the way counting as one component value of the value being skipped -
   and sec. 5.4.2 for an at-rule, which ends at its block or its [;]. Blink 146
   keeps both neighbours in every case below. *)
let spec_lenient_recovery_page_descriptors () =
  let recovered = "@page{margin:1cm;margin-top:2cm}" in
  lenient_recover "bad descriptor first in @page"
    "@page { width: 10; margin: 1cm; margin-top: 2cm }" recovered 1;
  lenient_recover "bad descriptor mid-body in @page"
    "@page { margin: 1cm; width: 10; margin-top: 2cm }" recovered 1;
  lenient_recover "bad descriptor last in @page"
    "@page { margin: 1cm; margin-top: 2cm; width: 10 }" recovered 1;
  lenient_recover "sole descriptor of @page dropped" "@page { width: 10 }" "" 1;
  lenient_recover "curly block inside a dropped page value is skipped with it"
    "@page { margin: 1cm; width: {1}; margin-top: 2cm }" recovered 1;
  lenient_recover "two curly blocks in a dropped page value are one skip"
    "@page { margin: 1cm; width: {1}{2}; margin-top: 2cm }" recovered 1;
  lenient_recover "tokens after a curly block in a dropped page value"
    "@page { margin: 1cm; width: {1} 2px; margin-top: 2cm }" recovered 1;
  (* An unclosed function has no top-level [;] left to stop at - the tokenizer
     closes it at the end of the block - so it takes the rest of the body, as it
     does in Blink. *)
  lenient_recover "unclosed function swallows the rest of the page body"
    "@page { margin: 1cm; width: calc(1px; margin-top: 2cm }"
    "@page{margin:1cm}" 1;
  (* A selector-shaped item is not a declaration, so sec. 5.4.4 skips it as a
     bad one: with no [;] after its block it takes the body's tail with it, and
     with a [;] it costs only itself. Blink 146 splits the two the same way. *)
  lenient_recover "selector-shaped item without a semicolon takes the tail"
    "@page { margin: 1cm; .a { b: c } margin-top: 2cm }" "@page{margin:1cm}" 1;
  lenient_recover "selector-shaped item with a semicolon costs only itself"
    "@page { margin: 1cm; .a { b: c }; margin-top: 2cm }" recovered 1;
  (* An at-rule ends at its block, so discarding an invalid one leaves the
     descriptor written after it in the page. *)
  lenient_recover "unknown page margin rule is dropped alone"
    "@page { @bogus-box { content: \"x\" } margin: 1cm; margin-top: 2cm }"
    recovered 1;
  lenient_recover "at-rule that is no page margin rule is dropped alone"
    "@page { margin: 1cm; @media screen { color: red } margin-top: 2cm }"
    recovered 1;
  lenient_recover "page margin rule with no block ends at its semicolon"
    "@page { margin: 1cm; @top-center; margin-top: 2cm }" recovered 1

(* A page-margin box holds a declaration block of its own (CSS Paged Media 3
   sec. 5), so sec. 4.1 applies inside it too: the descriptors written around a
   bad one stay in the box, and the box stays in the [@page]. *)
let spec_lenient_recovery_page_margin_box () =
  let recovered = "@page{@top-center{content:\"x\";margin:0}}" in
  lenient_recover "bad descriptor in a page margin box"
    "@page { @top-center { content: \"x\"; width: 10; margin: 0 } }" recovered 1;
  lenient_recover "at-rule in a page margin box ends at its block"
    "@page { @top-center { @media screen { a: b } content: \"x\"; margin: 0 } }"
    recovered 1;
  lenient_recover "selector-shaped item in a margin box with a semicolon"
    "@page { @top-center { content: \"x\"; .a { b: c }; margin: 0 } }" recovered
    1;
  (* The box outlives its only item, as it does in Blink 146, and what is left
     is an empty box: nothing to paint, so it is elided on output like any other
     block that applies nothing, and the page it empties goes with it. *)
  lenient_recover "selector-shaped item alone in a page margin box"
    "@page { @top-center { .a { b: c } } }" "" 1

(* A dropped descriptor leaves no gap: the page keeps the descriptors written
   around it in source order, and its margin boxes keep theirs. *)
let spec_lenient_recovery_page_body_order () =
  lenient_recover "bad descriptor before a page margin box"
    "@page { margin: 1cm; width: 10; @top-center { content: \"x\" } \
     margin-top: 2cm }"
    "@page{margin:1cm;margin-top:2cm;@top-center{content:\"x\"}}" 1;
  lenient_recover "bad descriptor after a page margin box"
    "@page { margin: 1cm; @top-center { content: \"x\" } width: 10; \
     margin-top: 2cm }"
    "@page{margin:1cm;margin-top:2cm;@top-center{content:\"x\"}}" 1;
  lenient_recover "bad descriptor between two page margin boxes"
    "@page { @top-center { content: \"x\" } width: 10; @bottom-center { \
     content: \"y\" } margin: 1cm }"
    "@page{margin:1cm;@top-center{content:\"x\"}@bottom-center{content:\"y\"}}"
    1

(* Each dropped page descriptor reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. A stray [;] is discarded without a
   declaration to validate (CSS Syntax 3 sec. 5.4.3), so it costs nothing. *)
let spec_page_recovery_warns_once_per_descriptor () =
  warns_exactly "@page { margin: 1cm; width: 10; margin-top: 2cm }" 1;
  warns_exactly "@page { margin: 1cm; width: {1}{2}; margin-top: 2cm }" 1;
  warns_exactly "@page { width: 10; margin: 1cm; height: 20 }" 2;
  warns_exactly "@page { @top-center { content: \"x\"; width: 10; margin: 0 } }"
    1;
  warns_exactly "@page { margin: 1cm; margin-top: 2cm }" 0;
  warns_exactly "@page { margin: 1cm;; margin-top: 2cm }" 0;
  warns_exactly "@page { ; margin: 1cm }" 0;
  warns_exactly "@page { @top-center { content: \"x\" } margin: 1cm }" 0;
  (* A name no module defines in a page context is undefined there, not invalid,
     so it neither warns nor is dropped - the same treatment cascade gives an
     unknown property name in a style rule. *)
  warns_exactly "@page { margin: 1cm; zzz: 1; margin-top: 2cm }" 0;
  warns_exactly "@page { @top-center { content: \"x\"; zzz: 1; margin: 0 } }" 0

(* CSS Properties and Values API 1 sec. 2: [@property] holds the [syntax],
   [inherits] and [initial-value] descriptors, and "unknown descriptors are
   invalid and ignored". Dropping one costs that descriptor, not the
   registration and not the sheet holding it. The discard follows CSS Syntax 3
   sec. 5.4.4 for a declaration - up to the next top-level [;], a [{}] met on
   the way counting as one component value of the value being skipped - and sec.
   5.4.2 for an at-rule, which ends at its block or its [;]. Blink 146 keeps the
   registration in every case below. *)
let spec_lenient_recovery_property_descriptors () =
  let registered =
    "@property --x{syntax:\"<length>\";inherits:false;initial-value:0}"
  in
  let body rest = "@property --x { syntax: \"<length>\"; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @property"
    "@property --x { zzz: 1; syntax: \"<length>\"; inherits: false; \
     initial-value: 0px }"
    registered 1;
  lenient_recover "unknown descriptor mid-body in @property"
    (body "zzz: 1; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "unknown descriptor last in @property"
    (body "inherits: false; initial-value: 0px; zzz: 1")
    registered 1;
  lenient_recover "curly block inside a dropped descriptor value"
    (body "zzz: {1}; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "two curly blocks in a dropped descriptor value are one skip"
    (body "zzz: {1}{2}; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "tokens after a curly block in a dropped descriptor value"
    (body "zzz: {1} 2px; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "a run of stray tokens is dropped like a bad descriptor"
    (body "!!!; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "selector-shaped item in @property is dropped alone"
    (body ".a { b: c }; inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "at-rule in @property ends at its block"
    (body "@media screen { color: red } inherits: false; initial-value: 0px")
    registered 1;
  lenient_recover "at-rule with no block in @property ends at its semicolon"
    (body "@media screen; inherits: false; initial-value: 0px")
    registered 1;
  (* A descriptor's value is its whole declaration, so anything left over makes
     the declaration invalid rather than the leftover alone. Losing [inherits]
     that way leaves the registration incomplete, which drops it - as it does in
     Blink. *)
  lenient_recover "trailing tokens invalidate the descriptor they follow"
    (body "inherits: false bogus; initial-value: 0px")
    "" 1;
  lenient_recover "an important flag invalidates the descriptor it follows"
    (body "inherits: false !important; initial-value: 0px")
    "" 1;
  lenient_recover "a later descriptor still overrides the one dropped"
    (body "inherits: false !important; initial-value: 0px; inherits: true")
    "@property --x{syntax:\"<length>\";inherits:true;initial-value:0}" 1

(* CSS Syntax 3 sec. 5.4.3 "consume a block's contents" discards a [;] that no
   declaration precedes rather than validating one, so a stray semicolon in an
   [@property] body costs nothing. Blink 146 reads all three of these. *)
let spec_property_skips_stray_semicolons () =
  let registered =
    "@property --x{syntax:\"<length>\";inherits:false;initial-value:0}"
  in
  lenient_recover "leading semicolon in @property"
    "@property --x { ; syntax: \"<length>\"; inherits: false; initial-value: \
     0px }"
    registered 0;
  lenient_recover "doubled semicolon in @property"
    "@property --x { syntax: \"<length>\";; inherits: false; initial-value: \
     0px }"
    registered 0;
  lenient_recover "trailing semicolon in @property"
    "@property --x { syntax: \"<length>\"; inherits: false; initial-value: \
     0px; }"
    registered 0

(* Each dropped [@property] descriptor reports once, and [~strict:true] rejects
   exactly the inputs the lenient parse warned about. *)
let spec_property_recovery_warns_once_per_descriptor () =
  let body rest = "@property --x { syntax: \"<length>\"; " ^ rest ^ " }" in
  warns_exactly (body "zzz: 1; inherits: false; initial-value: 0px") 1;
  warns_exactly (body "zzz: {1}{2}; inherits: false; initial-value: 0px") 1;
  warns_exactly (body "zzz: 1; yyy: 2; inherits: false; initial-value: 0px") 2;
  warns_exactly (body "inherits: false; initial-value: 0px") 0;
  warns_exactly (body ";; inherits: false; initial-value: 0px") 0

(* A rule that fails to parse inside a grouping at-rule's block ends where CSS
   Syntax 3 says its kind ends: an at-rule at its block or its [;] (sec. 5.4.2),
   a qualified rule at its block (sec. 5.4.3). A [(] or a [[] met before that
   block is a component value of the prelude, so the rule being discarded runs
   on past it. Stopping there instead would leave the tail of the prelude to be
   read as a rule of its own, and Blink 146 keeps no such rule: for each input
   below it reads back only the rule named in the expectation. *)
let spec_lenient_recovery_block_statements () =
  lenient_recover "bad @supports prelude in @media ends at its block"
    "@media screen { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "bad @supports prelude in @layer ends at its block"
    "@layer base { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    "@layer base{b{color:#00f}}" 1;
  lenient_recover "bad @supports prelude in @supports ends at its block"
    "@supports (color: red) { @supports (display: grid) bogus { a { color: red \
     } } b { color: blue } }"
    "@supports(color:red){b{color:#00f}}" 1;
  (* Media Queries 5 sec. 3.2 replaces a query that does not match the grammar
     with [not all] rather than dropping the rule, so the rule and its contents
     survive and match nothing. [@supports] and [@container] above still drop:
     their own specifications are read separately. *)
  lenient_recover "bad @media prelude in @media is replaced by not all"
    "@media screen { @media (min-width: 1px) and { a { color: red } } b { \
     color: blue } }"
    "@media screen{@media not all{a{color:red}}b{color:#00f}}" 1;
  (* The replacement is per entry of the comma-separated list, so the queries
     beside the bad one still say what they said. *)
  lenient_recover "only the bad query of a list is replaced"
    "@media ,(min-width: 10px) { a { color: red } }"
    "@media not all,(width>=10px){a{color:red}}" 1;
  lenient_recover "a list of empty queries is one not all each"
    "@media all,,all { a { color: red } }"
    "@media all,not all,all{a{color:red}}" 1;
  lenient_recover "bad @container prelude in @media ends at its block"
    "@media screen { @container (min-width: 1px) !! { a { color: red } } b { \
     color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "at-rule with no block in @media ends at its semicolon"
    "@media screen { @supports (display: grid) bogus; b { color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "bad selector holding a [] in @media ends at its block"
    "@media screen { a[href=] { color: red } p { color: blue } }"
    "@media screen{p{color:#00f}}" 1;
  lenient_recover "bad selector holding a [] in @layer ends at its block"
    "@layer base { a[href=] { color: red } p { color: blue } }"
    "@layer base{p{color:#00f}}" 1;
  lenient_recover "bad selector opening on a () in @media ends at its block"
    "@media screen { (foo) bar { a { color: red } } b { color: blue } }"
    "@media screen{b{color:#00f}}" 1;
  lenient_recover "two bad statements in one block cost only themselves"
    "@media screen { @supports (display: grid) bogus { a { color: red } } \
     @container (min-width: 1px) !! { c { color: lime } } b { color: blue } }"
    "@media screen{b{color:#00f}}" 2

(* Each statement dropped from a grouping at-rule's block reports once: the tail
   of its prelude is part of what is discarded, not a second rule to be read and
   rejected on its own. *)
let spec_block_recovery_warns_once_per_statement () =
  warns_exactly
    "@media screen { @supports (display: grid) bogus { a { color: red } } b { \
     color: blue } }"
    1;
  warns_exactly
    "@media screen { @container (min-width: 1px) !! { a { color: red } } b { \
     color: blue } }"
    1;
  warns_exactly
    "@media screen { @supports (display: grid) bogus; b { color: blue } }" 1;
  warns_exactly "@media screen { a[href=] { color: red } p { color: blue } }" 1;
  warns_exactly "@layer base { a[href=] { color: red } p { color: blue } }" 1;
  warns_exactly
    "@media screen { @supports (display: grid) bogus { a { color: red } } \
     @container (min-width: 1px) !! { c { color: lime } } b { color: blue } }"
    2;
  warns_exactly
    "@media screen { @supports (display: grid) { a { color: red } } b { color: \
     blue } }"
    0

(* CSS Counter Styles 3 sec. 3 gives [@counter-style] a block of descriptor
   declarations, so CSS Syntax 3 sec. 5.4.3 keeps what that block already
   yielded when one item fails to parse: a descriptor cascade rejects costs that
   descriptor, not the [@counter-style] holding it and not the sheet holding
   that. The discard follows sec. 5.4.4 for a declaration - up to the next
   top-level [;], a [{}] met on the way counting as one component value of the
   value being skipped - and sec. 5.4.2 for an at-rule, which ends at its block
   or its [;]. Blink 146 keeps the rule in every case below. *)
let spec_lenient_recovery_counter_style_descriptors () =
  let recovered =
    "@counter-style c{system:cyclic;symbols:\"a\";suffix:\" \"}"
  in
  let shorter = "@counter-style c{system:cyclic;symbols:\"a\"}" in
  let body rest = "@counter-style c { system: cyclic; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @counter-style"
    "@counter-style c { zzz: 1; system: cyclic; symbols: \"a\"; suffix: \" \" }"
    recovered 1;
  lenient_recover "unknown descriptor mid-body in @counter-style"
    (body "zzz: 1; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover "unknown descriptor last in @counter-style"
    (body "symbols: \"a\"; suffix: \" \"; zzz: 1")
    recovered 1;
  lenient_recover "bad value for a known @counter-style descriptor"
    (body "symbols: \"a\"; suffix: !!!")
    shorter 1;
  lenient_recover "two curly blocks in a dropped counter-style value"
    (body "zzz: {1}{2}; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  (* A descriptor's value is the whole of its declaration, and no descriptor
     takes an [!important] flag, so the declaration it follows is invalid rather
     than the flag alone. *)
  lenient_recover "an important flag invalidates the counter-style descriptor"
    (body "symbols: \"a\"; suffix: \" \" !important")
    shorter 1;
  lenient_recover "an important flag on an opaque counter-style descriptor"
    (body "symbols: \"a\"; pad: 2 \"0\" !important; suffix: \" \"")
    recovered 1;
  lenient_recover "at-rule in @counter-style ends at its block"
    (body "@media screen { color: red } symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover
    "at-rule with no block in @counter-style ends at its semicolon"
    (body "@media screen; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  lenient_recover "selector-shaped item in @counter-style with a semicolon"
    (body ".a { b: c }; symbols: \"a\"; suffix: \" \"")
    recovered 1;
  (* Sec. 5.4.4 skips a bad declaration to the next top-level [;], and a
     selector-shaped item has none of its own, so it takes the tail of the body
     with it. Blink 146 splits the two the same way. *)
  lenient_recover "selector-shaped item in @counter-style takes the tail"
    (body "symbols: \"a\"; .a { b: c } suffix: \" \"")
    shorter 1;
  lenient_recover "a dropped counter-style descriptor keeps the sheet whole"
    (".a { color: red } "
    ^ body "zzz: 1; symbols: \"a\"; suffix: \" \""
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS Fonts 4 sec. 12.1 gives [@font-palette-values] a block of descriptor
   declarations, so one descriptor cascade rejects costs that descriptor alone.
   A descriptor's value is the whole of its declaration: a trailing [!important]
   or a stray ident makes the declaration invalid rather than the leftover
   alone, as it does in Blink 146. *)
let spec_lenient_recovery_font_palette_descriptors () =
  let recovered = "@font-palette-values --p{font-family:X;base-palette:1}" in
  let body rest = "@font-palette-values --p { font-family: X; " ^ rest ^ " }" in
  lenient_recover "unknown descriptor first in @font-palette-values"
    "@font-palette-values --p { zzz: 1; font-family: X; base-palette: 1 }"
    recovered 1;
  lenient_recover "unknown descriptor mid-body in @font-palette-values"
    (body "zzz: 1; base-palette: 1")
    recovered 1;
  lenient_recover "unknown descriptor last in @font-palette-values"
    (body "base-palette: 1; zzz: 1")
    recovered 1;
  lenient_recover "bad value for a known @font-palette-values descriptor"
    (body "base-palette: -1; base-palette: 1")
    recovered 1;
  lenient_recover "two curly blocks in a dropped font-palette value"
    (body "zzz: {1}{2}; base-palette: 1")
    recovered 1;
  lenient_recover "trailing tokens invalidate the font-palette descriptor"
    (body "base-palette: 2 bogus; base-palette: 1")
    recovered 1;
  lenient_recover "an important flag invalidates the font-palette descriptor"
    (body "base-palette: 2 !important; base-palette: 1")
    recovered 1;
  lenient_recover "at-rule in @font-palette-values ends at its block"
    (body "@media screen { color: red } base-palette: 1")
    recovered 1;
  lenient_recover
    "at-rule with no block in @font-palette-values ends at its semicolon"
    (body "@media screen; base-palette: 1")
    recovered 1;
  lenient_recover
    "selector-shaped item in @font-palette-values with a semicolon"
    (body ".a { b: c }; base-palette: 1")
    recovered 1;
  lenient_recover "a dropped font-palette descriptor keeps the sheet whole"
    (".a { color: red } "
    ^ body "zzz: 1; base-palette: 1"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS View Transitions 2 sec. 2.3.1 gives [@view-transition] a block of
   descriptor declarations, so one descriptor cascade rejects costs that
   descriptor alone. Blink 146 keeps the rule in every case below. *)
let spec_lenient_recovery_view_transition_descriptors () =
  let recovered = "@view-transition{navigation:auto}" in
  let both = "@view-transition{navigation:auto;types:a}" in
  lenient_recover "unknown descriptor first in @view-transition"
    "@view-transition { zzz: 1; navigation: auto }" recovered 1;
  lenient_recover "unknown descriptor mid-body in @view-transition"
    "@view-transition { navigation: auto; zzz: 1; types: a }" both 1;
  lenient_recover "unknown descriptor last in @view-transition"
    "@view-transition { navigation: auto; zzz: 1 }" recovered 1;
  lenient_recover "bad value for a known @view-transition descriptor"
    "@view-transition { navigation: bogus; navigation: auto }" recovered 1;
  lenient_recover "two curly blocks in a dropped view-transition value"
    "@view-transition { zzz: {1}{2}; navigation: auto }" recovered 1;
  lenient_recover "trailing tokens invalidate the view-transition descriptor"
    "@view-transition { navigation: none bogus; navigation: auto }" recovered 1;
  lenient_recover "an important flag invalidates the view-transition descriptor"
    "@view-transition { navigation: none !important; navigation: auto }"
    recovered 1;
  lenient_recover "at-rule in @view-transition ends at its block"
    "@view-transition { @media screen { color: red } navigation: auto }"
    recovered 1;
  lenient_recover
    "at-rule with no block in @view-transition ends at its semicolon"
    "@view-transition { @media screen; navigation: auto }" recovered 1;
  lenient_recover "selector-shaped item in @view-transition with a semicolon"
    "@view-transition { .a { b: c }; navigation: auto }" recovered 1;
  lenient_recover "a dropped view-transition descriptor keeps the sheet whole"
    ".a { color: red } @view-transition { zzz: 1; navigation: auto } .z { \
     color: lime }"
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* CSS Fonts 4 sec. 11.1 fills an [@font-feature-values] body with feature value
   blocks, so it is a rule list: CSS Syntax 3 sec. 5.4.2 ends the block cascade
   rejects at its own [{}] or [;], leaving the blocks written around it in the
   rule. A [;] with no rule before it is discarded with nothing to validate
   (sec. 5.4.3), so it costs nothing. *)
let spec_lenient_recovery_font_feature_values_blocks () =
  let recovered = "@font-feature-values Xf{@swash{s:1}@ornaments{o:2}}" in
  let body rest = "@font-feature-values Xf { @swash { s: 1 } " ^ rest ^ " }" in
  lenient_recover "unknown block first in @font-feature-values"
    "@font-feature-values Xf { @zzz { a: 1 } @swash { s: 1 } @ornaments { o: 2 \
     } }"
    recovered 1;
  lenient_recover "unknown block mid-body in @font-feature-values"
    (body "@zzz { a: 1 } @ornaments { o: 2 }")
    recovered 1;
  lenient_recover "unknown block last in @font-feature-values"
    (body "@ornaments { o: 2 } @zzz { a: 1 }")
    recovered 1;
  lenient_recover
    "at-rule with no block in @font-feature-values ends at its semicolon"
    (body "@zzz; @ornaments { o: 2 }")
    recovered 1;
  lenient_recover
    "selector-shaped item in @font-feature-values ends at its block"
    (body ".a { b: c } @ornaments { o: 2 }")
    recovered 1;
  lenient_recover
    "declaration in an @font-feature-values body ends at its semicolon"
    (body "color: red; @ornaments { o: 2 }")
    recovered 1;
  lenient_recover "stray semicolon in @font-feature-values costs nothing"
    (body "; @ornaments { o: 2 }")
    recovered 0;
  lenient_recover "leading semicolon in @font-feature-values costs nothing"
    "@font-feature-values Xf { ; @swash { s: 1 } @ornaments { o: 2 } }"
    recovered 0;
  lenient_recover "a dropped feature block keeps the sheet whole"
    (".a { color: red } "
    ^ body "@zzz { a: 1 } @ornaments { o: 2 }"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* Each dropped descriptor reports once, and [~strict:true] rejects exactly the
   inputs the lenient parse warned about. *)
let spec_descriptor_recovery_warns_once_per_descriptor () =
  warns_exactly "@counter-style c { system: cyclic; zzz: 1; symbols: \"a\" }" 1;
  warns_exactly
    "@counter-style c { system: cyclic; zzz: {1}{2}; symbols: \"a\" }" 1;
  warns_exactly
    "@counter-style c { system: cyclic; zzz: 1; yyy: 2; symbols: \"a\" }" 2;
  warns_exactly "@counter-style c { system: cyclic; symbols: \"a\" }" 0;
  warns_exactly "@counter-style c { ;; system: cyclic; symbols: \"a\" }" 0;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: 1; base-palette: 1 }" 1;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: {1}{2}; base-palette: 1 }"
    1;
  warns_exactly
    "@font-palette-values --p { font-family: X; zzz: 1; yyy: 2; base-palette: \
     1 }"
    2;
  warns_exactly "@font-palette-values --p { font-family: X; base-palette: 1 }" 0;
  warns_exactly "@view-transition { zzz: 1; navigation: auto }" 1;
  warns_exactly "@view-transition { zzz: {1}{2}; navigation: auto }" 1;
  warns_exactly "@view-transition { zzz: 1; yyy: 2; navigation: auto }" 2;
  warns_exactly "@view-transition { navigation: auto }" 0;
  warns_exactly "@view-transition { ;; navigation: auto }" 0;
  warns_exactly "@font-feature-values Xf { @zzz { a: 1 } @swash { s: 1 } }" 1;
  warns_exactly
    "@font-feature-values Xf { @zzz { a: 1 } @yyy { b: 2 } @swash { s: 1 } }" 2;
  warns_exactly "@font-feature-values Xf { @swash { s: 1 } }" 0;
  warns_exactly "@font-feature-values Xf { ;; @swash { s: 1 } }" 0

(* CSS Fonts 4 sec. 9.2 names the one mandatory descriptor:
   "@font-palette-values rules require a font-family descriptor; if it is
   missing, the @font-palette-values rule is invalid and must be ignored
   entirely." base-palette is optional, and sec. 9.2.2 gives it a default: "If
   this descriptor is not present in the @font-palette-values, [...] it behaves
   as if 0 were specified." Both sections read the same in the Editor's Draft
   and in the TR Working Draft of 25 August 2026. *)
let spec_font_palette_values_requires_font_family () =
  warns_exactly
    "@font-palette-values --p { font-family: A; override-colors: 0 red }" 0;
  warns_exactly "@font-palette-values --p { font-family: A }" 0;
  warns_exactly
    "@font-palette-values --p { base-palette: 1; override-colors: 0 red }" 1;
  warns_exactly "@font-palette-values --p { override-colors: 0 red }" 1

(* CSS Animations 1 sec. 3 fills a [@keyframes] body with keyframe rules, so it
   is a list of rules: an at-rule has no place there, and CSS Syntax 3 sec.
   5.4.2 ends the one being discarded at its own [{}] block or at its [;],
   leaving the keyframes written around it in the animation. A [(] or a [[] met
   in the prelude on the way is a component value of that prelude, not an end.
   Blink 146 keeps both neighbours in every case below. *)
let spec_lenient_recovery_keyframes_at_rule () =
  let recovered = "@keyframes k{0%{color:red}50%{background:#0f0}}" in
  let body rest = "@keyframes k { " ^ rest ^ " }" in
  lenient_recover "at-rule first in @keyframes"
    (body
       "@media print { to { color: pink } } from { color: red } 50% { \
        background: lime }")
    recovered 1;
  lenient_recover "at-rule mid-body in @keyframes"
    (body
       "from { color: red } @media print { to { color: pink } } 50% { \
        background: lime }")
    recovered 1;
  lenient_recover "at-rule last in @keyframes"
    (body
       "from { color: red } 50% { background: lime } @media print { to { \
        color: pink } }")
    recovered 1;
  lenient_recover "at-rule with no block in @keyframes ends at its semicolon"
    (body "from { color: red } @zzz; 50% { background: lime }")
    recovered 1;
  lenient_recover "a paren in the prelude of a dropped @keyframes at-rule"
    (body
       "from { color: red } @media (min-width: 1px) { to { color: pink } } 50% \
        { background: lime }")
    recovered 1;
  lenient_recover "a bracket in the prelude of a dropped @keyframes at-rule"
    (body
       "from { color: red } @zzz [a] { to { color: pink } } 50% { background: \
        lime }")
    recovered 1;
  (* Sec. 5.5.5 consumes an AT-RULE when a block's contents meet an at-keyword,
     so it ends at its own block rather than at the next [;]: a keyframe's
     declarations after one survive. Chrome 153 keeps both frames here. *)
  lenient_recover "an at-rule inside a keyframe block costs only itself"
    "@keyframes k { from { y: 0 } to { @e {} opacity: 1 } }"
    "@keyframes k{0%{y:0}to{opacity:1}}" 1;
  lenient_recover "an at-rule inside a descriptor block costs only itself"
    "@font-face { font-family: X; @e {} src: url(a.woff2) }"
    "@font-face{font-family:X;src:url(a.woff2)}" 1;
  lenient_recover "two at-rules in @keyframes are dropped one at a time"
    (body
       "from { color: red } @media print { a: b } @supports (display: grid) { \
        b: c } 50% { background: lime }")
    recovered 2;
  (* The animation outlives its only item, as it does in Blink 146: what is left
     is a [@keyframes] that animates nothing. *)
  lenient_recover "at-rule alone in @keyframes"
    (body "@media print { to { color: pink } }")
    "@keyframes k{}" 1;
  lenient_recover "a dropped at-rule in @keyframes keeps the sheet whole"
    (".a { color: red } "
    ^ body
        "from { color: red } @media print { to { color: pink } } 50% { \
         background: lime }"
    ^ " .z { color: lime }")
    (".a{color:red}" ^ recovered ^ ".z{color:#0f0}")
    1

(* Each dropped keyframe rule reports once, and [~strict:true] rejects exactly
   the inputs the lenient parse warned about. *)
let spec_keyframes_recovery_warns_once_per_rule () =
  let body rest = "@keyframes k { " ^ rest ^ " }" in
  warns_exactly
    (body
       "from { color: red } @media print { to { color: pink } } 50% { \
        background: lime }")
    1;
  warns_exactly
    (body
       "from { color: red } @media print { a: b } @supports (display: grid) { \
        b: c } 50% { background: lime }")
    2;
  warns_exactly (body "from { color: red } @zzz; 50% { background: lime }") 1;
  warns_exactly (body "from { color: red } 50% { background: lime }") 0

let suite =
  ( "stylesheet_recovery",
    [
      ( "spec lenient recovery stylesheets",
        `Quick,
        spec_lenient_recovery_stylesheets );
      ( "spec lenient recovery in a nested at-rule",
        `Quick,
        spec_lenient_recovery_nested_at_rule_declarations );
      ( "spec lenient recovery keeps a nested declaration run whole",
        `Quick,
        spec_lenient_recovery_nested_declaration_run );
      ( "spec recovery warns once per dropped declaration",
        `Quick,
        spec_recovery_warns_once_per_declaration );
      ( "spec lenient recovery in a @page body",
        `Quick,
        spec_lenient_recovery_page_descriptors );
      ( "spec lenient recovery in a page margin box",
        `Quick,
        spec_lenient_recovery_page_margin_box );
      ( "spec lenient recovery keeps a @page body in order",
        `Quick,
        spec_lenient_recovery_page_body_order );
      ( "spec page recovery warns once per dropped descriptor",
        `Quick,
        spec_page_recovery_warns_once_per_descriptor );
      ( "spec lenient recovery in an @property body",
        `Quick,
        spec_lenient_recovery_property_descriptors );
      ( "spec @property skips stray semicolons",
        `Quick,
        spec_property_skips_stray_semicolons );
      ( "spec property recovery warns once per dropped descriptor",
        `Quick,
        spec_property_recovery_warns_once_per_descriptor );
      ( "spec lenient recovery in a grouping at-rule block",
        `Quick,
        spec_lenient_recovery_block_statements );
      ( "spec block recovery warns once per dropped statement",
        `Quick,
        spec_block_recovery_warns_once_per_statement );
      ( "spec lenient recovery in a @counter-style body",
        `Quick,
        spec_lenient_recovery_counter_style_descriptors );
      ( "spec lenient recovery in a @font-palette-values body",
        `Quick,
        spec_lenient_recovery_font_palette_descriptors );
      ( "spec lenient recovery in a @view-transition body",
        `Quick,
        spec_lenient_recovery_view_transition_descriptors );
      ( "spec lenient recovery in a @font-feature-values body",
        `Quick,
        spec_lenient_recovery_font_feature_values_blocks );
      ( "spec descriptor recovery warns once per dropped descriptor",
        `Quick,
        spec_descriptor_recovery_warns_once_per_descriptor );
      ( "spec font-palette-values requires font-family",
        `Quick,
        spec_font_palette_values_requires_font_family );
      ( "spec lenient recovery in a @keyframes body",
        `Quick,
        spec_lenient_recovery_keyframes_at_rule );
      ( "spec keyframes recovery warns once per dropped rule",
        `Quick,
        spec_keyframes_recovery_warns_once_per_rule );
      ( "partial recovery: bad declaration does not poison sibling rule",
        `Quick,
        fun () ->
          (* Strict [read] would raise on the bad [rgb()]. The partial entry
             point drops just the bad declaration; both rules survive (the empty
             [.a\{\}] and the good [.b]). Per 5.4.4. *)
          let sheet, warnings =
            parse_stylesheet_partial
              ".a { color: rgb(300); } .b { color: red; }"
          in
          Alcotest.(check int)
            "both rules survive" 2
            (List.length (Css.Stylesheet.rules sheet));
          Alcotest.(check int) "one warning" 1 (List.length warnings) );
      ( "partial recovery: warnings carry snippets",
        `Quick,
        fun () ->
          let _sheet, warnings =
            parse_stylesheet_partial ".a { color: rgb(300); }"
          in
          match warnings with
          | [ e ] -> (
              match Error.snippet e with
              | None -> Alcotest.fail "expected warning with snippet"
              | Some _ -> ())
          | _ -> Alcotest.fail "expected exactly one warning" );
      ( "partial recovery: Css.of_string ~strict:false entry point",
        `Quick,
        fun () ->
          let { Css.stylesheet; warnings; _ } =
            match
              Css.of_string ~strict:false
                ".a { color: red; } .b { color: rgb(300); }"
            with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient parse failed: %s"
                  (Cascade.Error.to_string err)
          in
          Alcotest.(check int)
            "both rules survive" 2
            (List.length (Css.rule_statements stylesheet));
          Alcotest.(check int) "one warning" 1 (List.length warnings);
          (* filename threads through *)
          match warnings with
          | [ w ] ->
              Alcotest.(check (option string))
                "default filename" (Some "<string>") w.Cascade.Error.filename
          | _ -> Alcotest.fail "expected one warning" );
      ( "of_string ~strict:true rejects unknown at-rule",
        `Quick,
        fun () ->
          match
            Css.of_string ~strict:true
              "@unknown { color: red } .a { color: blue }"
          with
          | Ok _ ->
              Alcotest.fail "strict mode should reject @unknown as a warning"
          | Error _ -> () );
      ( "of_string ~strict:true accepts clean CSS",
        `Quick,
        fun () ->
          match
            Css.of_string ~strict:true ".a { color: red } .b { color: blue }"
          with
          | Ok parsed ->
              let css = minify parsed.stylesheet in
              Alcotest.(check string)
                "strict output" ".a{color:red}.b{color:#00f}" css
          | Error e ->
              Alcotest.failf "strict mode rejected clean CSS: %s"
                (Error.to_string e) );
      ( "of_string default is lenient and returns parse_result warnings",
        `Quick,
        fun () ->
          match
            Css.of_string ~strict:false
              "@unknown { color: red } .a { color: blue }"
          with
          | Ok parsed ->
              Alcotest.(check bool)
                "warning surfaced" true
                (parsed.Css.warnings <> []);
              Alcotest.(check string)
                "recovered output" "@unknown{color:red}.a{color:#00f}"
                (minify parsed.stylesheet)
          | Error e ->
              Alcotest.failf "non-strict mode should not promote warnings: %s"
                (Error.to_string e) );
      ( "of_string ~strict:true rejects bad declaration value",
        `Quick,
        fun () ->
          match Css.of_string ~strict:true ".a { color: rgb(300); }" with
          | Ok _ ->
              Alcotest.fail "strict mode should reject invalid colour value"
          | Error _ -> () );
      ( "partial recovery: unclosed brace recovered per 5.3.7",
        `Quick,
        fun () ->
          let { Css.stylesheet; warnings = _; _ } =
            match Css.of_string ~strict:false ".btn { color: red;" with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient unclosed-brace recovery failed: %s"
                  (Cascade.Error.to_string err)
          in
          Alcotest.(check int)
            "one rule recovered" 1
            (List.length (Css.rule_statements stylesheet)) );
      ( "partial recovery: bad declaration drops, rule survives",
        `Quick,
        fun () ->
          (* CSS Syntax 5.4.4: an invalid declaration is discarded while the
             enclosing rule and its other declarations survive. *)
          let { Css.stylesheet; warnings; _ } =
            match
              Css.of_string ~strict:false
                ".a { color: invalidcolor; color: red; }"
            with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient bad-declaration recovery failed: %s"
                  (Cascade.Error.to_string err)
          in
          let rules = Css.rule_statements stylesheet in
          Alcotest.(check int) "rule kept" 1 (List.length rules);
          Alcotest.(check int)
            "one warning for the bad decl" 1 (List.length warnings) );
      ( "partial recovery: filename propagates to warnings",
        `Quick,
        fun () ->
          let { Css.warnings; _ } =
            match
              Css.of_string ~strict:false ~filename:"user.css"
                ".a { color: rgb(300); }"
            with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient filename propagation parse failed: %s"
                  (Cascade.Error.to_string err)
          in
          match warnings with
          | [ w ] ->
              Alcotest.(check (option string))
                "filename" (Some "user.css") w.Cascade.Error.filename
          | _ -> Alcotest.fail "expected one warning" );
      ( "meta: `Full attaches snippets, `None skips them",
        `Quick,
        fun () ->
          let input = ".a { color: rgb(300); }" in
          let full =
            match Css.of_string ~strict:false ~meta:`Full input with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "`Full parse failed: %s"
                  (Cascade.Error.to_string err)
          in
          let none =
            match Css.of_string ~strict:false ~meta:`None input with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "`None parse failed: %s"
                  (Cascade.Error.to_string err)
          in
          (match full.warnings with
          | [ e ] ->
              Alcotest.(check bool)
                "`Full: snippet present" true
                (Error.snippet e <> None)
          | _ -> Alcotest.fail "expected one warning under `Full");
          match none.warnings with
          | [ e ] ->
              Alcotest.(check bool)
                "`None: snippet skipped" true
                (Error.snippet e = None)
          | _ -> Alcotest.fail "expected one warning under `None" );
      ( "semicolon-terminated at-rule survives partial parse",
        `Quick,
        fun () ->
          (* [@layer base;] parses through section 5.5.2 to an at-rule with
             [block = None]. The replay cursor must still present a terminating
             [;] to [read_layer], otherwise the at-rule is silently dropped. *)
          let { Css.stylesheet; warnings; _ } =
            match Css.of_string ~strict:false "@layer base;" with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient @layer parse failed: %s"
                  (Cascade.Error.to_string err)
          in
          let stmts = Css.statements stylesheet in
          Alcotest.(check int) "one statement" 1 (List.length stmts);
          Alcotest.(check int) "no warnings" 0 (List.length warnings) );
      ( "unknown at-rule surfaces as Unknown_at_rule warning",
        `Quick,
        fun () ->
          (* Section 5.4.1: an at-rule with no handler is discarded with a typed
             warning; the surrounding stylesheet continues to parse. *)
          let { Css.stylesheet; warnings; _ } =
            match
              Css.of_string ~strict:false
                "@unknown-rule { color: red; } .a { color: blue; }"
            with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient unknown at-rule recovery failed: %s"
                  (Cascade.Error.to_string err)
          in
          Alcotest.(check int)
            "good rule survives" 1
            (List.length (Css.rule_statements stylesheet));
          match warnings with
          | [ e ] -> (
              match e.Error.kind with
              | Error.Unknown_at_rule name ->
                  Alcotest.(check string) "at-rule name" "unknown-rule" name
              | _ ->
                  Alcotest.failf "expected Unknown_at_rule, got %s"
                    (Error.to_string e))
          | _ -> Alcotest.fail "expected one warning" );
      ( "malformed @supports surfaces as Bad_condition warning",
        `Quick,
        fun () ->
          (* A bad @supports condition used to raise via the untyped
             [Reader.Parse_error] shape, bypassing the partial-parse contract.
             It now becomes a typed [Bad_condition] warning while surrounding
             rules keep parsing. *)
          let { Css.stylesheet; warnings; _ } =
            match
              Css.of_string ~strict:false
                "@supports not-a-function foo { .a { color: red } } .b { \
                 color: blue }"
            with
            | Ok parsed -> parsed
            | Error err ->
                Alcotest.failf "lenient @supports recovery failed: %s"
                  (Cascade.Error.to_string err)
          in
          Alcotest.(check int)
            "sibling rule survives" 1
            (List.length (Css.rule_statements stylesheet));
          match warnings with
          | [ e ] -> (
              match e.Error.kind with
              | Error.Bad_condition { at_rule; _ } ->
                  Alcotest.(check string) "at-rule label" "@supports" at_rule
              | _ ->
                  Alcotest.failf "expected Bad_condition, got %s"
                    (Error.to_string e))
          | _ -> Alcotest.fail "expected one warning" );
    ] )
