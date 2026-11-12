(** Stylesheet tests: CSS Nesting. *)

open Cascade
open Css.Stylesheet
open Css_test_helpers
open Stylesheet_helpers

(** {2 CSS Nesting Round-trip Tests} *)

(** Helper: parse CSS, print minified, compare to expected *)
let test_nesting_roundtrip ~expected input =
  let r = Cursor.of_string input in
  try
    let stylesheet = Css.Stylesheet.read r in
    let roundtrip =
      String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
    in
    Alcotest.(check string)
      ("nesting roundtrip for " ^ input)
      expected roundtrip
  with Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)

(** Helper: parse CSS, print minified, parse again, print again -- verify
    idempotent *)
let test_nesting_idempotent input =
  let r = Cursor.of_string input in
  try
    let sheet1 = Css.Stylesheet.read r in
    let printed1 = String.trim (Css.Stylesheet.to_string ~minify:true sheet1) in
    let r2 = Cursor.of_string printed1 in
    let sheet2 = Css.Stylesheet.read r2 in
    let printed2 = String.trim (Css.Stylesheet.to_string ~minify:true sheet2) in
    Alcotest.(check string)
      ("nesting idempotent for " ^ input)
      printed1 printed2
  with Cursor.Parse_error err ->
    Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)

(* ignore-test *)
let test_nesting_basic () =
  (* Basic nesting with & descendant combinator *)
  test_nesting_roundtrip ~expected:".parent{color:red;& .child{color:blue}}"
    ".parent { color: red; & .child { color: blue; } }";
  test_nesting_idempotent ".parent { color: red; & .child { color: blue; } }"

(* ignore-test *)
let test_nesting_ampersand_hover () =
  (* Ampersand with pseudo-class *)
  test_nesting_roundtrip ~expected:".btn{color:red;&:hover{color:blue}}"
    ".btn { color: red; &:hover { color: blue; } }";
  test_nesting_idempotent ".btn { color: red; &:hover { color: blue; } }"

(* ignore-test *)
let test_nesting_multiple () =
  (* Multiple nested rules *)
  test_nesting_roundtrip
    ~expected:
      ".card{padding:1rem;& .title{font-size:1.5rem}& .body{font-size:1rem}}"
    ".card { padding: 1rem; & .title { font-size: 1.5rem; } & .body { \
     font-size: 1rem; } }";
  test_nesting_idempotent
    ".card { padding: 1rem; & .title { font-size: 1.5rem; } & .body { \
     font-size: 1rem; } }"

(* ignore-test *)
let test_nesting_media () =
  (* Nested @media query inside a rule *)
  test_nesting_roundtrip
    ~expected:".foo{color:red;@media(min-width:768px){color:blue}}"
    ".foo { color: red; @media (min-width: 768px) { color: blue; } }";
  test_nesting_idempotent ".foo{color:red;@media(min-width:768px){color:blue}}"

(* ignore-test *)
let test_nesting_deep () =
  (* Deeply nested rules *)
  test_nesting_roundtrip ~expected:".a{& .b{& .c{color:red}}}"
    ".a { & .b { & .c { color: red; } } }";
  test_nesting_idempotent ".a { & .b { & .c { color: red; } } }"

(* ignore-test *)
let test_nesting_with_declarations () =
  (* Nested rule after multiple declarations *)
  test_nesting_roundtrip
    ~expected:".parent{color:red;padding:1rem;&:focus{outline:none}}"
    ".parent { color: red; padding: 1rem; &:focus { outline: none; } }";
  test_nesting_idempotent
    ".parent { color: red; padding: 1rem; &:focus { outline: none; } }"

(* ignore-test *)
let test_nesting_check_stylesheet () =
  (* Also test via check_stylesheet for consistency *)
  (* optimize folds blue -> #00f and drops the redundant leading "& " (a bare
     nested selector already means descendant); pp holds both. *)
  assert_minify_and_optimize ".parent { color: red; & .child { color: blue; } }"
    ~minified:".parent{color:red;& .child{color:blue}}"
    ~optimized:".parent{color:red;.child{color:#00f}}";
  assert_minify_and_optimize ".btn { color: red; &:hover { color: blue; } }"
    ~minified:".btn{color:red;&:hover{color:blue}}"
    ~optimized:".btn{color:red;&:hover{color:#00f}}";
  check_stylesheet ~expected:".a{& .b{& .c{color:red}}}"
    ".a { & .b { & .c { color: red; } } }"

(* CSS Syntax 3 sec. 5.5.3 consumes a qualified rule whole, block and all,
   before deciding it is invalid, so sec. 5.5.5 resumes right after that block.
   Recovering to the next semicolon, which is what sec. 5.5.6 does for a bad
   declaration, would take every item written after the bad rule and the parent
   with them. *)
(* CSS Syntax 3 sec. 4.3.5 returns the string token when the input ends before
   the closing quote, so the token is a string and prints as one. Omitting the
   quote to keep the original bytes leaves the value swallowing the brace that
   follows, and the sheet grows one per pass. *)
let unterminated_string_closes () =
  check_stylesheet ~expected:"foo{--foo:\"foo\"}" "foo{--foo:\"foo";
  check_stylesheet ~expected:"foo{--foo:\"foo\"}" "foo{--foo:\"foo\"}";
  check_stylesheet ~expected:"foo{content:\"a\"}" "foo{content:\"a\"}"

let nesting_invalid_rule_recovery () =
  (* Each expectation is what the same sheet gives with the bad rule deleted:
     dropping it costs the parent nothing and the items around it nothing. *)
  lenient_recover "invalid nested rule before a good one"
    ".a { .b <::::invalid::::> {} & .c { color: red } }" ".a .c{color:red}" 1;
  lenient_recover "invalid nested rule after a good one"
    ".a { & .c { color: red } .b <::::invalid::::> {} }" ".a .c{color:red}" 1;
  lenient_recover "invalid nested rule between declarations"
    ".a { color: red; .b <::::invalid::::> {} & .c { color: blue } }"
    ".a{color:red;.c{color:#00f}}" 1

(* CSS Syntax 3 (ED) sec. 7.2 keeps an unmatched [)], []] or [}] out of a
   [<declaration-value>], so a value carrying one is no declaration at all. The
   rule holds for a value the typed readers hand to the opaque path as much as
   for one they read: a [var()] postpones the property grammar to computed-value
   time, not the token grammar. *)
let opaque_value_is_a_declaration_value () =
  lenient_recover "unmatched close paren after a var()"
    ".a { width: var(--x)); color: green }" ".a{color:green}" 1;
  lenient_recover "unmatched close bracket before a var()"
    ".a { width: ]var(--x); color: green }" ".a{color:green}" 1;
  lenient_recover "unmatched close bracket after a var()"
    ".a { width: var(--x)]; color: green }" ".a{color:green}" 1;
  lenient_recover "unmatched close paren under an unknown property"
    ".a { cascade-nope: a)b; color: green }" ".a{color:green}" 1

(* Selectors 4 sec. 3.9 makes a selector list carrying an unknown pseudo-class
   invalid, and CSS Nesting 1 sec. 2 gives a nested style rule a selector list
   for its prelude, so the nested rule goes and the ones around it stay. An
   escaped [;] is an ident code point, so [color:green\;] is a type selector and
   a pseudo-class named [green;], which is the same case reached through a
   declaration that failed. *)
let nested_prelude_is_unforgiving () =
  lenient_recover "unknown pseudo-class in a nested prelude"
    ".a { b:future-pseudo { color: red } & .c { color: blue } }"
    ".a .c{color:#00f}" 1;
  lenient_recover "escaped semicolon leaves a pseudo-class behind"
    "a { color: green\\; & { color: red } .c { z-index: 1 } }" "a .c{z-index:1}"
    1

(* CSS Syntax 3 sec. 4.3.1 consumes a unicode-range token only while "unicode
   ranges allowed" is set, and sec. 4.3.14 says the one caller that sets it is
   the value of a unicode-range descriptor. Everywhere else [u+a] is an ident, a
   delim and an ident, which the selector grammar reads as a sibling
   combination. *)
let unicode_range_only_in_its_descriptor () =
  check_stylesheet ~expected:"u+a{color:red}" "u+a { color: red }";
  check_stylesheet ~expected:"a{color:red}u+a{color:blue}"
    "a { color: red } u+a { color: blue }";
  check_stylesheet ~expected:"u+a b{color:red}" "u+a b { color: red }";
  check_stylesheet
    ~expected:"@font-face{font-family:X;src:url(a.woff2);unicode-range:U+0-7F}"
    "@font-face { font-family: X; src: url(a.woff2); unicode-range: U+0-7F }";
  check_stylesheet
    ~expected:"@font-face{font-family:X;src:url(a.woff2);unicode-range:U+4??}"
    "@font-face { font-family: X; src: url(a.woff2); unicode-range: U+4?? }";
  check_stylesheet
    ~expected:
      "@font-face{font-family:X;src:url(a.woff2);unicode-range:U+26,U+0-7F}"
    "@font-face { font-family: X; src: url(a.woff2); unicode-range: U+26, \
     U+0-7F }";
  (* Sec. 4.3.14 writes the range's second half as one to six hex digits, so a
     one-digit tail is a whole tail. Every case above ends its range on two
     digits or more, which lets the lexer decide there is a tail by looking one
     code point too far and still answer right; the shortest tail is where that
     stops working. *)
  check_stylesheet
    ~expected:"@font-face{font-family:X;src:url(a.woff2);unicode-range:U+0-7}"
    "@font-face { font-family: X; src: url(a.woff2); unicode-range: U+0-7 }"

(* A nested @layer holds nesting content: bare declarations belong to the parent
   selector, exactly as in @media/@supports. Blink and WebKit both read
   [.a{@layer n{color:red}}] as a layer block wrapping nested declarations. *)
let spec_nesting_layer_block () =
  test_nesting_roundtrip ~expected:".a{@layer n{color:red}}"
    ".a { @layer n { color: red; } }";
  test_nesting_roundtrip ~expected:".a{@layer{color:red}}"
    ".a { @layer { color: red; } }";
  test_nesting_roundtrip ~expected:".a{color:red;@layer n{color:blue}}"
    ".a { color: red; @layer n { color: blue; } }";
  test_nesting_roundtrip ~expected:".a{@layer n{& b{color:red}}}"
    ".a { @layer n { & b { color: red; } } }";
  test_nesting_idempotent ".a { @layer n { color: red; } }"

(* The statement form [@layer n;] is only a layer-order declaration, which no
   style rule can contain: both Blink and WebKit drop it. An empty nested layer
   therefore keeps its block form so the output re-reads. *)
let spec_nesting_empty_layer_keeps_block () =
  test_nesting_roundtrip ~expected:".a{@layer n{}}" ".a { @layer n { } }";
  test_nesting_idempotent ".a { @layer n { } }";
  test_nesting_roundtrip ~expected:"@layer n;" "@layer n { }"

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well", and its body is then read as nesting
   content. [@starting-style] is a grouping rule over style rules (CSS
   Transitions 2 sec. 3.3), and Blink 146 keeps the whole shape. *)
let spec_nesting_starting_style () =
  test_nesting_roundtrip ~expected:".a{@starting-style{color:red}}"
    ".a { @starting-style { color: red } }";
  test_nesting_roundtrip ~expected:".a{@starting-style{color:red}color:green}"
    ".a { @starting-style { color: red } color: green }";
  test_nesting_roundtrip ~expected:".a{@starting-style{& b{color:red}}}"
    ".a { @starting-style { & b { color: red } } }";
  test_nesting_idempotent ".a { @starting-style { color: red } color: green }"

(* CSS Conditional 5 sec. 3 and sec. 4: a prelude the [@when] / [@else]
   condition grammar rejects is a condition failure of that at-rule, and the
   caret belongs on the slice of the condition that failed. The reader holds
   those components, so it can point at them rather than at the block that
   follows; the same rule the [@container] and [@supports] preludes read by. *)
let conditional_prelude_errors () =
  let case (input, at_rule, offending) =
    let warnings =
      match Css.of_string ~strict:false input with
      | Error e -> [ e ]
      | Ok { Css.warnings; _ } -> warnings
    in
    match warnings with
    | [ ({ Error.kind = Error.Bad_condition { at_rule = named; _ }; _ } as e) ]
      ->
        Alcotest.(check string) (input ^ ": at-rule named") at_rule named;
        Alcotest.(check string)
          (input ^ ": caret on the offending slice")
          offending
          (String.sub input e.Error.loc.Loc.start_pos
             (e.Error.loc.Loc.end_pos - e.Error.loc.Loc.start_pos))
    | [ e ] ->
        Alcotest.failf "%s: expected a condition error, got %s" input
          (Error.to_string e)
    | warnings ->
        Alcotest.failf "%s: expected one warning, got %d" input
          (List.length warnings)
  in
  List.iter case
    [
      ("@when foo(x){.a{color:red}}", "@when", "foo(x)");
      ( "@when media(screen) and supports(top:0) or media(print){.a{c:red}}",
        "@when",
        "or" );
      ( "@when media(screen) or supports(top:0) and media(print){.a{c:red}}",
        "@when",
        "and" );
      ("@when .a{color:red}", "@when", ".");
      ( "@when media(screen) media(print){.a{color:red}}",
        "@when",
        "media(print)" );
      ( "@when media(screen{.a{color:red}}",
        "@when",
        "media(screen{.a{color:red}}" );
      ( "@when media(width>0px){.a{color:red}}@else foo(x){.b{color:red}}",
        "@else",
        "foo(x)" );
      ( "@when media(width>0px){.a{c:red}}@else media(print) x{.b{c:red}}",
        "@else",
        "x" );
    ]

(* The same section covers every conditional group rule, not just the three
   cascade already nested: [@-moz-document] carries style rules, and CSS
   Conditional 5 sec. 3 and sec. 4 make [@when] and [@else] conditional group
   rules over a rule list. *)
let spec_nesting_other_group_rules () =
  test_nesting_roundtrip ~expected:".a{@-moz-document url-prefix(){color:red}}"
    ".a { @-moz-document url-prefix() { color: red } }";
  test_nesting_roundtrip ~expected:".a{@when media(width>0px){color:red}}"
    ".a { @when media(width > 0px) { color: red } }";
  test_nesting_roundtrip
    ~expected:".a{@when media(width>0px){color:red}@else{color:green}}"
    ".a { @when media(width > 0px) { color: red } @else { color: green } }";
  test_nesting_idempotent ".a { @-moz-document url-prefix() { color: red } }"

(* A nested group rule's body is nesting content all the way down, so an at-rule
   inside one is itself a nested group rule. Blink 146 keeps [.a{@layer n{@media
   screen{color:red}}}] whole. *)
let spec_nesting_at_rule_inside_nested_group () =
  test_nesting_roundtrip ~expected:".a{@layer n{@media screen{color:red}}}"
    ".a { @layer n { @media screen { color: red } } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{@supports(foo:bar){color:red}}}"
    ".a { @media screen { @supports (foo: bar) { color: red } } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{@starting-style{color:red}}}"
    ".a { @media screen { @starting-style { color: red } } }";
  test_nesting_idempotent ".a { @layer n { @media screen { color: red } } }"

(* CSS Nesting 1 sec. 3.3 nests "any at-rule whose body contains style rules"; a
   descriptor rule, a keyframe list and a declaration-list rule contain none, so
   inside a style rule they are invalid. Blink 146 drops each one and keeps the
   declarations written around it, which is the recovery CSS Syntax 3 sec. 5.4.4
   describes: discard the invalid construct, resume at the next one. *)
let spec_nesting_rejects_non_group_at_rules () =
  let drops at_rule =
    test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
      (".a { color: red; " ^ at_rule ^ " background: blue }")
  in
  drops "@font-face { font-family: F; src: url(f.woff2) }";
  drops "@keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@-webkit-keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@-moz-keyframes k { from { opacity: 0 } to { opacity: 1 } }";
  drops "@property --p { syntax: \"<color>\"; inherits: false }";
  drops "@page { margin: 1cm }";
  drops "@counter-style c { system: cyclic; symbols: \"x\" }";
  drops "@position-try --t { top: 1px }";
  drops "@font-palette-values --v { font-family: F; base-palette: 0 }";
  drops "@font-feature-values F { @styleset { s: 1 } }";
  drops "@viewport { width: 100px }";
  drops "@-ms-viewport { width: 100px }";
  drops "@supports-condition (color: red) { color: green }"

(* The same rejection inside every nesting context that a style rule opens: a
   nested group rule's body, a nested style rule, and a rule reached through a
   stylesheet-level group rule. *)
let spec_nesting_rejects_non_group_at_rules_deep () =
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @font-face { font-family: F; src: \
     url(f.woff2) } background: blue } }";
  test_nesting_roundtrip ~expected:".a{& b{color:red;background:blue}}"
    ".a { & b { color: red; @keyframes k { to { opacity: 1 } } background: \
     blue } }";
  test_nesting_roundtrip
    ~expected:"@media screen{.a{color:red;background:blue}}"
    "@media screen { .a { color: red; @page { margin: 1cm } background: blue } \
     }"

(* Dropping the invalid at-rule must not take a nested style rule written after
   it, and the surviving text has to read back unchanged. *)
let spec_nesting_rejection_keeps_the_rest () =
  test_nesting_roundtrip ~expected:".a{color:red;& span{color:lime}b:2}"
    ".a { color: red; @font-face { font-family: F; src: url(f.woff2) } & span \
     { color: lime } b: 2 }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @font-face; background: blue }";
  test_nesting_idempotent
    ".a { color: red; @page { margin: 1cm } background: blue }"

(* CSS View Transitions 2 gives [\@view-transition] a descriptor body, so CSS
   Nesting 1 sec. 3.3 does not nest it either - but Blink 146 keeps it inside a
   style rule, down to [&:hover], where every rule above is dropped. Dropping
   what a shipping engine still reads is the one lossy direction, so cascade
   keeps it. *)
let spec_nesting_keeps_view_transition () =
  test_nesting_roundtrip
    ~expected:".a{color:red;@view-transition{navigation:auto}background:blue}"
    ".a { color: red; @view-transition { navigation: auto } background: blue }"

(* CSS Conditional 5 sec. 4: [\@else] is only valid after a [\@when] or another
   [\@else], wherever it is written. *)
let spec_nesting_rejects_orphan_else () =
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @else { color: green } background: blue }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @else { color: green } background: blue \
     } }";
  test_nesting_roundtrip
    ~expected:".a{@when media(width>0px){color:red}@else{color:green}}"
    ".a { @when media(width > 0px) { color: red } @else { color: green } }"

(* A top-of-sheet rule is invalid in a style rule too, and Blink 146 drops it
   the same way. Discarding it ends at the at-rule: [\@import url(x){}] carries
   a block, and a skip to the next [;] runs past the end of the group rule
   holding it. *)
let spec_nesting_rejects_top_of_sheet_at_rules () =
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @charset \"utf-8\"; background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @import url(x.css); background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @import url(x.css) { color: pink } background: blue }";
  test_nesting_roundtrip ~expected:".a{color:red;background:blue}"
    ".a { color: red; @namespace n url(http://e.com); background: blue }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red; @import url(x.css); background: blue } }"

(* CSS Nesting 1 sec. 3: a nested rule's prelude may start with an ident, so
   [h2:where(.b) { ... }] is a rule and not a [h2] declaration, however much its
   head looks like one. That holds in a nested at-rule's body as much as in a
   style rule's, and Blink 146 reads both as rules. *)
let spec_nesting_ident_prelude_in_nested_at_rule () =
  test_nesting_roundtrip ~expected:".a{@media screen{h2:where(.b){color:red}}}"
    ".a { @media screen { h2:where(.b) { color: red } } }";
  test_nesting_idempotent ".a { @media screen { h2:where(.b) { color: red } } }";
  test_nesting_roundtrip
    ~expected:
      ".a{@media screen{color:red;h2:where(.b){margin:0}background:blue}}"
    ".a { @media screen { color: red; h2:where(.b) { margin: 0 } background: \
     blue } }"

(* CSS Syntax 3 sec. 5.4.3 "consume a block's contents" drops a [;] that no
   declaration precedes rather than validating one, so a stray semicolon in a
   nested at-rule's body costs nothing. Blink 146 keeps both neighbours. *)
let spec_nesting_skips_stray_semicolons () =
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { color: red;; background: blue } }";
  test_nesting_roundtrip
    ~expected:".a{@media screen{color:red;background:blue}}"
    ".a { @media screen { ; color: red; background: blue } }"

(* CSS Nesting 1 sec. 3.4: a run of declarations written after a nested rule is
   wrapped in a nested declarations rule, which keeps its place among the nested
   rules. The spec's own worked example names the hoisted spelling as NOT
   equivalent, and Blink and WebKit both compute the later declaration. *)
let spec_nesting_declaration_after_nested_rule () =
  test_nesting_roundtrip ~expected:".a{color:red;& b{color:blue}color:green}"
    ".a { color: red; & b { color: blue } color: green }";
  test_nesting_roundtrip
    ~expected:".a{@supports(color:red){color:blue}color:green}"
    ".a { @supports (color: red) { color: blue } color: green }";
  test_nesting_roundtrip ~expected:".a{c:1;& b{d:2}e:3;f:4;& g{h:5}i:6}"
    ".a { c: 1; & b { d: 2 } e: 3; f: 4; & g { h: 5 } i: 6 }";
  test_nesting_idempotent ".a { color: red; & b { color: blue } color: green }";
  test_nesting_idempotent
    ".a { @media (min-width: 1px) { padding: 2rem } color: green }"

let spec_nesting_selector_edges () =
  assert_minify_and_optimize
    ".card { color: red; &:is(:hover, :focus-visible) { color: blue } &:has(> \
     img) { display: grid } }"
    ~minified:
      ".card{color:red;&:is(:hover,:focus-visible){color:blue}&:has(>img){display:grid}}"
      (* nested rule order is preserved; selector spelling and declaration
         values are still canonicalized. *)
    ~optimized:
      ".card{color:red;&:is(:focus-visible,:hover){color:#00f}&:has(>img){display:grid}}";
  check_stylesheet
    ~expected:
      ".card{@supports \
       selector(:has(img)){&:has(img){display:grid}}@container(inline-size>30em){&>.media{display:block}}}"
    ".card { @supports selector(:has(img)) { &:has(img) { display: grid } } \
     @container (inline-size > 30em) { & > .media { display: block } } }";
  assert_minify_and_optimize
    "@scope (.card) to (.boundary) { .title { color: red; &:hover { color: \
     blue } } }"
    ~minified:
      "@scope(.card)to (.boundary){.title{color:red;&:hover{color:blue}}}"
    ~optimized:
      "@scope(.card)to (.boundary){.title{color:red;&:hover{color:#00f}}}";
  assert_minify_and_optimize
    ".card { @scope (&) to (.boundary) { & .title { color: blue } } }"
    ~minified:".card{@scope(&)to (.boundary){& .title{color:blue}}}"
    ~optimized:".card{@scope(&)to (.boundary){& .title{color:#00f}}}";
  check_stylesheet
    ~expected:
      ".card{color:red;@scope(.feature)to (.boundary){&>.title{display:grid}}}"
    ".card { color: red; @scope (.feature) to (.boundary) { & > .title { \
     display: grid } } }";
  check_stylesheet
    ~expected:"@starting-style{.dialog[open]{opacity:0;transform:scale(.95)}}"
    "@starting-style { .dialog[open] { opacity: 0; transform: scale(0.95) } }";
  neg_cursor read ".card { & { & { color: red } } }";
  neg_cursor read "@scope () { .x { color: red } }";
  neg_cursor read "@scope (.x) to () { .x { color: red } }";
  neg_cursor read "@starting-style;"

(* CSS Nesting Module Level 1, sections 1 and 2: a nested style rule is a
   qualified rule appearing inside another qualified rule's block. The grammar
   does not require flattening on serialization, so a parse and print of a
   nested input must keep the nested shape rather than collapsing it into
   sibling rules joined by descendant combinators. *)
let nesting_module_l1_preserves_structure () =
  let input =
    ".card { color: red; & .title { color: blue; } &:hover { color: green; } }"
  in
  let r = Cursor.of_string input in
  let sheet = Css.Stylesheet.read r in
  let printed = String.trim (Css.Stylesheet.to_string ~minify:true sheet) in
  Alcotest.(check string)
    "parse + print keeps nested rules nested"
    ".card{color:red;& .title{color:blue}&:hover{color:green}}" printed;
  let optimized = Css.Optimize.stylesheet sheet in
  let opt_printed =
    String.trim (Css.Stylesheet.to_string ~minify:true optimized)
  in
  Alcotest.(check string)
    "optimize keeps nested rules nested (no flattening)"
    ".card{color:red;.title{color:#00f}&:hover{color:green}}" opt_printed

let suite =
  ( "stylesheet_nesting",
    [
      (* CSS nesting round-trip tests *)
      ("nesting basic", `Quick, test_nesting_basic);
      ("nesting ampersand hover", `Quick, test_nesting_ampersand_hover);
      ("nesting multiple nested rules", `Quick, test_nesting_multiple);
      ("nesting nested media query", `Quick, test_nesting_media);
      ("nesting deeply nested", `Quick, test_nesting_deep);
      ("nesting with declarations", `Quick, test_nesting_with_declarations);
      ("nesting check_stylesheet", `Quick, test_nesting_check_stylesheet);
      ( "unicode range only in its descriptor",
        `Quick,
        unicode_range_only_in_its_descriptor );
      ("nesting invalid rule recovery", `Quick, nesting_invalid_rule_recovery);
      ("nested prelude is unforgiving", `Quick, nested_prelude_is_unforgiving);
      ( "opaque value is a declaration value",
        `Quick,
        opaque_value_is_a_declaration_value );
      ("unterminated string closes", `Quick, unterminated_string_closes);
      ( "spec nesting selector and conditional edges",
        `Quick,
        spec_nesting_selector_edges );
      ( "spec nesting declaration after a nested rule keeps its place",
        `Quick,
        spec_nesting_declaration_after_nested_rule );
      ( "spec nesting @layer block holds nesting content",
        `Quick,
        spec_nesting_layer_block );
      ( "spec nesting empty @layer keeps block form",
        `Quick,
        spec_nesting_empty_layer_keeps_block );
      ( "spec nesting @starting-style holds nesting content",
        `Quick,
        spec_nesting_starting_style );
      ( "conditional prelude errors name the at-rule and the slice",
        `Quick,
        conditional_prelude_errors );
      ( "spec nesting other group rules hold nesting content",
        `Quick,
        spec_nesting_other_group_rules );
      ( "spec nesting at-rule inside a nested group rule",
        `Quick,
        spec_nesting_at_rule_inside_nested_group );
      ( "spec nesting rejects a non-group at-rule",
        `Quick,
        spec_nesting_rejects_non_group_at_rules );
      ( "spec nesting rejects a non-group at-rule at every depth",
        `Quick,
        spec_nesting_rejects_non_group_at_rules_deep );
      ( "spec nesting rejection keeps the rest of the rule",
        `Quick,
        spec_nesting_rejection_keeps_the_rest );
      ( "spec nesting keeps @view-transition",
        `Quick,
        spec_nesting_keeps_view_transition );
      ( "spec nesting rejects an orphan @else",
        `Quick,
        spec_nesting_rejects_orphan_else );
      ( "spec nesting rejects a top-of-sheet at-rule",
        `Quick,
        spec_nesting_rejects_top_of_sheet_at_rules );
      ( "spec nesting reads an ident prelude in a nested at-rule",
        `Quick,
        spec_nesting_ident_prelude_in_nested_at_rule );
      ( "spec nesting skips stray semicolons in a nested at-rule",
        `Quick,
        spec_nesting_skips_stray_semicolons );
      ( "spec CSS Nesting L1 preserves nested structure",
        `Quick,
        nesting_module_l1_preserves_structure );
    ] )
