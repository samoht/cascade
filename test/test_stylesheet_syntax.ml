(** Stylesheet tests: invalid input, unknown at-rules and error spans. *)

open Cascade
open Css.Stylesheet
open Css_test_helpers
open Stylesheet_helpers

(* Tests for newly added check functions *)
(* Not a roundtrip test *)
let test_check () =
  (* Test basic stylesheet parsing using check function *)
  check ~expected:".test{color:red}" ".test { color: red }";
  assert_minify_and_optimize "@media screen { .test { color: blue } }"
    ~minified:"@media screen{.test{color:blue}}"
    ~optimized:"@media screen{.test{color:#00f}}"

let test_import_rule () =
  check_import_rule ~expected:"@import\"test.css\";" "@import 'test.css';";
  check_import_rule ~expected:"@import\"styles.css\"screen;"
    "@import url('styles.css') screen;";

  (* Test invalid import rules *)
  neg_cursor read_import_rule "@import";
  (* Missing URL *)
  neg_cursor read_import_rule "@import test.css";
  (* Missing quotes *)
  neg_cursor read_import_rule "import 'test.css'";
  (* Missing @ *)
  (* Unclosed quote at EOF -- per CSS Syntax sec. 4.3.5 the lexer still returns a
     string-token (the ill-formedness is a parse-error warning, not a
     token-level failure), so [\@import 'test.css] parses as a valid import. *)
  check_import_rule ~expected:"@import\"test.css\";" "@import 'test.css"

(* CSS Cascade 5 sec. 6.4.1: a [<layer-name>] is [<ident> ['.' <ident>]*]. The
   idents are the name; a [.] one of them carries is written back escaped (CSS
   Syntax 3 sec. 2.1) and only a bare [.] separates two. A CSS-wide keyword is
   reserved, in any ident of the name. *)
let test_layer_name () =
  check_layer_name ~roundtrip:true "a";
  check_layer_name ~roundtrip:true "a.b";
  check_layer_name ~roundtrip:true ~expected:"a\\.b" "a\\2e b";
  check_layer_name ~roundtrip:true ~expected:"a\\.b.c" "a\\2e b.c";
  check_layer_name ~roundtrip:true ~expected:"a.b\\.c" "a.b\\2e c";
  check_layer_name ~roundtrip:true ~expected:"a\\;b" "a\\3b b";
  neg_cursor read_layer_name "initial";
  neg_cursor read_layer_name "a.revert-layer";
  neg_cursor read_layer_name ".a";
  neg_cursor read_layer_name "a."

(* Not a roundtrip test *)
let test_advanced_selectors () =
  assert_minify_and_optimize ".btn:hover { color: blue; }"
    ~minified:".btn:hover{color:blue}" ~optimized:".btn:hover{color:#00f}";
  check_stylesheet ~expected:".btn:before{content:\"icon\"}"
    ".btn::before { content: 'icon'; }";
  (* Attribute values that are valid identifiers get normalized to unquoted
     form *)
  assert_minify_and_optimize ".btn[data-type='primary'] { background: blue; }"
    ~minified:".btn[data-type=primary]{background:blue}"
    ~optimized:".btn[data-type=primary]{background:#00f}";
  check_stylesheet ~expected:".parent>.child{margin:0}"
    ".parent > .child { margin: 0; }";
  check_stylesheet ~expected:".sibling+.next{padding:10px}"
    ".sibling + .next { padding: 10px; }";
  check_stylesheet ~expected:".element~.general-sibling{color:red}"
    ".element ~ .general-sibling { color: red; }"

(* Not a roundtrip test *)
let test_advanced_properties () =
  check_stylesheet ~expected:".box{transform:rotate(45deg)scale(1.2)}"
    ".box { transform: rotate(45deg) scale(1.2); }";
  check_stylesheet ~expected:".grid{display:grid;grid-template-columns:1fr 2fr}"
    ".grid { display: grid; grid-template-columns: 1fr 2fr; }";
  check_stylesheet ~expected:".flex{display:flex;justify-content:space-between}"
    ".flex { display: flex; justify-content: space-between; }";
  (* pp serializes the modern rgb() form (rgba unifies into rgb, commas ->
     spaces, alpha via slash); optimize cross-folds to the shorter 4-digit
     hex. *)
  assert_minify_and_optimize
    ".shadow { box-shadow: 0 4px 8px rgba(0,0,0,0.2); }"
    ~minified:".shadow{box-shadow:0 4px 8px rgb(0 0 0/.2)}"
    ~optimized:".shadow{box-shadow:0 4px 8px #0003}";
  (* "to right" is a <side-or-corner>, a distinct node from the <angle> 90deg
     (corners like "to top right" are not fixed angles), so pp holds the
     authored keyword; optimize folds the side to 90deg and Named blue to
     #00f. *)
  assert_minify_and_optimize
    ".gradient { background: linear-gradient(to right, red, blue); }"
    ~minified:".gradient{background:linear-gradient(to right,red,blue)}"
    ~optimized:".gradient{background:linear-gradient(90deg,red,#00f)}"

(* Not a roundtrip test *)
let test_complex_values () =
  check_stylesheet ~expected:".calc{width:calc(100% - 20px)}"
    ".calc { width: calc(100% - 20px); }";
  check_stylesheet ~expected:".multi{margin:10px 20px 30px 40px}"
    ".multi { margin: 10px 20px 30px 40px; }";
  check_stylesheet ~expected:".var{color:var(--primary-color,blue)}"
    ".var { color: var(--primary-color, blue); }";
  check_stylesheet ~expected:".clamp{font-size:clamp(1rem,2vw,2rem)}"
    ".clamp { font-size: clamp(1rem, 2vw, 2rem); }";
  check_stylesheet ~expected:".minmax{grid-template-columns:minmax(200px,1fr)}"
    ".minmax { grid-template-columns: minmax(200px, 1fr); }"

(* Not a roundtrip test *)
let test_nested_rules () =
  check_stylesheet
    ~expected:
      "@media(min-width:768px){@supports(display:grid){.grid{display:grid}}}"
    "@media (min-width: 768px) { @supports (display: grid) { .grid { display: \
     grid; } } }";
  check_stylesheet
    ~expected:"@layer base{@media print{.print-only{display:block}}}"
    "@layer base { @media print { .print-only { display: block; } } }";
  check_stylesheet
    ~expected:
      "@container(width>400px){@media(orientation:landscape){.landscape{color:green}}}"
    "@container (width > 400px) { @media (orientation: landscape) { .landscape \
     { color: green; } } }"

(** Negative tests for invalid CSS *)
let expect_parse_error input =
  let r = Cursor.of_string input in
  try
    let _ = read r in
    Alcotest.failf "Expected parse error for: %s" input
  with Cursor.Parse_error _ | Error.Parse_error _ -> ()

(* Not a roundtrip test *)
let spec_s7_block_examples () =
  (* CSS Syntax Level 3 section 7.1: these productions are parsed as generic
     block contents, then validated by the rule grammar that owns the block. *)
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  assert_minify_and_optimize
    "p > a { color: blue; text-decoration: underline; }"
    ~minified:"p>a{color:blue;text-decoration:underline}"
    ~optimized:"p>a{color:#00f;text-decoration:underline}";
  check_stylesheet
    ~expected:"@font-face{font-family:MyFont;src:url(font.woff2)}"
    "@font-face { font-family: MyFont; src: url(font.woff2); }";
  check_stylesheet ~expected:"@page:left{margin-left:4cm;margin-right:3cm}"
    "@page :left { margin-left: 4cm; margin-right: 3cm; }";
  check_stylesheet ~expected:"@keyframes slide{0%{opacity:0}to{opacity:1}}"
    "@keyframes slide { 0% { opacity: 0 } 100% { opacity: 1 } }";
  assert_minify_and_optimize ".card { color: red; & .title { color: blue; } }"
    ~minified:".card{color:red;& .title{color:blue}}"
    ~optimized:".card{color:red;.title{color:#00f}}";
  expect_parse_error "@media print { color: red; body { font-size: 10pt } }";
  check_stylesheet ~expected:"@keyframes slide{50%{opacity:1}}"
    "@keyframes slide { color: red; 50% { opacity: 1 } }";
  (* A qualified rule is not a descriptor, so it is dropped the way any other
     bad descriptor is and the @font-face is kept. It then carries neither
     font-family nor src, so nothing is emitted. Chrome 146 keeps an empty
     CSSFontFaceRule for the same input. *)
  check_stylesheet ~expected:"" "@font-face { .x { color: red } }"

(* Not a roundtrip test *)
let spec_s8_rule_shapes () =
  (* CSS Syntax Level 3 sections 8.1 and 8.2: top-level qualified rules are
     style rules, and at-rules are either statement or block rules depending on
     whether they end with a semicolon or a {} block. *)
  assert_minify_and_optimize "p > a { color: blue }" ~minified:"p>a{color:blue}"
    ~optimized:"p>a{color:#00f}";
  check_stylesheet ~expected:"@import\"theme.css\";" "@import \"theme.css\";";
  check_stylesheet ~expected:"@media print{body{font-size:10pt}}"
    "@media print { body { font-size: 10pt } }";
  check_stylesheet ~expected:"@layer reset,base;" "@layer reset, base;";
  expect_parse_error "p > a";
  expect_parse_error "@import \"theme.css\" .a { color: red }";
  expect_parse_error "@media print"

let spec_namespace_serialization () =
  (* CSS Namespaces: minified prefixed namespace rules can omit whitespace
     between the prefix ident and URL token while preserving token boundaries.
     URL tokens use the shorter string spelling when possible. *)
  check_stylesheet ~expected:"@namespace svg\"http://www.w3.org/2000/svg\";"
    "@namespace svg url(http://www.w3.org/2000/svg);";
  check_stylesheet
    ~expected:"@namespace math\"http://www.w3.org/1998/Math/MathML\";"
    "@namespace math \"http://www.w3.org/1998/Math/MathML\";";
  check_stylesheet ~expected:"@namespace \"http://www.w3.org/1999/xhtml\";"
    "@namespace url(http://www.w3.org/1999/xhtml);"

(* Not a roundtrip test *)
let spec_s8_charset_not_rule () =
  (* CSS Syntax Level 3 section 8.3: @charset is an encoding declaration shape,
     not a CSS at-rule after stylesheet parsing. *)
  expect_parse_error "@charset \"utf-8\";";
  expect_parse_error "@charset \"utf-8\"; .a { color: red }"

(* Not a roundtrip test *)
let test_invalid_selectors () =
  expect_parse_error "..double-class { color: red; }";
  expect_parse_error "# { color: red; }";
  expect_parse_error ". { color: red; }";
  expect_parse_error "[invalid-attr { color: red; }";
  expect_parse_error ".class:invalid-pseudo { color: red; }"

(* Not a roundtrip test *)
let test_invalid_properties () =
  expect_parse_error ".btn { color: invalid-color; }";
  expect_parse_error ".btn { display: invalid-display; }";
  expect_parse_error ".btn { width: 100invalid; }";
  expect_parse_error ".btn { margin: px; }"

(* Not a roundtrip test *)
let test_invalid_syntax () =
  (* CSS Syntax sec. 2.2: unclosed blocks auto-close at EOF and the inner
     declaration is preserved. Verify the AST, don't just accept "didn't
     crash". *)
  check_stylesheet ~expected:".btn{color:red}" ".btn { color: red ";
  expect_parse_error ".btn color: red; }";
  expect_parse_error "{ color: red; }";
  expect_parse_error ".btn { : red; }";
  expect_parse_error ".btn { color red; }"

(* Not a roundtrip test *)
let test_invalid_at_rules () =
  check_stylesheet ~expected:"@media{.btn{color:red}}"
    "@media { .btn { color: red; } }";
  expect_parse_error "@property { syntax: 'color'; inherits: true; }";
  expect_parse_error "@property --var { invalid-descriptor: value; }";
  expect_parse_error "@keyframes { 0% { opacity: 0; } }"

(* Not a roundtrip test *)
let css_syntax_recovery () =
  let check_strict name css expected =
    match Css.of_string ~strict:true css with
    | Ok { Css.stylesheet; warnings = _; _ } ->
        Alcotest.(check string)
          (name ^ " stylesheet") expected
          (minify stylesheet |> String.trim)
    | Error err ->
        Alcotest.failf "%s should parse strictly: %s" name
          (Cascade.Error.to_string err)
  in
  let check_recovery name css expected min_warnings =
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false css with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s did not recover leniently: %s" name
            (Cascade.Error.to_string err)
    in
    Alcotest.(check string)
      (name ^ " stylesheet") expected
      (minify stylesheet |> String.trim);
    Alcotest.(check bool)
      (name ^ " warning count") true
      (List.length warnings >= min_warnings)
  in
  (* Syntactically valid unknown declarations are preserved (browsers do, and
     dropping changes the stylesheet for vendor properties / future spec
     additions / properties Cascade just doesn't model). Only malformed or
     known-but-invalid declarations are dropped (see [invalid declaration] case
     below). *)
  check_strict "unknown declaration"
    ".btn { unknown-property: value; color: red; }"
    ".btn{unknown-property:value;color:red}";
  check_recovery "invalid declaration"
    ".btn { color: invalid-color; color: red; }" ".btn{color:red}" 1;
  check_recovery "invalid selector list"
    ".ok { color: green } .bad:not() { color: red }" ".ok{color:green}" 1;
  check_recovery "unknown at-rule"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }"
    "@unknown-rule{.bad{color:red}}.ok{color:#00f}" 1

let css_syntax_recovery_structural () =
  let declaration_counts stylesheet =
    Css.rule_statements stylesheet
    |> List.map (fun statement ->
        match Css.as_rule statement with
        | Some (_, declarations, _) -> List.length declarations
        | None -> Alcotest.fail "expected recovered qualified rule")
  in
  let check_counts name css expected_counts min_warnings =
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false css with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s did not recover leniently: %s" name
            (Cascade.Error.to_string err)
    in
    Alcotest.(check (list int))
      (name ^ " declaration counts")
      expected_counts
      (declaration_counts stylesheet);
    Alcotest.(check bool)
      (name ^ " warning count") true
      (List.length warnings >= min_warnings)
  in
  (* CSS Syntax 5.4.4: invalid declarations are discarded; qualified rules
     survive even if every declaration in the rule was invalid. *)
  check_counts "bad declarations leave empty rules"
    ".a { color: rgb(300); } .b { color: red; } .c { width: calc(1px + ); }"
    [ 0; 1; 0 ] 2;
  check_counts "bad declaration does not discard later declaration"
    ".a { color: rgb(300); background-color: red; }" [ 1 ] 1;
  check_counts "bad selector list drops rule only"
    ".ok { color: green } .bad:not() { color: red } .next { color: blue }"
    [ 1; 1 ] 1;
  check_counts "unknown at-rule block skipped"
    "@unknown-rule { .bad { color: red } } .ok { color: blue }" [ 1 ] 1;
  check_counts "unclosed block auto-closed"
    ".a { color: red; .b { color: blue }" [ 1 ] 0

(* CSS Syntax 3 sections 4.3.1 and 5.4.2: an unknown at-rule's at-keyword and
   its prelude are separate tokens, and the whitespace between them is the only
   thing that keeps them apart. Minified output that drops it turns [@foo bar]
   into the at-keyword [@foobar], so the printed text no longer re-parses to the
   value that produced it. Lightning CSS, esbuild, csso, clean-css, and cssnano
   all keep the space. An at-rule with no prelude has no boundary to preserve
   and stays unspaced. *)
let s3431_unknown_at_rule_prelude_separator () =
  let parse input = read (Cursor.of_string input) in
  let unknown input =
    match parse input with
    | [ Unknown_at_rule { name; prelude; block } ] -> (name, prelude, block)
    | _ -> Alcotest.failf "expected a single unknown at-rule for %S" input
  in
  let roundtrips input =
    let printed =
      String.trim (Css.Stylesheet.to_string ~minify:true (parse input))
    in
    let at_rule = Alcotest.(triple string string (option string)) in
    Alcotest.check at_rule (input ^ " roundtrip") (unknown input)
      (unknown printed);
    printed
  in
  Alcotest.(check string)
    "block form keeps the at-keyword boundary" "@foo bar{x:1}"
    (roundtrips "@foo bar{x:1}");
  Alcotest.(check string)
    "statement form keeps the at-keyword boundary" "@foo bar;"
    (roundtrips "@foo bar;");
  Alcotest.(check string)
    "multi-token prelude keeps the at-keyword boundary" "@foo bar baz{x:1}"
    (roundtrips "@foo bar baz{x:1}");
  (* Control: no prelude, so no separator to emit. *)
  Alcotest.(check string)
    "block form without a prelude stays unspaced" "@foo{x:1}"
    (roundtrips "@foo{x:1}");
  Alcotest.(check string)
    "statement form without a prelude stays unspaced" "@foo;"
    (roundtrips "@foo;")

(* CSS Syntax 3 sec. 4.3.1: a backslash is the start of an escape unless a
   newline follows it, so a raw body ending on an odd run of backslashes eats
   the [}] written straight after it and the at-rule never closes. Parsing can
   only produce such a body at EOF, where recovery closes the block again and
   hides the damage; the reachable case is a stylesheet that holds a statement
   after the at-rule, where the escape swallows that statement instead.

   A newline is the only separator that repairs it. Sec. 4.3.7 reads a space or
   a hex digit as part of the escape, so either one changes the last backslash
   from the delim token it was; a newline cannot be escaped, so the delim stays
   a delim and the closer stays a closer. *)
let s3431_unknown_at_rule_trailing_backslash () =
  let parse input = read (Cursor.of_string input) in
  let printed sheet =
    String.trim (Css.Stylesheet.to_string ~minify:true sheet)
  in
  let at_rule body =
    Unknown_at_rule { name = "o"; prelude = "x"; block = Some body }
  in
  let survives name body =
    let sheet = [ at_rule body; List.hd (parse ".b{color:red}") ] in
    Alcotest.(check int)
      (name ^ ": the statement after the at-rule survives a round-trip")
      2
      (List.length (parse (printed sheet)))
  in
  survives "one backslash" " a \\";
  survives "three backslashes" " a \\\\\\";
  (* Control: an even run is a complete escape, and the closer after it already
     closes the block. *)
  survives "two backslashes" " a \\\\";
  Alcotest.(check string)
    "a body ending on a delim backslash is closed after a newline"
    "@o x{ a \\\n}"
    (printed [ at_rule " a \\" ]);
  Alcotest.(check string)
    "an escaped backslash needs no separator" "@o x{ a \\\\}"
    (printed [ at_rule " a \\\\" ])

(* CSS Syntax 3 (editor's draft) sec. 5.5.2 "consume an at-rule": an at-rule is
   an at-keyword, then a prelude ending at the first top-level [;] or [{], then
   either that [;] or a block. Sec. 5.5.9 makes the block a balanced token
   sequence ending at its matching [}], and sec. 4.3.3 reads [@] as an
   at-keyword only when an ident follows it.

   [unknown_at_rule] hands those parts to a caller as text, so it answers for
   every boundary: a part carrying one of its own terminators prints a sheet
   that re-consumes to statements nobody wrote, and an empty name prints an [@]
   delim that re-consumes to nothing at all. *)
let s552_unknown_at_rule_constructor () =
  let printed stmt = String.trim (Css.to_string ~minify:true [ stmt ]) in
  let built what = function
    | Ok stmt -> stmt
    | Error e ->
        Alcotest.failf "%s: unexpected error %s" what
          (Cascade.Error.to_string e)
  in
  let rejected what = function
    | Error _ -> ()
    | Ok stmt ->
        Alcotest.failf "%s: expected an error, built %S" what (printed stmt)
  in
  let parts stmt =
    match Css.of_string_exn (printed stmt) with
    | [ Unknown_at_rule { name; prelude; block } ] -> (name, prelude, block)
    | other ->
        Alcotest.failf "expected one unknown at-rule, got %d statements"
          (List.length other)
  in
  let at_rule = Alcotest.(triple string string (option string)) in
  let block = Css.of_string_exn ".a{color:red}" in
  (* A block cascade does model reaches the body through its own printer, so a
     caller places one without assembling or re-reading a sheet. *)
  let statements =
    built "printed block body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:(Css.to_string ~minify:true block)
         ())
  in
  Alcotest.(check string)
    "a printed block sits inside the at-rule's block"
    "@utility tab-4{.a{color:red}}" (printed statements);
  Alcotest.check at_rule "a printed block re-consumes to the same at-rule"
    ("utility", "tab-4", Some ".a{color:red}")
    (parts statements);
  (* The body of an at-rule cascade does not model travels whole. *)
  let text =
    built "text body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:".a{color:red}" ())
  in
  Alcotest.check at_rule "a text body re-consumes to the same at-rule"
    ("utility", "tab-4", Some ".a{color:red}")
    (parts text);
  (* Sec. 5.5.2: with no block the at-rule ends at its [;]. *)
  let bodyless =
    built "no body" (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ())
  in
  Alcotest.(check string)
    "an at-rule with no body ends at its semicolon" "@utility tab-4;"
    (printed bodyless);
  Alcotest.check at_rule "an at-rule with no body re-consumes to the same one"
    ("utility", "tab-4", None) (parts bodyless);
  (* Every part that carries a terminator of its own is refused, not printed. *)
  rejected "a prelude holding its own block opener"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4{x:1}"
       ~block:"color:red" ());
  rejected "a prelude holding its own terminator"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4;.evil{color:red}" ());
  rejected "a body closing the block early"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
       ~block:"x} .evil{color:red" ());
  rejected "a body that never closes what it opens"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ~block:".a{color:red"
       ());
  rejected "an empty at-keyword"
    (Css.unknown_at_rule ~name:"" ~prelude:"tab-4" ());
  (* Sec. 4.3.2 "consume comments" runs an unclosed [/*] to EOF, so a part that
     opens one swallows the closer written after it and every statement that
     follows. A closed comment is only text and travels like the rest of the
     body. *)
  let commented =
    built "closed comment body"
      (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4"
         ~block:"/* keep */color:red" ())
  in
  Alcotest.check at_rule "a closed comment travels inside the body"
    ("utility", "tab-4", Some "/* keep */color:red")
    (parts commented);
  rejected "a body opening a comment it never closes"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4" ~block:"/* color:red"
       ());
  rejected "a prelude opening a comment it never closes"
    (Css.unknown_at_rule ~name:"utility" ~prelude:"tab-4 /*" ());
  (* An at-keyword cascade does have a grammar for reads back as that at-rule,
     so the statement would disagree with the sheet it prints to. Sec. 5.5.2
     dispatches on the at-keyword, and [media] is one this AST already models
     with a typed condition. *)
  rejected "an at-keyword cascade already models"
    (Css.unknown_at_rule ~name:"media" ~prelude:"screen" ~block:".a{color:red}"
       ())

(* The refusal is per at-rule. Assembling a sheet as text and re-parsing it
   gives the whole buffer one error path, so a single malformed body takes every
   other at-rule with it; building each statement on its own loses only the
   malformed one. *)
let s552_unknown_at_rule_error_is_local () =
  let declared =
    List.map
      (fun (prelude, body) ->
        Css.unknown_at_rule ~name:"utility" ~prelude ~block:body ())
      [
        ("tab-4", "color:red");
        ("bad", "x} .evil{color:blue}");
        ("tab-8", "color:lime");
      ]
  in
  let kept = List.filter_map Result.to_option declared in
  Alcotest.(check int) "only the malformed at-rule is lost" 2 (List.length kept);
  Alcotest.(check int)
    "the survivors print a sheet holding both of them" 2
    (List.length (Css.of_string_exn (Css.to_string ~minify:true kept)))

(* CSS Syntax 3 sec. 5.4.2: an unrecognised at-rule has no grammar to
   re-serialise its body from, so the body travels as the source text between
   its braces. That text is what sits between the at-rule's own braces. Taking
   it from the at-rule's child components instead stops at the last one the
   lexer produced, which is inside the closer whenever the body ends in a nested
   block, and the printed at-rule then never closes. Sec. 5.4.6: an unterminated
   nested construct swallows the closer the other way, leaving none in the
   source to exclude and one for the serializer to supply. *)
let s542_unknown_at_rule_block_body () =
  let printed input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  let roundtrips name input expected =
    Alcotest.(check string) name expected (printed input);
    Alcotest.(check string)
      (name ^ " is a fixed point")
      expected (printed expected)
  in
  roundtrips "a body ending in a nested block keeps both closers"
    "@foo test{div{color:red}}" "@foo test{div{color:red}}";
  roundtrips "nested blocks all the way down" "@foo{a{b{c:d}}}"
    "@foo{a{b{c:d}}}";
  roundtrips "an empty body stays empty" "@foo{}" "@foo{}";
  roundtrips "an unterminated function does not stack closers" "@foo{a:(b}"
    "@foo{a:(b)}";
  roundtrips "an unterminated nested block gets one closer" "@foo{a{b:c"
    "@foo{a{b:c}}"

(* CSS Syntax 3 (ED) sec. 5.5.2 "consume an at-rule" reads a prelude and a block
   whatever the at-keyword spells, and cascade keeps both as the source text it
   cannot re-serialise from a grammar. Sec. 4.3.5 returns the string token at
   end of input and 4.3.6 the url token, both calling it a parse error, 4.3.2
   ends the comment there and calls that one too, and sec. 5.5.9 and 5.5.10
   close a simple block and a function on the EOF token. Each defines what end
   of input leaves, so text that stopped mid-construct means the closed form.
   Written back open, that construct swallows the [;] or [}] the at-rule ends
   with, and the next reader sees one at-rule where cascade held an at-rule and
   the rule after it. *)
let s552_unknown_at_rule_eof_closers () =
  let printed input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  (* The at-rule has to end on its own: append a rule to what was printed and
     both must come back, and re-reading must report nothing left
     unterminated. *)
  let self_delimiting name printed =
    let input = String.concat "" [ printed; ".z{color:red}" ] in
    let { Css.stylesheet; warnings; _ } =
      match Css.of_string ~strict:false input with
      | Ok parsed -> parsed
      | Error err ->
          Alcotest.failf "%s: %S failed to reparse: %s" name input
            (Cascade.Error.to_string err)
    in
    Alcotest.(check int)
      (String.concat "" [ name; ": the rule after it survives" ])
      2
      (List.length (Css.statements stylesheet));
    let unterminated =
      List.filter
        (fun (e : Error.t) ->
          match e.Error.kind with Error.Unterminated _ -> true | _ -> false)
        warnings
    in
    Alcotest.(check int)
      (String.concat "" [ name; ": nothing left unterminated" ])
      0 (List.length unterminated)
  in
  let closes name input expected =
    Alcotest.(check string) name expected (printed input);
    Alcotest.(check string)
      (String.concat "" [ name; " is a fixed point" ])
      expected (printed expected);
    self_delimiting name expected
  in
  closes "a string closes at end of input" "@o x{ a \"i" "@o x{ a \"i\"}";
  closes "a single-quoted string closes too" "@o x{ a 'i" "@o x{ a 'i'}";
  closes "a url closes at end of input" "@o x{ a url(i" "@o x{ a url(i)}";
  closes "a bad url closes too" "@o x{ a url(i j" "@o x{ a url(i j)}";
  closes "a function closes at end of input" "@o x{ a f(i" "@o x{ a f(i)}";
  closes "a square block closes at end of input" "@o x{ a [i" "@o x{ a [i]}";
  closes "a paren block closes at end of input" "@o x{ a (i" "@o x{ a (i)}";
  closes "a curly block closes at end of input" "@o x{ a { b" "@o x{ a { b}}";
  closes "a comment ends at end of input" "@o x{ a /* b" "@o x{ a /* b*/}";
  (* The prelude runs to the [;] the at-rule ends with, and swallows it the same
     way. *)
  closes "a prelude string closes at end of input" "@o \"i" "@o \"i\";";
  closes "a prelude url closes at end of input" "@o url(i" "@o url(i);";
  (* Sec. 4.3.6 reads the whitespace before the missing [)] and keeps none of it
     in the url, so the closer takes its place rather than following it: a
     minified stylesheet carries no newline of its own. *)
  closes "whitespace before a missing closer is not carried" "@o url(i\n"
    "@o url(i);";
  closes "a prelude function closes at end of input" "@o f(i" "@o f(i);";
  (* Controls: nothing is open, so nothing is added. *)
  closes "a closed body is untouched" "@o x{ a b }" "@o x{ a b }";
  closes "a quote inside a comment is not an opener" "@o x{ a /* \" */ b"
    "@o x{ a /* \" */ b}";
  closes "an escaped quote is not an opener" "@o x{ a \"i\\\"j\""
    "@o x{ a \"i\\\"j\"}";
  closes "a closed url is not reopened" "@o x{url(a)}" "@o x{url(a)}";
  closes "a closed prelude is untouched" "@o bar" "@o bar;"

(* Gecko's [@document]/[@-moz-document] takes a comma-separated list of [<url> |
   url-prefix(<string>) | domain(<string>) | media-document(<string>) |
   regexp(<string>)]. Cascade modelled only [url-prefix()], so every other form
   failed the prelude read and the at-rule went down with every rule inside it.
   The five minifiers in [test/interop/lightning] all keep the at-rule verbatim.
   A [<string>] argument stays quoted; a [<url>] takes the shortest spelling. *)
let moz_document_prelude_forms () =
  let roundtrips input =
    String.trim
      (Css.Stylesheet.to_string ~minify:true (read (Cursor.of_string input)))
  in
  List.iter
    (fun (prelude, expected) ->
      let wrap p =
        String.concat "" [ "@-moz-document "; p; "{.b{color:red}}" ]
      in
      Alcotest.(check string)
        prelude (wrap expected)
        (roundtrips (wrap prelude)))
    [
      ("url-prefix(\"x\")", "url-prefix(\"x\")");
      ("url-prefix()", "url-prefix()");
      ("url(x)", "url(x)");
      ("url(\"x\")", "url(x)");
      ("domain(\"example.com\")", "domain(\"example.com\")");
      ("regexp(\"x\")", "regexp(\"x\")");
      ("media-document(\"video\")", "media-document(\"video\")");
      ("url-prefix(\"a\"),domain(\"b\")", "url-prefix(\"a\"),domain(\"b\")");
    ];
  (* A form outside the grammar still takes the at-rule down, which is what CSS
     Syntax 3 sec. 5.4.2 does with a prelude no grammar accepts. The recovering
     reader is the one that drops it; the raw one raises. *)
  match
    Css.of_string ~strict:false
      ".a{color:red}@-moz-document wibble(\"x\"){.b{color:red}}"
  with
  | Error e -> Alcotest.fail (Error.to_string e)
  | Ok { Css.stylesheet; _ } ->
      Alcotest.(check string)
        "an unknown prelude function drops the at-rule" ".a{color:red}"
        (String.trim (Css.Stylesheet.to_string ~minify:true stylesheet))

(* Not a roundtrip test *)
let test_invalid_functions () =
  expect_parse_error ".btn { color: rgb(300); }";
  expect_parse_error ".btn { transform: rotate(); }";
  expect_parse_error ".btn { width: calc(100% +); }";
  expect_parse_error ".btn { background: url(; }"

let cssom67_no_trailing_semicolon () =
  let normalize css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> minify parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let with_trailing = normalize ".a { color: red; padding: 1px; }" in
  let without_trailing = normalize ".a { color: red; padding: 1px }" in
  Alcotest.(check string)
    "trailing semicolon does not affect canonical serialization" with_trailing
    without_trailing;
  Alcotest.(check bool)
    "minified output has no trailing semicolon before }" false
    (Astring.String.is_infix ~affix:";}" with_trailing)

(* An [@container] prelude that does not parse is faulted against the slice of
   the query that failed, so the caret lands inside the query. Every span below
   is counted off the source text: the leading rule is 18 bytes plus a newline,
   so line 2 opens at offset 19 and [@container ] runs to offset 29. *)
let container_condition_error_spans () =
  let check name query (reason, start_pos, end_pos) =
    let input =
      ".ok { color: red }\n@container " ^ query ^ " { .a { color: blue } }\n"
    in
    match Css.of_string ~strict:false input with
    | Error err ->
        Alcotest.failf "%s: lenient parse failed: %s" name
          (Cascade.Error.to_string err)
    | Ok { Css.stylesheet; warnings; _ } -> (
        Alcotest.(check int)
          (name ^ ": sibling rule survives")
          1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] ->
            (match e.Error.kind with
            | Error.Bad_condition { at_rule; reason = got } ->
                Alcotest.(check string)
                  (name ^ ": at-rule") "@container" at_rule;
                Alcotest.(check string) (name ^ ": reason") reason got
            | _ ->
                Alcotest.failf "%s: expected Bad_condition, got %s" name
                  (Error.to_string e));
            Alcotest.(check (pair int int))
              (name ^ ": span") (start_pos, end_pos)
              (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
        | ws ->
            Alcotest.failf "%s: expected one warning, got %d" name
              (List.length ws))
  in
  (* [style()] spans offsets 30-36; an empty argument list has no components of
     its own, so the call itself carries the span. *)
  check "empty style()" "style()" ("empty style() container query", 30, 37);
  (* [scroll-state(] ends at offset 42, so [bogus] spans 43-47. *)
  check "bad scroll-state()" "scroll-state(bogus)"
    ("invalid scroll-state() container query", 43, 48);
  (* [style(] ends at offset 35, so [1px] spans 36-38. *)
  check "bad style() name" "style(1px: red)"
    ("invalid style() container query", 36, 39)

(* One warning off a lenient parse, checked against the at-rule it names, the
   reason it gives and the span it points at. *)
let one_condition_warning name input ~at_rule (reason, start_pos, end_pos) =
  match Css.of_string ~strict:false input with
  | Error err ->
      Alcotest.failf "%s: lenient parse failed: %s" name
        (Cascade.Error.to_string err)
  | Ok { Css.stylesheet; warnings; _ } -> (
      Alcotest.(check int)
        (name ^ ": sibling rule survives")
        1
        (List.length (Css.rule_statements stylesheet));
      match warnings with
      | [ e ] ->
          (match e.Error.kind with
          | Error.Bad_condition { at_rule = got_rule; reason = got } ->
              Alcotest.(check string) (name ^ ": at-rule") at_rule got_rule;
              Alcotest.(check string) (name ^ ": reason") reason got
          | _ ->
              Alcotest.failf "%s: expected Bad_condition, got %s" name
                (Error.to_string e));
          Alcotest.(check (pair int int))
            (name ^ ": span") (start_pos, end_pos)
            (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
      | ws ->
          Alcotest.failf "%s: expected one warning, got %d" name
            (List.length ws))

(* A media query that does not parse is faulted against the slice of the query
   that failed. Spans are counted off the source text: the leading rule is 18
   bytes plus a newline, so line 2 opens at offset 19 and [@media ] runs to
   offset 25. *)
let media_condition_error_spans () =
  let check name query expected =
    let input =
      ".ok { color: red }\n@media " ^ query ^ " { .a { color: blue } }\n"
    in
    one_condition_warning name input ~at_rule:"@media" expected
  in
  (* A lone [not] prefixes nothing; the query itself spans offsets 26-28. *)
  check "prefix with no type" "not" ("expected media type or condition", 26, 29);
  (* The [or] that follows an [and] spans offsets 46-47. *)
  check "mixed operators" "(color) and (hover) or (a)"
    ("mixed 'and'/'or' media condition", 46, 48);
  check_stylesheet ~expected:"@media(bogus !!!){.a{color:blue}}"
    "@media (bogus !!!) { .a { color: blue } }";
  check_import_rule ~expected:"@import\"a.css\"(bogus !!!);"
    "@import url(\"a.css\") (bogus !!!);"

(* An @font-face descriptor whose value does not parse is faulted against the
   value, not against the enclosing block. The descriptor is dropped and the
   rest of the at-rule is kept, so the [src] below keeps the @font-face valid
   and the warning list holds only the descriptor failure. Spans are counted off
   the source text: the leading rule is 18 bytes plus a newline, so line 2 opens
   at offset 19. *)
let descriptor_value_error_spans () =
  let check name descriptor (reason, start_pos, end_pos) =
    let input =
      ".ok { color: red }\n@font-face { font-family: x; src: url(a.woff2); "
      ^ descriptor ^ " }\n"
    in
    match Css.of_string ~strict:false input with
    | Error err ->
        Alcotest.failf "%s: lenient parse failed: %s" name
          (Cascade.Error.to_string err)
    | Ok { Css.stylesheet; warnings; _ } -> (
        Alcotest.(check int)
          (name ^ ": sibling rule survives")
          1
          (List.length (Css.rule_statements stylesheet));
        match warnings with
        | [ e ] ->
            (match e.Error.kind with
            | Error.Bad_value { reason = got; _ } ->
                Alcotest.(check string) (name ^ ": reason") reason got
            | _ ->
                Alcotest.failf "%s: expected Bad_value, got %s" name
                  (Error.to_string e));
            Alcotest.(check (pair int int))
              (name ^ ": span") (start_pos, end_pos)
              (e.Error.loc.Loc.start_pos, e.Error.loc.Loc.end_pos)
        | ws ->
            Alcotest.failf "%s: expected one warning, got %d" name
              (List.length ws))
  in
  (* [ascent-override] opens at offset 67, so its value spans 84-86. *)
  check "negative metric override" "ascent-override: -5%"
    ("invalid: metric override", 84, 87);
  (* [size-adjust] opens at offset 67, so its value spans 80-84. *)
  check "size-adjust that is not a percentage" "size-adjust: bogus"
    ("invalid: size-adjust", 80, 85)

(* An @supports condition that does not parse is faulted against the slice of
   the condition that failed, where the reader used to hand back a bare reason
   that the stylesheet re-anchored on the whole prelude. Spans are counted off
   the source text: the leading rule is 18 bytes plus a newline, so line 2 opens
   at offset 19 and [@supports ] runs to offset 28. *)
let supports_condition_error_spans () =
  let check name condition expected =
    let input =
      ".ok { color: red }\n@supports " ^ condition ^ " { .a { color: blue } }\n"
    in
    one_condition_warning name input ~at_rule:"@supports" expected
  in
  (* [extra-junk] follows a complete feature query and spans offsets 45-54. *)
  check "trailing content" "(display: grid) extra-junk"
    ("trailing content", 45, 55);
  (* The [or] that follows an [and] spans offsets 45-46. *)
  check "mixed operators" "(a:b) and (c:d) or (e:f)"
    ("Cannot mix and/or without parentheses in @supports", 45, 47);
  check_stylesheet ~expected:"@supports(){.a{color:blue}}"
    "@supports () { .a { color: blue } }";
  check_stylesheet ~expected:"@supports font-format(bogus){.a{color:blue}}"
    "@supports font-format(bogus) { .a { color: blue } }"

let suite =
  ( "stylesheet_syntax",
    [
      ("check function", `Quick, test_check);
      ("import_rule", `Quick, test_import_rule);
      ("layer_name", `Quick, test_layer_name);
      (* Positive tests *)
      ("advanced selectors", `Quick, test_advanced_selectors);
      ("advanced properties", `Quick, test_advanced_properties);
      ("complex values", `Quick, test_complex_values);
      ("nested rules", `Quick, test_nested_rules);
      ("spec section 7.1 block grammar examples", `Quick, spec_s7_block_examples);
      ( "spec section 8.1-8.2 stylesheet rule shapes",
        `Quick,
        spec_s8_rule_shapes );
      ( "spec namespace prefix serialization",
        `Quick,
        spec_namespace_serialization );
      ( "spec section 8.3 charset is not a rule",
        `Quick,
        spec_s8_charset_not_rule );
      ( "spec CSS Syntax structural recovery",
        `Quick,
        css_syntax_recovery_structural );
      ( "spec CSS Syntax 5.4.2 unknown at-rule block body",
        `Quick,
        s542_unknown_at_rule_block_body );
      ( "spec CSS Syntax 5.5.2 unknown at-rule raw text closes at EOF",
        `Quick,
        s552_unknown_at_rule_eof_closers );
      ( "spec CSS Syntax 4.3.1 unknown at-rule prelude separator",
        `Quick,
        s3431_unknown_at_rule_prelude_separator );
      ( "spec CSS Syntax 4.3.1 unknown at-rule trailing backslash",
        `Quick,
        s3431_unknown_at_rule_trailing_backslash );
      ( "spec CSS Syntax 5.5.2 unknown at-rule constructor",
        `Quick,
        s552_unknown_at_rule_constructor );
      ( "spec CSS Syntax 5.5.2 unknown at-rule constructor reports locally",
        `Quick,
        s552_unknown_at_rule_error_is_local );
      ("spec @-moz-document prelude forms", `Quick, moz_document_prelude_forms);
      ( "spec CSSOM 6.7 no trailing semicolon",
        `Quick,
        cssom67_no_trailing_semicolon );
      (* Negative tests *)
      ("invalid selectors", `Quick, test_invalid_selectors);
      ("invalid properties", `Quick, test_invalid_properties);
      ("invalid syntax", `Quick, test_invalid_syntax);
      ("invalid at-rules", `Quick, test_invalid_at_rules);
      ("spec CSS Syntax recovery", `Quick, css_syntax_recovery);
      ("invalid functions", `Quick, test_invalid_functions);
      ( "an @container query error points at the failing slice",
        `Quick,
        container_condition_error_spans );
      ( "a media query error points at the failing slice",
        `Quick,
        media_condition_error_spans );
      ( "an @font-face descriptor error points at the value",
        `Quick,
        descriptor_value_error_spans );
      ( "an @supports condition error points at the failing slice",
        `Quick,
        supports_condition_error_spans );
    ] )
