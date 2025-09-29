(** Tests for CSS Selector module *)

open Css.Selector
open Test_helpers

let check_nth = check_value "nth" read_nth pp_nth
let check_combinator = check_value "combinator" read_combinator pp_combinator
let check = check_value "selector" read pp
let check_ns = check_value "ns" read_ns (Css.Pp.option pp_ns)

(* Helper for checking invalid selectors *)
let check_invalid name exn_msg f =
  check_raises name (Invalid_argument exn_msg) f

let check_construct expected selector =
  check_construct expected (to_string ~minify:true) expected selector

(* Helper to check that a function returning an option returns None *)
let none : type a. (Css.Reader.t -> a option) -> string -> unit =
 fun reader input ->
  let t = Css.Reader.of_string input in
  match reader t with
  | None -> ()
  | Some _ -> Alcotest.failf "Expected no value for '%s' but found one" input

(* Not a roundtrip test *)
let element_cases () =
  (* Test element selectors *)
  check_construct "div" (element "div");
  check_construct "span" (element "span");
  check_construct "h1" (element "h1");
  check_construct "article" (element "article");
  check_construct "custom-element" (element "custom-element")

(* Not a roundtrip test *)
let class_cases () =
  (* Test class selectors *)
  check_construct ".test" (class_ "test");
  check_construct ".test-class" (class_ "test-class");
  check_construct ".test_class" (class_ "test_class");
  check_construct ".test123" (class_ "test123")

(* Not a roundtrip test *)
let id_cases () =
  (* Test ID selectors *)
  check_construct "#myid" (id "myid");
  check_construct "#my-id" (id "my-id");
  check_construct "#my_id" (id "my_id");
  check_construct "#id123" (id "id123")

(* Not a roundtrip test *)
let pseudo_class_cases () =
  (* Test pseudo-class selectors *)
  check_construct ":hover" Hover;
  check_construct ":active" Active;
  check_construct ":focus" Focus;
  check_construct ":first-child" First_child;
  check_construct ":last-child" Last_child;
  check_construct ":nth-child(2)" (nth_child (An_plus_b (0, 2)));
  check_construct ":nth-child(odd)" (nth_child Odd);
  check_construct ":nth-child(even)" (nth_child Even);
  check_construct ":nth-child(2n+1)" (nth_child (An_plus_b (2, 1)));
  (* nth with Index and of clause *)
  check ":nth-child(5)";
  check ~expected:":nth-child(odd of .item)" ":nth-child( odd of .item )";
  check ~expected:":nth-child(2n-1 of a,b)" ":nth-child( 2n-1 of a , b )";
  check ":nth-of-type(3)";
  check ~expected:":nth-last-child(2 of .x,.y)" ":nth-last-child(2 of .x , .y)";
  check ~expected:":nth-last-of-type(2n+2 of h1,.a)"
    ":nth-last-of-type(2n+2 of h1 , .a)";

  (* Additional link-related pseudo-classes *)
  check ":link";
  check ":visited";

  (* Form-related pseudo-classes *)
  check ":enabled";
  check ":disabled";
  check ":checked";
  check ":required";
  check ":optional";
  check ":valid";
  check ":invalid";
  check ":in-range";
  check ":out-of-range";
  check ":read-only";
  check ":read-write";

  (* Structural pseudo-classes *)
  check ":first-of-type";
  check ":last-of-type";
  check ":only-child";
  check ":only-of-type";
  check ":empty";

  (* Target and state pseudo-classes *)
  check ":target";
  check ":focus-visible";
  check ":focus-within";

  (* Language and direction pseudo-classes *)
  check ":lang(en)";
  check ":dir(ltr)";
  check ":dir(rtl)"

(* Not a roundtrip test *)
let pseudo_element_cases () =
  (* Test pseudo-element selectors *)
  check_construct ":before" Before;
  check_construct ":after" After;
  check_construct ":first-line" First_line;
  check_construct ":first-letter" First_letter;
  check_construct "::marker" Marker;

  (* Double-colon syntax (preferred in CSS3+) *)
  check "::before";
  check "::after";
  check "::first-line";
  check "::first-letter";

  (* Additional modern pseudo-elements *)
  check "::placeholder";
  check "::selection";
  check "::backdrop";
  check "::file-selector-button";

  (* Functional pseudo-elements *)
  check "::part(tab)";
  check "::slotted(p)";
  check "::slotted(.highlight)";

  (* Shadow DOM pseudo-elements (vendor-prefixed) *)
  check "::-webkit-input-placeholder";
  check "::-moz-placeholder";
  check "::-webkit-search-cancel-button";
  check "::-webkit-scrollbar";
  check "::-webkit-scrollbar-thumb"

(* Not a roundtrip test *)
let attribute_cases () =
  (* Basic attribute selectors *)
  check_construct "[href]" (attribute "href" Presence);
  check_construct "[type=text]" (attribute "type" (Exact "text"));
  check_construct "[class~=active]"
    (attribute "class" (Whitespace_list "active"));
  check_construct "[href^=https]" (attribute "href" (Prefix "https"));
  check_construct "[href$=\".pdf\"]" (attribute "href" (Suffix ".pdf"));
  check_construct "[title*=hello]" (attribute "title" (Substring "hello"));
  check_construct "[lang|=en]" (attribute "lang" (Hyphen_list "en"));

  (* Additional positive cases *)
  check_construct "[data-x=v]" (attribute "data-x" (Exact "v"));
  check_construct "[title^=Pre]" (attribute "title" (Prefix "Pre"));
  check_construct "[name$=end]" (attribute "name" (Suffix "end"));
  check_construct "[cls*=part]" (attribute "cls" (Substring "part"));
  check_construct "[role~=button]" (attribute "role" (Whitespace_list "button"));
  check_construct "[lang|=en]" (attribute "lang" (Hyphen_list "en"));

  (* Test attribute value quoting according to CSS spec *)
  (* Test cases where quotes are REQUIRED per CSS spec *)
  (* Values with spaces always require quotes *)
  check "[class=\"my class\"]";
  check "[data-value=\"hello world\"]";

  (* Values starting with digits require quotes *)
  check "[data-id=\"123\"]";
  check "[data-version=\"2.0\"]";

  (* Values with special characters require quotes *)
  check "[href=\"http://example.com\"]";

  (* Test attribute values for prose type selectors that need quoting *)
  (* Values with spaces like "A s", "a s", "I s", "i s" require quotes *)
  check_construct "[type=\"A s\"]" (attribute "type" (Exact "A s"));
  check_construct "[type=\"a s\"]" (attribute "type" (Exact "a s"));
  check_construct "[type=\"I s\"]" (attribute "type" (Exact "I s"));
  check_construct "[type=\"i s\"]" (attribute "type" (Exact "i s"));

  (* Single letter values don't need quotes *)
  check_construct "[type=A]" (attribute "type" (Exact "A"));
  check_construct "[type=a]" (attribute "type" (Exact "a"));
  check_construct "[type=I]" (attribute "type" (Exact "I"));
  check_construct "[type=i]" (attribute "type" (Exact "i"));

  (* Numeric values need quotes *)
  check_construct "[type=\"1\"]" (attribute "type" (Exact "1"));
  check "[data-path=\"/home/user\"]";
  check "[content=\"Hello, World!\"]";

  (* Values starting with double hyphen require quotes *)
  check "[id=\"--custom\"]";
  check "[class=\"--modifier\"]";

  (* Test cases where quotes are OPTIONAL per CSS spec *)
  (* Simple identifiers don't need quotes - test both forms are accepted and normalized *)
  check "[type=button]";
  check ~expected:"[type=button]" "[type=\"button\"]";
  (* Normalizes to unquoted *)
  check "[data-foo=bar]";
  check ~expected:"[data-foo=bar]" "[data-foo=\"bar\"]";

  (* Normalizes to unquoted *)

  (* Values with hyphens and underscores (valid identifiers) *)
  check "[data-name=foo-bar_123]";
  check ~expected:"[data-name=foo-bar_123]" "[data-name=\"foo-bar_123\"]";

  (* Normalizes *)

  (* Test roundtrip normalization *)
  (* Values that don't need quotes should remain unquoted *)
  check ~expected:"[type=button]" "[type=button]";
  check ~expected:"[type=button]" "[type=\"button\"]";
  (* Normalizes to unquoted *)
  check ~expected:"[data-foo=bar_baz]" "[data-foo=bar_baz]";

  (* Values that need quotes should remain quoted *)
  check ~expected:"[class=\"my class\"]" "[class=\"my class\"]";
  check ~expected:"[data-id=\"123\"]" "[data-id=\"123\"]";
  check ~expected:"[href=\"http://example.com\"]"
    "[href=\"http://example.com\"]";

  (* Test in complex selectors like :where() *)
  check "input:where([type=button],[type=reset])";
  check ~expected:"input:where([type=button],[type=reset])"
    "input:where([type=\"button\"],[type=\"reset\"])";

  (* Test different matching operators with quoting *)
  check "[class~=foo]";
  check "[class~=\"foo bar\"]";
  (* Needs quotes due to space *)
  check "[lang|=en]";
  check "[href^=\"http://\"]";
  (* Needs quotes due to special chars *)
  check "[href$=\".pdf\"]";
  (* Needs quotes due to dot *)
  check "[href*=example]";

  (* Test case sensitivity flags with different quoting *)
  check "[type=button i]";
  check ~expected:"[type=button i]" "[type=\"button\" i]";
  (* Normalizes to unquoted *)
  check "[class=\"My Class\" s]";

  (* Case modifiers *)
  check_construct "[attr=v i]"
    (attribute ~flag:Case_insensitive "attr" (Exact "v"));
  check_construct "[attr=v s]"
    (attribute ~flag:Case_sensitive "attr" (Exact "v"));

  (* Namespaced attributes *)
  check_construct "[ns|attr]" (attribute ~ns:(Prefix "ns") "attr" Presence);
  check_construct "[*|attr]" (attribute ~ns:Any "attr" Presence);

  (* Negative cases *)
  neg read "[";
  (* Missing closing ] *)
  neg read "[attr=]";
  (* Missing value *)
  neg read "[=value]";
  (* Empty attribute name *)
  neg read "[attr&=value]";
  (* Invalid operator *)
  neg read "[attr=\"value]";
  (* Unterminated string *)
  neg read "[]" (* Empty attribute *)

(* Not a roundtrip test *)
let combinator_cases () =
  (* Test combinators *)
  check_construct ".parent .child" (class_ "parent" ++ class_ "child");
  check_construct ".parent>.child" (class_ "parent" >> class_ "child");
  check_construct ".prev+.next"
    (combine (class_ "prev") Next_sibling (class_ "next"));
  check_construct ".first~.later"
    (combine (class_ "first") Subsequent_sibling (class_ "later"));
  check_construct ".col1||.col2"
    (combine (class_ "col1") Column (class_ "col2"))

(* Not a roundtrip test *)
let compound_cases () =
  (* Test compound selectors *)
  check_construct "div.container" (element "div" && class_ "container");
  check_construct "div#main" (element "div" && id "main");
  check_construct ".btn.primary" (class_ "btn" && class_ "primary");
  check_construct "a:hover" (element "a" && Hover);
  check_construct ".link[href]" (class_ "link" && attribute "href" Presence)

(* Not a roundtrip test *)
let list_cases () =
  (* Test selector lists *)
  check_construct ".a,.b,.c" (list [ class_ "a"; class_ "b"; class_ "c" ]);
  check_construct "h1,h2,h3" (list [ element "h1"; element "h2"; element "h3" ]);
  check_construct "div,.class,#id"
    (list [ element "div"; class_ "class"; id "id" ]);

  (* Complex grouped selectors *)
  check ~expected:".parent>.child,.other-parent .descendant"
    ".parent > .child, .other-parent .descendant";
  check ~expected:"h1:hover,h2:focus,h3:active" "h1:hover, h2:focus, h3:active";
  check ~expected:"[data-attr],[aria-label],[role=button]"
    "[data-attr], [aria-label], [role=button]";
  check ~expected:"::before,::after,::first-letter"
    "::before, ::after, ::first-letter";

  (* Mixed complexity grouped selectors *)
  check ~expected:"div.container>p:first-child,section#main .highlight"
    "div.container > p:first-child, section#main .highlight";
  check ~expected:"input[type=text]:focus,textarea:focus,select:focus"
    "input[type=text]:focus, textarea:focus, select:focus";
  check ~expected:".nav li:hover,.nav li.active,.nav li:focus-within"
    ".nav li:hover, .nav li.active, .nav li:focus-within";

  (* Whitespace variations in grouped selectors *)
  check ~expected:"a,b,c" "a, b, c";
  check ~expected:"a,b,c" "a , b , c";
  check ~expected:".class1,.class2" ".class1,.class2";
  check ~expected:".class1,.class2" ".class1 , .class2";

  (* Many grouped selectors *)
  check ~expected:"p,div,span,section,article,aside,nav,header,footer,main"
    "p, div, span, section, article, aside, nav, header, footer, main";

  (* Grouped selectors with pseudo-elements and classes *)
  check ~expected:"button:hover,a:hover,input[type=submit]:hover"
    "button:hover, a:hover, input[type=submit]:hover";
  check ~expected:"h1::before,h2::before,h3::before,h4::before"
    "h1::before, h2::before, h3::before, h4::before";

  (* Deeply nested grouped selectors *)
  check ~expected:".level1 .level2 .level3,#id1>#id2>#id3"
    ".level1 .level2 .level3, #id1 > #id2 > #id3";
  check ~expected:".sidebar .widget .title,.main .article .header"
    ".sidebar .widget .title, .main .article .header";

  (* Edge cases *)
  (* Note: Trailing/leading commas are invalid CSS and should fail parsing *)
  check ".a,.b,.c,.d,.e,.f,.g,.h,.i,.j";

  (* Many items *)

  (* Negative tests for malformed lists - should fail parsing *)
  neg read ".a,";
  (* Trailing comma is invalid *)
  neg read ",.b";
  (* Leading comma is invalid *)
  neg read ".a,,b";
  (* Double comma is invalid *)
  ()

(* Not a roundtrip test *)
let where_is_cases () =
  (* Test :where() and :is() *)
  check_construct ":where(div)" (where [ element "div" ]);
  check_construct ":where(.a,.b)" (where [ class_ "a"; class_ "b" ]);
  check_construct ":is(h1,h2)" (is_ [ element "h1"; element "h2" ]);
  check_construct ":not(.active)" (not [ class_ "active" ]);
  ()

(* Test parsing roundtrips *)
let roundtrip () =
  (* Basic selectors *)
  check "div";
  check ".class";
  check "#id";
  check "*";

  (* Pseudo-classes *)
  check ":hover";
  check ":nth-child(2)";
  check ":nth-child(2n+1)";
  check ":nth-child(odd)";
  check ":nth-child(even)";

  (* Pseudo-elements *)
  check "::before";
  check "::after";
  check "::part(foo)";
  check "::slotted(.class)";

  (* Attributes *)
  check "[href]";
  check ~expected:"[type=text]" "[type=\"text\"]";
  check ~expected:"[class~=active]" "[class~=\"active\"]";
  check ~expected:"[href^=https]" "[href^=\"https\"]";

  (* Combinators *)
  check ".parent .child";
  check ~expected:".parent>.child" ".parent > .child";
  check ~expected:".prev+.next" ".prev + .next";
  check ~expected:".first~.later" ".first ~ .later";

  (* Complex selectors *)
  check "div.class#id[href]:hover::after";
  check ~expected:".a,.b,.c" ".a, .b, .c";
  check ":where(.a,.b)";
  check ":is(h1,h2,h3)";
  check ":not(.active)"

(* Not a roundtrip test *)
(* Test invalid selectors *)
let invalid () =
  (* Empty identifier *)
  check_invalid "empty class" "CSS identifier '' cannot be empty" (fun () ->
      ignore (class_ ""));

  (* Starting with digit *)
  check_invalid "digit start element"
    "CSS identifier '9div' cannot start with digit" (fun () ->
      ignore (element "9div"));

  check_invalid "digit start class"
    "CSS identifier '9class' cannot start with digit" (fun () ->
      ignore (class_ "9class"));

  (* Double dash reserved for custom properties *)
  check_invalid "double dash class"
    "CSS identifier '--var' cannot start with '--' (reserved for custom \
     properties)" (fun () -> ignore (class_ "--var"));

  (* Dash followed by digit *)
  check_invalid "dash digit"
    "CSS identifier '-9' cannot start with '-' followed by digit" (fun () ->
      ignore (id "-9"));

  (* Invalid characters *)
  check_invalid "space in class"
    "CSS identifier 'my class' contains invalid character ' ' at position 2"
    (fun () -> ignore (class_ "my class"));

  check_invalid "dot in class"
    "CSS identifier 'my.class' contains invalid character '.' at position 2"
    (fun () -> ignore (class_ "my.class"));

  check_invalid "hash in id"
    "CSS identifier 'my#id' contains invalid character '#' at position 2"
    (fun () -> ignore (id "my#id"));

  (* Parsing invalid selector strings via Reader.option to avoid exceptions *)
  let open Css.Reader in
  let neg_parse s label =
    let r = of_string s in
    Alcotest.(check bool) label true (Option.is_none (Css.Reader.option read r))
  in
  neg_parse "[href" "unterminated attribute selector";
  neg_parse ":nth-child(2n+)" "invalid nth-child syntax";
  neg_parse ":unknown-pseudo(" "unterminated pseudo with paren";
  neg_parse ".class,,.other" "double comma in list";
  neg_parse "div > > span" "double combinator";
  neg_parse "[attr=value" "unterminated attribute value"

(* Test broken selectors with Parse_error exceptions *)
let check_parse_error input expected_msg =
  let t = Css.Reader.of_string input in
  try
    let _ = read t in
    Alcotest.failf "expected Parse_error for '%s' but parsing succeeded" input
  with
  | Css.Reader.Parse_error err ->
      if err.message <> expected_msg then
        Alcotest.failf "For input '%s':\n  expected: '%s'\n  got: '%s'" input
          expected_msg err.message
  | exn ->
      Alcotest.failf "For '%s': expected Parse_error but got %s" input
        (Printexc.to_string exn)

let parse_errors_attributes () =
  check_parse_error "[class=\"test\"" "Expected ']' but reached end of input";
  check_parse_error ".test[data-id=\"123\""
    "Expected ']' but reached end of input";
  check_parse_error ".test[]" "expected identifier";
  check_parse_error ".test[[attr]]" "expected identifier";
  check_parse_error ".test[data id=\"value\"]" "Expected ']' but got 'd'";
  check_parse_error ".test[data-id=value with spaces]"
    "Expected ']' but got 'w'"

let parse_errors_combinators () =
  check_parse_error ".test >> .child" "expected at least one selector";
  check_parse_error ".parent + + .child" "expected at least one selector";
  check_parse_error ".parent >" "expected at least one selector";
  check_parse_error ">" "expected at least one selector";
  check_parse_error ".parent ~> .child" "expected at least one selector"

let parse_errors_starts () =
  check_parse_error ".123test" "expected identifier";
  check_parse_error "#123" "expected identifier";
  check_parse_error "*.*" "expected identifier"

let parse_errors_pseudo () =
  check_parse_error ".test:not()" "expected at least one selector";
  check_parse_error ".test:not(.other" "Expected ')' but reached end of input";
  check_parse_error ":is()" "expected at least one selector";
  check_parse_error ".test:has()" "expected at least one selector"

let parse_errors_empty_list () =
  check_parse_error ", ," "expected at least one selector";
  check_parse_error ", h1, h2" "expected at least one selector";
  check_parse_error "h1, h2," "expected at least one selector"

let parse_errors_complex () =
  check_parse_error ".parent > [data-id=\"test\" .child:hover"
    "Expected ']' but got '.'";
  check_parse_error "body > main > section[data-tooltip"
    "Expected ']' but reached end of input";
  check_parse_error ".test[attr1=\"val1\"][attr4$="
    "Expected ']' but reached end of input";
  check_parse_error ".first ~ .second ~ [invalid"
    "Expected ']' but reached end of input"

(* Helpers for callstack accuracy checks to reduce nesting *)
let contains_substring haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  if len_n = 0 then true
  else
    let rec loop i =
      if i > len_h - len_n then false
      else if String.sub haystack i len_n = needle then true
      else loop (i + 1)
    in
    loop 0

let check_callstack name input expected_stack_parts =
  let t = Css.Reader.of_string input in
  try
    let _ = read t in
    Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
  with
  | Css.Reader.Parse_error err ->
      let callstack_str = String.concat " -> " err.callstack in
      List.iter
        (fun stack_item ->
          if Bool.not @@ contains_substring callstack_str stack_item then
            Alcotest.failf "%s: expected callstack containing '%s' but got '%s'"
              name stack_item callstack_str)
        expected_stack_parts;
      if err.position < 0 then
        Alcotest.failf "%s: position should be >= 0 but got %d" name
          err.position;
      if String.length err.context_window = 0 then
        Alcotest.failf "%s: context_window should not be empty" name
  | exn ->
      Alcotest.failf "%s: expected Parse_error but got %s" name
        (Printexc.to_string exn)

let check_full_css_callstack name css_input expected_stack_parts =
  match Css.of_string css_input with
  | Ok _ -> Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
  | Error err ->
      let callstack_str = String.concat " -> " err.callstack in
      List.iter
        (fun stack_item ->
          if Bool.not @@ contains_substring callstack_str stack_item then
            Alcotest.failf "%s: expected callstack containing '%s' but got '%s'"
              name stack_item callstack_str)
        expected_stack_parts

(* Test callstack accuracy for selector errors *)
let callstack_accuracy () =
  (* When parsing selectors directly (not through full CSS), callstack is
     shallower *)
  check_callstack "selector_list_error" ".test[[attr]]" [ "list" ];
  check_callstack "combinator_error" ".parent + +" [ "list" ];
  check_callstack "empty_selector" ", ," [ "list" ];

  (* These fail at even higher level when parsing selectors directly *)
  check_callstack "pseudo_function_error" ".test:not()" [];
  (* No specific context when selector fails early *)
  check_callstack "invalid_pseudo" ":is()" [];

  (* Full CSS parsing should show complete callstack *)
  check_full_css_callstack "full_css_selector_error"
    ".test[[attr]] { color: red; }"
    [ "stylesheet"; "rule"; "list" ];
  check_full_css_callstack "full_css_pseudo_error" ".test:not() { color: red; }"
    [ "stylesheet"; "rule" ];

  (* Escaped characters like \! are valid in CSS - they represent the literal
     character. So .test\!class is equivalent to .test!class which is a valid
     class selector. Testing an actually invalid selector instead: *)
  check_full_css_callstack "invalid_selector_error"
    ".test[[attr]] { color: red; }"
    [ "stylesheet"; "rule"; "list" ]

(* Test check functions for selector components *)
let component_parsing () =
  (* Test nth values *)
  check_nth "2n+1";
  check_nth "odd";
  check_nth "even";
  check_nth "3n";
  check_nth "5";

  (* Test combinators *)
  check_combinator ">";
  check_combinator "+";
  check_combinator "~";
  check_combinator "||";

  (* Test namespace *)
  check_ns "svg|";
  check_ns "xml|";
  check_ns "*|"

let test_attribute_match () =
  (* Test attribute matching types - these parse just the operator part *)
  let check_attribute_match =
    check_value "attribute_match" read_attribute_match pp_attribute_match
  in

  (* Presence match - empty string yields Presence *)
  check_attribute_match "";
  (* Exact match *)
  check_attribute_match "=test";
  (* Whitespace list match *)
  check_attribute_match "~=word";
  (* Hyphen list match *)
  check_attribute_match "|=lang";
  (* Prefix match *)
  check_attribute_match "^=prefix";
  (* Suffix match *)
  check_attribute_match "$=suffix";
  (* Substring match *)
  check_attribute_match "*=substring";

  (* Test invalid attribute matches *)
  neg read_attribute_match "%=invalid";
  (* Invalid operator *)
  neg read_attribute_match "!=not-equal";
  (* Not supported *)
  neg read_attribute_match "=";
  (* Missing value *)
  neg read_attribute_match "~=" (* Missing value *)

let test_attr_value_quoting () =
  (* Test the attr_value_needs_quoting function behavior *)
  let module S = Css.Selector in
  (* Helper to check if a value needs quoting *)
  let check_needs_quoting value expected =
    let actual = S.attr_value_needs_quoting value in
    Alcotest.(check bool)
      (Fmt.str "attr_value_needs_quoting %S" value)
      expected actual
  in

  (* Values that need quotes *)
  check_needs_quoting "" true;
  (* Empty string *)
  check_needs_quoting "A s" true;
  (* Contains space *)
  check_needs_quoting "a s" true;
  (* Contains space *)
  check_needs_quoting "I s" true;
  (* Contains space *)
  check_needs_quoting "i s" true;
  (* Contains space *)
  check_needs_quoting "hello world" true;
  (* Contains space *)
  check_needs_quoting "1" true;
  (* Starts with digit *)
  check_needs_quoting "123" true;
  (* Starts with digit *)
  check_needs_quoting "2.0" true;
  (* Starts with digit *)
  check_needs_quoting "--custom" true;
  (* Starts with double hyphen *)
  check_needs_quoting "a!b" true;
  (* Contains special character *)
  check_needs_quoting "a@b" true;
  (* Contains special character *)
  check_needs_quoting "a#b" true;

  (* Contains special character *)

  (* Values that don't need quotes *)
  check_needs_quoting "A" false;
  (* Single letter *)
  check_needs_quoting "a" false;
  (* Single letter *)
  check_needs_quoting "I" false;
  (* Single letter *)
  check_needs_quoting "i" false;
  (* Single letter *)
  check_needs_quoting "text" false;
  (* Simple identifier *)
  check_needs_quoting "my-class" false;
  (* With hyphen *)
  check_needs_quoting "my_id" false;
  (* With underscore *)
  check_needs_quoting "value123" false;
  (* Letters then digits *)
  check_needs_quoting "-webkit" false (* Single hyphen prefix *)

let test_attr_flag () =
  (* Test attribute selector flags - returns option type *)
  let check_attr_flag = check_value "attr_flag" read_attr_flag pp_attr_flag in

  (* Case insensitive flag *)
  check_attr_flag ~expected:" i" "i";
  (* Case sensitive flag *)
  check_attr_flag ~expected:" s" "s";
  (* No flag / empty should return None *)
  check_attr_flag "";

  (* Test invalid flags using neg *)
  neg read_attr_flag "x";
  (* Invalid flag *)
  neg read_attr_flag "I";
  (* Wrong case *)
  neg read_attr_flag "S";
  (* Wrong case *)
  neg read_attr_flag "is" (* Multiple characters *)

let test_attr_case_sensitivity_flags () =
  (* Test CSS Level 4 case-sensitivity flags (i and s) in attribute selectors *)
  let module S = Css.Selector in
  (* Test selector output with case-sensitivity flags *)
  let check_selector_with_flag value flag expected =
    let sel = S.attribute ?flag "type" (Exact value) in
    let output = Css.Pp.to_string ~minify:true S.pp sel in
    Alcotest.(check string)
      (Fmt.str "selector for type=%S with flag %s" value
         (match flag with
         | None -> "none"
         | Some Case_insensitive -> "i"
         | Some Case_sensitive -> "s"))
      expected output
  in

  (* Without flag *)
  check_selector_with_flag "A" None "[type=A]";
  check_selector_with_flag "a" None "[type=a]";
  check_selector_with_flag "I" None "[type=I]";
  check_selector_with_flag "i" None "[type=i]";

  (* With case-insensitive flag (i) *)
  check_selector_with_flag "A" (Some Case_insensitive) "[type=A i]";
  check_selector_with_flag "a" (Some Case_insensitive) "[type=a i]";
  check_selector_with_flag "foo" (Some Case_insensitive) "[type=foo i]";

  (* With case-sensitive flag (s) *)
  check_selector_with_flag "A" (Some Case_sensitive) "[type=A s]";
  check_selector_with_flag "a" (Some Case_sensitive) "[type=a s]";
  check_selector_with_flag "I" (Some Case_sensitive) "[type=I s]";
  check_selector_with_flag "i" (Some Case_sensitive) "[type=i s]";
  check_selector_with_flag "foo" (Some Case_sensitive) "[type=foo s]";

  (* Values with spaces should be quoted regardless of flag *)
  let sel = S.attribute ~flag:Case_sensitive "type" (Exact "A B") in
  let output = Css.Pp.to_string ~minify:true S.pp sel in
  Alcotest.(check string)
    "value with space and s flag" "[type=\"A B\" s]" output;

  let sel = S.attribute ~flag:Case_insensitive "type" (Exact "foo bar") in
  let output = Css.Pp.to_string ~minify:true S.pp sel in
  Alcotest.(check string)
    "value with space and i flag" "[type=\"foo bar\" i]" output;
  ()

let test_combinator () =
  (* Test combinator type *)
  check_combinator ">";
  check_combinator "+";
  check_combinator "~";
  check_combinator "||";

  (* Test invalid combinators using neg *)
  neg read_combinator "!";
  neg read_combinator "&";
  neg read_combinator "#";
  neg read_combinator ""

let test_ns () =
  (* Test namespace type *)
  check_ns "svg|";
  check_ns "xml|";
  check_ns "*|";

  (* Test invalid namespace syntax *)
  neg read_ns "|";
  (* Just pipe without namespace *)
  neg read_ns "||";
  neg read_ns "svg";
  (* Missing pipe *)
  neg read_ns "svg||";

  (* Double pipe *)

  (* Test cases that should return None (no namespace found) *)
  none read_ns "notanamespace";
  none read_ns "incomplete";
  none read_ns "";

  (* Test namespaced element selectors *)
  check "svg|rect";
  check "svg|*";
  check "*|div";
  check "*|*";
  check "math|mrow";
  check "xhtml|p";

  (* Namespaced selectors with other combinators *)
  check "svg|g svg|rect";
  check ~expected:"svg|g>svg|path" "svg|g > svg|path";
  check ~expected:"html|div+svg|svg" "html|div + svg|svg";
  check ~expected:"xml|root~xml|child" "xml|root ~ xml|child";

  (* Namespaced selectors with pseudo-classes *)
  check "svg|rect:hover";
  check "xml|element:first-child";
  check "*|*:not(svg|*)";

  (* Namespaced attributes in regular selectors *)
  check "div[xml|lang]";
  check "span[xlink|href]";
  check ~expected:"rect[svg|width=\"100\"]" "rect[svg|width=100]";

  (* Complex namespaced selectors *)
  check ~expected:"svg|g.highlight>svg|rect[fill=red]"
    "svg|g.highlight > svg|rect[fill=red]";
  check "*|div#main[*|attr]";

  (* Edge cases with namespace *)
  (* Note: Explicit no-namespace syntax (|element) may not be supported *)
  (* check "|div"; *)
  (* No namespace (explicitly) *)
  (* check "|*"; *)
  (* No namespace universal selector *)
  ()

let test_nth () =
  (* Test nth type *)
  check_nth "2n+1";
  check_nth "odd";
  check_nth "even";
  check_nth "3n";
  check_nth "5";

  (* Test invalid nth values *)
  neg read_nth "invalid";
  neg read_nth "";
  neg read_nth "2 n";
  neg read_nth "n+";
  ()

let test_selector () =
  (* Test main selector type *)
  check "div";
  check ".class";
  check "#id";
  check "*";
  check ":hover";
  check "[href]";
  check "div.class";
  check ".parent .child";

  (* Test invalid selectors *)
  neg read "123invalid";
  (* Can't start with digit *)
  neg read "";
  (* Empty selector *)
  neg read ".";
  (* Incomplete class *)
  neg read "#";
  (* Incomplete id *)
  neg read "[";
  (* Incomplete attribute *)
  neg read ":";
  (* Incomplete pseudo *)
  neg read "::";
  (* Incomplete pseudo-element *)
  neg read "...invalid";
  (* Multiple dots *)
  ()

(* Test negative cases for unused functions *)
let component_parsing_failures () =
  (* All negative tests are now properly distributed to their respective test_x
     functions *)
  ()

(* Not a roundtrip test *)
let test_complex_construction () =
  (* Universal selector *)
  check_construct "*" universal;

  (* Complex :where with descendants *)
  let s = class_ "prose" ++ where [ element "a" ++ element "strong" ] in
  check_construct ".prose :where(a strong)" s;

  (* Nested :where *)
  let nested = where [ where [ class_ "a" ] ] in
  check_construct ":where(:where(.a))" nested;

  (* Empty list should be invalid per spec *)
  check_invalid "empty list" "CSS selector list cannot be empty" (fun () ->
      ignore (list []))

(* Not a roundtrip test *)
let test_combinator_distribution () =
  (* Child combinator distributes over list *)
  let s = class_ "parent" >> list [ class_ "a"; class_ "b" ] in
  check_construct ".parent>.a,.parent>.b" s;

  (* Descendant distributes *)
  let s = class_ "parent" ++ list [ class_ "x"; class_ "y" ] in
  check_construct ".parent .x,.parent .y" s;

  (* Next sibling distributes *)
  let s =
    combine (class_ "prev") Next_sibling (list [ class_ "a"; class_ "b" ])
  in
  check_construct ".prev+.a,.prev+.b" s;

  (* Subsequent sibling distributes *)
  let s =
    combine (class_ "first") Subsequent_sibling
      (list [ class_ "x"; class_ "y" ])
  in
  check_construct ".first~.x,.first~.y" s

let suite =
  let open Alcotest in
  ( "selector",
    [
      (* Core type tests *)
      test_case "combinator" `Quick test_combinator;
      test_case "ns" `Quick test_ns;
      test_case "nth" `Quick test_nth;
      test_case "selector" `Quick test_selector;
      (* Basic selector types *)
      test_case "element" `Quick element_cases;
      test_case "class" `Quick class_cases;
      test_case "id" `Quick id_cases;
      test_case "pseudo class" `Quick pseudo_class_cases;
      test_case "pseudo element" `Quick pseudo_element_cases;
      test_case "attribute" `Quick attribute_cases;
      (* Combinations *)
      test_case "combinator cases" `Quick combinator_cases;
      test_case "compound" `Quick compound_cases;
      test_case "list" `Quick list_cases;
      test_case "where is" `Quick where_is_cases;
      (* Parsing *)
      test_case "roundtrip" `Quick roundtrip;
      test_case "selector component parsing" `Quick component_parsing;
      test_case "attribute match" `Quick test_attribute_match;
      test_case "attr value quoting" `Quick test_attr_value_quoting;
      test_case "attr flag" `Quick test_attr_flag;
      test_case "attr case sensitivity flags" `Quick
        test_attr_case_sensitivity_flags;
      test_case "selector component failures" `Quick component_parsing_failures;
      (* Error cases *)
      test_case "invalid" `Quick invalid;
      test_case "parse errors - attributes" `Quick parse_errors_attributes;
      test_case "parse errors - combinators" `Quick parse_errors_combinators;
      test_case "parse errors - starts" `Quick parse_errors_starts;
      test_case "parse errors - pseudo" `Quick parse_errors_pseudo;
      test_case "parse errors - empty list" `Quick parse_errors_empty_list;
      test_case "parse errors - complex" `Quick parse_errors_complex;
      test_case "callstack accuracy" `Quick callstack_accuracy;
      (* Special cases *)
      test_case "complex construction" `Quick test_complex_construction;
      test_case "combinator distribution" `Quick test_combinator_distribution;
    ] )
