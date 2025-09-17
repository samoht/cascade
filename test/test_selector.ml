(** Tests for CSS Selector module *)

open Css.Selector

(* Generic check function for selector types *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let result = reader t in
  let pp_str = Css.Pp.to_string ~minify:true pp result in
  Alcotest.(check string) (Fmt.str "%s %s" name input) expected pp_str

let check_nth = check_value "nth" pp_nth read_nth
let check_combinator = check_value "combinator" pp_combinator read_combinator
let check = check_value "selector" pp read

let check_ns input =
  let t = Css.Reader.of_string input in
  match read_ns t with
  | Some ns ->
      let output = Css.Pp.to_string ~minify:true pp_ns ns in
      Alcotest.(check string) (Fmt.str "ns %s" input) input output
  | None -> Fmt.failwith "Failed to parse ns: %s" input

(* Helper to check Parse_error fields match *)
let check_parse_error_fields name (expected : Css.Reader.parse_error)
    (actual : Css.Reader.parse_error) =
  if actual.message <> expected.message then
    Alcotest.failf "%s: expected message '%s' but got '%s'" name
      expected.message actual.message
  else if actual.got <> expected.got then
    Alcotest.failf "%s: expected got=%a but got=%a" name
      Fmt.(option string)
      expected.got
      Fmt.(option string)
      actual.got

(* Helper to check that a function raises a specific exception *)
let check_raises name expected_exn f =
  try
    f ();
    Alcotest.failf "%s: expected exception but none was raised" name
  with
  | Css.Reader.Parse_error actual
    when match expected_exn with
         | Css.Reader.Parse_error expected ->
             check_parse_error_fields name expected actual;
             true
         | _ -> false ->
      ()
  | exn when exn = expected_exn ->
      (* For other exceptions, use structural equality *)
      ()
  | exn ->
      Alcotest.failf "%s: expected %s but got %s" name
        (Printexc.to_string expected_exn)
        (Printexc.to_string exn)

(* Helper for checking invalid selectors *)
let check_invalid name exn_msg f =
  check_raises name (Invalid_argument exn_msg) f

(* Helper for testing selector construction *)
let check_construct expected selector =
  let actual = to_string ~minify:true selector in
  Alcotest.(check string) expected expected actual

(* Test element selectors *)
let element_cases () =
  check_construct "div" (element "div");
  check_construct "span" (element "span");
  check_construct "h1" (element "h1");
  check_construct "article" (element "article");
  check_construct "custom-element" (element "custom-element")

(* Test class selectors *)
let class_cases () =
  check_construct ".test" (class_ "test");
  check_construct ".test-class" (class_ "test-class");
  check_construct ".test_class" (class_ "test_class");
  check_construct ".test123" (class_ "test123")

(* Test ID selectors *)
let id_cases () =
  check_construct "#myid" (id "myid");
  check_construct "#my-id" (id "my-id");
  check_construct "#my_id" (id "my_id");
  check_construct "#id123" (id "id123")

(* Test pseudo-class selectors *)
let pseudo_class_cases () =
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
    ":nth-last-of-type(2n+2 of h1 , .a)"

(* Test pseudo-element selectors *)
let pseudo_element_cases () =
  check_construct ":before" Before;
  check_construct ":after" After;
  check_construct ":first-line" First_line;
  check_construct ":first-letter" First_letter;
  check_construct "::marker" Marker;
  ()

(* Test attribute selectors *)
let attribute_cases () =
  check_construct "[href]" (attribute "href" Presence);
  check_construct "[type=text]" (attribute "type" (Exact "text"));
  check_construct "[class~=active]"
    (attribute "class" (Whitespace_list "active"));
  check_construct "[href^=https]" (attribute "href" (Prefix "https"));
  check_construct "[href$=\".pdf\"]" (attribute "href" (Suffix ".pdf"));
  check_construct "[title*=hello]" (attribute "title" (Substring "hello"));
  check_construct "[lang|=en]" (attribute "lang" (Hyphen_list "en"))

(* Test combinators *)
let combinator_cases () =
  check_construct ".parent .child" (class_ "parent" ++ class_ "child");
  check_construct ".parent>.child" (class_ "parent" >> class_ "child");
  check_construct ".prev+.next"
    (combine (class_ "prev") Next_sibling (class_ "next"));
  check_construct ".first~.later"
    (combine (class_ "first") Subsequent_sibling (class_ "later"));
  check_construct ".col1||.col2"
    (combine (class_ "col1") Column (class_ "col2"))

(* Test compound selectors *)
let compound_cases () =
  check_construct "div.container" (element "div" && class_ "container");
  check_construct "div#main" (element "div" && id "main");
  check_construct ".btn.primary" (class_ "btn" && class_ "primary");
  check_construct "a:hover" (element "a" && Hover);
  check_construct ".link[href]" (class_ "link" && attribute "href" Presence)

(* Test selector lists *)
let list_cases () =
  check_construct ".a,.b,.c" (list [ class_ "a"; class_ "b"; class_ "c" ]);
  check_construct "h1,h2,h3" (list [ element "h1"; element "h2"; element "h3" ]);
  check_construct "div,.class,#id"
    (list [ element "div"; class_ "class"; id "id" ])

(* Test :where() and :is() *)
let where_is_cases () =
  check_construct ":where(div)" (where [ element "div" ]);
  check_construct ":where(.a,.b)" (where [ class_ "a"; class_ "b" ]);
  check_construct ":is(h1,h2)" (is_ [ element "h1"; element "h2" ]);
  check_construct ":not(.active)" (not [ class_ "active" ])

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

  (* Test that escaped characters now give proper Parse_error instead of
     Invalid_argument *)
  check_full_css_callstack "escaped_character_error"
    ".test\\!class { color: red; }"
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
    check_value "attribute_match" pp_attribute_match read_attribute_match
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
  check_attribute_match "*=substring"

let test_attr_flag () =
  (* Test attribute selector flags - returns option type *)
  let check_attr_flag = check_value "attr_flag" pp_attr_flag read_attr_flag in

  (* Case insensitive flag *)
  check_attr_flag ~expected:" i" "i";
  (* Case sensitive flag *)
  check_attr_flag ~expected:" s" "s";
  (* No flag / empty should return None *)
  check_attr_flag ""

(* Test negative cases for unused functions *)
let component_parsing_failures () =
  let open Css.Reader in
  let neg reader s =
    let r = of_string s in
    let result = option reader r in
    Alcotest.(check bool) ("should reject: " ^ s) true (Option.is_none result)
  in

  (* Invalid nth values *)
  neg read_nth "invalid";
  neg read_nth "";
  neg read_nth "2 n";

  (* Invalid combinators *)
  neg read_combinator "!";
  neg read_combinator "";

  (* Invalid namespace syntax *)
  let test_invalid_ns () =
    let r = of_string "notanamespace" in
    let result = read_ns r in
    Alcotest.(check bool)
      "invalid namespace should fail" true (Option.is_none result)
  in
  test_invalid_ns ()

(* Test special cases *)
let special_cases () =
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

(* Test distribution semantics *)
let distribution () =
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
      (* Basic selector types *)
      test_case "element" `Quick element_cases;
      test_case "class" `Quick class_cases;
      test_case "id" `Quick id_cases;
      test_case "pseudo class" `Quick pseudo_class_cases;
      test_case "pseudo element" `Quick pseudo_element_cases;
      test_case "attribute" `Quick attribute_cases;
      (* Combinations *)
      test_case "combinator" `Quick combinator_cases;
      test_case "compound" `Quick compound_cases;
      test_case "list" `Quick list_cases;
      test_case "where is" `Quick where_is_cases;
      (* Parsing *)
      test_case "roundtrip" `Quick roundtrip;
      test_case "selector component parsing" `Quick component_parsing;
      test_case "attribute match" `Quick test_attribute_match;
      test_case "attr flag" `Quick test_attr_flag;
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
      test_case "special cases" `Quick special_cases;
      test_case "distribution" `Quick distribution;
    ] )
