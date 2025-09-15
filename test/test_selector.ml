(** Tests for CSS Selector module *)

open Alcotest
open Css.Selector

(* Generic check function for selectors - handles parse/print roundtrip
   testing *)
let check_selector ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let selector = read t in
  let output = to_string ~minify:true selector in
  check string (Fmt.str "selector %s" input) expected output

(* Generic check function for selector types *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let result = reader t in
  let pp_str = Css.Pp.to_string ~minify:true pp result in
  check string (Fmt.str "%s %s" name input) expected pp_str

let check_nth = check_value "nth" pp_nth read_nth
let check_combinator = check_value "combinator" pp_combinator read_combinator

let check_ns input =
  let t = Css.Reader.of_string input in
  match read_ns t with
  | Some ns ->
      let output = Css.Pp.to_string ~minify:true pp_ns ns in
      Alcotest.(check string) (Fmt.str "ns %s" input) input output
  | None -> Fmt.failwith "Failed to parse ns: %s" input

let check_attribute_match =
  check_value "attribute_match" pp_attribute_match read_attribute_match

let check_attr_flag = check_value "attr_flag" pp_attr_flag read_attr_flag

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
let check_construct name expected selector =
  let actual = to_string ~minify:true selector in
  Alcotest.(check string) name expected actual

(* Test element selectors *)
let element_cases () =
  check_construct "div" "div" (element "div");
  check_construct "span" "span" (element "span");
  check_construct "h1" "h1" (element "h1");
  check_construct "article" "article" (element "article");
  check_construct "custom-element" "custom-element" (element "custom-element")

(* Test class selectors *)
let class_cases () =
  check_construct "class" ".test" (class_ "test");
  check_construct "class with dash" ".test-class" (class_ "test-class");
  check_construct "class with underscore" ".test_class" (class_ "test_class");
  check_construct "class with number" ".test123" (class_ "test123")

(* Test ID selectors *)
let id_cases () =
  check_construct "id" "#myid" (id "myid");
  check_construct "id with dash" "#my-id" (id "my-id");
  check_construct "id with underscore" "#my_id" (id "my_id");
  check_construct "id with number" "#id123" (id "id123")

(* Test pseudo-class selectors *)
let pseudo_class_cases () =
  check_construct "hover" ":hover" (pseudo_class "hover");
  check_construct "active" ":active" (pseudo_class "active");
  check_construct "focus" ":focus" (pseudo_class "focus");
  check_construct "first-child" ":first-child" (pseudo_class "first-child");
  check_construct "last-child" ":last-child" (pseudo_class "last-child");
  check_construct "nth-child(2)" ":nth-child(2)" (nth_child (An_plus_b (0, 2)));
  check_construct "nth-child(odd)" ":nth-child(odd)" (nth_child Odd);
  check_construct "nth-child(even)" ":nth-child(even)" (nth_child Even);
  check_construct "nth-child(2n+1)" ":nth-child(2n+1)"
    (nth_child (An_plus_b (2, 1)));
  (* nth with Index and of clause *)
  check_selector ":nth-child(5)";
  check_selector ~expected:":nth-child(odd of .item)"
    ":nth-child( odd of .item )";
  check_selector ~expected:":nth-child(2n-1 of a,b)"
    ":nth-child( 2n-1 of a , b )";
  check_selector ":nth-of-type(3)";
  check_selector ~expected:":nth-last-child(2 of .x,.y)"
    ":nth-last-child(2 of .x , .y)";
  check_selector ~expected:":nth-last-of-type(2n+2 of h1,.a)"
    ":nth-last-of-type(2n+2 of h1 , .a)"

(* Test pseudo-element selectors *)
let pseudo_element_cases () =
  check_construct "before" "::before" (pseudo_element "before");
  check_construct "after" "::after" (pseudo_element "after");
  check_construct "first-line" "::first-line" (pseudo_element "first-line");
  check_construct "first-letter" "::first-letter"
    (pseudo_element "first-letter");
  check_construct "marker" "::marker" (pseudo_element "marker");
  ()

(* Test attribute selectors *)
let attribute_cases () =
  check_construct "has attribute" "[href]" (attribute "href" Presence);
  check_construct "exact match" "[type=text]" (attribute "type" (Exact "text"));
  check_construct "contains word" "[class~=active]"
    (attribute "class" (Whitespace_list "active"));
  check_construct "starts with" "[href^=https]"
    (attribute "href" (Prefix "https"));
  check_construct "ends with" "[href$=\".pdf\"]"
    (attribute "href" (Suffix ".pdf"));
  check_construct "contains substring" "[title*=hello]"
    (attribute "title" (Substring "hello"));
  check_construct "dash match" "[lang|=en]"
    (attribute "lang" (Hyphen_list "en"))

(* Test combinators *)
let combinator_cases () =
  check_construct "descendant" ".parent .child"
    (class_ "parent" ++ class_ "child");
  check_construct "child" ".parent>.child" (class_ "parent" >> class_ "child");
  check_construct "next sibling" ".prev+.next"
    (combine (class_ "prev") Next_sibling (class_ "next"));
  check_construct "subsequent sibling" ".first~.later"
    (combine (class_ "first") Subsequent_sibling (class_ "later"));
  check_construct "column" ".col1||.col2"
    (combine (class_ "col1") Column (class_ "col2"))

(* Test compound selectors *)
let compound_cases () =
  check_construct "element.class" "div.container"
    (element "div" && class_ "container");
  check_construct "element#id" "div#main" (element "div" && id "main");
  check_construct "class.class" ".btn.primary" (class_ "btn" && class_ "primary");
  check_construct "element:hover" "a:hover" (element "a" && pseudo_class "hover");
  check_construct "class[attr]" ".link[href]"
    (class_ "link" && attribute "href" Presence)

(* Test selector lists *)
let list_cases () =
  check_construct "list of classes" ".a,.b,.c"
    (list [ class_ "a"; class_ "b"; class_ "c" ]);
  check_construct "list of elements" "h1,h2,h3"
    (list [ element "h1"; element "h2"; element "h3" ]);
  check_construct "mixed list" "div,.class,#id"
    (list [ element "div"; class_ "class"; id "id" ])

(* Test :where() and :is() *)
let where_is_cases () =
  check_construct ":where(div)" ":where(div)" (where [ element "div" ]);
  check_construct ":where(.a,.b)" ":where(.a,.b)"
    (where [ class_ "a"; class_ "b" ]);
  check_construct ":is(h1,h2)" ":is(h1,h2)" (is_ [ element "h1"; element "h2" ]);
  check_construct ":not(.active)" ":not(.active)" (not [ class_ "active" ])

(* Test parsing roundtrips *)
let roundtrip () =
  (* Basic selectors *)
  check_selector "div";
  check_selector ".class";
  check_selector "#id";
  check_selector "*";

  (* Pseudo-classes *)
  check_selector ":hover";
  check_selector ":nth-child(2)";
  check_selector ":nth-child(2n+1)";
  check_selector ":nth-child(odd)";
  check_selector ":nth-child(even)";

  (* Pseudo-elements *)
  check_selector "::before";
  check_selector "::after";
  check_selector "::part(foo)";
  check_selector "::slotted(.class)";

  (* Attributes *)
  check_selector "[href]";
  check_selector ~expected:"[type=text]" "[type=\"text\"]";
  check_selector ~expected:"[class~=active]" "[class~=\"active\"]";
  check_selector ~expected:"[href^=https]" "[href^=\"https\"]";

  (* Combinators *)
  check_selector ".parent .child";
  check_selector ~expected:".parent>.child" ".parent > .child";
  check_selector ~expected:".prev+.next" ".prev + .next";
  check_selector ~expected:".first~.later" ".first ~ .later";

  (* Complex selectors *)
  check_selector "div.class#id[href]:hover::after";
  check_selector ~expected:".a,.b,.c" ".a, .b, .c";
  check_selector ":where(.a,.b)";
  check_selector ":is(h1,h2,h3)";
  check_selector ":not(.active)"

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
let parse_errors () =
  let check_parse_error name input expected_msg =
    let t = Css.Reader.of_string input in
    try
      let _ = read t in
      Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
    with
    | Css.Reader.Parse_error err ->
        let contains_substring haystack needle =
          let len_h = String.length haystack in
          let len_n = String.length needle in
          let rec search i =
            if i > len_h - len_n then false
            else if String.sub haystack i len_n = needle then true
            else search (i + 1)
          in
          if len_n = 0 then true else search 0
        in
        if Bool.not @@ contains_substring err.message expected_msg then
          Alcotest.failf "%s: expected message containing '%s' but got '%s'"
            name expected_msg err.message
    | exn ->
        Alcotest.failf "%s: expected Parse_error but got %s" name
          (Printexc.to_string exn)
  in

  (* Unclosed attribute selectors - check for common error patterns *)
  check_parse_error "unclosed_attr" "[class=\"test\"" "Expected";
  check_parse_error "missing_bracket" ".test[data-id=\"123\"" "Expected";

  (* Empty attribute selector *)
  check_parse_error "empty_attr" ".test[]" "expected identifier";

  (* Invalid combinators *)
  check_parse_error "invalid_combinator" ".test >> .child"
    "expected at least one selector";
  check_parse_error "multiple_combinators" ".parent + + .child"
    "expected at least one selector";
  check_parse_error "missing_after_combinator" ".parent >"
    "expected at least one selector";
  check_parse_error "just_combinator" ">" "expected at least one selector";

  (* Invalid selector starts *)
  check_parse_error "invalid_class_start" ".123test" "expected identifier";
  check_parse_error "invalid_id_start" "#123" "expected identifier";

  (* Nested brackets *)
  check_parse_error "nested_brackets" ".test[[attr]]" "expected identifier";

  (* Space in attribute name *)
  check_parse_error "space_in_attr" ".test[data id=\"value\"]" "Expected ']'";

  (* Unquoted spaces in attribute value *)
  check_parse_error "unquoted_spaces" ".test[data-id=value with spaces]"
    "Expected ']'";

  (* Invalid pseudo-function calls *)
  check_parse_error "invalid_not" ".test:not()" "expected at least one selector";
  check_parse_error "unclosed_not" ".test:not(.other" "unexpected end of input";
  check_parse_error "invalid_is" ":is()" "expected at least one selector";
  check_parse_error "invalid_has" ".test:has()" "expected at least one selector";

  (* Mixed up combinators *)
  check_parse_error "mixed_combinators" ".parent ~> .child"
    "expected at least one selector";

  (* Empty selector list *)
  check_parse_error "empty_list" ", ," "expected at least one selector";
  check_parse_error "leading_comma" ", h1, h2" "expected at least one selector";
  check_parse_error "trailing_comma" "h1, h2," "expected at least one selector";

  (* Invalid universal selector combinations *)
  check_parse_error "invalid_universal" "*.*" "expected identifier";

  (* Complex broken selectors *)
  check_parse_error "complex_broken" ".parent > [data-id=\"test\" .child:hover"
    "Expected ']'";
  check_parse_error "long_chain_broken" "body > main > section[data-tooltip"
    "expected one of";
  check_parse_error "multi_attr_broken" ".test[attr1=\"val1\"][attr4$="
    "expected one of";
  check_parse_error "sibling_chain_broken" ".first ~ .second ~ [invalid"
    "expected one of"

(* Test callstack accuracy for selector errors *)
let callstack_accuracy () =
  let contains_substring haystack needle =
    let len_h = String.length haystack in
    let len_n = String.length needle in
    let rec search i =
      if i > len_h - len_n then false
      else if String.sub haystack i len_n = needle then true
      else search (i + 1)
    in
    if len_n = 0 then true else search 0
  in
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
              Alcotest.failf
                "%s: expected callstack containing '%s' but got '%s'" name
                stack_item callstack_str)
          expected_stack_parts;
        (* Also verify position and context window are set *)
        if err.position < 0 then
          Alcotest.failf "%s: position should be >= 0 but got %d" name
            err.position;
        if String.length err.context_window = 0 then
          Alcotest.failf "%s: context_window should not be empty" name
    | exn ->
        Alcotest.failf "%s: expected Parse_error but got %s" name
          (Printexc.to_string exn)
  in

  (* When parsing selectors directly (not through full CSS), callstack is
     shallower *)
  check_callstack "selector_list_error" ".test[[attr]]" [ "list" ];
  check_callstack "combinator_error" ".parent + +" [ "list" ];
  check_callstack "empty_selector" ", ," [ "list" ];

  (* These fail at even higher level when parsing selectors directly *)
  check_callstack "pseudo_function_error" ".test:not()" [];
  (* No specific context when selector fails early *)
  check_callstack "invalid_pseudo" ":is()" [];

  (* Test full CSS parsing to get complete callstack like cascade binary *)
  let check_full_css_callstack name css_input expected_stack_parts =
    match Css.of_string css_input with
    | Ok _ ->
        Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
    | Error err ->
        let callstack_str = String.concat " -> " err.callstack in
        List.iter
          (fun stack_item ->
            if Bool.not @@ contains_substring callstack_str stack_item then
              Alcotest.failf
                "%s: expected callstack containing '%s' but got '%s'" name
                stack_item callstack_str)
          expected_stack_parts
  in

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
let test_selector_component_parsing () =
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
  check_ns "svg";
  check_ns "xml";
  check_ns "foo";

  (* Test attribute matching *)
  check_attribute_match "=";
  check_attribute_match "~=";
  check_attribute_match "^=";
  check_attribute_match "$=";
  check_attribute_match "*=";
  check_attribute_match "|=";

  (* Test attribute flags *)
  check_attr_flag "i";
  check_attr_flag "s";

  (* Test basic selector parsing *)
  check_selector "div";
  check_selector ".class";
  check_selector "#id";
  check_selector "*"

(* Test negative cases for unused functions *)
let test_selector_component_parsing_failures () =
  let open Css.Reader in
  let neg name reader s =
    let r = of_string s in
    let result = option reader r in
    Alcotest.(check bool) (name ^ " should fail") true (Option.is_none result)
  in

  (* Invalid nth values *)
  neg "invalid nth" read_nth "invalid";
  neg "empty nth" read_nth "";
  neg "nth with space" read_nth "2 n";

  (* Invalid combinators *)
  neg "invalid combinator" read_combinator "!";
  neg "empty combinator" read_combinator "";

  (* Invalid attribute match *)
  neg "invalid attr match" read_attribute_match "!";
  neg "empty attr match" read_attribute_match "";

  (* Invalid attribute flags *)
  neg "invalid flag" read_attr_flag "z";
  neg "empty flag" read_attr_flag ""

(* Test special cases *)
let special_cases () =
  (* Universal selector *)
  check_construct "universal" "*" universal;

  (* Complex :where with descendants *)
  let s = class_ "prose" ++ where [ element "a" ++ element "strong" ] in
  check_construct "prose :where descendant" ".prose :where(a strong)" s;

  (* Nested :where *)
  let nested = where [ where [ class_ "a" ] ] in
  check_construct "nested where" ":where(:where(.a))" nested;

  (* Empty list should be invalid per spec *)
  check_invalid "empty list" "CSS selector list cannot be empty" (fun () ->
      ignore (list []))

(* Test distribution semantics *)
let distribution () =
  (* Child combinator distributes over list *)
  let s = class_ "parent" >> list [ class_ "a"; class_ "b" ] in
  check_construct "child distribution" ".parent>.a,.parent>.b" s;

  (* Descendant distributes *)
  let s = class_ "parent" ++ list [ class_ "x"; class_ "y" ] in
  check_construct "descendant distribution" ".parent .x,.parent .y" s;

  (* Next sibling distributes *)
  let s =
    combine (class_ "prev") Next_sibling (list [ class_ "a"; class_ "b" ])
  in
  check_construct "next sibling distribution" ".prev+.a,.prev+.b" s;

  (* Subsequent sibling distributes *)
  let s =
    combine (class_ "first") Subsequent_sibling
      (list [ class_ "x"; class_ "y" ])
  in
  check_construct "subsequent sibling distribution" ".first~.x,.first~.y" s

let suite =
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
      test_case "selector component parsing" `Quick
        test_selector_component_parsing;
      test_case "selector component failures" `Quick
        test_selector_component_parsing_failures;
      (* Error cases *)
      test_case "invalid" `Quick invalid;
      test_case "parse errors" `Quick parse_errors;
      test_case "callstack accuracy" `Quick callstack_accuracy;
      (* Special cases *)
      test_case "special cases" `Quick special_cases;
      test_case "distribution" `Quick distribution;
    ] )
