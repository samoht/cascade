(** Tests for CSS Selector module *)

open Alcotest
open Css.Selector

(* Helper function for testing selectors - parse and print *)
let check_selector ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let selector = read t in
  let output = to_string ~minify:true selector in
  check string (Fmt.str "selector %s" input) expected output

(* Helper for checking invalid selectors *)
let check_invalid name exn_msg f =
  check_raises name (Invalid_argument exn_msg) f

(* Helper for testing selector construction *)
let check_construct name expected selector =
  let actual = to_string ~minify:true selector in
  check string name expected actual

(* Test element selectors *)
let test_element_selectors () =
  check_construct "div" "div" (element "div");
  check_construct "span" "span" (element "span");
  check_construct "h1" "h1" (element "h1");
  check_construct "article" "article" (element "article");
  check_construct "custom-element" "custom-element" (element "custom-element")

(* Test class selectors *)
let test_class_selectors () =
  check_construct "class" ".test" (class_ "test");
  check_construct "class with dash" ".test-class" (class_ "test-class");
  check_construct "class with underscore" ".test_class" (class_ "test_class");
  check_construct "class with number" ".test123" (class_ "test123")

(* Test ID selectors *)
let test_id_selectors () =
  check_construct "id" "#myid" (id "myid");
  check_construct "id with dash" "#my-id" (id "my-id");
  check_construct "id with underscore" "#my_id" (id "my_id");
  check_construct "id with number" "#id123" (id "id123")

(* Test pseudo-class selectors *)
let test_pseudo_classes () =
  check_construct "hover" ":hover" (pseudo_class "hover");
  check_construct "active" ":active" (pseudo_class "active");
  check_construct "focus" ":focus" (pseudo_class "focus");
  check_construct "first-child" ":first-child" (pseudo_class "first-child");
  check_construct "last-child" ":last-child" (pseudo_class "last-child");
  check_construct "nth-child(2)" ":nth-child(2)" (nth_child (An_plus_b (0, 2)));
  check_construct "nth-child(odd)" ":nth-child(odd)" (nth_child Odd);
  check_construct "nth-child(even)" ":nth-child(even)" (nth_child Even);
  check_construct "nth-child(2n+1)" ":nth-child(2n+1)"
    (nth_child (An_plus_b (2, 1)))

(* Test pseudo-element selectors *)
let test_pseudo_elements () =
  check_construct "before" "::before" (pseudo_element "before");
  check_construct "after" "::after" (pseudo_element "after");
  check_construct "first-line" "::first-line" (pseudo_element "first-line");
  check_construct "first-letter" "::first-letter"
    (pseudo_element "first-letter");
  check_construct "marker" "::marker" (pseudo_element "marker")

(* Test attribute selectors *)
let test_attribute_selectors () =
  check_construct "has attribute" "[href]" (attribute "href" Presence);
  check_construct "exact match" "[type=\"text\"]"
    (attribute "type" (Exact "text"));
  check_construct "contains word" "[class~=\"active\"]"
    (attribute "class" (Whitespace_list "active"));
  check_construct "starts with" "[href^=\"https\"]"
    (attribute "href" (Prefix "https"));
  check_construct "ends with" "[href$=\".pdf\"]"
    (attribute "href" (Suffix ".pdf"));
  check_construct "contains substring" "[title*=\"hello\"]"
    (attribute "title" (Substring "hello"));
  check_construct "dash match" "[lang|=\"en\"]"
    (attribute "lang" (Hyphen_list "en"))

(* Test combinators *)
let test_combinators () =
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
let test_compound_selectors () =
  check_construct "element.class" "div.container"
    (element "div" && class_ "container");
  check_construct "element#id" "div#main" (element "div" && id "main");
  check_construct "class.class" ".btn.primary" (class_ "btn" && class_ "primary");
  check_construct "element:hover" "a:hover" (element "a" && pseudo_class "hover");
  check_construct "class[attr]" ".link[href]"
    (class_ "link" && attribute "href" Presence)

(* Test selector lists *)
let test_selector_lists () =
  check_construct "list of classes" ".a,.b,.c"
    (list [ class_ "a"; class_ "b"; class_ "c" ]);
  check_construct "list of elements" "h1,h2,h3"
    (list [ element "h1"; element "h2"; element "h3" ]);
  check_construct "mixed list" "div,.class,#id"
    (list [ element "div"; class_ "class"; id "id" ])

(* Test :where() and :is() *)
let test_where_is () =
  check_construct ":where(div)" ":where(div)" (where [ element "div" ]);
  check_construct ":where(.a,.b)" ":where(.a,.b)"
    (where [ class_ "a"; class_ "b" ]);
  check_construct ":is(h1,h2)" ":is(h1,h2)" (is_ [ element "h1"; element "h2" ]);
  check_construct ":not(.active)" ":not(.active)" (not [ class_ "active" ])

(* Test parsing roundtrips *)
let test_parsing_roundtrips () =
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
  check_selector "[type=\"text\"]";
  check_selector "[class~=\"active\"]";
  check_selector "[href^=\"https\"]";

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
let test_invalid_selectors () =
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
    (fun () -> ignore (id "my#id"))

(* Test special cases *)
let test_special_cases () =
  (* Universal selector *)
  check_construct "universal" "*" universal;

  (* Complex :where with descendants *)
  let s = class_ "prose" ++ where [ element "a" ++ element "strong" ] in
  check_construct "prose :where descendant" ".prose :where(a strong)" s;

  (* Nested :where *)
  let nested = where [ where [ class_ "a" ] ] in
  check_construct "nested where" ":where(:where(.a))" nested;

  (* Empty list edge case - should probably be invalid but test current
     behavior *)
  let empty = list [] in
  check_construct "empty list" "" empty

(* Test distribution semantics *)
let test_distribution () =
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
  [
    ( "selector",
      [
        (* Basic selector types *)
        test_case "element selectors" `Quick test_element_selectors;
        test_case "class selectors" `Quick test_class_selectors;
        test_case "id selectors" `Quick test_id_selectors;
        test_case "pseudo classes" `Quick test_pseudo_classes;
        test_case "pseudo elements" `Quick test_pseudo_elements;
        test_case "attribute selectors" `Quick test_attribute_selectors;
        (* Combinations *)
        test_case "combinators" `Quick test_combinators;
        test_case "compound selectors" `Quick test_compound_selectors;
        test_case "selector lists" `Quick test_selector_lists;
        test_case "where and is" `Quick test_where_is;
        (* Parsing *)
        test_case "parsing roundtrips" `Quick test_parsing_roundtrips;
        (* Error cases *)
        test_case "invalid selectors" `Quick test_invalid_selectors;
        (* Special cases *)
        test_case "special cases" `Quick test_special_cases;
        test_case "distribution" `Quick test_distribution;
      ] );
  ]
