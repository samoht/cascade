(** Tests for CSS Selector module *)

open Alcotest
open Css.Selector

let pp_selector ppf s = Fmt.string ppf (to_string s)
let selector = testable pp_selector ( = )

let check_selector ?(minify = false) desc expected s =
  check string desc expected (Css.Pp.to_string ~minify pp s)

let check_roundtrip ?(minify = false) ?expected input =
  let expected = Option.value ~default:input expected in
  let t = Css.Reader.of_string input in
  let result = read t in
  let pp_str = Css.Pp.to_string ~minify pp result in
  check string (Fmt.str "roundtrip %s" input) expected pp_str

(* (constructor-only tests removed; replaced with round-trip parsing tests
   below) *)

let test_parsing_pseudo_element_functions () =
  (* Prefer round-trip expectations; parser will be updated if needed. *)
  check_roundtrip "::part(foo,bar)";
  check_roundtrip ~minify:true "::part(foo, bar)" ~expected:"::part(foo,bar)";
  check_roundtrip "::slotted(.a, .b)";
  check_roundtrip ~minify:true "::slotted(.a, .b)" ~expected:"::slotted(.a,.b)";
  check_roundtrip "::cue(p)";
  check_roundtrip "::cue-region(span)"

(* Combinator distribution over lists *)
let test_distribution_rhs_child () =
  let base = class_ "base" in
  let rhs = list [ class_ "a"; class_ "b" ] in
  let s = combine base Child rhs in
  check_selector "child distributes over RHS list" ".base > .a, .base > .b" s;
  check_selector ~minify:true "child distributes (minified)" ".base>.a,.base>.b"
    s

let test_distribution_rhs_descendant () =
  let base = class_ "base" in
  let rhs = list [ class_ "x"; class_ "y" ] in
  let s = base ++ rhs in
  check_selector "descendant distributes over RHS list" ".base .x, .base .y" s;
  check_selector ~minify:true "descendant distributes (minified)"
    ".base .x,.base .y" s

let test_distribution_rhs_siblings () =
  let base = class_ "a" in
  let rhs = list [ class_ "b"; class_ "c" ] in
  let next = combine base Next_sibling rhs in
  check_selector "next-sibling distributes over RHS list" ".a + .b, .a + .c"
    next;
  check_selector ~minify:true "next-sibling distributes (minified)"
    ".a+.b,.a+.c" next;
  let subs = combine base Subsequent_sibling rhs in
  check_selector "subsequent-sibling distributes over RHS list"
    ".a ~ .b, .a ~ .c" subs;
  check_selector ~minify:true "subsequent-sibling distributes (minified)"
    ".a~.b,.a~.c" subs

let test_distribution_rhs_column () =
  let base = class_ "left" in
  let rhs = list [ class_ "r1"; class_ "r2" ] in
  let s = combine base Column rhs in
  check_selector "column distributes over RHS list" ".left || .r1, .left || .r2"
    s;
  check_selector ~minify:true "column distributes (minified)"
    ".left||.r1,.left||.r2" s

let test_validation () =
  (* Test invalid identifiers *)
  check_raises "empty class name"
    (Invalid_argument "CSS identifier '' cannot be empty") (fun () ->
      ignore (class_ ""));
  check_raises "digit start"
    (Invalid_argument "CSS identifier '9foo' cannot start with digit")
    (fun () -> ignore (element "9foo"));
  check_raises "double dash start"
    (Invalid_argument
       "CSS identifier '--var' cannot start with '--' (reserved for custom \
        properties)") (fun () -> ignore (class_ "--var"));
  check_raises "dash digit start"
    (Invalid_argument
       "CSS identifier '-9' cannot start with '-' followed by digit") (fun () ->
      ignore (id "-9"));
  (* Additional invalid ASCII cases *)
  check_raises "element with space"
    (Invalid_argument
       "CSS identifier 'invalid space' contains invalid character ' ' at \
        position 7") (fun () -> ignore (element "invalid space"));
  check_raises "class with dot"
    (Invalid_argument
       "CSS identifier 'test.class' contains invalid character '.' at position \
        4") (fun () -> ignore (class_ "test.class"));
  check_raises "id with hash"
    (Invalid_argument
       "CSS identifier 'test#id' contains invalid character '#' at position 4")
    (fun () -> ignore (id "test#id"))

(* Round-trip parsing tests *)
let test_parsing_elements () =
  check_roundtrip "div";
  check_roundtrip "p";
  check_roundtrip "button";
  check_roundtrip "article";
  check_roundtrip "section"

let test_parsing_classes () =
  check_roundtrip ".foo";
  check_roundtrip ".bg-blue-500";
  check_roundtrip ".hover\\:text-red";
  check_roundtrip ".sm\\:p-4";
  check_roundtrip ".with-dash"

let test_parsing_ids () =
  check_roundtrip "#header";
  check_roundtrip "#main-content";
  check_roundtrip "#footer";
  check_roundtrip "#nav-bar"

let test_parsing_universal () = check_roundtrip "*"

let test_parsing_attributes () =
  check_roundtrip "[data-test]";
  check_roundtrip "[type=\"button\"]";
  check_roundtrip ~minify:true "[type=\"button\"]" ~expected:"[type=button]";
  check_roundtrip "[type=button]" ~expected:"[type=\"button\"]";
  check_roundtrip "[class~=\"foo\"]";
  check_roundtrip ~minify:true "[class~=\"foo\"]" ~expected:"[class~=foo]";
  check_roundtrip "[lang|=\"en\"]";
  check_roundtrip ~minify:true "[lang|=\"en\"]" ~expected:"[lang|=en]";
  check_roundtrip "[href^=\"https\"]";
  check_roundtrip ~minify:true "[href^=\"https\"]" ~expected:"[href^=https]";
  check_roundtrip "[href$=\".pdf\"]";
  check_roundtrip ~minify:true "[href$=\".pdf\"]" ~expected:"[href$=\".pdf\"]";
  check_roundtrip "[title*=\"important\"]";
  check_roundtrip ~minify:true "[title*=\"important\"]"
    ~expected:"[title*=important]"

let test_parsing_pseudo () =
  check_roundtrip ":hover";
  check_roundtrip ":active";
  check_roundtrip ":first-child";
  check_roundtrip ":nth-child(2n+1)";
  check_roundtrip "::before";
  check_roundtrip "::after";
  check_roundtrip "::marker"

let test_parsing_functional () =
  (* Roundtrip functional pseudo-classes *)
  check_roundtrip ":is(button, input)";
  check_roundtrip ~minify:true ":is(button, input)"
    ~expected:":is(button,input)";
  check_roundtrip ":is(div.active, span.highlight)";
  check_roundtrip ~minify:true ":is(div.active, span.highlight)"
    ~expected:":is(div.active,span.highlight)";
  check_roundtrip ":has(.active)";
  check_roundtrip ":has(.a, .b)";
  check_roundtrip ~minify:true ":has(.a, .b)" ~expected:":has(.a,.b)";
  check_roundtrip ":not(.excluded)";
  check_roundtrip ":not(.a, .b)";
  check_roundtrip ~minify:true ":not(.a, .b)" ~expected:":not(.a,.b)";
  check_roundtrip ":where(.a, .b)";
  check_roundtrip ~minify:true ":where(.a, .b)" ~expected:":where(.a,.b)";
  check_roundtrip "select:is([multiple], [size])";
  check_roundtrip ~minify:true "select:is([multiple], [size])"
    ~expected:"select:is([multiple],[size])";
  (* descendant stays spaced even when minified *)
  check_roundtrip ~minify:true "select :is([multiple], [size])"
    ~expected:"select :is([multiple],[size])";
  (* Nested combinators inside :is minify correctly *)
  check_roundtrip ~minify:true ":is(.a > .b, .c + .d)"
    ~expected:":is(.a>.b,.c+.d)"

let test_parsing_lists () =
  (* Pretty output (spaces after comma) and minified variant *)
  check_roundtrip ".a, .b, .c" ~expected:".a, .b, .c";
  check_roundtrip ~minify:true ".a, .b, .c" ~expected:".a,.b,.c";
  check_roundtrip "h1,h2" ~expected:"h1, h2";
  check_roundtrip ~minify:true "h1, h2" ~expected:"h1,h2"

let test_parsing_compound () =
  check_roundtrip "div.container";
  check_roundtrip "div#main";
  check_roundtrip ".foo.bar.baz";
  check_roundtrip "input[type=\"text\"]";
  check_roundtrip "a:hover";
  check_roundtrip "*#id.class";
  check_roundtrip "button:hover:active"

let test_parsing_combinators () =
  check_roundtrip "div p";
  check_roundtrip ~minify:true "div p" ~expected:"div p";
  check_roundtrip "div > p" ~expected:"div > p";
  check_roundtrip "div>p" ~expected:"div > p";
  check_roundtrip ~minify:true "div > p" ~expected:"div>p";
  check_roundtrip "h1 + p" ~expected:"h1 + p";
  check_roundtrip "h1+p" ~expected:"h1 + p";
  check_roundtrip ~minify:true "h1 + p" ~expected:"h1+p";
  check_roundtrip "h1 ~ p" ~expected:"h1 ~ p";
  check_roundtrip "h1~p" ~expected:"h1 ~ p";
  check_roundtrip ~minify:true "h1 ~ p" ~expected:"h1~p";
  check_roundtrip ".a || .b" ~expected:".a || .b";
  check_roundtrip ".a||.b" ~expected:".a || .b";
  check_roundtrip ~minify:true ".a || .b" ~expected:".a||.b"

let test_parsing_nth () =
  check_roundtrip ":nth-child(even)";
  check_roundtrip ":nth-child(odd)";
  check_roundtrip ":nth-child(2n+1)";
  check_roundtrip ":nth-last-child(3 of .item, div)";
  check_roundtrip ":nth-of-type(-n+3)"

let test_parsing_complex () =
  check_roundtrip ".container > div.item + p"
    ~expected:".container > div.item + p";
  check_roundtrip ~minify:true ".container > div.item + p"
    ~expected:".container>div.item+p";
  check_roundtrip ".container>div.item+p" ~expected:".container > div.item + p";
  check_roundtrip ".a:hover > div + p ~ span::before"
    ~expected:".a:hover > div + p ~ span::before";
  check_roundtrip ~minify:true ".a:hover > div + p ~ span::before"
    ~expected:".a:hover>div+p~span::before";
  check_roundtrip "body > div > section > article > p"
    ~expected:"body > div > section > article > p"

let test_parsing_edge_cases () =
  (* Test whitespace handling *)
  check_roundtrip "  div  " ~expected:"div";
  check_roundtrip " .class " ~expected:".class";
  check_roundtrip " div > p " ~expected:"div > p";
  (* Test quotes in attributes *)
  check_roundtrip "[title=\"hello world\"]";
  check_roundtrip "[data-value='single']" ~expected:"[data-value=\"single\"]"

let test_parsing_namespaces_and_flags () =
  check_roundtrip "svg|rect";
  check_roundtrip "*|*";
  check_roundtrip "svg|*";
  check_roundtrip "[svg|viewBox]";
  check_roundtrip "[type=\"text\" i]";
  check_roundtrip ~minify:true "[type=\"text\" i]" ~expected:"[type=text i]";
  check_roundtrip "[data=\"X\" s]";
  check_roundtrip ~minify:true "[data=\"X\" s]" ~expected:"[data=X s]"

let test_read_opt () =
  let t = Css.Reader.of_string ".valid-class" in
  (match read_opt t with
  | Some _ -> check bool "parsed successfully" true true
  | None -> fail "Expected selector");
  let t = Css.Reader.of_string "" in
  match read_opt t with
  | Some _ -> fail "Should not parse empty string"
  | None -> check bool "returned None as expected" true true

let suite =
  [
    ( "selector",
      [
        (* Round-trip parsing tests *)
        test_case "parsing_elements" `Quick test_parsing_elements;
        test_case "parsing_classes" `Quick test_parsing_classes;
        test_case "parsing_ids" `Quick test_parsing_ids;
        test_case "parsing_universal" `Quick test_parsing_universal;
        test_case "parsing_attributes" `Quick test_parsing_attributes;
        test_case "parsing_pseudo" `Quick test_parsing_pseudo;
        test_case "parsing_functional" `Quick test_parsing_functional;
        test_case "parsing_lists" `Quick test_parsing_lists;
        test_case "parsing_compound" `Quick test_parsing_compound;
        test_case "parsing_combinators" `Quick test_parsing_combinators;
        test_case "parsing_nth" `Quick test_parsing_nth;
        test_case "parsing_complex" `Quick test_parsing_complex;
        test_case "parsing_edge_cases" `Quick test_parsing_edge_cases;
        test_case "parsing_namespaces_and_flags" `Quick
          test_parsing_namespaces_and_flags;
        test_case "parsing_pseudo_element_functions" `Quick
          test_parsing_pseudo_element_functions;
        test_case "read_opt" `Quick test_read_opt;
        (* Validation *)
        test_case "validation" `Quick test_validation;
        (* Distribution semantics *)
        test_case "distribution_rhs_child" `Quick test_distribution_rhs_child;
        test_case "distribution_rhs_descendant" `Quick
          test_distribution_rhs_descendant;
        test_case "distribution_rhs_siblings" `Quick
          test_distribution_rhs_siblings;
        test_case "distribution_rhs_column" `Quick test_distribution_rhs_column;
      ] );
  ]
