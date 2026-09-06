(** Tests for CSS Selector module *)

open Cascade
open Css.Selector
open Css_test_helpers

let check_nth = check_value_cursor "nth" read_nth pp_nth

let check_combinator =
  check_value_cursor "combinator" read_combinator pp_combinator

let check = check_value_cursor "selector" read pp

let check_component_values =
  check_value_cursor "component_values" read_component_values
    pp_component_values

let check_ns = check_value_cursor "ns" read_ns (Css.Pp.option pp_ns)
let check_aria_attr = check_value_cursor "aria_attr" read_aria_attr pp_aria_attr
let check_attr_name = check_value_cursor "attr_name" read_attr_name pp_attr_name

(* Helper for checking invalid selectors *)
let check_invalid name exn_msg f =
  check_raises name (Invalid_argument exn_msg) f

let check_construct expected selector =
  check_construct expected (to_string ~minify:true) expected selector

let check_pretty_to expected input =
  let actual = to_string ~minify:false (of_string input) in
  Alcotest.(check string) ("pretty " ^ input) expected actual

let check_spec_tuple name expected actual =
  Alcotest.(check int) (name ^ " ids") expected.ids actual.ids;
  Alcotest.(check int) (name ^ " classes") expected.classes actual.classes;
  Alcotest.(check int) (name ^ " elements") expected.elements actual.elements

(* Extra minifier invariant: reparse and preserve specificity. *)
let check_minified_to expected input =
  let original = of_string input in
  let minified = to_string ~minify:true original in
  Alcotest.(check string) ("minify " ^ input) expected minified;
  let reparsed = of_string minified in
  let expected_ast = of_string expected in
  Alcotest.(check bool)
    ("minified selector reparses to expected AST: " ^ input)
    true
    (equal reparsed expected_ast);
  check_spec_tuple
    ("specificity preserved: " ^ input)
    (specificity original) (specificity reparsed)

(* Minified serialisation under [--enforce-spec], where no fact about the
   rendering browser or the host document language is available: a rewrite runs
   only when the CSS text and the CSS specs prove it on their own. *)
let check_enforce_spec_to expected input =
  let actual =
    Css.Pp.to_string ~minify:true ~enforce_spec:true pp (of_string input)
  in
  Alcotest.(check string) ("enforce-spec " ^ input) expected actual

(* Extra minifier invariant: minify is idempotent (a second pass produces no
   further change), and specificity is preserved. Strict AST equality between
   [original] and [reparsed] does not hold for inputs the minifier rewrites
   (e.g. CSS Selectors 4 sec. 5.2 lets the printer drop a redundant [*] from a
   compound), so we compare the minified form against itself after re-parsing
   and re-minifying. *)
let check_minified_equiv input =
  let original = of_string input in
  let minified = to_string ~minify:true original in
  let reparsed = of_string minified in
  let reminified = to_string ~minify:true reparsed in
  Alcotest.(check string)
    ("minified selector is idempotent: " ^ input)
    minified reminified;
  check_spec_tuple
    ("specificity preserved: " ^ input)
    (specificity original) (specificity reparsed)

let of_string_empty_raises_parse_error () =
  match of_string "" with
  | _ -> Alcotest.fail "empty selector parsed"
  | exception Error.Parse_error _ -> ()
  | exception exn ->
      Alcotest.failf "empty selector raised %s instead of Error.Parse_error"
        (Printexc.to_string exn)

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
  check_construct ".test123" (class_ "test123");

  (* Test that raw class name gets properly escaped *)
  let check_class_escaping raw expected_escaped =
    let sel = class_ raw in
    let output = to_string ~minify:true sel in
    Alcotest.(check string)
      (Fmt.str "class escaping for %S" raw)
      expected_escaped output
  in

  (* Characters that need escaping *)
  check_class_escaping "w-1/2" ".w-1\\/2";
  check_class_escaping "sm:p-4" ".sm\\:p-4";
  check_class_escaping "p-[10px]" ".p-\\[10px\\]";
  check_class_escaping "text-#ff0000" ".text-\\#ff0000";
  check_class_escaping "content-\"hello\"" ".content-\\\"hello\\\"";
  check_class_escaping "my class" ".my\\ class";
  check_class_escaping "data-@value" ".data-\\@value";
  check_class_escaping "grid-*-auto" ".grid-\\*-auto";
  check_class_escaping "has,comma" ".has\\,comma";
  check_class_escaping "has.dot" ".has\\.dot";
  check_class_escaping "has(paren)" ".has\\(paren\\)";
  check_class_escaping "has%percent" ".has\\%percent";
  check_class_escaping "has'quote" ".has\\'quote";

  (* Characters that don't need escaping *)
  check_class_escaping "normal-class" ".normal-class";
  check_class_escaping "with-hyphen" ".with-hyphen";
  check_class_escaping "with_underscore" ".with_underscore";
  check_class_escaping "CamelCase123" ".CamelCase123"

(* Not a roundtrip test *)
let id_cases () =
  (* Test ID selectors *)
  check_construct "#myid" (id "myid");
  check_construct "#my-id" (id "my-id");
  check_construct "#my_id" (id "my_id");
  check_construct "#id123" (id "id123");

  (* Test that raw ID gets properly escaped *)
  let check_id_escaping raw expected_escaped =
    let sel = id raw in
    let output = to_string ~minify:true sel in
    Alcotest.(check string)
      (Fmt.str "id escaping for %S" raw)
      expected_escaped output
  in

  (* Characters that need escaping *)
  check_id_escaping "item:1" "#item\\:1";
  check_id_escaping "user@domain" "#user\\@domain";
  check_id_escaping "ref#123" "#ref\\#123";
  check_id_escaping "item[0]" "#item\\[0\\]";
  check_id_escaping "my.id" "#my\\.id";
  check_id_escaping "has space" "#has\\ space";

  (* Identifiers starting with digits need hex escaping *)
  check_id_escaping "9section" "#\\39 section";
  check_id_escaping "0item" "#\\30 item";

  (* Characters that don't need escaping *)
  check_id_escaping "normal-id" "#normal-id";
  check_id_escaping "with_underscore" "#with_underscore";
  check_id_escaping "CamelCase" "#CamelCase"

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
  (* Per CSS Selectors 4 section 13.3.1 the printer canonicalizes [even] to [2n]
     and [2n+1] to [odd] under minify. *)
  check_construct ":nth-child(2n)" (nth_child Even);
  check_construct ":nth-child(odd)" (nth_child (An_plus_b (2, 1)));
  (* nth with Index and of clause. Per CSS Selectors 4 section 13.3.1 the
     printer canonicalizes [<an+b>] to the shortest equivalent spelling -
     [(0n+5)] -> [(5)] / [(1)] -> [:first-child]. *)
  check ~expected:":first-child" ":nth-child(1)";
  check ":nth-child(5)";
  check ~expected:":nth-child(odd of.item)" ":nth-child( odd of .item )";
  check ~expected:":nth-child(2n-1 of a,b)" ":nth-child( 2n-1 of a , b )";
  check ":nth-of-type(3)";
  check ~expected:":nth-last-child(2 of.x,.y)" ":nth-last-child(2 of .x , .y)";
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
  check_construct ":before" (Before Single);
  check_construct ":after" (After Single);
  check_construct ":first-line" (First_line Single);
  check_construct ":first-letter" (First_letter Single);
  check_construct "::marker" Marker;

  (* Selectors 4: legacy single-colon syntax remains accepted for the CSS2
     pseudo-elements. Minified output uses the shorter (single-colon) spelling;
     pretty output preserves the authored form. *)
  check ":before";
  check ~expected:":before" "::before";
  check_pretty_to ":before" ":before";
  check_pretty_to "::before" "::before";
  check ":after";
  check ~expected:":after" "::after";
  check_pretty_to ":after" ":after";
  check_pretty_to "::after" "::after";
  check ":first-line";
  check ~expected:":first-line" "::first-line";
  check_pretty_to ":first-line" ":first-line";
  check_pretty_to "::first-line" "::first-line";
  check ":first-letter";
  check ~expected:":first-letter" "::first-letter";
  check_pretty_to ":first-letter" ":first-letter";
  check_pretty_to "::first-letter" "::first-letter";

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
  check "::-webkit-scrollbar"

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

  (* CSS Syntax ident sequences can start with [--], so minification can leave
     these as identifiers instead of forcing strings. *)
  check ~expected:"[id=--custom]" "[id=\"--custom\"]";
  check ~expected:"[class=--modifier]" "[class=\"--modifier\"]";

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
  check ~expected:"[type=button i]" "[type=button I]";
  check ~expected:"[type=button i]" "[type=\"button\" i]";
  (* Normalizes to unquoted *)
  check "[class=\"My Class\" s]";
  check ~expected:"[class=\"My Class\" s]" "[class=\"My Class\" S]";

  (* Case modifiers *)
  check_construct "[attr=v i]" (attribute ~flag:Insensitive "attr" (Exact "v"));
  check_construct "[attr=v s]" (attribute ~flag:Sensitive "attr" (Exact "v"));

  (* Namespaced attributes *)
  check_construct "[ns|attr]" (attribute ~ns:(Prefix "ns") "attr" Presence);
  check_construct "[*|attr]" (attribute ~ns:Any "attr" Presence);

  (* Negative cases: CSS Syntax sec. 2.2 / sec. 4.3.5 mandate recovery for
     unterminated brackets (['\[']) and strings at EOF, so those are spec-valid
     and not tested here. *)
  neg_cursor read "[attr=]";
  (* Missing value *)
  neg_cursor read "[=value]";
  (* Empty attribute name *)
  neg_cursor read "[attr&=value]";
  (* Invalid operator *)
  neg_cursor read "[]" (* Empty attribute *)

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
  (* Test selector lists. pp holds the authored branch order in BOTH pretty and
     minify; sorting branches into canonical order (and deduping them) is a node
     change, so it is an optimize transform, not a pp/minify one (see the
     selector-list both-paths oracles in test_stylesheet). *)
  check_construct ".a,.b,.c" (list [ class_ "a"; class_ "b"; class_ "c" ]);
  check_construct "h1,h2,h3" (list [ element "h1"; element "h2"; element "h3" ]);
  check_construct "div,.class,#id"
    (list [ element "div"; class_ "class"; id "id" ]);
  check ~expected:"div,.class,#id" "div, .class, #id";
  check_pretty_to "div, .class, #id" "div, .class, #id";

  (* Complex grouped selectors. pp holds the authored branch order (whitespace
     normalized, legacy pseudo-elements lowercased to a single colon); only
     optimize sorts branches into canonical order (see test_stylesheet). *)
  check ~expected:".parent>.child,.other-parent .descendant"
    ".parent > .child, .other-parent .descendant";
  check ~expected:"h1:hover,h2:focus,h3:active" "h1:hover, h2:focus, h3:active";
  check ~expected:"[data-attr],[aria-label],[role=button]"
    "[data-attr], [aria-label], [role=button]";
  check ~expected:":before,:after,:first-letter"
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
  check ~expected:"h1:before,h2:before,h3:before,h4:before"
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
  neg_cursor read ".a,";
  (* Trailing comma is invalid *)
  neg_cursor read ",.b";
  (* Leading comma is invalid *)
  neg_cursor read ".a,,b";
  (* Double comma is invalid *)
  ()

(* Not a roundtrip test *)
let where_is_cases () =
  (* Test :where() and :is() *)
  check_construct ":where(div)" (where [ element "div" ]);
  check_construct ":where(.a,.b)" (where [ class_ "a"; class_ "b" ]);
  (* Splitting an equal-specificity [:is()] into a selector list is a node
     change reserved for [Selector.canonicalize]; pp is lexical-only and holds
     the wrapper. *)
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

  (* Pseudo-classes. Per CSS Selectors 4 section 13.3.1 the printer
     canonicalizes [:nth-child] formulas to the shortest spec-equivalent
     spelling. *)
  check ":hover";
  check ":nth-child(2)";
  check ~expected:":nth-child(odd)" ":nth-child(2n+1)";
  check ":nth-child(odd)";
  check ~expected:":nth-child(2n)" ":nth-child(even)";

  (* Legacy pseudo-elements use the shortest valid spelling when minified. *)
  check ":before";
  check ~expected:":before" "::before";
  check ":after";
  check ~expected:":after" "::after";
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
  check ~expected:"div.class#id[href]:hover:after"
    "div.class#id[href]:hover::after";
  check ~expected:".a,.b,.c" ".a, .b, .c";
  check ":where(.a,.b)";
  check ":is(h1,h2,h3)";
  check ":not(.active)";

  (* Escaping roundtrip tests *)
  (* Helper to test roundtrip for class selectors *)
  let check_class_roundtrip escaped =
    let sel = of_string ("." ^ escaped) in
    let output = to_string ~minify:true sel in
    Alcotest.(check string)
      (Fmt.str "class roundtrip for %S" escaped)
      ("." ^ escaped) output
  in

  (* Helper to test roundtrip for ID selectors *)
  let check_id_roundtrip escaped =
    let sel = of_string ("#" ^ escaped) in
    let output = to_string ~minify:true sel in
    Alcotest.(check string)
      (Fmt.str "id roundtrip for %S" escaped)
      ("#" ^ escaped) output
  in

  (* Helper to test roundtrip for element selectors *)
  let check_element_roundtrip escaped =
    let sel = of_string escaped in
    let output = to_string ~minify:true sel in
    Alcotest.(check string)
      (Fmt.str "element roundtrip for %S" escaped)
      escaped output
  in

  (* Class selectors - simple escapes *)
  check_class_roundtrip "w-1\\/2";
  check_class_roundtrip "sm\\:p-4";
  check_class_roundtrip "p-\\[10px\\]";
  check_class_roundtrip "text-\\#ff0000";
  check_class_roundtrip "content-\\\"hello\\\"";
  check_class_roundtrip "my\\ class";
  check_class_roundtrip "data-\\@value";
  check_class_roundtrip "grid-\\*-auto";

  (* ID selectors - simple escapes *)
  check_id_roundtrip "my-id";
  check_id_roundtrip "header\\:main";
  check_id_roundtrip "item-\\#1";
  check_id_roundtrip "user\\@domain";

  (* Element selectors - no escaping needed *)
  check_element_roundtrip "div";
  check_element_roundtrip "custom-element";
  check_element_roundtrip "my-component";

  (* Hex escapes - these get normalized to simpler escape forms *)
  let sel = of_string ".\\3A hover" in
  let output = to_string ~minify:true sel in
  (* \3A (hex for ':') + "hover" -> unescapes to ":hover" -> escapes to
     "\:hover" *)
  Alcotest.(check string) "class hex escape normalizes" ".\\:hover" output;

  let sel = of_string "#\\2F value" in
  let output = to_string ~minify:true sel in
  (* \2F (hex for '/') + "value" -> unescapes to "/value" -> escapes to
     "\/value" *)
  Alcotest.(check string) "id hex escape normalizes" "#\\/value" output;

  let sel = of_string ".\\5B test\\5D" in
  let output = to_string ~minify:true sel in
  (* \5B = '[', \5D = ']' - note: trailing space after hex escape is consumed *)
  Alcotest.(check string) "class hex escape normalizes" ".\\[test\\]" output;

  (* Mixed escapes *)
  check_class_roundtrip "sm\\:hover\\:bg-\\[\\#ff0000\\]";

  (* Unescaped parts remain unescaped *)
  check_class_roundtrip "normal-class";
  check_class_roundtrip "with-hyphen";
  check_class_roundtrip "with_underscore"

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

  (* class_ accepts raw strings including those starting with digits, as they
     will be escaped during output. This is by design for Tailwind-style
     classes. *)
  let sel = class_ "9class" in
  let output = to_string ~minify:true sel in
  Alcotest.(check string)
    "class starting with digit gets escaped" ".\\39 class" output;

  (* Local constructor policy, not selector syntax. *)
  check_invalid "double dash class"
    "CSS identifier '--var' cannot start with '--' (reserved for custom \
     properties)" (fun () -> ignore (class_ "--var"));

  check_invalid "double dash id"
    "CSS identifier '--id' cannot start with '--' (reserved for custom \
     properties)" (fun () -> ignore (id "--id"));

  (* class_ and id accept dash-digit, but escape it properly *)
  let sel = class_ "-9test" in
  let output = to_string ~minify:true sel in
  Alcotest.(check string)
    "class starting with dash-digit gets escaped" ".\\2d 9test" output;

  let sel = id "-9test" in
  let output = to_string ~minify:true sel in
  Alcotest.(check string)
    "id starting with dash-digit gets escaped" "#\\2d 9test" output;

  (* Parser syntax path: [--] is a valid ident start. *)
  check_minified_equiv ".--var";
  check_minified_equiv "#--id";

  (* Parsing invalid selector strings via Cursor.option to avoid exceptions *)
  let neg_parse s label =
    let c = Cursor.of_string s in
    Alcotest.(check bool) label true (Option.is_none (Cursor.option read c))
  in
  (* CSS Syntax 5.3.7 / 4.3.5 mandate recovery for unterminated blocks and
     strings at EOF -- assert the recovered AST matches what an explicit closing
     bracket / quote would have produced, rather than silently dropping
     content. *)
  check ~expected:"[href]" "[href";
  check ~expected:"[attr=value]" "[attr=value";
  check ~expected:"[attr=\"value]\"]" "[attr=\"value]";
  neg_parse ":nth-child(2n+)" "invalid nth-child syntax";
  neg_parse ".class,,.other" "double comma in list";
  neg_parse "div > > span" "double combinator";
  (* Selectors 4 sec. 5.1 spells a class as a [.] "immediately followed by" an
     ident, and sec. 3.5 a pseudo-class as a [:] followed by its name, so a
     space between the two names nothing. Reading past it turned [. x a] into
     [.x a] and [: hover] into [:hover], selectors the author never wrote. *)
  neg_parse ". x" "space between the dot and the class name";
  neg_parse "a . b" "space between the dot and the class name after a type";
  neg_parse ": hover" "space between the colon and the pseudo-class name";
  neg_parse ":: before" "space between the colons and the pseudo-element name";
  check_minified_equiv ".x";
  check_minified_equiv "a .b";
  check_minified_equiv ":hover";
  check_minified_equiv "::before"

(* Test broken selectors with Parse_error exceptions. The exact error rendering
   moved when we switched to the component-stream parser; tests now only assert
   that parsing fails (and mention what was broken). *)
let check_parse_error input _expected_msg =
  let t = Cursor.of_string input in
  try
    let _ = read t in
    Alcotest.failf "expected Parse_error for '%s' but parsing succeeded" input
  with
  | Cursor.Parse_error _ -> ()
  | exn ->
      Alcotest.failf "For '%s': expected Parse_error but got %s" input
        (Printexc.to_string exn)

let parse_errors_attributes () =
  (* CSS Syntax 5.3.7 auto-closes unterminated brackets at EOF, so missing
     closing bracket selectors are spec-valid and not asserted here. *)
  check_parse_error ".test[]" "expected identifier";
  check_parse_error ".test[[attr]]" "expected identifier";
  check_parse_error ".test[data id=\"value\"]" "trailing tokens";
  check_parse_error ".test[data-id=value with spaces]" "trailing tokens"

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
  (* Unterminated [:not(...)] auto-closes at EOF per CSS Syntax 5.3.7. *)
  check_parse_error ".test:not()" "expected at least one selector";
  check_parse_error ".test:has()" "expected at least one selector"

(* Every [::] spelling cascade parses, in the shortest form that reaches its own
   constructor. The list is the pseudo-element inventory of CSS Pseudo-Elements
   4, Selectors 4, CSS Highlight API 1, CSS Shadow Parts 1, WebVTT and CSS View
   Transitions 1, plus the vendor names shipping engines expose, plus the two
   kinds cascade cannot name: an unrecognised [::foo] and the framework-only
   [::deep] / [::v-deep] / [::ng-deep]. *)
let pseudo_element_spellings =
  [
    ":before";
    ":after";
    ":first-line";
    ":first-letter";
    "::before";
    "::after";
    "::first-line";
    "::first-letter";
    "::backdrop";
    "::marker";
    "::placeholder";
    "::selection";
    "::target-text";
    "::spelling-error";
    "::grammar-error";
    "::file-selector-button";
    "::details-content";
    "::view-transition";
    "::view-transition-group(*)";
    "::view-transition-image-pair(*)";
    "::view-transition-old(*)";
    "::view-transition-new(*)";
    "::part(tab)";
    "::slotted(p)";
    "::cue";
    "::cue-region";
    "::highlight(find)";
    "::-moz-placeholder";
    "::-webkit-input-placeholder";
    "::-ms-input-placeholder";
    "::-webkit-scrollbar";
    "::-webkit-search-cancel-button";
    "::-webkit-search-decoration";
    "::-webkit-datetime-edit";
    "::-webkit-date-and-time-value";
    "::-webkit-inner-spin-button";
    "::-webkit-outer-spin-button";
    "::-webkit-calendar-picker-indicator";
    "::-webkit-details-marker";
    "::future-pseudo-element";
    "::foo(bar)";
    "::deep";
    "::v-deep";
    "::ng-deep";
    "::-webkit-scrollbar-thumb";
    "::-webkit-scrollbar-track";
    "::-webkit-scrollbar-track-piece";
    "::-webkit-scrollbar-button";
    "::-webkit-scrollbar-corner";
    "::-webkit-resizer";
  ]

(* The [::] names above that cascade carries as a raw ident rather than a
   constructor of its own. Every other entry of [pseudo_element_spellings] names
   a pseudo-element cascade models.

   No engine implements any of these, so cascade has nothing to judge them
   against and keeps the rule: the name may be one a browser ships next.
   [::deep], [::v-deep] and [::ng-deep] are Vue and Angular tooling spellings
   that no engine has ever parsed. Chrome 151 and WebKit 26.5 drop every one,
   and Lightning CSS reads every one back verbatim, which is what
   test/interop/lightning records. *)
let unmodelled_pseudo_element_spellings =
  [ "::future-pseudo-element"; "::foo(bar)"; "::deep"; "::v-deep"; "::ng-deep" ]

let modelled_pseudo_element_spellings =
  List.filter
    (fun pe -> Stdlib.not (List.mem pe unmodelled_pseudo_element_spellings))
    pseudo_element_spellings

(* Selectors 4 sec. 16: a complex selector unit is [<compound-selector>?
   <pseudo-compound-selector>*] and a pseudo-compound is
   [<pseudo-element-selector> <pseudo-class-selector>*], so a class, id or
   attribute selector may never follow a pseudo-element; sec. 4.5 keeps
   pseudo-elements out of [:has()] as well. Both rules key off the [::] form,
   not off whether the name is one cascade knows, so an unrecognised [::foo] and
   the framework-only [::deep] family are bound by them too. Cascade keeps a
   bare pseudo-element it does not recognise, since the name may be one a
   browser already ships, but the shape rules apply all the same. Chrome 151 and
   WebKit 26.5 drop every rule the negatives below build. *)
let pseudo_element_compound_guard () =
  let parses input =
    match Cursor.option read (Cursor.of_string input) with
    | Some _ -> ()
    | None -> Alcotest.failf "pseudo-element selector should parse: %s" input
  in
  List.iter
    (fun pe ->
      parses (String.concat "" [ ".a"; pe ]);
      neg_cursor read (String.concat "" [ ".a"; pe; ".b" ]);
      neg_cursor read (String.concat "" [ ".a"; pe; "#c" ]);
      neg_cursor read (String.concat "" [ ".a"; pe; "[d]" ]);
      neg_cursor read (String.concat "" [ ".a:has("; pe; ")" ]))
    pseudo_element_spellings

(* Selectors 4 sec. 16 builds both argument lists of the logical combinations
   out of [<complex-real-selector>], which is [<compound-selector> [
   <combinator>? <compound-selector> ]*] and so has no room for a
   [<pseudo-compound-selector>]: sec. 4.3 spells that out for [:not()]
   ("Pseudo-elements cannot be represented by the negation pseudo-class; they
   are not valid within :not()"), sec. 4.2 for [:is()], sec. 4.4 gives
   [:where()] the syntax of [:is()], and sec. 4.5 keeps them out of [:has()] as
   well.

   What a caller sees differs, because [:is()] and [:where()] take a
   [<forgiving-selector-list>] (sec. 16.1: parse each item, drop the ones that
   fail) while [:not()] and [:has()] do not: the pseudo-element takes the item
   with it in the first pair and the whole selector in the second. Chrome 151
   and WebKit 26.5 agree on both halves - they drop every rule the negatives
   build, and keep every rule the positives build (Chrome prunes the dead item
   from its [cssRules] readback, WebKit leaves it in place). *)
let logical_combinator_pseudo_element () =
  let concat = String.concat "" in
  List.iter
    (fun pe ->
      (* Unforgiving: the pseudo-element invalidates the whole selector,
         wherever in the argument it sits and however the two nest. *)
      neg_cursor read (concat [ ".a:not("; pe; ")" ]);
      neg_cursor read (concat [ ".a:not("; pe; ",.b)" ]);
      neg_cursor read (concat [ ".a:not(.b "; pe; ")" ]);
      neg_cursor read (concat [ ".a:not(:not("; pe; "))" ]);
      neg_cursor read (concat [ ".a:has("; pe; ",.b)" ]);
      neg_cursor read (concat [ ".a:has(:not("; pe; "))" ]);
      neg_cursor read (concat [ ".a:not(:has("; pe; "))" ]);
      (* Forgiving: only the item carrying the pseudo-element goes. *)
      check_minified_to ".a:is()" (concat [ ".a:is("; pe; ")" ]);
      check_minified_to ".a:where()" (concat [ ".a:where("; pe; ")" ]);
      check_minified_to ".a:is(.b)" (concat [ ".a:is("; pe; ",.b)" ]);
      check_minified_to ".a:is()" (concat [ ".a:is(:not("; pe; "))" ]);
      check_minified_to ".a:where()" (concat [ ".a:where(:not("; pe; "))" ]);
      check_minified_to ".a:not(:is())" (concat [ ".a:not(:is("; pe; "))" ]);
      check_minified_to ".a:not(:where())"
        (concat [ ".a:not(:where("; pe; "))" ]))
    pseudo_element_spellings

(* CSS Selectors 4 (Editor's Draft, drafts.csswg.org/selectors-4/) sec. 3.6.5
   "Internal Structure": "Some pseudo-elements are defined to have internal
   structure. These pseudo-elements may be followed by child/descendant
   combinators to express those relationships. Selectors containing combinators
   after the pseudo-element are otherwise invalid", with "::first-letter + span
   and ::first-letter em are invalid selectors" as the section's own examples.
   Sec. 3.6.2 says the same thing from the other side: a pseudo-element follows
   the compound selector matching its originating element, and what comes after
   applies to the pseudo-element itself.

   Nothing shipping claims that internal structure, so the exception stays
   theoretical and every name cascade models takes the rule. Chrome 151 and
   WebKit 26.5 both drop [::part(x) > .b], [::details-content > div],
   [::file-selector-button > div] and [::view-transition-group(...) > div]
   alongside the plain [.a::before .b], and the Servo selectors crate - the
   parser Firefox's Stylo shares, reached here through Lightning CSS - rejects
   the same shapes with UnexpectedSelectorAfterPseudoElement.

   A [::] name cascade does not model is exempt, for the reason
   [pseudo_element_allows] already exempts it from sec. 3.6.3: cascade preserves
   such a name rather than judging it. test/interop/lightning carries seven of
   these ([.foo ::deep .bar], [.foo ::unknown(.foo) .bar] and their kin) and all
   six reference minifiers keep every one verbatim, while Lightning CSS rejects
   the identical shape as soon as the name is one it knows ([::-moz-placeholder
   .b]).

   [>>>] and [/deep/] are outside all of this: they are Vue and Angular tooling
   spellings, no engine parses them, and cascade passes them through under the
   same bargain as an unmodelled [::] name. *)
let pseudo_element_combinator_guard () =
  let concat = String.concat "" in
  let parses input =
    match Cursor.option read (Cursor.of_string input) with
    | Some _ -> ()
    | None -> Alcotest.failf "selector should parse: %s" input
  in
  (* Selectors 4 sec. 15: every combinator the specification defines. *)
  let combinators = [ " "; ">"; "+"; "~"; " || " ] in
  (* Tooling spellings cascade passes through; sec. 3.6.5 never reaches them. *)
  let passthrough_combinators = [ ">>>"; "/deep/" ] in
  List.iter
    (fun pe ->
      (* The pseudo-compound itself, and a pseudo-element ending the selector,
         are what sec. 3.6.5 leaves alone. *)
      parses (concat [ ".a"; pe ]);
      parses (concat [ ".z .a"; pe ]);
      parses (concat [ ".z > .a"; pe ]);
      List.iter
        (fun c -> neg_cursor read (concat [ ".a"; pe; c; ".b" ]))
        combinators;
      List.iter
        (fun c -> parses (concat [ ".a"; pe; c; ".b" ]))
        passthrough_combinators)
    modelled_pseudo_element_spellings;
  List.iter
    (fun pe ->
      List.iter
        (fun c -> parses (concat [ ".a"; pe; c; ".b" ]))
        (combinators @ passthrough_combinators))
    unmodelled_pseudo_element_spellings;
  (* The corpus case, spelled as test/interop/lightning carries it. *)
  parses ".foo ::deep .bar";
  parses ".foo ::unknown(.foo) .bar";
  (* [::cue] and [::cue-region] are modelled in their functional form. *)
  neg_cursor read ".a::cue(v) .b";
  neg_cursor read ".a::cue-region(v) > .b";
  parses ".a::cue(v)";
  (* A pseudo-class between the pseudo-element and the combinator does not
     reopen the compound: [::part()] takes [:focus] (CSS Pseudo-Elements 4 sec.
     5) and the combinator after it is still invalid. *)
  neg_cursor read ".a::part(x):focus > .b";
  parses ".a::part(x):focus";
  (* Sec. 3.6.5 governs the top level of the selector. A combinator inside a
     functional pseudo-class argument is a different question and stays
     untouched. *)
  parses ".a:is(.b > .c) .d";
  parses ".a:has(.b > .c) .d";
  parses ".a:not(.b > .c) .d";
  parses ".a:has(> .b) .c";
  (* Sec. 3.9 and sec. 16.1: [:is()] and [:where()] take a forgiving list, so
     the refused argument goes and the selector stands; [:not()] does not, so
     the whole selector goes. Chrome 151 agrees on both, reading the first back
     as [.a:is(.d)] and dropping the rule for the third. *)
  check_minified_to ".a:is(.d)" ".a:is(.b::before .c,.d)";
  check_minified_to ".a:where(.d)" ".a:where(.b::before .c,.d)";
  neg_cursor read ".a:not(.b::before .c)"

(* CSS Selectors 4 (Editor's Draft, drafts.csswg.org/selectors-4/) sec. 3.6.4
   "Sub-pseudo-elements": "Unless the corresponding sub-pseudo-element is
   explicitly defined to exist in another specification, pseudo-element
   selectors are not valid when compounded to another pseudo-element selector.
   So, for example, ::before::before is an invalid selector, but
   ::before::marker is valid". The rule is a pointer, so the question is which
   other specification defines what, and the answer is three rows.

   CSS Pseudo-Elements 4 (Editor's Draft, drafts.csswg.org/css-pseudo-4/) sec.
   4.2: "The ::before::marker or ::after::marker selectors are valid [...]
   However ::marker::marker is invalid". Chrome 151 keeps [::before::marker] and
   its legacy [:before::marker] spelling, and drops [::marker::marker].

   Same draft, sec. 5 "Element-backed Pseudo-Elements": "All pseudo-classes and
   pseudo-elements are syntactically allowed after an element-backed
   pseudo-element (such as x-button::part(label):hover or
   x-button::part(label)::before), just as if the pseudo-element were a type
   selector; but some are disallowed from matching: [...] ::part() never
   matches". cascade turns the never-matching rows into a refusal, the way
   [pseudo_element_allows] already turns sec. 5's [:has()] and structural rows
   into one, so [::part()] and [::slotted()] never follow another
   pseudo-element: Chrome 151, WebKit 27 and Lightning CSS all drop
   [::part(x)::part(y)] and [::slotted(a)::part(y)]. [::slotted()] is in no such
   row, so it stays allowed there and WebKit 27 keeps [::part(x)::slotted(b)].
   The element-backed row is [::part()] and [::details-content], the pair
   [pseudo_element_allows] already carries. Sec. 5.1 puts
   [::file-selector-button] in the same section, but all three drop
   [::file-selector-button::before], so it stays out on the same
   engines-over-spec-text bargain the sec. 3.6.3 rows take.

   CSS Shadow 1 (Editor's Draft, drafts.csswg.org/css-shadow-1/, the document
   css-scoping-1 now redirects to) sec. 3.2.4: "The ::slotted() pseudo-element
   can be followed by a tree-abiding pseudo-element, like ::slotted()::before".
   Tree-abiding is narrower than element-backed and the engines show the gap:
   all three keep [::slotted(a)::before] and [::slotted(a)::marker] and drop
   [::slotted(a)::first-line], while all three keep [::part(x)::first-line]. The
   tree-abiding names are css-pseudo-4 sec. 4 ([::before], [::after],
   [::marker], [::placeholder]), the [::backdrop] and [::view-transition] of its
   sec. 7.1 list, and the element-backed ones its sec. 5 calls "always
   tree-abiding".

   The rule reads one adjacent pair at a time, so a chain longer than two is
   exactly as valid as each of its links: Chrome 151 keeps
   [::part(x)::before::marker] and [::slotted(a)::before::marker] and drops
   [::part(x)::marker::before] and [::before::marker::before].

   A [::] name cascade does not model keeps its exemption in this position too,
   for the reason [pseudo_element_allows] exempts it from sec. 3.6.3: cascade
   preserves such a name rather than judging it, and Lightning CSS reads
   [::foo::bar] and [::foo::before] back unchanged. A name cascade does model is
   judged, so [::before::foo] goes. *)
let sub_pseudo_element_guard () =
  let concat = String.concat "" in
  let parses input =
    let c = Cursor.of_string input in
    match read c with
    | exception Error.Parse_error _ ->
        Alcotest.failf "selector should parse: %s" input
    | _ ->
        if Stdlib.not (Cursor.is_done c) then
          Alcotest.failf "selector only partly read: %s" input
  in
  let mem s = List.exists (String.equal s) in
  let element_backed = [ "::part(tab)"; "::details-content" ] in
  let tree_abiding =
    [
      ":before";
      "::before";
      ":after";
      "::after";
      "::marker";
      "::placeholder";
      "::backdrop";
      "::view-transition";
      "::details-content";
      "::file-selector-button";
    ]
  in
  let never_a_sub = [ "::part(tab)" ] in
  let generated_content = [ ":before"; "::before"; ":after"; "::after" ] in
  let allowed origin sub =
    if mem sub never_a_sub then false
    else if mem origin element_backed then true
    else if String.equal origin "::slotted(p)" then mem sub tree_abiding
    else if mem origin generated_content then String.equal sub "::marker"
    else false
  in
  List.iter
    (fun origin ->
      List.iter
        (fun sub ->
          let s = concat [ ".a"; origin; sub ] in
          if allowed origin sub then parses s else neg_cursor read s)
        modelled_pseudo_element_spellings;
      List.iter
        (fun sub ->
          let s = concat [ ".a"; origin; sub ] in
          if mem origin element_backed then parses s else neg_cursor read s)
        unmodelled_pseudo_element_spellings)
    modelled_pseudo_element_spellings;
  List.iter
    (fun origin ->
      List.iter
        (fun sub -> parses (concat [ ".a"; origin; sub ]))
        pseudo_element_spellings)
    unmodelled_pseudo_element_spellings;
  (* The three shapes the guard used to delete, read back after a print. *)
  check_minified_to ".a:before::marker" ".a::before::marker";
  check_minified_to "x-b::part(label):before" "x-b::part(label)::before";
  check_minified_to "::slotted(a):before" "::slotted(a)::before";
  check_minified_to ".a:after::marker" ".a::after::marker";
  check_minified_to ".a:before::marker" ".a:before::marker";
  check_minified_to "::part(label):first-line" "::part(label)::first-line";
  check_minified_to "::slotted(a)::marker" "::slotted(a)::marker";
  check_minified_to "::details-content:before" "::details-content::before";
  check_minified_to "::part(label)::slotted(a)" "::part(label)::slotted(a)";
  neg_cursor read "::slotted(a)::slotted(b)";
  neg_cursor read "::part(label)::part(x)";
  neg_cursor read "::details-content::part(x)";
  check_minified_to "::part(label):before::marker"
    "::part(label)::before::marker";
  check_minified_to "::slotted(a):before::marker" "::slotted(a)::before::marker";
  neg_cursor read ".a::before::marker::before";
  neg_cursor read ".a::part(label)::marker::before";
  (* A pseudo-class between the two belongs to the pseudo-element on its left,
     and the one after it to the pseudo-element on its own left. *)
  check_minified_to "::part(label):hover:before" "::part(label):hover::before";
  (* Sec. 3.6.5 is the other question and answers it as before: a combinator
     after the chain is invalid whatever the chain is, and an unmodelled name
     stays exempt. *)
  neg_cursor read ".a::before::marker > .b";
  neg_cursor read ".a::part(label)::before .b";
  parses ".foo ::deep .bar";
  (* Sec. 3.6.3 is untouched: [::before] still takes no [:hover], [::part()]
     still does, and what may never follow a pseudo-element still may not. *)
  neg_cursor read ".a::before:hover";
  neg_cursor read ".a::before::marker:hover";
  neg_cursor read ".a::part(label)::before:hover";
  check_minified_to ".a::part(label):hover" ".a::part(label):hover";
  neg_cursor read ".a::before::marker.class";
  neg_cursor read ".a::before::marker#id";
  neg_cursor read ".a::before::marker[x]"

(* CSS Selectors 4 sec. 9. Sec. 3.6.3 allows these after every pseudo-element;
   the engines are narrower, and disagree with the spec on the four Level 2
   pseudo-elements ([::first-line:hover] is the spec's own example and both
   engines drop it), so the rows below follow the engines. *)
let user_action_pseudo_classes =
  [ ":hover"; ":active"; ":focus"; ":focus-visible"; ":focus-within" ]

(* CSS Pseudo-Elements 4 sec. 5: an element-backed pseudo-element takes the
   pseudo-classes a real element takes, bar the ones that would report on the
   tree it sits in - the tree-structural pseudo-classes (Selectors 4 sec. 13)
   and [:has()]. *)
let element_backed_pseudo_classes =
  user_action_pseudo_classes
  @ [
      ":enabled";
      ":disabled";
      ":checked";
      ":defined";
      ":link";
      ":target";
      ":dir(ltr)";
      ":lang(en)";
    ]

(* WebKit, "Styling Scrollbars": the state a scrollbar part reports about
   itself. [:window-inactive] is the one that is about the window rather than
   the scrollbar, and the one that reaches past the scrollbar. *)
let scrollbar_state_pseudo_classes =
  [
    ":horizontal";
    ":vertical";
    ":decrement";
    ":increment";
    ":start";
    ":end";
    ":double-button";
    ":single-button";
    ":no-button";
    ":corner-present";
    ":window-inactive";
  ]

(* Each probe names a pseudo-class cascade recognises: an unrecognised one is
   already a parse error at an unforgiving site, whatever precedes it, so it
   would not tell us anything about the pseudo-element's own rules. *)
let probe_pseudo_classes =
  element_backed_pseudo_classes @ scrollbar_state_pseudo_classes
  @ [
      ":root";
      ":empty";
      ":first-child";
      ":only-child";
      ":nth-child(1)";
      ":has(.b)";
    ]

(* What each pseudo-element accepts after it. Rows come from CSS Pseudo-Elements
   4 sec. 5 for the element-backed ones and from Chrome 151 and WebKit 26.5
   everywhere else, taking a pseudo-class as accepted when either engine keeps
   the rule: Selectors 4 sec. 3.6.3 hands the per-pseudo-element list to "other
   specifications", and for the UA widgets that list is only written down in the
   engines. *)
let pseudo_element_pseudo_class_rows =
  [
    (* Nothing beyond the logical combinations. *)
    ( [
        ":before";
        ":after";
        ":first-line";
        ":first-letter";
        "::before";
        "::after";
        "::first-line";
        "::first-letter";
        "::backdrop";
        "::marker";
        "::target-text";
        "::spelling-error";
        "::grammar-error";
        "::highlight(find)";
        "::view-transition";
        "::slotted(p)";
        "::cue(v)";
      ],
      [] );
    (* The UA widgets that stand in for a real control. *)
    ( [
        "::placeholder";
        "::file-selector-button";
        "::-webkit-input-placeholder";
        "::-webkit-search-cancel-button";
        "::-webkit-search-decoration";
        "::-webkit-datetime-edit";
        "::-webkit-datetime-edit-fields-wrapper";
        "::-webkit-datetime-edit-year-field";
        "::-webkit-datetime-edit-month-field";
        "::-webkit-datetime-edit-day-field";
        "::-webkit-datetime-edit-hour-field";
        "::-webkit-datetime-edit-minute-field";
        "::-webkit-datetime-edit-second-field";
        "::-webkit-datetime-edit-millisecond-field";
        "::-webkit-datetime-edit-meridiem-field";
        "::-webkit-date-and-time-value";
        "::-webkit-inner-spin-button";
        "::-webkit-outer-spin-button";
        "::-webkit-calendar-picker-indicator";
        "::-webkit-details-marker";
      ],
      user_action_pseudo_classes );
    (* The scrollbar takes no focus, and reports its own state instead. Every
       part reads the same row: Chrome 151, WebKit 26.5 and Lightning CSS keep
       [::-webkit-resizer:vertical] and [::-webkit-scrollbar-thumb:enabled] and
       drop [::-webkit-scrollbar-thumb:focus]. *)
    ( [
        "::-webkit-scrollbar";
        "::-webkit-scrollbar-button";
        "::-webkit-scrollbar-track";
        "::-webkit-scrollbar-track-piece";
        "::-webkit-scrollbar-thumb";
        "::-webkit-scrollbar-corner";
        "::-webkit-resizer";
      ],
      [ ":hover"; ":active"; ":enabled"; ":disabled" ]
      @ scrollbar_state_pseudo_classes );
    (* WebVTT 1 sec. 8.2.1 and sec. 8.2.3 define a cue and a region as boxes the
       page styles, not as controls it drives, and the engines give the pair the
       user-action row and nothing more: all three keep [::cue:focus-within] and
       drop [::cue:enabled] and [::cue:lang(en)]. Chrome and WebKit implement
       neither form of [::cue-region], so Lightning CSS answers that row alone
       and answers it the same way. *)
    ([ "::cue"; "::cue-region" ], user_action_pseudo_classes);
    (* WebKit's [::selection:window-inactive], and nothing else. *)
    ([ "::selection" ], [ ":window-inactive" ]);
    (* CSS View Transitions 1 sec. 3.1: [:only-child] matches a view transition
       pseudo with no sibling in the pseudo-element tree. *)
    ( [
        "::view-transition-group(*)";
        "::view-transition-image-pair(*)";
        "::view-transition-old(*)";
        "::view-transition-new(*)";
      ],
      [ ":only-child" ] );
    (* [:window-inactive] is about the window, so both engines take it after a
       shadow part. The rows stop where the engines agree: only WebKit takes the
       other ten after [::part()], and only Chrome takes [:window-inactive]
       after [::details-content]. *)
    ([ "::part(tab)" ], element_backed_pseudo_classes @ [ ":window-inactive" ]);
    ([ "::details-content" ], element_backed_pseudo_classes);
    (* Names no shipping engine knows: cascade keeps all of these for forward
       compatibility and cannot know their rules, so it keeps taking any
       pseudo-class after them. *)
    ( [
        "::-moz-placeholder";
        "::-ms-input-placeholder";
        "::future-pseudo-element";
        "::foo(bar)";
        "::deep";
        "::v-deep";
        "::ng-deep";
      ],
      probe_pseudo_classes );
  ]

(* CSS Selectors 4 sec. 3.6.3: "Certain pseudo-elements may be immediately
   followed by any combination of certain pseudo-classes [...] Combinations that
   are not explicitly allowed are invalid selectors." Which combinations those
   are is per pseudo-element, so one predicate over all of them cannot answer
   it: Chrome 151 and WebKit 26.5 keep [::file-selector-button:hover] and drop
   [::before:hover], and agree on every row below. *)
let pseudo_element_pseudo_classes () =
  let concat = String.concat "" in
  let parses input =
    match Cursor.option read (Cursor.of_string input) with
    | Some _ -> ()
    | None -> Alcotest.failf "selector should parse: %s" input
  in
  List.iter
    (fun (spellings, allowed) ->
      List.iter
        (fun pe ->
          List.iter
            (fun pc ->
              let sel = concat [ ".a"; pe; pc ] in
              if List.mem pc allowed then parses sel else neg_cursor read sel)
            probe_pseudo_classes;
          (* Sec. 3.6.3 allows the logical combinations after any pseudo-element
             and passes the row on to their arguments; what an argument the row
             rules out costs is then the argument list's own business. [:is()]
             and [:where()] take a [<forgiving-selector-list>] (sec. 4.1), which
             drops it and leaves a selector that still parses and matches
             nothing, so both engines keep the rule whatever the row says. *)
          List.iter
            (fun pc -> parses (concat [ ".a"; pe; pc ]))
            [ ":is(.b)"; ":where(.b)"; ":is(:hover)"; ":not(:is(.b))" ];
          (* [:not()] takes an unforgiving list, so it goes down with an
             argument the row rules out. *)
          List.iter
            (fun pc ->
              let sel = concat [ ".a"; pe; ":not("; pc; ")" ] in
              if List.mem pc allowed then parses sel else neg_cursor read sel)
            probe_pseudo_classes;
          (* Only a pseudo-class may follow a pseudo-element at all, so a
             [:not()] holding anything else goes down with it too - except after
             a name cascade does not recognise, whose row takes every probe
             because cascade knows none of its rules. *)
          let knows_nothing =
            List.for_all (fun pc -> List.mem pc allowed) probe_pseudo_classes
          in
          let sel = concat [ ".a"; pe; ":not(.b)" ] in
          if knows_nothing then parses sel else neg_cursor read sel)
        spellings)
    pseudo_element_pseudo_class_rows;
  (* The row reaches all the way down a nest of unforgiving lists, and stops at
     the first forgiving one. *)
  parses ".a::part(tab):not(:not(:hover))";
  parses ".a::-webkit-scrollbar:not(:not(:hover))";
  parses ".a::part(tab):not(:where(.b))";
  neg_cursor read ".a::part(tab):not(:not(.b))";
  neg_cursor read ".a::-webkit-scrollbar:not(:not(:focus))";
  neg_cursor read ".a::before:not(:not(:hover))";
  neg_cursor read ".a::marker:not(:not(:hover))";
  (* A whole complex selector never fits where a pseudo-class goes. *)
  neg_cursor read ".a::part(tab):not(div>.b)";
  (* No pseudo-element is left to a default: every spelling the compound guard
     covers names its own row. *)
  let covered pe =
    List.exists
      (fun (spellings, _) -> List.mem pe spellings)
      pseudo_element_pseudo_class_rows
  in
  List.iter
    (fun pe ->
      Alcotest.(check bool)
        (String.concat "" [ "pseudo-class row for "; pe ])
        true (covered pe))
    pseudo_element_spellings

(* The names sec. 3.6.5 and sec. 3.6.4 used to miss, because cascade carried
   them as raw idents and both rules exempt a name cascade does not model.

   WebVTT 1 (Editor's Draft, w3c.github.io/webvtt/, 20 May 2026) sec. 8.2: "A
   CSS user agent that implements the text tracks model must implement the
   ::cue, ::cue(selector), ::cue-region and ::cue-region(selector)
   pseudo-elements". Sec. 8.2.1 and sec. 8.2.3 define the argument-less forms
   ("The ::cue pseudo-element (with no argument) matches any list of WebVTT Node
   Objects", "The ::cue-region pseudo-element (with no argument) matches any
   list of WebVTT region objects"); the W3C Candidate Recommendation Draft of
   the same date carries the same three section numbers. Chrome 151, WebKit 26.5
   and Lightning CSS all take [::cue]; Chrome and WebKit implement neither form
   of [::cue-region], so Lightning CSS is the oracle for that one.

   No specification defines the scrollbar parts. WebKit's "Styling Scrollbars"
   introduces the seven names together and nothing else covers them, so the
   oracle is browsers and independent parsers alone: all three take every part
   bare, and all three drop a combinator or a pseudo-element after it.

   Reading a name gives it a constructor, and a constructor prints its own
   spelling, so an uppercase source folds to lower case where a raw ident kept
   it. *)
let modelled_raw_ident_pseudo_elements () =
  let parses input =
    let c = Cursor.of_string input in
    match read c with
    | exception Error.Parse_error _ ->
        Alcotest.failf "selector should parse: %s" input
    | _ ->
        if Stdlib.not (Cursor.is_done c) then
          Alcotest.failf "selector only partly read: %s" input
  in
  (* Sec. 3.6.5: no combinator after any of the eight. *)
  neg_cursor read ".a::cue .b";
  neg_cursor read ".a::cue > .b";
  neg_cursor read ".a::cue-region .b";
  neg_cursor read ".a::cue-region + .b";
  neg_cursor read ".a::-webkit-scrollbar-button .b";
  neg_cursor read ".a::-webkit-scrollbar-track > .b";
  neg_cursor read ".a::-webkit-scrollbar-track-piece ~ .b";
  neg_cursor read ".a::-webkit-scrollbar-thumb .b";
  neg_cursor read ".a::-webkit-scrollbar-corner .b";
  neg_cursor read ".a::-webkit-resizer .b";
  (* Sec. 3.6.4: none of the eight is defined to take a sub-pseudo-element. The
     first two rows are where cascade differed from all three engines. *)
  neg_cursor read "::cue::before";
  neg_cursor read "::cue::marker";
  neg_cursor read "::cue::after";
  neg_cursor read "::cue::cue";
  neg_cursor read "::cue-region::before";
  neg_cursor read "::-webkit-scrollbar-thumb::before";
  neg_cursor read "::-webkit-resizer::before";
  (* Sec. 3.6.3, per the rows above: a cue takes the user-action pseudo-classes,
     a scrollbar part its own states. *)
  parses ".a::cue:hover";
  parses ".a::cue:focus-within";
  parses ".a::cue-region:active";
  neg_cursor read ".a::cue:enabled";
  neg_cursor read ".a::cue:lang(en)";
  neg_cursor read ".a::cue-region:first-child";
  parses ".a::-webkit-scrollbar-thumb:enabled";
  parses ".a::-webkit-scrollbar-thumb:window-inactive";
  parses ".a::-webkit-resizer:vertical";
  neg_cursor read ".a::-webkit-scrollbar-thumb:focus";
  (* The functional forms are the ones cascade already modelled, and their rows
     are untouched: [::cue()] takes no pseudo-class and [::cue-region()] takes
     any, both as before. *)
  check_minified_to "::cue(v)" "::cue(v)";
  check_minified_to "::cue-region(v)" "::cue-region(v)";
  check_minified_to "::cue(v[voice=active])" "::cue(v[voice='active'])";
  parses ".a::cue-region(v):hover";
  neg_cursor read ".a::cue(v):hover";
  neg_cursor read ".a::cue(v) .b";
  neg_cursor read ".a::cue-region(v) > .b";
  neg_cursor read "::cue()";
  neg_cursor read "::cue-region()";
  (* Chrome and WebKit take none of the eight with one colon, so cascade reads
     them off the [::] table alone and a single colon stays a parse error. *)
  neg_cursor read ".a:cue";
  neg_cursor read ".a:cue-region";
  neg_cursor read ".a:-webkit-scrollbar-thumb";
  neg_cursor read ".a:-webkit-resizer";
  (* CSS Values 4 sec. 4.1: the name is case-insensitive, and a constructor
     prints the canonical lower-case spelling. *)
  check_minified_to "::cue" "::CUE";
  check_minified_to "::cue-region" "::Cue-Region";
  check_minified_to "::-webkit-scrollbar-thumb" "::-WEBKIT-SCROLLBAR-THUMB";
  check_minified_to "::-webkit-resizer" "::-WebKit-Resizer";
  check_minified_to "::cue(v)" "::CUE(v)";
  (* The exemption the interop corpus rests on is untouched: a name cascade does
     not model still takes a combinator, a sub-pseudo-element and any
     pseudo-class after it, and still keeps the source's case. *)
  parses ".foo ::deep .bar";
  parses ".foo ::unknown(.foo) .bar";
  parses ".foo ::v-deep .bar";
  parses ".foo ::ng-deep .bar";
  parses ".a::deep > .b";
  parses ".a::deep::before";
  parses ".a::foo::before::marker";
  parses ".a::deep:first-child";
  parses ".a::unknown(.foo):lang(en)";
  check_minified_to "::DEEP" "::DEEP";
  check_minified_to "::Foo(bar)" "::Foo(bar)"

(* [canonicalize] splices a single-argument [:is(s)] into the compound around
   it, since [:is(s)] matches [s] with the same specificity (Selectors 4 sec.
   4.2). A pseudo-compound is [<pseudo-element-selector>
   <pseudo-class-selector>*] (sec. 16) and which pseudo-classes it takes is per
   pseudo-element (sec. 3.6.3), so the splice would build a compound cascade's
   own reader rejects. Chrome 151 and WebKit 26.5 keep every input below and
   drop [.a::before.b] and [.a::before:hover]. *)
let canonicalize_pseudo_compound_is () =
  let canon expected input =
    let actual = to_string ~minify:true (canonicalize (of_string input)) in
    Alcotest.(check string) ("canonicalize " ^ input) expected actual;
    match Cursor.option read (Cursor.of_string actual) with
    | Some _ -> ()
    | None -> Alcotest.failf "canonicalized selector does not parse: %s" actual
  in
  (* The wrapper stays in a pseudo-compound. *)
  canon ".a:before:is(.b)" ".a::before:is(.b)";
  canon ".a:before:is(#c)" ".a::before:is(#c)";
  canon ".a:before:is([d])" ".a::before:is([d])";
  canon ".a:before:is(div)" ".a::before:is(div)";
  canon ".a:before:is(.b.c)" ".a::before:is(.b.c)";
  canon ".a:before:is(:hover)" ".a::before:is(:hover)";
  canon ".a:before:is(.b)" ".a:before:is(.b)";
  canon "div .a:before:is(.b)" "div .a::before:is(.b)";
  (* [::part()] takes the user action pseudo-classes, so that one splices. *)
  canon ".a::part(p):hover" ".a::part(p):is(:hover)";
  (* Outside a pseudo-compound the splice stands. *)
  canon ".a.b" ".a:is(.b)";
  canon "div>.b" "div>:is(.b)";
  canon ".b" ":is(.b)";
  canon ".x .a" ".x :is(:is(.a))";
  (* [:where()] never unwraps: it contributes zero specificity. *)
  canon ".a:before:where(.b)" ".a::before:where(.b)"

(* Selectors 4 sec. 4.2 replaces the specificity of [:is()] with that of its
   most specific argument, and its own note contrasts [:is(ul, ol, .list) >
   [hidden]] with the split list to show the two do not agree. They agree
   exactly when the arguments already share one specificity, and then a
   whole-rule [:is(s1, ..., sn)] is the selector list [s1, ..., sn]. The split
   has to reach the AST: everything downstream that groups rules compares
   selector nodes. *)
let canonicalize_top_level_is_unwrap () =
  let canon expected input =
    let actual = canonicalize (of_string input) in
    Alcotest.(check string)
      ("canonicalize " ^ input) expected
      (to_string ~minify:true actual);
    Alcotest.(check bool)
      (input ^ " canonicalizes to the AST of " ^ expected)
      true
      (equal actual (canonicalize (of_string expected)));
    match Cursor.option read (Cursor.of_string expected) with
    | Some _ -> ()
    | None ->
        Alcotest.failf "canonicalized selector does not parse: %s" expected
  in
  let distinct a b =
    Alcotest.(check bool)
      (a ^ " and " ^ b ^ " are different selectors")
      false
      (equal (canonicalize (of_string a)) (canonicalize (of_string b)))
  in
  canon "a,b" ":is(a,b)";
  canon ".a,.b" ":is(.a,.b)";
  (* Split into a list that already had members, the alternatives are one
     unordered set again (sec. 4.1: an element matches a selector list when it
     matches any of its selectors). *)
  canon "a,m,z" ":is(a,z),m";
  canon "a,m,z" "a,z,m";
  (* A lone argument shares its own specificity, so the equality holds whatever
     it is. [canonicalize_is] still holds a type or universal one back, because
     it cannot see the compound it would land in; at the top of a rule selector
     there is none. *)
  canon "a" ":is(a)";
  canon "*" ":is(*)";
  canon ".x" ":is(.x)";
  canon "a" ":is(a,a)";
  (* Unequal specificity: an [a] match weighs (0,0,1) as a list branch and
     (0,1,0) under the wrapper. *)
  canon ":is(.x,a)" ":is(.x,a)";
  canon ":is(#i,a)" ":is(#i,a)";
  distinct ":is(.x,a)" ".x,a";
  distinct ":is(#i,a)" "#i,a";
  (* Sec. 4.4: neither [:where()] nor any of its arguments contribute
     specificity, so it never becomes a list, not even one its arguments agree
     on. *)
  canon ":where(.x,a)" ":where(.x,a)";
  canon ":where(a,b)" ":where(a,b)";
  distinct ":where(a,b)" "a,b";
  distinct ":where(.x,a)" ".x,a";
  (* Not the whole selector: there is no enclosing list to split into. *)
  canon "p:is(.x,a)" "p:is(.x,a)";
  canon ":is(.x,a) .z" ":is(.x,a) .z";
  canon "p:is(a,b)" "p:is(a,b)";
  canon ":is(a,b) .z" ":is(a,b) .z";
  (* CSS Nesting 1 sec. 3 weighs [&] as the most specific selector in the parent
     rule, a weight the selector text alone does not name, so an argument
     holding one never enters the equality test. Split, [&, *] would weigh a
     universal match at zero where the wrapper weighs it at the parent
     specificity. *)
  canon ":is(&,*)" ":is(&,*)";
  canon ":is(&,.b)" ":is(&,.b)";
  distinct ":is(&,*)" "&,*"

(* WebKit, "Styling Scrollbars" defines eleven pseudo-classes for the state a
   scrollbar part is in. They are not in any spec, and Chrome 151 and WebKit
   26.5 both read them wherever a pseudo-class goes, so an unforgiving selector
   list holding one keeps its rule. *)
let scrollbar_state_pseudo_classes_read () =
  let concat = String.concat "" in
  let round_trips input =
    match Cursor.option read (Cursor.of_string input) with
    | None -> Alcotest.failf "selector should parse: %s" input
    | Some sel ->
        Alcotest.(check string)
          ("round trip " ^ input) input
          (to_string ~minify:true sel)
  in
  List.iter
    (fun pc ->
      (* On a plain element, where both engines read them. *)
      round_trips (concat [ ".a"; pc ]);
      (* And in the argument lists, forgiving and unforgiving alike. *)
      round_trips (concat [ ".a:not("; pc; ")" ]);
      round_trips (concat [ ".a:is("; pc; ")" ]);
      round_trips (concat [ ".a:has("; pc; ")" ]);
      (* Selectors 4 sec. 17: a pseudo-class weighs a class. *)
      Alcotest.(check int)
        ("specificity " ^ pc) 1 (specificity (of_string pc)).classes;
      (* None of them is functional. *)
      neg_cursor read (concat [ ".a"; pc; "(1)" ]))
    scrollbar_state_pseudo_classes;
  (* A scrollbar part reads any combination of them. *)
  round_trips "::-webkit-scrollbar:vertical:hover";
  round_trips "::-webkit-scrollbar:corner-present:window-inactive";
  round_trips "::-webkit-scrollbar-button:start:decrement"

let parse_errors_nesting_depth () =
  (* A pathologically deep functional-pseudo-class nest is capped rather than
     driving the per-level selector validation into super-linear time (a
     parse-time DoS on untrusted CSS). [:not] is non-forgiving, so the cap
     surfaces as a [Parse_error]; a nest within the cap still parses. *)
  let nested fn n =
    let rep s = String.concat "" (List.init n (fun _ -> s)) in
    ".x" ^ rep (fn ^ "(") ^ "a" ^ rep ")"
  in
  check_parse_error (nested ":not" 200) "nesting too deep";
  List.iter
    (fun s ->
      match Cursor.option read (Cursor.of_string s) with
      | Some _ -> ()
      | None ->
          Alcotest.failf "selector within the nesting cap should parse: %s" s)
    [ nested ":not" 8; nested ":is" 8 ]

let parse_errors_empty_list () =
  check_parse_error ", ," "expected at least one selector";
  check_parse_error ", h1, h2" "expected at least one selector";
  check_parse_error "h1, h2," "expected at least one selector"

let parse_errors_complex () =
  (* Unterminated [...] auto-close at EOF per CSS Syntax 5.3.7, so
     bare-unterminated tests are removed. This one still fails because the
     attribute value contains a selector component after the string. *)
  check_parse_error ".parent > [data-id=\"test\" .child:hover" "trailing tokens"

(* Helpers for callstack accuracy checks to reduce nesting *)
let matches_literal haystack needle =
  let re = Re.(compile (str needle)) in
  Re.execp re haystack

let check_callstack name input expected_stack_parts =
  let t = Cursor.of_string input in
  try
    let _ = read t in
    Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
  with
  | Cursor.Parse_error err ->
      let callstack_str = String.concat " -> " err.path in
      List.iter
        (fun stack_item ->
          if Bool.not @@ matches_literal callstack_str stack_item then
            Alcotest.failf "%s: expected callstack containing '%s' but got '%s'"
              name stack_item callstack_str)
        expected_stack_parts;
      if err.loc.start_pos < 0 then
        Alcotest.failf "%s: position should be >= 0 but got %d" name
          err.loc.start_pos;
      if err.loc.start_pos > err.loc.end_pos then
        Alcotest.failf "%s: loc span should be non-empty but got [%d,%d]" name
          err.loc.start_pos err.loc.end_pos
  | exn ->
      Alcotest.failf "%s: expected Parse_error but got %s" name
        (Printexc.to_string exn)

let check_full_css_callstack name css_input expected_stack_parts =
  match Css.of_string ~strict:true css_input with
  | Ok _ -> Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
  | Error err ->
      let callstack_str = String.concat " -> " err.Cascade.Error.path in
      List.iter
        (fun stack_item ->
          if Bool.not @@ matches_literal callstack_str stack_item then
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

  check_full_css_callstack "full_css_selector_error"
    ".test[[attr]] { color: red; }" [ "rule"; "selector" ];
  check_full_css_callstack "full_css_pseudo_error" ".test:not() { color: red; }"
    [ "rule"; "selector" ];

  (* Escaped characters like \! are valid in CSS - they represent the literal
     character. So .test\!class is equivalent to .test!class which is a valid
     class selector. Testing an actually invalid selector instead: *)
  check_full_css_callstack "invalid_selector_error"
    ".test[[attr]] { color: red; }"
    [ "rule"; "selector"; "list" ]

(* Test check functions for selector components *)
let component_parsing () =
  (* Per CSS Selectors 4 section 14 the printer canonicalizes [<an+b>] to the
     shortest spec-equivalent spelling. *)
  check_nth ~expected:"odd" "2n+1";
  check_nth "odd";
  check_nth ~expected:"2n" "even";
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
    check_value_cursor "attribute_match" read_attribute_match pp_attribute_match
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
  neg_cursor ~allow_partial:true read_attribute_match "%=invalid";
  (* Invalid operator *)
  neg_cursor ~allow_partial:true read_attribute_match "!=not-equal";
  (* Not supported *)
  neg_cursor read_attribute_match "=";
  (* Missing value *)
  neg_cursor read_attribute_match "~=" (* Missing value *)

(* Not a roundtrip test *)
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
  check_needs_quoting "--custom" false;
  (* CSS Syntax allows [--] to start an identifier. *)
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
  let check_attr_flag =
    check_value_cursor "attr_flag" read_attr_flag pp_attr_flag
  in

  (* Case insensitive flag *)
  check_attr_flag ~expected:" i" "i";
  check_attr_flag ~expected:" i" "I";
  (* Case sensitive flag *)
  check_attr_flag ~expected:" s" "s";
  check_attr_flag ~expected:" s" "S";
  (* No flag / empty should return None *)
  check_attr_flag "";

  (* [read_attr_flag] returns an option and rewinds, so its rejection is [None]:
     [x] is not a flag, and [is] is one ident rather than the flag [i] followed
     by anything. *)
  none_cursor read_attr_flag "x";
  none_cursor read_attr_flag "is"

(* Not a roundtrip test *)
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
         | Some Insensitive -> "i"
         | Some Sensitive -> "s"))
      expected output
  in

  (* Without flag *)
  check_selector_with_flag "A" None "[type=A]";
  check_selector_with_flag "a" None "[type=a]";
  check_selector_with_flag "I" None "[type=I]";
  check_selector_with_flag "i" None "[type=i]";

  (* Uppercase input minifies to lowercase flags. *)
  check_selector_with_flag "A" (Some Insensitive) "[type=A i]";
  check_selector_with_flag "a" (Some Insensitive) "[type=a i]";
  check_selector_with_flag "foo" (Some Insensitive) "[type=foo i]";
  check ~expected:"[type=foo i]" "[type=foo I]";

  (* With case-sensitive flag (s) *)
  check_selector_with_flag "A" (Some Sensitive) "[type=A s]";
  check_selector_with_flag "a" (Some Sensitive) "[type=a s]";
  check_selector_with_flag "I" (Some Sensitive) "[type=I s]";
  check_selector_with_flag "i" (Some Sensitive) "[type=i s]";
  check_selector_with_flag "foo" (Some Sensitive) "[type=foo s]";
  check ~expected:"[type=foo s]" "[type=foo S]";

  (* Values with spaces should be quoted regardless of flag *)
  let sel = S.attribute ~flag:Sensitive "type" (Exact "A B") in
  let output = Css.Pp.to_string ~minify:true S.pp sel in
  Alcotest.(check string)
    "value with space and s flag" "[type=\"A B\" s]" output;

  let sel = S.attribute ~flag:Insensitive "type" (Exact "foo bar") in
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

  (* [!] and the empty input are refused outright. *)
  neg_cursor read_combinator "!";
  neg_cursor read_combinator "";
  (* A character that starts a compound is not a combinator, and the reader says
     so by returning the implicit descendant without taking it, leaving it for
     the caller to read as the next compound. *)
  neg_cursor ~allow_partial:true read_combinator "&";
  neg_cursor ~allow_partial:true read_combinator "#"

let test_ns () =
  (* Test namespace type *)
  check_ns "svg|";
  check_ns "xml|";
  check_ns "*|";

  (* [read_ns] returns an option and rewinds rather than raising, so its
     rejection is [None] and not leftover input. *)
  none_cursor read_ns "|";
  none_cursor read_ns "||";
  none_cursor read_ns "svg";
  none_cursor read_ns "svg||";
  none_cursor read_ns "*||";

  (* Test cases that should return None (no namespace found) *)
  none_cursor read_ns "notanamespace";
  none_cursor read_ns "incomplete";
  none_cursor read_ns "";

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
  check "|div";
  check "|*";
  ()

(* CSS Selectors 4 sec. 15.2 makes [||] the column combinator and sec. 6.1 makes
   [ns|name] a namespaced name, so a pipe that opens a [||] never separates a
   prefix from a name. *)
let column_combinator_vs_namespace () =
  check ~roundtrip:true "svg||td";
  check ~roundtrip:true "*||td";
  check ~roundtrip:true "a||b";
  check ~roundtrip:true "svg|table||svg|td";
  check ~expected:"a||b" "a || b";
  check_minified_to "svg||td" "svg || td";
  check_minified_to "*||td" "* || td";

  (* The namespace forms a column combinator must leave alone. *)
  check "svg|td";
  check "*|a";
  check "|a";
  check "svg|a[*|href]"

let test_nth () =
  (* Test nth type -- odd/even are canonicalized to 2n+1/2n *)
  Alcotest.(check bool)
    "odd parses to Odd AST" true
    (read_nth (Cursor.of_string "odd") = Odd);
  Alcotest.(check bool)
    "even parses to Even AST" true
    (read_nth (Cursor.of_string "even") = Even);
  (* Per CSS Selectors 4 section 14 the printer canonicalizes [<an+b>] to the
     shortest spec-equivalent spelling under minify. *)
  check_nth ~expected:"odd" "2n+1";
  check_nth "odd";
  check_nth ~expected:"2n" "even";
  check_nth "3n";
  check_nth "5";

  (* CSS Syntax Level 3 section 6 An+B examples. *)
  check_nth ~expected:"2n" "2n+0";
  check_nth "4n+1";
  check_nth "-n+6";
  check_nth "-4n+10";
  check_nth ~expected:"5" "0n+5";
  check_nth "5";
  check_nth ~expected:"n" "1n+0";
  check_nth ~expected:"n" "+n";
  check_nth ~expected:"n" "n+0";
  check_nth "n";
  check_nth "-n";
  check_nth "n-1";
  check_nth "-n-1";
  check_nth "2n";
  check_nth ~expected:"2n-1" "+2n-1";
  check_nth "-2n-1";
  check_nth "3n-6";
  check_nth ~expected:"3n+1" "3n + 1";
  check_nth ~expected:"3n-2" "+3n - 2";
  check_nth ~expected:"-n+6" "-n+ 6";
  check_nth ~expected:"n-6" "n - 6";
  check_nth ~expected:"6" "+6";

  (* The coefficient and offset are integer tokens, not the rounded float the
     lexer also carries for number-valued grammars. *)
  check_nth "9007199254740993";
  check_nth "9007199254740993n+9007199254740993";

  (* Test invalid nth values *)
  (* Nothing here is an [<an+b>], so the reader refuses each outright. *)
  List.iter (neg_cursor read_nth)
    [ "invalid"; ""; "+ 2n"; "+ 2"; "3n + -6"; "n+"; "n+-1"; "2n--1"; "evenn" ];
  (* [read_nth] reads the [<an+b>] that is there and stops, so on [2 n] it takes
     [2] and on [odd+1] it takes [odd], leaving the rest. Consuming the whole
     argument is the caller's rule, so the rejection is pinned where it is
     enforced. *)
  List.iter (neg_cursor read)
    [ ":nth-child(2 n)"; ":nth-child(3 n)"; ":nth-child(odd+1)" ];
  List.iter (neg_cursor read_nth)
    [
      "999999999999999999999999999999999999";
      "999999999999999999999999999999999999n+1";
    ];
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
  neg_cursor read "123invalid";
  (* Can't start with digit *)
  neg_cursor read "";
  (* Empty selector *)
  neg_cursor read ".";
  (* Incomplete class *)
  neg_cursor read "#";
  (* Incomplete id *)
  neg_cursor read "[";
  (* Incomplete attribute *)
  neg_cursor read ":";
  (* Incomplete pseudo *)
  neg_cursor read "::";
  (* Incomplete pseudo-element *)
  neg_cursor read "...invalid";
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

let test_aria_attr () =
  check_aria_attr "aria-busy";
  check_aria_attr "aria-checked";
  check_aria_attr "aria-disabled";
  check_aria_attr "aria-expanded";
  check_aria_attr "aria-hidden";
  check_aria_attr "aria-pressed";
  check_aria_attr "aria-readonly";
  check_aria_attr "aria-required";
  check_aria_attr "aria-selected";
  neg_cursor read_aria_attr "aria-custom";
  neg_cursor read_aria_attr "not-aria"

let test_attr_name () =
  check_attr_name "aria-busy";
  check_attr_name "aria-custom";
  check_attr_name "data-testid";
  check_attr_name "href"

(* A table keyed by a selector separates its keys only if the hash reads far
   enough into one to reach what distinguishes it. [Combined] nests to the
   right, so the class that tells [.p0 ... .p7 .t1] from [.p0 ... .p7 .t2] is
   the deepest node in both, behind a prefix they share: a hash that stops after
   a fixed count of nodes returns one value for the whole family, and every
   probe of such a table then walks the whole bucket comparing selector subtrees
   down the shared chain. *)
let hash_separates_deep_tails () =
  let depth = 8 and n = 500 in
  let prefix = String.concat " " (List.init depth (Fmt.str ".p%d")) in
  let seen = Hashtbl.create 1024 in
  for i = 1 to n do
    Hashtbl.replace seen
      (Fmt.kstr (fun src -> hash (of_string src)) "%s .t%d" prefix i)
      ()
  done;
  Alcotest.(check int)
    (Fmt.str "%d selectors sharing a %d-deep prefix take distinct hashes" n
       depth)
    n (Hashtbl.length seen)

(* [hash] is consistent with [equal], and [equal] is the structural order's
   zero: both hold across shapes that put what distinguishes them at every depth
   a selector can. Each source is read twice, so the two sides are separate
   trees and the hash is read off the structure, not off a shared pointer. *)
let hash_agrees_with_equal () =
  let sources =
    [
      ".a";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6 .p7 .t1";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6 .p7 .t2";
      ".p0>.p1>.p2>.p3>.p4>.p5>.p6>.t1";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6 .p7 .t1:hover";
      "div.p0 .p1 .p2 .p3 .p4 .p5 .p6 span.t1";
      ":is(.p0 .p1 .p2 .p3 .p4 .p5 .t1)";
      ":is(.p0 .p1 .p2 .p3 .p4 .p5 .t2)";
      ":where(.p0 .p1 .p2 .p3 .p4 .p5 .t1)";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6[data-x=one]";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6[data-x=two]";
      ".p0 .p1 .p2 .p3 .p4 .p5 .p6::before";
    ]
  in
  List.iter
    (fun src_a ->
      List.iter
        (fun src_b ->
          let a = of_string src_a and b = of_string src_b in
          let eq = equal a b in
          if eq <> (compare a b = 0) then
            Alcotest.failf "equal disagrees with compare on %s / %s" src_a src_b;
          if eq <> String.equal src_a src_b then
            Alcotest.failf "equal conflates %s with %s" src_a src_b;
          if eq then
            if hash a <> hash b then
              Alcotest.failf "equal selectors hash apart: %s" src_a)
        sources)
    sources

(** {2 CSS Nesting Selector Tests} *)

(* ignore-test *)
let test_nesting_selector () =
  (* Basic & nesting selector *)
  check "&";
  check_construct "&" Nesting;

  (* & with descendant: & .child *)
  check "& .child";
  check_construct "& .child" (combine Nesting Descendant (class_ "child"));

  (* & with pseudo-class: &:hover *)
  check "&:hover";
  check_construct "&:hover" (compound [ Nesting; Hover ]);

  (* & with pseudo-class: &:focus *)
  check "&:focus";
  check_construct "&:focus" (compound [ Nesting; Focus ]);

  (* & with child combinator: & > .child *)
  check ~expected:"&>.child" "& > .child";
  check_construct "&>.child" (combine Nesting Child (class_ "child"));

  (* & with class: &.active *)
  check "&.active";
  check_construct "&.active" (compound [ Nesting; class_ "active" ]);

  (* & with attribute: &[data-active] *)
  check "&[data-active]";
  check_construct "&[data-active]"
    (compound [ Nesting; attribute "data-active" Presence ]);

  (* & in selector list: pp holds the authored branch order in both modes; only
     optimize sorts into canonical order (see test_stylesheet). *)
  check ~expected:"&:hover,&:focus" "&:hover,&:focus";
  check ~expected:"&:hover,&:focus" "&:hover, &:focus";
  check_pretty_to "&:hover, &:focus" "&:hover, &:focus"

(* ignore-test *)
let test_spec_selector_specificity () =
  let check_specificity name input ids classes elements =
    let actual = specificity (of_string input) in
    Alcotest.(check int) (name ^ " ids") ids actual.ids;
    Alcotest.(check int) (name ^ " classes") classes actual.classes;
    Alcotest.(check int) (name ^ " elements") elements actual.elements
  in
  check_specificity "type selector" "div" 0 0 1;
  check_specificity "class selector" ".item" 0 1 0;
  check_specificity "id selector" "#main" 1 0 0;
  check_specificity "compound selector" "main#app.card[data-x]:hover" 1 3 1;
  check_specificity "descendant selector" "article .card > h2" 0 1 2;
  check_specificity "where zero" ":where(#app,.card,main)" 0 0 0;
  check_specificity "empty where zero" ":where()" 0 0 0;
  check_specificity "is takes max" ":is(.card,#app,main)" 1 0 0;
  check_specificity "empty is zero" ":is()" 0 0 0;
  check_specificity "not takes max" ":not(.card,#app,main)" 1 0 0;
  check_specificity "has takes max" ".card:has(> img.selected)" 0 2 1;
  check_specificity "nth child with selector list" ":nth-child(2n of .a,#b)" 1 1
    0;
  check_specificity "selector list takes max" ".a,#b,main section" 1 0 0

let spec_minifier_semantics () =
  let check_specificity name input ids classes elements =
    let minified = to_string ~minify:true (of_string input) in
    let actual = specificity (of_string minified) in
    Alcotest.(check int) (name ^ " ids") ids actual.ids;
    Alcotest.(check int) (name ^ " classes") classes actual.classes;
    Alcotest.(check int) (name ^ " elements") elements actual.elements
  in
  List.iter check_minified_equiv
    [
      "main#app.card[data-x]:hover";
      ":where(nav, main, aside) a:any-link";
      ":is(section,article,aside)>:where(h1,h2)";
      ".card:has(> img.selected)";
      "li:nth-child(2n+1 of .visible:not([hidden]))";
      "[svg|href=\"--icon\" I]";
      ".\\31 0\\/12:hover";
    ];
  check_minified_to ":before" ":before";
  check_minified_to ":before" "::before";
  check_pretty_to ":before" ":before";
  check_minified_to ":after" ":after";
  check_minified_to ":after" "::after";
  check_pretty_to ":after" ":after";
  check_minified_to ":first-letter" ":first-letter";
  check_minified_to ":first-letter" "::first-letter";
  check_pretty_to ":first-letter" ":first-letter";
  check_minified_to ":first-line" ":first-line";
  check_minified_to ":first-line" "::first-line";
  check_pretty_to ":first-line" ":first-line";
  check_minified_to ":is(.valid,#id)" ":is(.valid,::before,:future-pseudo,#id)";
  check_minified_to ":where(.valid,#id)"
    ":where(.valid,::before,:future-pseudo,#id)";
  check_specificity "where stays zero" ":where(#id,.a,main)" 0 0 0;
  check_specificity "is max specificity" ":is(main,.a,#id)" 1 0 0;
  check_specificity "not max specificity" ":not(main,.a,#id)" 1 0 0;
  check_specificity "has max specificity" ".card:has(> img.selected)" 0 2 1;
  check_specificity "nth of list specificity" ":nth-child(2n of .a,#b)" 1 1 0;
  neg_cursor read ".valid,:future-pseudo";
  neg_cursor read ":not(.valid,:future-pseudo)";
  neg_cursor read ".card:has(:has(img))";
  neg_cursor read ".card:has(::before)";
  neg_cursor read ".a::before.class";
  neg_cursor read ".a::before::before"

(* CSS Selectors 4 sec. 7.1 defers directionality to the document language, and
   an element the language gives no directionality matches neither [:dir(ltr)]
   nor [:dir(rtl)], so the CSS text alone never proves the two are a partition.
   [HTML] proves it for a host HTML document: the directionality of an element,
   any element and not just an HTML one, is either 'ltr' or 'rtl'. Default
   minify takes that host fact; [--enforce-spec] drops it and keeps the author's
   [:not()]. *)
let spec_selector_dir_fold_is_a_host_fact () =
  check_minified_to ".a:dir(rtl)" ".a:not(:dir(ltr))";
  check_minified_to ".a:dir(ltr)" ".a:not(:dir(rtl))";
  check_enforce_spec_to ".a:not(:dir(ltr))" ".a:not(:dir(ltr))";
  check_enforce_spec_to ".a:not(:dir(rtl))" ".a:not(:dir(rtl))";
  (* The author's own [:dir()] is a plain serialisation in either mode. *)
  check_minified_to ".a:dir(rtl)" ".a:dir(rtl)";
  check_enforce_spec_to ".a:dir(ltr)" ".a:dir(ltr)";
  (* The gate is on the host fact alone: rewrites the CSS specs prove on their
     own still run. Selectors 4 sec. 8.1 defines [:any-link] as equivalent to
     [:is(:link, :visited)], and sec. 4.3 makes [:not(:not(X))] equivalent to
     [X]. *)
  check_enforce_spec_to ".a:any-link" ".a:is(:link,:visited)";
  check_enforce_spec_to ".a:hover" ".a:not(:not(:hover))"

(* CSS Selectors 4 sec. 7.1: "The argument to :dir() must be a single
   identifier, otherwise the selector is invalid. [...] Values other than ltr
   and rtl are not invalid, but do not match anything." An unrecognised
   directionality is a valid selector that matches no element, so it parses and
   round-trips; a non-identifier argument stays invalid. *)
let spec_selector_dir_argument_is_an_ident () =
  check ":dir(auto)";
  check ":dir(sideways)";
  check ":dir(--custom)";
  check ~expected:":dir(auto)" ":dir( auto )";
  check_minified_to ".a:dir(auto)" ".a:dir(auto)";
  (* [:dir(auto)] matches nothing, so [:not(:dir(auto))] matches everything.
     That is not [:dir(<the other one>)]: the sec. 7.1 fold speaks only for the
     two directionalities the spec names, and must not fire here in either
     mode. *)
  check_minified_to ".a:not(:dir(auto))" ".a:not(:dir(auto))";
  check_enforce_spec_to ".a:not(:dir(auto))" ".a:not(:dir(auto))";
  (* The two directionalities sec. 7.1 does name still fold, which is what makes
     the pair above a contrast rather than a blanket ban. *)
  check_minified_to ".a:dir(rtl)" ".a:not(:dir(ltr))";
  check_enforce_spec_to ".a:not(:dir(ltr))" ".a:not(:dir(ltr))";
  (* "a single identifier, otherwise the selector is invalid": no argument, a
     number, a string, and two idents are all outside the grammar. *)
  neg_cursor read ":dir()";
  neg_cursor read ":dir(1)";
  neg_cursor read ":dir(\"ltr\")";
  neg_cursor read ":dir(ltr, rtl)"

(* CSS Values 4 sec. 4.1: "Keywords are identifiers and are interpreted ASCII
   case-insensitively (i.e., [a-z] and [A-Z] are equivalent)." CSS Selectors 4
   sec. 7.1 names [ltr] and [rtl] as the two directionalities [:dir()] matches,
   so [LTR] is that keyword and reaches the same node. The identifier sec. 7.1
   leaves valid but non-matching is no keyword, so its case is the author's. *)
let spec_selector_dir_keyword_is_case_insensitive () =
  (* The keyword reaches its canonical node, so the sec. 7.1 fold applies. *)
  check_minified_to ".a:dir(rtl)" ".a:not(:dir(LTR))";
  check_minified_to ".a:dir(ltr)" ".a:not(:dir(RTL))";
  check_minified_to ".a:dir(rtl)" ".a:not(:dir(Ltr))";
  (* A positive [:dir()] carries the same keyword, in either mode. *)
  check_minified_to ".a:dir(ltr)" ".a:dir(LTR)";
  check_pretty_to ".a:dir(rtl)" ".a:dir(RTL)";
  (* Case is a fact of the CSS text, so it holds under [--enforce-spec] too.
     What that flag drops is the host partition of sec. 7.1, which is what keeps
     the [:not()] here. *)
  check_enforce_spec_to ".a:not(:dir(ltr))" ".a:not(:dir(LTR))";
  check_enforce_spec_to ".a:dir(ltr)" ".a:dir(LTR)";
  (* An identifier that is neither keyword keeps its case and pairs with no
     directionality, in either mode. *)
  check_minified_to ".a:dir(Auto)" ".a:dir(Auto)";
  check_minified_to ".a:not(:dir(AUTO))" ".a:not(:dir(AUTO))";
  check_enforce_spec_to ".a:not(:dir(Auto))" ".a:not(:dir(Auto))";
  (* The same holds for a node built rather than read. *)
  Alcotest.(check string)
    "an identifier that is neither keyword does not fold" ":not(:dir(auto))"
    (to_string ~minify:true (Not [ Dir "auto" ]))

(* CSS Selectors 4 sec. 12 makes each of these pseudo-class pairs a partition of
   one set of elements only, and an element outside that set matches neither
   half: sec. 12.1.1 "In a typical document most elements will be neither
   :enabled nor :disabled", sec. 12.3.1 "An element which lacks data validity
   semantics is neither :valid nor :invalid [...] a p element has no validity
   semantics at all", sec. 12.3.3 "Elements that are not form elements are
   neither required nor optional". So [:not()] over one half of a pair is the
   other half only inside a compound that proves its subject carries that state,
   and [HTML] names a different set of elements for each pair. *)
let spec_selector_state_folds_need_a_carrier () =
  (* A class names no element type, so [<p class=c>] matches [.c:not(:enabled)]
     and none of the six positive halves. *)
  check_minified_to ".c:not(:enabled)" ".c:not(:enabled)";
  check_minified_to ".c:not(:disabled)" ".c:not(:disabled)";
  check_minified_to ".c:not(:valid)" ".c:not(:valid)";
  check_minified_to ".c:not(:invalid)" ".c:not(:invalid)";
  check_minified_to ".c:not(:required)" ".c:not(:required)";
  check_minified_to ".c:not(:optional)" ".c:not(:optional)";
  (* Nothing at all around the negation proves even less. *)
  check_minified_to ":not(:enabled)" ":not(:enabled)";
  (* An attribute selector names no element type either: [type] is a plain
     attribute on any element. *)
  check_minified_to "[type=text]:not(:required)" "[type=text]:not(:required)";
  (* HTML sec. 4.16.3: [:enabled] matches a non-disabled [button], [input],
     [select], [textarea], [optgroup], [option] or [fieldset], and [:disabled]
     matches an actually disabled one (sec. 4.15). [a] is on neither list. *)
  check_minified_to "a:not(:enabled)" "a:not(:enabled)";
  check_minified_to "input:disabled" "input:not(:enabled)";
  check_minified_to "input:enabled" "input:not(:disabled)";
  check_minified_to "option:enabled" "option:not(:disabled)";
  check_minified_to "fieldset:disabled" "fieldset:not(:enabled)";
  (* HTML sec. 4.16.3: [:valid] and [:invalid] split a [form] and a [fieldset]
     whole, but reach a form control only while it is a candidate for constraint
     validation. A disabled, readonly or [type=hidden] [input] is barred from it
     (HTML sec. 4.10.19.5, sec. 4.10.5.3.3, sec. 4.10.5.1.1), so it matches
     neither half. *)
  check_minified_to "input:not(:invalid)" "input:not(:invalid)";
  check_minified_to "input:not(:valid)" "input:not(:valid)";
  check_minified_to "button:not(:valid)" "button:not(:valid)";
  check_minified_to "form:invalid" "form:not(:valid)";
  check_minified_to "fieldset:valid" "fieldset:not(:invalid)";
  (* HTML sec. 4.16.3: [:optional] wants an [input] "to which the required
     attribute applies". It does not apply in the Hidden or Submit Button state
     (HTML sec. 4.10.5.1.1), so [<input type=hidden>] matches neither
     [:required] nor [:optional], while every [select] and [textarea] carries
     one or the other. *)
  check_minified_to "input:not(:required)" "input:not(:required)";
  check_minified_to "input:not(:optional)" "input:not(:optional)";
  check_minified_to "select:optional" "select:not(:required)";
  check_minified_to "textarea:required" "textarea:not(:optional)";
  (* The three sets are distinct: an [input] proves the enabled pair and neither
     of the other two. *)
  check_minified_to "input:enabled:not(:required)"
    "input:not(:disabled):not(:required)";
  (* Selectors 4 sec. 3.5: the subject of a complex selector is its rightmost
     compound, so an ancestor part neither proves nor blocks the fold. *)
  check_minified_to "form input:disabled" "form input:not(:enabled)";
  check_minified_to "form .c:not(:enabled)" "form .c:not(:enabled)";
  check_minified_to "input.c:disabled" "input.c:not(:enabled)";
  (* Selectors 4 sec. 4.2: [:is()] matches any element one of its branches
     matches, so it proves the state only when every branch does. [:where()]
     matches the same elements. *)
  check_minified_to ":is(select,textarea):optional"
    ":is(select,textarea):not(:required)";
  check_minified_to ":is(input,textarea):not(:required)"
    ":is(input,textarea):not(:required)";
  check_minified_to ":is(input,.c):not(:enabled)" ":is(input,.c):not(:enabled)";
  check_minified_to ":where(input):disabled" ":where(input):not(:enabled)";
  (* Selectors 4 sec. 5: [:has()] constrains the subject's descendants, not its
     element type. *)
  check_minified_to ":has(input):not(:enabled)" ":has(input):not(:enabled)";
  (* Selectors 4 sec. 12.1.1 leaves what counts as an enabled state, a disabled
     state and a user interface element to the host language, so it is [HTML],
     not the CSS text, that puts [input] on the list. [--enforce-spec] drops
     that host fact. *)
  check_enforce_spec_to "input:not(:enabled)" "input:not(:enabled)";
  check_enforce_spec_to "select:not(:required)" "select:not(:required)";
  check_enforce_spec_to "form:not(:valid)" "form:not(:valid)"

(* ignore-test *)
let test_spec_forgiving_selector_lists () =
  (* Selectors Level 4: :is() and :where() use forgiving selector-list parsing.
     Invalid selector branches are dropped before minification; top-level lists,
     :not(), and :has() remain unforgiving. *)
  check_minified_to ":is(.valid,#id)" ":is(.valid,:future-pseudo,#id)";
  check_minified_to ":where(.valid,#id)" ":where(.valid,:future-pseudo,#id)";
  (* Single-argument [:is(.item)] is spec-equivalent to bare [.item] (same match
     set and specificity per Selectors L4 sec. 4.2), but unwrapping it is a node
     change reserved for the optimizer's [Selector.canonicalize]; pp is
     lexical-only and holds the [:is()]. [:where()] holds too (and could not
     unwrap regardless, contributing zero specificity). *)
  check_minified_to ":is(.item)" ":is(, .item)";
  check_minified_to ":where(.item)" ":where(, .item)";
  check_minified_to ":is()" ":is()";
  check_minified_to ":where()" ":where()";
  check_minified_to ":is()" ":is(,)";
  check_minified_to ":where()" ":where(,)";
  check_minified_to ":is()" ":is(:future-pseudo,::before)";
  check_minified_to ":where()" ":where(:future-pseudo,::before)";
  (* A single-argument [:is(s)] only unwraps when [s] is itself a single
     compound. When [s] contains a combinator the [:is()] is a grouping
     boundary: splicing its argument into the surrounding compound would
     re-anchor the combinator and change the match set, so the wrapper stays.
     [.a:is(.b .c)] must NOT become [.a.b .c]. *)
  check_minified_to ".a:is(.b .c)" ".a:is(.b .c)";
  check_minified_to "div:is(.b>.c)" "div:is(.b > .c)";
  check_minified_to ".flex:is(:where(.group):focus *)"
    ".flex:is(:where(.group):focus *)";
  check ":not(.a,#b)";
  neg_cursor read ".a,:future-pseudo";
  neg_cursor read ":not(.a,:future-pseudo)";
  neg_cursor read ".card:has(> img,:future-pseudo)";
  neg_cursor read ":has()"

let spec_selector_current_pseudos () =
  (* Parser coverage; matching needs DOM/UA state. *)
  check ":popover-open";
  check ":modal";
  check ":picture-in-picture";
  check ":fullscreen";
  check ":autofill";
  check ":user-valid";
  check ":user-invalid";
  check ":open";
  check ":state(selected)";
  check ":host";
  check ":host(.active)";
  check ":host-context(.theme-dark)";
  check "::part(tab panel)";
  check "::slotted(img.selected)";
  check "::cue(.warning)";
  check "::cue-region(.speaker)";
  check "::highlight(search)";
  check "::view-transition-group(root)";
  check "::view-transition-image-pair(root)";
  check "::view-transition-old(root)";
  check "::view-transition-new(root)";
  check ":active-view-transition-type(forwards,backwards)";
  check ":playing";
  check ":paused";
  check ":seeking";
  check ":buffering";
  check ":stalled";
  check ":muted";
  check ":volume-locked";
  neg_cursor read ":state()";
  neg_cursor read "::part()";
  neg_cursor read "::slotted()";
  neg_cursor read "::highlight()";
  neg_cursor read ":active-view-transition-type()"

let spec_selector_scope_pseudo_edges () =
  (* Mixed parser/minifier coverage. *)
  check ":scope";
  check_minified_to ":scope>.item" ":scope > .item";
  check_minified_to ":scope+.item" ":scope + .item";
  check_minified_to ".card:has(>img)" ".card:has(> img)";
  check_minified_to ".card:has(+.summary)" ".card:has(+ .summary)";
  check_minified_to ".card:has(~.summary)" ".card:has(~ .summary)";
  check_minified_to "section:has(:scope>h2)" "section:has(:scope > h2)";
  check "article :is(h1,h2,h3):not(.muted)";
  check ":where(nav,main,aside) a:any-link";
  check ~expected:"li:nth-child(odd of.visible:not([hidden]))"
    "li:nth-child(2n+1 of .visible:not([hidden]))";
  check ~expected:"li:nth-last-child(-n+3 of:not([hidden]))"
    "li:nth-last-child(-n+3 of :not([hidden]))";
  check_minified_to "input:not([type],[type=hidden])"
    "input:not([type], [type=hidden])";
  check_minified_to "a:before" "a::before";
  (* Chrome 151 and WebKit 26.5 drop [.a::before:hover], so the legacy fold is
     pinned with the pseudo-class in the position both keep. *)
  check_minified_to ".a:hover:before" ".a:hover::before";
  (* CSS Selectors 4 section 5.2: [*] in a non-solitary compound is redundant,
     but dropping it is a node change reserved for the optimizer's
     [Selector.canonicalize]; pp is lexical-only and holds it. *)
  check_minified_to "::slotted(*:not([hidden]))" "::slotted(*:not([hidden]))";
  check "::selection";
  check "input::file-selector-button";
  neg_cursor read "> .item";
  neg_cursor read "+ .item";
  neg_cursor read "~ .item";
  neg_cursor read ".a::before.class";
  neg_cursor read ".a::before::before";
  neg_cursor read ".a:has(:has(img))";
  neg_cursor read ".a:has(::before)";
  neg_cursor read "div#";
  neg_cursor read ".class#";
  neg_cursor read "[data-x=foo q]"

let spec_selector_l4_pseudo_matrix () =
  (* Parser-surface coverage for Selectors 4 pseudo forms. *)
  List.iter check
    [
      ":any-link";
      ":local-link";
      ":target-within";
      ":defined";
      ":blank";
      ":placeholder-shown";
      ":default";
      ":indeterminate";
      ":scope";
      ":current";
      ":current(.slide)";
      ":past";
      ":future";
      ":nth-col(odd)";
      ":nth-last-col(odd)";
      ":is(section,article,aside)>:where(h1,h2)";
      ":not(:where(.muted,[hidden]))";
      ":not(:is(.muted,[hidden]))";
      ":has(>:is(img, picture, video))";
      ":has(+:is(summary, .summary))";
      ":where(:modal,:popover-open)";
      "dialog:modal::backdrop";
      "::part(tab)";
      "::part(tab panel)";
      "::cue(b)";
      "::highlight(search-results)";
      "::view-transition-group(root)";
    ];
  check ~expected:":nth-child(2n of:is(.item,[data-visible]))"
    ":nth-child(2n of :is(.item,[data-visible]))";
  check ~expected:":nth-last-child(3n+1 of:where(.visible,:not([hidden])))"
    ":nth-last-child(3n+1 of :where(.visible,:not([hidden])))";
  List.iter
    (fun input -> neg_cursor read input)
    [
      ":nth-col()";
      ":nth-last-col(of .item)";
      ":current()";
      ":has(, .item)";
      ":has(+)";
      ":not()";
      ":not(:has())";
      "::part(tab, panel)";
      "::slotted(.a, .b)";
      "::cue()";
      "::view-transition-old()";
    ]

let spec_selector_tree_structural_pseudos () =
  List.iter check
    [
      ":root";
      ":empty";
      ":only-child";
      ":only-of-type";
      ":nth-child(-n+3)";
      ":nth-of-type(3n+1)";
      ":nth-last-of-type(-n+2)";
    ];
  check ~expected:":nth-last-child(odd of:not([hidden]))"
    ":nth-last-child(odd of :not([hidden]))";
  check ~expected:":nth-child(odd of.a,.b)" ":nth-child(2n+1 of.a,.b)";
  List.iter
    (fun input -> neg_cursor read input)
    [
      ":nth-child(n+)";
      ":nth-last-child(2n of)";
      ":nth-of-type(odd even)";
      (* CSS Values 4 sec. 5.7.3: the [of <complex-selector-list>] clause is a
         [#] list, so a trailing comma is invalid. *)
      ":nth-child(2n+1 of .a,.b,)";
    ]

let spec_selector_input_state_pseudos () =
  List.iter check
    [
      ":enabled";
      ":disabled";
      ":read-only";
      ":read-write";
      ":placeholder-shown";
      ":autofill";
      ":default";
      ":checked";
      ":indeterminate";
      ":valid";
      ":invalid";
      ":user-valid";
      ":user-invalid";
      ":in-range";
      ":out-of-range";
      ":required";
      ":optional";
    ];
  List.iter
    (fun input -> neg_cursor read input)
    [ ":checked()"; ":valid(.x)"; ":required(.x)" ]

let spec_selector_pseudo_element_matrix () =
  check ":before";
  check ~expected:":before" "::before";
  check_pretty_to ":before" ":before";
  check ":after";
  check ~expected:":after" "::after";
  check_pretty_to ":after" ":after";
  check ":first-line";
  check ~expected:":first-line" "::first-line";
  check_pretty_to ":first-line" ":first-line";
  check ":first-letter";
  check ~expected:":first-letter" "::first-letter";
  check_pretty_to ":first-letter" ":first-letter";
  List.iter check
    [
      "::marker";
      "::selection";
      "::target-text";
      "::spelling-error";
      "::grammar-error";
      "::file-selector-button";
      "::part(tab panel)";
      "::slotted(img.selected)";
      "::highlight(search-results)";
      "::view-transition-group(root)";
      "::view-transition-image-pair(root)";
      "::view-transition-old(root)";
      "::view-transition-new(root)";
    ];
  check_minified_to "a:before" "a::before";
  check_minified_to "p:first-line" "p::first-line";
  List.iter
    (fun input -> neg_cursor read input)
    [ "::part()"; "::part(a,b)"; "::slotted()"; "::highlight()" ]

let spec_selector_pseudo_manifest () =
  let positive_pseudos =
    [
      ":active";
      ":active-view-transition";
      ":active-view-transition-type(forwards)";
      ":any-link";
      ":autofill";
      ":blank";
      ":buffering";
      ":checked";
      ":current";
      ":current(h1,h2)";
      ":default";
      ":defined";
      ":dir(ltr)";
      ":disabled";
      ":empty";
      ":enabled";
      ":first-child";
      ":first-of-type";
      ":focus";
      ":focus-visible";
      ":focus-within";
      ":fullscreen";
      ":future";
      ":has(>img)";
      ":host";
      ":host(.active)";
      ":host-context(.theme-dark)";
      ":hover";
      ":in-range";
      ":indeterminate";
      ":invalid";
      "#b,.a";
      ":last-child";
      ":last-of-type";
      ":link";
      ":local-link";
      ":modal";
      ":muted";
      ":not(.a,#b)";
      ":nth-col(odd)";
      ":nth-last-child(2n of:not([hidden]))";
      ":nth-last-child(3n+1 of:where(.visible,:not([hidden])))";
      ":nth-last-col(even)";
      ":nth-last-of-type(-n+3)";
      ":nth-of-type(3n)";
      ":only-child";
      ":only-of-type";
      ":open";
      ":optional";
      ":out-of-range";
      ":past";
      ":paused";
      ":picture-in-picture";
      ":placeholder-shown";
      ":playing";
      ":popover-open";
      ":read-only";
      ":read-write";
      ":required";
      ":root";
      ":scope";
      ":seeking";
      ":stalled";
      ":state(selected)";
      ":target";
      ":target-within";
      ":user-invalid";
      ":user-valid";
      ":valid";
      ":visited";
      ":volume-locked";
      ":where(.a,#b)";
      ":where(:modal,:popover-open)";
      ":after";
      ":before";
      "::cue(.warning)";
      "::cue-region(.speaker)";
      "::file-selector-button";
      "::grammar-error";
      "::highlight(search)";
      "::marker";
      "::part(tab panel)";
      "::selection";
      "::slotted(img.selected)";
      "::spelling-error";
      "::target-text";
      "::view-transition-group(root)";
      "::view-transition-image-pair(root)";
      "::view-transition-new(root)";
      "::view-transition-old(root)";
    ]
  in
  let negative_pseudos =
    [
      ":active-view-transition-type()";
      ":dir()";
      ":has()";
      ":host-context()";
      (* CSS Shadow Parts 1 sec. 3.2.3: [:host()] takes one
         [<compound-selector>]. [Cursor.option] swallows a [Parse_error] from an
         unparsable argument and restores the sub-cursor, so [Cursor.call] saw
         it as untouched rather than left with trailing content; a bare digit is
         not a compound selector, so [:host(1)] silently became [Host None]
         (prints as bare [:host]) instead of invalidating the whole selector. *)
      ":host(1)";
      ":lang()";
      ":not()";
      ":nth-child(2n of)";
      ":not(:has())";
      ":nth-col()";
      ":nth-last-col(of .item)";
      ":state()";
      "::cue()";
      "::highlight()";
      "::part()";
      "::part(tab, panel)";
      "::slotted()";
      "::slotted(.a, .b)";
      "::view-transition-old()";
    ]
  in
  List.iter check positive_pseudos;
  check ~expected:":lang(en,fr)" ":lang(en, fr)";
  check ~expected:":nth-child(odd of.item)" ":nth-child(odd of .item)";
  check ~expected:":nth-child(2n of:is(.item,[data-visible]))"
    ":nth-child(2n of :is(.item,[data-visible]))";
  List.iter (fun input -> neg_cursor read input) negative_pseudos

let spec_selector_attr_ns_edges () =
  (* Mixed parser/minifier coverage. *)
  check "|a";
  check "|*";
  check "[|href]";
  check "[*|href]";
  check "[svg|href]";
  check "svg|a[*|href]";
  check "*|a[|href]";
  check ~expected:"[data-x=--value]" "[data-x=\"--value\"]";
  check ~expected:"[data-x=\"1value\"]" "[data-x=\"1value\"]";
  check ~expected:"[data-x=foo\\ bar]" "[data-x=foo\\ bar]";
  check "[data-x=foo i]";
  check ~expected:"[data-x=foo i]" "[data-x=foo I]";
  check "[data-x=foo s]";
  check ~expected:"[data-x=foo s]" "[data-x=foo S]";
  neg_cursor read "[*||href]";
  neg_cursor read "[svg|]";
  neg_cursor read "[data-x=foo z]";
  neg_cursor read "[data-x~=]";
  neg_cursor read "[data-x|=]"

let spec_selector_serialization_invariant_matrix () =
  List.iter check_minified_equiv
    [
      "svg|a[href^=\"https\" i] > :is(img,picture):not([hidden])";
      ":where(main, article, aside) :has(> h2 + p)";
      "section:has(:scope > h2, :scope > h3)";
      "li:nth-child(2n+1 of .item:not([hidden]))";
      "li:nth-last-child(-n+3 of :where(.visible,[data-visible]))";
      "::part(tab panel)";
      "::slotted(*:not([hidden]))";
      "dialog:modal::backdrop";
      ":host(.active) ::slotted(img.selected)";
      ":is(:lang(en, fr), :dir(ltr), :state(selected))";
      ":root > body :where(nav,main,aside) a:any-link";
      "form \
       :is(input:user-invalid,select:user-valid,textarea:placeholder-shown)";
      ":not(:is(.a,#b,[hidden]))";
      ".media:has(> img.selected + figcaption)";
      "article:target-within :focus-visible";
      "video:playing:picture-in-picture";
      "input:read-write:placeholder-shown";
      ":active-view-transition-type(forwards, backwards)";
      "::highlight(search-results)";
      "::cue-region(.speaker)";
    ];
  (* Forgiving parsing drops the invalid branch; the split of what is left is
     [Selector.canonicalize]'s, not pp's. *)
  check_minified_to ":is(.a,.b)" ":is(.a, [=bad], .b)";
  List.iter
    (fun input -> neg_cursor read input)
    [
      ":not(.a, :future-pseudo)";
      ".card:has(:has(img))";
      ".card:has(::before)";
      "li:nth-child(2n+1 of)";
      ":nth-child(of .item)";
      ":nth-last-of-type(odd even)";
      ":has(+)";
      ":lang(, en)";
      "::part(tab, panel)";
      "::slotted(.a, .b)";
      "::cue-region()";
      "::highlight(search, spelling)";
      ":host-context()";
    ]

let test_component_values () =
  check_component_values ~expected:":unknown(.a,.b)" ":unknown(.a, .b)";
  check_component_values ~expected:"foo(1,2)" "foo(1, 2)"

let suite =
  let open Alcotest in
  ( "selector",
    [
      (* Core type tests *)
      test_case "combinator" `Quick test_combinator;
      test_case "ns" `Quick test_ns;
      test_case "column combinator vs namespace" `Quick
        column_combinator_vs_namespace;
      test_case "nth" `Quick test_nth;
      test_case "selector" `Quick test_selector;
      test_case "aria_attr" `Quick test_aria_attr;
      test_case "attr_name" `Quick test_attr_name;
      test_case "component_values" `Quick test_component_values;
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
      (* Parser/serializer coverage. *)
      test_case "roundtrip" `Quick roundtrip;
      test_case "selector component parsing" `Quick component_parsing;
      test_case "attribute match" `Quick test_attribute_match;
      test_case "attr_value_quoting" `Quick test_attr_value_quoting;
      test_case "attr flag" `Quick test_attr_flag;
      test_case "attr_case_sensitivity_flags" `Quick
        test_attr_case_sensitivity_flags;
      test_case "selector component failures" `Quick component_parsing_failures;
      (* Parser/API-policy error coverage. *)
      test_case "of_string empty raises Parse_error" `Quick
        of_string_empty_raises_parse_error;
      test_case "invalid" `Quick invalid;
      test_case "parse errors - attributes" `Quick parse_errors_attributes;
      test_case "parse errors - combinators" `Quick parse_errors_combinators;
      test_case "parse errors - starts" `Quick parse_errors_starts;
      test_case "parse errors - pseudo" `Quick parse_errors_pseudo;
      test_case "pseudo-element compound guard" `Quick
        pseudo_element_compound_guard;
      test_case "logical combinator pseudo-element" `Quick
        logical_combinator_pseudo_element;
      test_case "pseudo-element combinator guard" `Quick
        pseudo_element_combinator_guard;
      test_case "sub-pseudo-element guard" `Quick sub_pseudo_element_guard;
      test_case "pseudo-element pseudo-classes" `Quick
        pseudo_element_pseudo_classes;
      test_case "modelled raw-ident pseudo-elements" `Quick
        modelled_raw_ident_pseudo_elements;
      test_case "scrollbar state pseudo-classes" `Quick
        scrollbar_state_pseudo_classes_read;
      test_case "canonicalize pseudo-compound :is()" `Quick
        canonicalize_pseudo_compound_is;
      test_case "canonicalize top-level :is()" `Quick
        canonicalize_top_level_is_unwrap;
      test_case "parse errors - nesting depth" `Quick parse_errors_nesting_depth;
      test_case "parse errors - empty list" `Quick parse_errors_empty_list;
      test_case "parse errors - complex" `Quick parse_errors_complex;
      test_case "callstack accuracy" `Quick callstack_accuracy;
      (* Local construction helpers. *)
      test_case "complex construction" `Quick test_complex_construction;
      test_case "combinator distribution" `Quick test_combinator_distribution;
      (* Spec/minifier coverage. *)
      test_case "spec selector specificity" `Quick
        test_spec_selector_specificity;
      test_case "spec selector minifier semantics" `Quick
        spec_minifier_semantics;
      test_case "spec selector :dir() fold is a host fact" `Quick
        spec_selector_dir_fold_is_a_host_fact;
      test_case "spec selector :dir() argument is an ident" `Quick
        spec_selector_dir_argument_is_an_ident;
      test_case "spec selector :dir() keyword is case-insensitive" `Quick
        spec_selector_dir_keyword_is_case_insensitive;
      test_case "spec selector state folds need a carrier" `Quick
        spec_selector_state_folds_need_a_carrier;
      test_case "spec forgiving selector lists" `Quick
        test_spec_forgiving_selector_lists;
      test_case "spec selector current pseudo vectors" `Quick
        spec_selector_current_pseudos;
      test_case "spec selector relative/scope/pseudo-element edges" `Quick
        spec_selector_scope_pseudo_edges;
      test_case "spec selector level 4 pseudo matrix edges" `Quick
        spec_selector_l4_pseudo_matrix;
      test_case "spec selector tree structural pseudos" `Quick
        spec_selector_tree_structural_pseudos;
      test_case "spec selector input state pseudos" `Quick
        spec_selector_input_state_pseudos;
      test_case "spec selector pseudo-element matrix" `Quick
        spec_selector_pseudo_element_matrix;
      test_case "spec selector pseudo manifest" `Quick
        spec_selector_pseudo_manifest;
      test_case "spec selector serialization invariant matrix" `Quick
        spec_selector_serialization_invariant_matrix;
      test_case "spec selector attribute namespace edges" `Quick
        spec_selector_attr_ns_edges;
      test_case "hash separates deep tails" `Quick hash_separates_deep_tails;
      test_case "hash agrees with equal" `Quick hash_agrees_with_equal;
      test_case "nesting selector" `Quick test_nesting_selector;
    ] )
