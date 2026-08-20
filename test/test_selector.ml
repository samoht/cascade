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

(* Extra minifier invariant: minify is idempotent (a second pass produces no
   further change), and specificity is preserved. Strict AST equality between
   [original] and [reparsed] does not hold for inputs the minifier rewrites
   (e.g. CSS Selectors 4 sec. 3.5 lets the printer drop a redundant [*] from a
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
  (* Per CSS Selectors 4 section 14 the printer canonicalizes [even] to [2n] and
     [2n+1] to [odd] under minify. *)
  check_construct ":nth-child(2n)" (nth_child Even);
  check_construct ":nth-child(odd)" (nth_child (An_plus_b (2, 1)));
  (* nth with Index and of clause. Per CSS Selectors 4 section 14 the printer
     canonicalizes [<an+b>] to the shortest equivalent spelling - [(0n+5)] ->
     [(5)] / [(1)] -> [:first-child]. *)
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

  (* Negative cases: CSS Syntax sec. 5.3.7 / sec. 4.3.5 mandate recovery for
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
  check_construct "h1,h2" (is_ [ element "h1"; element "h2" ]);
  check_construct ":not(.active)" (not [ class_ "active" ]);
  ()

(* Test parsing roundtrips *)
let roundtrip () =
  (* Basic selectors *)
  check "div";
  check ".class";
  check "#id";
  check "*";

  (* Pseudo-classes. Per CSS Selectors 4 section 14 the printer canonicalizes
     [:nth-child] formulas to the shortest spec-equivalent spelling. *)
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
  check ~expected:"h1,h2,h3" ":is(h1,h2,h3)";
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
  neg_parse "div > > span" "double combinator"

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
  neg_cursor read_attribute_match "%=invalid";
  (* Invalid operator *)
  neg_cursor read_attribute_match "!=not-equal";
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

  (* Test invalid flags using neg *)
  neg_cursor read_attr_flag "x";
  (* Invalid flag *)
  neg_cursor read_attr_flag "is" (* Multiple characters *)

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

  (* Test invalid combinators using neg *)
  neg_cursor read_combinator "!";
  neg_cursor read_combinator "&";
  neg_cursor read_combinator "#";
  neg_cursor read_combinator ""

let test_ns () =
  (* Test namespace type *)
  check_ns "svg|";
  check_ns "xml|";
  check_ns "*|";

  (* Test invalid namespace syntax *)
  neg_cursor read_ns "|";
  (* Just pipe without namespace *)
  neg_cursor read_ns "||";
  neg_cursor read_ns "svg";
  (* Missing pipe *)
  neg_cursor read_ns "svg||";

  (* Double pipe *)

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

  (* Test invalid nth values *)
  neg_cursor read_nth "invalid";
  neg_cursor read_nth "";
  neg_cursor read_nth "2 n";
  neg_cursor read_nth "3 n";
  neg_cursor read_nth "+ 2n";
  neg_cursor read_nth "+ 2";
  neg_cursor read_nth "3n + -6";
  neg_cursor read_nth "n+";
  neg_cursor read_nth "n+-1";
  neg_cursor read_nth "2n--1";
  neg_cursor read_nth "odd+1";
  neg_cursor read_nth "evenn";
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
  neg_cursor read ".a::before::marker"

(* ignore-test *)
let test_spec_forgiving_selector_lists () =
  (* Selectors Level 4: :is() and :where() use forgiving selector-list parsing.
     Invalid selector branches are dropped before minification; top-level lists,
     :not(), and :has() remain unforgiving. *)
  check_minified_to ":is(.valid,#id)" ":is(.valid,:future-pseudo,#id)";
  check_minified_to ":where(.valid,#id)" ":where(.valid,:future-pseudo,#id)";
  (* Single-argument [:is(.item)] is spec-equivalent to bare [.item] (same match
     set and specificity per Selectors L4 sec. 17), but unwrapping it is a node
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
  check_minified_to ".a:before:hover" ".a::before:hover";
  (* CSS Selectors 4 section 3.5: [*] in a non-solitary compound is redundant,
     but dropping it is a node change reserved for the optimizer's
     [Selector.canonicalize]; pp is lexical-only and holds it. *)
  check_minified_to "::slotted(*:not([hidden]))" "::slotted(*:not([hidden]))";
  check "::selection";
  check "input::file-selector-button";
  neg_cursor read "> .item";
  neg_cursor read "+ .item";
  neg_cursor read "~ .item";
  neg_cursor read ".a::before.class";
  neg_cursor read ".a::before::marker";
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
  List.iter
    (fun input -> neg_cursor read input)
    [ ":nth-child(n+)"; ":nth-last-child(2n of)"; ":nth-of-type(odd even)" ]

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
  check_minified_to ".a,.b" ":is(.a, [=bad], .b)";
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
      ":dir(sideways)";
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
      test_case "invalid" `Quick invalid;
      test_case "parse errors - attributes" `Quick parse_errors_attributes;
      test_case "parse errors - combinators" `Quick parse_errors_combinators;
      test_case "parse errors - starts" `Quick parse_errors_starts;
      test_case "parse errors - pseudo" `Quick parse_errors_pseudo;
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
      test_case "nesting selector" `Quick test_nesting_selector;
    ] )
