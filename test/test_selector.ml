(** Tests for CSS Selector module *)

open Cascade
open Css.Selector
open Css_test_helpers
open Selector_helpers

let of_string_empty_is_error () =
  (match of_string "" with
  | Ok _ -> Alcotest.fail "empty selector parsed"
  | Error _ -> ());
  match v "" with
  | _ -> Alcotest.fail "empty selector built"
  | exception Invalid_argument _ -> ()

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

  (* Selectors 4 sec. 6.1: "Attribute values must be <ident-token>s or
     <string-token>s." A string drops its quotes only when the bare text reads
     back as one <ident-token> (CSS Syntax 3 sec. 4.3.9). Each of these would
     come back as a <number-token>, a <dimension-token> or a <delim-token> (sec.
     4.3.10, 4.3.1), and Chrome drops [[x=-2]] and [[x=-]]
     (curated/attr_negative_number_value and curated/attr_lone_dash_value in
     test/render/parse_recovery.ml), so the quotes stay. *)
  List.iter
    (fun value -> check (String.concat "" [ "[x=\""; value; "\"]" ]))
    [ "2"; "-2"; "+2"; ".5"; "-.5"; "1e3"; "2px"; "-" ];
  check_construct "[x=\"-2\"]" (attribute "x" (Exact "-2"));
  check_construct "[x=\"-\"]" (attribute "x" (Exact "-"));
  (* Selectors 4 sec. 7.2 takes an <ident> or a <string> as a :lang() range, by
     the same rule. *)
  check ":lang(\"-2\")";
  (* CSS Syntax 3 sec. 4.3.9: [--] and a [-] before a name code point start an
     ident sequence, and Chrome keeps [[x=--2]] and [[x=-a]]
     (curated/attr_double_dash_value, curated/attr_dash_ident_value). *)
  check "[x=--2]";
  check "[x=-a]";
  check ~expected:"[x=-a]" "[x=\"-a\"]";
  (* An escape starts one too: [\32] is the value "2" and [-\32] the value "-2",
     kept as written (curated/attr_escaped_digit_value,
     curated/attr_dash_escaped_digit_value). *)
  check "[x=\\32]";
  check "[x=-\\32]";
  (* CSS Syntax 3 sec. 4.3.5: a backslash in a string escapes the next code
     point, so the string ["a\\b"] holds [a\b]. Printed bare, [a\b] is [a] then
     the hex escape [\b], U+000B (sec. 4.3.7), which Chrome reads as another
     value (curated/attr_string_backslash_value). *)
  check "[x=\"a\\\\b\"]";
  check ~minify:false "[x=\"a\\\\b\"]";
  check ~minify:false "[x='a\\\\b']";

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
  match read (Cursor.of_string input) with
  | _ ->
      Alcotest.failf "expected Parse_error for '%s' but parsing succeeded" input
  | exception Cursor.Parse_error _ -> ()

let parse_errors_attributes () =
  (* CSS Syntax 5.3.7 auto-closes unterminated brackets at EOF, so missing
     closing bracket selectors are spec-valid and not asserted here. *)
  check_parse_error ".test[]" "expected identifier";
  check_parse_error ".test[[attr]]" "expected identifier";
  check_parse_error ".test[data id=\"value\"]" "trailing tokens";
  check_parse_error ".test[data-id=value with spaces]" "trailing tokens";
  (* Selectors 4 sec. 6.1 takes an [<ident-token>] or a [<string-token>] as the
     value, and Chrome drops a rule whose value is a number, a dimension or a
     percentage instead of reading it as a string. *)
  check_parse_error "[data-x=2]" "attribute value";
  check_parse_error "[data-x=1.0]" "attribute value";
  check_parse_error "[data-x=1e30]" "attribute value";
  check_parse_error "[data-x=2px]" "attribute value";
  check_parse_error "[data-x=50%]" "attribute value";
  check_parse_error "rect[svg|width=100]" "attribute value";
  (* CSS Syntax 3 sec. 4.3.10: a sign or a [.] before a digit starts a number,
     so each value is a <number-token>, and Chrome drops each rule
     (curated/attr_*_value in test/render/parse_recovery.ml). Sec. 4.3.1 makes
     [#a] a <hash-token> and a lone [-] a <delim-token>. *)
  check_parse_error "[x=-2]" "attribute value";
  check_parse_error "[x=+2]" "attribute value";
  check_parse_error "[x=.5]" "attribute value";
  check_parse_error "[x=2 i]" "attribute value";
  check_parse_error "[*|x=2]" "attribute value";
  check_parse_error "[|x=2]" "attribute value";
  check_parse_error "[x=#a]" "attribute value";
  check_parse_error "[x=-]" "attribute value"

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
  match read t with
  | _ -> Alcotest.failf "%s: expected Parse_error but parsing succeeded" name
  | exception Cursor.Parse_error err ->
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
      test_case "combinator cases" `Quick combinator_cases;
      test_case "compound" `Quick compound_cases;
      test_case "list" `Quick list_cases;
      test_case "where is" `Quick where_is_cases;
      (* Parser/serializer coverage. *)
      test_case "roundtrip" `Quick roundtrip;
      (* Parser/API-policy error coverage. *)
      test_case "of_string empty is an error" `Quick of_string_empty_is_error;
      test_case "invalid" `Quick invalid;
      test_case "parse errors - attributes" `Quick parse_errors_attributes;
      test_case "parse errors - combinators" `Quick parse_errors_combinators;
      test_case "parse errors - starts" `Quick parse_errors_starts;
      test_case "parse errors - pseudo" `Quick parse_errors_pseudo;
      test_case "parse errors - nesting depth" `Quick parse_errors_nesting_depth;
      test_case "parse errors - empty list" `Quick parse_errors_empty_list;
      test_case "parse errors - complex" `Quick parse_errors_complex;
      test_case "callstack accuracy" `Quick callstack_accuracy;
    ] )
