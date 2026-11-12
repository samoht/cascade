open Cascade
open Css.Selector
open Css_test_helpers
open Selector_helpers

let check_nth = check_value_cursor "nth" read_nth pp_nth

let check_combinator =
  check_value_cursor "combinator" read_combinator pp_combinator

let check_component_values =
  check_value_cursor "component_values" read_component_values
    pp_component_values

let check_ns = check_value_cursor "ns" read_ns (Css.Pp.option pp_ns)
let check_aria_attr = check_value_cursor "aria_attr" read_aria_attr pp_aria_attr
let check_attr_name = check_value_cursor "attr_name" read_attr_name pp_attr_name

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
(* Substituting [&] splices a complex parent into the leading slot of the
   nested compound, and [canonicalize] lifts the trailing components onto the
   parent's subject so the spliced form and the read form compare equal. The
   lifted subject is a compound like any other: Selectors 4 sec. 3.5 lets the
   universal selector go when it is not the only component, so [svg *] under
   [&:where(.dark)] has to reach [svg :where(.dark)], as reading that text
   does. *)
let canonicalize_lifted_subject_drops_universal () =
  let canon expected parent child =
    let spliced =
      Nest.substitute ~parent:(of_string parent) (of_string child)
    in
    let actual = to_string ~minify:true (canonicalize spliced) in
    Alcotest.(check string)
      ("canonicalize " ^ parent ^ " { " ^ child ^ " }")
      expected actual;
    Alcotest.(check bool)
      (parent ^ " { " ^ child ^ " } canonicalizes to the AST of " ^ expected)
      true
      (equal (canonicalize spliced) (canonicalize (of_string expected)))
  in
  canon "svg :where(.dark,.dark *)" "svg *" "&:where(.dark, .dark *)";
  canon "svg :where(.dark,.dark *)" "svg *" "&:where(.dark,.dark *)";
  canon ".a svg :where(.dark,.dark *)" ".a svg *" "&:where(.dark, .dark *)";
  canon "svg :hover" "svg *" "&:hover";
  (* A universal alone stays. *)
  canon "svg *" "svg *" "&"

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
  check "rect[svg|width=\"100\"]";

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

let test_component_values () =
  check_component_values ~expected:":unknown(.a,.b)" ":unknown(.a, .b)";
  check_component_values ~expected:"foo(1,2)" "foo(1, 2)"

let suite =
  let open Alcotest in
  ( "selector_api",
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
      test_case "selector component parsing" `Quick component_parsing;
      test_case "attribute match" `Quick test_attribute_match;
      test_case "attr_value_quoting" `Quick test_attr_value_quoting;
      test_case "attr flag" `Quick test_attr_flag;
      test_case "attr_case_sensitivity_flags" `Quick
        test_attr_case_sensitivity_flags;
      test_case "selector component failures" `Quick component_parsing_failures;
      test_case "canonicalize pseudo-compound :is()" `Quick
        canonicalize_pseudo_compound_is;
      test_case "canonicalize top-level :is()" `Quick
        canonicalize_top_level_is_unwrap;
      test_case "canonicalize lifted subject drops the universal" `Quick
        canonicalize_lifted_subject_drops_universal;
      (* Local construction helpers. *)
      test_case "complex construction" `Quick test_complex_construction;
      test_case "combinator distribution" `Quick test_combinator_distribution;
      test_case "hash separates deep tails" `Quick hash_separates_deep_tails;
      test_case "hash agrees with equal" `Quick hash_agrees_with_equal;
    ] )
