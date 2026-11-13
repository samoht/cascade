open Cascade
open Css.Selector
open Css_test_helpers
open Selector_helpers

(* Minified serialisation under [--enforce-spec], where no fact about the
   rendering browser or the host document language is available: a rewrite runs
   only when the CSS text and the CSS specs prove it on their own. *)
let check_enforce_spec_to expected input =
  let actual = Css.Pp.to_string ~minify:true ~enforce_spec:true pp (v input) in
  Alcotest.(check string) ("enforce-spec " ^ input) expected actual

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
    let actual = specificity (v input) in
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
    let minified = to_string ~minify:true (v input) in
    let actual = specificity (v minified) in
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

let suite =
  let open Alcotest in
  ( "selector_spec",
    [
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
      test_case "nesting selector" `Quick test_nesting_selector;
    ] )
