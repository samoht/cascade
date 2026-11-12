open Cascade
open Css.Selector
open Css_test_helpers
open Selector_helpers

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
      Alcotest.(check int) ("specificity " ^ pc) 1 (specificity (v pc)).classes;
      (* None of them is functional. *)
      neg_cursor read (concat [ ".a"; pc; "(1)" ]))
    scrollbar_state_pseudo_classes;
  (* A scrollbar part reads any combination of them. *)
  round_trips "::-webkit-scrollbar:vertical:hover";
  round_trips "::-webkit-scrollbar:corner-present:window-inactive";
  round_trips "::-webkit-scrollbar-button:start:decrement"

let suite =
  let open Alcotest in
  ( "selector_pseudo",
    [
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
    ] )
