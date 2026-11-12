open Selector_intf

(* Pseudo-class and pseudo-element identifier mappings *)
let pseudo_class_base_idents =
  [
    (* Interactive *)
    ("hover", Hover);
    ("active", Active);
    ("focus", Focus);
    ("focus-visible", Focus_visible);
    ("focus-within", Focus_within);
    ("target", Target);
    ("target-within", Target_within);
    (* Link *)
    ("link", Link);
    ("visited", Visited);
    ("any-link", Any_link);
    ("local-link", Local_link);
    (* Structural *)
    ("root", Root);
    ("empty", Empty);
    ("first-child", First_child);
    ("last-child", Last_child);
    ("only-child", Only_child);
    ("first-of-type", First_of_type);
    ("last-of-type", Last_of_type);
    ("only-of-type", Only_of_type);
    (* Input *)
    ("enabled", Enabled);
    ("disabled", Disabled);
    ("read-only", Read_only);
    ("read-write", Read_write);
    ("placeholder-shown", Placeholder_shown);
    ("default", Default);
    ("checked", Checked);
    ("indeterminate", Indeterminate);
    ("blank", Blank);
    ("valid", Valid);
    ("invalid", Invalid);
    ("in-range", In_range);
    ("out-of-range", Out_of_range);
    ("required", Required);
    ("optional", Optional);
    ("user-invalid", User_invalid);
    ("user-valid", User_valid);
    ("inert", Inert);
    ("autofill", Autofill);
    (* Display *)
    ("fullscreen", Fullscreen);
    ("modal", Modal);
    ("picture-in-picture", Picture_in_picture);
    ("popover-open", Popover_open);
    ("open", Open);
    (* CSS Modules scope keywords - non-standard but emitted by tooling *)
    ("local", Local_scope);
    ("global", Global_scope);
    (* Paged *)
    ("left", Left);
    ("right", Right);
    ("first", First);
    (* Component *)
    ("defined", Defined);
    ("scope", Scope);
    ("host", Host None);
    (* Media *)
    ("playing", Playing);
    ("paused", Paused);
    ("seeking", Seeking);
    ("buffering", Buffering);
    ("stalled", Stalled);
    ("muted", Muted);
    ("volume-locked", Volume_locked);
    ("current", Current);
    ("past", Past);
    ("future", Future);
    (* View transitions *)
    ("active-view-transition", Active_view_transition);
  ]

let scrollbar_state_ident = function
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"
  | Decrement -> "decrement"
  | Increment -> "increment"
  | Start -> "start"
  | End -> "end"
  | Double_button -> "double-button"
  | Single_button -> "single-button"
  | No_button -> "no-button"
  | Corner_present -> "corner-present"
  | Window_inactive -> "window-inactive"

let scrollbar_part_ident = function
  | Scrollbar -> "-webkit-scrollbar"
  | Button -> "-webkit-scrollbar-button"
  | Track -> "-webkit-scrollbar-track"
  | Track_piece -> "-webkit-scrollbar-track-piece"
  | Thumb -> "-webkit-scrollbar-thumb"
  | Corner -> "-webkit-scrollbar-corner"
  | Resizer -> "-webkit-resizer"

(* WebKit, "Styling Scrollbars". Both engines read these on any element, so they
   are ordinary pseudo-classes here; which pseudo-elements take them is
   [pseudo_element_allows]'s business. *)
let pseudo_class_scrollbar_idents =
  List.map
    (fun state -> (scrollbar_state_ident state, Scrollbar_state state))
    [
      Horizontal;
      Vertical;
      Decrement;
      Increment;
      Start;
      End;
      Double_button;
      Single_button;
      No_button;
      Corner_present;
      Window_inactive;
    ]

let pseudo_element_legacy_idents form =
  [
    (* Legacy pseudo-elements: parser records [Single] or [Double] colon for
       tests and compatibility, but the printer canonicalizes pretty output to
       the modern double-colon spelling. *)
    ("before", Before form);
    ("after", After form);
    ("first-letter", First_letter form);
    ("first-line", First_line form);
  ]

let pseudo_element_modern_idents =
  [
    (* Modern pseudo-elements *)
    ("backdrop", Backdrop);
    ("marker", Marker);
    ("placeholder", Placeholder);
    ("selection", Selection);
    ("target-text", Target_text);
    ("spelling-error", Spelling_error);
    ("grammar-error", Grammar_error);
    ("file-selector-button", File_selector_button);
    ("view-transition", View_transition);
  ]

let pseudo_vendor_idents =
  [
    (* Vendor-specific *)
    ("-moz-focusring", Moz_focusring);
    ("-webkit-any", Webkit_any);
    ("-webkit-autofill", Webkit_autofill);
    ("-moz-placeholder", Moz_placeholder);
    ("-webkit-input-placeholder", Webkit_input_placeholder);
    ("-ms-input-placeholder", Ms_input_placeholder);
    ("-moz-ui-invalid", Moz_ui_invalid);
    ("-moz-ui-valid", Moz_ui_valid);
    ("-webkit-scrollbar", Webkit_scrollbar Scrollbar);
    ("-webkit-search-cancel-button", Webkit_search_cancel_button);
    ("-webkit-search-decoration", Webkit_search_decoration);
    (* Webkit datetime pseudo-elements *)
    ("-webkit-datetime-edit-fields-wrapper", Webkit_datetime_edit_fields_wrapper);
    ("-webkit-date-and-time-value", Webkit_date_and_time_value);
    ("-webkit-datetime-edit", Webkit_datetime_edit);
    ("-webkit-datetime-edit-year-field", Webkit_datetime_edit_year_field);
    ("-webkit-datetime-edit-month-field", Webkit_datetime_edit_month_field);
    ("-webkit-datetime-edit-day-field", Webkit_datetime_edit_day_field);
    ("-webkit-datetime-edit-hour-field", Webkit_datetime_edit_hour_field);
    ("-webkit-datetime-edit-minute-field", Webkit_datetime_edit_minute_field);
    ("-webkit-datetime-edit-second-field", Webkit_datetime_edit_second_field);
    ( "-webkit-datetime-edit-millisecond-field",
      Webkit_datetime_edit_millisecond_field );
    ("-webkit-datetime-edit-meridiem-field", Webkit_datetime_edit_meridiem_field);
    ("-webkit-inner-spin-button", Webkit_inner_spin_button);
    ("-webkit-outer-spin-button", Webkit_outer_spin_button);
    ("-webkit-calendar-picker-indicator", Webkit_calendar_picker_indicator);
    ("-webkit-details-marker", Webkit_details_marker);
    ("details-content", Details_content);
  ]

(* Names the [::] reader takes and the [:] reader does not. Chrome 151 and
   WebKit 26.5 drop [:cue] and [:-webkit-scrollbar-thumb] while keeping the [::]
   spelling of both, so these stay out of [pseudo_class_all_idents];
   [::-webkit-scrollbar] itself keeps the single-colon spelling cascade has
   always read for it and stays in [pseudo_vendor_idents].

   WebVTT 1 sec. 8.2 requires "the ::cue, ::cue(selector), ::cue-region and
   ::cue-region(selector) pseudo-elements", and sec. 8.2.1 and sec. 8.2.3 define
   the argument-less pair the [calls] table above cannot reach. Chrome, WebKit
   and Lightning CSS all take [::cue]; only Lightning CSS takes [::cue-region],
   the other two implement neither of its forms. *)
let pseudo_element_double_colon_idents =
  ("cue", Cue None)
  :: ("cue-region", Cue_region None)
  :: List.map
       (fun part -> (scrollbar_part_ident part, Webkit_scrollbar part))
       [ Button; Track; Track_piece; Thumb; Corner; Resizer ]

(* Every [::] form, and only those: a pseudo-element names a box other than the
   originating element, where an element-tied pseudo-class ([:hover], [:focus])
   only narrows which elements match. An unrecognised [::foo] belongs here too.
   Selectors 4 sec. 16 builds a pseudo-compound out of
   [<pseudo-element-selector> <pseudo-class-selector>*] whatever the
   pseudo-element's name is, so the shape rules that read this predicate cannot
   wait for cascade to learn the name; the framework-only [::deep] / [::v-deep]
   / [::ng-deep] are unknown [::] names like any other, since Selectors 4 has no
   piercing combinator. *)
let is_pseudo_element = function
  | Before _ | After _ | First_letter _ | First_line _ | Backdrop | Marker
  | Placeholder | Selection | Target_text | Spelling_error | Grammar_error
  | File_selector_button | Moz_placeholder | Webkit_input_placeholder
  | Ms_input_placeholder | Webkit_scrollbar _ | Webkit_search_cancel_button
  | Webkit_search_decoration | Webkit_datetime_edit_fields_wrapper
  | Webkit_date_and_time_value | Webkit_datetime_edit
  | Webkit_datetime_edit_year_field | Webkit_datetime_edit_month_field
  | Webkit_datetime_edit_day_field | Webkit_datetime_edit_hour_field
  | Webkit_datetime_edit_minute_field | Webkit_datetime_edit_second_field
  | Webkit_datetime_edit_millisecond_field | Webkit_datetime_edit_meridiem_field
  | Webkit_inner_spin_button | Webkit_outer_spin_button
  | Webkit_calendar_picker_indicator | Webkit_details_marker | Details_content
  | Part _ | Slotted _ | Cue _ | Cue_region _ | Highlight _ | View_transition
  | View_transition_group _ | View_transition_image_pair _
  | View_transition_old _ | View_transition_new _ | Unknown_pseudo_element _
  | Unknown_pseudo_element_call _ ->
      true
  | _ -> false

let rec any p = function
  | Compound xs as sel -> List.exists (any p) xs || p sel
  | Combined (a, _, b) as sel -> any p a || any p b || p sel
  | Relative (_, b) as sel -> any p b || p sel
  | List xs as sel -> List.exists (any p) xs || p sel
  | ( Is xs
    | Where xs
    | Not xs
    | Has xs
    | Moz_any_call xs
    | Webkit_any_call xs
    | Slotted xs
    | Current_of xs ) as sel ->
      List.exists (any p) xs || p sel
  | ( Nth_child (_, Some xs)
    | Nth_last_child (_, Some xs)
    | Nth_of_type (_, Some xs)
    | Nth_last_of_type (_, Some xs)
    | Host (Some xs)
    | Cue (Some xs)
    | Cue_region (Some xs)
    | Host_context xs ) as sel ->
      List.exists (any p) xs || p sel
  | Part _ as sel -> p sel
  | s -> p s

let has_pseudo_element sel = any is_pseudo_element sel

let has_unknown_pseudo_class =
  any (function
    | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ -> true
    | _ -> false)

(* CSS Selectors 4 17.1: a forgiving [:is()] / [:where()] with no surviving
   valid argument matches nothing, and a compound or combined selector that
   contains such a sub-selector inherits the same behaviour. A selector list
   matches nothing only when every entry does. Useful for dropping dead rules
   under [Optimize.stylesheet]. *)
let rec matches_nothing = function
  | Is [] | Where [] -> true
  | Compound xs -> List.exists matches_nothing xs
  | Combined (a, _, b) -> matches_nothing a || matches_nothing b
  | Relative (_, b) -> matches_nothing b
  | List [] -> true
  | List xs -> List.for_all matches_nothing xs
  | _ -> false

(* CSS Selectors 4 sec. 3.6.3: what may follow a pseudo-element in its compound
   is a pseudo-class, and sec. 3.6.4 adds a sub-pseudo-element
   ([pseudo_element_allows_sub]). A class, id, type or attribute selector after
   the pseudo-element makes the compound invalid whatever the pseudo-element
   is. *)
let is_pe_action = function
  | Element _ | Class _ | Id _ | Universal _ | Attribute _ | Nesting -> false
  | sel -> not (is_pseudo_element sel)

(* CSS Selectors 4 sec. 9. *)
let is_user_action_pseudo_class = function
  | Hover | Active | Focus | Focus_visible | Focus_within -> true
  | _ -> false

(* CSS Selectors 4 sec. 13: the tree-structural pseudo-classes, the ones that
   answer a question about the element's place among its siblings. *)
let is_structural_pseudo_class = function
  | Root | Empty | First_child | Last_child | Only_child | First_of_type
  | Last_of_type | Only_of_type | Nth_child _ | Nth_last_child _ | Nth_of_type _
  | Nth_last_of_type _ ->
      true
  | _ -> false

(* Which pseudo-classes each pseudo-element takes after it. CSS Selectors 4 sec.
   3.6.3 allows the logical combinations and hands the rest of the list to
   "other specifications", so the rows below come from CSS Pseudo-Elements 4
   sec. 5 for the element-backed pseudo-elements and, for the UA widgets whose
   list only exists in the engines, from Chrome and WebKit, taking a
   pseudo-class as allowed when either engine keeps the rule. *)
let rec pseudo_element_allows pe pc =
  match pe with
  (* A name no engine recognises: cascade keeps it so a pseudo-element newer
     than this list survives a format pass, and knows nothing about its rules,
     so it keeps taking any pseudo-class after it. [::cue-region()] keeps the
     same bargain: Chrome 151 and WebKit 26.5 implement neither of its forms. *)
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ | Moz_placeholder
  | Ms_input_placeholder
  | Cue_region (Some _) ->
      true
  | pe -> (
      match pc with
      (* Sec. 3.6.3 allows the logical combinations after every pseudo-element
         and passes the row below on to their arguments; what an argument the
         row refuses costs is the argument list's own business. [:is()] and
         [:where()] take a [<forgiving-selector-list>] (sec. 4.1), which drops
         it and leaves a selector that still parses and matches nothing, so both
         engines keep the rule; the [:-moz-any()] / [:-webkit-any()] aliases
         read back the same way. *)
      | Is _ | Where _ | Moz_any_call _ | Webkit_any_call _ -> true
      (* [:not()] takes an unforgiving list, so the refused argument takes the
         compound down with it: both engines drop [::before:not(:hover)] and
         keep [::part(p):not(:hover)]. *)
      | Not args -> List.for_all (pseudo_element_allows_argument pe) args
      (* WebKit, "Styling Scrollbars": a scrollbar part reports its own state,
         and [:window-inactive] reaches past the scrollbar to a selection and to
         a shadow part, where both engines take it. Past that they disagree one
         cell each way (only WebKit takes the other ten after [::part()], only
         Chrome takes [:window-inactive] after [::details-content]), so the list
         stops where they agree. *)
      | Scrollbar_state state -> (
          match (pe, state) with
          | Webkit_scrollbar _, _ -> true
          | (Selection | Part _), Window_inactive -> true
          | _ -> false)
      (* Same forward-compatibility bargain as an unknown pseudo-element. *)
      | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ -> true
      | pc -> (
          match pe with
          (* CSS Pseudo-Elements 4 sec. 5: an element-backed pseudo-element
             takes what a real element takes, bar the pseudo-classes that would
             report on the tree it sits in. *)
          | Part _ | Details_content -> (
              match pc with
              | Has _ -> false
              | pc -> not (is_structural_pseudo_class pc))
          (* A scrollbar part takes no focus, and reports which part of which
             scrollbar it is through the state pseudo-classes below. *)
          | Webkit_scrollbar _ -> (
              match pc with
              | Hover | Active | Enabled | Disabled -> true
              | _ -> false)
          (* CSS View Transitions 1 sec. 3.1: [:only-child] matches a view
             transition pseudo with no sibling in the pseudo-element tree. *)
          | View_transition_group _ | View_transition_image_pair _
          | View_transition_old _ | View_transition_new _ -> (
              match pc with Only_child -> true | _ -> false)
          (* The UA widgets that stand in for a real control, and the cue and
             region boxes WebVTT 1 sec. 8.2.1 and sec. 8.2.3 define: all three
             engines keep [::cue:focus-within] and drop [::cue:enabled].
             [::cue(...)] selects inside the cue and takes none of them. *)
          | Cue None
          | Cue_region None
          | Placeholder | File_selector_button | Webkit_input_placeholder
          | Webkit_search_cancel_button | Webkit_search_decoration
          | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
          | Webkit_datetime_edit | Webkit_datetime_edit_year_field
          | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
          | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
          | Webkit_datetime_edit_second_field
          | Webkit_datetime_edit_millisecond_field
          | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
          | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
          | Webkit_details_marker ->
              is_user_action_pseudo_class pc
          | _ -> false))

(* An argument of a logical combination sits where the pseudo-element's own
   pseudo-classes sit, so it reads as a pseudo-compound tail: pseudo-classes the
   pseudo-element takes, and nothing else. A combinator or a selector list makes
   the argument a whole complex selector, which no pseudo-element takes. *)
and pseudo_element_allows_argument pe = function
  | Compound components ->
      List.for_all (pseudo_element_allows_argument pe) components
  | Combined _ | Relative _ | List _ -> false
  | c -> is_pe_action c && pseudo_element_allows pe c

(* CSS Pseudo-Elements 4 sec. 4 names the tree-abiding pseudo-elements, sec. 7.1
   lists [::backdrop] and [::view-transition] among them, and sec. 5 adds that
   "element-backed pseudo-elements are always tree-abiding". [::part()] is
   element-backed and so belongs here on that reading, and is left out because
   sec. 5 also says it never matches after a pseudo-element. *)
let is_tree_abiding_pseudo_element = function
  | Before _ | After _ | Marker | Placeholder | Backdrop | View_transition
  | Details_content | File_selector_button ->
      true
  | _ -> false

(* Which pseudo-elements each pseudo-element takes after it, in the same
   compound. CSS Selectors 4 sec. 3.6.4 refuses the shape "unless the
   corresponding sub-pseudo-element is explicitly defined to exist in another
   specification", so the rows below are those definitions and nothing else:
   [::before::before] goes, [::before::marker] stays.

   CSS Pseudo-Elements 4 sec. 4.2 defines the [::marker] of a [::before] or
   [::after] that is a list item, and rules [::marker::marker] out. Sec. 5 lets
   an element-backed pseudo-element take every pseudo-element "just as if the
   pseudo-element were a type selector", bar [::part()], which "never matches"
   there; cascade turns that never-matching row into a refusal, as it does one
   row up for [:has()] and the structural pseudo-classes. Sec. 5.1 files
   [::file-selector-button] as element-backed, but Chrome 151, WebKit 27 and
   Lightning CSS all drop [::file-selector-button::before], so the originating
   row keeps the [::part()] / [::details-content] pair [pseudo_element_allows]
   carries.

   CSS Shadow 1 sec. 3.2.4 gives [::slotted()] a narrower row of its own: "the
   ::slotted() pseudo-element can be followed by a tree-abiding pseudo-element".
   The engines show the difference, dropping [::slotted(a)::first-line] while
   keeping [::part(x)::first-line].

   One adjacent pair at a time, so a longer chain is as valid as each of its
   links: Chrome keeps [::part(x)::before::marker] and drops
   [::part(x)::marker::before].

   An unmodelled [::] name keeps the bargain [pseudo_element_allows] makes for
   it: cascade preserves the name rather than judging what may follow it. *)
let pseudo_element_allows_sub pe sub =
  match pe with
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ -> true
  | Part _ | Details_content -> ( match sub with Part _ -> false | _ -> true)
  | Slotted _ -> is_tree_abiding_pseudo_element sub
  | Before _ | After _ -> ( match sub with Marker -> true | _ -> false)
  | _ -> false

(* CSS Selectors 4 sec. 3.6.5: a pseudo-element defined to have internal
   structure may be followed by a child or descendant combinator, and a selector
   with a combinator after the pseudo-element is invalid otherwise. Nothing
   shipping claims that structure, so the exception stays empty: Chrome 151 and
   WebKit 26.5 drop [::part(x) > .b], [::details-content > div] and
   [::first-letter em] alike, and the Servo selectors crate raises
   UnexpectedSelectorAfterPseudoElement for the same shapes.

   A [::] name cascade does not model is exempt, for the reason
   [pseudo_element_allows] already exempts it from sec. 3.6.3: cascade preserves
   such a name rather than judging it. test/interop/lightning carries seven
   rules of that kind ([.foo ::deep .bar], [.foo ::unknown(.foo) .bar] and their
   kin) and all six reference minifiers keep every one verbatim, while Lightning
   CSS rejects the identical shape as soon as the name is one it knows
   ([::-moz-placeholder .b]). Dropping the exemption would delete those
   rules. *)
let is_modelled_pseudo_element = function
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ -> false
  | sel -> is_pseudo_element sel

(* Only the compound's own components. A pseudo-element inside a functional
   pseudo-class argument belongs to that argument's own rules, so
   [.a:has(.b::before) .c] is not this one's to judge. *)
let bars_following_combinator = function
  | Compound components -> List.exists is_modelled_pseudo_element components
  | sel -> is_modelled_pseudo_element sel

(* The same sec. 3.6.5 rule read off a built selector rather than applied while
   reading one. Nesting reaches a selector the reader would refuse, because it
   joins a valid parent to a valid child without either passing the check. The
   two tooling combinators are exempt for the reason [check_combinator] exempts
   them: no engine parses them, so the rule never reaches them. *)
let rec has_combinator_after_pseudo_element = function
  | List branches -> List.exists has_combinator_after_pseudo_element branches
  | Combined (left, comb, right) ->
      (match comb with
        | Shadow_piercing | Shadow_deep -> false
        | Descendant | Child | Next_sibling | Subsequent_sibling | Column ->
            bars_following_combinator left)
      || has_combinator_after_pseudo_element left
      || has_combinator_after_pseudo_element right
  | _ -> false

(* Why a compound may not carry [s] after the pseudo-element it follows.
   [read_compound] turns each into its own message and [has_refused_simple_-
   after_pseudo_element] reads the same answer off a built selector, so the two
   cannot drift. [seen] is the compound's earlier components, most recent
   first. *)
type compound_refusal =
  | Sub_pseudo_element  (** sec. 3.6.4, via [pseudo_element_allows_sub]. *)
  | Simple_after_pseudo_element  (** sec. 3.6.3, via [is_pe_action]. *)
  | Pseudo_class_after_pseudo_element
      (** sec. 3.6.3, via [pseudo_element_allows]. *)

let compound_refusal ~seen s =
  match List.find_opt is_pseudo_element seen with
  | Some pe when is_pseudo_element s ->
      if pseudo_element_allows_sub pe s then Option.None
      else Option.Some Sub_pseudo_element
  | Some _ when not (is_pe_action s) -> Option.Some Simple_after_pseudo_element
  | Some pe when not (pseudo_element_allows pe s) ->
      Option.Some Pseudo_class_after_pseudo_element
  | _ -> Option.None

(* The sec. 3.6.3 / 3.6.4 companion of [has_combinator_after_pseudo_element]:
   nesting extends a pseudo-element's own compound the same way it puts a
   combinator after one, out of a valid parent and a valid child that neither
   one meets the reader's check. A pseudo-element inside a functional
   pseudo-class argument is that argument's own business, as it is for
   [bars_following_combinator]. *)
(* Substituting [&] splices the parent compound in as one component, so the walk
   flattens a nested [Compound] rather than treating it as a simple selector the
   pseudo-element would refuse outright. *)
let rec refuses_compound seen = function
  | [] -> false
  | Compound inner :: rest -> refuses_compound seen (inner @ rest)
  | c :: rest -> (
      match compound_refusal ~seen c with
      | Some _ -> true
      | None -> refuses_compound (c :: seen) rest)

let rec has_refused_simple_in_compound = function
  | List branches -> List.exists has_refused_simple_in_compound branches
  | Combined (left, _, right) ->
      has_refused_simple_in_compound left
      || has_refused_simple_in_compound right
  | Relative (_, right) -> has_refused_simple_in_compound right
  | Compound components -> refuses_compound [] components
  | _ -> false

(* The merged lists are static across the lifetime of the program (every
   constituent is a [let] binding above); memoise them so the [@] cons-chain
   only happens once instead of per [:foo] / [::foo] pseudo read. *)
let pseudo_class_all_idents_lazy =
  lazy
    (pseudo_class_base_idents @ pseudo_class_scrollbar_idents
    @ pseudo_element_legacy_idents Single
    @ pseudo_element_modern_idents @ pseudo_vendor_idents)

let pseudo_class_all_idents () = Lazy.force pseudo_class_all_idents_lazy

let pseudo_element_unknown_idents =
  lazy
    (pseudo_element_modern_idents @ pseudo_vendor_idents
   @ pseudo_element_double_colon_idents
    @ pseudo_element_legacy_idents Double)
