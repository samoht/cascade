type component_values = Values.component_values
(** Parsed CSS component values preserved inside unknown functional selectors.
*)

type attribute_match =
  | Presence
  | Exact of string
  | Exact_quoted of string * char
  | Whitespace_list of string
  | Whitespace_list_quoted of string * char
  | Hyphen_list of string
  | Hyphen_list_quoted of string * char
  | Prefix of string
  | Prefix_quoted of string * char
  | Suffix of string
  | Suffix_quoted of string * char
  | Substring of string
  | Substring_quoted of string * char

type combinator =
  | Descendant
  | Child
  | Next_sibling
  | Subsequent_sibling
  | Column
  | Shadow_piercing
      (** Legacy [>>>] (Vue / Angular pre-Shadow-DOM-v1) shadow-piercing
          combinator. Not part of any current spec but still emitted by tooling,
          so cascade preserves it as its own combinator arm. *)
  | Shadow_deep
      (** Legacy [/deep/] alias for [>>>]; same scope and rationale. *)

type ns = Any | None | Prefix of string
type attr_flag = Insensitive | Sensitive
type specificity = { ids : int; classes : int; elements : int }

(** CSS Selectors 4 3.6.1 colon-prefix form for the legacy pseudo-elements
    [::before], [::after], [::first-letter], [::first-line]. [Single] is the CSS
    2.1 spelling kept as a backwards-compatibility alias; [Double] is the modern
    canonical spelling. *)
type colon_form = Single | Double

type aria_attr = Aria.t
(** ARIA attribute names for type-safe handling *)

(** Structured attribute names *)
type attr_name =
  | Aria of aria_attr  (** aria-* attributes *)
  | Data of string  (** data-* attributes *)
  | Regular of string  (** All other attributes (class, type, etc.) *)

type nth =
  | Odd (* 2n+1 *)
  | Even (* 2n *)
  | Index of int (* Just B: matches a single index *)
  | An_plus_b of int * int (* An+B: a is coefficient, b is offset *)

type vt_class_selector = { name : string option; classes : string list }
(** CSS View Transitions 2 §3.4.1 [<vt-class-selector>]: an optional vt-name
    ([<custom-ident>] or [*]) followed by zero or more [.<custom-ident>] class
    qualifiers. The empty case (no name and no classes) does not appear in
    cascade output - the parser always reads at least one component. *)

type t =
  | Element of ns option * string
  | Class of string
  | Id of string
  | Universal of ns option
  | Attribute of ns option * attr_name * attribute_match * attr_flag option
  (* Simple pseudo-classes *)
  | Hover
  | Active
  | Focus
  | Focus_visible
  | Focus_within
  | Target
  | Link
  | Visited
  | Any_link
  | Local_link
  | Target_within
  | Scope
  | Root
  | Empty
  | First_child
  | Last_child
  | Only_child
  | First_of_type
  | Last_of_type
  | Only_of_type
  | Enabled
  | Disabled
  | Read_only
  | Read_write
  | Placeholder_shown
  | Default
  | Checked
  | Indeterminate
  | Blank
  | Valid
  | Invalid
  | In_range
  | Out_of_range
  | Required
  | Optional
  | User_invalid
  | User_valid
  | Inert
  | Autofill
  | Fullscreen
  | Modal
  | Picture_in_picture
  | Left
  | Right
  | First
  | Defined
  | Playing
  | Paused
  | Seeking
  | Buffering
  | Stalled
  | Muted
  | Volume_locked
  | Future
  | Past
  | Current
  | Popover_open
  | Open
  | Unknown_pseudo_class of string
      (** Vendor / prerelease pseudo-classes cascade doesn't recognise. *)
  | Unknown_pseudo_class_call of string * component_values
      (** Functional vendor / prerelease pseudo-classes with preserved
          arguments. *)
  | Local_scope
      (** CSS Modules [:local] - non-standard but emitted by the css-modules /
          postcss-modules toolchain to mark a class as locally scoped. *)
  | Local_call of t list
      (** CSS Modules [:local(<selector-list>)] - functional form. *)
  | Global_scope  (** CSS Modules [:global] - opposite of [:local]. *)
  | Global_call of t list  (** CSS Modules [:global(<selector-list>)]. *)
  (* CSS Selectors 4 3.6.1: [::before] / [::after] / [::first-letter] /
     [::first-line] are the modern double-colon pseudo-element forms; the CSS
     2.1 single-colon spelling is preserved as a backwards-compatibility alias.
     The [colon_form] field records which form the source used. Minified output
     always picks the [Single]-colon form since it is one character shorter;
     pretty output uses the modern double-colon spelling. *)
  | Before of colon_form
  | After of colon_form
  | First_letter of colon_form
  | First_line of colon_form
  (* Modern double-colon pseudo-elements *)
  | Backdrop
  | Marker
  | Placeholder
  | Selection
  | Target_text
  | Spelling_error
  | Grammar_error
  | File_selector_button
  (* Known vendor-specific pseudo-classes *)
  | Moz_focusring
  | Moz_any_call of t list
  | Webkit_any
  | Webkit_any_call of t list
  | Webkit_autofill
  | Moz_placeholder
  | Webkit_input_placeholder
  | Ms_input_placeholder
  | Moz_ui_invalid
  | Moz_ui_valid
  | Webkit_scrollbar
  | Webkit_search_cancel_button
  | Webkit_search_decoration
  (* Webkit datetime pseudo-elements *)
  | Webkit_datetime_edit_fields_wrapper
  | Webkit_date_and_time_value
  | Webkit_datetime_edit
  | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field
  | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field
  | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field
  | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field
  | Webkit_inner_spin_button
  | Webkit_outer_spin_button
  | Webkit_calendar_picker_indicator
  | Webkit_details_marker
  | Details_content
  (* Functional pseudo-classes *)
  | Is of t list (* :is() - matches any selector in list *)
  | Where of t list (* :where() - like :is() but 0 specificity *)
  | Not of t list (* :not() - negation *)
  | Has of t list (* :has() - relational selector *)
  | Current_of of t list (* :current(selector) *)
  | Nth_child of nth * t list option (* :nth-child(An+B [of S]) *)
  | Nth_last_child of nth * t list option (* :nth-last-child(An+B [of S]) *)
  | Nth_of_type of nth * t list option (* :nth-of-type(An+B [of S]) *)
  | Nth_last_of_type of nth * t list option (* :nth-last-of-type(An+B [of S]) *)
  | Nth_col of nth (* :nth-col(An+B) *)
  | Nth_last_col of nth (* :nth-last-col(An+B) *)
  | Dir of string (* :dir(ltr|rtl) *)
  | Lang of string list (* :lang(en|fr|...) - comma-separated language codes *)
  | Host of t list option (* :host or :host(selector) *)
  | Host_context of t list (* :host-context(selector) *)
  | State of string (* :state(custom-state) *)
  | Active_view_transition (* :active-view-transition *)
  | Active_view_transition_type of
      string list option (* :active-view-transition-type(type,...) *)
  | Heading (* :heading() - matches h1-h6 *)
  (* Pseudo-elements *)
  | Part of string list (* ::part(...) - takes list of part names *)
  | Slotted of t list (* ::slotted(...) - takes selectors *)
  | Cue of t list (* ::cue(...) - takes selectors *)
  | Cue_region of t list (* ::cue-region(...) - takes selectors *)
  | Highlight of
      string list (* ::highlight(...) - takes custom highlight names *)
  | View_transition (* ::view-transition (CSS View Transitions 1 §3.2) *)
  | View_transition_group of vt_class_selector
      (** ::view-transition-group with optional name and class suffixes. *)
  | View_transition_image_pair of vt_class_selector
      (** ::view-transition-image-pair with optional name and class suffixes. *)
  | View_transition_old of vt_class_selector
      (** ::view-transition-old with optional name and class suffixes. *)
  | View_transition_new of vt_class_selector
      (** ::view-transition-new with optional name and class suffixes. *)
  | Unknown_pseudo_element of string
      (** Vendor / prerelease pseudo-elements cascade doesn't recognise (e.g.
          [::deep], [::unknown]). Preserved as the raw ident. *)
  | Unknown_pseudo_element_call of string * component_values
      (** Functional form: [::unknown(<arbitrary tokens>)]. Argument list
          captured as component values so the printer re-emits verbatim. *)
  | Compound of t list
  | Combined of t * combinator * t
  | Relative of combinator * t
    (* relative selector: combinator without left operand, e.g. + img inside
       :has() *)
  | List of t list
  | Nesting (* & - CSS nesting selector *)
