type attribute_match =
  | Presence
  | Exact of string
  | Whitespace_list of string
  | Hyphen_list of string
  | Prefix of string
  | Suffix of string
  | Substring of string

type combinator =
  | Descendant
  | Child
  | Next_sibling
  | Subsequent_sibling
  | Column

type ns = Any | None | Prefix of string
type attr_flag = Case_insensitive | Case_sensitive

type nth =
  | Odd (* 2n+1 *)
  | Even (* 2n *)
  | Index of int (* Just B: matches a single index *)
  | An_plus_b of int * int (* An+B: a is coefficient, b is offset *)

type t =
  | Element of ns option * string
  | Class of string
  | Id of string
  | Universal of ns option
  | Attribute of ns option * string * attribute_match * attr_flag option
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
  (* Legacy single-colon pseudo-elements for backwards compatibility *)
  | Before
  | After
  | First_letter
  | First_line
  (* Modern double-colon pseudo-elements *)
  | Backdrop
  | Marker
  | Placeholder
  | Selection
  | File_selector_button
  (* Known vendor-specific pseudo-classes *)
  | Moz_focusring
  | Webkit_any
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
  (* Functional pseudo-classes *)
  | Is of t list (* :is() - matches any selector in list *)
  | Where of t list (* :where() - like :is() but 0 specificity *)
  | Not of t list (* :not() - negation *)
  | Has of t list (* :has() - relational selector *)
  | Nth_child of nth * t list option (* :nth-child(An+B [of S]) *)
  | Nth_last_child of nth * t list option (* :nth-last-child(An+B [of S]) *)
  | Nth_of_type of nth * t list option (* :nth-of-type(An+B [of S]) *)
  | Nth_last_of_type of nth * t list option (* :nth-last-of-type(An+B [of S]) *)
  | Dir of string (* :dir(ltr|rtl) *)
  | Lang of string list (* :lang(en|fr|...) - comma-separated language codes *)
  | Host of t list option (* :host or :host(selector) *)
  | Host_context of t list (* :host-context(selector) *)
  | State of string (* :state(custom-state) *)
  | Active_view_transition_type of
      string list option (* :active-view-transition-type() *)
  | Heading (* :heading() - matches h1-h6 *)
  (* Pseudo-elements *)
  | Part of string list (* ::part(...) - takes list of part names *)
  | Slotted of t list (* ::slotted(...) - takes selectors *)
  | Cue of t list (* ::cue(...) - takes selectors *)
  | Cue_region of t list (* ::cue-region(...) - takes selectors *)
  | Highlight of
      string list (* ::highlight(...) - takes custom highlight names *)
  | View_transition_group of string (* ::view-transition-group(name) *)
  | View_transition_image_pair of
      string (* ::view-transition-image-pair(name) *)
  | View_transition_old of string (* ::view-transition-old(name) *)
  | View_transition_new of string (* ::view-transition-new(name) *)
  | Compound of t list
  | Combined of t * combinator * t
  | List of t list
  | Nesting (* & - CSS nesting selector *)
