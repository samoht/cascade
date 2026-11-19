(* Which evergreen targets drop a selector. Selectors 4 sec. 3.9 drops a whole
   list in a browser that does not parse one of its selectors, so a list is read
   as its selectors are only when every target drops all of them or none of
   them: two selectors share a list only when the same targets drop each.

   [reach] has a case for every node, so a new one is decided here. A standard
   pseudo-class or pseudo-element names its BCD key, and the web-features data
   ({!Support.engine_implements}) answers for each target; it records a partial
   implementation as none, which can only keep two selectors apart. A node the
   data does not carry, and a pseudo-class cascade does not know, has no answer
   and lists with nothing. A prefixed name has no key of its own and is decided
   from BCD's prefix entries, Firefox's selector parser (Servo's
   gecko/selector_parser.rs and pseudo_element_definition.mako.rs, ESR 128) and
   probes of Chromium 151 and WebKit 26.5: - every target parses a [::-webkit-]
   pseudo-element, known or not: Chromium and WebKit take any such name as a
   custom pseudo-element, and Firefox has parsed an unknown one as
   [UnknownWebkit] since 91 at least; - every target parses [:-webkit-autofill],
   Firefox since 86; - Chromium and WebKit drop the [-moz-] names, which Firefox
   reads, Firefox drops [:-webkit-any()], [:-webkit-full-screen] and the
   scrollbar states, and WebKit drops [:-webkit-full-screen-ancestor] too; -
   every target drops a [-ms-] name, a bare [:-webkit-any], a page pseudo-class
   or a CSS Modules scope marker in a style rule, and the column and
   shadow-piercing combinators. *)
type dropped = {
  chrome : bool;
  firefox : bool;
  safari : bool;
  ios_safari : bool;
}

let none =
  { chrome = false; firefox = false; safari = false; ios_safari = false }

let all = { chrome = true; firefox = true; safari = true; ios_safari = true }
let gecko_only = { all with firefox = false }
let blink_and_webkit = { none with firefox = true }

let union a b =
  {
    chrome = a.chrome || b.chrome;
    firefox = a.firefox || b.firefox;
    safari = a.safari || b.safari;
    ios_safari = a.ios_safari || b.ios_safari;
  }

let equal_dropped a b =
  Bool.equal a.chrome b.chrome
  && Bool.equal a.firefox b.firefox
  && Bool.equal a.safari b.safari
  && Bool.equal a.ios_safari b.ios_safari

type reach = Dropped of dropped | Key of string | Unknown

let webkit_pseudo_element name =
  String.length name > 8
  && String.equal (String.lowercase_ascii (String.sub name 0 8)) "-webkit-"

let reach : Selector.t -> reach = function
  | Element _ | Class _ | Id _ | Universal _ | Compound _ | List _ | Relative _
    ->
      Dropped none
  | Combined (_, (Descendant | Child | Next_sibling | Subsequent_sibling), _) ->
      Dropped none
  | Attribute (_, _, _, None) -> Dropped none
  | Attribute (_, _, _, Some Insensitive) ->
      Key "css.selectors.attribute.case_insensitive_modifier"
  | Attribute (_, _, _, Some Sensitive) ->
      Key "css.selectors.attribute.case_sensitive_modifier"
  | Nesting -> Key "css.selectors.nesting"
  | Hover -> Key "css.selectors.hover"
  | Active -> Key "css.selectors.active"
  | Focus -> Key "css.selectors.focus"
  | Focus_visible -> Key "css.selectors.focus-visible"
  | Focus_within -> Key "css.selectors.focus-within"
  | Target -> Key "css.selectors.target"
  | Link -> Key "css.selectors.link"
  | Visited -> Key "css.selectors.visited"
  | Any_link -> Key "css.selectors.any-link"
  | Local_link -> Key "css.selectors.local-link"
  | Target_within -> Key "css.selectors.target-within"
  | Scope -> Key "css.selectors.scope"
  | Root -> Key "css.selectors.root"
  | Empty -> Key "css.selectors.empty"
  | First_child -> Key "css.selectors.first-child"
  | Last_child -> Key "css.selectors.last-child"
  | Only_child -> Key "css.selectors.only-child"
  | First_of_type -> Key "css.selectors.first-of-type"
  | Last_of_type -> Key "css.selectors.last-of-type"
  | Only_of_type -> Key "css.selectors.only-of-type"
  | Enabled -> Key "css.selectors.enabled"
  | Disabled -> Key "css.selectors.disabled"
  | Read_only -> Key "css.selectors.read-only"
  | Read_write -> Key "css.selectors.read-write"
  | Placeholder_shown -> Key "css.selectors.placeholder-shown"
  | Default -> Key "css.selectors.default"
  | Checked -> Key "css.selectors.checked"
  | Indeterminate -> Key "css.selectors.indeterminate"
  | Blank -> Key "css.selectors.blank"
  | Valid -> Key "css.selectors.valid"
  | Invalid -> Key "css.selectors.invalid"
  | In_range -> Key "css.selectors.in-range"
  | Out_of_range -> Key "css.selectors.out-of-range"
  | Required -> Key "css.selectors.required"
  | Optional -> Key "css.selectors.optional"
  | User_invalid -> Key "css.selectors.user-invalid"
  | User_valid -> Key "css.selectors.user-valid"
  | Inert -> Key "css.selectors.inert"
  | Autofill -> Key "css.selectors.autofill"
  | Fullscreen -> Key "css.selectors.fullscreen"
  | Modal -> Key "css.selectors.modal"
  | Picture_in_picture -> Key "css.selectors.picture-in-picture"
  | Playing -> Key "css.selectors.playing"
  | Paused -> Key "css.selectors.paused"
  | Seeking -> Key "css.selectors.seeking"
  | Buffering -> Key "css.selectors.buffering"
  | Stalled -> Key "css.selectors.stalled"
  | Muted -> Key "css.selectors.muted"
  | Volume_locked -> Key "css.selectors.volume-locked"
  | Future -> Key "css.selectors.future"
  | Past -> Key "css.selectors.past"
  | Current -> Key "css.selectors.current"
  | Popover_open -> Key "css.selectors.popover-open"
  | Open -> Key "css.selectors.open"
  | Defined -> Key "css.selectors.defined"
  | Before -> Key "css.selectors.before"
  | After -> Key "css.selectors.after"
  | First_letter -> Key "css.selectors.first-letter"
  | First_line -> Key "css.selectors.first-line"
  | Backdrop -> Key "css.selectors.backdrop"
  | Marker -> Key "css.selectors.marker"
  | Placeholder -> Key "css.selectors.placeholder"
  | Selection -> Key "css.selectors.selection"
  | Target_text -> Key "css.selectors.target-text"
  | Spelling_error -> Key "css.selectors.spelling-error"
  | Grammar_error -> Key "css.selectors.grammar-error"
  | File_selector_button -> Key "css.selectors.file-selector-button"
  | Details_content -> Key "css.selectors.details-content"
  | Is _ -> Key "css.selectors.is"
  | Where _ -> Key "css.selectors.where"
  | Not [ _ ] -> Key "css.selectors.not"
  | Not _ -> Key "css.selectors.not.selector_list"
  | Has _ -> Key "css.selectors.has"
  | Current_of _ -> Key "css.selectors.current"
  | Nth_child (_, None) -> Key "css.selectors.nth-child"
  | Nth_child (_, Some _) -> Key "css.selectors.nth-child.of_syntax"
  | Nth_last_child (_, None) -> Key "css.selectors.nth-last-child"
  | Nth_last_child (_, Some _) -> Key "css.selectors.nth-last-child.of_syntax"
  | Nth_of_type (_, None) -> Key "css.selectors.nth-of-type"
  | Nth_of_type (_, Some _) -> Key "css.selectors.nth-of-type.of_syntax"
  | Nth_last_of_type (_, None) -> Key "css.selectors.nth-last-of-type"
  | Nth_last_of_type (_, Some _) ->
      Key "css.selectors.nth-last-of-type.of_syntax"
  | Nth_col _ -> Key "css.selectors.nth-col"
  | Nth_last_col _ -> Key "css.selectors.nth-last-col"
  | Dir _ -> Key "css.selectors.dir"
  | Lang langs when List.exists (fun l -> String.contains l '*') langs ->
      Key "css.selectors.lang.wildcards"
  | Lang [ _ ] -> Key "css.selectors.lang"
  | Lang _ -> Key "css.selectors.lang.argument_list"
  | Host None -> Key "css.selectors.host"
  | Host (Some _) -> Key "css.selectors.hostfunction"
  | Host_context _ -> Key "css.selectors.host-context"
  | State _ -> Key "css.selectors.state"
  | Active_view_transition -> Key "css.selectors.active-view-transition"
  | Active_view_transition_type _ ->
      Key "css.selectors.active-view-transition-type"
  | Heading -> Key "css.selectors.heading"
  | Part _ -> Key "css.selectors.part"
  | Slotted _ -> Key "css.selectors.slotted"
  | Cue None -> Key "css.selectors.cue"
  | Cue (Some _) -> Key "css.selectors.cue.selector_argument"
  | Cue_region _ -> Key "css.selectors.cue-region"
  | Highlight _ -> Key "css.selectors.highlight"
  | View_transition -> Key "css.selectors.view-transition"
  | View_transition_group _ -> Key "css.selectors.view-transition-group"
  | View_transition_image_pair _ ->
      Key "css.selectors.view-transition-image-pair"
  | View_transition_old _ -> Key "css.selectors.view-transition-old"
  | View_transition_new _ -> Key "css.selectors.view-transition-new"
  | Webkit_autofill | Webkit_input_placeholder | Webkit_scrollbar _
  | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
  | Webkit_details_marker ->
      Dropped none
  | Unknown_pseudo_element name when webkit_pseudo_element name -> Dropped none
  | Moz_focusring | Moz_any_call _ | Moz_placeholder | Moz_ui_invalid
  | Moz_ui_valid | Moz_full_screen ->
      Dropped gecko_only
  | Webkit_any_call _ | Webkit_full_screen | Scrollbar_state _ ->
      Dropped blink_and_webkit
  | Webkit_full_screen_ancestor ->
      Dropped { none with firefox = true; safari = true; ios_safari = true }
  | Combined (_, (Column | Shadow_piercing | Shadow_deep), _)
  | Ms_input_placeholder | Webkit_any | Left | Right | First | Local_scope
  | Local_call _ | Global_scope | Global_call _ ->
      Dropped all
  | Unknown_pseudo_class _ | Unknown_pseudo_class_call _
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ ->
      Unknown

let engines =
  [
    (Support.Chrome, Support.evergreen.chrome);
    (Support.Firefox, Support.evergreen.firefox);
    (Support.Safari, Support.evergreen.safari);
    (Support.Ios_safari, Support.evergreen.ios_safari);
  ]

let drops key (engine, version) =
  match Support.engine_implements engine version key with
  | Some true -> Some false
  | Some false -> Some true
  | None -> None

let read_dropped_by_key key =
  match List.map (drops key) engines with
  | [ Some chrome; Some firefox; Some safari; Some ios_safari ] ->
      Some { chrome; firefox; safari; ios_safari }
  | _ -> None

(* Every selector key the data carries, read once: a grouping pass asks about
   each rule's selector many times. *)
let selector_keys =
  lazy
    (let keys = Support.keys_under "css.selectors." in
     let table = Hashtbl.create (List.length keys) in
     List.iter
       (fun key -> Hashtbl.replace table key (read_dropped_by_key key))
       keys;
     table)

let dropped_by_key key =
  match Hashtbl.find_opt (Lazy.force selector_keys) key with
  | Some dropped -> dropped
  | None -> read_dropped_by_key key

(* The union of what drops each node of [sel], or [None] when [reach] has no
   answer for one. *)
let rec dropped_by_nodes acc sel =
  match acc with
  | None -> None
  | Some d -> (
      match reach sel with
      | Unknown -> None
      | Dropped e -> dropped_by_children (Some (union d e)) sel
      | Key key -> (
          match dropped_by_key key with
          | None -> None
          | Some e -> dropped_by_children (Some (union d e)) sel))

(* Selectors 4 sec. 4.2 and 4.3 drop a selector a browser does not parse from a
   forgiving [:is()] or [:where()] alone, so what drops their arguments drops
   nothing outside them. *)
and dropped_by_children acc : Selector.t -> dropped option = function
  | Is _ | Where _ -> acc
  | Compound xs
  | List xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs
  | Host_context xs
  | Local_call xs
  | Global_call xs
  | Nth_child (_, Some xs)
  | Nth_last_child (_, Some xs)
  | Nth_of_type (_, Some xs)
  | Nth_last_of_type (_, Some xs)
  | Host (Some xs)
  | Cue (Some xs)
  | Cue_region (Some xs) ->
      List.fold_left dropped_by_nodes acc xs
  | Combined (a, _, b) -> dropped_by_nodes (dropped_by_nodes acc a) b
  | Relative (_, b) -> dropped_by_nodes acc b
  | _ -> acc

(* Everything [compatible] reads off a selector: [Uniform d] when the targets
   [d] drop each of its branches, [Apart] when they drop different branches or
   [reach] has no answer for one, so that it lists with nothing. *)
type compatibility = Uniform of dropped | Apart

let dropped_by sel =
  match Selector.as_list sel with
  | None | Some [] -> dropped_by_nodes (Some none) sel
  | Some (first :: rest) -> (
      match dropped_by_nodes (Some none) first with
      | None -> None
      | Some d ->
          let same branch =
            match dropped_by_nodes (Some none) branch with
            | Some e -> equal_dropped d e
            | None -> false
          in
          if List.for_all same rest then Some d else None)

let compatibility sel =
  match dropped_by sel with Some d -> Uniform d | None -> Apart

let listable sel =
  match compatibility sel with Uniform _ -> true | Apart -> false

let equal_compatibility a b =
  match (a, b) with
  | Uniform a, Uniform b -> equal_dropped a b
  | (Uniform _ | Apart), _ -> false

let compatible sel1 sel2 =
  equal_compatibility (compatibility sel1) (compatibility sel2)

let key sel =
  match Selector.as_list sel with
  | Some xs -> List.sort compare xs
  | None -> [ sel ]

let selector_list = function
  | [ s ] -> s
  | sels ->
      let flatten = function Selector.List xs -> xs | s -> [ s ] in
      Selector.List (List.concat_map flatten sels)

(* [compatible] is an equivalence, so a group holds only compatible pairs
   exactly when every member is compatible with the first: the group remembers
   that one fact and a candidate joins by reading its own once. *)
type run = Empty | Run of dropped | Broken

let empty_run = Empty

let extend_run run sel =
  match (run, compatibility sel) with
  | Broken, _ | _, Apart -> Broken
  | Empty, Uniform d -> Run d
  | Run d, Uniform e -> if equal_dropped d e then run else Broken

let run_compatible = function Broken -> false | Empty | Run _ -> true

let all_compatible sels =
  let rec loop run = function
    | [] -> true
    | sel :: rest ->
        let run = extend_run run sel in
        run_compatible run && loop run rest
  in
  loop empty_run sels

let rec declaration_lists_equal ~same d1 d2 =
  match (d1, d2) with
  | [], [] -> true
  | a :: rest_a, b :: rest_b ->
      same a b && declaration_lists_equal ~same rest_a rest_b
  | _ -> false

let declarations_equal ~same d1 d2 =
  d1 == d2 || declaration_lists_equal ~same d1 d2
