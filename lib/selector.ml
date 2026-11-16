(** CSS Selectors - types and pretty printing *)

include Selector_intf

let pp_component_values = Values.pp_component_values
let read_component_values = Values.read_component_values

include Selector_make
include Selector_pseudo
include Selector_reader
include Selector_printer
open Common

let list_map_preserve = List.map_preserve
let list_same = List.same

(** Recursively map over all selectors in the tree. [f] is applied bottom-up; a
    node whose children are unchanged and for which [f] returns its argument
    keeps its physical identity. *)
let rec map f node =
  let lst ctor xs =
    let xs' = list_map_preserve (map f) xs in
    if xs' == xs then node else ctor xs'
  in
  let node' =
    match node with
    | Combined (left, combinator, right) ->
        let left' = map f left and right' = map f right in
        if left' == left && right' == right then node
        else Combined (left', combinator, right')
    | Relative (combinator, right) ->
        let right' = map f right in
        if right' == right then node else Relative (combinator, right')
    | Compound xs -> lst (fun xs -> Compound xs) xs
    | Where xs -> lst (fun xs -> Where xs) xs
    | Is xs -> lst (fun xs -> Is xs) xs
    | Not xs -> lst (fun xs -> Not xs) xs
    | Has xs -> lst (fun xs -> Has xs) xs
    | Moz_any_call xs -> lst (fun xs -> Moz_any_call xs) xs
    | Webkit_any_call xs -> lst (fun xs -> Webkit_any_call xs) xs
    | List xs -> lst (fun xs -> List xs) xs
    | Nth_child (nth, Some xs) -> lst (fun xs -> Nth_child (nth, Some xs)) xs
    | Nth_last_child (nth, Some xs) ->
        lst (fun xs -> Nth_last_child (nth, Some xs)) xs
    | Nth_of_type (nth, Some xs) ->
        lst (fun xs -> Nth_of_type (nth, Some xs)) xs
    | Nth_last_of_type (nth, Some xs) ->
        lst (fun xs -> Nth_last_of_type (nth, Some xs)) xs
    | Host (Some xs) -> lst (fun xs -> Host (Some xs)) xs
    | Current_of xs -> lst (fun xs -> Current_of xs) xs
    | Host_context xs -> lst (fun xs -> Host_context xs) xs
    | Slotted xs -> lst (fun xs -> Slotted xs) xs
    | Cue (Some xs) -> lst (fun xs -> Cue (Some xs)) xs
    | Cue_region (Some xs) -> lst (fun xs -> Cue_region (Some xs)) xs
    | other -> other
  in
  f node'

(* Dedup and sort an unordered selector list by minified-printed form so that
   permutations of the same alternatives collapse to a single canonical AST. *)
let canonicalize_unordered_list selectors =
  let seen = Hashtbl.create (List.length selectors) in
  let uniq =
    List.filter_map
      (fun s ->
        let key = Pp.to_string ~minify:true pp s in
        if Hashtbl.mem seen key then None
        else (
          Hashtbl.add seen key ();
          Some (key, s)))
      selectors
  in
  List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) uniq |> List.map snd

(* CSS Selectors 4 3.5: when the universal selector [*] is not the only
   component of a compound, the [*] may be omitted. Namespaced universals
   ([ns|*], [*|*]) carry namespace information and are preserved. *)
let drop_redundant_universal = function
  | [ _ ] as singleton -> singleton
  | components ->
      let kept =
        List.filter (function Universal None -> false | _ -> true) components
      in
      if kept = [] then components else kept

(* CSS Selectors 4 3.5: a compound selector that "contains a type selector or
   universal selector [...] must come first in the sequence". So a rewrite that
   splices a wrapped selector into the surrounding compound has to leave a
   type-bearing argument wrapped: spliced, the two names fuse and [.a:is(code)]
   reads as [.acode], a class nobody wrote. *)
let rec carries_type_selector = function
  | Element _ | Universal _ -> true
  | Compound parts -> List.exists carries_type_selector parts
  | _ -> false

(* Which elements [HTML] gives each state pseudo-class pair to. The three sets
   differ. [:enabled] and [:disabled] split every form control plus [optgroup],
   [option] and [fieldset] (HTML sec. 4.15, sec. 4.16.3). [:valid] and
   [:invalid] split a [form] and a [fieldset] whole, but reach a form control
   only while it is a candidate for constraint validation, which a disabled,
   readonly or [type=hidden] one is not. [:required] and [:optional] split
   [select] and [textarea]; [:optional] wants an [input] "to which the required
   attribute applies", and it does not apply in e.g. the Hidden state. *)
let enabled_carriers =
  [ "button"; "fieldset"; "input"; "optgroup"; "option"; "select"; "textarea" ]

let validity_carriers = [ "fieldset"; "form" ]
let optionality_carriers = [ "select"; "textarea" ]

let state_pair = function
  | Enabled -> Some (Disabled, enabled_carriers)
  | Disabled -> Some (Enabled, enabled_carriers)
  | Valid -> Some (Invalid, validity_carriers)
  | Invalid -> Some (Valid, validity_carriers)
  | Required -> Some (Optional, optionality_carriers)
  | Optional -> Some (Required, optionality_carriers)
  | _ -> None

(* Whether every element [sel] can select is one of [carriers]. A type selector
   proves it, and [:is()]/[:where()] prove it when every branch does; a class,
   an attribute, a negation or [:has()] constrains something other than the
   subject's element type, so it proves nothing. Selectors 4 sec. 3.5: the
   subject of a complex selector is its rightmost compound. A namespaced type
   selector is left out: the prefix binds to a namespace declared elsewhere in
   the stylesheet, and these names are the HTML ones. *)
let rec selects_only carriers = function
  | Element (None, name) -> List.mem (String.lowercase_ascii name) carriers
  | Compound parts -> List.exists (selects_only carriers) parts
  | Is branches | Where branches ->
      branches <> [] && List.for_all (selects_only carriers) branches
  | Combined (_, _, right) -> selects_only carriers right
  | _ -> false

(* CSS Selectors 4 sec. 12.1.1, 12.3.1 and 12.3.3: an element outside a pair's
   set matches neither half, so [X:not(:enabled)] is [X:disabled] only inside a
   compound that proves its subject carries the state. Which elements those are
   is a [HTML] fact rather than one the CSS text proves, so the caller asks only
   outside [enforce_spec]. *)
let fold_state_negations components =
  list_map_preserve
    (function
      | Not [ state ] as component -> (
          match state_pair state with
          | Some (complement, carriers)
            when List.exists (selects_only carriers) components ->
              complement
          | _ -> component)
      | component -> component)
    components

(* [map] rewrites a compound's components before the compound itself, so the
   [Is] branch below has already spliced a single-argument [:is()] into this
   list. A component the reader refuses after the pseudo-element can only have
   arrived that way, so the compound puts the wrapper back: Selectors 4 sec. 16
   builds a pseudo-compound out of [<pseudo-element-selector>
   <pseudo-class-selector>*] and sec. 3.6.3 makes the pseudo-class list per
   pseudo-element. *)
let rewrap_pseudo_compound components =
  let rec loop seen = function
    | [] -> []
    | c :: rest when is_pseudo_element c -> c :: loop (Some c) rest
    | c :: rest ->
        let c =
          match seen with
          | Some pe when not (is_pe_action c && pseudo_element_allows pe c) ->
              Is [ c ]
          | _ -> c
        in
        c :: loop seen rest
  in
  if List.exists is_pseudo_element components then loop None components
  else components

(* A compound in canonical form: the universal a longer compound implies is gone
   (Selectors 4 sec. 3.5) and a pseudo-element keeps its own compound. *)
let canonical_compound components =
  match rewrap_pseudo_compound (drop_redundant_universal components) with
  | [ single ] -> single
  | components -> Compound components

(* CSS Selectors 4 sec. 4.2: a single-argument [:is(s)] matches the same
   elements as [s] with the same specificity, so it reduces to [s]. Sound only
   when [s] is a single compound: a combinator ([Combined] / [Relative]) or a
   [List] makes [:is()] a grouping boundary that cannot be spliced into the
   surrounding compound. A type or universal selector cannot be spliced either:
   this rewrite is node-local and cannot see whether the [:is()] heads its
   compound, and only there may such a selector stand
   ([carries_type_selector]). *)
let canonicalize_is node selectors =
  let sorted = canonicalize_unordered_list selectors in
  match sorted with
  | [ single ]
    when match single with
         | Combined _ | Relative _ | List _ -> false
         | _ -> not (carries_type_selector single) ->
      single
  (* Sec. 8.1 defines [:any-link] as [:is(:link, :visited)], at the same
     specificity. *)
  | [ Link; Visited ] -> Any_link
  | _ -> if list_same sorted selectors then node else Is sorted

(* CSS Selectors 4 sec. 7.1 leaves directionality to the document language, so
   CSS alone does not make [ltr] and [rtl] a partition: an element the language
   gives no directionality matches neither. [HTML] makes it one for every
   element in an HTML document, which is the host fact [enforce_spec] drops. *)
let canonicalize_not ~enforce_spec node selectors =
  match selectors with
  (* Sec. 4.3: [:not(:not(s1, ...))] matches what [:is(s1, ...)] matches, and
     both weigh their most specific argument. *)
  | [ Not inner ] -> canonicalize_is (Is inner) inner
  | [ Dir "ltr" ] when not enforce_spec -> Dir "rtl"
  | [ Dir "rtl" ] when not enforce_spec -> Dir "ltr"
  | _ ->
      let sorted = canonicalize_unordered_list selectors in
      if list_same sorted selectors then node else Not sorted

(* Canonicalise so selectors denoting the same thing are structurally equal:
   drop the implied [*] from a multi-part compound ([*.foo] -> [.foo]), collapse
   a one-part compound, and dedup/sort selector-list alternatives by printed
   form (both [List] and the set-based [:is]/[:where]/[:not]/[:has] lists, incl.
   the [:-moz-any]/[:-webkit-any] aliases, whose order Selectors 4 makes
   irrelevant to matching and specificity). [map] hands each node over without
   saying where it sits, so every rewrite below has to be sound in any position;
   the one that needs the top of a rule selector is [canonicalize]. *)
let rec canonicalize_nodes ~enforce_spec sel =
  map
    (fun node ->
      let canon ctor selectors =
        let sorted = canonicalize_unordered_list selectors in
        if list_same sorted selectors then node else ctor sorted
      in
      match node with
      | Compound components ->
          canonicalize_compound ~enforce_spec node components
      | Combined ((Combined _ | Relative _), _, _) -> reshape node
      | List selectors -> canon (fun xs -> List xs) selectors
      | Where selectors -> canon (fun xs -> Where xs) selectors
      | Is selectors -> canonicalize_is node selectors
      | Not selectors -> canonicalize_not ~enforce_spec node selectors
      | Has selectors -> canon (fun xs -> Has xs) selectors
      | Moz_any_call selectors -> canon (fun xs -> Moz_any_call xs) selectors
      | Webkit_any_call selectors ->
          canon (fun xs -> Webkit_any_call xs) selectors
      (* Sec. 14: the first and last index are the [:first-*] and [:last-*]
         keywords, at the same specificity. *)
      | Nth_child (Index 1, None) -> First_child
      | Nth_last_child (Index 1, None) -> Last_child
      | Nth_of_type (Index 1, None) -> First_of_type
      | Nth_last_of_type (Index 1, None) -> Last_of_type
      | Nth_child (nth, Some selectors) ->
          canon (fun xs -> Nth_child (nth, Some xs)) selectors
      | Nth_last_child (nth, Some selectors) ->
          canon (fun xs -> Nth_last_child (nth, Some xs)) selectors
      | Nth_of_type (nth, Some selectors) ->
          canon (fun xs -> Nth_of_type (nth, Some xs)) selectors
      | Nth_last_of_type (nth, Some selectors) ->
          canon (fun xs -> Nth_last_of_type (nth, Some xs)) selectors
      | other -> other)
    sel

(* A spliced [:is()] argument keeps its own compound behind a pseudo-element,
   where [canonical_compound] wraps it whole; anywhere else the compound takes
   the reader's shape, whose compounds are built here, after [map] visited the
   parts, so they are canonicalised again: [svg *] then [:where(.dark)] is [svg
   :where(.dark)], as reading that text gives. *)
and canonicalize_compound ~enforce_spec node components =
  let folded =
    if enforce_spec then components else fold_state_negations components
  in
  let canonical =
    match canonical_compound folded with
    | Compound components' when list_same components' components -> node
    | canonical -> canonical
  in
  match reshape canonical with
  | reshaped when reshaped != canonical ->
      canonicalize_nodes ~enforce_spec reshaped
  | _ -> canonical

let is_ sels = Is sels
let has sels = Has sels
let not selectors = Not selectors
let nth_child ?of_ nth = Nth_child (nth, of_)
let host ?selectors () = Host selectors

(* ========================= *)
(* Analysis helpers          *)
(* ========================= *)

let has_focus sel = any (function Focus -> true | _ -> false) sel

let has_focus_within sel =
  any (function Focus_within -> true | _ -> false) sel

let has_focus_visible sel =
  any (function Focus_visible -> true | _ -> false) sel

let zero_specificity = { ids = 0; classes = 0; elements = 0 }

let add_specificity a b =
  {
    ids = a.ids + b.ids;
    classes = a.classes + b.classes;
    elements = a.elements + b.elements;
  }

let compare_specificity a b =
  match compare a.ids b.ids with
  | 0 -> (
      match compare a.classes b.classes with
      | 0 -> compare a.elements b.elements
      | n -> n)
  | n -> n

let max_specificity xs =
  List.fold_left
    (fun acc x -> if compare_specificity x acc > 0 then x else acc)
    zero_specificity xs

let rec specificity = function
  | Id _ -> { ids = 1; classes = 0; elements = 0 }
  | Class _ | Attribute _ | Hover | Active | Focus | Focus_visible
  | Focus_within | Target | Link | Visited | Any_link | Local_link
  | Target_within | Scope | Root | Empty | First_child | Last_child | Only_child
  | First_of_type | Last_of_type | Only_of_type | Enabled | Disabled | Read_only
  | Read_write | Placeholder_shown | Default | Checked | Indeterminate | Blank
  | Valid | Invalid | In_range | Out_of_range | Required | Optional
  | User_invalid | User_valid | Inert | Autofill | Fullscreen | Modal
  | Picture_in_picture | Left | Right | First | Defined | Playing | Paused
  | Seeking | Buffering | Stalled | Muted | Volume_locked | Future | Past
  | Current | Popover_open | Open | Moz_focusring | Webkit_any | Webkit_autofill
  | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ | Moz_placeholder
  | Webkit_input_placeholder | Ms_input_placeholder | Moz_ui_invalid
  | Moz_ui_valid | Scrollbar_state _
  | Webkit_scrollbar Scrollbar
  | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
  | Webkit_details_marker | Details_content | Nth_col _ | Nth_last_col _ | Dir _
  | Lang _ | State _ | Active_view_transition | Active_view_transition_type _
  | Heading | Local_scope | Global_scope ->
      { ids = 0; classes = 1; elements = 0 }
  | Element _ -> { ids = 0; classes = 0; elements = 1 }
  | Universal _ | Nesting -> zero_specificity
  | Before _ | After _ | First_letter _ | First_line _ | Backdrop | Marker
  | Placeholder | Selection | Target_text | Spelling_error | Grammar_error
  | File_selector_button | Part _ | View_transition | View_transition_group _
  | View_transition_image_pair _ | View_transition_old _ | View_transition_new _
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _
  (* Sec. 17 counts a pseudo-element in C. [::-webkit-scrollbar] is read with
     one colon too and weighs a class above, as every [pseudo_vendor_idents]
     name does; the parts are [::]-only. *)
  | Webkit_scrollbar (Button | Track | Track_piece | Thumb | Corner | Resizer)
  | Cue None
  | Cue_region None ->
      { ids = 0; classes = 0; elements = 1 }
  | Where _ -> zero_specificity
  | Is xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Not xs
  | Has xs
  | Current_of xs ->
      xs |> List.map specificity |> max_specificity
  | Nth_child (_, of_)
  | Nth_last_child (_, of_)
  | Nth_of_type (_, of_)
  | Nth_last_of_type (_, of_) ->
      add_specificity
        { ids = 0; classes = 1; elements = 0 }
        (match of_ with
        | None -> zero_specificity
        | Some xs -> xs |> List.map specificity |> max_specificity)
  | Host None -> { ids = 0; classes = 1; elements = 0 }
  | Host (Some xs) | Host_context xs ->
      add_specificity
        { ids = 0; classes = 1; elements = 0 }
        (xs |> List.map specificity |> max_specificity)
  | Slotted xs | Cue (Some xs) | Cue_region (Some xs) ->
      add_specificity
        { ids = 0; classes = 0; elements = 1 }
        (xs |> List.map specificity |> max_specificity)
  | Local_call xs | Global_call xs ->
      xs |> List.map specificity |> max_specificity
  | Highlight _ -> { ids = 0; classes = 0; elements = 1 }
  | Compound xs ->
      List.fold_left
        (fun acc sel -> add_specificity acc (specificity sel))
        zero_specificity xs
  | Combined (a, _, b) -> add_specificity (specificity a) (specificity b)
  | Relative (_, sel) -> specificity sel
  | List xs -> xs |> List.map specificity |> max_specificity

(* Selectors 4 sec. 4.2 replaces the specificity of [:is()] with that of its
   most specific argument, so splitting [:is(s1, s2, ...)] into the selector
   list [s1, s2, ...] holds the weight of every match only when the arguments
   already agree on one specificity. The arguments also have to be structurally
   simple: [&] is out because [specificity] reads [Nesting] as zero while CSS
   Nesting 1 sec. 3 weighs it as the most specific selector in the parent rule,
   so the equality below would pass on a weight it never measured. *)
let rec is_unwrap_safe_is_arg : t -> bool = function
  | Element _ | Class _ | Id _ | Universal _ | Attribute _ -> true
  | Compound parts -> List.for_all is_unwrap_safe_is_arg parts
  | _ -> false

let is_unwrap_safe selectors =
  List.for_all is_unwrap_safe_is_arg selectors
  &&
  match List.map specificity selectors with
  | [] -> false
  | s :: rest -> List.for_all (fun s' -> s' = s) rest

(* Sound only where the [:is()] is a whole rule selector, or a whole member of
   one top-level list: there the split lands in a selector list, which weighs
   each branch on its own. Nested inside a [Compound] or a [Combined] it is a
   grouping boundary and the split would change what matches. A lone argument
   goes further than [canonicalize_is] can: that one is node-local and has to
   leave a type or universal argument wrapped rather than splice it into a
   compound it cannot see, and here there is no compound to splice into. *)
let rec top_level_is_unwrap sel =
  match sel with
  | Is [ single ] when is_unwrap_safe_is_arg single -> single
  | Is selectors when is_unwrap_safe selectors -> List selectors
  | List selectors ->
      let expanded =
        List.concat_map
          (fun sel ->
            match top_level_is_unwrap sel with
            | List members -> members
            | other -> [ other ])
          selectors
      in
      if list_same expanded selectors then sel
      else List (canonicalize_unordered_list expanded)
  | _ -> sel

let canonicalize ?(enforce_spec = false) sel =
  top_level_is_unwrap (canonicalize_nodes ~enforce_spec sel)

let to_string ?minify t = Pp.to_string ?minify pp t
let to_buffer ?minify buf t = Pp.to_buffer ?minify buf pp t

let exists_class pred sel =
  any (function Class name -> pred name | _ -> false) sel

let rec first_class = function
  | Class n -> Some n
  | Compound xs -> List.find_map first_class xs
  | Combined (a, _, _) -> first_class a
  | List (h :: _) -> first_class h
  | Is xs
  | Where xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs -> (
      match xs with [] -> None | h :: _ -> first_class h)
  | Cue (Some (h :: _)) | Cue_region (Some (h :: _)) -> first_class h
  | Part _ -> None
  | _ -> None

let contains_modifier_colon sel =
  exists_class (fun name -> String.contains name ':') sel

(** Check if selector contains :where(.group) - used for group-* modifiers.
    Note: can't use 'any' because it transforms Where to List before calling p.
*)
let is_group_class = function
  | Class s ->
      s = "group" || (String.length s > 6 && String.sub s 0 6 = "group/")
  | _ -> false

let rec has_group_marker = function
  | Where xs ->
      List.exists
        (function
          | sel when is_group_class sel -> true
          | Compound cs -> List.exists is_group_class cs
          | other -> has_group_marker other)
        xs
  | Compound xs -> List.exists has_group_marker xs
  | Combined (a, _, b) -> has_group_marker a || has_group_marker b
  | Relative (_, b) -> has_group_marker b
  | List xs -> List.exists has_group_marker xs
  | Cue (Some xs) | Cue_region (Some xs) -> List.exists has_group_marker xs
  | Is xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs ->
      List.exists has_group_marker xs
  | _ -> false

(** Check if selector contains :where(.peer) - used for peer-* modifiers *)
let is_peer_class = function
  | Class s -> s = "peer" || (String.length s > 5 && String.sub s 0 5 = "peer/")
  | _ -> false

let rec has_peer_marker = function
  | Where xs ->
      List.exists
        (function
          | sel when is_peer_class sel -> true
          | Compound cs -> List.exists is_peer_class cs
          | other -> has_peer_marker other)
        xs
  | Compound xs -> List.exists has_peer_marker xs
  | Combined (a, _, b) -> has_peer_marker a || has_peer_marker b
  | Relative (_, b) -> has_peer_marker b
  | List xs -> List.exists has_peer_marker xs
  | Cue (Some xs) | Cue_region (Some xs) -> List.exists has_peer_marker xs
  | Is xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs ->
      List.exists has_peer_marker xs
  | _ -> false

(** Check if selector uses the :is(:where(...)) pattern used by group-* and
    peer-* variants. *)
let has_is_where_pattern sel = has_group_marker sel || has_peer_marker sel

(** A "newer" pseudo-class with limited browser support: it must not be combined
    in a selector list with an [:is(:where())] variant, since a browser that
    lacks it drops the whole rule, whereas the forgiving variant would survive.
*)
let is_newer_pseudo_class = function
  | User_valid | User_invalid -> true
  | _ -> false

(** Check if a selector directly uses a newer pseudo-class (not nested inside
    :is()/:where() which provides forgiving parsing). *)
let rec has_newer_pseudo_class = function
  | User_valid | User_invalid -> true
  | Compound xs -> List.exists has_newer_pseudo_class xs
  | Combined (a, _, b) -> has_newer_pseudo_class a || has_newer_pseudo_class b
  | Relative (_, b) -> has_newer_pseudo_class b
  | List xs -> List.exists has_newer_pseudo_class xs
  | Current_of xs -> List.exists has_newer_pseudo_class xs
  (* Stop recursion at forgiving selectors -- :is()/:where() have forgiving
     parsing, so newer pseudo-classes inside them don't cause the whole rule to
     fail *)
  | Is _ | Where _ | Moz_any_call _ | Webkit_any_call _ -> false
  | Not xs | Has xs -> List.exists has_newer_pseudo_class xs
  | _ -> false

let modifier_prefix sel =
  match first_class sel with
  | Option.None -> Option.None
  | Option.Some class_name -> (
      match String.index_opt class_name ':' with
      | Option.Some idx -> Option.Some (String.sub class_name 0 (idx + 1))
      | Option.None -> Option.None)

let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
let ( || ) s1 s2 = combine s1 Column s2
let compare (a : t) b = Stdlib.compare a b
