module String_set = Set.Make (String)

type attr_key = Selector.ns option * Selector.attr_name

type attr_fact = {
  key : attr_key;
  matcher : Selector.attribute_match;
  flag : Selector.attr_flag option;
}

type nth_fact = Index of int | Odd | Even

type t = {
  ids : String_set.t;
  classes : String_set.t;
  element : string option;
  attrs : attr_fact list;
  nth : nth_fact list;
  not_ids : String_set.t;
  not_classes : String_set.t;
  not_elements : String_set.t;
  not_attrs : attr_fact list;
  not_nth : nth_fact list;
  pseudo_element : Selector.t option;
  parent : t option;
  complex : bool;
}

let empty =
  {
    ids = String_set.empty;
    classes = String_set.empty;
    element = None;
    attrs = [];
    nth = [];
    not_ids = String_set.empty;
    not_classes = String_set.empty;
    not_elements = String_set.empty;
    not_attrs = [];
    not_nth = [];
    pseudo_element = None;
    parent = None;
    complex = false;
  }

let mark_complex s = if s.complex then s else { s with complex = true }

(* Pseudo-elements as far as overlap analysis is concerned: anything that
   identifies a generated box distinct from its originating element.
   Element-tied pseudo-classes (Hover, Focus, ...) do not appear here - they
   tighten which elements match but do not change the subject identity. *)
let is_pseudo_element = function
  | Selector_intf.Before _ | After _ | First_letter _ | First_line _ | Backdrop
  | Marker | Placeholder | Selection | Target_text | Spelling_error
  | Grammar_error | File_selector_button | Details_content | Moz_placeholder
  | Webkit_input_placeholder | Ms_input_placeholder | Webkit_scrollbar
  | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
  | Webkit_details_marker | View_transition | View_transition_group _
  | View_transition_image_pair _ | View_transition_old _ | View_transition_new _
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ | Part _
  | Slotted _ | Cue _ | Cue_region _ | Highlight _ ->
      true
  | _ -> false

let add_nth nth s = { s with nth = nth :: s.nth; complex = true }

let nth_of_selector = function
  | Selector_intf.First_child | Selector_intf.Only_child -> Some (Index 1)
  | Selector_intf.Nth_child (Selector_intf.Odd, None) -> Some Odd
  | Selector_intf.Nth_child (Selector_intf.Even, None) -> Some Even
  | Selector_intf.Nth_child (Selector_intf.Index n, None) when n >= 1 ->
      Some (Index n)
  | Selector_intf.Nth_child (Selector_intf.An_plus_b (2, 1), None) -> Some Odd
  | Selector_intf.Nth_child (Selector_intf.An_plus_b (2, 0), None)
  | Selector_intf.Nth_child (Selector_intf.An_plus_b (2, 2), None) ->
      Some Even
  | _ -> None

let add_not_simple s (sel : Selector.t) =
  match sel with
  | Selector_intf.Class c ->
      { s with not_classes = String_set.add c s.not_classes }
  | Selector_intf.Id i -> { s with not_ids = String_set.add i s.not_ids }
  | Selector_intf.Element (_, name) ->
      { s with not_elements = String_set.add name s.not_elements }
  | Selector_intf.Attribute (ns, attr_name, matcher, flag) ->
      {
        s with
        not_attrs = { key = (ns, attr_name); matcher; flag } :: s.not_attrs;
      }
  | sel -> (
      match nth_of_selector sel with
      | Some nth -> { s with not_nth = nth :: s.not_nth }
      | None -> s)

let add_not_selector s = function
  | ( Selector_intf.Class _ | Selector_intf.Id _ | Selector_intf.Element _
    | Selector_intf.Attribute _ ) as sel ->
      add_not_simple s sel
  | sel -> (
      match nth_of_selector sel with
      | Some _ -> add_not_simple s sel
      | None -> s)

let rec fold_simple s (sel : Selector.t) =
  match sel with
  | Class c -> { s with classes = String_set.add c s.classes }
  | Id i -> { s with ids = String_set.add i s.ids }
  | Element (_, name) -> (
      match s.element with
      | None -> { s with element = Some name }
      | Some prev when prev = name -> s
      | Some _ -> mark_complex s)
  | Universal _ -> s
  | Compound xs -> List.fold_left fold_simple s xs
  | sel when is_pseudo_element sel -> (
      (* Normalize legacy single-colon spellings ([:before] vs [::before]) so
         the two parses of the same pseudo-element compare equal. *)
      let canonical : Selector.t =
        match sel with
        | Before _ -> Before Double
        | After _ -> After Double
        | First_letter _ -> First_letter Double
        | First_line _ -> First_line Double
        | other -> other
      in
      match s.pseudo_element with
      | None -> { s with pseudo_element = Some canonical }
      | Some prev when Selector.equal prev canonical -> s
      | Some _ -> mark_complex s)
  | Attribute (ns, attr_name, matcher, flag) ->
      {
        s with
        attrs = { key = (ns, attr_name); matcher; flag } :: s.attrs;
        complex = true;
      }
  | Not xs -> { (List.fold_left add_not_selector s xs) with complex = true }
  | sel -> (
      match nth_of_selector sel with
      | Some nth -> add_nth nth s
      | None -> mark_complex s)

let clear_memo () = ()

let rec of_selector (sel : Selector.t) : t =
  match sel with
  | Combined (left, Child, right) ->
      let s = of_selector right in
      { s with parent = Some (of_selector left) }
  | Combined (_, _, right) -> of_selector right
  | Relative (_, right) -> of_selector right
  | List [ single ] -> of_selector single
  | List _ -> { empty with complex = true }
  | _ -> fold_simple empty sel

let exact_value = function
  | Selector.Exact s | Exact_quoted (s, _) -> Some s
  | _ -> None

let same_attr_key a b = a.key = b.key

let attr_flag_is_case_insensitive = function
  | Some Selector_intf.Insensitive -> true
  | _ -> false

let exact_attr_disjoint a b =
  match (exact_value a.matcher, exact_value b.matcher) with
  | Some x, Some y -> (
      match (a.flag, b.flag) with
      | Some Selector_intf.Sensitive, Some Selector_intf.Sensitive -> x <> y
      | _ -> String.lowercase_ascii x <> String.lowercase_ascii y)
  | _ -> false

let attrs_disjoint a b =
  List.exists
    (fun attr_a ->
      List.exists
        (fun attr_b ->
          same_attr_key attr_a attr_b && exact_attr_disjoint attr_a attr_b)
        b.attrs)
    a.attrs

let exact_attr_implies a b =
  same_attr_key a b
  &&
  match (exact_value a.matcher, exact_value b.matcher) with
  | Some x, Some y when attr_flag_is_case_insensitive b.flag ->
      String.lowercase_ascii x = String.lowercase_ascii y
  | Some x, Some y -> String.equal x y
  | _ -> false

let attr_negation_disjoint attrs not_attrs =
  List.exists
    (fun attr ->
      List.exists (fun not_attr -> exact_attr_implies attr not_attr) not_attrs)
    attrs

let nth_disjoint a b =
  match (a, b) with
  | Index x, Index y -> x <> y
  | Index x, Odd | Odd, Index x -> x mod 2 = 0
  | Index x, Even | Even, Index x -> x mod 2 <> 0
  | Odd, Even | Even, Odd -> true
  | Odd, Odd | Even, Even -> false

let nths_disjoint a b =
  List.exists (fun nth_a -> List.exists (nth_disjoint nth_a) b.nth) a.nth

let nth_implies a b =
  match (a, b) with
  | Index x, Index y -> x = y
  | Index x, Odd -> x mod 2 <> 0
  | Index x, Even -> x mod 2 = 0
  | Odd, Odd | Even, Even -> true
  | Odd, (Index _ | Even) | Even, (Index _ | Odd) -> false

let nth_negation_disjoint nth not_nth =
  List.exists
    (fun fact ->
      List.exists (fun not_fact -> nth_implies fact not_fact) not_nth)
    nth

let set_negation_disjoint required forbidden =
  not (String_set.disjoint required forbidden)

let element_negation_disjoint element forbidden =
  match element with Some el -> String_set.mem el forbidden | None -> false

let positive_vs_negative_disjoint positive negative =
  set_negation_disjoint positive.ids negative.not_ids
  || set_negation_disjoint positive.classes negative.not_classes
  || element_negation_disjoint positive.element negative.not_elements
  || attr_negation_disjoint positive.attrs negative.not_attrs
  || nth_negation_disjoint positive.nth negative.not_nth

let negation_disjoint a b =
  positive_vs_negative_disjoint a b || positive_vs_negative_disjoint b a

let direct_facts_disjoint a b =
  let pe_disjoint =
    match (a.pseudo_element, b.pseudo_element) with
    | None, None -> false
    | Some _, None | None, Some _ -> true
    | Some x, Some y -> not (Selector.equal x y)
  in
  if pe_disjoint then true
  else
    let elem_disjoint =
      match (a.element, b.element) with Some x, Some y -> x <> y | _ -> false
    in
    if elem_disjoint then true
    else
      let id_disjoint =
        (not (String_set.is_empty a.ids))
        && (not (String_set.is_empty b.ids))
        && String_set.disjoint a.ids b.ids
      in
      id_disjoint || attrs_disjoint a b || nths_disjoint a b
      || negation_disjoint a b

let rec may_overlap a b =
  if direct_facts_disjoint a b then false
  else
    match (a.parent, b.parent) with
    | Some pa, Some pb when not (may_overlap pa pb) -> false
    | _ -> true

let ids s = String_set.elements s.ids
let classes s = String_set.elements s.classes
let element s = s.element
let is_complex s = s.complex
