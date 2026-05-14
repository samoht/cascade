module String_set = Set.Make (String)

type t = {
  ids : String_set.t;
  classes : String_set.t;
  element : string option;
  pseudo_element : Selector.t option;
  complex : bool;
}

let empty =
  {
    ids = String_set.empty;
    classes = String_set.empty;
    element = None;
    pseudo_element = None;
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
      | Some prev when prev = canonical -> s
      | Some _ -> mark_complex s)
  | _ -> mark_complex s

let rec of_selector (sel : Selector.t) : t =
  match sel with
  | Combined (_, _, right) -> of_selector right
  | Relative (_, right) -> of_selector right
  | List [ single ] -> of_selector single
  | List _ -> { empty with complex = true }
  | _ -> fold_simple empty sel

let may_overlap a b =
  if a.complex || b.complex then true
  else
    let pe_disjoint =
      match (a.pseudo_element, b.pseudo_element) with
      | None, None -> false
      | Some _, None | None, Some _ -> true
      | Some x, Some y -> x <> y
    in
    if pe_disjoint then false
    else
      let elem_disjoint =
        match (a.element, b.element) with
        | Some x, Some y -> x <> y
        | _ -> false
      in
      if elem_disjoint then false
      else
        let id_disjoint =
          (not (String_set.is_empty a.ids))
          && (not (String_set.is_empty b.ids))
          && String_set.disjoint a.ids b.ids
        in
        not id_disjoint

let ids s = String_set.elements s.ids
let classes s = String_set.elements s.classes
let element s = s.element
let is_complex s = s.complex
