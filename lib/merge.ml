let rec vendor : Selector.t -> bool = function
  | File_selector_button -> true
  | Webkit_scrollbar | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_details_marker ->
      true
  | Compound sels -> List.exists vendor sels
  | Combined (left, _, right) -> vendor left || vendor right
  | Relative (_, right) -> vendor right
  | List sels | Not sels | Is sels | Where sels | Has sels ->
      List.exists vendor sels
  | _ -> false

let key sel =
  match Selector.as_list sel with
  | Some xs -> List.sort compare xs
  | None -> [ sel ]

let selector_list = function
  | [ s ] -> s
  | sels ->
      let flatten = function Selector.List xs -> xs | s -> [ s ] in
      Selector.List (List.concat_map flatten sels)

(* Everything [compatible] reads off a selector. [Guarded] is an
   [:is(:where(...))] group- or peer- variant; [Newer] carries, outside any
   forgiving [:is()]/[:where()], a pseudo-class an older browser drops the whole
   rule for; [Plain] is neither. *)
type compatibility = Plain | Newer | Guarded

let compatibility sel =
  if Selector.has_is_where_pattern sel then Guarded
  else if Selector.has_newer_pseudo_class sel then Newer
  else Plain

let compatible sel1 sel2 =
  match (compatibility sel1, compatibility sel2) with
  | Newer, Guarded | Guarded, Newer -> false
  | _ -> true

(* [compatible] rejects one pair and no other, so a set of selectors holds only
   compatible pairs exactly when it does not hold both ends of it. That is all a
   group needs to remember about the members it already has, so a candidate
   joins by reading its own two facts once rather than once per member.

   Sorting a group and checking neighbours would not do, because [compatible] is
   reflexive and symmetric but NOT transitive - [Newer] sits with [Plain] and
   [Plain] sits with [Guarded], while [Newer] and [Guarded] never sit
   together. *)
type run = Neither | Has_newer | Has_guarded | Mixed

let empty_run = Neither

let extend_run run sel =
  match (run, compatibility sel) with
  | Mixed, _ | _, Plain -> run
  | (Neither | Has_newer), Newer -> Has_newer
  | (Neither | Has_guarded), Guarded -> Has_guarded
  | Has_newer, Guarded | Has_guarded, Newer -> Mixed

let run_compatible = function Mixed -> false | _ -> true

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
