let rec pseudo : Selector.t -> Selector.t option = function
  | Before f -> Some (Before f)
  | After f -> Some (After f)
  | First_letter f -> Some (First_letter f)
  | First_line f -> Some (First_line f)
  | Marker -> Some Marker
  | Placeholder -> Some Placeholder
  | Selection -> Some Selection
  | File_selector_button -> Some File_selector_button
  | Backdrop -> Some Backdrop
  | Details_content -> Some Details_content
  | Compound sels ->
      List.fold_left
        (fun acc sel ->
          match pseudo sel with Some _ as pe -> pe | None -> acc)
        None sels
  | Combined (_, _, right) | Relative (_, right) -> pseudo right
  | _ -> None

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

let compatible sel1 sel2 =
  let sel1_complex = Selector.has_is_where_pattern sel1 in
  let sel2_complex = Selector.has_is_where_pattern sel2 in
  if sel1_complex <> sel2_complex then
    let plain_sel = if sel1_complex then sel2 else sel1 in
    not (Selector.has_newer_pseudo_class plain_sel)
  else true

let rec declaration_lists_equal ~same d1 d2 =
  match (d1, d2) with
  | [], [] -> true
  | a :: rest_a, b :: rest_b ->
      same a b && declaration_lists_equal ~same rest_a rest_b
  | _ -> false

let declarations_equal ~same d1 d2 =
  d1 == d2 || declaration_lists_equal ~same d1 d2
