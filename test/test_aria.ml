(** Tests for WAI-ARIA states and properties used by CSS selectors. *)

open Cascade

let attributes =
  [
    (Aria.Active_descendant, "aria-activedescendant");
    (Aria.Atomic, "aria-atomic");
    (Aria.Autocomplete, "aria-autocomplete");
    (Aria.Braillelabel, "aria-braillelabel");
    (Aria.Brailleroledescription, "aria-brailleroledescription");
    (Aria.Busy, "aria-busy");
    (Aria.Checked, "aria-checked");
    (Aria.Colcount, "aria-colcount");
    (Aria.Colindex, "aria-colindex");
    (Aria.Colindextext, "aria-colindextext");
    (Aria.Colspan, "aria-colspan");
    (Aria.Controls, "aria-controls");
    (Aria.Current, "aria-current");
    (Aria.Describedby, "aria-describedby");
    (Aria.Description, "aria-description");
    (Aria.Details, "aria-details");
    (Aria.Disabled, "aria-disabled");
    (Aria.Dropeffect, "aria-dropeffect");
    (Aria.Errormessage, "aria-errormessage");
    (Aria.Expanded, "aria-expanded");
    (Aria.Flowto, "aria-flowto");
    (Aria.Grabbed, "aria-grabbed");
    (Aria.Haspopup, "aria-haspopup");
    (Aria.Hidden, "aria-hidden");
    (Aria.Invalid, "aria-invalid");
    (Aria.Keyshortcuts, "aria-keyshortcuts");
    (Aria.Label, "aria-label");
    (Aria.Labelledby, "aria-labelledby");
    (Aria.Level, "aria-level");
    (Aria.Live, "aria-live");
    (Aria.Modal, "aria-modal");
    (Aria.Multiline, "aria-multiline");
    (Aria.Multiselectable, "aria-multiselectable");
    (Aria.Orientation, "aria-orientation");
    (Aria.Owns, "aria-owns");
    (Aria.Placeholder, "aria-placeholder");
    (Aria.Posinset, "aria-posinset");
    (Aria.Pressed, "aria-pressed");
    (Aria.Readonly, "aria-readonly");
    (Aria.Relevant, "aria-relevant");
    (Aria.Required, "aria-required");
    (Aria.Roledescription, "aria-roledescription");
    (Aria.Rowcount, "aria-rowcount");
    (Aria.Rowindex, "aria-rowindex");
    (Aria.Rowindextext, "aria-rowindextext");
    (Aria.Rowspan, "aria-rowspan");
    (Aria.Selected, "aria-selected");
    (Aria.Setsize, "aria-setsize");
    (Aria.Sort, "aria-sort");
    (Aria.Valuemax, "aria-valuemax");
    (Aria.Valuemin, "aria-valuemin");
    (Aria.Valuenow, "aria-valuenow");
    (Aria.Valuetext, "aria-valuetext");
  ]

let parse_with reader input =
  let cursor = Cursor.of_string input in
  let value = reader cursor in
  Alcotest.(check bool) (input ^ " consumed") true (Cursor.is_done cursor);
  value

let test_all_attributes_roundtrip () =
  List.iter
    (fun (attr, name) ->
      Alcotest.(check string) (name ^ " to_string") name (Aria.to_string attr);
      Alcotest.(check string)
        (name ^ " suffix")
        (String.sub name 5 (String.length name - 5))
        (Aria.suffix attr);
      Alcotest.(check string)
        (name ^ " of_string") name
        (Aria.to_string (Aria.of_string name));
      Alcotest.(check string)
        (name ^ " pp") name
        (Css.Pp.to_string Aria.pp attr))
    attributes

let test_selector_aria_reader () =
  List.iter
    (fun (attr, name) ->
      let parsed = parse_with Css.Selector.read_aria_attr name in
      Alcotest.(check string)
        (name ^ " selector aria") (Aria.to_string attr) (Aria.to_string parsed))
    attributes

let test_selector_attr_name_classification () =
  List.iter
    (fun (attr, name) ->
      match parse_with Css.Selector.read_attr_name name with
      | Css.Selector.Aria parsed ->
          Alcotest.(check string)
            (name ^ " attr name") (Aria.to_string attr) (Aria.to_string parsed)
      | Css.Selector.Data _ | Css.Selector.Regular _ ->
          Alcotest.failf "%s did not parse as a typed ARIA attribute" name)
    attributes;
  match parse_with Css.Selector.read_attr_name "aria-custom" with
  | Css.Selector.Regular name ->
      Alcotest.(check string)
        "unknown aria-* remains a CSS attribute name" "aria-custom" name
  | Css.Selector.Aria _ | Css.Selector.Data _ ->
      Alcotest.fail "unknown aria-* parsed as a typed ARIA attribute"

let test_invalid_aria_enum_names () =
  List.iter
    (fun name ->
      Alcotest.check_raises ("reject " ^ name)
        (Invalid_argument ("not a supported aria attribute: " ^ name))
        (fun () -> ignore (Aria.of_string name)))
    [ "aria-custom"; "aria"; "aria-"; "ARIA-hidden"; "aria-disabled-extra" ];
  List.iter
    (fun name ->
      match
        try Some (parse_with Css.Selector.read_aria_attr name)
        with Cursor.Parse_error _ -> None
      with
      | None -> ()
      | Some parsed ->
          Alcotest.failf "%s parsed as %s" name (Aria.to_string parsed))
    [ "aria-custom"; "aria"; "aria-"; "ARIA-hidden"; "aria-disabled-extra" ]

let suite =
  let open Alcotest in
  ( "aria",
    [
      test_case "all attributes roundtrip" `Quick test_all_attributes_roundtrip;
      test_case "selector aria reader" `Quick test_selector_aria_reader;
      test_case "selector attr name classification" `Quick
        test_selector_attr_name_classification;
      test_case "invalid aria enum names" `Quick test_invalid_aria_enum_names;
    ] )
