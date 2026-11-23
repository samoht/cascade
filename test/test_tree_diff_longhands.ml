(* The tree diff of a shorthand written on one side against the longhands it
   sets written on the other. *)

open Cascade

let diff_of ~expected ~actual =
  let parse css = Css.stylesheet ~strict:false css in
  Cascade_diff.Tree_diff.diff ~expected:(parse expected) ~actual:(parse actual)

let content_changes d =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Content_changed
          { property_changes; added_properties; removed_properties; _ } ->
          Some (property_changes, added_properties, removed_properties)
      | _ -> None)
    d.Cascade_diff.Tree_diff.rules

let check_change ~changed ~added ~removed d =
  let changed', added', removed' =
    match content_changes d with
    | [ change ] -> change
    | changes ->
        Alcotest.failf "%d content changes, not one" (List.length changes)
  in
  let show (p : Cascade_diff.Tree_diff.declaration) =
    Fmt.str "%s: %s -> %s" p.property_name p.expected_value p.actual_value
  in
  Alcotest.(check (list string))
    "each longhand against the slot of the shorthand it writes" changed
    (List.map show changed');
  Alcotest.(check (list (pair string string))) "added" added added';
  Alcotest.(check (list (pair string string))) "removed" removed removed'

(* CSS Backgrounds 3 sec. 4.4: [border] sets [border-width], [border-style] and
   [border-color] and resets [border-image] to its initial value, [none]. Set
   against the three longhands, each one answers the slot of [border] it writes,
   and the reset is what only the shorthand's side makes. *)

let shorthand_against_longhands () =
  diff_of ~expected:"a{border:1px solid red}"
    ~actual:"a{border-color:blue;border-style:solid;border-width:2px}"
  |> check_change
       ~changed:[ "border-color: red -> blue"; "border-width: 1px -> 2px" ]
       ~added:[]
       ~removed:[ ("border-image", "none") ]

let shorthand_agreeing_with_longhands () =
  diff_of ~expected:"a{border:1px solid red}"
    ~actual:"a{border-color:red;border-style:solid;border-width:1px}"
  |> check_change ~changed:[] ~added:[] ~removed:[ ("border-image", "none") ]

let longhands_against_shorthand () =
  diff_of ~expected:"a{border-color:blue;border-style:solid;border-width:1px}"
    ~actual:"a{border:1px solid red}"
  |> check_change
       ~changed:[ "border-color: blue -> red" ]
       ~added:[ ("border-image", "none") ]
       ~removed:[]

let suite =
  ( "tree_diff_longhands",
    [
      Alcotest.test_case "shorthand against longhands" `Quick
        shorthand_against_longhands;
      Alcotest.test_case "shorthand agreeing with longhands" `Quick
        shorthand_agreeing_with_longhands;
      Alcotest.test_case "longhands against shorthand" `Quick
        longhands_against_shorthand;
    ] )
