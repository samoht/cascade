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

(* CSS Backgrounds 3 sec. 3.4: [border] sets [border-width], [border-style] and
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

let check_no_difference d =
  Alcotest.(check int)
    "the shorthand and the longhands it expands to write the same values" 0
    (List.length d.Cascade_diff.Tree_diff.rules)

(* CSS Backgrounds 3 sec. 3.4: [border] writes [border-width], [border-style]
   and [border-color], and resets [border-image] to [none]. Written out in full,
   those four longhands are the shorthand. *)
let shorthand_expanded_to_longhands () =
  diff_of ~expected:".a{border:1px solid red}"
    ~actual:
      ".a{border-width:1px;border-style:solid;border-color:red;border-image:none}"
  |> check_no_difference

(* CSS Backgrounds 3 sec. 3.4: a value the shorthand omits is set to its initial
   value, and sec. 3.1 gives [border-color] the initial [currentcolor]. *)
let shorthand_omitting_a_slot () =
  diff_of ~expected:".a{border:1px solid}"
    ~actual:
      ".a{border-width:1px;border-style:solid;border-color:currentcolor;border-image:none}"
  |> check_no_difference

(* The initial a slot is filled with is a value like any other, so a longhand
   that writes something else there is reported against it. CSS Color 4 sec. 6.1
   makes keywords ASCII case-insensitive, so the case the initial is spelt in is
   not part of the answer. *)
let omitted_slot_against_other_value () =
  let d =
    diff_of ~expected:".a{border:1px solid}"
      ~actual:
        ".a{border-width:1px;border-style:solid;border-color:red;border-image:none}"
  in
  let changed =
    match content_changes d with
    | [ (changed, [], []) ] ->
        List.map
          (fun (p : Cascade_diff.Tree_diff.declaration) ->
            Fmt.str "%s: %s -> %s" p.property_name
              (String.lowercase_ascii p.expected_value)
              p.actual_value)
          changed
    | _ -> Alcotest.fail "not one content change with only changed slots"
  in
  Alcotest.(check (list string))
    "the omitted colour against the longhand"
    [ "border-color: currentcolor -> red" ]
    changed

(* CSS Backgrounds 3 sec. 3.4: sec. 3.3 and 3.2 give [border-width] the initial
   [medium] and [border-style] the initial [none]. *)
let shorthand_omitting_width_and_style () =
  diff_of ~expected:".a{border:red}"
    ~actual:
      ".a{border-width:medium;border-style:none;border-color:red;border-image:none}"
  |> check_no_difference

(* CSS Backgrounds 3 sec. 2.10: [background] sets every omitted component to its
   initial value: [none] (sec. 2.3), [0% 0%] (sec. 2.6), [auto] (sec. 2.9),
   [repeat] (sec. 2.4), [scroll] (sec. 2.5), [padding-box] (sec. 2.8) and
   [border-box] (sec. 2.7). *)
let background_expanded_to_longhands () =
  diff_of ~expected:".a{background:red}"
    ~actual:
      ".a{background-image:none;background-position:0% \
       0%;background-size:auto;background-repeat:repeat;background-attachment:scroll;background-origin:padding-box;background-clip:border-box;background-color:red}"
  |> check_no_difference

(* CSS Backgrounds 3 sec. 2.10: one [<box>] sets both [background-origin] and
   [background-clip]. *)
let background_one_box () =
  diff_of ~expected:".a{background:url(a.png) no-repeat content-box}"
    ~actual:
      ".a{background-image:url(a.png);background-position:0% \
       0%;background-size:auto;background-repeat:no-repeat;background-attachment:scroll;background-origin:content-box;background-clip:content-box;background-color:transparent}"
  |> check_no_difference

(* CSS Flexible Box 1 sec. 7.1: [flex: none] is [0 0 auto], [flex: auto] is [1 1
   auto], and a lone [<'flex-basis'>] leaves grow and shrink at [1]. *)
let flex_keyword_expanded () =
  diff_of ~expected:".a{flex:none}"
    ~actual:".a{flex-grow:0;flex-shrink:0;flex-basis:auto}"
  |> check_no_difference;
  diff_of ~expected:".a{flex:auto}"
    ~actual:".a{flex-grow:1;flex-shrink:1;flex-basis:auto}"
  |> check_no_difference;
  diff_of ~expected:".a{flex:10px}"
    ~actual:".a{flex-grow:1;flex-shrink:1;flex-basis:10px}"
  |> check_no_difference

let flex_expanded_to_longhands () =
  diff_of ~expected:".a{flex:1 2 3px}"
    ~actual:".a{flex-grow:1;flex-shrink:2;flex-basis:3px}"
  |> check_no_difference

(* CSS Transitions 1 sec. 2.5 and CSS Transitions 2 sec. 2.6: [transition] sets
   every omitted component to its initial value: [all], [0s], [ease], [0s] and
   [normal]. *)
let transition_expanded_to_longhands () =
  diff_of ~expected:".a{transition:opacity 1s}"
    ~actual:
      ".a{transition-property:opacity;transition-duration:1s;transition-timing-function:ease;transition-delay:0s;transition-behavior:normal}"
  |> check_no_difference;
  diff_of ~expected:".a{transition:1s}"
    ~actual:
      ".a{transition-property:all;transition-duration:1s;transition-timing-function:ease;transition-delay:0s;transition-behavior:normal}"
  |> check_no_difference

let suite =
  ( "tree_diff_longhands",
    [
      Alcotest.test_case "shorthand against longhands" `Quick
        shorthand_against_longhands;
      Alcotest.test_case "shorthand agreeing with longhands" `Quick
        shorthand_agreeing_with_longhands;
      Alcotest.test_case "longhands against shorthand" `Quick
        longhands_against_shorthand;
      Alcotest.test_case "shorthand expanded to longhands" `Quick
        shorthand_expanded_to_longhands;
      Alcotest.test_case "shorthand omitting a slot" `Quick
        shorthand_omitting_a_slot;
      Alcotest.test_case "omitted slot against another value" `Quick
        omitted_slot_against_other_value;
      Alcotest.test_case "shorthand omitting width and style" `Quick
        shorthand_omitting_width_and_style;
      Alcotest.test_case "background expanded to longhands" `Quick
        background_expanded_to_longhands;
      Alcotest.test_case "background with one box" `Quick background_one_box;
      Alcotest.test_case "flex keyword expanded" `Quick flex_keyword_expanded;
      Alcotest.test_case "flex expanded to longhands" `Quick
        flex_expanded_to_longhands;
      Alcotest.test_case "transition expanded to longhands" `Quick
        transition_expanded_to_longhands;
    ] )
