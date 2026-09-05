(** Tests for Cascade_diff.Tree_diff module *)

open Cascade

let parse css = Css.of_string_exn ~strict:false css

let string_contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  go 0

let render d =
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  Buffer.contents buf

(* ===== Identical stylesheets ===== *)

let diff_identical () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_identical_multiple_rules () =
  let css = parse ".a { color: red } .b { margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical multi-rule is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_empty_stylesheets () =
  let css = parse "" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "empty stylesheets is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Rule additions ===== *)

let diff_rule_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } .b { margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "addition is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> []);
  (* Check that at least one Added exists *)
  let has_added =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with Cascade_diff.Tree_diff.Added _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Added" true has_added

(* ===== Rule removals ===== *)

let diff_rule_removed () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "removal is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let has_removed =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with Cascade_diff.Tree_diff.Removed _ -> true | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Removed" true has_removed

(* ===== Property value changes ===== *)

let diff_property_changed () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "property change is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has rule diffs" true (d.rules <> [])

let diff_rule_added_property () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red; margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "added property is not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Rule reordering ===== *)

let diff_rule_reordered () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".b { margin: 0 } .a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "reorder is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let has_reordered =
    List.exists
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with
        | Cascade_diff.Tree_diff.Reordered _ -> true
        | _ -> false)
      d.rules
  in
  Alcotest.(check bool) "has Reordered" true has_reordered

let reorders d =
  List.filter
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with Cascade_diff.Tree_diff.Reordered _ -> true | _ -> false)
    d.Cascade_diff.Tree_diff.rules

let diff_swap_reports_one_reorder () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".b { margin: 0 } .a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check int) "one reorder for one swap" 1 (List.length (reorders d))

let diff_move_names_the_rule_that_moved () =
  (* [.d] jumps to the front. The three rules it passed kept their order against
     each other, so naming them reports one move three times over. *)
  let expected =
    parse
      ".a { color: red } .b { margin: 0 } .c { padding: 0 } .d { border: 0 }"
  in
  let actual =
    parse
      ".d { border: 0 } .a { color: red } .b { margin: 0 } .c { padding: 0 }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let moved =
    List.filter_map
      (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
        match diff with
        | Cascade_diff.Tree_diff.Reordered { selector; _ } -> Some selector
        | _ -> None)
      d.rules
  in
  Alcotest.(check (list string)) "only the mover is named" [ ".d" ] moved

let reordered_selectors d =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Cascade_diff.Tree_diff.Reordered { selector; _ } -> Some selector
      | _ -> None)
    d.Cascade_diff.Tree_diff.rules

let content_changed_selectors d =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Cascade_diff.Tree_diff.Content_changed { selector; _ } -> Some selector
      | _ -> None)
    d.Cascade_diff.Tree_diff.rules

let added_selectors d =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Cascade_diff.Tree_diff.Added { selector; _ } -> Some selector
      | _ -> None)
    d.Cascade_diff.Tree_diff.rules

(* A transposition is a cascade change whatever else the two sides differ on, so
   it has to be reported next to that other change and not instead of it. The
   control below is these same three rules with [.c] left alone; the three that
   follow add one unrelated difference each. *)

let diff_swap_alone_reports_the_reorder () =
  let expected =
    parse ".a { color: red } .b { color: blue } .c { color: green }"
  in
  let actual =
    parse ".b { color: blue } .a { color: red } .c { color: green }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check (list string))
    "the swap is named" [ ".a" ] (reordered_selectors d);
  Alcotest.(check (list string))
    "nothing else changed" []
    (content_changed_selectors d)

let diff_swap_with_modified_rule_reports_both () =
  let expected =
    parse ".a { color: red } .b { color: blue } .c { color: green }"
  in
  let actual =
    parse ".b { color: blue } .a { color: red } .c { color: teal }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check (list string))
    "the swap is still named" [ ".a" ] (reordered_selectors d);
  Alcotest.(check (list string))
    "so is the changed rule" [ ".c" ]
    (content_changed_selectors d)

let diff_swap_with_added_rule_reports_both () =
  let expected = parse ".a { color: red } .b { color: blue }" in
  let actual =
    parse ".b { color: blue } .a { color: red } .d { color: pink }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check (list string))
    "the swap is still named" [ ".a" ] (reordered_selectors d);
  Alcotest.(check (list string))
    "so is the new rule" [ ".d" ] (added_selectors d)

let diff_move_with_modified_rule_names_the_mover () =
  (* [.d] jumps from fourth to first while [.e] changes value. The move is one
     entry, naming the rule that moved rather than the three it passed. *)
  let expected =
    parse
      ".a { color: red } .b { color: blue } .c { color: green } .d { color: \
       pink } .e { color: gray }"
  in
  let actual =
    parse
      ".d { color: pink } .a { color: red } .b { color: blue } .c { color: \
       green } .e { color: navy }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check (list string))
    "the mover is named" [ ".d" ] (reordered_selectors d);
  Alcotest.(check (list string))
    "so is the changed rule" [ ".e" ]
    (content_changed_selectors d)

let diff_dropped_rules_are_not_a_reorder () =
  (* Dropping the leading rules shifts every position after them. Nothing was
     transposed, so the survivors are unchanged and their cascade-neutral
     declaration reorder stays the non-difference it is. *)
  let expected =
    parse ".x { color: red } .y { color: red } .d { color: red; margin: 0 }"
  in
  let actual = parse ".d { margin: 0; color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check int) "no reorder from the shift" 0 (List.length (reorders d))

let diff_dropped_rules_keep_the_decl_reorder () =
  (* Same shift, but [.d] does hold a cascade-significant declaration swap. The
     shift used to overwrite it with a claim about the rule's position, which
     drops the declarations the report needs to name it. *)
  let expected =
    parse
      ".x { color: red } .y { color: red } .d { margin: 0; margin-top: 1px }"
  in
  let actual = parse ".d { margin-top: 1px; margin: 0 }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  match reorders d with
  | [ Cascade_diff.Tree_diff.Reordered { old_declarations; _ } ] ->
      Alcotest.(check bool)
        "the reorder is about declarations" true (old_declarations <> None)
  | rs ->
      Alcotest.failf "expected one declaration reorder, got %d" (List.length rs)

let diff_neutral_decl_reorder_is_empty () =
  (* Disjoint declarations commute, so reordering them is no difference. *)
  let expected = parse ".a { color: red; background: blue }" in
  let actual = parse ".a { background: blue; color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "neutral declaration reorder is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_overlapping_decl_reorder_flagged () =
  (* A shorthand and its longhand overlap, so their order decides the
     cascade. *)
  let expected = parse ".a { margin: 1px; margin-top: 2px }" in
  let actual = parse ".a { margin-top: 2px; margin: 1px }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "overlapping declaration reorder is not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

(* The selectors whose declarations were reordered, from every container the
   diff reports, at any nesting depth. *)
let rec nested_decl_reorders (c : Cascade_diff.Tree_diff.container_diff) =
  match c with
  | Cascade_diff.Tree_diff.Modified { rule_changes; container_changes; _ } ->
      List.filter_map
        (fun (r : Cascade_diff.Tree_diff.rule_diff) ->
          match r with
          | Cascade_diff.Tree_diff.Reordered
              { selector; old_declarations = Some _; _ } ->
              Some selector
          | _ -> None)
        rule_changes
      @ List.concat_map nested_decl_reorders container_changes
  | _ -> []

let decl_reorders_in_containers d =
  List.concat_map nested_decl_reorders d.Cascade_diff.Tree_diff.containers

(* An at-rule does not commute the declarations it wraps: a swap that decides
   the cascade at the top level decides it inside @media, @layer and @supports
   too. *)
let significant_reorder_inside wrap () =
  let expected = parse (wrap ".a { margin: 1px; margin-top: 2px }") in
  let actual = parse (wrap ".a { margin-top: 2px; margin: 1px }") in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "overlapping reorder inside an at-rule is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check (list string))
    "the report names the rule that was reordered" [ ".a" ]
    (decl_reorders_in_containers d)

let neutral_reorder_inside wrap () =
  let expected = parse (wrap ".a { color: red; background: blue }") in
  let actual = parse (wrap ".a { background: blue; color: red }") in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "neutral reorder inside an at-rule is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let in_media body = "@media (min-width: 10px) { " ^ body ^ " }"
let in_layer body = "@layer base { " ^ body ^ " }"
let in_supports body = "@supports (color: red) { " ^ body ^ " }"
let diff_media_decl_reorder_flagged = significant_reorder_inside in_media
let diff_layer_decl_reorder_flagged = significant_reorder_inside in_layer
let diff_supports_decl_reorder_flagged = significant_reorder_inside in_supports
let diff_media_neutral_decl_reorder_empty = neutral_reorder_inside in_media
let diff_layer_neutral_decl_reorder_empty = neutral_reorder_inside in_layer

let diff_supports_neutral_decl_reorder_empty =
  neutral_reorder_inside in_supports

(* ===== @property registrations ===== *)

(* The descriptor changes a modified [@property] reports, as "name: expected ->
   actual". *)
let property_descriptor_changes d =
  List.concat_map
    (fun (c : Cascade_diff.Tree_diff.container_diff) ->
      match c with
      | Cascade_diff.Tree_diff.Modified
          { info = { container_type = `Property; _ }; rule_changes; _ } ->
          List.concat_map
            (fun (r : Cascade_diff.Tree_diff.rule_diff) ->
              match r with
              | Cascade_diff.Tree_diff.Content_changed { property_changes; _ }
                ->
                  List.map
                    (fun (p : Cascade_diff.Tree_diff.declaration) ->
                      p.property_name ^ ": " ^ p.expected_value ^ " -> "
                      ^ p.actual_value)
                    property_changes
              | _ -> [])
            rule_changes
      | _ -> [])
    d.Cascade_diff.Tree_diff.containers

let diff_property_syntax_changed () =
  (* A registration decides how every use of the custom property parses, so a
     different syntax and initial value is a difference, not a match. *)
  let expected =
    parse
      "@property --x { syntax: '<length>'; inherits: false; initial-value: 0px \
       }"
  in
  let actual =
    parse
      "@property --x { syntax: '<color>'; inherits: false; initial-value: red }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "changed registration is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check (list string))
    "names both descriptors"
    [ "syntax: \"<length>\" -> \"<color>\""; "initial-value: 0px -> red" ]
    (property_descriptor_changes d)

let diff_property_inherits_changed () =
  (* [inherits] was the one descriptor compared, but the entry carried no change
     detail and rendered as a position change. *)
  let expected = parse "@property --x { syntax: '*'; inherits: false }" in
  let actual = parse "@property --x { syntax: '*'; inherits: true }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check (list string))
    "names the descriptor that changed"
    [ "inherits: false -> true" ]
    (property_descriptor_changes d)

let diff_property_identical_is_empty () =
  let css =
    parse
      "@property --x { syntax: '<length>'; inherits: false; initial-value: 0px \
       }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical registration is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_property_initial_value_added () =
  let expected = parse "@property --x { syntax: '*'; inherits: false }" in
  let actual =
    parse "@property --x { syntax: '*'; inherits: false; initial-value: red }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let added =
    List.concat_map
      (fun (c : Cascade_diff.Tree_diff.container_diff) ->
        match c with
        | Cascade_diff.Tree_diff.Modified { rule_changes; _ } ->
            List.concat_map
              (fun (r : Cascade_diff.Tree_diff.rule_diff) ->
                match r with
                | Cascade_diff.Tree_diff.Content_changed { added_properties; _ }
                  ->
                    added_properties
                | _ -> [])
              rule_changes
        | _ -> [])
      d.containers
  in
  Alcotest.(check (list (pair string string)))
    "names the descriptor gained"
    [ ("initial-value", "red") ]
    added

(* ===== Container (media) changes ===== *)

let diff_media_added () =
  let expected = parse ".a { color: red }" in
  let actual =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media addition is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool) "has container diffs" true (d.containers <> []);
  Alcotest.(check bool)
    "has media added" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `Media d)

let diff_media_removed () =
  let expected =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let actual = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "media removal is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has media removed" true
    (Cascade_diff.Tree_diff.has_container_removed_of_type `Media d)

let diff_layer_added () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: red } @layer base { .b { margin: 0 } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "layer addition not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "has layer added" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `Layer d)

(* ===== CSS nesting ===== *)

let diff_nesting_modified () =
  let expected =
    parse
      ".card { padding: 1rem; & .title { font-size: 1.5rem; color: #111 } & \
       .body { color: #333 } }"
  in
  let actual =
    parse
      ".card { padding: 1.5rem; & .title { font-size: 1.25rem; color: #000; \
       font-weight: 600 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nesting diff not empty" false
    (d.rules = [] && d.containers = []);
  (* Should detect nested rule changes *)
  let has_nesting =
    Cascade_diff.Tree_diff.count_containers_by_type `Nesting d > 0
  in
  Alcotest.(check bool) "has nesting container diff" true has_nesting

let diff_nesting_identical () =
  let css = parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical nesting is empty" true
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_child_added () =
  let expected = parse ".card { padding: 1rem }" in
  let actual =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nested child added not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_child_removed () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1.5rem } }"
  in
  let actual = parse ".card { padding: 1rem }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "nested child removed not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_deep () =
  let expected = parse ".a { & .b { & .c { color: red } } }" in
  let actual = parse ".a { & .b { & .c { color: blue } } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "deep nesting diff not empty" false
    (Cascade_diff.Tree_diff.is_empty d)

let diff_nesting_parent_props_only () =
  let expected =
    parse ".card { padding: 1rem; & .title { font-size: 1rem } }"
  in
  let actual = parse ".card { padding: 2rem; & .title { font-size: 1rem } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "parent-only change not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  (* Nested rules are identical, so no nesting container diff *)
  let nesting_count =
    Cascade_diff.Tree_diff.count_containers_by_type `Nesting d
  in
  Alcotest.(check int) "no nesting container diff" 0 nesting_count

(* A comma group is a set, so the two sides write the same rule and the only
   difference between them is the one inside its nested body. *)
let diff_nesting_under_reordered_selector_list () =
  let expected = parse ".a, .b { color: red; &:hover { color: blue } }" in
  let actual = parse ".b, .a { color: red; &:hover { color: green } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let s = render d in
  Alcotest.(check bool)
    "the nested colour change is reported" true
    (string_contains ~needle:"blue" s && string_contains ~needle:"green" s)

(* The same selectors with the same specificities in another order select the
   same elements, so nothing about the rule changed. *)
let diff_selector_list_reorder_is_not_a_change () =
  let expected = parse ".a, .b { color: red }" in
  let actual = parse ".b, .a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "a reordered selector list is not a difference" true
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Query functions ===== *)

let single_rule_diff_one_change () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "single_rule_diff returns Some" true
    (Option.is_some (Cascade_diff.Tree_diff.single_rule_diff d))

let single_rule_diff_no_change () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  Alcotest.(check bool)
    "single_rule_diff returns None" true
    (Option.is_none (Cascade_diff.Tree_diff.single_rule_diff d))

let single_rule_diff_multiple_changes () =
  let expected = parse ".a { color: red } .b { margin: 0 }" in
  let actual = parse ".a { color: blue } .b { margin: 10px }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  (* Multiple rule changes means single_rule_diff should return None *)
  let result = Cascade_diff.Tree_diff.single_rule_diff d in
  (* Only None when there are exactly != 1 rule changes *)
  if List.length d.rules = 1 then
    Alcotest.(check bool) "single with 1 change" true (Option.is_some result)
  else
    Alcotest.(check bool)
      "none with multiple changes" true (Option.is_none result)

let count_containers_media () =
  let expected = parse ".a { color: red }" in
  let actual =
    parse ".a { color: red } @media (min-width: 768px) { .b { margin: 0 } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let count = Cascade_diff.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check bool) "at least one media container" true (count >= 1)

(* A container that survived is reported as [Modified] and carries the
   containers that came and went inside it, so a container added one level down
   is in the diff but not at its top level. [count_containers_by_type] descends
   into [Modified], and the two existence queries name the same containers, so
   they answer for the same set: a query that says no about a container the
   count reports is answering about the shape of the diff rather than about the
   stylesheets. *)
let nested_container_added_is_found () =
  let expected = parse "@media print { .a { color: red } }" in
  let actual =
    parse
      "@media print { .a { color: red } @supports (color: red) { .b { color: \
       blue } } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check int)
    "the nested @supports is counted" 1
    (Cascade_diff.Tree_diff.count_containers_by_type `Supports d);
  Alcotest.(check bool)
    "and the same @supports is found as added" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `Supports d)

let nested_container_removed_is_found () =
  let expected =
    parse
      "@media print { .a { color: red } @supports (color: red) { .b { color: \
       blue } } }"
  in
  let actual = parse "@media print { .a { color: red } }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check int)
    "the nested @supports is counted" 1
    (Cascade_diff.Tree_diff.count_containers_by_type `Supports d);
  Alcotest.(check bool)
    "and the same @supports is found as removed" true
    (Cascade_diff.Tree_diff.has_container_removed_of_type `Supports d)

(* An added container is not a removed one and the reverse, however deep it
   sits. *)
let nested_container_added_is_not_removed () =
  let expected = parse "@media print { .a { color: red } }" in
  let actual =
    parse
      "@media print { .a { color: red } @supports (color: red) { .b { color: \
       blue } } }"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "the nested addition is not reported as a removal" false
    (Cascade_diff.Tree_diff.has_container_removed_of_type `Supports d)

let count_containers_zero () =
  let css = parse ".a { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected:css ~actual:css in
  let count = Cascade_diff.Tree_diff.count_containers_by_type `Media d in
  Alcotest.(check int) "zero media containers" 0 count

(* ===== Pretty printing ===== *)

let pp_does_not_crash () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  let output = Buffer.contents buf in
  Alcotest.(check bool) "pp produces output" true (String.length output > 0)

let pp_rule_diff_simple_ok () =
  let expected = parse ".a { color: red }" in
  let actual = parse ".a { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  match d.rules with
  | [] -> Alcotest.fail "expected rule diffs"
  | rule :: _ ->
      let buf = Buffer.create 256 in
      Cascade_diff.Tree_diff.pp_rule_diff_simple buf rule;
      let output = Buffer.contents buf in
      Alcotest.(check bool)
        "pp_rule_diff_simple produces output" true
        (String.length output > 0)

(* ===== Selector grouping reconciliation ===== *)

let diff_selector_group_split_reported () =
  (* Splitting a group with identical declarations is reported as a structural
     regroup (not add/remove noise, not silently identical). *)
  let expected = parse ".a, .b { color: red }" in
  let actual = parse ".a { color: red } .b { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "split is reported, not identical" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "describes a split" true
    (string_contains ~needle:"split" s);
  Alcotest.(check bool)
    "names both selectors" true
    (string_contains ~needle:".a" s && string_contains ~needle:".b" s)

let diff_selector_group_merge_reported () =
  let expected = parse ".a { color: red } .b { color: red }" in
  let actual = parse ".a, .b { color: red }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "merge is reported, not identical" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "describes a merge" true
    (string_contains ~needle:"merged" s)

let diff_selector_group_partial_change () =
  (* Splitting the group and changing one selector: only the changed selector
     surfaces, the unchanged one is reconciled away (not add/remove noise). *)
  let expected = parse ".a, .b { color: red }" in
  let actual = parse ".a { color: red } .b { color: blue }" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "partial regroup is not empty" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "reports the changed .b" true
    (string_contains ~needle:".b" s);
  Alcotest.(check bool)
    "does not report the unchanged .a" false
    (string_contains ~needle:".a" s)

(* ===== Suite ===== *)

(* Three blocks carrying one condition against two of them. Testing existence
   rather than claiming each right-hand block once hides the duplicate: neither
   side counts as added or removed, and the survivors are paired twice, so the
   report shows two changed containers inventing an added rule apiece. *)
let duplicate_condition_blocks_reconcile () =
  let expected =
    parse
      "@container (width>=48rem){.a{color:red}}\n\
       @container (width>=48rem){.b{color:blue}}\n\
       @container (width>=48rem){.c{color:lime}}"
  in
  let actual =
    parse
      "@container (width>=48rem){.a{color:red}}\n\
       @container (width>=48rem){.b{color:blue}}"
  in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check int)
    "one container difference, not one per duplicate" 1
    (List.length d.containers);
  Alcotest.(check bool)
    "the third block is removed" true
    (Cascade_diff.Tree_diff.has_container_removed_of_type `Container d);
  Alcotest.(check bool)
    "and nothing is added" false
    (Cascade_diff.Tree_diff.has_container_added_of_type `Container d)

(* ===== Declarations redistributed between rules of one selector ===== *)

let rearranged_of (d : Cascade_diff.Tree_diff.t) =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Rearranged { selector; declarations } -> Some (selector, declarations)
      | _ -> None)
    d.rules

let diff_of ~expected ~actual =
  Cascade_diff.Tree_diff.diff ~expected:(parse expected) ~actual:(parse actual)

(* [.a] writes both declarations on both sides, split over two rules on one and
   one rule on the other. *)
let split = ".a{color:red}.b{color:blue}.a{margin:0}"
let joined = ".a{color:red;margin:0}.b{color:blue}"

let rearranged_reported () =
  let d = diff_of ~expected:split ~actual:joined in
  match rearranged_of d with
  | [ (selector, declarations) ] ->
      Alcotest.(check string) "the selector that moved" ".a" selector;
      Alcotest.(check (list string))
        "carries every declaration it writes" [ "color"; "margin" ]
        (List.map Css.declaration_name declarations |> List.sort compare)
  | rs ->
      Alcotest.failf "expected one rearranged rule, got %d:\n%s"
        (List.length rs) (render d)

let rearranged_is_a_difference () =
  let d = diff_of ~expected:split ~actual:joined in
  Alcotest.(check bool)
    "a move between rules is still reported, not folded away" false
    (Cascade_diff.Tree_diff.is_empty d)

let rearranged_reports_one_node () =
  (* One selector was named twice, once losing the declaration and once gaining
     it, which read as an unrelated addition and removal. *)
  let d = diff_of ~expected:split ~actual:joined in
  Alcotest.(check int) "one entry for the one selector" 1 (List.length d.rules)

let rearranged_names_the_move () =
  let out = render (diff_of ~expected:split ~actual:joined) in
  Alcotest.(check bool)
    "the report says the declarations moved" true
    (Astring.String.is_infix ~affix:"moved between rules" out);
  Alcotest.(check bool)
    "and shows what moved" true
    (Astring.String.is_infix ~affix:"margin" out)

let rearranged_same_at_either_depth () =
  (* Top-level rules and rules inside a container are assembled the same way, so
     one difference does not report two ways. *)
  let top = render (diff_of ~expected:split ~actual:joined) in
  let layered =
    render
      (diff_of
         ~expected:("@layer u{" ^ split ^ "}")
         ~actual:("@layer u{" ^ joined ^ "}"))
  in
  Alcotest.(check bool)
    "the container report names the move too" true
    (Astring.String.is_infix ~affix:"moved between rules" layered);
  Alcotest.(check bool)
    "and counts it by name rather than as a position change" true
    (Astring.String.is_infix ~affix:"1 rearranged" layered);
  Alcotest.(check bool)
    "the top-level report names it as well" true
    (Astring.String.is_infix ~affix:"moved between rules" top)

(* These pin the classification against silencing a real difference. *)

let lost_declaration_is_not_a_move () =
  let d = diff_of ~expected:split ~actual:".a{color:red}.b{color:blue}" in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d));
  Alcotest.(check bool)
    "the loss is still reported" true
    (Astring.String.is_infix ~affix:"- margin: 0" (render d))

let gained_declaration_is_not_a_move () =
  let d =
    diff_of ~expected:".a{color:red}.b{color:blue}"
      ~actual:".a{color:red;margin:0}.b{color:blue}.a{top:0}"
  in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d));
  Alcotest.(check bool)
    "the difference is still reported" false
    (Cascade_diff.Tree_diff.is_empty d)

let gained_declaration_shows_value () =
  let d =
    diff_of ~expected:".a{color:red}"
      ~actual:".a{color:red;margin:100px!important}"
  in
  Alcotest.(check bool)
    "the gained value and priority are reported" true
    (Astring.String.is_infix ~affix:"+ margin: 100px !important" (render d))

let changed_value_is_not_a_move () =
  let d =
    diff_of ~expected:".a{color:red}.b{x:1}.a{margin:0}"
      ~actual:".a{color:green;margin:0}.b{x:1}"
  in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d));
  Alcotest.(check bool)
    "the value change is named" true
    (Astring.String.is_infix ~affix:"red -> green" (render d))

let added_important_is_not_a_move () =
  (* [!important] decides the cascade winner, so the same property and value
     with it added is not the declaration that left. *)
  let d =
    diff_of ~expected:".a{color:red}.b{x:1}.a{margin:0}"
      ~actual:".a{color:red;margin:0 !important}.b{x:1}"
  in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d));
  Alcotest.(check bool)
    "the change of weight is named" true
    (Astring.String.is_infix ~affix:"!important" (render d))

let one_of_two_duplicates_lost_is_not_a_move () =
  (* Declarations are compared as a multiset, so dropping one of two identical
     ones is a loss even though the property still appears on both sides. *)
  let d =
    diff_of ~expected:".a{top:0}.b{x:1}.a{top:0}" ~actual:".a{top:0}.b{x:1}"
  in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d))

let unrelated_selectors_are_not_a_move () =
  (* The declaration moves to a different selector, changing what it applies
     to. *)
  let d =
    diff_of ~expected:".a{color:red}.b{margin:0}"
      ~actual:".a{color:red;margin:0}.b{}"
  in
  Alcotest.(check int)
    "nothing is called a move" 0
    (List.length (rearranged_of d))

let rearranged_survives_pp_simple () =
  match rearranged_of (diff_of ~expected:split ~actual:joined) with
  | [ _ ] ->
      let d = diff_of ~expected:split ~actual:joined in
      let buf = Buffer.create 64 in
      List.iter (Cascade_diff.Tree_diff.pp_rule_diff_simple buf) d.rules;
      Alcotest.(check bool)
        "the compact form names it" true
        (Astring.String.is_infix ~affix:"Rearranged" (Buffer.contents buf))
  | _ -> Alcotest.fail "expected one rearranged rule"

(* ===== At-rules that carry no selector ===== *)

(* These at-rules have no selector to pair on, so a change confined to the body
   is only seen if the at-rule is a diff subject in its own right. *)

let at_rule_change ~name ~affix ~expected ~actual () =
  let d = diff_of ~expected ~actual in
  Alcotest.(check bool)
    (name ^ " body change is a difference")
    false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    (name ^ " is named in the report")
    true
    (Astring.String.is_infix ~affix (render d))

let page_body_change_reported =
  at_rule_change ~name:"@page" ~affix:"@page" ~expected:"@page{margin:1cm}"
    ~actual:"@page{margin:2cm}"

let starting_style_body_change_reported =
  at_rule_change ~name:"@starting-style" ~affix:"@starting-style"
    ~expected:"@starting-style{a{color:red}}"
    ~actual:"@starting-style{a{color:blue}}"

let counter_style_body_change_reported =
  at_rule_change ~name:"@counter-style" ~affix:"@counter-style"
    ~expected:"@counter-style c{system:cyclic;symbols:\"a\"}"
    ~actual:"@counter-style c{system:cyclic;symbols:\"b\"}"

let scope_body_change_reported =
  at_rule_change ~name:"@scope" ~affix:"@scope"
    ~expected:"@scope(.r){a{color:red}}" ~actual:"@scope(.r){a{color:blue}}"

(* Repeated blocks of one at-rule pair positionally; reading only the first pair
   leaves every later block unchecked. *)
let repeated_font_face_change_reported =
  at_rule_change ~name:"@font-face" ~affix:"@font-face"
    ~expected:
      "@font-face{font-family:F;src:url(1)}@font-face{font-family:G;src:url(2)}"
    ~actual:
      "@font-face{font-family:F;src:url(1)}@font-face{font-family:G;src:url(3)}"

(* Adding or dropping a whole block stays a difference. *)

let page_removed_reported () =
  let d =
    diff_of ~expected:"@page{margin:1cm}.a{color:red}" ~actual:".a{color:red}"
  in
  Alcotest.(check bool)
    "a dropped @page is a difference" false
    (Cascade_diff.Tree_diff.is_empty d)

(* An at-rule that carries no condition still has an identity of its own, and
   the ordering comparison reads it as a position marker. Describing every one
   of them the same way collapsed [@page] and [@starting-style] onto a single
   key, and a [@media] that moved between them read as no change - the very
   collapse the [@charset] versus [@namespace] naming above avoids. *)
let selectorless_at_rules_key_the_ordering () =
  let d =
    diff_of
      ~expected:
        "@page{margin:1cm}@media \
         print{a{color:red}}@starting-style{b{color:red}}"
      ~actual:
        "@page{margin:1cm}@starting-style{b{color:red}}@media \
         print{a{color:red}}"
  in
  Alcotest.(check bool)
    "a @media that moved between two of them is a difference" false
    (Cascade_diff.Tree_diff.is_empty d)

let font_face_added_reported () =
  let d =
    diff_of ~expected:".a{color:red}"
      ~actual:"@font-face{font-family:F;src:url(1)}.a{color:red}"
  in
  Alcotest.(check bool)
    "an added @font-face is a difference" false
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== One property written more than once in a rule ===== *)

(* A fallback chain writes one property several times, so a rule holds one value
   per occurrence and occurrence n on one side answers occurrence n on the
   other. Matching by name alone binds every occurrence to the first entry
   opposite, which names values neither side holds. *)

let rule_property_changes d =
  List.concat_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Content_changed { property_changes; _ } ->
          List.map
            (fun (p : Cascade_diff.Tree_diff.declaration) ->
              p.property_name ^ ": " ^ p.expected_value ^ " -> "
              ^ p.actual_value)
            property_changes
      | _ -> [])
    d.Cascade_diff.Tree_diff.rules

let rule_added_properties d =
  List.concat_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Content_changed { added_properties; _ } -> added_properties
      | _ -> [])
    d.Cascade_diff.Tree_diff.rules

let rule_removed_properties d =
  List.concat_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Content_changed { removed_properties; _ } -> removed_properties
      | _ -> [])
    d.Cascade_diff.Tree_diff.rules

let repeated_property_pairs_by_occurrence () =
  let d =
    diff_of ~expected:"a{color:red;color:blue}"
      ~actual:"a{color:red;color:green}"
  in
  Alcotest.(check (list string))
    "the occurrence that changed, against its counterpart"
    [ "color: blue -> green" ] (rule_property_changes d)

let repeated_property_pairs_every_occurrence () =
  let d =
    diff_of ~expected:"a{color:teal;color:red}"
      ~actual:"a{color:green;color:blue}"
  in
  Alcotest.(check (list string))
    "each occurrence against the one at its own index"
    [ "color: teal -> green"; "color: red -> blue" ]
    (rule_property_changes d)

let repeated_property_surplus_is_removed () =
  let d = diff_of ~expected:"a{color:red;color:blue}" ~actual:"a{color:red}" in
  Alcotest.(check (list string)) "no value changed" [] (rule_property_changes d);
  Alcotest.(check (list (pair string string)))
    "the occurrence with no counterpart is removed"
    [ ("color", "blue") ]
    (rule_removed_properties d)

let repeated_property_surplus_is_added () =
  let d = diff_of ~expected:"a{color:red}" ~actual:"a{color:red;color:blue}" in
  Alcotest.(check (list string)) "no value changed" [] (rule_property_changes d);
  Alcotest.(check (list (pair string string)))
    "the occurrence with no counterpart is added"
    [ ("color", "blue") ]
    (rule_added_properties d)

(* One side may split a selector's declarations around a compatibility
   container. Pairing repeated selectors by bucket order binds the combined rule
   to the trailing padding-only occurrence and invents two declaration losses.
   The closest declaration footprint is the rule before [@supports]. *)
let repeated_selector_pairs_by_declaration_similarity () =
  let d =
    diff_of
      ~expected:
        ":root{--c:#364153}.a{position:fixed;color:color-mix(in oklab,var(--c) \
         25%,transparent);padding:1px}"
      ~actual:
        ":root{--c:#364153}.a{position:fixed;color:#36415340}@supports(color:color-mix(in \
         lab,red,red)){.a{color:color-mix(in oklab,var(--c) \
         25%,transparent)}}.a{padding:1px}"
  in
  let out = render d in
  Alcotest.(check bool)
    "position survives" false
    (Astring.String.is_infix ~affix:"- position: fixed" out);
  Alcotest.(check bool)
    "padding survives" false
    (Astring.String.is_infix ~affix:"+ padding: 1px" out);
  Alcotest.(check bool)
    "the compatibility color is still reported" true
    (Astring.String.is_infix ~affix:"color" out)

(* Values do not make two occurrence footprints more similar. Otherwise these
   exact values pair across source positions and hide the changed last
   winner. *)
let repeated_selector_value_swap_remains_visible () =
  let d =
    diff_of ~expected:".a{color:red}.a{color:blue}"
      ~actual:".a{color:blue}.a{color:red}"
  in
  Alcotest.(check bool)
    "the last-wins change is reported" false
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Containers nested past the old recursion cutoff ===== *)

(* The walker recurses on strictly smaller statement lists, so nothing needs a
   depth cutoff to terminate; one at five levels of at-rule nesting made a leaf
   difference vanish, verdict and exit code included. *)

let nested_at_rules leaf =
  "@media (min-width:1px){@supports (display:grid){@media \
   (min-width:2px){@supports (display:flex){@media (min-width:3px){a{color:"
  ^ leaf ^ "}}}}}}"

let deeply_nested_leaf_change_reported () =
  let d =
    diff_of ~expected:(nested_at_rules "red") ~actual:(nested_at_rules "blue")
  in
  Alcotest.(check bool)
    "a leaf five containers down is still a difference" false
    (Cascade_diff.Tree_diff.is_empty d)

let deeply_nested_leaf_change_named () =
  let d =
    diff_of ~expected:(nested_at_rules "red") ~actual:(nested_at_rules "blue")
  in
  let s = render d in
  Alcotest.(check bool)
    "and the report names the value that changed" true
    (string_contains ~needle:"color: red -> blue" s)

let deeply_nested_identical_is_empty () =
  let css = nested_at_rules "red" in
  let d = diff_of ~expected:css ~actual:css in
  Alcotest.(check bool)
    "identical deep nesting stays empty" true
    (Cascade_diff.Tree_diff.is_empty d)

(* ===== Container position ===== *)

(* The container names carried by every [Reordered] container entry, at any
   depth. *)
let rec container_reorders (c : Cascade_diff.Tree_diff.container_diff) =
  match c with
  | Reordered { info = { condition; _ }; _ } -> [ condition ]
  | Modified { container_changes; _ } ->
      List.concat_map container_reorders container_changes
  | Added _ | Removed _ | Block_structure_changed _ -> []

let reordered_containers d =
  List.concat_map container_reorders d.Cascade_diff.Tree_diff.containers

(* Source order decides the winner between a conditional block and a rule that
   writes the same property on the same selector, so swapping the two is a
   difference whichever side the block starts on. *)
let media_swapped_with_rule_is_reported () =
  let d =
    diff_of ~expected:"@media (min-width:10px){a{color:red}}a{color:blue}"
      ~actual:"a{color:blue}@media (min-width:10px){a{color:red}}"
  in
  Alcotest.(check bool)
    "swapping a block with a rule is a difference" false
    (Cascade_diff.Tree_diff.is_empty d)

(* The mirror image of the case above, which the rule-level ordering already
   caught: both directions must report, and neither may report twice. *)
let rule_swapped_with_media_is_reported () =
  let d =
    diff_of ~expected:"a{color:blue}@media (min-width:10px){a{color:red}}"
      ~actual:"@media (min-width:10px){a{color:red}}a{color:blue}"
  in
  Alcotest.(check bool)
    "the swap is a difference in the other direction too" false
    (Cascade_diff.Tree_diff.is_empty d)

(* Control. Inserting a rule ahead of a block shifts its absolute index without
   moving it past anything, so the block did not move and must stay quiet. *)
let insertion_ahead_of_media_is_not_a_move () =
  let d =
    diff_of ~expected:"a{color:red}@media print{.b{top:0}}"
      ~actual:"a{color:red}.c{left:0}@media print{.b{top:0}}"
  in
  Alcotest.(check (list string))
    "an insertion is not a container move" [] (reordered_containers d)

(* Control. Same, with the insertion far enough ahead to shift the block past
   any fixed distance: absolute index is not the coordinate. *)
let distant_insertion_is_not_a_move () =
  let filler n =
    String.concat ""
      (List.init n (fun i ->
           let s = string_of_int i in
           ".f" ^ s ^ "{order:" ^ s ^ "}"))
  in
  let d =
    diff_of
      ~expected:("a{color:red}" ^ "@media print{.b{top:0}}")
      ~actual:("a{color:red}" ^ filler 12 ^ "@media print{.b{top:0}}")
  in
  Alcotest.(check (list string))
    "twelve insertions are still not a container move" []
    (reordered_containers d)

(* Each repeated condition is a distinct source-order participant. Keeping the
   first block fixed must not make a later block under the same condition
   invisible when it crosses a rule. *)
let later_repeated_media_move_is_reported () =
  let d =
    diff_of
      ~expected:
        "@media print{.a{color:red}}.x{color:blue}@media \
         print{.b{color:green}}.y{color:black}"
      ~actual:
        "@media print{.a{color:red}}.x{color:blue}.y{color:black}@media \
         print{.b{color:green}}"
  in
  Alcotest.(check (list string))
    "the later block is a move" [ "print" ] (reordered_containers d)

(* ===== Entries the report cannot name ===== *)

(* The names every rule-level entry claiming an addition or a removal carries.
   An entry with no name cannot be classified, so it corrupts the counts the
   summary prints from the same list. *)
let added_or_removed_names d =
  List.filter_map
    (fun (diff : Cascade_diff.Tree_diff.rule_diff) ->
      match diff with
      | Added { selector; _ } | Removed { selector; _ } -> Some selector
      | _ -> None)
    d.Cascade_diff.Tree_diff.rules

(* [@property] has a container processor of its own, which names the block it
   dropped; the rule level has no rule to name and prints a bare tree
   connector. *)
let removed_property_rule_is_named () =
  let d =
    diff_of ~expected:"@property --a{syntax:\"*\";inherits:false}.x{color:red}"
      ~actual:".x{color:red}"
  in
  Alcotest.(check bool)
    "dropping a registration is a difference" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check (list string))
    "no rule-level entry without a name" [] (added_or_removed_names d)

let removed_keyframes_is_named () =
  let d =
    diff_of ~expected:"@keyframes k{from{opacity:0}}.x{color:red}"
      ~actual:".x{color:red}"
  in
  Alcotest.(check (list string))
    "no rule-level entry without a name" [] (added_or_removed_names d)

(* A type selector matches in the namespace its sheet declares, so two sheets
   that name different namespace URLs style different elements. The description
   a prelude statement is keyed on has to carry the URL, or the two are one
   entry and the walk reports nothing. *)
let changed_namespace_is_a_difference () =
  let d =
    diff_of ~expected:"@namespace url(http://a.example);.x{color:red}"
      ~actual:"@namespace url(http://b.example);.x{color:red}"
  in
  Alcotest.(check bool)
    "changing the namespace URL is a difference" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "and the entry names the at-rule" true
    (string_contains ~needle:"@namespace" (render d))

(* The selectorless-at-rule machinery is what owns the namespace statement, so a
   change has to surface as a container entry, not fall through to the rule walk
   where it currently shares the universal selector and gets paired with other
   selectorless statements. *)
let changed_namespace_is_an_at_rule_container () =
  let d =
    diff_of ~expected:"@namespace url(http://a.example);.x{color:red}"
      ~actual:"@namespace url(http://b.example);.x{color:red}"
  in
  Alcotest.(check bool)
    "namespace URL change is reported as a container diff" true
    (Cascade_diff.Tree_diff.count_containers_by_type `At_rule d > 0)

let added_namespace_is_an_at_rule_container () =
  let d =
    diff_of ~expected:".x{color:red}"
      ~actual:"@namespace url(http://a.example);.x{color:red}"
  in
  Alcotest.(check bool)
    "adding a namespace is a difference" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "the added namespace is an at-rule container" true
    (Cascade_diff.Tree_diff.has_container_added_of_type `At_rule d)

(* Nothing else reports a [@charset], so the rule level has to keep it - and
   name it. *)
let removed_charset_is_named () =
  let d =
    diff_of ~expected:"@charset \"UTF-8\";.x{color:red}" ~actual:".x{color:red}"
  in
  Alcotest.(check bool)
    "dropping the charset is a difference" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "the entry names what was dropped" true
    (string_contains ~needle:"@charset" (render d))

(* A [@keyframes] block is an at-rule of its own. Naming it as a [@layer] prints
   "@layer @keyframes spin", which describes no CSS construct. *)
let modified_keyframes_names_the_at_rule () =
  let d =
    diff_of ~expected:"@keyframes spin{from{opacity:0}to{opacity:1}}"
      ~actual:"@keyframes spin{from{opacity:0}to{opacity:.5}}"
  in
  let s = render d in
  Alcotest.(check bool)
    "the entry names the keyframes block" true
    (string_contains ~needle:"@keyframes spin" s);
  Alcotest.(check bool)
    "and does not call it a layer" false
    (string_contains ~needle:"@layer" s)

(* A rule whose own declarations are untouched is not a difference: the nested
   rule that changed is reported as the nesting container it is, and a second
   entry naming the parent counts a difference the report then has nothing to
   show for. *)
let unchanged_parent_of_a_changed_nested_rule () =
  let d =
    diff_of ~expected:".a{color:red;&:hover{color:blue}}"
      ~actual:".a{color:red;&:hover{color:lime}}"
  in
  Alcotest.(check int)
    "the parent is no top-level difference" 0
    (List.length d.Cascade_diff.Tree_diff.rules);
  Alcotest.(check bool)
    "and the nested change is reported" false
    (Cascade_diff.Tree_diff.is_empty d)

let unchanged_parent_of_an_added_nested_rule () =
  let d =
    diff_of ~expected:".a{color:red}"
      ~actual:".a{color:red;&:hover{color:blue}}"
  in
  Alcotest.(check int)
    "the parent is no top-level difference" 0
    (List.length d.Cascade_diff.Tree_diff.rules);
  Alcotest.(check bool)
    "and the nested rule is reported" false
    (Cascade_diff.Tree_diff.is_empty d)

(* Naming a frame modified without saying what changed states a difference the
   report then withholds; a frame carries its declarations like any other
   rule. *)
let modified_keyframe_shows_its_change () =
  let d =
    diff_of ~expected:"@keyframes fade{0%{opacity:0}100%{opacity:1}}"
      ~actual:"@keyframes fade{0%{opacity:.7}100%{opacity:1}}"
  in
  let s = render d in
  Alcotest.(check bool)
    "the frame that changed is named" true
    (string_contains ~needle:"0%" s);
  Alcotest.(check bool)
    "and the property change is shown" true
    (string_contains ~needle:"opacity" s)

let added_keyframe_shows_its_declarations () =
  let d =
    diff_of ~expected:"@keyframes fade{0%{opacity:0}}"
      ~actual:"@keyframes fade{0%{opacity:0}100%{opacity:1}}"
  in
  let s = render d in
  Alcotest.(check bool)
    "the frame that arrived carries its declarations" true
    (string_contains ~needle:"opacity" s)

let removed_layer_statement_is_named () =
  let d =
    diff_of ~expected:"@layer a,b;.x{color:red}" ~actual:".x{color:red}"
  in
  Alcotest.(check bool)
    "dropping the layer order is a difference" false
    (Cascade_diff.Tree_diff.is_empty d);
  Alcotest.(check bool)
    "the entry names what was dropped" true
    (string_contains ~needle:"@layer" (render d))

(* ===== Cascade layer order ===== *)

(* An empty [@layer] statement pins the layer order at the point it stands, so
   [@layer a;] ahead of a [@layer b] block makes [a] the weaker layer, and
   dropping it makes [b] weaker instead. The two sheets hold the same two
   [@layer] blocks with the same bodies, so nothing but the declared order tells
   them apart, and they resolve a conflict between [a] and [b] the opposite
   way. *)
let layer_order_pin_is_reported () =
  let expected = parse "@layer a;@layer b{y{top:1px}}@layer a{x{top:0}}" in
  let actual = parse "@layer b{y{top:1px}}@layer a{x{top:0}}" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "dropping the pin swaps the two layers" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "and the report names the pair that swapped" true
    (string_contains ~needle:"b now precedes a" s);
  Alcotest.(check bool)
    "canonical mode reports it too" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       "@layer a;@layer b{y{top:1px}}@layer a{x{top:0}}"
       "@layer b{y{top:1px}}@layer a{x{top:0}}")

(* Same order, two spellings of the pin: [@layer a;@layer b;] and [@layer a,b;]
   both declare [a] weaker than [b]. Nothing changed, so the report stays
   quiet. *)
let layer_order_declared_two_ways_is_quiet () =
  let expected =
    parse "@layer a;@layer b;@layer b{y{top:1px}}@layer a{x{top:0}}"
  in
  let actual = parse "@layer a,b;@layer b{y{top:1px}}@layer a{x{top:0}}" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "one order written two ways is no difference" true
    (Cascade_diff.Tree_diff.is_empty d)

(* A sublayer sorts inside its parent, so [@layer a.b;] pins [a.b] ahead of the
   [a.c] that the block below declares first. Dropping the pin swaps the two
   sublayers while leaving [a] itself, and both bodies, where they were. *)
let nested_layer_order_pin_is_reported () =
  let expected =
    parse "@layer a.b;@layer a{@layer c{x{top:0}}@layer b{y{top:1px}}}"
  in
  let actual = parse "@layer a{@layer c{x{top:0}}@layer b{y{top:1px}}}" in
  let d = Cascade_diff.Tree_diff.diff ~expected ~actual in
  Alcotest.(check bool)
    "the sublayers swapped" false
    (Cascade_diff.Tree_diff.is_empty d);
  let s = render d in
  Alcotest.(check bool)
    "and the report names them by their dotted paths" true
    (string_contains ~needle:"a.c now precedes a.b" s)

(* A block added or dropped wholesale is rendered from its own body, and the
   header claims children the reader then has to find. Reading the body through
   a private list of at-rules left a [@scope], a [@starting-style] and a nested
   rule printing a header with nothing under it, which reads as an empty block
   rather than one whose contents the report failed to walk. *)
let wholesale_block_shows_every_child () =
  let cases =
    [
      ( "@scope inside an added @media",
        "@media print{@scope(.card){.x{color:blue}.y{color:green}}}",
        [ "@scope(.card)"; ".x"; ".y" ] );
      ( "@starting-style inside an added @layer",
        "@layer l{@starting-style{.x{color:blue}}}",
        [ "@starting-style"; ".x" ] );
      ( "a nested rule inside an added @media",
        "@media print{.x{color:blue;.y{color:green}}}",
        [ ".x"; ".y" ] );
      ( "@when inside an added @supports",
        "@supports (top:0){@when media(screen){.x{color:blue}}}",
        [ ".x" ] );
    ]
  in
  List.iter
    (fun (name, added, wanted) ->
      let out =
        render
          (diff_of ~expected:".keep{color:red}"
             ~actual:(".keep{color:red}" ^ added))
      in
      List.iter
        (fun affix ->
          Alcotest.(check bool)
            (name ^ ": the tree names " ^ affix)
            true
            (Astring.String.is_infix ~affix out))
        wanted)
    cases

(* ===== Whitespace the matching key folds ===== *)

(* Each side is one rule holding one declaration, so the report is empty exactly
   when the two spellings share a matching key. *)
let value_pair ~folds ~name one other () =
  let wrap decl = String.concat "" [ "a{"; decl; "}" ] in
  let d = diff_of ~expected:(wrap one) ~actual:(wrap other) in
  Alcotest.(check bool) name folds (Cascade_diff.Tree_diff.is_empty d)

let folded_value = value_pair ~folds:true
let distinct_value = value_pair ~folds:false

(* CSS Values 4 (ED) sec. 10.8 leaves the whitespace around [*] and [/] optional
   inside a math function, so both streams substitute the same tokens. *)
let key_folds_whitespace_in_calc =
  folded_value ~name:"a spaced calc division is the spaceless one"
    "--k:calc(2 / 1.5)" "--k:calc(2/1.5)"

(* Outside a math function the whitespace next to a [*] or [/] is insignificant
   too (CSS Values 4 sec. 10.1): [16 / 9] and [16/9] re-tokenise identically. *)
let key_folds_whitespace_around_a_bare_slash =
  folded_value ~name:"a spaced ratio is the spaceless one" "--k:16 / 9"
    "--k:16/9"

(* A function that is not a substitution function closes on [)], a token
   boundary nothing on its right can merge across, so the space after it
   separates nothing. *)
let key_folds_whitespace_after_a_function =
  folded_value ~name:"a space after a closed function is nothing"
    "--k:cubic-bezier(.4,0,.6,1) infinite" "--k:cubic-bezier(.4,0,.6,1)infinite"

(* CSS Syntax 3 ends a percentage token at the [%], so the number that follows
   cannot merge into it and the space between them carries nothing. *)
let key_folds_whitespace_after_a_percentage =
  folded_value ~name:"a space after a percentage is nothing"
    "--k:oklch(63.7% .237 25.331)" "--k:oklch(63.7%.237 25.331)"

(* The key reads a typed value through the same minified spelling, so a number
   written long and a number written short stop reading as a change. *)
let key_folds_a_typed_number_spelling =
  folded_value ~name:"a padded number is the short one" "padding:0.50px"
    "padding:.5px"

(* ===== Whitespace the matching key keeps ===== *)

(* Sec. 10.8 requires the whitespace around a binary [+] or [-]: [100%- 10px] is
   not the same declaration written shorter, it is one the browser drops. *)
let key_keeps_whitespace_around_a_calc_sum =
  distinct_value ~name:"a calc sum missing its space is a difference"
    "--k:calc(100% - 10px)" "--k:calc(100%- 10px)"

(* CSS Values 4 sec. 2.5 substitutes a [var()] body textually, so the space
   after one separates the two substituted streams and is part of the value. *)
let key_keeps_whitespace_between_two_vars =
  distinct_value ~name:"a space between two var() is a difference"
    "--k:var(--a) var(--b)" "--k:var(--a)var(--b)"

let key_keeps_whitespace_after_a_var =
  distinct_value ~name:"a space after a var() is a difference"
    "--k:var(--a) 10px" "--k:var(--a)10px"

(* Two idents against one: the streams hold different tokens, whatever the
   property they are substituted into reads them as. *)
let key_keeps_whitespace_between_two_idents =
  distinct_value ~name:"two idents are not one" "--k:a b" "--k:ab"

(* No generic family in the stream, so nothing proves it is a font-family list
   and a [<string>] there is not the equivalent [<ident>] sequence. *)
let key_keeps_quotes_without_a_generic_family =
  distinct_value ~name:"an unproven quoted family is a difference"
    "--k:a,\"Segoe UI\",b" "--k:a,Segoe UI,b"

(* Two typed values that differ still differ, however far the minified spelling
   shortens each of them. *)
let key_keeps_two_typed_values_apart =
  distinct_value ~name:"two colours are a difference" "color:#ff0000"
    "color:#ff1100"

(* Every affix the report of [expected] against [actual] has to read. *)
let report_reads ~name ~expected ~actual affixes =
  let out = render (diff_of ~expected ~actual) in
  List.iter
    (fun affix ->
      Alcotest.(check bool)
        (String.concat "" [ name; " reads "; affix ])
        true
        (Astring.String.is_infix ~affix out))
    affixes

(* The key is for matching, not for display: a pair that still differs prints
   the bytes each side's author wrote, not the spelling the key folded them
   onto, which is a value neither file holds. *)
let report_quotes_the_author_spelling () =
  report_reads ~name:"a custom stream" ~expected:"a{--k:calc(100% - 10px)}"
    ~actual:"a{--k:calc(100%- 10px)}"
    [ "calc(100% - 10px)"; "calc(100%- 10px)" ];
  report_reads ~name:"a typed colour" ~expected:"a{color:#ff0000}"
    ~actual:"a{color:#ff1100}" [ "#ff0000"; "#ff1100" ];
  report_reads ~name:"a quoted family"
    ~expected:"a{--f:ui-sans-serif,\"Noto Color Emoji\"}"
    ~actual:"a{--f:ui-serif,\"Noto Color Emoji\"}"
    [ "ui-sans-serif,\"Noto Color Emoji\""; "ui-serif,\"Noto Color Emoji\"" ]

(* An added or removed rule shows its whole body, and that body decides which
   declaration wins: [color:red] and [color:red !important] are not the same
   rule. Dropping the flag reports the two as one. *)
let whole_rule_body_carries_the_flag () =
  let base = "a{color:blue}" in
  let gained = "a{color:blue}b{color:red!important;margin:0}" in
  report_reads ~name:"an added rule" ~expected:base ~actual:gained
    [ "+ color: red !important"; "+ margin: 0" ];
  report_reads ~name:"a removed rule" ~expected:gained ~actual:base
    [ "- color: red !important"; "- margin: 0" ]

(* A value may hold a colon of its own, so the body needs the separator a
   declaration is written with to say where the property name ended. *)
let whole_rule_body_separates_name_from_value () =
  let base = "a{color:blue}" in
  let gained =
    "a{color:blue}b{background:url(http://x/y.png);--k:var(--u, 1px)}"
  in
  report_reads ~name:"a url value" ~expected:base ~actual:gained
    [ "+ background: url(http://x/y.png)" ];
  report_reads ~name:"a custom property" ~expected:base ~actual:gained
    [ "+ --k: var(--u, 1px)" ]

(* --- allocation / complexity guard --- *)

let changed_rules colour n =
  let buf = Buffer.create (n * 24) in
  let out = Fmt.with_buffer buf in
  for i = 0 to n - 1 do
    Fmt.pf out ".c%d{color:%s}" i colour
  done;
  parse (Buffer.contents buf)

let diff_words expected actual =
  Css_test_helpers.measure (fun () ->
      Cascade_diff.Tree_diff.diff ~expected ~actual)

(* Grouping the reported changes by scanning the whole change list once per
   entry makes allocation quadratic in the number of changed rules. Doubling the
   sheet must stay well below that 4x slope. *)
let changed_rule_diff_is_subquadratic () =
  let small_expected = changed_rules "red" 1_000 in
  let small_actual = changed_rules "blue" 1_000 in
  let large_expected = changed_rules "red" 2_000 in
  let large_actual = changed_rules "blue" 2_000 in
  let small_words = diff_words small_expected small_actual in
  let large_words = diff_words large_expected large_actual in
  let ratio = large_words /. small_words in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx for 2x changes)" small_words large_words
       ratio)
    true (ratio < 3.)

let suite =
  ( "tree_diff",
    [
      Alcotest.test_case "layer-order pin reported" `Quick
        layer_order_pin_is_reported;
      Alcotest.test_case "one layer order written two ways is quiet" `Quick
        layer_order_declared_two_ways_is_quiet;
      Alcotest.test_case "nested layer-order pin reported" `Quick
        nested_layer_order_pin_is_reported;
      Alcotest.test_case "media swapped with rule is reported" `Quick
        media_swapped_with_rule_is_reported;
      Alcotest.test_case "rule swapped with media is reported" `Quick
        rule_swapped_with_media_is_reported;
      Alcotest.test_case "insertion ahead of media is not a move" `Quick
        insertion_ahead_of_media_is_not_a_move;
      Alcotest.test_case "distant insertion is not a move" `Quick
        distant_insertion_is_not_a_move;
      Alcotest.test_case "later repeated media move is reported" `Quick
        later_repeated_media_move_is_reported;
      Alcotest.test_case "removed property rule is named" `Quick
        removed_property_rule_is_named;
      Alcotest.test_case "removed keyframes is named" `Quick
        removed_keyframes_is_named;
      Alcotest.test_case "removed charset is named" `Quick
        removed_charset_is_named;
      Alcotest.test_case "changed namespace is a difference" `Quick
        changed_namespace_is_a_difference;
      Alcotest.test_case "changed namespace is an at-rule container" `Quick
        changed_namespace_is_an_at_rule_container;
      Alcotest.test_case "added namespace is an at-rule container" `Quick
        added_namespace_is_an_at_rule_container;
      Alcotest.test_case "removed layer statement is named" `Quick
        removed_layer_statement_is_named;
      Alcotest.test_case "unchanged parent of a changed nested rule" `Quick
        unchanged_parent_of_a_changed_nested_rule;
      Alcotest.test_case "unchanged parent of an added nested rule" `Quick
        unchanged_parent_of_an_added_nested_rule;
      Alcotest.test_case "modified keyframe shows its change" `Quick
        modified_keyframe_shows_its_change;
      Alcotest.test_case "added keyframe shows its declarations" `Quick
        added_keyframe_shows_its_declarations;
      Alcotest.test_case "modified keyframes names the at-rule" `Quick
        modified_keyframes_names_the_at_rule;
      Alcotest.test_case "selector group split reported" `Quick
        diff_selector_group_split_reported;
      Alcotest.test_case "selector group merge reported" `Quick
        diff_selector_group_merge_reported;
      Alcotest.test_case "selector group partial change" `Quick
        diff_selector_group_partial_change;
      Alcotest.test_case "identical" `Quick diff_identical;
      Alcotest.test_case "identical multiple rules" `Quick
        diff_identical_multiple_rules;
      Alcotest.test_case "empty stylesheets" `Quick diff_empty_stylesheets;
      Alcotest.test_case "rule added" `Quick diff_rule_added;
      Alcotest.test_case "rule removed" `Quick diff_rule_removed;
      Alcotest.test_case "property changed" `Quick diff_property_changed;
      Alcotest.test_case "property added to rule" `Quick
        diff_rule_added_property;
      Alcotest.test_case "rule reordered" `Quick diff_rule_reordered;
      Alcotest.test_case "swap reports one reorder" `Quick
        diff_swap_reports_one_reorder;
      Alcotest.test_case "move names the rule that moved" `Quick
        diff_move_names_the_rule_that_moved;
      Alcotest.test_case "swap alone reports the reorder" `Quick
        diff_swap_alone_reports_the_reorder;
      Alcotest.test_case "swap with a modified rule reports both" `Quick
        diff_swap_with_modified_rule_reports_both;
      Alcotest.test_case "swap with an added rule reports both" `Quick
        diff_swap_with_added_rule_reports_both;
      Alcotest.test_case "move with a modified rule names the mover" `Quick
        diff_move_with_modified_rule_names_the_mover;
      Alcotest.test_case "dropped rules are not a reorder" `Quick
        diff_dropped_rules_are_not_a_reorder;
      Alcotest.test_case "dropped rules keep the declaration reorder" `Quick
        diff_dropped_rules_keep_the_decl_reorder;
      Alcotest.test_case "neutral declaration reorder is empty" `Quick
        diff_neutral_decl_reorder_is_empty;
      Alcotest.test_case "overlapping declaration reorder flagged" `Quick
        diff_overlapping_decl_reorder_flagged;
      Alcotest.test_case "declaration reorder in media flagged" `Quick
        diff_media_decl_reorder_flagged;
      Alcotest.test_case "declaration reorder in layer flagged" `Quick
        diff_layer_decl_reorder_flagged;
      Alcotest.test_case "declaration reorder in supports flagged" `Quick
        diff_supports_decl_reorder_flagged;
      Alcotest.test_case "neutral declaration reorder in media is empty" `Quick
        diff_media_neutral_decl_reorder_empty;
      Alcotest.test_case "neutral declaration reorder in layer is empty" `Quick
        diff_layer_neutral_decl_reorder_empty;
      Alcotest.test_case "neutral declaration reorder in supports is empty"
        `Quick diff_supports_neutral_decl_reorder_empty;
      Alcotest.test_case "property syntax changed" `Quick
        diff_property_syntax_changed;
      Alcotest.test_case "property inherits changed" `Quick
        diff_property_inherits_changed;
      Alcotest.test_case "property identical is empty" `Quick
        diff_property_identical_is_empty;
      Alcotest.test_case "property initial-value added" `Quick
        diff_property_initial_value_added;
      Alcotest.test_case "media added" `Quick diff_media_added;
      Alcotest.test_case "media removed" `Quick diff_media_removed;
      Alcotest.test_case "layer added" `Quick diff_layer_added;
      Alcotest.test_case "single_rule_diff one change" `Quick
        single_rule_diff_one_change;
      Alcotest.test_case "single_rule_diff no change" `Quick
        single_rule_diff_no_change;
      Alcotest.test_case "single_rule_diff multiple changes" `Quick
        single_rule_diff_multiple_changes;
      Alcotest.test_case "count containers media" `Quick count_containers_media;
      Alcotest.test_case "count containers zero" `Quick count_containers_zero;
      Alcotest.test_case "nested container added is found" `Quick
        nested_container_added_is_found;
      Alcotest.test_case "nested container removed is found" `Quick
        nested_container_removed_is_found;
      Alcotest.test_case "nested container added is not removed" `Quick
        nested_container_added_is_not_removed;
      Alcotest.test_case "nesting modified" `Quick diff_nesting_modified;
      Alcotest.test_case "nesting identical" `Quick diff_nesting_identical;
      Alcotest.test_case "nesting child added" `Quick diff_nesting_child_added;
      Alcotest.test_case "nesting child removed" `Quick
        diff_nesting_child_removed;
      Alcotest.test_case "nesting deep" `Quick diff_nesting_deep;
      Alcotest.test_case "nesting only parent props changed" `Quick
        diff_nesting_parent_props_only;
      Alcotest.test_case "nesting under a reordered selector list" `Quick
        diff_nesting_under_reordered_selector_list;
      Alcotest.test_case "selector list reorder is not a change" `Quick
        diff_selector_list_reorder_is_not_a_change;
      Alcotest.test_case "duplicate condition blocks reconcile" `Quick
        duplicate_condition_blocks_reconcile;
      Alcotest.test_case "rearranged reported" `Quick rearranged_reported;
      Alcotest.test_case "rearranged is a difference" `Quick
        rearranged_is_a_difference;
      Alcotest.test_case "rearranged reports one node" `Quick
        rearranged_reports_one_node;
      Alcotest.test_case "rearranged names the move" `Quick
        rearranged_names_the_move;
      Alcotest.test_case "rearranged same at either depth" `Quick
        rearranged_same_at_either_depth;
      Alcotest.test_case "lost declaration is not a move" `Quick
        lost_declaration_is_not_a_move;
      Alcotest.test_case "gained declaration is not a move" `Quick
        gained_declaration_is_not_a_move;
      Alcotest.test_case "gained declaration shows value" `Quick
        gained_declaration_shows_value;
      Alcotest.test_case "changed value is not a move" `Quick
        changed_value_is_not_a_move;
      Alcotest.test_case "added important is not a move" `Quick
        added_important_is_not_a_move;
      Alcotest.test_case "one of two duplicates lost is not a move" `Quick
        one_of_two_duplicates_lost_is_not_a_move;
      Alcotest.test_case "unrelated selectors are not a move" `Quick
        unrelated_selectors_are_not_a_move;
      Alcotest.test_case "rearranged survives pp_rule_diff_simple" `Quick
        rearranged_survives_pp_simple;
      Alcotest.test_case "@page body change reported" `Quick
        page_body_change_reported;
      Alcotest.test_case "@starting-style body change reported" `Quick
        starting_style_body_change_reported;
      Alcotest.test_case "@counter-style body change reported" `Quick
        counter_style_body_change_reported;
      Alcotest.test_case "@scope body change reported" `Quick
        scope_body_change_reported;
      Alcotest.test_case "repeated @font-face change reported" `Quick
        repeated_font_face_change_reported;
      Alcotest.test_case "@page removed reported" `Quick page_removed_reported;
      Alcotest.test_case "selectorless at-rules key the ordering" `Quick
        selectorless_at_rules_key_the_ordering;
      Alcotest.test_case "@font-face added reported" `Quick
        font_face_added_reported;
      Alcotest.test_case "repeated property pairs by occurrence" `Quick
        repeated_property_pairs_by_occurrence;
      Alcotest.test_case "repeated property pairs every occurrence" `Quick
        repeated_property_pairs_every_occurrence;
      Alcotest.test_case "repeated property surplus is removed" `Quick
        repeated_property_surplus_is_removed;
      Alcotest.test_case "repeated property surplus is added" `Quick
        repeated_property_surplus_is_added;
      Alcotest.test_case "repeated selector pairs by declaration similarity"
        `Quick repeated_selector_pairs_by_declaration_similarity;
      Alcotest.test_case "repeated selector value swap remains visible" `Quick
        repeated_selector_value_swap_remains_visible;
      Alcotest.test_case "deeply nested leaf change reported" `Quick
        deeply_nested_leaf_change_reported;
      Alcotest.test_case "deeply nested leaf change named" `Quick
        deeply_nested_leaf_change_named;
      Alcotest.test_case "deeply nested identical is empty" `Quick
        deeply_nested_identical_is_empty;
      Alcotest.test_case "wholesale block shows every child" `Quick
        wholesale_block_shows_every_child;
      Alcotest.test_case "key folds whitespace in calc" `Quick
        key_folds_whitespace_in_calc;
      Alcotest.test_case "key folds whitespace around a bare slash" `Quick
        key_folds_whitespace_around_a_bare_slash;
      Alcotest.test_case "key folds whitespace after a function" `Quick
        key_folds_whitespace_after_a_function;
      Alcotest.test_case "key folds whitespace after a percentage" `Quick
        key_folds_whitespace_after_a_percentage;
      Alcotest.test_case "key folds a typed number spelling" `Quick
        key_folds_a_typed_number_spelling;
      Alcotest.test_case "key keeps whitespace around a calc sum" `Quick
        key_keeps_whitespace_around_a_calc_sum;
      Alcotest.test_case "key keeps whitespace between two vars" `Quick
        key_keeps_whitespace_between_two_vars;
      Alcotest.test_case "key keeps whitespace after a var" `Quick
        key_keeps_whitespace_after_a_var;
      Alcotest.test_case "key keeps whitespace between two idents" `Quick
        key_keeps_whitespace_between_two_idents;
      Alcotest.test_case "key keeps quotes without a generic family" `Quick
        key_keeps_quotes_without_a_generic_family;
      Alcotest.test_case "key keeps two typed values apart" `Quick
        key_keeps_two_typed_values_apart;
      Alcotest.test_case "report quotes the author spelling" `Quick
        report_quotes_the_author_spelling;
      Alcotest.test_case "whole rule body carries the flag" `Quick
        whole_rule_body_carries_the_flag;
      Alcotest.test_case "whole rule body separates name from value" `Quick
        whole_rule_body_separates_name_from_value;
      Alcotest.test_case "changed-rule diff is subquadratic" `Quick
        changed_rule_diff_is_subquadratic;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "pp_rule_diff_simple does not crash" `Quick
        pp_rule_diff_simple_ok;
    ] )
