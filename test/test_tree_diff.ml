(** Tests for Cascade_diff.Tree_diff module *)

open Cascade

let parse css = Css.of_string_exn ~strict:false css

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
  Alcotest.(check (list string))
    "names the descriptor gained" [ "initial-value" ] added

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

let string_contains ~needle hay =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  go 0

let render d =
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  Buffer.contents buf

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

let render d =
  let buf = Buffer.create 256 in
  Cascade_diff.Tree_diff.pp buf d;
  Buffer.contents buf

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
    (Astring.String.is_infix ~affix:"- margin" (render d))

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
  Alcotest.(check (list string))
    "the occurrence with no counterpart is removed" [ "color" ]
    (rule_removed_properties d)

let repeated_property_surplus_is_added () =
  let d = diff_of ~expected:"a{color:red}" ~actual:"a{color:red;color:blue}" in
  Alcotest.(check (list string)) "no value changed" [] (rule_property_changes d);
  Alcotest.(check (list string))
    "the occurrence with no counterpart is added" [ "color" ]
    (rule_added_properties d)

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
      Alcotest.test_case "removed property rule is named" `Quick
        removed_property_rule_is_named;
      Alcotest.test_case "removed keyframes is named" `Quick
        removed_keyframes_is_named;
      Alcotest.test_case "removed charset is named" `Quick
        removed_charset_is_named;
      Alcotest.test_case "removed layer statement is named" `Quick
        removed_layer_statement_is_named;
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
      Alcotest.test_case "nesting modified" `Quick diff_nesting_modified;
      Alcotest.test_case "nesting identical" `Quick diff_nesting_identical;
      Alcotest.test_case "nesting child added" `Quick diff_nesting_child_added;
      Alcotest.test_case "nesting child removed" `Quick
        diff_nesting_child_removed;
      Alcotest.test_case "nesting deep" `Quick diff_nesting_deep;
      Alcotest.test_case "nesting only parent props changed" `Quick
        diff_nesting_parent_props_only;
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
      Alcotest.test_case "deeply nested leaf change reported" `Quick
        deeply_nested_leaf_change_reported;
      Alcotest.test_case "deeply nested leaf change named" `Quick
        deeply_nested_leaf_change_named;
      Alcotest.test_case "deeply nested identical is empty" `Quick
        deeply_nested_identical_is_empty;
      Alcotest.test_case "pp does not crash" `Quick pp_does_not_crash;
      Alcotest.test_case "pp_rule_diff_simple does not crash" `Quick
        pp_rule_diff_simple_ok;
    ] )
