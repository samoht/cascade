open Cascade
module C = Cascade_diff.Css_compare
module D = Cascade_diff.Tree_diff
open Diff_report_index

(* ===== What a report names ===== *)

let rec container_names (c : D.container_diff) =
  match c with
  | Added i | Removed i -> [ i.condition ]
  | Reordered { info; _ } -> [ info.condition ]
  | Block_structure_changed { condition; _ } -> [ condition ]
  | Modified { info; container_changes; _ } ->
      info.condition :: List.concat_map container_names container_changes

let rule_names (r : D.rule_diff) =
  match r with
  | Added { selector; _ }
  | Removed { selector; _ }
  | Rearranged { selector; _ }
  | Reordered { selector; _ }
  | Content_changed { selector; _ } ->
      [ selector ]
  | Selector_changed { old_selector; new_selector; _ } ->
      [ old_selector; new_selector ]
  | Regrouped { from_selectors; to_selectors } -> from_selectors @ to_selectors

(* A container that came or went names every rule it carries, so those rules are
   named by the report even though no rule entry mentions them. *)
let carried stmts =
  List.concat_map
    (fun stmt -> List.map (fun e -> e.selector) (rule_index (Css.v [ stmt ])))
    stmts

let rec container_rule_names (c : D.container_diff) =
  match c with
  | Added info | Removed info | Reordered { info; _ } -> carried info.rules
  | Block_structure_changed { expected_blocks; actual_blocks; _ } ->
      List.concat_map (fun (_, stmts) -> carried stmts) expected_blocks
      @ List.concat_map (fun (_, stmts) -> carried stmts) actual_blocks
  | Modified { rule_changes; container_changes; _ } ->
      List.concat_map rule_names rule_changes
      @ List.concat_map container_rule_names container_changes

(* An entry that asserts nothing: a rule reported as modified whose before and
   after are the same declarations and whose three change lists are empty. It
   names a rule that did not change, it is counted by the summary and written
   into the JSON document, and the human report cannot print it. *)
let vacuous (r : D.rule_diff) =
  match r with
  | Content_changed
      {
        selector;
        old_declarations;
        new_declarations;
        property_changes = [];
        added_properties = [];
        removed_properties = [];
      }
    when List.length old_declarations = List.length new_declarations
         && List.for_all2
              (fun a b ->
                String.equal (Css.declaration_name a) (Css.declaration_name b)
                && String.equal (shown_value a) (shown_value b))
              old_declarations new_declarations ->
      Some selector
  | _ -> None

let rec container_vacuous (c : D.container_diff) =
  match c with
  | Added _ | Removed _ | Reordered _ | Block_structure_changed _ -> []
  | Modified { rule_changes; container_changes; _ } ->
      List.filter_map vacuous rule_changes
      @ List.concat_map container_vacuous container_changes

let vacuous_entries (d : D.t) =
  List.filter_map vacuous d.rules
  @ List.concat_map container_vacuous d.containers

(* Only the entries that assert something. An entry naming a rule and showing no
   change of it is a finding of its own, reported by [vacuous_entries]; counting
   it here would report the same defect twice, once under the wrong heading. *)
let named_selectors (d : D.t) =
  List.concat_map
    (fun r -> if Option.is_some (vacuous r) then [] else rule_names r)
    d.rules
  @ List.concat_map container_rule_names d.containers
  |> List.filter (fun s -> not (String.equal s ""))
  |> List.sort_uniq String.compare

let named_containers (d : D.t) =
  List.concat_map container_names d.containers |> List.sort_uniq String.compare

(* Every selector a swap also reports as reordered because a swap moves other
   rules past it: the report's own [swapped_with] field. *)
let swap_partners (d : D.t) =
  List.filter_map
    (fun (r : D.rule_diff) ->
      match r with Reordered { swapped_with; _ } -> swapped_with | _ -> None)
    d.rules

(* ===== Claim verification ===== *)

(* Each entry of a report asserts something about the two files. [claims] turns
   one entry into those assertions, each with the text a finding shows. *)

type claim = { text : string; ok : bool }

let claim text ok = { text; ok }
let quoted parts = String.concat "" parts

let decl_claims ~side ~sel ~idx ~what decls =
  List.map
    (fun d ->
      let p = Css.declaration_name d and v = shown_value d in
      claim
        (quoted
           [ what; " "; sel; " { "; p; ": "; v; " } in the "; side; " sheet" ])
        (has_decl idx sel p v))
    decls

let prop_claims ~side ~sel ~idx ~what props =
  List.map
    (fun (p, v) ->
      claim
        (quoted
           [ what; " "; sel; " { "; p; ": "; v; " } in the "; side; " sheet" ])
        (has_decl idx sel p v))
    props

let rule_claims ~expected ~actual (r : D.rule_diff) =
  match r with
  | Added { selector; declarations } ->
      claim
        (quoted [ "added rule "; selector; " is in the actual sheet" ])
        (has_selector actual selector)
      :: decl_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"added"
           declarations
  | Removed { selector; declarations } ->
      claim
        (quoted [ "removed rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: decl_claims ~side:"expected" ~sel:selector ~idx:expected
           ~what:"removed" declarations
  | Content_changed
      { selector; property_changes; added_properties; removed_properties; _ }
    when not (String.equal selector "") ->
      claim
        (quoted [ "modified rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: claim
           (quoted [ "modified rule "; selector; " is in the actual sheet" ])
           (has_selector actual selector)
      :: List.concat_map
           (fun (c : D.declaration) ->
             [
               claim
                 (quoted
                    [
                      selector;
                      " { ";
                      c.property_name;
                      ": ";
                      c.expected_value;
                      " } in the expected sheet";
                    ])
                 (has_decl expected selector c.property_name c.expected_value);
               claim
                 (quoted
                    [
                      selector;
                      " { ";
                      c.property_name;
                      ": ";
                      c.actual_value;
                      " } in the actual sheet";
                    ])
                 (has_decl actual selector c.property_name c.actual_value);
               claim
                 (quoted
                    [
                      selector;
                      " ";
                      c.property_name;
                      ": the two values it reports differ";
                    ])
                 (not (String.equal c.expected_value c.actual_value));
             ])
           property_changes
      @ prop_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"added"
          added_properties
      @ prop_claims ~side:"expected" ~sel:selector ~idx:expected ~what:"removed"
          removed_properties
  | Content_changed _ -> []
  | Selector_changed { old_selector; new_selector; _ } ->
      [
        claim
          (quoted
             [
               "selector changed from "; old_selector; ", in the expected sheet";
             ])
          (has_selector expected old_selector);
        claim
          (quoted
             [ "selector changed to "; new_selector; ", in the actual sheet" ])
          (has_selector actual new_selector);
      ]
  | Reordered { selector; _ } ->
      [
        claim
          (quoted [ "reordered rule "; selector; " is in the expected sheet" ])
          (has_selector expected selector);
        claim
          (quoted [ "reordered rule "; selector; " is in the actual sheet" ])
          (has_selector actual selector);
      ]
  | Rearranged { selector; declarations } ->
      claim
        (quoted [ "rearranged rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: claim
           (quoted [ "rearranged rule "; selector; " is in the actual sheet" ])
           (has_selector actual selector)
      :: decl_claims ~side:"expected" ~sel:selector ~idx:expected ~what:"kept"
           declarations
      @ decl_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"kept"
          declarations
  | Regrouped { from_selectors; to_selectors } ->
      List.map
        (fun s ->
          claim
            (quoted [ "regrouped from "; s; ", in the expected sheet" ])
            (has_selector expected s))
        from_selectors
      @ List.map
          (fun s ->
            claim
              (quoted [ "regrouped to "; s; ", in the actual sheet" ])
              (has_selector actual s))
          to_selectors

(* A container names the rules it carries; those rules are in the sheet the
   container is claimed to be in. *)
let container_rules_claims ~side ~idx ~what stmts =
  List.concat_map
    (fun stmt ->
      List.concat_map
        (fun e ->
          claim
            (quoted
               [
                 what;
                 " container holds ";
                 e.selector;
                 " in the ";
                 side;
                 " sheet";
               ])
            (has_selector idx e.selector)
          :: List.map
               (fun (p, v) ->
                 claim
                   (quoted
                      [
                        what;
                        " container holds ";
                        e.selector;
                        " { ";
                        p;
                        ": ";
                        v;
                        " } in the ";
                        side;
                        " sheet";
                      ])
                   (has_decl idx e.selector p v))
               e.props)
        (rule_index (Css.v [ stmt ])))
    stmts

let rec container_claims ~expected ~actual (c : D.container_diff) =
  match c with
  | Added info ->
      container_rules_claims ~side:"actual" ~idx:actual ~what:"added" info.rules
  | Removed info ->
      container_rules_claims ~side:"expected" ~idx:expected ~what:"removed"
        info.rules
  | Reordered { info; _ } ->
      container_rules_claims ~side:"expected" ~idx:expected ~what:"reordered"
        info.rules
  | Block_structure_changed { expected_blocks; actual_blocks; _ } ->
      List.concat_map
        (fun (_, stmts) ->
          container_rules_claims ~side:"expected" ~idx:expected
            ~what:"restructured" stmts)
        expected_blocks
      @ List.concat_map
          (fun (_, stmts) ->
            container_rules_claims ~side:"actual" ~idx:actual
              ~what:"restructured" stmts)
          actual_blocks
  | Modified { rule_changes; container_changes; _ } ->
      List.concat_map (rule_claims ~expected ~actual) rule_changes
      @ List.concat_map (container_claims ~expected ~actual) container_changes

let claims ~expected ~actual (d : D.t) =
  List.concat_map (rule_claims ~expected ~actual) d.rules
  @ List.concat_map (container_claims ~expected ~actual) d.containers

(* ===== Self-consistency of the printed report ===== *)

let render_report ?entries result =
  let buf = Buffer.create 1024 in
  C.pp_diff ~expected:"expected" ~actual:"actual" ~color:false ?entries buf
    result;
  Buffer.contents buf

let render_summary ~expected_str ~actual_str result =
  let buf = Buffer.create 256 in
  C.pp_stats buf (C.stats ~expected_str ~actual_str result);
  Buffer.contents buf

let lines s = String.split_on_char '\n' s

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix

(* A top-level entry opens with the tree connector at column 0; a nested one is
   indented behind its parent's continuation. *)
let top_level_lines body =
  List.filter
    (fun l ->
      starts_with "\xe2\x94\x9c\xe2\x94\x80 " l
      || starts_with "\xe2\x94\x94\xe2\x94\x80 " l)
    (lines body)

(* [...N more differences] closes a shortened report. *)
let elided body =
  List.fold_left
    (fun acc l ->
      if contains l "more difference" then
        match String.index_opt l '.' with
        | None -> acc
        | Some i ->
            let rest = String.sub l i (String.length l - i) in
            let digits =
              String.concat ""
                (List.filter_map
                   (fun c ->
                     if c >= '0' && c <= '9' then Some (String.make 1 c)
                     else None)
                   (List.init (String.length rest) (String.get rest)))
            in
            Option.value ~default:acc (int_of_string_opt digits)
      else acc)
    0 (top_level_lines body)

let stats_total (s : C.stats) =
  s.added_rules + s.removed_rules + s.modified_rules + s.reordered_rules
  + s.rearranged_rules + s.regrouped_rules + s.container_changes
