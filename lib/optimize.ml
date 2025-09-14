(** CSS optimization implementation *)

open Declaration
open Stylesheet

(** {1 Declaration Optimization} *)

let duplicate_buggy_properties decls =
  List.concat_map
    (fun decl ->
      match decl with
      | Declaration { property = Webkit_text_decoration; value = Inherit; _ } ->
          [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration { property = Webkit_text_decoration_color; _ } ->
          [ decl; decl ] (* Always duplicate webkit-text-decoration-color *)
      | Declaration { property = Transform; value; important } ->
          (* Add -webkit-transform prefix for Transform properties *)
          (* Since -webkit-transform now maps to Transform property, create directly *)
          let webkit_decl = Declaration.v Transform value in
          let webkit_decl =
            if important then Declaration.important webkit_decl else webkit_decl
          in
          [ decl; webkit_decl ]
      | _ -> [ decl ])
    decls

let deduplicate_declarations props =
  (* CSS cascade rules: 1. !important declarations always win over normal
     declarations 2. Among declarations of same importance, last one wins We
     need to track normal and important separately *)
  let normal_seen = Hashtbl.create 16 in
  let important_seen = Hashtbl.create 16 in

  (* First pass: collect last occurrence of each property by importance *)
  List.iter
    (fun decl ->
      let prop_name = property_name decl in
      if is_important decl then Hashtbl.replace important_seen prop_name decl
      else Hashtbl.replace normal_seen prop_name decl)
    props;

  (* Second pass: build result, important wins over normal for same property *)
  let deduped = ref [] in
  let processed = Hashtbl.create 16 in

  (* Process in reverse order to maintain first occurrence position *)
  List.iter
    (fun decl ->
      let prop_name = property_name decl in

      if not (Hashtbl.mem processed prop_name) then (
        Hashtbl.add processed prop_name ();
        (* If there's an important version, use it; otherwise use normal *)
        let final_decl =
          if Hashtbl.mem important_seen prop_name then
            Hashtbl.find important_seen prop_name
          else if Hashtbl.mem normal_seen prop_name then
            Hashtbl.find normal_seen prop_name
          else decl (* Should not happen *)
        in
        deduped := final_decl :: !deduped))
    props;

  (* Apply buggy property duplication after deduplication *)
  duplicate_buggy_properties (List.rev !deduped)

(** {1 Rule Optimization} *)

let single_rule (rule : rule) : rule =
  { rule with declarations = deduplicate_declarations rule.declarations }

let merge_rules (rules : Stylesheet.rule list) : Stylesheet.rule list =
  (* Only merge truly adjacent rules with the same selector to preserve cascade
     order. This is safe because we don't reorder rules - we only combine
     immediately adjacent rules with identical selectors, which maintains
     cascade semantics. *)
  let rec merge_adjacent (acc : Stylesheet.rule list)
      (prev_rule : Stylesheet.rule option) :
      Stylesheet.rule list -> Stylesheet.rule list = function
    | [] -> List.rev (match prev_rule with Some r -> r :: acc | None -> acc)
    | (rule : Stylesheet.rule) :: rest -> (
        match prev_rule with
        | None ->
            (* First rule - just store it *)
            merge_adjacent acc (Some rule) rest
        | Some prev ->
            if prev.selector = rule.selector then
              (* Same selector immediately following - safe to merge *)
              let merged : Stylesheet.rule =
                {
                  selector = prev.selector;
                  declarations =
                    deduplicate_declarations
                      (prev.declarations @ rule.declarations);
                  nested = prev.nested @ rule.nested;
                }
              in
              merge_adjacent acc (Some merged) rest
            else
              (* Different selector - emit previous rule and continue with
                 current *)
              merge_adjacent (prev :: acc) (Some rule) rest)
  in
  merge_adjacent [] None rules

(* Check if a selector should not be combined with others *)
let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  ||
  (* Check string representation for specific prefixes *)
  let s = Pp.to_string Selector.pp selector in
  String.starts_with ~prefix:"::file-selector-button" s
  || String.starts_with ~prefix:"::-webkit-" s

(* Convert group of selectors to a rule *)
let group_to_rule :
    (Selector.t * declaration list) list -> Stylesheet.rule option = function
  | [ (sel, decls) ] ->
      Some { selector = sel; declarations = decls; nested = [] }
  | [] -> None
  | group ->
      let selector_list = List.map fst (List.rev group) in
      let decls = snd (List.hd group) in
      (* Create a List selector from all the selectors *)
      let combined_selector =
        if List.length selector_list = 1 then List.hd selector_list
        else Selector.list selector_list
      in
      Some { selector = combined_selector; declarations = decls; nested = [] }

(* Flush current group to accumulator *)
let flush_group acc group =
  match group_to_rule group with Some rule -> rule :: acc | None -> acc

let combine_identical_rules (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  (* Only combine consecutive rules to preserve cascade semantics *)
  let rec combine_consecutive acc current_group = function
    | [] -> List.rev (flush_group acc current_group)
    | (rule : Stylesheet.rule) :: rest -> (
        if should_not_combine rule.selector then
          (* Don't combine this selector, flush current group and start fresh *)
          let acc' = rule :: flush_group acc current_group in
          combine_consecutive acc' [] rest
        else
          match current_group with
          | [] ->
              (* Start a new group *)
              combine_consecutive acc
                [ (rule.selector, rule.declarations) ]
                rest
          | (_prev_sel, prev_decls) :: _ ->
              if prev_decls = rule.declarations then
                (* Same declarations, add to current group *)
                combine_consecutive acc
                  ((rule.selector, rule.declarations) :: current_group)
                  rest
              else
                (* Different declarations, flush current group and start new
                   one *)
                let acc' = flush_group acc current_group in
                combine_consecutive acc'
                  [ (rule.selector, rule.declarations) ]
                  rest)
  in
  combine_consecutive [] [] rules

let rules (rules : rule list) : rule list =
  let deduped = List.map single_rule rules in
  let merged = merge_rules deduped in
  combine_identical_rules merged

(** {1 Statement Optimization} *)

let rec statements (stmts : statement list) : statement list =
  (* Process rules in batches separated by non-Rule items *)
  let rec process_statements (acc : statement list) (remaining : statement list)
      : statement list =
    match remaining with
    | [] -> List.rev acc
    | Rule r :: rest ->
        (* Collect consecutive Rule items *)
        let rec collect_rules (rules_acc : rule list) :
            statement list -> rule list * statement list = function
          | Rule r :: rest -> collect_rules (r :: rules_acc) rest
          | rest -> (List.rev rules_acc, rest)
        in
        let plain_rules, rest = collect_rules [ r ] rest in
        (* Optimize this batch of consecutive rules *)
        let optimized = rules plain_rules in
        let as_statements = List.map (fun r -> Rule r) optimized in
        process_statements (List.rev_append as_statements acc) rest
    | Media (cond, block) :: rest ->
        let optimized = Media (cond, statements block) in
        process_statements (optimized :: acc) rest
    | Container (name, cond, block) :: rest ->
        let optimized = Container (name, cond, statements block) in
        process_statements (optimized :: acc) rest
    | Supports (cond, block) :: rest ->
        let optimized = Supports (cond, statements block) in
        process_statements (optimized :: acc) rest
    | Layer (name, block) :: rest ->
        let optimized = Layer (name, statements block) in
        process_statements (optimized :: acc) rest
    | hd :: rest ->
        (* Other statement types - keep as-is *)
        process_statements (hd :: acc) rest
  in
  process_statements [] stmts

(** {1 Stylesheet Optimization} *)

let stylesheet (stylesheet : t) : t =
  (* Apply CSS optimizations while preserving cascade semantics *)
  statements stylesheet
