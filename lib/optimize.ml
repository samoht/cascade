(** CSS optimization implementation *)

open Declaration
open Stylesheet

(** {1 Declaration Optimization} *)

let duplicate_buggy_properties decls =
  (* Check if webkit-text-decoration:inherit is already duplicated *)
  let webkit_text_decoration_inherit_count =
    List.fold_left
      (fun count decl ->
        match decl with
        | Declaration { property = Webkit_text_decoration; value = Inherit; _ }
          ->
            count + 1
        | _ -> count)
      0 decls
  in

  (* Check if webkit-text-decoration-color is already duplicated *)
  let webkit_text_decoration_color_count =
    List.fold_left
      (fun count decl ->
        match decl with
        | Declaration { property = Webkit_text_decoration_color; _ } ->
            count + 1
        | _ -> count)
      0 decls
  in

  List.concat_map
    (fun decl ->
      match decl with
      | Declaration { property = Webkit_text_decoration; value = Inherit; _ } ->
          if webkit_text_decoration_inherit_count >= 3 then [ decl ]
            (* Already tripled *)
          else [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration { property = Webkit_text_decoration_color; _ } ->
          if webkit_text_decoration_color_count >= 2 then [ decl ]
            (* Already duplicated *)
          else [ decl; decl ]
            (* Always duplicate webkit-text-decoration-color *)
      | Declaration { property = Transform; _ } ->
          (* Do not duplicate transform to -webkit-transform. Tailwind v4 does
             not emit vendor-prefixed transform here, and tests expect a single
             canonical property. *)
          [ decl ]
      | _ -> [ decl ])
    decls

let track_last_occurrence normal_seen important_seen decl =
  let prop_name = property_name decl in
  (* Skip tracking for content and outline - these properties use duplicate
     declarations intentionally (content counters, outline webkit fallbacks) *)
  if prop_name = "content" || prop_name = "outline" then ()
  else if is_important decl then Hashtbl.replace important_seen prop_name decl
  else Hashtbl.replace normal_seen prop_name decl

let choose_final_decl important_seen normal_seen prop_name decl =
  if Hashtbl.mem important_seen prop_name then
    Hashtbl.find important_seen prop_name
  else if Hashtbl.mem normal_seen prop_name then
    Hashtbl.find normal_seen prop_name
  else decl

let deduplicate_declarations props =
  (* CSS cascade rules: 1. !important declarations always win over normal
     declarations 2. Among declarations of same importance, last one wins We
     need to track normal and important separately

     Special case: Don't deduplicate content properties as they may be
     intentionally duplicated for fallback patterns *)
  let normal_seen = Hashtbl.create 16 in
  let important_seen = Hashtbl.create 16 in

  (* First pass: collect last occurrence of each property by importance *)
  List.iter (track_last_occurrence normal_seen important_seen) props;

  (* Second pass: build result, important wins over normal for same property *)
  let deduped = ref [] in
  let processed = Hashtbl.create 16 in

  (* Process in reverse order to maintain first occurrence position *)
  List.iter
    (fun decl ->
      let prop_name = property_name decl in
      (* Always keep content and outline properties (no deduplication) *)
      if prop_name = "content" || prop_name = "outline" then
        deduped := decl :: !deduped
      else if not (Hashtbl.mem processed prop_name) then (
        Hashtbl.add processed prop_name ();
        let final_decl =
          choose_final_decl important_seen normal_seen prop_name decl
        in
        deduped := final_decl :: !deduped))
    props;

  (* Apply buggy property duplication after deduplication *)
  duplicate_buggy_properties (List.rev !deduped)

(** {1 Rule Optimization} *)

(* Extract the pseudo-element from a selector (::before, ::after, etc.). Returns
   None if no pseudo-element is present. Used to prevent combining selectors
   with different pseudo-elements which would change semantics. *)
let rec extract_pseudo_element : Selector.t -> Selector.t option = function
  | Before -> Some Before
  | After -> Some After
  | First_letter -> Some First_letter
  | First_line -> Some First_line
  | Marker -> Some Marker
  | Placeholder -> Some Placeholder
  | Selection -> Some Selection
  | File_selector_button -> Some File_selector_button
  | Backdrop -> Some Backdrop
  | Compound sels ->
      (* For compound selectors, look for pseudo-element at the end *)
      List.fold_left
        (fun acc sel ->
          match extract_pseudo_element sel with
          | Some _ as pe -> pe
          | None -> acc)
        None sels
  | Combined (_, _, right) ->
      (* Pseudo-element is always at the end of a combined selector *)
      extract_pseudo_element right
  | _ -> None

(* Check if a selector contains vendor-specific pseudo-elements. These should
   not be grouped because if one selector in a group is invalid in a browser,
   the entire rule fails. Keeping them separate ensures maximum
   compatibility. *)
let rec contains_vendor_pseudo_element : Selector.t -> bool = function
  | File_selector_button -> true
  | Webkit_scrollbar | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button ->
      true
  | Compound sels -> List.exists contains_vendor_pseudo_element sels
  | Combined (left, _, right) ->
      contains_vendor_pseudo_element left
      || contains_vendor_pseudo_element right
  | List sels -> List.exists contains_vendor_pseudo_element sels
  | Not sels -> List.exists contains_vendor_pseudo_element sels
  | Is sels -> List.exists contains_vendor_pseudo_element sels
  | Where sels -> List.exists contains_vendor_pseudo_element sels
  | Has sels -> List.exists contains_vendor_pseudo_element sels
  | _ -> false

let single_rule_without_nested (rule : rule) : rule =
  { rule with declarations = deduplicate_declarations rule.declarations }

let merge_rules (rules : Stylesheet.rule list) : Stylesheet.rule list =
  (* Only merge truly adjacent rules with the same selector to preserve cascade
     order. This is safe because we don't reorder rules - we only combine
     immediately adjacent rules with identical selectors, which maintains
     cascade semantics.

     However, we don't merge vendor-specific pseudo-elements to match Tailwind's
     behavior and ensure browser compatibility. *)
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
            if
              prev.selector = rule.selector
              && not (contains_vendor_pseudo_element rule.selector)
            then
              (* Same selector immediately following and not vendor-specific -
                 safe to merge *)
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
              (* Different selector or vendor-specific - emit previous rule and
                 continue *)
              merge_adjacent (prev :: acc) (Some rule) rest)
  in
  merge_adjacent [] None rules

(* Check if a selector should not be combined with others *)
let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  ||
  (* Check if selector contains vendor-specific pseudo-elements These should not
     be grouped because: - If one selector in a group is invalid in a browser,
     the entire rule fails - Keeping them separate ensures maximum browser
     compatibility *)
  contains_vendor_pseudo_element selector

(** Check if two selectors can be combined. This is only called when the
    declarations are already verified to be identical. Following Tailwind v4's
    behavior:
    - Don't combine if the selectors have different pseudo-elements (::before vs
      ::after) since they target different generated content
    - Don't combine if one has modifiers and one doesn't (e.g., .bg-blue-500 and
      .aria-selected:bg-blue-500) to preserve ordering semantics
    - Otherwise, can combine since both rules set the same values *)
let can_combine_selectors sel1 sel2 =
  (* Check if pseudo-elements match (both None, or both the same) *)
  let pe1 = extract_pseudo_element sel1 in
  let pe2 = extract_pseudo_element sel2 in
  if pe1 <> pe2 then false
  else
    (* If both have pseudo-elements, check if they have the same base class.
       This prevents merging .form-input::placeholder and
       .form-textarea::placeholder which are different utilities that happen to
       share a pseudo-element. *)
    let base_class_matches =
      match (pe1, pe2) with
      | Some _, Some _ ->
          (* Both have pseudo-elements - check base classes *)
          Selector.first_class sel1 = Selector.first_class sel2
      | _ -> true (* No pseudo-elements, can combine if modifiers match *)
    in
    if not base_class_matches then false
    else
      (* Compare modifier prefixes. Selectors can only be combined if they have
         the same modifier prefix (or both have none). This ensures that: -
         .before:... and .after:... stay separate (different prefixes) -
         .group-focus:... and .group-has-[:checked]:... stay separate -
         .dark:group-has-[:checked]:... and .dark:peer-checked:... CAN combine
         (same outer prefix "dark:") *)
      Selector.modifier_prefix sel1 = Selector.modifier_prefix sel2

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
        if
          (* Don't combine rules with nested statements - they have different
             semantics *)
          should_not_combine rule.selector || rule.nested <> []
        then
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
          | (prev_sel, prev_decls) :: _ ->
              if
                prev_decls = rule.declarations
                && can_combine_selectors prev_sel rule.selector
              then
                (* Same declarations and compatible selectors, add to current
                   group *)
                combine_consecutive acc
                  ((rule.selector, rule.declarations) :: current_group)
                  rest
              else
                (* Different declarations or incompatible selectors, flush
                   current group and start new one *)
                let acc' = flush_group acc current_group in
                combine_consecutive acc'
                  [ (rule.selector, rule.declarations) ]
                  rest)
  in
  combine_consecutive [] [] rules

(** {1 Statement Optimization} *)

(* Merge consecutive media queries with the same condition. This only merges
   immediately adjacent media queries to preserve cascade order. When blocks are
   merged, we recursively call merge_consecutive_media on the combined content
   to merge any inner consecutive media queries. *)
(* Forward declaration to allow merge_consecutive_media to call statements *)
let statements_ref : (statement list -> statement list) ref =
  ref (fun stmts -> stmts)

let merge_consecutive_media (stmts : statement list) : statement list =
  let optimize_merged_block block =
    (* When we merge media blocks, the resulting block may have consecutive
       rules with identical declarations that should be combined. We need to
       re-run the full optimization pipeline on the merged content. *)
    !statements_ref block
  in
  (* Check if a block contains nested preference-based media queries. Preference
     media (contrast, reduced-motion, color-scheme) inside another preference
     media should not be merged to preserve Tailwind's structure for stacked
     modifiers like contrast-more:dark. *)
  let has_nested_preference_media block =
    List.exists
      (function
        | Media (cond, _) -> (
            match cond with
            | Prefers_contrast _ | Prefers_reduced_motion _
            | Prefers_color_scheme _ ->
                true
            | _ -> false)
        | _ -> false)
      block
  in
  let rec merge result prev_media = function
    | [] -> (
        match prev_media with
        | Some (cond, block) ->
            (* Emit pending media block with re-optimized content *)
            result @ [ Media (cond, optimize_merged_block block) ]
        | None -> result)
    | Media (cond, block) :: rest -> (
        match prev_media with
        | Some (prev_cond, prev_block)
          when Media.equal prev_cond cond
               (* Don't merge if either block contains nested preference media.
                  This keeps stacked preference modifiers separate while
                  allowing other nested media (like hover inside dark) to be
                  merged. *)
               && not
                    (has_nested_preference_media prev_block
                    || has_nested_preference_media block) ->
            (* Same condition and compatible structure - merge the blocks *)
            let merged_block = prev_block @ block in
            merge result (Some (cond, merged_block)) rest
        | Some (prev_cond, prev_block) ->
            (* Different condition - emit previous (with optimized content),
               store new one *)
            merge
              (result @ [ Media (prev_cond, optimize_merged_block prev_block) ])
              (Some (cond, block))
              rest
        | None ->
            (* First media query - store it *)
            merge result (Some (cond, block)) rest)
    | stmt :: rest -> (
        (* Non-media statement - flush any pending media query *)
        match prev_media with
        | Some (cond, block) ->
            (* Emit media query (with optimized content) then the statement *)
            merge
              (result @ [ Media (cond, optimize_merged_block block); stmt ])
              None rest
        | None -> merge (result @ [ stmt ]) None rest)
  in
  merge [] None stmts

(* Check if a layer block contains only empty rules or no statements *)
let is_layer_empty (block : statement list) : bool =
  List.for_all
    (function Rule { declarations = []; _ } -> true | _ -> false)
    block
  || block = []

(* Collect consecutive empty named layers and merge them into a Layer_decl *)
let rec collect_empty_layer_names names remaining =
  match remaining with
  | Layer (Some layer_name, layer_block) :: rest when is_layer_empty layer_block
    ->
      collect_empty_layer_names (layer_name :: names) rest
  | Layer_decl existing_names :: rest ->
      (* Merge with existing layer declaration *)
      (List.rev names @ existing_names, rest)
  | _ -> (List.rev names, remaining)

(* Merge consecutive Layer_decl statements *)
let merge_layer_declarations (stmts : statement list) : statement list =
  let rec merge acc = function
    | [] -> List.rev acc
    | Layer_decl names1 :: Layer_decl names2 :: rest ->
        (* Merge consecutive layer declarations *)
        merge acc (Layer_decl (names1 @ names2) :: rest)
    | stmt :: rest -> merge (stmt :: acc) rest
  in
  merge [] stmts

(* Main statement processing function with layer optimization *)
let rec statements (stmts : statement list) : statement list =
  process_statements [] stmts
  |> merge_consecutive_media |> merge_layer_declarations

and process_statements (acc : statement list) (remaining : statement list) :
    statement list =
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
      (* Optimize this batch of consecutive rules, including their nested
         statements *)
      let optimized = optimize_rules plain_rules in
      let as_statements = List.map (fun r -> Rule r) optimized in
      process_statements (List.rev_append as_statements acc) rest
  | Media (cond, block) :: rest ->
      (* Just optimize the block and pass through - grouping happens later *)
      let optimized_block = statements block in
      let optimized = Media (cond, optimized_block) in
      process_statements (optimized :: acc) rest
  | Container (name, cond, block) :: rest ->
      (* Recursively optimize container query content *)
      let optimized = Container (name, cond, statements block) in
      process_statements (optimized :: acc) rest
  | Supports (cond, block) :: rest ->
      (* Recursively optimize supports block content *)
      let optimized = Supports (cond, statements block) in
      process_statements (optimized :: acc) rest
  | Layer (name, block) :: rest ->
      let optimized_block = statements block in
      if is_layer_empty optimized_block then
        (* Handle empty layer optimization *)
        match name with
        | Some layer_name ->
            let all_names, remaining =
              collect_empty_layer_names [ layer_name ] rest
            in
            let layer_decl = Layer_decl all_names in
            process_statements (layer_decl :: acc) remaining
        | None ->
            (* Anonymous empty layer - just remove it *)
            process_statements acc rest
      else
        let optimized = Layer (name, optimized_block) in
        process_statements (optimized :: acc) rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements (hd :: acc) rest

and optimize_rules (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively *)
  let with_optimized_nested =
    List.map (fun rule -> { rule with nested = statements rule.nested }) rules
  in
  (* Then apply standard rule optimizations *)
  let deduped = List.map single_rule_without_nested with_optimized_nested in
  let merged = merge_rules deduped in
  (* Combine consecutive rules with identical declarations into selector
     lists *)
  combine_identical_rules merged

let single_rule (rule : rule) : rule =
  {
    rule with
    declarations = deduplicate_declarations rule.declarations;
    nested = statements rule.nested;
  }

let rules (rules : rule list) : rule list = optimize_rules rules

(* Initialize the forward reference for merge_consecutive_media *)
let () = statements_ref := statements

(** {1 Stylesheet Optimization} *)

let apply_property_duplication (stylesheet : t) : t =
  (* Apply only property duplication without other optimizations *)
  let rec apply_to_statements stmts =
    List.map
      (function
        | Rule rule ->
            Rule
              {
                rule with
                declarations = duplicate_buggy_properties rule.declarations;
              }
        | Media (cond, inner_stmts) ->
            Media (cond, apply_to_statements inner_stmts)
        | Layer (name, inner_stmts) ->
            Layer (name, apply_to_statements inner_stmts)
        | Container (name, cond, inner_stmts) ->
            Container (name, cond, apply_to_statements inner_stmts)
        | Supports (cond, inner_stmts) ->
            Supports (cond, apply_to_statements inner_stmts)
        | other -> other)
      stmts
  in
  apply_to_statements stylesheet

let stylesheet (stylesheet : t) : t =
  (* Apply CSS optimizations while preserving cascade semantics *)
  (* Also remove the initial layer declaration list (@layer theme,base,components,utilities;)
     as Tailwind v4 doesn't include it in minified+optimized output *)
  let remove_initial_layer_decl = function
    | Layer_decl names :: rest
      when List.mem "theme" names && List.mem "utilities" names ->
        (* This is the full layer declaration list - remove it *)
        rest
    | stmts -> stmts
  in
  stylesheet |> remove_initial_layer_decl |> statements
