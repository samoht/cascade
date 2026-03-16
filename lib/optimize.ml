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

let is_intentionally_duplicated prop_name =
  prop_name = "content" || prop_name = "outline"
  || (String.length prop_name > 12 && String.sub prop_name 0 12 = "-webkit-mask")

let track_last_occurrence normal_seen important_seen decl =
  let prop_name = property_name decl in
  (* Skip tracking for properties that use intentional duplication: - content:
     counter fallbacks - outline: webkit fallbacks - -webkit-mask-*: vendor
     prefix fallback patterns *)
  if is_intentionally_duplicated prop_name then ()
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
      (* Always keep intentionally duplicated properties (no deduplication) *)
      if is_intentionally_duplicated prop_name then deduped := decl :: !deduped
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
  | Details_content -> Some Details_content
  | Compound sels ->
      (* For compound selectors, look for pseudo-element at the end *)
      List.fold_left
        (fun acc sel ->
          match extract_pseudo_element sel with
          | Some _ as pe -> pe
          | None -> acc)
        None sels
  | Combined (_, _, right) | Relative (_, right) ->
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
  | Webkit_outer_spin_button | Webkit_details_marker ->
      true
  | Compound sels -> List.exists contains_vendor_pseudo_element sels
  | Combined (left, _, right) ->
      contains_vendor_pseudo_element left
      || contains_vendor_pseudo_element right
  | Relative (_, right) -> contains_vendor_pseudo_element right
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
                  merge_key = prev.merge_key;
                }
              in
              merge_adjacent acc (Some merged) rest
            else
              (* Different selector or vendor-specific - emit previous rule and
                 continue *)
              merge_adjacent (prev :: acc) (Some rule) rest)
  in
  merge_adjacent [] None rules

(** Check if a selector uses a descendant combinator with a pseudo-element (e.g.
    [.marker:flex ::marker]). These must not be combined with direct
    pseudo-element selectors (e.g. [.marker:flex::marker]) because they target
    different elements. *)
let rec has_descendant_pseudo_element : Selector.t -> bool = function
  | Combined (_, Descendant, right) -> extract_pseudo_element right <> None
  | Compound sels -> List.exists has_descendant_pseudo_element sels
  | List sels -> List.exists has_descendant_pseudo_element sels
  | _ -> false

(* Check if a selector has a descendant combinator that would make combining
   unsafe. Descendant combinators where the ancestor is :where() are safe
   because :where() has zero specificity and is always valid in selector lists
   (e.g., :where(.group) .in-[.group]:flex). *)
let rec has_descendant_combinator : Selector.t -> bool = function
  | Combined (Where _, Descendant, _) -> false
  | Combined (_, Descendant, _) -> true
  | Compound sels -> List.exists has_descendant_combinator sels
  | List sels -> List.exists has_descendant_combinator sels
  | _ -> false

let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  (* Check if selector contains vendor-specific pseudo-elements These should not
     be grouped because: - If one selector in a group is invalid in a browser,
     the entire rule fails - Keeping them separate ensures maximum browser
     compatibility *)
  || contains_vendor_pseudo_element selector
  (* Descendant pseudo-element selectors (.x ::marker) must not be combined with
     direct ones (.x::marker) — they target different elements *)
  || has_descendant_pseudo_element selector
  ||
  (* Descendant combinator selectors (e.g., .prose :where(ol[type=i
     s]):not(...)) should not be combined — Tailwind keeps them separate even
     with identical declarations, to match tailwindcss output exactly. *)
  has_descendant_combinator selector

(* Count the modifier depth (number of ':' separators) in a class name. E.g.,
   "group-focus:flex" has depth 1, "group-focus:group-hover:flex" has depth 2.
   Only selectors with the same modifier depth should be combined. *)

(** Check if two selectors can be combined. This is only called when the
    declarations are already verified to be identical. Following Tailwind v4's
    behavior:
    - Don't combine if the selectors have different pseudo-elements (::before vs
      ::after) since they target different generated content
    - Don't combine if one has modifiers and one doesn't (e.g., .bg-blue-500 and
      .aria-selected:bg-blue-500) to preserve ordering semantics
    - Otherwise, can combine since both rules set the same values *)
let modifier_depth class_name =
  let len = String.length class_name in
  let rec loop i depth bracket_depth =
    if i >= len then depth
    else
      match class_name.[i] with
      | '[' -> loop (i + 1) depth (bracket_depth + 1)
      | ']' -> loop (i + 1) depth (max 0 (bracket_depth - 1))
      | '\\' when i + 1 < len && class_name.[i + 1] = ':' ->
          (* Skip escaped colon \: — not a modifier separator *)
          loop (i + 2) depth bracket_depth
      | ':' when bracket_depth = 0 -> loop (i + 1) (depth + 1) bracket_depth
      | _ -> loop (i + 1) depth bracket_depth
  in
  loop 0 0 0

let has_escaped_colon class_name =
  let len = String.length class_name in
  let rec check i =
    if i >= len - 1 then false
    else if class_name.[i] = '\\' && class_name.[i + 1] = ':' then true
    else check (i + 1)
  in
  check 0

let can_combine_selectors sel1 sel2 =
  (* Check if pseudo-elements match (both None, or both the same) *)
  let pe1 = extract_pseudo_element sel1 in
  let pe2 = extract_pseudo_element sel2 in
  if pe1 <> pe2 then false
  else
    (* Tailwind v4 combines consecutive rules with identical declarations. For
       variant-prefixed classes (e.g., group-X:flex, peer-X:flex), we require
       the same modifier depth to avoid combining different nesting levels.
       Simple class selectors always combine. *)
    match (Selector.first_class sel1, Selector.first_class sel2) with
    | Some c1, Some c2 ->
        (* Don't combine if one has escaped colon (variant-prefixed like
           peer-checked\:font-semibold) and the other doesn't *)
        if has_escaped_colon c1 <> has_escaped_colon c2 then false
        else
          let d1 = modifier_depth c1 in
          let d2 = modifier_depth c2 in
          (d1 > 0 && d2 > 0) || d1 = d2
    | _ -> false

(* Check if a selector contains a :not() pseudo-class at the top level *)
let rec has_not_pseudo = function
  | Selector.Not _ -> true
  | Selector.Compound sels -> List.exists has_not_pseudo sels
  | _ -> false

(* Check if a selector contains a :has() pseudo-class at the top level *)
let rec has_has_pseudo = function
  | Selector.Has _ -> true
  | Selector.Compound sels -> List.exists has_has_pseudo sels
  | _ -> false

(* Sort selectors for merging: not-* first (sub-sorted by group/peer/plain),
   group-* second, peer-* third, ancestor-context fourth, has-* last. Uses
   structured selector analysis. *)
let base_sort_key sel =
  if has_not_pseudo sel then
    (* Sub-classify not-* selectors by group/peer/plain *)
    if Selector.has_group_marker sel then -3
    else if Selector.has_peer_marker sel then -2
    else -1
  else if has_has_pseudo sel then
    (* Only give :has() a distinct sort key when combined with group/peer
       markers, so group-has-* sorts after group-* and peer-has-* after peer-*.
       Plain has-* selectors sort among regular selectors. *)
    if Selector.has_group_marker sel || Selector.has_peer_marker sel then 3
    else 2
  else if Selector.has_group_marker sel then 0
  else if Selector.has_peer_marker sel then 1
  else 2

let is_ancestor_context = function
  | Selector.Combined (Selector.Where _, Selector.Descendant, _) -> true
  | _ -> false

let selector_sort_key sel =
  let base = if is_ancestor_context sel then 2 else base_sort_key sel in
  let depth =
    match Selector.first_class sel with
    | Some cls -> modifier_depth cls
    | None -> 0
  in
  (* Sort nth variants by selector AST type: nth-child < nth-last-child <
     nth-of-type < nth-last-of-type, matching Tailwind v4 ordering where "last"
     follows its non-last counterpart *)
  let nth_order =
    let rec find_nth = function
      | Selector.Nth_last_of_type _ -> 3
      | Selector.Nth_of_type _ -> 2
      | Selector.Nth_last_child _ -> 1
      | Selector.Compound sels ->
          List.fold_left (fun acc s -> max acc (find_nth s)) 0 sels
      | _ -> 0
    in
    find_nth sel
  in
  (base, nth_order, depth)

let class_has_bracket cls =
  (* Check if the class name contains a bracket [, indicating an arbitrary
     variant like group-has-[:checked] vs a named variant like
     group-has-checked. Named variants sort before bracket variants. Note: class
     names are stored unescaped in the AST. *)
  String.contains cls '['

let class_base_and_slash cls =
  (* Extract the base variant name and whether there's a /name suffix. E.g.,
     "group-has-checked/parent-name:flex" -> ("group-has-checked", true)
     "group-has-checked:flex" -> ("group-has-checked", false)
     "group-has-[:checked]:flex" -> ("group-has-[:checked]", false) This allows
     grouping variants by their base name, with plain before /name variants
     within each group. *)
  let len = String.length cls in
  (* Find / or trailing :utility, whichever comes first (outside brackets) *)
  let rec find_end i depth =
    if i >= len then (cls, false)
    else
      match cls.[i] with
      | '[' -> find_end (i + 1) (depth + 1)
      | ']' -> find_end (i + 1) (max 0 (depth - 1))
      | '/' when depth = 0 -> (String.sub cls 0 i, true)
      | ':' when depth = 0 -> (String.sub cls 0 i, false)
      | _ -> find_end (i + 1) depth
  in
  find_end 0 0

let compare_selectors_for_merge (sel1, i1) (sel2, i2) =
  let k1 = selector_sort_key sel1 and k2 = selector_sort_key sel2 in
  let c = compare k1 k2 in
  if c <> 0 then c
  else
    let cls1 = Selector.first_class sel1 in
    let cls2 = Selector.first_class sel2 in
    let c1 = match cls1 with Some c -> c | None -> "" in
    let c2 = match cls2 with Some c -> c | None -> "" in
    let b1 = class_has_bracket c1 in
    let b2 = class_has_bracket c2 in
    (* Extract variant family prefix (e.g., "group-has-" from
       "group-has-[:checked]:flex" or "group-has-checked:flex"). Used to group
       named and bracket variants of the same family. *)
    let variant_family cls =
      let bracket_pos = String.index_opt cls '[' in
      let colon_pos =
        (* Find first : outside brackets *)
        let len = String.length cls in
        let rec find i depth =
          if i >= len then None
          else
            match cls.[i] with
            | '[' -> find (i + 1) (depth + 1)
            | ']' -> find (i + 1) (max 0 (depth - 1))
            | ':' when depth = 0 -> Some i
            | _ -> find (i + 1) depth
        in
        find 0 0
      in
      match (bracket_pos, colon_pos) with
      | Some bi, _ -> String.sub cls 0 bi (* prefix before bracket *)
      | _, Some ci -> String.sub cls 0 ci (* prefix before colon *)
      | _ -> cls
    in
    let fam1 = variant_family c1 in
    let fam2 = variant_family c2 in
    if fam1 = fam2 then
      (* Same family: named before bracket, then sort within *)
      if b1 <> b2 then Bool.compare b1 b2
      else if b1 then
        (* Both bracket, same family *)
        let bracket_content_type cls =
          match String.index_opt cls '[' with
          | Some i when i + 1 < String.length cls && cls.[i + 1] = ':' -> 0
          | _ -> 1
        in
        let bt_cmp =
          Int.compare (bracket_content_type c1) (bracket_content_type c2)
        in
        if bt_cmp <> 0 then bt_cmp
        else
          let base1, slash1 = class_base_and_slash c1 in
          let base2, slash2 = class_base_and_slash c2 in
          let starts_with s p =
            String.length s >= String.length p
            && String.sub s 0 (String.length p) = p
          in
          let is_attr_variant b =
            starts_with b "aria-[" || starts_with b "data-["
            || starts_with b "group-aria-["
            || starts_with b "group-data-["
            || starts_with b "peer-aria-["
            || starts_with b "peer-data-["
          in
          let norm b =
            if is_attr_variant b then
              String.map (fun c -> if c = '_' then ' ' else c) b
            else b
          in
          let base_cmp = String.compare (norm base1) (norm base2) in
          if base_cmp <> 0 then base_cmp
          else
            let slash_cmp = Bool.compare slash1 slash2 in
            if slash_cmp <> 0 then slash_cmp else Int.compare i1 i2
      else
        (* Both non-bracket, same family: preserve original order *)
        Int.compare i1 i2
    else if b1 && b2 then
      (* Different families, both bracket: compare by class name so that e.g.
         hover:peer-[&_p] sorts before peer-[&_p]:hover *)
      let cls_cmp = String.compare c1 c2 in
      if cls_cmp <> 0 then cls_cmp else Int.compare i1 i2
    else
      (* Different families, at least one non-bracket: preserve original
         insertion order to respect variant cascade ordering *)
      Int.compare i1 i2

(* Convert group of selectors to a rule *)
let group_to_rule :
    (Selector.t * declaration list * string option) list ->
    Stylesheet.rule option = function
  | [ (sel, decls, _) ] ->
      Some
        { selector = sel; declarations = decls; nested = []; merge_key = None }
  | [] -> None
  | group ->
      let selector_list = List.rev group |> List.map (fun (s, _, _) -> s) in
      (* Always sort: group-* first, peer-* second, base last with stable index
         tiebreaker. This matches Tailwind's selector list ordering. *)
      let sorted_selectors =
        let indexed = List.mapi (fun i s -> (s, i)) selector_list in
        List.sort compare_selectors_for_merge indexed |> List.map fst
      in
      let _, decls, _ = List.hd group in
      (* Create a List selector from all the selectors *)
      let combined_selector =
        if List.length sorted_selectors = 1 then List.hd sorted_selectors
        else Selector.list sorted_selectors
      in
      Some
        {
          selector = combined_selector;
          declarations = decls;
          nested = [];
          merge_key = None;
        }

(* Flush current group to accumulator *)
let flush_group acc group =
  match group_to_rule group with Some rule -> rule :: acc | None -> acc

(* Don't combine selectors when one uses :is(:where()) (group/peer) and the
   other uses a newer pseudo-class directly. In a selector list, if the newer
   pseudo-class is unsupported, the entire rule is dropped — but the
   :is(:where()) variant would have survived on its own due to forgiving
   selector parsing. *)
let newer_pseudo_class_compatible sel1 sel2 =
  let sel1_complex = Selector.has_is_where_pattern sel1 in
  let sel2_complex = Selector.has_is_where_pattern sel2 in
  if sel1_complex <> sel2_complex then
    let plain_sel = if sel1_complex then sel2 else sel1 in
    not (Selector.has_newer_pseudo_class plain_sel)
  else true

(* Lightning CSS does not merge rules when values contain the 'none' keyword in
   color functions like oklab(). Check if a CSS string contains this pattern. *)
let has_oklab_none s =
  (* Search for "oklab(" then check if "none" appears before the closing ")" *)
  let len = String.length s in
  let rec find_oklab i =
    if i > len - 6 then false
    else if
      s.[i] = 'o'
      && s.[i + 1] = 'k'
      && s.[i + 2] = 'l'
      && s.[i + 3] = 'a'
      && s.[i + 4] = 'b'
      && s.[i + 5] = '('
    then check_none (i + 6)
    else find_oklab (i + 1)
  and check_none i =
    if i > len - 4 then false
    else if s.[i] = ')' then find_oklab (i + 1)
    else if
      s.[i] = 'n'
      && s.[i + 1] = 'o'
      && s.[i + 2] = 'n'
      && s.[i + 3] = 'e'
      && (i + 4 >= len || s.[i + 4] = ' ' || s.[i + 4] = ')' || s.[i + 4] = '/')
    then true
    else check_none (i + 1)
  in
  find_oklab 0

let declarations_css_equal d1 d2 =
  (d1 = d2
  ||
  let pp_decls ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  let s1 = Pp.to_string ~minify:true pp_decls d1 in
  let s2 = Pp.to_string ~minify:true pp_decls d2 in
  s1 = s2)
  &&
  let pp_decls ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  let s = Pp.to_string ~minify:true pp_decls d1 in
  not (has_oklab_none s)

let can_combine_rules (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  declarations_css_equal prev.declarations rule.declarations
  && newer_pseudo_class_compatible prev.selector rule.selector
  &&
  match (prev.merge_key, rule.merge_key) with
  | Some k1, Some k2 when k1 = k2 ->
      (* When both have the same merge_key, allow combining unless they have
         incompatible pseudo-elements. Pseudo-elements in the same "tier" can
         combine (e.g. ::placeholder + ::backdrop), but pseudo-elements from
         different tiers cannot (e.g. ::backdrop + ::details-content). *)
      let pe1 = extract_pseudo_element prev.selector in
      let pe2 = extract_pseudo_element rule.selector in
      let pseudo_tier = function
        | None -> 0
        | Some Selector.Before | Some After -> 1
        | Some First_letter | Some First_line -> 2
        | Some Placeholder | Some Backdrop -> 3
        | Some Details_content -> 4
        | Some Marker -> 5
        | Some Selection -> 6
        | Some File_selector_button -> 7
        | Some _ -> 8
      in
      pseudo_tier pe1 = pseudo_tier pe2
  | _ ->
      (* Different or missing merge_keys: fall through to selector compatibility
         check. Declarations are already verified identical at call site, so two
         utilities with the same CSS content (e.g. shadow and shadow-sm in v4)
         can combine when their selectors are compatible. *)
      can_combine_selectors prev.selector rule.selector

let combine_identical_rules (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  (* Only combine consecutive rules to preserve cascade semantics *)
  let rec combine_consecutive acc current_group = function
    | [] -> List.rev (flush_group acc current_group)
    | (rule : Stylesheet.rule) :: rest -> (
        if
          (* Don't combine rules with nested statements or rules whose selectors
             are structurally incompatible with combining (e.g., vendor
             pseudo-elements, descendant pseudo-elements). Always check
             should_not_combine regardless of merge_key — merge_key controls
             whether identical-declaration rules CAN combine, but structural
             selector constraints still apply. *)
          rule.nested <> [] || should_not_combine rule.selector
        then
          (* Don't combine this selector, flush current group and start fresh *)
          let acc' = rule :: flush_group acc current_group in
          combine_consecutive acc' [] rest
        else
          match current_group with
          | [] ->
              (* Start a new group *)
              combine_consecutive acc
                [ (rule.selector, rule.declarations, rule.merge_key) ]
                rest
          | (prev_sel, prev_decls, prev_merge_key) :: _ ->
              let prev_rule =
                {
                  Stylesheet_intf.selector = prev_sel;
                  declarations = prev_decls;
                  nested = [];
                  merge_key = prev_merge_key;
                }
              in
              if can_combine_rules prev_rule rule then
                (* Same declarations and compatible selectors, add to current
                   group *)
                combine_consecutive acc
                  ((rule.selector, rule.declarations, rule.merge_key)
                  :: current_group)
                  rest
              else
                (* Different declarations or incompatible selectors, flush
                   current group and start new one *)
                let acc' = flush_group acc current_group in
                combine_consecutive acc'
                  [ (rule.selector, rule.declarations, rule.merge_key) ]
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

(* Shared predicates for media block optimization *)
let rec should_consolidate cond =
  match cond with
  | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
  | Media.Min_width_length _ | Media.Not_min_width_length _
  | Media.Prefers_reduced_motion _ | Media.Prefers_color_scheme _ ->
      true
  | Media.Negated inner -> should_consolidate inner
  | _ -> false

let is_responsive_media = function
  | Media (cond, _) -> (
      match cond with
      | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
      | Media.Min_width_length _ | Media.Not_min_width_length _ ->
          true
      | Media.Negated inner -> (
          match inner with
          | Media.Min_width _ | Media.Min_width_rem _ | Media.Max_width _
          | Media.Min_width_length _ | Media.Not_min_width_length _ ->
              true
          | _ -> false)
      | _ -> false)
  | _ -> false

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

let is_container_block block =
  List.exists
    (function
      | Rule { selector; _ } -> Selector.to_string selector = ".container"
      | _ -> false)
    block

let collect_media_data stmts =
  let media_map = Hashtbl.create 16 in
  let last_pos = Hashtbl.create 16 in
  let first_responsive_pos = ref None in
  let has_responsive = ref false in
  let has_preference_media = ref false in
  List.iteri
    (fun i stmt ->
      match stmt with
      | Media (cond, block)
        when should_consolidate cond
             && (not (has_nested_preference_media block))
             && not (is_container_block block) ->
          let key = Media.to_string cond in
          Hashtbl.replace last_pos key i;
          let existing =
            try Hashtbl.find media_map key with Not_found -> (cond, [])
          in
          let _, blocks = existing in
          Hashtbl.replace media_map key (cond, blocks @ [ block ]);
          if is_responsive_media stmt then (
            has_responsive := true;
            if !first_responsive_pos = None then first_responsive_pos := Some i)
      | Media (cond, _) -> (
          match cond with
          | Media.Prefers_color_scheme _ | Media.Prefers_reduced_motion _
          | Media.Prefers_contrast _ ->
              has_preference_media := true
          | _ -> ())
      | _ when is_responsive_media stmt ->
          has_responsive := true;
          if !first_responsive_pos = None then first_responsive_pos := Some i
      | _ -> ())
    stmts;
  ( media_map,
    last_pos,
    !first_responsive_pos,
    !has_responsive,
    !has_preference_media )

let compute_hover_insert_pos stmts ~first_responsive_pos ~has_responsive
    ~has_preference_media =
  let regular_stmt_count =
    List.fold_left
      (fun acc stmt -> match stmt with Rule _ -> acc + 1 | _ -> acc)
      0 stmts
  in
  let is_top_level =
    has_responsive || has_preference_media || regular_stmt_count > 10
  in
  let hover_insert_pos =
    match (first_responsive_pos, is_top_level) with
    | Some pos, _ -> pos
    | None, true -> List.length stmts
    | None, false -> -1
  in
  (hover_insert_pos, is_top_level)

(* Group all media blocks with the same condition together, for specific media
   types (Hover, Min_width, Max_width, Prefers_reduced_motion). This allows
   @media (hover:hover) blocks to be consolidated into a single block matching
   Tailwind's behavior.

   For @media (hover:hover), the consolidated block is placed after all Regular
   utilities but before responsive media queries (@media (min-width:...)). For
   responsive media, the consolidated block is placed at the last occurrence. *)
(* Flush pending hover/motion blocks at the insertion point. Returns the
   updated accumulator with pending blocks prepended (in reverse order). *)
let emit_pending_hover ~hover_insert_pos ~pending_hover_blocks
    ~pending_motion_blocks i acc =
  if i = hover_insert_pos && List.length !pending_hover_blocks > 0 then (
    let all_pending = !pending_hover_blocks @ !pending_motion_blocks in
    let hover_acc = List.rev_append all_pending acc in
    pending_hover_blocks := [];
    pending_motion_blocks := [];
    hover_acc)
  else acc

(* Route a consolidated media block: either defer it to a pending list for later
   repositioning, or emit it directly at the current position. *)
let route_consolidated ~should_reposition_hover ~is_top_level
    ~pending_hover_blocks ~pending_motion_blocks:_ consolidated cond acc =
  match cond with
  | Media.Hover when should_reposition_hover ->
      (* For hover at top-level, add to pending list for repositioning. Append
         to maintain order (first occurrence stays first). *)
      pending_hover_blocks := !pending_hover_blocks @ [ consolidated ];
      acc
  | Media.Prefers_reduced_motion _ when is_top_level ->
      (* Motion blocks are positioned correctly by variant_order sorting. Emit
         directly at their sorted position. *)
      consolidated :: acc
  | _ ->
      (* For responsive media, or hover in nested context, emit at last
         position *)
      consolidated :: acc

let try_consolidate_media ~optimize_merged_block ~media_map ~last_pos
    ~emitted_media ~should_reposition_hover ~is_top_level ~pending_hover_blocks
    ~pending_motion_blocks i stmt acc =
  match stmt with
  | Media (cond, block)
    when should_consolidate cond
         && (not (has_nested_preference_media block))
         && not (is_container_block block) ->
      let key = Media.to_string cond in
      if Hashtbl.mem last_pos key then
        let is_last_pos = Hashtbl.find last_pos key = i in
        if is_last_pos && not (Hashtbl.mem emitted_media key) then (
          Hashtbl.add emitted_media key true;
          let _, all_blocks = Hashtbl.find media_map key in
          let merged = List.concat all_blocks in
          let consolidated = Media (cond, optimize_merged_block merged) in
          route_consolidated ~should_reposition_hover ~is_top_level
            ~pending_hover_blocks ~pending_motion_blocks consolidated cond acc)
        else acc
      else stmt :: acc
  | _ -> stmt :: acc

let consolidate_media_blocks (stmts : statement list) : statement list =
  let optimize_merged_block block = !statements_ref block in
  let ( media_map,
        last_pos,
        first_responsive_pos,
        has_responsive,
        has_preference_media ) =
    collect_media_data stmts
  in
  let hover_insert_pos, is_top_level =
    compute_hover_insert_pos stmts ~first_responsive_pos ~has_responsive
      ~has_preference_media
  in
  let emitted_media = Hashtbl.create 16 in
  let pending_hover_blocks = ref [] in
  let pending_motion_blocks = ref [] in
  let should_reposition_hover = hover_insert_pos >= 0 in

  let rec filter_with_index i acc = function
    | [] -> List.rev_append acc (!pending_hover_blocks @ !pending_motion_blocks)
    | stmt :: rest ->
        let acc_with_hover =
          emit_pending_hover ~hover_insert_pos ~pending_hover_blocks
            ~pending_motion_blocks i acc
        in
        let new_acc =
          try_consolidate_media ~optimize_merged_block ~media_map ~last_pos
            ~emitted_media ~should_reposition_hover ~is_top_level
            ~pending_hover_blocks ~pending_motion_blocks i stmt acc_with_hover
        in
        filter_with_index (i + 1) new_acc rest
  in
  filter_with_index 0 [] stmts

let merge_consecutive_media (stmts : statement list) : statement list =
  let optimize_merged_block block =
    (* When we merge media blocks, the resulting block may have consecutive
       rules with identical declarations that should be combined. We need to
       re-run the full optimization pipeline on the merged content. *)
    !statements_ref block
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
  |> consolidate_media_blocks |> merge_consecutive_media
  |> merge_layer_declarations

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
      let optimized = rules_aux plain_rules in
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

and rules_aux (rules : rule list) : rule list =
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

let rules (rules : rule list) : rule list = rules_aux rules

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
