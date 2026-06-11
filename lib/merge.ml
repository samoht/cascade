open Common

let preserve = List.preserve

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

let same_selector (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  key prev.Stylesheet_intf.selector = key rule.Stylesheet_intf.selector
  && not (vendor rule.Stylesheet_intf.selector)

let pair (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  {
    Stylesheet_intf.selector = prev.Stylesheet_intf.selector;
    declarations =
      prev.Stylesheet_intf.declarations @ rule.Stylesheet_intf.declarations;
    nested = prev.Stylesheet_intf.nested @ rule.Stylesheet_intf.nested;
    merge_key = prev.Stylesheet_intf.merge_key;
  }

let adjacent rules =
  let step acc prev rule rest =
    if same_selector prev rule then
      let merged = pair prev rule in
      (acc, Some merged, rest)
    else (prev :: acc, Some rule, rest)
  in
  let rec merge acc prev_rule = function
    | [] -> List.rev (match prev_rule with Some r -> r :: acc | None -> acc)
    | (rule : Stylesheet.rule) :: rest -> (
        match prev_rule with
        | None -> merge acc (Some rule) rest
        | Some prev ->
            let acc, prev, rest = step acc prev rule rest in
            merge acc prev rest)
  in
  preserve rules (merge [] None rules)

let rec has_descendant_pseudo_element : Selector.t -> bool = function
  | Combined (_, Descendant, right) -> pseudo right <> None
  | Compound sels -> List.exists has_descendant_pseudo_element sels
  | List sels -> List.exists has_descendant_pseudo_element sels
  | _ -> false

let should_not_combine selector =
  Selector.is_compound_list selector
  || vendor selector
  || has_descendant_pseudo_element selector

let modifier_depth class_name =
  let len = String.length class_name in
  let rec loop i depth bracket_depth =
    if i >= len then depth
    else
      match class_name.[i] with
      | '[' -> loop (i + 1) depth (bracket_depth + 1)
      | ']' -> loop (i + 1) depth (max 0 (bracket_depth - 1))
      | '\\' when i + 1 < len && class_name.[i + 1] = ':' ->
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
  let pe1 = pseudo sel1 in
  let pe2 = pseudo sel2 in
  if pe1 <> pe2 then false
  else
    match (Selector.first_class sel1, Selector.first_class sel2) with
    | Some c1, Some c2 ->
        if has_escaped_colon c1 <> has_escaped_colon c2 then false
        else
          let d1 = modifier_depth c1 in
          let d2 = modifier_depth c2 in
          (d1 > 0 && d2 > 0) || d1 = d2
    | _ -> true

let rec has_not_pseudo = function
  | Selector.Not _ -> true
  | Selector.Compound sels -> List.exists has_not_pseudo sels
  | _ -> false

let rec has_has_pseudo = function
  | Selector.Has _ -> true
  | Selector.Compound sels -> List.exists has_has_pseudo sels
  | _ -> false

let base_sort_key sel =
  if has_not_pseudo sel then
    if Selector.has_group_marker sel then -3
    else if Selector.has_peer_marker sel then -2
    else -1
  else if has_has_pseudo sel then
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

let class_has_bracket cls = String.contains cls '['

let class_base_and_slash cls =
  let len = String.length cls in
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

let variant_family cls =
  let bracket_pos = String.index_opt cls '[' in
  let colon_pos =
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
  | Some bi, _ -> String.sub cls 0 bi
  | _, Some ci -> String.sub cls 0 ci
  | _ -> cls

let bracket_content_type cls =
  match String.index_opt cls '[' with
  | Some i when i + 1 < String.length cls && cls.[i + 1] = ':' -> 0
  | _ -> 1

let normalize_attr_base b =
  let starts_with s p =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  if
    starts_with b "aria-[" || starts_with b "data-["
    || starts_with b "group-aria-["
    || starts_with b "group-data-["
    || starts_with b "peer-aria-["
    || starts_with b "peer-data-["
  then String.map (fun c -> if c = '_' then ' ' else c) b
  else b

let compare_bracket_selectors c1 c2 i1 i2 =
  let bt_cmp =
    Int.compare (bracket_content_type c1) (bracket_content_type c2)
  in
  if bt_cmp <> 0 then bt_cmp
  else
    let base1, slash1 = class_base_and_slash c1 in
    let base2, slash2 = class_base_and_slash c2 in
    let base_cmp =
      String.compare (normalize_attr_base base1) (normalize_attr_base base2)
    in
    if base_cmp <> 0 then base_cmp
    else
      let slash_cmp = Bool.compare slash1 slash2 in
      if slash_cmp <> 0 then slash_cmp else Int.compare i1 i2

type selector_kind = Named | Bracket

let compare_same_family_selectors kind1 kind2 c1 c2 i1 i2 =
  match (kind1, kind2) with
  | Named, Bracket -> -1
  | Bracket, Named -> 1
  | Bracket, Bracket -> compare_bracket_selectors c1 c2 i1 i2
  | Named, Named -> Int.compare i1 i2

let compare_selectors (sel1, i1) (sel2, i2) =
  let k1 = selector_sort_key sel1 and k2 = selector_sort_key sel2 in
  let c = compare k1 k2 in
  if c <> 0 then c
  else
    let c1 = match Selector.first_class sel1 with Some c -> c | None -> "" in
    let c2 = match Selector.first_class sel2 with Some c -> c | None -> "" in
    let kind_of b = if b then Bracket else Named in
    let k1 = kind_of (class_has_bracket c1) in
    let k2 = kind_of (class_has_bracket c2) in
    let fam1 = variant_family c1 in
    let fam2 = variant_family c2 in
    if fam1 = fam2 then compare_same_family_selectors k1 k2 c1 c2 i1 i2
    else if k1 = Bracket && k2 = Bracket then
      let cls_cmp = String.compare c1 c2 in
      if cls_cmp <> 0 then cls_cmp else Int.compare i1 i2
    else Int.compare i1 i2

let rule_of_group = function
  | [ (sel, decls, _) ] ->
      Some
        {
          Stylesheet_intf.selector = sel;
          declarations = decls;
          nested = [];
          merge_key = None;
        }
  | [] -> None
  | group ->
      let selectors = List.rev group |> List.map (fun (s, _, _) -> s) in
      let sorted =
        let indexed = List.mapi (fun i s -> (s, i)) selectors in
        List.sort compare_selectors indexed |> List.map fst
      in
      let _, decls, _ = List.hd group in
      Some
        {
          Stylesheet_intf.selector = selector_list sorted;
          declarations = decls;
          nested = [];
          merge_key = None;
        }

let flush_group acc group =
  match rule_of_group group with Some rule -> rule :: acc | None -> acc

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

let can_combine_rules ~same (prev : Stylesheet.rule) (rule : Stylesheet.rule) =
  declarations_equal ~same prev.Stylesheet_intf.declarations
    rule.Stylesheet_intf.declarations
  && compatible prev.Stylesheet_intf.selector rule.Stylesheet_intf.selector
  &&
  match (prev.Stylesheet_intf.merge_key, rule.Stylesheet_intf.merge_key) with
  | Some k1, Some k2 when k1 = k2 ->
      let pe1 = pseudo prev.Stylesheet_intf.selector in
      let pe2 = pseudo rule.Stylesheet_intf.selector in
      let pseudo_tier : Selector.t option -> int = function
        | None -> 0
        | Some (Selector.Before _) | Some (After _) -> 1
        | Some (First_letter _) | Some (First_line _) -> 2
        | Some Placeholder | Some Backdrop -> 3
        | Some Details_content -> 4
        | Some Marker -> 5
        | Some Selection -> 6
        | Some File_selector_button -> 7
        | Some _ -> 8
      in
      pseudo_tier pe1 = pseudo_tier pe2
  | _ ->
      can_combine_selectors prev.Stylesheet_intf.selector
        rule.Stylesheet_intf.selector

let cannot_combine (rule : Stylesheet.rule) =
  rule.Stylesheet_intf.nested <> []
  || should_not_combine rule.Stylesheet_intf.selector

let group_member_of_rule (rule : Stylesheet.rule) =
  ( rule.Stylesheet_intf.selector,
    rule.Stylesheet_intf.declarations,
    rule.Stylesheet_intf.merge_key )

let prev_rule_of_group_head (prev_sel, prev_decls, prev_merge_key) =
  {
    Stylesheet_intf.selector = prev_sel;
    declarations = prev_decls;
    nested = [];
    merge_key = prev_merge_key;
  }

let summary_of_rule (rule : Stylesheet.rule) =
  Selector_summary.of_selector rule.Stylesheet_intf.selector

let property_set decls = List.map Declaration.property_name decls

let writes_any_of props decls =
  List.exists (fun d -> List.mem (Declaration.property_name d) props) decls

let delayed_blocks_group ~group_props ~rule_summary delayed =
  List.exists
    (fun ((dr : Stylesheet.rule), ds) ->
      Selector_summary.may_overlap ds rule_summary
      && writes_any_of group_props dr.Stylesheet_intf.declarations)
    delayed

let rule_perturbs_group ~group_props ~rule_summary current_group
    (rule : Stylesheet.rule) =
  let overlaps_group =
    List.exists
      (fun (_, _, s) -> Selector_summary.may_overlap s rule_summary)
      current_group
  in
  overlaps_group && writes_any_of group_props rule.Stylesheet_intf.declarations

let flush_combined_group acc current_group delayed =
  let acc =
    match current_group with
    | [] -> acc
    | [ (rule, _, _) ] -> rule :: acc
    | _ ->
        let group_members =
          List.map (fun (_, member, _) -> member) current_group
        in
        flush_group acc group_members
  in
  List.fold_left (fun acc (rule, _) -> rule :: acc) acc delayed

let can_join_group ~same ~group_props ~rule_summary delayed prev_rule rule =
  can_combine_rules ~same prev_rule rule
  && not (delayed_blocks_group ~group_props ~rule_summary delayed)

let group_member rule rule_summary =
  (rule, group_member_of_rule rule, rule_summary)

let combine_adjacent_identical_decls ~same rules =
  let rec walk acc = function
    | [] -> List.rev acc
    | r1 :: r2 :: rest
      when (not (cannot_combine r1))
           && (not (cannot_combine r2))
           && can_combine_rules ~same r1 r2 ->
        let members = [ group_member_of_rule r2; group_member_of_rule r1 ] in
        let combined =
          match rule_of_group members with Some r -> r | None -> r1
        in
        walk acc (combined :: rest)
    | r :: rest -> walk (r :: acc) rest
  in
  preserve rules (walk [] rules)

(* Body-keyed global combine. The local sliding walker in {!identical} closes
   its current group the moment one intermediate rule writes a group property on
   a selector overlapping the group head, even when a same-body rule further
   down could still safely join (the perturbing intermediate may not overlap the
   later candidate at all). This pass buckets every eligible rule by its body
   fingerprint, sorts each bucket by source position, then greedily absorbs the
   later occurrences into the earliest as long as the gap is cascade-safe
   against the actual candidate's selector.

   Absorbing candidate [T_j] (at source position [p_j]) into the anchor at
   [p_anchor] effectively moves [T_j]'s body up the cascade. The only elements
   whose cascade is affected are those matching [T_j] and also matching some
   intermediate [S_k] at [p_anchor < p_k < p_j] that writes one of the body's
   properties. For those, soundness requires the cascade winner be the same
   before and after the move. With same-tier importance this needs strictly
   different specificity on every overlapping subselector pair (so the winner
   is determined by specificity alone, not source order); with different
   importance the [!important] half wins regardless of position. Previously
   absorbed group members keep their existing cascade because their bodies are
   already at the anchor's position; the combined rule writes the same value to
   every member's elements anyway, so cross-member overlaps within the group are
   never a hazard. *)
let identical_global ?(extend_lists = false) ~same
    (rules : Stylesheet.rule list) : Stylesheet.rule list =
  let arr = Array.of_list rules in
  let n = Array.length arr in
  if n < 2 then rules
  else
    let subselectors = Array.make n [] in
    let props = Array.make n [] in
    let importance_of = Array.make n [] in
    let eligible = Array.make n false in
    for i = 0 to n - 1 do
      let r = arr.(i) in
      subselectors.(i) <-
        Selector.as_list r.Stylesheet_intf.selector
        |> Option.value ~default:[ r.Stylesheet_intf.selector ];
      props.(i) <- property_set r.Stylesheet_intf.declarations;
      importance_of.(i) <-
        List.map
          (fun d ->
            (Declaration.property_name d, Declaration.is_important d))
          r.Stylesheet_intf.declarations;
      let blocked =
        if extend_lists then
          r.Stylesheet_intf.nested <> []
          || vendor r.Stylesheet_intf.selector
          || has_descendant_pseudo_element r.Stylesheet_intf.selector
        else cannot_combine r
      in
      if not blocked then eligible.(i) <- true
    done;
    let buckets : (string list, int list ref) Hashtbl.t = Hashtbl.create 64 in
    for i = 0 to n - 1 do
      if eligible.(i) then begin
        let key = props.(i) in
        match Hashtbl.find_opt buckets key with
        | Some lst -> lst := i :: !lst
        | None -> Hashtbl.add buckets key (ref [ i ])
      end
    done;
    let specificity_equal a b =
      let sa = Selector.specificity a in
      let sb = Selector.specificity b in
      sa.ids = sb.ids && sa.classes = sb.classes && sa.elements = sb.elements
    in
    let specificity_ties subs_a subs_b =
      List.exists
        (fun a ->
          let sa = Selector_summary.of_selector a in
          List.exists
            (fun b ->
              let sb = Selector_summary.of_selector b in
              Selector_summary.may_overlap sa sb && specificity_equal a b)
            subs_b)
        subs_a
    in
    let merge_into = Array.make n None in
    let attempt_absorb ~anchor ~group_indices ~anchor_importance j =
      let anchor_r = arr.(anchor) in
      let rj = arr.(j) in
      if not (can_combine_rules ~same anchor_r rj) then false
      else
        let lo = List.fold_left min j !group_indices in
        let hi = List.fold_left max j !group_indices in
        let candidate_subs = subselectors.(j) in
        let safe = ref true in
        let k = ref (lo + 1) in
        while !safe && !k < hi do
          let kk = !k in
          if kk <> j && not (List.mem kk !group_indices) then begin
            let intersects_with_same_importance =
              List.exists
                (fun (p, imp_k) ->
                  match List.assoc_opt p anchor_importance with
                  | Some imp_body -> imp_k = imp_body
                  | None -> false)
                importance_of.(kk)
            in
            if intersects_with_same_importance then begin
              let inter_subs = subselectors.(kk) in
              if specificity_ties inter_subs candidate_subs then
                safe := false
            end
          end;
          incr k
        done;
        if !safe then begin
          merge_into.(j) <- Some anchor;
          group_indices := j :: !group_indices;
          true
        end
        else false
    in
    Hashtbl.iter
      (fun _ entries_ref ->
        match List.sort compare !entries_ref with
        | [] | [ _ ] -> ()
        | anchor :: rest ->
            let group_indices = ref [ anchor ] in
            let anchor_importance = importance_of.(anchor) in
            List.iter
              (fun j ->
                ignore (attempt_absorb ~anchor ~group_indices ~anchor_importance j))
              rest)
      buckets;
    let any_change = ref false in
    Array.iter (function Some _ -> any_change := true | None -> ()) merge_into;
    if not !any_change then rules
    else
      let absorbed_by = Array.make n [] in
      Array.iteri
        (fun j m ->
          match m with
          | Some i -> absorbed_by.(i) <- j :: absorbed_by.(i)
          | None -> ())
        merge_into;
      let out = ref [] in
      for i = n - 1 downto 0 do
        if merge_into.(i) = None then
          begin match absorbed_by.(i) with
          | [] -> out := arr.(i) :: !out
          | js ->
              let indices = List.sort compare (i :: js) in
              let members =
                List.map (fun k -> group_member_of_rule arr.(k)) indices
              in
              let combined =
                match rule_of_group (List.rev members) with
                | Some r -> r
                | None -> arr.(i)
              in
              out := combined :: !out
          end
      done;
      !out

let identical ~same rules =
  let rec combine_consecutive acc current_group delayed = function
    | [] -> List.rev (flush_combined_group acc current_group delayed)
    | (rule : Stylesheet.rule) :: rest ->
        if cannot_combine rule then
          let acc = rule :: flush_combined_group acc current_group delayed in
          combine_consecutive acc [] [] rest
        else extend_delay_or_restart acc current_group delayed rule rest
  and extend_delay_or_restart acc current_group delayed rule rest =
    let rule_summary = summary_of_rule rule in
    let push_to_group () =
      let member = group_member rule rule_summary in
      combine_consecutive acc (member :: current_group) delayed rest
    in
    match current_group with
    | [] -> push_to_group ()
    | (_, head_member, _) :: _ ->
        let prev_rule = prev_rule_of_group_head head_member in
        let group_props = property_set prev_rule.Stylesheet_intf.declarations in
        if
          can_join_group ~same ~group_props ~rule_summary delayed prev_rule rule
        then push_to_group ()
        else
          let perturbs =
            rule_perturbs_group ~group_props ~rule_summary current_group rule
          in
          if not perturbs then
            combine_consecutive acc current_group
              (delayed @ [ (rule, rule_summary) ])
              rest
          else
            let acc = flush_combined_group acc current_group delayed in
            let member = group_member rule rule_summary in
            combine_consecutive acc [ member ] [] rest
  in
  combine_consecutive [] [] [] rules
  |> preserve rules
  |> combine_adjacent_identical_decls ~same
