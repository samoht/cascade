(** Rule-local optimization passes. *)

open Declaration
open Stylesheet
open Common

let preserve_list = List.preserve
let list_map_preserve = List.map_preserve
let list_filter_preserve = List.filter_preserve

let with_declarations (rule : rule) declarations =
  if declarations == rule.declarations then rule else { rule with declarations }

let same_property = Shorthand.same_property
let is_intentionally_duplicated = Shorthand.is_intentionally_duplicated
let deduplicate_declarations_with = Shorthand.deduplicate_declarations_with
let canonical_selector_key = Merge.key

let rec selector_targets_host_or_root : Selector.t -> bool = function
  | Selector.Root | Selector.Host _ -> true
  | Selector.List xs -> List.for_all selector_targets_host_or_root xs
  | Selector.Where xs | Selector.Is xs ->
      List.for_all selector_targets_host_or_root xs
  | _ -> false

let custom_property_name = function
  | Declaration { property = Custom_property name; _ } -> Some name
  | _ -> None

let sort_custom_property_declarations_stable decls =
  let customs = List.filter (fun d -> custom_property_name d <> None) decls in
  let sorted =
    List.stable_sort
      (fun d1 d2 ->
        match (custom_property_name d1, custom_property_name d2) with
        | Some n1, Some n2 -> String.compare n1 n2
        | _ -> 0)
      customs
  in
  let next = ref sorted in
  let result =
    List.map
      (fun d ->
        if custom_property_name d <> None then
          match !next with
          | x :: rest ->
              next := rest;
              x
          | [] -> d
        else d)
      decls
  in
  if List.equal ( == ) result decls then decls else result

let sort_commuting ?selector decls =
  match selector with
  | Some sel when selector_targets_host_or_root sel ->
      sort_custom_property_declarations_stable decls
  | _ -> decls

(** {1 Rule Optimization} *)

let single ~ctx (rule : rule) : rule =
  let declarations =
    list_map_preserve
      (Declaration.normalize ~lossless:(Ctx.lossless ctx))
      rule.declarations
    |> deduplicate_declarations_with ~ctx ~merge_box:false
    |> sort_commuting ~selector:rule.selector
    |> preserve_list rule.declarations
  in
  with_declarations rule declarations

(* CSS Multicol 2 sec. 6.1: [column-width] + [column-count] collapse to
   [columns], which resets exactly those two longhands, so the rewrite is
   cascade-safe with no closed-world assumption. The longhands are unknown
   properties, so their values are re-parsed here. *)
let columns_value_of_longhands width count : Properties.columns_value =
  match (width, count) with
  | `Auto, `Auto -> (Auto : Properties.columns_value)
  | `Auto, `Count n -> Auto_count n
  | `Width w, `Auto -> Width w
  | `Width w, `Count n -> Both (w, n)

(* Compose the unique [column-width]/[column-count] pair into [columns] when
   both carry a plain (non-[var()], non-CSS-wide) matching-importance value, at
   the position of the first. *)
let synthesize_columns decls =
  let width_of d : (Properties.column_width * bool) option =
    match d with
    | Declaration { property = Column_width; value; important; _ } ->
        Some (value, important)
    | _ -> None
  in
  let count_of d : (Properties.column_count * bool) option =
    match d with
    | Declaration { property = Column_count; value; important; _ } ->
        Some (value, important)
    | _ -> None
  in
  let uniq f =
    match List.filter_map f decls with [ x ] -> Some x | _ -> None
  in
  match (uniq width_of, uniq count_of) with
  | Some (w, wi), Some (c, ci) when wi = ci -> (
      let plain_width :
          Properties.column_width -> [ `Auto | `Width of Values.length ] option
          = function
        | Auto -> Some `Auto
        | Width l -> Some (`Width l)
        | _ -> None
      in
      let plain_count :
          Properties.column_count -> [ `Auto | `Count of int ] option = function
        | Auto -> Some `Auto
        | Count n -> Some (`Count n)
        | _ -> None
      in
      match (plain_width w, plain_count c) with
      | Some w, Some c ->
          let shorthand =
            Declaration.v ~important:wi Properties.Columns
              (columns_value_of_longhands w c)
          in
          let placed = ref false in
          List.filter_map
            (fun d ->
              match d with
              | Declaration { property = Column_width; _ }
              | Declaration { property = Column_count; _ } ->
                  if !placed then None
                  else (
                    placed := true;
                    Some shorthand)
              | _ -> Some d)
            decls
      | _ -> decls)
  | _ -> decls

(* CSS Anchor Positioning 1: [position-try] is [<order> || <fallbacks>]. No
   typed [position-try] shorthand exists, so the synthesised value is re-parsed
   into the round-tripping unknown property. *)
let synthesize_position_try decls =
  let order_of d : (Properties.position_try_order * bool) option =
    match d with
    | Declaration { property = Position_try_order; value; important; _ } ->
        Some (value, important)
    | _ -> None
  in
  let fallbacks_of d : (Properties.position_try_fallbacks * bool) option =
    match d with
    | Declaration { property = Position_try_fallbacks; value; important; _ } ->
        Some (value, important)
    | _ -> None
  in
  let uniq f =
    match List.filter_map f decls with [ x ] -> Some x | _ -> None
  in
  match (uniq order_of, uniq fallbacks_of) with
  | Some (order, oi), Some (fallbacks, fi) when oi = fi -> (
      (* Compose only plain values: a CSS-wide keyword or [var()] in one
         longhand cannot share a shorthand with a real value in the other. *)
      let plain_order : Properties.position_try_order -> bool = function
        | Normal | Most_width | Most_height | Most_block_size | Most_inline_size
          ->
            true
        | _ -> false
      in
      let plain_fallbacks : Properties.position_try_fallbacks -> bool = function
        | None | Fallbacks _ -> true
        | _ -> false
      in
      match (plain_order order, plain_fallbacks fallbacks) with
      | true, true ->
          let shorthand =
            Declaration.v ~important:oi Properties.Position_try
              (Try (order, fallbacks))
          in
          let placed = ref false in
          List.filter_map
            (fun d ->
              match d with
              | Declaration { property = Position_try_order; _ }
              | Declaration { property = Position_try_fallbacks; _ } ->
                  if !placed then None
                  else (
                    placed := true;
                    Some shorthand)
              | _ -> Some d)
            decls
      | _ -> decls)
  | _ -> decls

let finalize ?(canonicalize_selector = true) ~ctx (rule : rule) : rule =
  let declarations =
    deduplicate_declarations_with ~ctx rule.declarations
    |> synthesize_columns |> synthesize_position_try
    |> sort_commuting ~selector:rule.selector
    |> preserve_list rule.declarations
  in
  (* Selectors merged during factoring are fresh comma lists, so
     re-canonicalise; unchanged selectors keep their identity for the
     fixpoint. *)
  let rule =
    if canonicalize_selector then
      let canon = Selector.canonicalize rule.selector in
      if canon == rule.selector then rule else { rule with selector = canon }
    else rule
  in
  with_declarations rule declarations

let drop_shadowed_rules (rules : rule list) : rule list =
  let later_by_selector = Cover.v () in
  let rules_arr = Array.of_list rules in
  let len = Array.length rules_arr in
  let dropped = Array.make len false in
  let changed = ref false in
  for i = len - 1 downto 0 do
    let rule = rules_arr.(i) in
    dropped.(i) <-
      (rule.declarations = [] && rule.nested = [])
      || rule.declarations <> []
         && List.for_all
              (Cover.covered later_by_selector rule.Stylesheet_intf.selector)
              rule.declarations;
    if dropped.(i) then changed := true;
    List.iter
      (Cover.add later_by_selector rule.Stylesheet_intf.selector)
      rule.declarations
  done;
  let rec filter i = function
    | [] -> []
    | rule :: rest ->
        let rest = filter (i + 1) rest in
        if dropped.(i) then rest else rule :: rest
  in
  if !changed then filter 0 rules else rules

(* Finer-grained sibling of [drop_shadowed_rules]: keep the rule but drop
   declarations whose property a later same-selector rule rewrites for every
   selector this rule targets, at same-or-stronger importance (regardless of
   intervening specificity; cleancss and csso rely on this). For a list selector
   each selector must be independently shadowed, so later rules are indexed per
   selector key to keep the check linear. Empty rules left behind are pruned by
   [drop_empty_rules] downstream. *)
let selector_keys (r : rule) =
  match Selector.as_list r.Stylesheet_intf.selector with
  | Some xs -> List.map canonical_selector_key xs
  | None -> [ canonical_selector_key r.Stylesheet_intf.selector ]

let later_by_selector_key (indexed : (int * rule) list) =
  let later_by_key :
      (Selector.t list, (int * Declaration.t list) list) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun ((i, r) : int * rule) ->
      List.iter
        (fun key ->
          let prev =
            Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
          in
          Hashtbl.replace later_by_key key
            ((i, r.Stylesheet_intf.declarations) :: prev))
        (selector_keys r))
    indexed;
  later_by_key

let shadowed_by_later ~later_by_key ~rule_index ~keys decl =
  let property_shadowed_for_key key =
    let writes =
      Hashtbl.find_opt later_by_key key |> Option.value ~default:[]
    in
    List.exists
      (fun (j, decls) ->
        j > rule_index
        && List.exists
             (fun ld ->
               same_property decl ld
               && (Declaration.is_important ld
                  || not (Declaration.is_important decl)))
             decls)
      writes
  in
  (not (is_intentionally_duplicated decl))
  && List.for_all property_shadowed_for_key keys

let drop_shadowed_declarations (rules : rule list) : rule list =
  let indexed = List.mapi (fun i r -> (i, r)) rules in
  let later_by_key = later_by_selector_key indexed in
  let changed = ref false in
  let rules' =
    List.map
      (fun (i, rule) ->
        let keys = selector_keys rule in
        let kept =
          list_filter_preserve
            (fun d ->
              not (shadowed_by_later ~later_by_key ~rule_index:i ~keys d))
            rule.declarations
        in
        let rule' = with_declarations rule kept in
        if not (rule' == rule) then changed := true;
        rule')
      indexed
  in
  if !changed then rules' else rules

let drop_shadowed rules =
  rules |> drop_shadowed_declarations |> drop_shadowed_rules

(* Adjacent identical-body merging: two neighbouring rules with the same
   declarations apply identically whether split or grouped, so the merge is
   cascade-safe for any DOM with no ordering analysis. It runs locally because
   global factoring finds these groups only when its preflight predicts enough
   gain and discards its result when the transfer estimate grows - neither
   gamble should cost the obvious local merge. *)
let adjacent_merge_eligible ~ctx (r : rule) =
  r.nested = [] && r.merge_key = None
  && (not (Merge.vendor r.selector))
  && (Ctx.extend_lists ctx || not (Selector.is_compound_list r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let merged_selector group =
  Selector.canonicalize
    (Merge.selector_list
       (List.concat_map (fun (r : rule) -> Edge.selectors r.selector) group))

let merge_adjacent_identical ~ctx (rules : rule list) : rule list =
  let bodies_equal a b =
    Merge.declarations_equal ~same:Shorthand.same_minified_declaration a b
  in
  let changed = ref false in
  let rec go = function
    | [] -> []
    | r :: rest when adjacent_merge_eligible ~ctx r && r.declarations <> [] ->
        let rec take group = function
          | s :: tail
            when adjacent_merge_eligible ~ctx s
                 && bodies_equal r.declarations s.declarations
                 && List.for_all
                      (fun (g : rule) -> Merge.compatible g.selector s.selector)
                      group ->
              take (s :: group) tail
          | tail -> (group, tail)
        in
        let group, rest = take [ r ] rest in
        let merged =
          match group with
          | [ _ ] -> r
          | group ->
              changed := true;
              { r with selector = merged_selector (List.rev group) }
        in
        merged :: go rest
    | r :: rest -> r :: go rest
  in
  let result = go rules in
  if !changed then result else rules
