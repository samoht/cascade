(** Fast local rewrite candidate generation for {!Rule_graph}. *)

open Stylesheet
open Stdlib
open Rule_rewrite
module Node_set = Set.Make (Rule_graph.Node_id)

let same_decl = Shorthand.same_minified_declaration

type decl_fact = {
  decl : Declaration.declaration;
  important : bool;
  keys : Shorthand.overlap_key list;
}

let decl_fact decl =
  {
    decl;
    important = Declaration.is_important decl;
    keys = Shorthand.declaration_overlap_keys decl;
  }

let decl_facts decls = List.map decl_fact decls

let decl_facts_conflict a b =
  a.important = b.important
  && (not (same_decl a.decl b.decl))
  && Shorthand.declarations_overlap_with_keys a.decl a.keys b.decl b.keys

let declaration_blocks_commute left right =
  let left = decl_facts left in
  let right = decl_facts right in
  not
    (List.exists
       (fun a -> List.exists (fun b -> decl_facts_conflict a b) right)
       left)

let declarations_equal (a : Declaration.declaration list)
    (b : Declaration.declaration list) =
  Merge.declarations_equal ~same:same_decl a b

let merge_selector_list = Merge.selector_list
let contains_vendor_pseudo_element = Merge.vendor
let selectors_compatible = Merge.compatible
let decls_size = Size.decls
let mix_int acc x = ((acc lsl 5) - acc) lxor x
let hash_bool = function false -> 0 | true -> 1

let hash_string s =
  let hash = ref 0x811c9dc5 in
  String.iter (fun c -> hash := mix_int !hash (Char.code c)) s;
  !hash

let hash_ints xs = List.fold_left mix_int 0x345678 xs

module Int_table = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash x = x land max_int
end)

module String_table = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash s = hash_string s land max_int
end)

let hash_strings strings =
  strings
  |> List.fold_left (fun hash s -> mix_int hash (hash_string s)) 0x123456

let rule_eligible (r : rule) =
  r.nested = [] && r.merge_key = Option.None
  && (not (contains_vendor_pseudo_element r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

(* Identical-body grouping merges same-body rules under a combined selector list
   without touching the body or its order, so it is safe for custom-property
   rules too - unlike the factoring passes, which reorder declarations and would
   disturb a later [var()] resolution. [try_rewrite]'s acyclicity check still
   rejects a group whose merge would cross a conflicting (re)definition. *)
let identical_body_eligible ~ctx (r : rule) =
  r.nested = [] && r.merge_key = Option.None
  && (not (contains_vendor_pseudo_element r.selector))
  && (Ctx.extend_lists ctx || not (Selector.is_compound_list r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let selector_branch_key selector =
  Pp.to_string ~minify:true Selector.pp (Selector.canonicalize selector)

let selector_key (r : rule) =
  Edge.selectors r.selector
  |> List.map selector_branch_key
  |> List.sort String.compare

let selector_list_key selectors =
  List.map selector_branch_key selectors |> List.sort String.compare

let hash_selector_list selectors =
  selectors |> selector_list_key |> hash_strings

let selector_key_hash (r : rule) =
  Edge.selectors r.selector |> hash_selector_list

let selector_keys_equal left right =
  let rec loop left right =
    match (left, right) with
    | [], [] -> true
    | l :: left, r :: right -> String.equal l r && loop left right
    | _ -> false
  in
  loop left right

let pairwise_compatible rules =
  let rec loop = function
    | [] | [ _ ] -> true
    | (r : rule) :: rest ->
        List.for_all
          (fun (other : rule) -> selectors_compatible r.selector other.selector)
          rest
        && loop rest
  in
  loop rules

(* Any identical-body rules can group into a selector list: a list of disjoint
   selectors, including distinct pseudo-elements ([::before] and [::after] never
   match a common box), is exactly equivalent to the separate rules, and the DAG
   enforces cascade-order safety. *)
let can_group_selectors rules = pairwise_compatible rules
let body_equal_key g id = Rule_graph.declaration_body_key g id
let body_bucket_key g id = body_equal_key g id |> hash_ints

let add_int_bucket tbl key value =
  let prev = Int_table.find_opt tbl key |> Option.value ~default:[] in
  Int_table.replace tbl key (value :: prev)

let add_string_bucket tbl key value =
  let prev = String_table.find_opt tbl key |> Option.value ~default:[] in
  String_table.replace tbl key (value :: prev)

let touching_set = function
  | Option.None -> Option.None
  | Option.Some ids -> Option.Some (Node_set.of_list ids)

let touches_node touching id =
  match touching with
  | Option.None -> true
  | Option.Some ids -> Node_set.mem id ids

let touches_any touching ids =
  match touching with
  | Option.None -> true
  | Option.Some touching -> List.exists (fun id -> Node_set.mem id touching) ids

let live_rules g =
  Rule_graph.live_nodes g
  |> List.map (fun id -> (id, Rule_graph.node_rule g id))

let rules_with_ids g ids =
  List.map (fun id -> (id, Rule_graph.node_rule g id)) ids

let candidate ?size_cache ~kind ~finalize g ~consume ~produce =
  Rule_rewrite.v ?size_cache ~kind ~finalize g ~consume ~produce

let selector_size (r : rule) = Pp.size ~minify:true Selector.pp r.selector
let decls_inline_cost decls = decls_size decls + List.length decls

let specificity_equal a b =
  let a = Selector.specificity a in
  let b = Selector.specificity b in
  a.ids = b.ids && a.classes = b.classes && a.elements = b.elements

let selectors_tie_and_overlap a b =
  List.exists
    (fun selector_a ->
      let summary_a = Selector_summary.of_selector selector_a in
      List.exists
        (fun selector_b ->
          specificity_equal selector_a selector_b
          && Selector_summary.may_overlap summary_a
               (Selector_summary.of_selector selector_b))
        (Edge.selectors b))
    (Edge.selectors a)

let grouped_rule (rules : rule list) : rule option =
  match rules with
  | [] -> Option.None
  | first :: _ ->
      if not (can_group_selectors rules) then Option.None
      else
        Option.Some
          {
            first with
            selector =
              merge_selector_list
                (List.map (fun (r : rule) -> r.selector) rules);
            nested = [];
            merge_key = Option.None;
          }

type declaration_order_key = { property : string; important : bool; hash : int }

let declaration_order_key decl =
  {
    property = Declaration.property_name decl;
    important = Declaration.is_important decl;
    hash = Declaration.hash decl;
  }

let compare_declaration_order_key left right =
  match String.compare left.property right.property with
  | 0 -> (
      match Bool.compare left.important right.important with
      | 0 -> Int.compare left.hash right.hash
      | order -> order)
  | order -> order

let rec compare_declaration_order_keys left right =
  match (left, right) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | left :: left_rest, right :: right_rest -> (
      match compare_declaration_order_key left right with
      | 0 -> compare_declaration_order_keys left_rest right_rest
      | order -> order)

let compare_rule_order_key (left_id, (left_rule : rule))
    (right_id, (right_rule : rule)) =
  match
    compare_declaration_order_keys
      (List.map declaration_order_key left_rule.declarations)
      (List.map declaration_order_key right_rule.declarations)
  with
  | 0 ->
      Int.compare
        (Rule_graph.Node_id.to_int left_id)
        (Rule_graph.Node_id.to_int right_id)
  | order -> order

let same_selector_merge_order
    (rules_with_ids : (Rule_graph.node_id * rule) list) =
  let rows = Array.of_list rules_with_ids in
  let n = Array.length rows in
  let succ = Array.make n [] in
  let pred = Array.make n 0 in
  for i = 0 to n - 1 do
    let _, left = rows.(i) in
    for j = i + 1 to n - 1 do
      let _, right = rows.(j) in
      if not (declaration_blocks_commute left.declarations right.declarations)
      then begin
        succ.(i) <- j :: succ.(i);
        pred.(j) <- pred.(j) + 1
      end
    done
  done;
  let rec insert index = function
    | [] -> [ index ]
    | head :: rest as queue ->
        if compare_rule_order_key rows.(index) rows.(head) < 0 then
          index :: queue
        else head :: insert index rest
  in
  let queue = ref [] in
  for i = 0 to n - 1 do
    if pred.(i) = 0 then queue := insert i !queue
  done;
  let emitted = ref [] in
  while !queue <> [] do
    match !queue with
    | [] -> ()
    | index :: rest ->
        queue := rest;
        emitted := rows.(index) :: !emitted;
        List.iter
          (fun next ->
            pred.(next) <- pred.(next) - 1;
            if pred.(next) = 0 then queue := insert next !queue)
          succ.(index)
  done;
  match List.rev !emitted with
  | ordered when List.length ordered = n -> ordered
  | _ -> rules_with_ids

let ordered_ids g ids =
  let compare_by_origin a b =
    match
      Int.compare (Rule_graph.node_origin g a) (Rule_graph.node_origin g b)
    with
    | 0 -> Rule_graph.Node_id.compare a b
    | order -> order
  in
  ids |> Node_set.of_list |> Node_set.elements |> List.sort compare_by_origin

let first_origin g ids =
  match ordered_ids g ids with
  | [] -> max_int
  | id :: _ -> Rule_graph.node_origin g id

let candidate_set_key ids =
  ids |> List.map Rule_graph.Node_id.to_int |> hash_ints

let unique_ids_preserve_order ids =
  let seen = ref Node_set.empty in
  List.filter
    (fun id ->
      if Node_set.mem id !seen then false
      else begin
        seen := Node_set.add id !seen;
        true
      end)
    ids

let bounded_subsets ?(large_bucket_candidates = 32) ~limit ids =
  let ids = unique_ids_preserve_order ids in
  let n = List.length ids in
  let seen = Int_table.create 32 in
  let max_candidates = if n > limit then large_bucket_candidates else 128 in
  let add acc ids =
    match ids with
    | [] | [ _ ] -> acc
    | _ ->
        let key = candidate_set_key ids in
        if Int_table.mem seen key then acc
        else begin
          Int_table.replace seen key ();
          ids :: acc
        end
  in
  let acc = if n <= limit then add [] ids else [] in
  let acc =
    if n <= 2 || n > limit then acc
    else
      List.fold_left
        (fun acc drop ->
          if List.length acc >= max_candidates then acc
          else
            add acc
              (List.filter
                 (fun id -> Rule_graph.Node_id.compare id drop <> 0)
                 ids))
        acc ids
  in
  let rec take n xs =
    if n <= 0 then []
    else match xs with [] -> [] | x :: xs -> x :: take (n - 1) xs
  in
  let rec drop n xs =
    if n <= 0 then xs else match xs with [] -> [] | _ :: xs -> drop (n - 1) xs
  in
  let window width start = ids |> drop start |> take width in
  let max_width = if n > limit then min 128 n else min limit n in
  let rec widths width acc count =
    if width < 2 || count >= max_candidates then acc
    else
      let rec starts start acc count =
        if start + width > n || count >= max_candidates then (acc, count)
        else starts (start + 1) (add acc (window width start)) (count + 1)
      in
      let acc, count = starts 0 acc count in
      widths (width - 1) acc count
  in
  widths max_width acc (List.length acc) |> List.rev

type indexed_budget = {
  occurrence_window : int;
  max_span : int;
  max_candidates : int;
}

type subset_budget =
  | Exhaustive of { limit : int; large_bucket_candidates : int }
  | Indexed of indexed_budget

type subset_budget_state = { mutable remaining : int }

let indexed_budget_state budget = { remaining = budget.max_candidates }

let normal_indexed_budget =
  { occurrence_window = 24; max_span = 128; max_candidates = 256 }

let exact_subset_budget ~ctx g =
  if Ctx.aggressive ctx || Rule_graph.node_count g <= 128 then
    Exhaustive
      {
        limit = (if Ctx.aggressive ctx then 12 else 8);
        large_bucket_candidates = 32;
      }
  else Indexed normal_indexed_budget

let default_subset_budget ~ctx g =
  if Ctx.aggressive ctx || Rule_graph.node_count g <= 128 then
    Exhaustive
      {
        limit = (if Ctx.aggressive ctx then 12 else 8);
        large_bucket_candidates = (if Ctx.aggressive ctx then 32 else 1);
      }
  else Indexed normal_indexed_budget

let origin_span g first last =
  Rule_graph.node_origin g last - Rule_graph.node_origin g first

let indexed_rows g ids =
  let ids = ids |> unique_ids_preserve_order |> ordered_ids g in
  Array.of_list ids

let indexed_candidate_count g budget ids =
  let rows = indexed_rows g ids in
  let len = Array.length rows in
  let count = ref 0 in
  if len >= 2 then
    for first = 0 to len - 2 do
      let last_limit = min (len - 1) (first + budget.occurrence_window - 1) in
      let last = ref (first + 1) in
      while
        !count <= budget.max_candidates
        && !last <= last_limit
        && origin_span g rows.(first) rows.(!last) <= budget.max_span
      do
        incr count;
        incr last
      done
    done;
  !count

let indexed_bucket_is_bounded g budget ids =
  indexed_candidate_count g budget ids <= budget.max_candidates

let indexed_subsets g state budget ids =
  let rows = indexed_rows g ids in
  let len = Array.length rows in
  let seen = Int_table.create 32 in
  let candidates = ref [] in
  let add first last =
    if state.remaining > 0 then
      let subset =
        let rec loop index acc =
          if index < first then acc else loop (index - 1) (rows.(index) :: acc)
        in
        loop last []
      in
      let key = candidate_set_key subset in
      if not (Int_table.mem seen key) then begin
        Int_table.replace seen key ();
        state.remaining <- state.remaining - 1;
        candidates := subset :: !candidates
      end
  in
  if len >= 2 then
    for first = 0 to len - 2 do
      let last_limit = min (len - 1) (first + budget.occurrence_window - 1) in
      let last = ref (first + 1) in
      while
        state.remaining > 0 && !last <= last_limit
        && origin_span g rows.(first) rows.(!last) <= budget.max_span
      do
        add first !last;
        incr last
      done
    done;
  List.rev !candidates

let subset_candidates g ?budget_state budget ids =
  match budget with
  | Exhaustive { limit; large_bucket_candidates } ->
      bounded_subsets ~limit ~large_bucket_candidates ids
  | Indexed indexed -> (
      if not (indexed_bucket_is_bounded g indexed ids) then []
      else
        match budget_state with
        | Option.Some state -> indexed_subsets g state indexed ids
        | Option.None ->
            indexed_subsets g (indexed_budget_state indexed) indexed ids)

let unique_decls decls =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | decl :: rest ->
        if List.exists (same_decl decl) seen then loop seen acc rest
        else loop (decl :: seen) (decl :: acc) rest
  in
  loop [] [] decls

let contains_decl (rule : rule) decl =
  List.exists (same_decl decl) rule.declarations

let common_exact_decls (rules : rule list) =
  match rules with
  | [] -> []
  | first :: rest ->
      unique_decls first.declarations
      |> List.filter (fun decl ->
          List.for_all (fun r -> contains_decl r decl) rest)

let remove_common common decls =
  List.filter
    (fun decl -> not (List.exists (fun common -> same_decl common decl) common))
    decls

let append_unique_decls base extras =
  List.fold_left
    (fun acc decl ->
      if List.exists (same_decl decl) acc then acc else acc @ [ decl ])
    base extras

let keep_cost_aware_member ~removed (rule : rule) =
  match remove_common removed rule.declarations with
  | [] -> true
  | _ -> selector_size rule + 1 <= decls_inline_cost removed

let cost_aware_exact_members common rules_with_ids =
  List.filter
    (fun (_, rule) -> keep_cost_aware_member ~removed:common rule)
    rules_with_ids

let same_body_groups g ids =
  let rec insert id = function
    | [] -> [ (id, [ id ]) ]
    | (head, ids) :: rest ->
        let rule = Rule_graph.node_rule g id in
        let head_rule = Rule_graph.node_rule g head in
        if declarations_equal head_rule.declarations rule.declarations then
          (head, id :: ids) :: rest
        else (head, ids) :: insert id rest
  in
  ids
  |> List.fold_left (fun groups id -> insert id groups) []
  |> List.map (fun (_, ids) -> List.rev ids)

let selector_is_list g id =
  Selector.is_compound_list (Rule_graph.node_rule g id).selector

let has_selector_list g ids = List.exists (selector_is_list g) ids

let non_list_selector_ids g ids =
  List.filter (fun id -> not (selector_is_list g id)) ids

let identical_body_candidate ?size_cache ~finalize g ids =
  match ids with
  | [] | [ _ ] -> Option.None
  | ids -> (
      let rules : rule list = List.map (Rule_graph.node_rule g) ids in
      match grouped_rule rules with
      | Option.None -> Option.None
      | Option.Some grouped ->
          candidate ?size_cache ~kind:Identical_body ~finalize g ~consume:ids
            ~produce:[ grouped ])

let add_candidate candidates = function
  | Option.None -> ()
  | Option.Some candidate -> candidates := candidate :: !candidates

let better_than candidate = function
  | Option.None -> true
  | Option.Some baseline -> candidate.saving > baseline.saving

let add_identical_body_group ?size_cache ~ctx ~finalize g ~candidates ids =
  if Ctx.extend_lists ctx && has_selector_list g ids then begin
    let strict =
      identical_body_candidate ?size_cache ~finalize g
        (non_list_selector_ids g ids)
    in
    add_candidate candidates strict;
    match identical_body_candidate ?size_cache ~finalize g ids with
    | Option.Some extended when better_than extended strict ->
        add_candidate candidates (Option.Some extended)
    | _ -> ()
  end
  else
    add_candidate candidates
      (identical_body_candidate ?size_cache ~finalize g ids)

let identical_body_candidates ?size_cache ?touching ~ctx ~finalize g =
  let touching = touching_set touching in
  let buckets = Int_table.create 128 in
  List.iter
    (fun (id, rule) ->
      if identical_body_eligible ~ctx rule && rule.declarations <> [] then
        add_int_bucket buckets (body_bucket_key g id) id)
    (live_rules g);
  let candidates = ref [] in
  Int_table.iter
    (fun _ ids ->
      match List.rev ids with
      | [] | [ _ ] -> ()
      | ids when touches_any touching ids ->
          same_body_groups g ids
          |> List.iter (fun ids ->
              if touches_any touching ids then
                add_identical_body_group ?size_cache ~ctx ~finalize g
                  ~candidates ids)
      | _ -> ())
    buckets;
  !candidates

(* Partition a hash bucket into runs of the exact same selector key, so a
   [selector_key_hash] collision between two distinct selectors no longer
   suppresses both groups (mirrors {!same_body_groups}). *)
let same_selector_groups g ids =
  let rec insert id = function
    | [] -> [ (id, [ id ]) ]
    | (head, ids) :: rest ->
        let key = selector_key (Rule_graph.node_rule g id) in
        let head_key = selector_key (Rule_graph.node_rule g head) in
        if selector_keys_equal head_key key then (head, id :: ids) :: rest
        else (head, ids) :: insert id rest
  in
  ids
  |> List.fold_left (fun groups id -> insert id groups) []
  |> List.map (fun (_, ids) -> List.rev ids)

let add_same_selector_group ?size_cache ~finalize g ~candidates ids =
  let rules_with_ids =
    ordered_ids g ids |> rules_with_ids g |> same_selector_merge_order
  in
  let ordered = List.map fst rules_with_ids in
  let rules = List.map snd rules_with_ids in
  match rules with
  | [] | [ _ ] -> ()
  | (first : rule) :: _ -> (
      let merged =
        {
          first with
          declarations =
            List.concat_map (fun (r : rule) -> r.declarations) rules;
          nested = [];
          merge_key = Option.None;
        }
      in
      match
        candidate ?size_cache ~kind:Same_selector ~finalize g ~consume:ordered
          ~produce:[ merged ]
      with
      | Option.None -> ()
      | Option.Some c -> candidates := c :: !candidates)

let same_selector_candidates ?size_cache ?touching ~finalize g =
  let touching = touching_set touching in
  let buckets = Int_table.create 128 in
  List.iter
    (fun (id, rule) ->
      if rule_eligible rule then
        add_int_bucket buckets (selector_key_hash rule) id)
    (live_rules g);
  let candidates = ref [] in
  Int_table.iter
    (fun _ ids ->
      match List.rev ids with
      | [] | [ _ ] -> ()
      | ids when touches_any touching ids ->
          same_selector_groups g ids
          |> List.iter (fun ids ->
              if touches_any touching ids then
                add_same_selector_group ?size_cache ~finalize g ~candidates ids)
      | _ -> ())
    buckets;
  !candidates

let decl_hash_bucket decl = Declaration.hash decl

let add_decl_bucket buckets decl id =
  let hash = decl_hash_bucket decl in
  let entries = Int_table.find_opt buckets hash |> Option.value ~default:[] in
  let rec insert acc = function
    | [] -> List.rev ((decl, [ id ]) :: acc)
    | (existing, ids) :: rest when same_decl existing decl ->
        List.rev_append acc ((existing, id :: ids) :: rest)
    | entry :: rest -> insert (entry :: acc) rest
  in
  Int_table.replace buckets hash (insert [] entries)

let shared_decl_buckets g =
  let buckets = Int_table.create 256 in
  List.iter
    (fun (id, rule) ->
      if rule_eligible rule then
        unique_decls rule.declarations
        |> List.iter (fun decl -> add_decl_bucket buckets decl id))
    (live_rules g);
  buckets

let shared_decl_buckets_by_origin g buckets =
  Int_table.fold
    (fun _ entries acc ->
      List.fold_left (fun acc (_, ids) -> ids :: acc) acc entries)
    buckets []
  |> List.sort (fun left right ->
      match Int.compare (first_origin g left) (first_origin g right) with
      | 0 -> Int.compare (List.length left) (List.length right)
      | order -> order)

let exact_group_key rules common =
  mix_int
    (hash_selector_list (List.map (fun (r : rule) -> r.selector) rules))
    (common |> List.map Declaration.hash |> hash_ints)

let exact_leftovers common rules =
  List.filter_map
    (fun (r : rule) ->
      match remove_common common r.declarations with
      | [] -> Option.None
      | declarations ->
          Option.Some
            { r with declarations; nested = []; merge_key = Option.None })
    rules

let decl_fact_mem fact facts =
  List.exists (fun other -> same_decl fact.decl other.decl) facts

let unsafe_lift_over_leftover_facts group_facts decls =
  let decls = decl_facts decls in
  let rec loop prior_leftovers = function
    | [] -> false
    | fact :: rest ->
        if decl_fact_mem fact group_facts then
          List.exists
            (fun prior -> decl_facts_conflict fact prior)
            prior_leftovers
          || loop prior_leftovers rest
        else loop (fact :: prior_leftovers) rest
  in
  loop [] decls

let unsafe_lift_in_any_member group_decls rules =
  let group_facts = decl_facts group_decls in
  List.exists
    (fun (rule : rule) ->
      unsafe_lift_over_leftover_facts group_facts rule.declarations)
    rules

let decl_mem decl decls = List.exists (same_decl decl) decls

let group_orders_before group_decls left right =
  let rec loop = function
    | [] -> false
    | decl :: rest ->
        if same_decl decl left then true
        else if same_decl decl right then false
        else loop rest
  in
  loop group_decls

let unsafe_group_order_in_member group_decls (rule : rule) =
  let grouped =
    decl_facts rule.declarations
    |> List.filter (fun fact -> decl_mem fact.decl group_decls)
  in
  let rec loop prior = function
    | [] -> false
    | fact :: rest ->
        List.exists
          (fun prior ->
            group_orders_before group_decls fact.decl prior.decl
            && decl_facts_conflict fact prior)
          prior
        || loop (fact :: prior) rest
  in
  loop [] grouped

let unsafe_group_order group_decls rules =
  List.exists (unsafe_group_order_in_member group_decls) rules

let declarations_conflict_with_group_facts group_facts declarations =
  let declarations = decl_facts declarations in
  List.exists
    (fun decl ->
      List.exists (fun group -> decl_facts_conflict decl group) group_facts)
    declarations

let unsafe_cross_member_leftover group_decls rules =
  let group_facts = decl_facts group_decls in
  List.exists
    (fun (rule : rule) ->
      let leftovers = remove_common group_decls rule.declarations in
      declarations_conflict_with_group_facts group_facts leftovers
      && List.exists
           (fun (other : rule) ->
             other != rule
             && selectors_tie_and_overlap rule.selector other.selector)
           rules)
    rules

let unsafe_lift_over_prior_member g group_decls rules_with_ids =
  let group_facts = decl_facts group_decls in
  let rec loop prior = function
    | [] -> false
    | (id, (rule : rule)) :: rest ->
        let unsafe_prior =
          List.exists
            (fun (prior_id, (prior_rule : rule)) ->
              Rule_graph.conflict g prior_id id
              && remove_common group_decls prior_rule.declarations
                 |> declarations_conflict_with_group_facts group_facts)
            prior
        in
        unsafe_prior || loop ((id, rule) :: prior) rest
  in
  loop [] rules_with_ids

let origin_bounds g ids =
  match ids with
  | [] -> Option.None
  | first :: rest ->
      let first = Rule_graph.node_origin g first in
      let min_origin, max_origin =
        List.fold_left
          (fun (min_origin, max_origin) id ->
            let origin = Rule_graph.node_origin g id in
            (min min_origin origin, max max_origin origin))
          (first, first) rest
      in
      Option.Some (min_origin, max_origin)

let id_mem id ids =
  List.exists (fun other -> Rule_graph.Node_id.compare id other = 0) ids

let crosses_bounds g ~min_origin ~max_origin id =
  let origin = Rule_graph.node_origin g id in
  min_origin < origin && origin < max_origin

let decl_facts_shorthand_order_conflict grouped external_decl =
  decl_facts_conflict grouped external_decl
  && not (Declaration.same_property grouped.decl external_decl.decl)

let declarations_cross_order grouped external_decls =
  let grouped = decl_facts grouped in
  let external_decls = decl_facts external_decls in
  List.exists
    (fun grouped ->
      List.exists
        (fun external_decl ->
          decl_facts_shorthand_order_conflict grouped external_decl)
        external_decls)
    grouped

let group_crosses_external_conflict g ~ids (grouped : rule) =
  match origin_bounds g ids with
  | Option.None -> false
  | Option.Some (min_origin, max_origin) ->
      live_rules g
      |> List.exists (fun (external_id, (external_rule : rule)) ->
          (not (id_mem external_id ids))
          && crosses_bounds g ~min_origin ~max_origin external_id
          && selectors_tie_and_overlap grouped.selector external_rule.selector
          && declarations_cross_order grouped.declarations
               external_rule.declarations)

let eligible_ids_from_bucket g ids =
  ids |> ordered_ids g
  |> List.filter (fun id -> rule_eligible (Rule_graph.node_rule g id))

let exact_shared_candidate ?size_cache ~finalize g ~seen common rules_with_ids =
  match cost_aware_exact_members common rules_with_ids with
  | [] | [ _ ] -> Option.None
  | members ->
      let ids = List.map fst members in
      let rules = List.map snd members in
      let key = exact_group_key rules common in
      if Int_table.mem seen key then Option.None
      else begin
        Int_table.replace seen key ();
        match grouped_rule rules with
        | Option.None -> Option.None
        | Option.Some grouped ->
            let grouped = { grouped with declarations = common } in
            if
              unsafe_lift_in_any_member common rules
              || unsafe_group_order common rules
              || unsafe_cross_member_leftover common rules
              || unsafe_lift_over_prior_member g common members
              || group_crosses_external_conflict g ~ids grouped
            then Option.None
            else
              let produce = grouped :: exact_leftovers common rules in
              candidate ?size_cache ~kind:Exact_shared_declarations ~finalize g
                ~consume:ids ~produce
      end

let shared_decl_candidates ?size_cache ?touching ~ctx ~finalize g =
  let touching = touching_set touching in
  let budget = exact_subset_budget ~ctx g in
  let budget_state =
    match budget with
    | Exhaustive _ -> Option.None
    | Indexed budget -> Option.Some (indexed_budget_state budget)
  in
  let buckets = shared_decl_buckets g in
  let seen_groups = Int_table.create 128 in
  let candidates = ref [] in
  shared_decl_buckets_by_origin g buckets
  |> List.iter (fun ids ->
      let ids = ids |> eligible_ids_from_bucket g |> ordered_ids g in
      if touches_any touching ids then
        ids
        |> subset_candidates g ?budget_state budget
        |> List.iter (fun ids ->
            if touches_any touching ids then
              let rules_with_ids = rules_with_ids g ids in
              match common_exact_decls (List.map snd rules_with_ids) with
              | [] -> ()
              | common -> (
                  match
                    exact_shared_candidate ?size_cache ~finalize g
                      ~seen:seen_groups common rules_with_ids
                  with
                  | Option.None -> ()
                  | Option.Some candidate ->
                      candidates := candidate :: !candidates)));
  !candidates

type property_key = { property_name : string; important : bool; hash : int }

let declaration_property_key decl : property_key =
  let property_name = Declaration.property_name decl in
  let important = Declaration.is_important decl in
  {
    property_name;
    important;
    hash = mix_int (hash_string property_name) (hash_bool important);
  }

let property_key_equal left right =
  Bool.equal left.important right.important
  && String.equal left.property_name right.property_name

let property_key_hash key = key.hash

let single_declaration_in_list key decls =
  let rec loop found = function
    | [] -> found
    | decl :: rest ->
        if property_key_equal (declaration_property_key decl) key then
          match found with
          | Option.None -> loop (Option.Some decl) rest
          | Option.Some _ -> Option.None
        else loop found rest
  in
  loop Option.None decls

let single_declaration_for_property key (r : rule) =
  single_declaration_in_list key r.declarations

let property_keys (r : rule) =
  List.fold_left
    (fun keys decl ->
      let key = declaration_property_key decl in
      if List.exists (property_key_equal key) keys then keys else key :: keys)
    [] r.declarations

let rec has_property_key key = function
  | [] -> false
  | decl :: rest ->
      property_key_equal (declaration_property_key decl) key
      || has_property_key key rest

let add_property_buckets buckets id (rule : rule) =
  let add key id =
    let hash = property_key_hash key in
    let entries = Int_table.find_opt buckets hash |> Option.value ~default:[] in
    let rec insert acc = function
      | [] -> List.rev ((key, [ id ]) :: acc)
      | (existing, ids) :: rest when property_key_equal existing key ->
          List.rev_append acc ((existing, id :: ids) :: rest)
      | entry :: rest -> insert (entry :: acc) rest
    in
    Int_table.replace buckets hash (insert [] entries)
  in
  let rec loop seen = function
    | [] -> ()
    | decl :: rest ->
        let key = declaration_property_key decl in
        if List.exists (property_key_equal key) seen then loop seen rest
        else begin
          if not (has_property_key key rest) then add key id;
          loop (key :: seen) rest
        end
  in
  loop [] rule.declarations

let key_mem key keys = List.exists (property_key_equal key) keys

let remove_property_declarations keys decls =
  List.filter
    (fun decl -> not (key_mem (declaration_property_key decl) keys))
    decls

type default_member = { id : Rule_graph.node_id; rule : rule }

type default_entry = {
  key : property_key;
  default_decl : Declaration.declaration;
}

let default_member_rules members = List.map (fun member -> member.rule) members
let default_member_ids members = List.map (fun member -> member.id) members

let collect_default_members g key ids =
  ordered_ids g ids
  |> List.filter_map (fun id ->
      let rule = Rule_graph.node_rule g id in
      match single_declaration_for_property key rule with
      | Option.None -> Option.None
      | Option.Some _ -> Option.Some { id; rule })

let common_property_keys members =
  match members with
  | [] -> []
  | first :: _ ->
      property_keys first.rule
      |> List.filter (fun key ->
          List.for_all
            (fun member ->
              single_declaration_for_property key member.rule <> Option.None)
            members)
      |> List.rev

let default_entries first keys =
  List.filter_map
    (fun key ->
      match single_declaration_for_property key first.rule with
      | Option.None -> Option.None
      | Option.Some default_decl -> Option.Some { key; default_decl })
    keys

let member_declaration entry member =
  single_declaration_for_property entry.key member.rule

let override_declarations entries member =
  List.filter_map
    (fun entry ->
      match member_declaration entry member with
      | Option.None -> Option.None
      | Option.Some decl ->
          if same_decl decl entry.default_decl then Option.None
          else Option.Some decl)
    entries

let member_has_default entry member =
  match member_declaration entry member with
  | Option.None -> false
  | Option.Some decl -> same_decl decl entry.default_decl

let member_overrides_default entry member =
  match member_declaration entry member with
  | Option.None -> false
  | Option.Some decl -> not (same_decl decl entry.default_decl)

(* A default entry places the provider's value at the group position and every
   differing member's value as a later leftover override. For an element that
   matches both an override member [M] and the provider, that inverts the
   cascade whenever [M] is order-constrained to precede the provider: the
   provider's value won originally (it was cascade-later), but the group makes
   [M]'s leftover win. This can arise once same-selector consolidation has moved
   the provider's declaration earlier than [M]'s conflicting one. Drop the entry
   so the property stays ungrouped. *)
let override_precedes_provider g ~provider members entry =
  List.exists
    (fun member ->
      Rule_graph.Node_id.compare member.id provider.id <> 0
      && member_overrides_default entry member
      && Rule_graph.precedes g member.id provider.id)
    members

let needs_default_reassertion entry ~prior member =
  member_has_default entry member
  && List.exists
       (fun prior ->
         member_overrides_default entry prior
         && selectors_tie_and_overlap prior.rule.selector member.rule.selector)
       prior

let entry_needs_later_default_reassertion members entry =
  let rec loop prior = function
    | [] -> false
    | member :: rest ->
        needs_default_reassertion entry ~prior member
        || loop (member :: prior) rest
  in
  loop [] members

let safe_default_entries members entries =
  List.filter
    (fun entry -> not (entry_needs_later_default_reassertion members entry))
    entries

let reassert_default_declarations entries ~prior member =
  List.filter_map
    (fun entry ->
      if needs_default_reassertion entry ~prior member then
        Option.Some entry.default_decl
      else Option.None)
    entries

let removed_default_declarations entries member =
  List.filter_map
    (fun entry ->
      match member_declaration entry member with
      | Option.None -> Option.None
      | Option.Some decl ->
          if same_decl decl entry.default_decl then
            Option.Some entry.default_decl
          else Option.None)
    entries

let exact_common_without keys members =
  common_exact_decls (default_member_rules members)
  |> List.filter (fun decl ->
      not (key_mem (declaration_property_key decl) keys))

let default_group_decls ~exact_common entries =
  append_unique_decls exact_common
    (List.map (fun entry -> entry.default_decl) entries)

let default_member_leftover ~keys ~entries ~exact_common ~prior member =
  member.rule.declarations |> remove_common exact_common
  |> remove_property_declarations keys
  |> fun kept ->
  kept
  @ reassert_default_declarations entries ~prior member
  @ override_declarations entries member

let default_decl_removed_after_reassertion entries ~prior member =
  removed_default_declarations entries member
  |> remove_common (reassert_default_declarations entries ~prior member)

let default_member_group_decls ~exact_common entries member =
  exact_common @ removed_default_declarations entries member

let unsafe_default_prior_lift g ~keys ~entries ~exact_common members =
  let prior_leftover member =
    member.rule.declarations |> remove_common exact_common
    |> remove_property_declarations keys
    |> fun kept -> kept @ override_declarations entries member
  in
  let rec loop prior = function
    | [] -> false
    | member :: rest ->
        let group_decls =
          default_member_group_decls ~exact_common entries member
        in
        let group_facts = decl_facts group_decls in
        let unsafe_prior =
          group_decls <> []
          && List.exists
               (fun prior_member ->
                 Rule_graph.conflict g prior_member.id member.id
                 && declarations_conflict_with_group_facts group_facts
                      (prior_leftover prior_member))
               prior
        in
        unsafe_prior || loop (member :: prior) rest
  in
  loop [] members

let keep_default_member ~keys ~entries ~exact_common ~prior member =
  let leftover =
    default_member_leftover ~keys ~entries ~exact_common ~prior member
  in
  leftover = []
  || selector_size member.rule + 1
     <= decls_inline_cost
          (exact_common
          @ default_decl_removed_after_reassertion entries ~prior member)

let prune_default_members ~keys ~entries ~exact_common members =
  let rec loop prior acc = function
    | [] -> List.rev acc
    | member :: rest ->
        if keep_default_member ~keys ~entries ~exact_common ~prior member then
          loop (member :: prior) (member :: acc) rest
        else loop prior acc rest
  in
  loop [] [] members

let default_group_key group_decls members =
  mix_int
    (hash_selector_list (List.map (fun member -> member.rule.selector) members))
    (group_decls |> List.map Declaration.hash |> hash_ints)

let default_produce g ~keys ~entries ~exact_common ~group_decls members =
  match grouped_rule (default_member_rules members) with
  | Option.None -> Option.None
  | Option.Some grouped ->
      let grouped = { grouped with declarations = group_decls } in
      let ids = default_member_ids members in
      if
        unsafe_lift_in_any_member group_decls (default_member_rules members)
        || unsafe_group_order group_decls (default_member_rules members)
        || unsafe_lift_over_prior_member g exact_common
             (List.map (fun member -> (member.id, member.rule)) members)
        || unsafe_default_prior_lift g ~keys ~entries ~exact_common members
        || group_crosses_external_conflict g ~ids grouped
      then Option.None
      else
        let leftovers =
          let rec loop prior acc = function
            | [] -> List.rev acc
            | member :: rest -> (
                match
                  default_member_leftover ~keys ~entries ~exact_common ~prior
                    member
                with
                | [] -> loop (member :: prior) acc rest
                | declarations ->
                    let leftover =
                      {
                        member.rule with
                        declarations;
                        nested = [];
                        merge_key = Option.None;
                      }
                    in
                    loop (member :: prior) (leftover :: acc) rest)
          in
          loop [] [] members
        in
        Option.Some (grouped :: leftovers)

let default_candidate_from_members ?size_cache ~finalize g ~seen members =
  match members with
  | [] | [ _ ] -> Option.None
  | first :: _ -> (
      let entries =
        common_property_keys members
        |> default_entries first
        |> safe_default_entries members
        |> List.filter (fun entry ->
            not (override_precedes_provider g ~provider:first members entry))
      in
      let keys = List.map (fun entry -> entry.key) entries in
      let exact_common = exact_common_without keys members in
      let group_decls = default_group_decls ~exact_common entries in
      let members =
        prune_default_members ~keys ~entries ~exact_common members
      in
      let group_key = default_group_key group_decls members in
      if Int_table.mem seen group_key then Option.None
      else
        match members with
        | [] | [ _ ] -> Option.None
        | _ -> (
            match
              default_produce g ~keys ~entries ~exact_common ~group_decls
                members
            with
            | Option.None -> Option.None
            | Option.Some produce ->
                Int_table.replace seen group_key ();
                candidate ?size_cache ~kind:Default_factoring ~finalize g
                  ~consume:(default_member_ids members)
                  ~produce))

let default_candidate_for_ids ?size_cache ~finalize g ~seen ids =
  match ids with
  | [] | [ _ ] -> Option.None
  | ids ->
      let members =
        ordered_ids g ids |> unique_ids_preserve_order
        |> List.filter_map (fun id ->
            let rule = Rule_graph.node_rule g id in
            if rule_eligible rule then Option.Some { id; rule } else Option.None)
      in
      default_candidate_from_members ?size_cache ~finalize g ~seen members

let default_value_candidates ?size_cache ?touching ~ctx ~finalize g =
  let touching = touching_set touching in
  let budget = default_subset_budget ~ctx g in
  let budget_state =
    match budget with
    | Exhaustive _ -> Option.None
    | Indexed budget -> Option.Some (indexed_budget_state budget)
  in
  let buckets = Int_table.create 256 in
  List.iter
    (fun (id, rule) ->
      if rule_eligible rule then add_property_buckets buckets id rule)
    (live_rules g);
  let seen_groups = Int_table.create 128 in
  let candidates = ref [] in
  Int_table.fold (fun _ entries acc -> List.rev_append entries acc) buckets []
  |> List.sort (fun (_, left) (_, right) ->
      match Int.compare (first_origin g left) (first_origin g right) with
      | 0 -> Int.compare (List.length left) (List.length right)
      | order -> order)
  |> List.iter (fun (key, ids) ->
      let ids =
        ids |> Node_set.of_list |> Node_set.elements
        |> List.filter (fun id -> rule_eligible (Rule_graph.node_rule g id))
        |> collect_default_members g key
        |> List.map (fun member -> member.id)
      in
      if touches_any touching ids then
        ids
        |> subset_candidates g ?budget_state budget
        |> List.iter (fun ids ->
            if touches_any touching ids then
              match
                default_candidate_for_ids ?size_cache ~finalize g
                  ~seen:seen_groups ids
              with
              | Option.None -> ()
              | Option.Some candidate -> candidates := candidate :: !candidates));
  !candidates

let selector_branches (rule : rule) = Edge.selectors rule.selector

let single_selector_branch_key (rule : rule) : string option =
  match selector_branches rule with
  | [ selector ] -> Option.Some (selector_branch_key selector)
  | _ -> Option.None

let selector_branch_size selector = Pp.size ~minify:true Selector.pp selector
let selector_rank = Rule_graph.Node_id.to_int

let add_single_selector_receivers receivers id rule =
  match single_selector_branch_key rule with
  | Option.None -> ()
  | Option.Some key -> add_string_bucket receivers key id

let remaining_selector_branches ~removed_key selectors =
  List.filter
    (fun selector -> selector_branch_key selector <> removed_key)
    selectors

let rule_for_selector_branches rule selectors : rule option =
  match selectors with
  | [] -> Option.None
  | _ ->
      Option.Some
        {
          rule with
          selector = merge_selector_list selectors;
          nested = [];
          merge_key = Option.None;
        }

let selector_inline_candidate ?size_cache ~finalize g ~receiver_id ~group_id
    ~remaining_selectors =
  let receiver = Rule_graph.node_rule g receiver_id in
  let group = Rule_graph.node_rule g group_id in
  let receiver =
    {
      receiver with
      declarations =
        append_unique_decls receiver.declarations group.declarations;
      nested = [];
      merge_key = Option.None;
    }
  in
  let produce =
    match rule_for_selector_branches group remaining_selectors with
    | Option.None -> [ receiver ]
    | Option.Some group -> [ receiver; group ]
  in
  candidate ?size_cache ~kind:Selector_branch_inline ~finalize g
    ~consume:[ receiver_id; group_id ] ~produce

let single_selector_receivers g =
  let receivers = String_table.create 256 in
  List.iter
    (fun (id, rule) ->
      if rule_eligible rule then add_single_selector_receivers receivers id rule)
    (live_rules g);
  receivers

let seen_key receiver_id group_id branch_key =
  mix_int
    (mix_int
       (Rule_graph.Node_id.to_int receiver_id)
       (Rule_graph.Node_id.to_int group_id))
    (hash_string branch_key)

let add_selector_inline_candidate ?size_cache ?touching ~finalize g ~rank ~seen
    ~candidates ~receiver_id ~group_id ~branch_key ~remaining_selectors =
  if
    Rule_graph.Node_id.compare receiver_id group_id <> 0
    && rank receiver_id < rank group_id
    && (touches_node touching receiver_id || touches_node touching group_id)
  then
    match
      selector_inline_candidate ?size_cache ~finalize g ~receiver_id ~group_id
        ~remaining_selectors
    with
    | Option.None -> ()
    | Option.Some candidate ->
        let key = seen_key receiver_id group_id branch_key in
        if not (Int_table.mem seen key) then begin
          Int_table.replace seen key ();
          candidates := candidate :: !candidates
        end

let selector_branch_inline_for_group ?size_cache ?touching ~finalize g
    ~receivers ~rank ~seen ~candidates group_id group =
  if rule_eligible group && group.declarations <> [] then
    match selector_branches group with
    | [] | [ _ ] -> ()
    | selectors ->
        let inline_cost = decls_inline_cost group.declarations in
        List.iter
          (fun selector ->
            if selector_branch_size selector + 1 > inline_cost then
              let branch_key = selector_branch_key selector in
              let remaining_selectors =
                remaining_selector_branches ~removed_key:branch_key selectors
              in
              String_table.find_opt receivers branch_key
              |> Option.value ~default:[]
              |> List.iter (fun receiver_id ->
                  add_selector_inline_candidate ?size_cache ?touching ~finalize
                    g ~rank ~seen ~candidates ~receiver_id ~group_id ~branch_key
                    ~remaining_selectors))
          selectors

let selector_branch_inline_candidates ?size_cache ?touching ~finalize g =
  let touching = touching_set touching in
  let receivers = single_selector_receivers g in
  let rank = selector_rank in
  let seen_groups = Int_table.create 128 in
  let candidates = ref [] in
  List.iter
    (fun (group_id, group) ->
      selector_branch_inline_for_group ?size_cache ~finalize g ~receivers ~rank
        ~seen:seen_groups ~candidates ?touching group_id group)
    (live_rules g);
  !candidates

let enumerate ?touching ~ctx ~finalize g =
  let size_cache = Rule_rewrite.size_cache g in
  let candidates =
    identical_body_candidates ~size_cache ?touching ~ctx ~finalize g
    @ same_selector_candidates ~size_cache ?touching ~finalize g
    @ selector_branch_inline_candidates ~size_cache ?touching ~finalize g
  in
  candidates
  @ shared_decl_candidates ~size_cache ?touching ~ctx ~finalize g
  @ default_value_candidates ~size_cache ?touching ~ctx ~finalize g
