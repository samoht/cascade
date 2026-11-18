(** Fast local rewrite candidate generation for {!Rule_graph}. *)

open Stylesheet
open Stdlib
open Rule_rewrite
module Node_set = Set.Make (Rule_graph.Node_id)
module Int_set = Set.Make (Int)

(* The graph the search reads, which a rewrite replaces, and the nodes it has
   consumed. *)
type view = { mutable graph : Rule_graph.t; mutable dead : Int_set.t }

let node_rule g id = Rule_graph.node_rule g.graph id
let is_live g id = not (Int_set.mem (Rule_graph.Node_id.to_int id) g.dead)

let view graph =
  let live =
    Int_set.of_list
      (List.map Rule_graph.Node_id.to_int (Rule_graph.live_nodes graph))
  in
  let dead = ref Int_set.empty in
  for i = 0 to Rule_graph.node_count graph - 1 do
    if not (Int_set.mem i live) then dead := Int_set.add i !dead
  done;
  { graph; dead = !dead }

let rewrite_view g graph ~consume =
  g.graph <- graph;
  g.dead <-
    List.fold_left
      (fun dead id -> Int_set.add (Rule_graph.Node_id.to_int id) dead)
      g.dead consume

let same_id a b = Rule_graph.Node_id.compare a b = 0
let same_decl = Declaration.same_minified
let decl_facts = Commute.facts

let declarations_equal (a : Declaration.declaration list)
    (b : Declaration.declaration list) =
  Merge.declarations_equal ~same:same_decl a b

let merge_selector_list = Merge.selector_list
let contains_vendor_pseudo_element = Merge.vendor
let decls_size = Size.decls
let mix_int = Common.mix_int
let hash_string = Common.hash_string
let hash_bool = function false -> 0 | true -> 1
let hash_ints xs = List.fold_left mix_int 0x345678 xs

module Int_table = Common.Table.Make (struct
  type t = int

  let equal = Int.equal
  let hash x = x land max_int
end)

module String_table = Common.Table.Make (struct
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
(* CSS Nesting 1 sec. 3.4 puts a declaration written after a nested rule behind
   it, so a rule's body is not the run of declarations [declarations] holds:
   merging a later same-selector rule into it moves that rule's declarations
   ahead of everything nested. [nested_merge_is_safe] below reads the whole body
   and decides that per property, so a rule carrying nested content stands on
   the same terms as any other. *)
let same_selector_eligible (r : rule) =
  r.merge_key = Option.None
  && (not (contains_vendor_pseudo_element r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let identical_body_eligible ~ctx (r : rule) =
  r.nested = [] && r.merge_key = Option.None
  && (not (contains_vendor_pseudo_element r.selector))
  && (Ctx.extend_lists ctx || not (Selector.is_compound_list r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let selector_branch_key = Rule_graph.selector_branch_key
let selector_key g id = Rule_graph.node_branches g.graph id

let selector_keys_equal left right =
  let rec loop left right =
    match (left, right) with
    | [], [] -> true
    | l :: left, r :: right -> String.equal l r && loop left right
    | _ -> false
  in
  loop left right

(* Any identical-body rules can group into a selector list: a list of disjoint
   selectors, including distinct pseudo-elements ([::before] and [::after] never
   match a common box), is exactly equivalent to the separate rules, and the DAG
   enforces cascade-order safety. *)
let can_group_selectors = Merge.all_compatible
let body_equal_key g id = Rule_graph.declaration_body_key g.graph id
let body_bucket_key g id = body_equal_key g id |> hash_ints
let rules_with_ids g ids = List.map (fun id -> (id, node_rule g id)) ids

(* Each declaration of [r] paired with a later one that is the other half of a
   compatibility fallback. Almost every rule has none, and the scan below only
   reaches the produced rules for a rule that has one. *)
let fallback_pairs (r : rule) =
  let rec walk acc = function
    | [] -> acc
    | decl :: rest ->
        let acc =
          List.fold_left
            (fun acc other ->
              if Webkit_fallback.is_pair decl other then (decl, other) :: acc
              else acc)
            acc rest
        in
        walk acc rest
  in
  if Webkit_fallback.holds_a_prefixed_spelling r.declarations then
    walk [] r.declarations
  else []

(* The prefix synthesis reads one rule at a time and asks only whether THAT rule
   already carries the prefixed spelling. So a rewrite that leaves the two
   halves of a pair in two rules has the missing half written back beside the
   one it kept, even though the group rule above already serves it: the element
   ends up carrying the prefixed declaration twice and the emission is longer
   than the rules the rewrite replaced. Keep the pair in one rule or leave it
   alone. *)
let splits_a_fallback_pair g ~consume ~produce =
  let together (a, b) =
    let holds decl (r : rule) = List.exists (same_decl decl) r.declarations in
    (* A rewrite that keeps neither half has not separated them: the shorthand
       contraction writes both as their shorthands, which is one pair again
       under two other names. Only a half that outlives its partner is a
       split. *)
    (not (List.exists (holds a) produce || List.exists (holds b) produce))
    || List.exists (fun r -> holds a r && holds b r) produce
  in
  List.exists
    (fun id ->
      List.exists
        (fun pair -> not (together pair))
        (fallback_pairs (node_rule g id)))
    consume

let rewrite_candidate ~kind ~finalize g ~consume ~produce =
  if splits_a_fallback_pair g ~consume ~produce then Option.None
  else Rule_rewrite.v ~kind ~finalize g.graph ~consume ~produce

let selector_size g id = Rule_graph.node_selector_size g.graph id
let decls_inline_cost decls = decls_size decls + List.length decls

let grouped_rule (rules : rule list) : rule option =
  match rules with
  | [] -> Option.None
  | first :: _ ->
      let selectors = List.map (fun (r : rule) -> r.selector) rules in
      if not (can_group_selectors selectors) then Option.None
      else
        Option.Some
          {
            first with
            selector = merge_selector_list selectors;
            nested = [];
            merge_key = Option.None;
          }

(* [ids] once each, in the order the graph prints them. *)
let ordered_ids g ids =
  ids |> Node_set.of_list |> Node_set.elements
  |> List.sort (Rule_graph.compare_position g.graph)

let candidate_set_key ids =
  ids |> List.map Rule_graph.Node_id.to_int |> hash_ints

(* A node keeps its rule for as long as it is in the graph, since a rewrite only
   appends nodes, so on every graph one rewrite lineage passes through a list of
   node ids names the same rules. *)
module Ids_table = Hashtbl.Make (struct
  type t = Rule_graph.node_id list

  let equal = List.equal (fun a b -> Rule_graph.Node_id.compare a b = 0)
  let hash = candidate_set_key
end)

(* A candidate worked out on an earlier graph of the lineage, as [g] offers
   it. *)
let offered_on g (candidate : candidate) =
  { candidate with generation = Rule_graph.generation g.graph }

let keep_cost_aware_member g ~removed id (rule : rule) =
  match Lift.remove_common removed rule.declarations with
  | [] -> true
  | _ -> selector_size g id + 1 <= decls_inline_cost removed

let cost_aware_exact_members g common rules_with_ids =
  List.filter
    (fun (id, rule) -> keep_cost_aware_member g ~removed:common id rule)
    rules_with_ids

let same_body_groups g ids =
  let rec insert id = function
    | [] -> [ (id, [ id ]) ]
    | (head, ids) :: rest ->
        let rule = node_rule g id in
        let head_rule = node_rule g head in
        if declarations_equal head_rule.declarations rule.declarations then
          (head, id :: ids) :: rest
        else (head, ids) :: insert id rest
  in
  ids
  |> List.fold_left (fun groups id -> insert id groups) []
  |> List.map (fun (_, ids) -> List.rev ids)

let selector_is_list g id = Selector.is_compound_list (node_rule g id).selector
let has_selector_list g ids = List.exists (selector_is_list g) ids

let non_list_selector_ids g ids =
  List.filter (fun id -> not (selector_is_list g id)) ids

let identical_body_candidate ~finalize g ids =
  match ids with
  | [] | [ _ ] -> Option.None
  | ids -> (
      let rules : rule list = List.map (node_rule g) ids in
      match grouped_rule rules with
      | Option.None -> Option.None
      | Option.Some grouped ->
          rewrite_candidate ~kind:Identical_body ~finalize g ~consume:ids
            ~produce:[ grouped ])

let add_candidate candidates = function
  | Option.None -> ()
  | Option.Some candidate -> candidates := candidate :: !candidates

let better_than candidate = function
  | Option.None -> true
  | Option.Some baseline -> candidate.saving > baseline.saving

let add_identical_body_group ~ctx ~finalize g ~candidates ids =
  if Ctx.extend_lists ctx && has_selector_list g ids then begin
    let strict =
      identical_body_candidate ~finalize g (non_list_selector_ids g ids)
    in
    add_candidate candidates strict;
    match identical_body_candidate ~finalize g ids with
    | Option.Some extended when better_than extended strict ->
        add_candidate candidates (Option.Some extended)
    | _ -> ()
  end
  else add_candidate candidates (identical_body_candidate ~finalize g ids)

(* A hash bucket split into runs of one exact selector key, as
   {!same_body_groups} splits one by body, so two selectors whose hashes collide
   make two groups. *)
let same_selector_groups g ids =
  let rec insert id = function
    | [] -> [ (id, [ id ]) ]
    | (head, ids) :: rest ->
        let key = selector_key g id in
        let head_key = selector_key g head in
        if selector_keys_equal head_key key then (head, id :: ids) :: rest
        else (head, ids) :: insert id rest
  in
  ids
  |> List.fold_left (fun groups id -> insert id groups) []
  |> List.map (fun (_, ids) -> List.rev ids)

let add_same_selector_group ~stats ~finalize g ~candidates ids =
  let source_order = ordered_ids g ids |> rules_with_ids g in
  let safe = Commute.nested_merge_is_safe ~stats (List.map snd source_order) in
  let rules_with_ids = Commute.merge_order source_order in
  let ordered = List.map fst rules_with_ids in
  let rules = List.map snd rules_with_ids in
  match rules with
  | [] | [ _ ] -> ()
  | (first : rule) :: _ when safe -> (
      let merged =
        {
          first with
          declarations =
            List.concat_map (fun (r : rule) -> r.declarations) rules;
          nested = List.concat_map (fun (r : rule) -> r.nested) rules;
          merge_key = Option.None;
        }
      in
      match
        rewrite_candidate ~kind:Same_selector ~finalize g ~consume:ordered
          ~produce:[ merged ]
      with
      | Option.None -> ()
      | Option.Some c -> candidates := c :: !candidates)
  | _ -> ()

(* The first and the last of [ids] in the order the graph prints them. *)
let printed_bounds g ids =
  match ids with
  | [] -> Option.None
  | first :: rest ->
      let compare = Rule_graph.compare_position g.graph in
      Option.Some
        (List.fold_left
           (fun (lo, hi) id ->
             ( (if compare id lo < 0 then id else lo),
               if compare id hi > 0 then id else hi ))
           (first, first) rest)

(* Recursive rather than [List.exists] over a closure: the closure captures
   [id], so it is allocated afresh at every node the external scan walks
   past. *)
let rec id_mem id = function
  | [] -> false
  | other :: rest -> Rule_graph.Node_id.compare id other = 0 || id_mem id rest

let crosses_bounds g ~lo ~hi id =
  Rule_graph.compare_position g.graph lo id < 0
  && Rule_graph.compare_position g.graph id hi < 0

(* Keyed on {!Declaration.prop_key}, the property's typed identity, rather than
   on its name, which exists only once [pp_property] has printed it: this key is
   built for every declaration of every rule the candidate search touches. *)
type property_key = {
  prop : Declaration.prop_key;
  important : bool;
  hash : int;
}

let declaration_property_key decl : property_key =
  let prop = Declaration.property_key decl in
  let important = Declaration.is_important decl in
  {
    prop;
    important;
    hash = mix_int (Declaration.hash_prop_key prop) (hash_bool important);
  }

let property_key_equal left right =
  Bool.equal left.important right.important
  && Declaration.equal_prop_key left.prop right.prop

let property_key_hash key = key.hash

(* A group rule with the candidate it makes, shared by every subset that reaches
   it: [members] are the rules it consumes, and [decls] and [props] what it
   factors out of them, which together decide the candidate. The fields below
   [grouped] are what the candidate read beyond those rules: whether a live rule
   printed strictly between the members [lo] and [hi], the first and the last
   the graph prints, would be crossed, and [witness] is one that would. *)
type group = {
  uid : int;
  members : Rule_graph.node_id list;
  decls : Declaration.declaration list;
  props : property_key list;
  grouped : (rule * candidate option Lazy.t) option Lazy.t;
  mutable refs : int;
  mutable checked : int;
  mutable lo : Rule_graph.node_id;
  mutable hi : Rule_graph.node_id;
  mutable witness : Rule_graph.node_id option;
  mutable made : candidate option;
  mutable group_offer : offer option;
}

and offer = { key : int; candidate : candidate; mutable withdrawn : bool }

(* The groups met so far, by the rules they consume. *)
type shared = { by_members : group list Ids_table.t; mutable next_group : int }

let shared size = { by_members = Ids_table.create size; next_group = 0 }

let share shared ~members ~decls ~props grouped =
  let same group =
    List.equal same_decl group.decls decls
    && List.equal property_key_equal group.props props
  in
  let known =
    match Ids_table.find_opt shared.by_members members with
    | Option.Some known -> known
    | Option.None -> []
  in
  match List.find_opt same known with
  | Option.Some group -> group
  | Option.None ->
      let group =
        {
          uid = shared.next_group;
          members;
          decls;
          props;
          grouped = lazy (grouped ());
          refs = 0;
          checked = -1;
          (* settled by the first scan *)
          lo = Rule_graph.Node_id.of_int_exn 0;
          hi = Rule_graph.Node_id.of_int_exn 0;
          witness = Option.None;
          made = Option.None;
          group_offer = Option.None;
        }
      in
      shared.next_group <- shared.next_group + 1;
      Ids_table.replace shared.by_members members (group :: known);
      group

(* What the rules of a subset make of an exact-shared group, before the one
   question that reads the rest of the graph: whether the group would cross a
   live rule it conflicts with. Each step past the members waits until a
   candidate is asked for. *)
type exact_group = No_exact_group | Exact_group of group

let exact_group ~shared ~finalize g ids =
  let rules_with_ids = rules_with_ids g ids in
  match Lift.common_exact_decls (List.map snd rules_with_ids) with
  | [] -> No_exact_group
  | common -> (
      match cost_aware_exact_members g common rules_with_ids with
      | [] | [ _ ] -> No_exact_group
      | members ->
          let ids = List.map fst members in
          let rules = List.map snd members in
          let grouped () =
            match grouped_rule rules with
            | Option.None -> Option.None
            | Option.Some grouped ->
                let grouped = { grouped with declarations = common } in
                if
                  Lift.unsafe_lift_in_any_member common rules
                  || Lift.unsafe_group_order common rules
                  || Lift.unsafe_cross_member_leftover common rules
                  || Lift.unsafe_lift_over_prior_member
                       ~conflict:(Rule_graph.conflict g.graph)
                       common members
                then Option.None
                else
                  let produce = grouped :: Lift.exact_leftovers common rules in
                  Option.Some
                    ( grouped,
                      lazy
                        (rewrite_candidate ~kind:Exact_shared_declarations
                           ~finalize g ~consume:ids ~produce) )
          in
          Exact_group
            (share shared ~members:ids ~decls:common ~props:[] grouped))

(* [property_key_equal (declaration_property_key decl) key] without building
   [decl]'s key: the scans below ask it of every declaration of every member of
   every candidate group. *)
let declaration_has_key key decl =
  Bool.equal (Declaration.is_important decl) key.important
  && Declaration.equal_prop_key (Declaration.property_key decl) key.prop

let declaration_has_any_key keys decl =
  List.exists (fun key -> declaration_has_key key decl) keys

let single_declaration_in_list key decls =
  let rec loop found = function
    | [] -> found
    | decl :: rest ->
        if declaration_has_key key decl then
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
  | decl :: rest -> declaration_has_key key decl || has_property_key key rest

let remove_property_declarations keys decls =
  List.filter (fun decl -> not (declaration_has_any_key keys decl)) decls

type default_member = { id : Rule_graph.node_id; rule : rule }

type default_entry = {
  key : property_key;
  default_decl : Declaration.declaration;
}

let default_member_rules members = List.map (fun member -> member.rule) members
let default_member_ids members = List.map (fun member -> member.id) members

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
   the provider's declaration earlier than [M]'s conflicting one. The entry is
   dropped, so the property stays ungrouped, once one of the members below
   precedes the provider. *)
let overriders ~provider members entry =
  List.filter_map
    (fun member ->
      if
        Rule_graph.Node_id.compare member.id provider.id <> 0
        && member_overrides_default entry member
      then Option.Some member.id
      else Option.None)
    members

let needs_default_reassertion entry ~prior member =
  member_has_default entry member
  && List.exists
       (fun prior ->
         member_overrides_default entry prior
         && Rule_graph.selectors_conflict prior.rule.selector
              member.rule.selector)
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
  Lift.common_exact_decls (default_member_rules members)
  |> List.filter (fun decl -> not (declaration_has_any_key keys decl))

let default_group_decls ~exact_common entries =
  Lift.append_unique_decls exact_common
    (List.map (fun entry -> entry.default_decl) entries)

let default_member_leftover ~keys ~entries ~exact_common ~prior member =
  member.rule.declarations
  |> Lift.remove_common exact_common
  |> remove_property_declarations keys
  |> fun kept ->
  kept
  @ reassert_default_declarations entries ~prior member
  @ override_declarations entries member

let default_decl_removed_after_reassertion entries ~prior member =
  removed_default_declarations entries member
  |> Lift.remove_common (reassert_default_declarations entries ~prior member)

let default_member_group_decls ~exact_common entries member =
  exact_common @ removed_default_declarations entries member

let unsafe_default_prior_lift g ~keys ~entries ~exact_common members =
  let prior_leftover member =
    member.rule.declarations
    |> Lift.remove_common exact_common
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
                 Rule_graph.conflict g.graph prior_member.id member.id
                 && Lift.declarations_conflict_with_group_facts group_facts
                      (prior_leftover prior_member))
               prior
        in
        unsafe_prior || loop (member :: prior) rest
  in
  loop [] members

let keep_default_member g ~keys ~entries ~exact_common ~prior member =
  let leftover =
    default_member_leftover ~keys ~entries ~exact_common ~prior member
  in
  leftover = []
  || selector_size g member.id + 1
     <= decls_inline_cost
          (exact_common
          @ default_decl_removed_after_reassertion entries ~prior member)

let prune_default_members g ~keys ~entries ~exact_common members =
  let rec loop prior acc = function
    | [] -> List.rev acc
    | member :: rest ->
        if keep_default_member g ~keys ~entries ~exact_common ~prior member then
          loop (member :: prior) (member :: acc) rest
        else loop prior acc rest
  in
  loop [] [] members

(* The group rule and, once asked for, the candidate it makes, unless the
   members' own rules refuse it. Whether it crosses a live rule it conflicts
   with is the caller's question, as that reads the rest of the graph. *)
let default_produce ~finalize g ~keys ~entries ~exact_common ~group_decls
    members =
  match grouped_rule (default_member_rules members) with
  | Option.None -> Option.None
  | Option.Some grouped ->
      let grouped = { grouped with declarations = group_decls } in
      if
        Lift.unsafe_lift_in_any_member group_decls
          (default_member_rules members)
        || Lift.unsafe_group_order group_decls (default_member_rules members)
        || Lift.unsafe_lift_over_prior_member
             ~conflict:(Rule_graph.conflict g.graph)
             exact_common
             (List.map (fun member -> (member.id, member.rule)) members)
        || unsafe_default_prior_lift g ~keys ~entries ~exact_common members
      then Option.None
      else
        let produce () =
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
          grouped :: leftovers
        in
        Option.Some
          ( grouped,
            lazy
              (rewrite_candidate ~kind:Default_factoring ~finalize g
                 ~consume:(default_member_ids members)
                 ~produce:(produce ())) )

(* Pruning keeps a member only two ways: everything it writes disappears into
   the group body, or the body hands it back at least as many bytes as naming
   its selector costs. Both are bounded by the provider's own declarations. The
   body is drawn from them - [exact_common] filters the provider's list and
   every default is the provider's declaration for that key - and every
   declaration the body hands a member is one the member already writes, since
   [exact_common] holds only declarations common to all members and a removed
   default is the member's own value. So a member that writes something the
   provider does not, and whose selector costs more than the provider
   declarations it shares, is pruned whichever entries survive the safety
   filters. When no member past the provider clears that bound the group cannot
   keep two, and it is dropped before the four cross-member scans that would
   otherwise reach the same answer. The bound is one-sided: it lets through
   groups that pruning still rejects, never the reverse, so the candidates are
   the same either way. *)
let member_may_survive_pruning g provider_decls (member : default_member) =
  List.for_all
    (fun decl -> Lift.decl_mem decl provider_decls)
    member.rule.declarations
  || selector_size g member.id + 1
     <= decls_inline_cost
          (List.filter
             (fun decl -> Lift.decl_mem decl member.rule.declarations)
             provider_decls)

let default_group_can_keep_two g provider members =
  List.exists (member_may_survive_pruning g provider.rule.declarations) members

(* Which entries survive reads the graph, whether an overriding member precedes
   the provider, so a subset keeps the group it makes for each set of survivors
   it has met. *)
type default_members =
  | No_default_group
  | Default_members of {
      first : default_member;
      members : default_member list;
      entries : (default_entry * Rule_graph.node_id list) list;
          (** each with the members overriding it *)
      mutable groups :
        ((default_entry * Rule_graph.node_id list) list * group) list;
    }

let default_members g ids =
  let members =
    ordered_ids g ids
    |> List.filter_map (fun id ->
        let rule = node_rule g id in
        if rule_eligible rule then Option.Some { id; rule } else Option.None)
  in
  match members with
  | [] | [ _ ] -> No_default_group
  | first :: rest when not (default_group_can_keep_two g first rest) ->
      No_default_group
  | first :: _ ->
      Default_members
        {
          first;
          members;
          entries =
            common_property_keys members
            |> default_entries first
            |> safe_default_entries members
            |> List.map (fun entry ->
                (entry, overriders ~provider:first members entry));
          groups = [];
        }

(* The pruned members, the keys and the group body decide the candidate: the
   body is the members' common declarations followed by the provider's value for
   each key. *)
let default_group ~shared ~finalize g members entries =
  let keys = List.map (fun entry -> entry.key) entries in
  let exact_common = exact_common_without keys members in
  let group_decls = default_group_decls ~exact_common entries in
  let members = prune_default_members g ~keys ~entries ~exact_common members in
  share shared ~members:(default_member_ids members)
    ~decls:group_decls ~props:keys (fun () ->
      match members with
      | [] | [ _ ] -> Option.None
      | _ ->
          default_produce ~finalize g ~keys ~entries ~exact_common ~group_decls
            members)

(* The group [known] makes on [g] - the one met before for the entries that
   survive here, or a new one - and the order questions that decide those
   entries: whether a member overriding a surviving entry precedes the provider.
   {!Rule_graph.try_rewrite} preserves the partial order, so an entry dropped
   for an override that precedes the provider stays dropped, and these are the
   only questions a later graph can answer differently. *)
let surviving_group ~shared ~finalize g known =
  match known with
  | No_default_group -> Option.None
  | Default_members known ->
      let provider = known.first.id in
      let survivors =
        List.filter
          (fun (_, overriders) ->
            not
              (List.exists
                 (fun member -> Rule_graph.precedes g.graph member provider)
                 overriders))
          known.entries
      in
      let group =
        match
          List.find_opt
            (fun (met, _) -> List.equal ( == ) met survivors)
            known.groups
        with
        | Option.Some (_, group) -> group
        | Option.None ->
            let group =
              default_group ~shared ~finalize g known.members
                (List.map fst survivors)
            in
            known.groups <- (survivors, group) :: known.groups;
            group
      in
      let unordered =
        List.fold_left
          (fun unordered (_, overriders) ->
            List.fold_left
              (fun unordered member ->
                if
                  List.exists (fun (other, _) -> same_id other member) unordered
                then unordered
                else (member, provider) :: unordered)
              unordered overriders)
          [] survivors
      in
      Option.Some (group, unordered)

let selector_branches (rule : rule) = Edge.selectors rule.selector

let single_selector_branch_key (rule : rule) : string option =
  match selector_branches rule with
  | [ selector ] -> Option.Some (selector_branch_key selector)
  | _ -> Option.None

let selector_branch_size = Size.selector

let remaining_selector_branches ~removed_key keyed =
  List.filter_map
    (fun (key, selector) ->
      if String.equal key removed_key then Option.None else Option.Some selector)
    keyed

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

let selector_inline_candidate ~finalize g ~receiver_id ~group_id
    ~remaining_selectors =
  let receiver = node_rule g receiver_id in
  let group = node_rule g group_id in
  let receiver =
    {
      receiver with
      declarations =
        Lift.append_unique_decls receiver.declarations group.declarations;
      nested = [];
      merge_key = Option.None;
    }
  in
  let produce =
    match rule_for_selector_branches group remaining_selectors with
    | Option.None -> [ receiver ]
    | Option.Some group -> [ receiver; group ]
  in
  rewrite_candidate ~kind:Selector_branch_inline ~finalize g
    ~consume:[ receiver_id; group_id ] ~produce

(* The candidates of one graph, kept up to date as the scheduler rewrites it.

   A candidate is decided by a few rules and a few questions about the rest of
   the graph, so the state below files every live node under the keys that
   decide which candidates it can take part in, and every candidate under what
   it read. A rewrite then refiles the nodes it consumed and produced, works out
   again the candidates of the keys they sit under, and asks again only the
   questions whose answer the rewrite can have changed. What comes out is the
   set a full enumeration of the new graph offers: {!enumerate} is that full
   enumeration, run from an empty state. *)

type core = {
  ctx : Ctx.t;
  finalize : rule -> rule;
  view : view;
  mutable epoch : int;
  mutable next_key : int;
  mutable offered : offer list;
  mutable consumed : Rule_graph.node_id list;
  mutable produced : Rule_graph.node_id list;
  mutable moved : Rule_graph.node_id list;
}

let key (offer : offer) = offer.key
let candidate (offer : offer) = offer.candidate

let offer core candidate =
  let key = core.next_key in
  core.next_key <- key + 1;
  let offer =
    { key; candidate = offered_on core.view candidate; withdrawn = false }
  in
  core.offered <- offer :: core.offered;
  offer

let withdraw (offer : offer) = offer.withdrawn <- true
let withdraw_option = Option.iter withdraw
let without id ids = List.filter (fun other -> not (same_id id other)) ids

(* Identical-body and same-selector grouping: the candidates of a bucket read
   its members' rules and nothing else. *)

type grouping = Bodies | Selectors

type group_bucket = {
  group_key : int;
  mutable members : Rule_graph.node_id list;  (** newest first *)
  mutable offers : offer list;
  mutable group_stamp : int;
}

type groups = {
  grouping : grouping;
  group_buckets : group_bucket Int_table.t;
  mutable group_dirty : group_bucket list;
}

let grouping_key core grouping id =
  let rule = node_rule core.view id in
  match grouping with
  | Bodies ->
      if identical_body_eligible ~ctx:core.ctx rule && rule.declarations <> []
      then Option.Some (body_bucket_key core.view id)
      else Option.None
  | Selectors ->
      if same_selector_eligible rule then
        Option.Some (hash_strings (selector_key core.view id))
      else Option.None

let touch_group core groups bucket =
  if bucket.group_stamp <> core.epoch then begin
    bucket.group_stamp <- core.epoch;
    groups.group_dirty <- bucket :: groups.group_dirty
  end

let groups_remove core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      let bucket = Int_table.find groups.group_buckets key in
      bucket.members <- without id bucket.members;
      touch_group core groups bucket

let groups_touch core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      touch_group core groups (Int_table.find groups.group_buckets key)

let groups_add core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      let bucket =
        match Int_table.find_opt groups.group_buckets key with
        | Option.Some bucket -> bucket
        | Option.None ->
            let bucket =
              { group_key = key; members = []; offers = []; group_stamp = -1 }
            in
            Int_table.replace groups.group_buckets key bucket;
            bucket
      in
      bucket.members <- id :: bucket.members;
      touch_group core groups bucket

let group_candidates core grouping ids =
  let g = core.view in
  let finalize = core.finalize in
  let candidates = ref [] in
  (match grouping with
  | Bodies ->
      same_body_groups g ids
      |> List.iter
           (add_identical_body_group ~ctx:core.ctx ~finalize g ~candidates)
  | Selectors ->
      same_selector_groups g ids
      |> List.iter
           (add_same_selector_group ~stats:(Ctx.stats core.ctx) ~finalize g
              ~candidates));
  List.rev !candidates

let settle_groups core groups =
  List.iter
    (fun bucket ->
      List.iter withdraw bucket.offers;
      bucket.offers <-
        (match bucket.members with
        | [] | [ _ ] -> []
        | members ->
            group_candidates core groups.grouping (List.rev members)
            |> List.map (offer core));
      if bucket.members = [] then
        Int_table.remove groups.group_buckets bucket.group_key)
    (List.rev groups.group_dirty);
  groups.group_dirty <- []

(* Selector-branch inlining: a receiver and a group meet on a branch key, and
   are a candidate when the receiver precedes the group. Both carry that branch,
   which the graph pins in source order, so the answer is settled when they meet
   and only their own rewrite changes it. *)

type inline_group = {
  group : Rule_graph.node_id;
  branches : (string * Selector.t list) list;
  mutable pairs : (Rule_graph.node_id * offer) list;  (** by receiver *)
}

type inlining = {
  receivers : Rule_graph.node_id list String_table.t;
  by_branch : inline_group list String_table.t;
  inline_groups : inline_group Int_table.t;
}

let receiver_branch (rule : rule) =
  if rule_eligible rule then single_selector_branch_key rule else Option.None

(* The branch keys [rule] can hand to a receiver, each once, with the selector
   list left behind: a branch longer than the declarations it would carry. *)
let inline_branches (rule : rule) =
  if not (rule_eligible rule && rule.declarations <> []) then []
  else
    match selector_branches rule with
    | [] | [ _ ] -> []
    | selectors ->
        let inline_cost = decls_inline_cost rule.declarations in
        let keyed =
          List.map
            (fun selector -> (selector_branch_key selector, selector))
            selectors
        in
        List.fold_left
          (fun acc (key, selector) ->
            if selector_branch_size selector + 1 > inline_cost then
              if List.exists (fun (other, _) -> String.equal other key) acc then
                acc
              else
                (key, remaining_selector_branches ~removed_key:key keyed) :: acc
            else acc)
          [] keyed
        |> List.rev

let pair_up core group remaining receiver =
  if Rule_graph.precedes core.view.graph receiver group.group then
    match
      selector_inline_candidate ~finalize:core.finalize core.view
        ~receiver_id:receiver ~group_id:group.group
        ~remaining_selectors:remaining
    with
    | Option.None -> ()
    | Option.Some candidate ->
        group.pairs <- (receiver, offer core candidate) :: group.pairs

let table_list table key =
  Option.value ~default:[] (String_table.find_opt table key)

let set_table_list table key = function
  | [] -> String_table.remove table key
  | values -> String_table.replace table key values

let inlining_remove core inlining id =
  let rule = node_rule core.view id in
  (match receiver_branch rule with
  | Option.None -> ()
  | Option.Some key ->
      set_table_list inlining.receivers key
        (without id (table_list inlining.receivers key));
      List.iter
        (fun group ->
          group.pairs <-
            List.filter
              (fun (receiver, offer) ->
                if same_id receiver id then begin
                  withdraw offer;
                  false
                end
                else true)
              group.pairs)
        (table_list inlining.by_branch key));
  let index = Rule_graph.Node_id.to_int id in
  match Int_table.find_opt inlining.inline_groups index with
  | Option.None -> ()
  | Option.Some group ->
      List.iter (fun (_, offer) -> withdraw offer) group.pairs;
      List.iter
        (fun (key, _) ->
          set_table_list inlining.by_branch key
            (List.filter
               (fun other -> other != group)
               (table_list inlining.by_branch key)))
        group.branches;
      Int_table.remove inlining.inline_groups index

let inlining_add core inlining id =
  let rule = node_rule core.view id in
  (match receiver_branch rule with
  | Option.None -> ()
  | Option.Some key ->
      String_table.push inlining.receivers key id;
      List.iter
        (fun group ->
          let remaining =
            snd
              (List.find
                 (fun (other, _) -> String.equal other key)
                 group.branches)
          in
          pair_up core group remaining id)
        (table_list inlining.by_branch key));
  match inline_branches rule with
  | [] -> ()
  | branches ->
      let group = { group = id; branches; pairs = [] } in
      Int_table.replace inlining.inline_groups
        (Rule_graph.Node_id.to_int id)
        group;
      List.iter
        (fun (key, remaining) ->
          String_table.push inlining.by_branch key group;
          List.iter
            (pair_up core group remaining)
            (table_list inlining.receivers key))
        branches

(* Exact-shared and default factoring: a bucket per shared declaration or per
   property, a run of subsets of each bucket's members, and a candidate per
   subset. *)

type factoring = Exact | Default

type factoring_key =
  | Shared of Declaration.declaration
  | Property of property_key

let factoring_key_equal a b =
  match (a, b) with
  | Shared a, Shared b -> same_decl a b
  | Property a, Property b -> property_key_equal a b
  | Shared _, Property _ | Property _, Shared _ -> false

let factoring_key_hash = function
  | Shared decl -> Declaration.hash decl
  | Property key -> property_key_hash key

(* The keys [rule] is filed under, each with the position of its declaration:
   every declaration once for exact sharing, and every property written exactly
   once for default factoring. *)
let factoring_keys factoring (rule : rule) =
  if not (rule_eligible rule) then []
  else
    match factoring with
    | Exact ->
        List.mapi
          (fun i decl -> (Shared decl, i))
          (Lift.unique_decls rule.declarations)
    | Default ->
        let rec loop position seen acc = function
          | [] -> List.rev acc
          | decl :: rest ->
              let key = declaration_property_key decl in
              if List.exists (property_key_equal key) seen then
                loop (position + 1) seen acc rest
              else
                let acc =
                  if has_property_key key rest then acc
                  else (Property key, position) :: acc
                in
                loop (position + 1) (key :: seen) acc rest
        in
        loop 0 [] [] rule.declarations

type answer = Exact_answer of exact_group | Default_answer of default_members

(* A subset of a bucket's members and the group it reaches on the current graph.
   An exact-shared subset reaches one group on every graph; a default one
   reaches the group of the entries that survive, which [unordered] decides. *)
type subset = {
  ids : Rule_graph.node_id list;
  uid : int;
  answer : answer Lazy.t;
  mutable taken_by : int;
  mutable resolved : int;
  mutable reached : group option;
  mutable unordered : (Rule_graph.node_id * Rule_graph.node_id) list;
}

(* Where a bucket stands in the order an indexed budget is spent in: the slot of
   its first printed member, its size, then where its key first appears, by node
   and declaration position. *)
type rank = { least : int; size : int; first : int; position : int }

let compare_rank a b =
  match Int.compare a.least b.least with
  | 0 -> (
      match Int.compare a.size b.size with
      | 0 -> (
          match Int.compare a.first b.first with
          | 0 -> Int.compare a.position b.position
          | order -> order)
      | order -> order)
  | order -> order

type factoring_bucket = {
  fkey : factoring_key;
  mutable fmembers : (Rule_graph.node_id * int) list;
      (** with the key's position, the last printed first *)
  mutable subsets : Rule_graph.node_id list list;
  mutable taken : subset list;
  mutable taken_count : int;
  mutable want : int;
  mutable rank : rank;
  mutable fstamp : int;
  mutable asked : int;
}

module Ranked = Set.Make (struct
  type t = factoring_bucket

  let compare a b = compare_rank a.rank b.rank
end)

type family = {
  factoring : factoring;
  budget : Subsets.t;
  fbuckets : factoring_bucket list Int_table.t;
  mutable ranked : Ranked.t;
  mutable takers : factoring_bucket list;
  mutable fdirty : factoring_bucket list;
  known : subset Ids_table.t;
  shared : shared;
  watched : subset Int_table.t;
  live_groups : group Int_table.t;
  mutable next_subset : int;
}

let compare_member g (a, _) (b, _) = Rule_graph.compare_position g.graph a b

let rec insert_member g member = function
  | [] -> [ member ]
  | other :: rest as members ->
      if compare_member g member other > 0 then member :: members
      else other :: insert_member g member rest

let bucket_rank g members =
  let least, first, position =
    List.fold_left
      (fun (least, first, position) (id, at) ->
        let index = Rule_graph.Node_id.to_int id in
        let least = Int.min least (Rule_graph.node_slot g.graph id) in
        if index < first then (least, index, at) else (least, first, position))
      (max_int, max_int, 0) members
  in
  { least; size = List.length members; first; position }

let touch_bucket core family bucket =
  if bucket.fstamp <> core.epoch then begin
    bucket.fstamp <- core.epoch;
    family.fdirty <- bucket :: family.fdirty
  end

let bucket_for family key =
  Int_table.find_opt family.fbuckets (factoring_key_hash key)
  |> Option.value ~default:[]
  |> List.find_opt (fun bucket -> factoring_key_equal bucket.fkey key)

let family_remove core family id =
  List.iter
    (fun (key, _) ->
      match bucket_for family key with
      | Option.None -> ()
      | Option.Some bucket ->
          bucket.fmembers <-
            List.filter
              (fun (other, _) -> not (same_id id other))
              bucket.fmembers;
          touch_bucket core family bucket)
    (factoring_keys family.factoring (node_rule core.view id))

let empty_rank = { least = 0; size = 0; first = 0; position = 0 }

let family_add core family id =
  List.iter
    (fun (key, position) ->
      let bucket =
        match bucket_for family key with
        | Option.Some bucket -> bucket
        | Option.None ->
            let bucket =
              {
                fkey = key;
                fmembers = [];
                subsets = [];
                taken = [];
                taken_count = 0;
                want = 0;
                rank = empty_rank;
                fstamp = -1;
                asked = -1;
              }
            in
            Int_table.push family.fbuckets (factoring_key_hash key) bucket;
            bucket
      in
      bucket.fmembers <- insert_member core.view (id, position) bucket.fmembers;
      touch_bucket core family bucket)
    (factoring_keys family.factoring (node_rule core.view id))

let indexed family = Option.is_some (Subsets.shared family.budget)

let drop_bucket family bucket =
  let hash = factoring_key_hash bucket.fkey in
  match
    List.filter
      (fun other -> other != bucket)
      (Int_table.find family.fbuckets hash)
  with
  | [] -> Int_table.remove family.fbuckets hash
  | buckets -> Int_table.replace family.fbuckets hash buckets

let refresh_bucket core family bucket =
  if indexed family && bucket.subsets <> [] then
    family.ranked <- Ranked.remove bucket family.ranked;
  bucket.subsets <-
    (match bucket.fmembers with
    | [] | [ _ ] -> []
    | members ->
        Subsets.of_bucket family.budget
          ~slot:(Rule_graph.node_slot core.view.graph)
          (List.rev_map fst members));
  bucket.rank <- bucket_rank core.view bucket.fmembers;
  if bucket.fmembers = [] then drop_bucket family bucket;
  if indexed family && bucket.subsets <> [] then
    family.ranked <- Ranked.add bucket family.ranked

let subset_of core family ids =
  match Ids_table.find_opt family.known ids with
  | Option.Some subset -> subset
  | Option.None ->
      let g = core.view in
      let finalize = core.finalize and shared = family.shared in
      let answer =
        match family.factoring with
        | Exact -> lazy (Exact_answer (exact_group ~shared ~finalize g ids))
        | Default -> lazy (Default_answer (default_members g ids))
      in
      let subset =
        {
          ids;
          uid = family.next_subset;
          answer;
          taken_by = 0;
          resolved = -1;
          reached = Option.None;
          unordered = [];
        }
      in
      family.next_subset <- family.next_subset + 1;
      Ids_table.replace family.known ids subset;
      subset

let all_live core ids = List.for_all (is_live core.view) ids

(* Whether [group] would cross the live rule [id], printed between its first and
   last member: [id] is not a member, matches with the group and writes across
   its declarations' order. *)
let crossed core (group : group) (grouped : rule) id =
  (not (id_mem id group.members))
  &&
  let external_rule : rule = node_rule core.view id in
  Rule_graph.selectors_conflict grouped.selector external_rule.selector
  && Lift.declarations_cross_order grouped.declarations
       external_rule.declarations

(* Whether [group] would cross the live rule [id], wherever it prints. *)
let crosses core (group : group) (grouped : rule) id =
  crosses_bounds core.view ~lo:group.lo ~hi:group.hi id
  && crossed core group grouped id

let scan core (group : group) =
  group.checked <- core.epoch;
  group.witness <- Option.None;
  match (Lazy.force group.grouped, printed_bounds core.view group.members) with
  | Option.None, _ | _, Option.None -> ()
  | Option.Some (grouped, _), Option.Some (lo, hi) ->
      group.lo <- lo;
      group.hi <- hi;
      let witness = ref Option.None in
      let found id =
        witness := Option.Some id;
        true
      in
      if
        Rule_graph.exists_live_between core.view.graph lo hi (fun id ->
            crossed core group grouped id && found id)
      then group.witness <- !witness

(* A candidate is offered again only when it changed: a group computes its
   candidate once, so an unchanged one is the same value. *)
let offer_group core (group : group) =
  let made =
    match (group.witness, Lazy.force group.grouped) with
    | Option.None, Option.Some (_, scored) -> Lazy.force scored
    | Option.Some _, _ | _, Option.None -> Option.None
  in
  if not (Option.equal ( == ) made group.made) then begin
    withdraw_option group.group_offer;
    group.made <- made;
    group.group_offer <- Option.map (offer core) made
  end

let forget_group family (group : group) =
  match Ids_table.find_opt family.shared.by_members group.members with
  | Option.None -> ()
  | Option.Some groups -> (
      match List.filter (fun other -> other != group) groups with
      | [] -> Ids_table.remove family.shared.by_members group.members
      | groups ->
          Ids_table.replace family.shared.by_members group.members groups)

let reach core family (group : group) =
  group.refs <- group.refs + 1;
  if group.refs = 1 then begin
    Int_table.replace family.live_groups group.uid group;
    scan core group;
    offer_group core group
  end

let unreach core family (group : group) =
  group.refs <- group.refs - 1;
  if group.refs = 0 then begin
    withdraw_option group.group_offer;
    group.group_offer <- Option.None;
    group.made <- Option.None;
    Int_table.remove family.live_groups group.uid;
    if not (all_live core group.members) then forget_group family group
  end

(* A live group is asked again only about what the rewrites since the last
   refresh changed: a witness they consumed or moved, a member they moved, or a
   node they produced or moved into the span. Every live group is asked at every
   refresh, so the answer it holds is the previous refresh's. *)
let recheck_group core ~consumed ~produced ~moved ~is_moved (group : group) =
  if group.checked <> core.epoch then begin
    group.checked <- core.epoch;
    let rescan () =
      scan core group;
      offer_group core group
    in
    match (Lazy.force group.grouped, group.witness) with
    | Option.None, _ -> ()
    | Option.Some _, _ when List.exists is_moved group.members -> rescan ()
    | Option.Some _, Option.Some witness ->
        if id_mem witness consumed || is_moved witness then rescan ()
    | Option.Some (grouped, _), Option.None -> (
        match List.find_opt (crosses core group grouped) (produced @ moved) with
        | Option.None -> ()
        | Option.Some id ->
            group.witness <- Option.Some id;
            offer_group core group)
  end

let resolve core family subset =
  subset.resolved <- core.epoch;
  subset.unordered <- [];
  let group =
    match Lazy.force subset.answer with
    | Exact_answer No_exact_group -> Option.None
    | Exact_answer (Exact_group group) -> Option.Some group
    | Default_answer known -> (
        match
          surviving_group ~shared:family.shared ~finalize:core.finalize
            core.view known
        with
        | Option.None -> Option.None
        | Option.Some (group, unordered) ->
            subset.unordered <- unordered;
            Option.Some group)
  in
  (match subset.unordered with
  | [] -> Int_table.remove family.watched subset.uid
  | _ -> Int_table.replace family.watched subset.uid subset);
  group

(* Point [subset] at the group it reaches now, reaching the new one before
   leaving the old so a group both name stays live. *)
let reresolve core family subset =
  let previous = subset.reached in
  let group = resolve core family subset in
  if not (Option.equal ( == ) group previous) then begin
    Option.iter (reach core family) group;
    Option.iter (unreach core family) previous;
    subset.reached <- group
  end

let release core family subset =
  subset.taken_by <- subset.taken_by - 1;
  if subset.taken_by = 0 then begin
    Option.iter (unreach core family) subset.reached;
    subset.reached <- Option.None;
    Int_table.remove family.watched subset.uid;
    if not (all_live core subset.ids) then
      Ids_table.remove family.known subset.ids
  end

let rec take n = function
  | [] -> []
  | x :: rest -> if n <= 0 then [] else x :: take (n - 1) rest

(* Point [bucket] at its first [n] subsets. A subset some bucket takes reaches
   its group; one no bucket takes reaches none. *)
let retake core family ~activated ~released bucket n =
  let subsets = List.map (subset_of core family) (take n bucket.subsets) in
  List.iter
    (fun subset ->
      subset.taken_by <- subset.taken_by + 1;
      if subset.taken_by = 1 then activated := subset :: !activated)
    subsets;
  released := List.rev_append bucket.taken !released;
  bucket.taken <- subsets;
  bucket.taken_count <- n

(* An indexed budget is spent on the buckets in rank order, so every bucket that
   took subsets before, or might take some now, is asked again; an exhaustive
   one leaves every bucket all of its subsets. *)
let wanted family dirty =
  match Subsets.shared family.budget with
  | Option.None ->
      List.iter (fun bucket -> bucket.want <- List.length bucket.subsets) dirty;
      dirty
  | Option.Some max_candidates ->
      let previous = family.takers in
      List.iter (fun bucket -> bucket.want <- 0) previous;
      List.iter (fun bucket -> bucket.want <- 0) dirty;
      let rec spend remaining takers seq =
        if remaining <= 0 then takers
        else
          match seq () with
          | Seq.Nil -> takers
          | Seq.Cons (bucket, rest) ->
              let n = Int.min remaining (List.length bucket.subsets) in
              bucket.want <- n;
              spend (remaining - n) (bucket :: takers) rest
      in
      let takers = spend max_candidates [] (Ranked.to_seq family.ranked) in
      family.takers <- takers;
      List.rev_append takers (List.rev_append previous dirty)

let settle_family core family ~consumed ~produced ~moved =
  let dirty = List.rev family.fdirty in
  family.fdirty <- [];
  List.iter (refresh_bucket core family) dirty;
  let activated = ref [] and released = ref [] in
  List.iter
    (fun bucket ->
      if bucket.asked <> core.epoch then begin
        bucket.asked <- core.epoch;
        if bucket.fstamp = core.epoch || bucket.want <> bucket.taken_count then
          retake core family ~activated ~released bucket bucket.want
      end)
    (wanted family dirty);
  List.iter (reresolve core family) (List.rev !activated);
  List.iter (release core family) !released;
  if consumed <> [] then begin
    Int_table.fold
      (fun _ subset acc ->
        if
          subset.resolved <> core.epoch
          && List.exists
               (fun (before, after) ->
                 Rule_graph.precedes core.view.graph before after)
               subset.unordered
        then subset :: acc
        else acc)
      family.watched []
    |> List.iter (reresolve core family);
    let moved_set = Node_set.of_list moved in
    let is_moved id = Node_set.mem id moved_set in
    Int_table.fold (fun _ group acc -> group :: acc) family.live_groups []
    |> List.iter (recheck_group core ~consumed ~produced ~moved ~is_moved)
  end

(* The whole state. *)

type t = {
  core : core;
  bodies : groups;
  selectors : groups;
  inlining : inlining;
  exact : family;
  default : family;
}

let groups size grouping =
  { grouping; group_buckets = Int_table.create size; group_dirty = [] }

let family size factoring budget =
  {
    factoring;
    budget;
    fbuckets = Int_table.create size;
    ranked = Ranked.empty;
    takers = [];
    fdirty = [];
    known = Ids_table.create size;
    shared = shared size;
    watched = Int_table.create 8;
    live_groups = Int_table.create size;
    next_subset = 0;
  }

let remove_node t id =
  groups_remove t.core t.bodies id;
  groups_remove t.core t.selectors id;
  inlining_remove t.core t.inlining id;
  family_remove t.core t.exact id;
  family_remove t.core t.default id

let add_node t id =
  groups_add t.core t.bodies id;
  groups_add t.core t.selectors id;
  inlining_add t.core t.inlining id;
  family_add t.core t.exact id;
  family_add t.core t.default id

(* The nodes a rewrite moved keep their rules and keys. A factoring bucket holds
   its members in printed order, so a bucket holding one is sorted again, and
   asked again when its order changed or, for an indexed budget, which reads
   where its members print, whatever changed; a grouping bucket holds them in
   creation order, and its candidates read the printed order afresh. *)
let reorder_family core family moved =
  let buckets = ref [] in
  List.iter
    (fun id ->
      List.iter
        (fun (key, _) ->
          match bucket_for family key with
          | Option.Some bucket when not (List.memq bucket !buckets) ->
              buckets := bucket :: !buckets
          | _ -> ())
        (factoring_keys family.factoring (node_rule core.view id)))
    moved;
  List.iter
    (fun bucket ->
      let sorted =
        List.stable_sort
          (fun a b -> compare_member core.view b a)
          bucket.fmembers
      in
      let reordered =
        not
          (List.equal (fun (a, _) (b, _) -> same_id a b) sorted bucket.fmembers)
      in
      bucket.fmembers <- sorted;
      if reordered || indexed family then touch_bucket core family bucket)
    !buckets

let reorder_moved t moved =
  List.iter (groups_touch t.core t.selectors) moved;
  reorder_family t.core t.exact moved;
  reorder_family t.core t.default moved

let settle t ~consumed ~produced ~moved =
  settle_groups t.core t.bodies;
  settle_groups t.core t.selectors;
  settle_family t.core t.exact ~consumed ~produced ~moved;
  settle_family t.core t.default ~consumed ~produced ~moved

module Rule_table = Hashtbl.Make (struct
  type t = rule

  let equal a b = Stylesheet.equal_statement (Rule a) (Rule b)
  let hash = Stylesheet.rule_hash
end)

(* [finalize] is a pure normalisation, and the candidates of one run hand it the
   same leftover rules over and over: every subset that factors the same
   declarations out of a member leaves it the same rule. *)
let remembered size finalize =
  let known = Rule_table.create size in
  fun rule ->
    match Rule_table.find_opt known rule with
    | Option.Some finalized -> finalized
    | Option.None ->
        let finalized = finalize rule in
        Rule_table.replace known rule finalized;
        finalized

let v ?nodes ~ctx ~finalize g =
  (* Tables start at the run's size: most runs hold a handful of rules. *)
  let size = Rule_graph.node_count g in
  let finalize = remembered size finalize in
  let nodes = Option.value nodes ~default:size in
  let view = view g in
  let core =
    {
      ctx;
      finalize;
      view;
      epoch = 0;
      next_key = 0;
      offered = [];
      consumed = [];
      produced = [];
      moved = [];
    }
  in
  let t =
    {
      core;
      bodies = groups size Bodies;
      selectors = groups size Selectors;
      inlining =
        {
          receivers = String_table.create size;
          by_branch = String_table.create 8;
          inline_groups = Int_table.create 8;
        };
      exact =
        family size Exact (Subsets.exact ~aggressive:(Ctx.aggressive ctx) nodes);
      default =
        family size Default
          (Subsets.default ~aggressive:(Ctx.aggressive ctx) nodes);
    }
  in
  List.iter (add_node t) (Rule_graph.live_nodes g);
  t

let refresh t =
  let core = t.core in
  if core.epoch > 0 && core.consumed = [] then []
  else
    let consumed = core.consumed in
    let produced = List.filter (is_live core.view) core.produced in
    let moved = List.filter (is_live core.view) core.moved in
    settle t ~consumed ~produced ~moved;
    core.epoch <- core.epoch + 1;
    core.consumed <- [];
    core.produced <- [];
    core.moved <- [];
    let offered =
      List.rev
        (List.filter (fun (offer : offer) -> not offer.withdrawn) core.offered)
    in
    core.offered <- [];
    offered

let start ~ctx ~finalize g = v ~ctx ~finalize g

let rewrite t graph ~consume =
  let core = t.core in
  let first = Rule_graph.node_count core.view.graph in
  rewrite_view core.view graph ~consume;
  let produced =
    List.init
      (Rule_graph.node_count graph - first)
      (fun offset -> Rule_graph.Node_id.of_int_exn (first + offset))
  in
  List.iter (remove_node t) consume;
  List.iter (add_node t) produced;
  let moved = Rule_graph.moved graph in
  if moved <> [] then reorder_moved t moved;
  core.consumed <- List.rev_append consume core.consumed;
  core.produced <- List.rev_append produced core.produced;
  core.moved <- List.rev_append moved core.moved

let offers t =
  let acc = ref [] in
  let add = function
    | Option.None -> ()
    | Option.Some offer -> acc := offer :: !acc
  in
  let groups groups =
    Int_table.iter
      (fun _ bucket ->
        List.iter (fun offer -> add (Option.Some offer)) bucket.offers)
      groups.group_buckets
  in
  groups t.bodies;
  groups t.selectors;
  Int_table.iter
    (fun _ group ->
      List.iter (fun (_, offer) -> add (Option.Some offer)) group.pairs)
    t.inlining.inline_groups;
  let family family =
    Int_table.iter (fun _ group -> add group.group_offer) family.live_groups
  in
  family t.exact;
  family t.default;
  List.sort (fun (a : offer) (b : offer) -> Int.compare a.key b.key) !acc

let is_live t id = is_live t.core.view id
let candidates t = List.map (fun (offer : offer) -> offer.candidate) (offers t)

let enumerate ?nodes ~ctx ~finalize g =
  let t = v ?nodes ~ctx ~finalize g in
  ignore (refresh t);
  candidates t
