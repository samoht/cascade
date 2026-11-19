open Stylesheet
open Stdlib
open Rule_rewrite
open Candidate_kind
module Node_set = Set.Make (Rule_graph.Node_id)

let decl_facts = Commute.facts

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

let keep_cost_aware_member g ~removed id (rule : rule) =
  match Lift.remove_common removed rule.declarations with
  | [] -> true
  | _ -> selector_size g id + 1 <= decls_inline_cost removed

let cost_aware_exact_members g common rules_with_ids =
  List.filter
    (fun (id, rule) -> keep_cost_aware_member g ~removed:common id rule)
    rules_with_ids

(* The first and the last of [ids] in the order the graph prints them. *)
let printed_bounds g ids =
  match ids with
  | [] -> Option.None
  | first :: rest ->
      let compare = Rule_graph.compare_position (graph g) in
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
  Rule_graph.compare_position (graph g) lo id < 0
  && Rule_graph.compare_position (graph g) id hi < 0

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
  props : Property_key.t list;
  grouped : (rule * candidate option Lazy.t) option Lazy.t;
  mutable refs : int;
  mutable checked : int;
  mutable lo : Rule_graph.node_id;
  mutable hi : Rule_graph.node_id;
  mutable witness : Rule_graph.node_id option;
  mutable made : candidate option;
  mutable group_offer : offer option;
}

(* The groups met so far, by the rules they consume. *)
type shared = { by_members : group list Ids_table.t; mutable next_group : int }

let shared size = { by_members = Ids_table.create size; next_group = 0 }

let share shared ~members ~decls ~props grouped =
  let same group =
    List.equal same_decl group.decls decls
    && List.equal Property_key.equal group.props props
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
                       ~conflict:(Rule_graph.conflict (graph g))
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

type default_member = { id : Rule_graph.node_id; rule : rule }

type default_entry = {
  key : Property_key.t;
  default_decl : Declaration.declaration;
}

let default_member_rules members = List.map (fun member -> member.rule) members
let default_member_ids members = List.map (fun member -> member.id) members

let common_property_keys members =
  match members with
  | [] -> []
  | first :: _ ->
      Property_key.of_declarations first.rule.declarations
      |> List.filter (fun key ->
          List.for_all
            (fun member ->
              Property_key.single key member.rule.declarations <> Option.None)
            members)
      |> List.rev

let default_entries first keys =
  List.filter_map
    (fun key ->
      match Property_key.single key first.rule.declarations with
      | Option.None -> Option.None
      | Option.Some default_decl -> Option.Some { key; default_decl })
    keys

let member_declaration entry member =
  Property_key.single entry.key member.rule.declarations

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
  |> List.filter (fun decl -> not (Property_key.has_any keys decl))

let default_group_decls ~exact_common entries =
  Lift.append_unique_decls exact_common
    (List.map (fun entry -> entry.default_decl) entries)

let default_member_leftover ~keys ~entries ~exact_common ~prior member =
  member.rule.declarations
  |> Lift.remove_common exact_common
  |> Property_key.remove keys
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
    |> Property_key.remove keys
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
                 Rule_graph.conflict (graph g) prior_member.id member.id
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
             ~conflict:(Rule_graph.conflict (graph g))
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
let surviving_group ?(precedes = fun g -> Rule_graph.precedes (graph g)) ~shared
    ~finalize g known =
  match known with
  | No_default_group -> Option.None
  | Default_members known ->
      let provider = known.first.id in
      let survivors =
        List.filter
          (fun (_, overriders) ->
            not
              (List.exists
                 (fun member -> precedes g member provider)
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

type factoring = Exact | Default

type factoring_key =
  | Shared of Declaration.declaration
  | Property of Property_key.t

let factoring_key_equal a b =
  match (a, b) with
  | Shared a, Shared b -> same_decl a b
  | Property a, Property b -> Property_key.equal a b
  | Shared _, Property _ | Property _, Shared _ -> false

let factoring_key_hash = function
  | Shared decl -> Declaration.hash decl
  | Property key -> Property_key.hash key

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
              let key = Property_key.of_declaration decl in
              if List.exists (Property_key.equal key) seen then
                loop (position + 1) seen acc rest
              else
                let acc =
                  if Property_key.written key rest then acc
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

type t = {
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

let compare_member g (a, _) (b, _) = Rule_graph.compare_position (graph g) a b

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
        let least = Int.min least (Rule_graph.node_slot (graph g) id) in
        if index < first then (least, index, at) else (least, first, position))
      (max_int, max_int, 0) members
  in
  { least; size = List.length members; first; position }

let touch_bucket core family bucket =
  if bucket.fstamp <> epoch core then begin
    bucket.fstamp <- epoch core;
    family.fdirty <- bucket :: family.fdirty
  end

let bucket_for family key =
  Int_table.find_opt family.fbuckets (factoring_key_hash key)
  |> Option.value ~default:[]
  |> List.find_opt (fun bucket -> factoring_key_equal bucket.fkey key)

let remove core family id =
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
    (factoring_keys family.factoring (node_rule (view core) id))

let empty_rank = { least = 0; size = 0; first = 0; position = 0 }

let add core family id =
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
      bucket.fmembers <-
        insert_member (view core) (id, position) bucket.fmembers;
      touch_bucket core family bucket)
    (factoring_keys family.factoring (node_rule (view core) id))

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
          ~slot:(Rule_graph.node_slot (graph (view core)))
          (List.rev_map fst members));
  bucket.rank <- bucket_rank (view core) bucket.fmembers;
  if bucket.fmembers = [] then drop_bucket family bucket;
  if indexed family && bucket.subsets <> [] then
    family.ranked <- Ranked.add bucket family.ranked

let subset_of core family ids =
  match Ids_table.find_opt family.known ids with
  | Option.Some subset -> subset
  | Option.None ->
      let g = view core in
      let finalize = finalize core and shared = family.shared in
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

let all_live core ids = List.for_all (is_live (view core)) ids

(* Whether [group] would cross the live rule [id], printed between its first and
   last member: [id] is not a member, matches with the group and writes across
   its declarations' order. *)
let crossed core (group : group) branches (grouped : rule) id =
  (not (id_mem id group.members))
  && Rule_graph.node_selector_conflict
       (graph (view core))
       (Lazy.force branches) id
  && Lift.declarations_cross_order grouped.declarations
       (node_rule (view core) id).declarations

(* Whether [group] would cross the live rule [id], wherever it prints. *)
let crosses core (group : group) branches (grouped : rule) id =
  crosses_bounds (view core) ~lo:group.lo ~hi:group.hi id
  && crossed core group branches grouped id

let scan core (group : group) =
  group.checked <- epoch core;
  group.witness <- Option.None;
  match
    (Lazy.force group.grouped, printed_bounds (view core) group.members)
  with
  | Option.None, _ | _, Option.None -> ()
  | Option.Some (grouped, _), Option.Some (lo, hi) ->
      group.lo <- lo;
      group.hi <- hi;
      let witness = ref Option.None in
      let found id =
        witness := Option.Some id;
        true
      in
      let branches = lazy (Rule_graph.branches grouped.selector) in
      if
        Rule_graph.exists_live_between
          (graph (view core))
          lo hi
          (fun id -> crossed core group branches grouped id && found id)
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
  if group.checked <> epoch core then begin
    group.checked <- epoch core;
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
        let branches = lazy (Rule_graph.branches grouped.selector) in
        match
          List.find_opt (crosses core group branches grouped) (produced @ moved)
        with
        | Option.None -> ()
        | Option.Some id ->
            group.witness <- Option.Some id;
            offer_group core group)
  end

let resolve core family subset =
  subset.resolved <- epoch core;
  subset.unordered <- [];
  let group =
    match Lazy.force subset.answer with
    | Exact_answer No_exact_group -> Option.None
    | Exact_answer (Exact_group group) -> Option.Some group
    | Default_answer known -> (
        match
          surviving_group ~shared:family.shared ~finalize:(finalize core)
            (view core) known
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

let settle core family { consumed; produced; moved } =
  let dirty = List.rev family.fdirty in
  family.fdirty <- [];
  List.iter (refresh_bucket core family) dirty;
  let activated = ref [] and released = ref [] in
  List.iter
    (fun bucket ->
      if bucket.asked <> epoch core then begin
        bucket.asked <- epoch core;
        if bucket.fstamp = epoch core || bucket.want <> bucket.taken_count then
          retake core family ~activated ~released bucket bucket.want
      end)
    (wanted family dirty);
  List.iter (reresolve core family) (List.rev !activated);
  List.iter (release core family) !released;
  if consumed <> [] then begin
    Int_table.fold
      (fun _ subset acc ->
        if
          subset.resolved <> epoch core
          && List.exists
               (fun (before, after) ->
                 Rule_graph.precedes (graph (view core)) before after)
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

let v size factoring budget =
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

(* The nodes a rewrite moved keep their rules and keys. A bucket holds its
   members in printed order, so a bucket holding one is sorted again, and asked
   again when its order changed or, for an indexed budget, which reads where its
   members print, whatever changed. *)
let reorder core family moved =
  let buckets = ref [] in
  List.iter
    (fun id ->
      List.iter
        (fun (key, _) ->
          match bucket_for family key with
          | Option.Some bucket when not (List.memq bucket !buckets) ->
              buckets := bucket :: !buckets
          | _ -> ())
        (factoring_keys family.factoring (node_rule (view core) id)))
    moved;
  List.iter
    (fun bucket ->
      let sorted =
        List.stable_sort
          (fun a b -> compare_member (view core) b a)
          bucket.fmembers
      in
      let reordered =
        not
          (List.equal (fun (a, _) (b, _) -> same_id a b) sorted bucket.fmembers)
      in
      bucket.fmembers <- sorted;
      if reordered || indexed family then touch_bucket core family bucket)
    !buckets

let offers family acc =
  Int_table.fold
    (fun _ group acc ->
      match group.group_offer with
      | Option.None -> acc
      | Option.Some offer -> offer :: acc)
    family.live_groups acc

(* The subsets a graph built afresh from the rules [core]'s graph prints, in
   that order, takes in [family]: there each rule's slot and node are its rank,
   and the budget is chosen for that many rules. *)
let fresh_subsets core family ~ranks =
  let rank id = ranks.(Rule_graph.Node_id.to_int id) in
  let aggressive = Ctx.aggressive (ctx core) in
  let budget =
    match family.factoring with
    | Exact -> Subsets.exact ~aggressive (live (view core))
    | Default -> Subsets.default ~aggressive (live (view core))
  in
  let buckets =
    Int_table.fold
      (fun _ buckets acc ->
        List.fold_left
          (fun acc bucket ->
            match List.rev bucket.fmembers with
            | [] | [ _ ] -> acc
            | (first, position) :: _ as members -> (
                match
                  Subsets.of_bucket budget ~slot:rank (List.map fst members)
                with
                | [] -> acc
                | subsets ->
                    let at = rank first in
                    ( {
                        least = at;
                        size = List.length members;
                        first = at;
                        position;
                      },
                      subsets )
                    :: acc))
          acc buckets)
      family.fbuckets []
  in
  Subsets.take budget
    (List.map snd (List.sort (fun (a, _) (b, _) -> compare_rank a b) buckets))

(* The group [subset] reaches on a graph built afresh from the printed rules: an
   exact-shared group reads only the rules, and a default one keeps each entry
   no overrider precedes the provider of through live rules. *)
let fresh_group core family subset =
  match Lazy.force subset.answer with
  | Exact_answer No_exact_group -> Option.None
  | Exact_answer (Exact_group group) -> Option.Some group
  | Default_answer known ->
      Option.map fst
        (surviving_group
           ~precedes:(fun g member provider ->
             Option.is_some
               (Rule_graph.precedes_afresh (graph g) member provider))
           ~shared:family.shared ~finalize:(finalize core) (view core) known)

(* Whether a graph built afresh accepts the candidate of [group], unoffered. *)
let accepted_afresh core (group : group) =
  group.refs = 0
  &&
  (scan core group;
   match (group.witness, Lazy.force group.grouped) with
   | Option.None, Option.Some (_, scored) -> (
       match Lazy.force scored with
       | Option.None -> false
       | Option.Some (candidate : candidate) ->
           not
             (Rule_graph.refused_afresh
                (graph (view core))
                ~consume:candidate.consume ~produce:candidate.produce))
   | Option.Some _, _ | _, Option.None -> false)

(* Whether a graph built afresh from the rules the graph prints would accept a
   candidate of [family] the current graph has not offered: one a subset the
   buckets take reaches only there, or one a subset only there would take
   reaches. [ranks] gives each live rule its rank in the printed order. *)
let unsearched core family ~ranks =
  let taken = Ids_table.create 64 in
  let reaches_another subset =
    match fresh_group core family subset with
    | Option.None -> false
    | Option.Some group ->
        (not (Option.equal ( == ) (Option.Some group) subset.reached))
        && accepted_afresh core group
  in
  Int_table.fold
    (fun _ buckets found ->
      found
      || List.exists
           (fun bucket ->
             List.exists
               (fun subset ->
                 Ids_table.replace taken subset.ids ();
                 reaches_another subset)
               bucket.taken)
           buckets)
    family.fbuckets false
  || indexed family
     && List.exists
          (fun ids ->
            (not (Ids_table.mem taken ids))
            &&
            match fresh_group core family (subset_of core family ids) with
            | Option.None -> false
            | Option.Some group -> accepted_afresh core group)
          (fresh_subsets core family ~ranks:(Lazy.force ranks))

let searched core family ~ranks = not (unsearched core family ~ranks)
