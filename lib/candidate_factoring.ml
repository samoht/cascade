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

(* A rule a group consumes. *)
type member = { id : Rule_graph.node_id; rule : rule }

let member_of g id = { id; rule = node_rule g id }
let member_rules members = List.map (fun member -> member.rule) members

(* A member as the group reads it, worked out once when it joins: [leftover] is
   what it keeps once the group is lifted out of it, [lifts_over] whether what
   it keeps of its declarations past the exact-shared ones conflicts with them,
   [kept_facts] what a later member of a default group is checked against,
   [finalized] its leftover as the candidate produces it, and [paired] whether
   it holds a WebKit fallback pair. *)
type link = {
  member : member;
  leftover : Declaration.declaration list;
  lifts_over : bool Lazy.t;
  kept_facts : Commute.fact list Lazy.t;
  finalized : rule option Lazy.t;
  paired : bool Lazy.t;
}

let no_facts = Lazy.from_val []

let leftover_rule member declarations =
  { member.rule with declarations; nested = []; merge_key = Option.None }

let link ~finalize member leftover ~lifts_over ~kept_facts =
  {
    member;
    leftover;
    lifts_over;
    kept_facts;
    finalized =
      lazy
        (match leftover with
        | [] -> Option.None
        | declarations -> (
            let (rule : rule) = finalize (leftover_rule member declarations) in
            match rule.declarations with
            | [] -> Option.None
            | _ -> Option.Some rule));
    paired =
      lazy
        (match Webkit_fallback.pairs member.rule.declarations with
        | [] -> false
        | _ -> true);
  }

(* The members a window keeps under one choice of what to lift, read in the
   order they print, so that a window one member longer reads that member alone
   against the ones before it. [members] and [links] hold them the last first;
   [alone] is whether a check on one member refuses lifting the group out of
   them, and [across] whether a check on two members does, asked only when no
   member alone refuses it; [produced] is their leftovers as the candidate
   produces them, the last first, with their size. *)
type 'params chain = {
  params : 'params;
  members : member list;
  links : link list;
  alone : bool Lazy.t;
  across : bool Lazy.t;
  produced : (int * rule list) Lazy.t;
}

let start params =
  {
    params;
    members = [];
    links = [];
    alone = lazy false;
    across = lazy false;
    produced = lazy (0, []);
  }

(* [refuses link] asks the questions about the new member [link] alone, and
   [crosses prior link] the questions about it and each of the members [prior]
   before it; the questions about two earlier members were asked when the later
   of them joined. *)
let extend chain ~refuses ~crosses link =
  let prior = chain.links and alone = chain.alone and across = chain.across in
  let produced = chain.produced in
  {
    chain with
    members = link.member :: chain.members;
    links = link :: prior;
    alone = lazy (Lazy.force alone || refuses link);
    across = lazy (Lazy.force across || crosses prior link);
    produced =
      lazy
        (let size, rules = Lazy.force produced in
         match Lazy.force link.finalized with
         | Option.None -> (size, rules)
         | Option.Some rule -> (size + Size.rule rule, rule :: rules));
  }

(* The candidate of [grouped] lifted out of the members of [chain], as
   {!rewrite_candidate} scores it: the group rule is finalized first, then each
   leftover once, whichever window holds it. *)
let score ~kind ~finalize g chain grouped =
  let consume = List.rev_map (fun member -> member.id) chain.members in
  let leftovers () =
    List.fold_left
      (fun acc link ->
        match link.leftover with
        | [] -> acc
        | declarations -> leftover_rule link.member declarations :: acc)
      [] chain.links
  in
  if
    List.exists (fun link -> Lazy.force link.paired) chain.links
    && splits_a_fallback_pair g ~consume ~produce:(grouped :: leftovers ())
  then Option.None
  else
    let (grouped : rule) = finalize grouped in
    let size, leftovers = Lazy.force chain.produced in
    let produce = List.rev leftovers in
    match grouped.declarations with
    | [] -> Rule_rewrite.of_finalized ~kind (graph g) ~consume ~produce ~size
    | _ ->
        Rule_rewrite.of_finalized ~kind (graph g) ~consume
          ~produce:(grouped :: produce)
          ~size:(Size.rule grouped + size)

(* The group rule of [members] lifting [decls], with the candidate it makes once
   asked for, unless the members' selectors cannot share a list or a check
   refuses the lift. *)
let grouped_candidate ~kind ~finalize g chain decls =
  match chain.members with
  | [] | [ _ ] -> Option.None
  | members -> (
      match grouped_rule (List.rev_map (fun member -> member.rule) members) with
      | Option.None -> Option.None
      | Option.Some grouped ->
          let grouped = { grouped with declarations = decls } in
          if Lazy.force chain.alone || Lazy.force chain.across then Option.None
          else
            Option.Some (grouped, lazy (score ~kind ~finalize g chain grouped)))

(* What an exact-shared group lifts: the declarations every rule of the window
   holds. *)
type exact = {
  common : Declaration.declaration list;
  facts : Commute.fact list Lazy.t;
}

let exact_start common = start { common; facts = lazy (decl_facts common) }

(* Lifting [common] out of a new member is refused when it moves a declaration
   of it above one the member keeps and conflicts with, or when its order
   reverses two of the member's; and across two members when the new one, or an
   earlier one, keeps a leftover conflicting with it while their selectors may
   match one element, or when the new one is ordered after an earlier one
   keeping such a leftover. *)
let exact_refuses { common; facts } link =
  Lift.lifts_over_leftover (Lazy.force facts) link.member.rule
  || Lift.reorders_group common link.member.rule

let exact_crosses g prior link =
  let { id; rule } = link.member in
  let graph = graph g in
  Lazy.force link.lifts_over
  && List.exists
       (fun prior ->
         prior.member.rule != rule
         && Rule_graph.node_selectors_conflict graph id prior.member.id)
       prior
  || List.exists
       (fun prior ->
         Lazy.force prior.lifts_over
         && prior.member.rule != rule
         && Rule_graph.node_selectors_conflict graph prior.member.id id)
       prior
  || List.exists
       (fun prior ->
         Lazy.force prior.lifts_over
         && Rule_graph.conflict graph prior.member.id id)
       prior

(* A member stays in the group when everything it writes is lifted, or when the
   lift saves at least what naming its selector once more costs. *)
let exact_step g ~finalize chain member =
  let { common; facts } = chain.params in
  match Lift.remove_common common member.rule.declarations with
  | _ :: _ when selector_size g member.id + 1 > decls_inline_cost common ->
      chain
  | leftover ->
      extend chain
        ~refuses:(exact_refuses chain.params)
        ~crosses:(exact_crosses g)
        (link ~finalize member leftover
           ~lifts_over:
             (lazy
               (Lift.declarations_conflict_with_group_facts (Lazy.force facts)
                  leftover))
           ~kept_facts:no_facts)

type default_entry = {
  key : Property_key.t;
  default_decl : Declaration.declaration;
}

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

let needs_default_reassertion g entry ~prior member =
  member_has_default entry member
  && List.exists
       (fun prior ->
         member_overrides_default entry prior
         && Rule_graph.node_selectors_conflict (graph g) prior.id member.id)
       prior

let entry_needs_later_default_reassertion g members entry =
  let rec loop prior = function
    | [] -> false
    | member :: rest ->
        needs_default_reassertion g entry ~prior member
        || loop (member :: prior) rest
  in
  loop [] members

let safe_default_entries g members entries =
  List.filter
    (fun entry -> not (entry_needs_later_default_reassertion g members entry))
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

(* What a default group lifts: the declarations every member holds, past the
   [keys] the [entries] give the provider's value for. *)
type default = {
  keys : Property_key.t list;
  entries : default_entry list;
  exact_common : Declaration.declaration list;
  group_decls : Declaration.declaration list;
  group_facts : Commute.fact list Lazy.t;
  exact_facts : Commute.fact list Lazy.t;
}

let default_params ~common entries =
  let keys = List.map (fun entry -> entry.key) entries in
  let exact_common =
    List.filter (fun decl -> not (Property_key.has_any keys decl)) common
  in
  let group_decls =
    Lift.append_unique_decls exact_common
      (List.map (fun entry -> entry.default_decl) entries)
  in
  {
    keys;
    entries;
    exact_common;
    group_decls;
    group_facts = lazy (decl_facts group_decls);
    exact_facts = lazy (decl_facts exact_common);
  }

let same_default a b =
  List.equal same_decl a.exact_common b.exact_common
  && List.equal
       (fun a b ->
         Property_key.equal a.key b.key
         && same_decl a.default_decl b.default_decl)
       a.entries b.entries

(* Lifting a default group out of a new member is refused when it moves a
   declaration of the group above one the member keeps and conflicts with, or
   when the group's order reverses two of the member's; and across two members
   when the new one is ordered after an earlier one whose leftover conflicts
   with the exact-shared declarations, or after an earlier one keeping a
   declaration that conflicts with what the group takes of the new one. *)
let default_refuses p link =
  Lift.lifts_over_leftover (Lazy.force p.group_facts) link.member.rule
  || Lift.reorders_group p.group_decls link.member.rule

let default_crosses g p prior link =
  let id = link.member.id in
  let graph = graph g in
  List.exists
    (fun prior ->
      Lazy.force prior.lifts_over
      && Rule_graph.conflict graph prior.member.id id)
    prior
  ||
  match p.exact_common @ removed_default_declarations p.entries link.member with
  | [] -> false
  | taken ->
      let taken = decl_facts taken in
      List.exists
        (fun prior ->
          Rule_graph.conflict graph prior.member.id id
          && List.exists
               (fun fact -> List.exists (Commute.conflict fact) taken)
               (Lazy.force prior.kept_facts))
        prior

(* A member keeps what the group does not lift, a default an earlier overriding
   member it conflicts with would otherwise win back, and its own overriding
   values, in the order it wrote them: an override is often a shorthand
   resetting a longhand written after it ([border] then [border-top-color]). It
   stays in the group when it keeps nothing, or when what the group takes of it
   saves at least what naming its selector once more costs. *)
let default_step g ~finalize chain member =
  let p = chain.params in
  let reasserting =
    List.filter
      (fun entry ->
        needs_default_reassertion g entry ~prior:chain.members member)
      p.entries
  in
  let reasserted = List.map (fun entry -> entry.default_decl) reasserting in
  let unlifted = Lift.remove_common p.exact_common member.rule.declarations in
  let leftover =
    List.filter
      (fun decl ->
        match
          List.find_opt (fun e -> Property_key.has e.key decl) p.entries
        with
        | Option.None -> true
        | Option.Some entry ->
            (not (same_decl decl entry.default_decl))
            || List.memq entry reasserting)
      unlifted
  in
  let stays =
    match leftover with
    | [] -> true
    | _ ->
        selector_size g member.id + 1
        <= decls_inline_cost
             (p.exact_common
             @ (removed_default_declarations p.entries member
               |> Lift.remove_common reasserted))
  in
  if not stays then chain
  else
    extend chain ~refuses:(default_refuses p) ~crosses:(default_crosses g p)
      (link ~finalize member leftover
         ~lifts_over:
           (lazy
             (Lift.declarations_conflict_with_group_facts
                (Lazy.force p.exact_facts) unlifted))
         ~kept_facts:
           (lazy
             (decl_facts
                (Property_key.remove p.keys unlifted
                @ override_declarations p.entries member))))

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
let member_may_survive_pruning g provider_decls member =
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

(* What the rules of a subset make of a default group before the order questions
   decide its entries: [entries] each with the members overriding it, [common]
   the declarations every member holds, and the group met for each set of
   surviving entries. *)
type default_known = {
  first : member;
  members : member list;
  can_keep_two : bool;
  common : Declaration.declaration list Lazy.t;
  entries : (default_entry * Rule_graph.node_id list) list Lazy.t;
  mutable groups : ((default_entry * Rule_graph.node_id list) list * group) list;
}

type default_members = No_default_group | Default_members of default_known

(* The window whose members were read last, with what reading them made, so the
   window one member longer reads that member alone: {!Subsets.take} lists a
   bucket's windows by first member and then by length. [exact] is the window
   with its chain, [known] what a default group made of it, and [chain] the
   members of the window a default group last read with its chain. *)
type recent = {
  mutable exact : (Rule_graph.node_id list * exact chain) option;
  mutable known : default_known option;
  mutable chain : (member list * default chain) option;
}

let recent () =
  { exact = Option.None; known = Option.None; chain = Option.None }

(* The one element [window] lacks of [items], when [items] is [window] followed
   by it. *)
let rec extension key window items =
  match (window, items) with
  | [], [ item ] -> Option.Some item
  | prior :: window, item :: items when same_id (key prior) (key item) ->
      extension key window items
  | _ -> Option.None

let new_id prior ids = extension Fun.id prior ids
let new_member prior members = extension (fun member -> member.id) prior members

(* What the rules of a subset make of an exact-shared group, before the one
   question that reads the rest of the graph: whether the group would cross a
   live rule it conflicts with. Each step past the members waits until a
   candidate is asked for. *)
type exact_group = No_exact_group | Exact_group of group

let exact_group ~shared ~finalize ~recent g ids =
  let fold common =
    List.fold_left (exact_step g ~finalize) (exact_start common)
      (List.map (member_of g) ids)
  in
  let extended =
    match recent.exact with
    | Option.None -> Option.None
    | Option.Some (window, chain) ->
        Option.map (fun id -> (chain, member_of g id)) (new_id window ids)
  in
  let chain =
    match extended with
    | Option.Some (chain, member) -> (
        match
          List.filter
            (fun decl -> Lift.decl_mem decl member.rule.declarations)
            chain.params.common
        with
        | [] -> Option.None
        | common ->
            if List.compare_lengths common chain.params.common = 0 then
              Option.Some (exact_step g ~finalize chain member)
            else Option.Some (fold common))
    | Option.None -> (
        match Lift.common_exact_decls (List.map (node_rule g) ids) with
        | [] -> Option.None
        | common -> Option.Some (fold common))
  in
  recent.exact <- Option.map (fun chain -> (ids, chain)) chain;
  match chain with
  | Option.None -> No_exact_group
  | Option.Some { members = [] | [ _ ]; _ } -> No_exact_group
  | Option.Some ({ members; _ } as chain) ->
      let common = chain.params.common in
      Exact_group
        (share shared
           ~members:(List.rev_map (fun member -> member.id) members)
           ~decls:common ~props:[]
           (fun () ->
             grouped_candidate ~kind:Exact_shared_declarations ~finalize g chain
               common))

(* What [members] make of a default group, read afresh. *)
let fresh_known g ~first ~rest members =
  {
    first;
    members;
    can_keep_two = default_group_can_keep_two g first rest;
    common = lazy (Lift.common_exact_decls (member_rules members));
    entries =
      lazy
        (common_property_keys members
        |> default_entries first
        |> safe_default_entries g members
        |> List.map (fun entry ->
            (entry, overriders ~provider:first members entry)));
    groups = [];
  }

(* What [members], the members of [known] and then [member], make of a default
   group: [member] is read against the others alone. *)
let extended_known g known ~first members member =
  {
    first;
    members;
    can_keep_two =
      known.can_keep_two
      || member_may_survive_pruning g first.rule.declarations member;
    common =
      lazy
        (List.filter
           (fun decl -> Lift.decl_mem decl member.rule.declarations)
           (Lazy.force known.common));
    entries =
      lazy
        (List.filter_map
           (fun (entry, overriders) ->
             match member_declaration entry member with
             | Option.None -> Option.None
             | Option.Some _ ->
                 if
                   needs_default_reassertion g entry ~prior:known.members member
                 then Option.None
                 else if member_overrides_default entry member then
                   Option.Some (entry, overriders @ [ member.id ])
                 else Option.Some (entry, overriders))
           (Lazy.force known.entries));
    groups = [];
  }

let default_members g ~recent ids =
  let members =
    ordered_ids g ids
    |> List.filter_map (fun id ->
        let rule = node_rule g id in
        if rule_eligible rule then Option.Some { id; rule } else Option.None)
  in
  match members with
  | [] | [ _ ] -> No_default_group
  | first :: rest ->
      let known =
        match
          Option.bind recent.known (fun known ->
              Option.map
                (fun member -> (known, member))
                (new_member known.members members))
        with
        | Option.Some (known, member) ->
            extended_known g known ~first members member
        | Option.None -> fresh_known g ~first ~rest members
      in
      recent.known <- Option.Some known;
      Default_members known

(* The pruned members, the keys and the group body decide the candidate: the
   body is the members' common declarations followed by the provider's value for
   each key. *)
let default_group ~shared ~finalize ~recent g known entries =
  let params = default_params ~common:(Lazy.force known.common) entries in
  let extended =
    match recent.chain with
    | Option.Some (window, chain) when same_default chain.params params ->
        Option.map
          (fun member -> (chain, member))
          (new_member window known.members)
    | Option.Some _ | Option.None -> Option.None
  in
  let chain =
    match extended with
    | Option.Some (chain, member) -> default_step g ~finalize chain member
    | Option.None ->
        List.fold_left (default_step g ~finalize) (start params) known.members
  in
  recent.chain <- Option.Some (known.members, chain);
  share shared
    ~members:(List.rev_map (fun member -> member.id) chain.members)
    ~decls:params.group_decls ~props:params.keys
    (fun () ->
      grouped_candidate ~kind:Default_factoring ~finalize g chain
        params.group_decls)

(* The group [known] makes on [g] - the one met before for the entries that
   survive here, or a new one - and the order questions that decide those
   entries: whether a member overriding a surviving entry precedes the provider.
   {!Rule_graph.try_rewrite} preserves the partial order, so an entry dropped
   for an override that precedes the provider stays dropped, and these are the
   only questions a later graph can answer differently. *)
let surviving_group ~shared ~finalize ~recent g known =
  match known with
  | No_default_group -> Option.None
  | Default_members known when not known.can_keep_two -> Option.None
  | Default_members known ->
      let provider = known.first.id in
      let survivors =
        List.filter
          (fun (_, overriders) ->
            not
              (List.exists
                 (fun member -> Rule_graph.precedes (graph g) member provider)
                 overriders))
          (Lazy.force known.entries)
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
              default_group ~shared ~finalize ~recent g known
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

(* Where a bucket stands in the order a shared budget is spent in: its first
   printed member, with the position of the key's declaration there, and its
   size. *)
type lead = { first : Rule_graph.node_id; position : int; size : int }

(* By where the first member prints, which [compare] orders, then by size, then
   by where the key's declaration stands in that member. Two buckets of one kind
   that lead with one member file it under two keys, at two positions. *)
let compare_lead compare a b =
  match compare a.first b.first with
  | 0 -> (
      match Int.compare a.size b.size with
      | 0 -> Int.compare a.position b.position
      | order -> order)
  | order -> order

type factoring_bucket = {
  fkey : factoring_key;
  buid : int;
  mutable fmembers : (Rule_graph.node_id * int) list;
      (** with the key's position, the last printed first *)
  mutable lead : lead;
  mutable subsets : Subsets.bucket;
  mutable taken : subset list;
  mutable taken_count : int;
  mutable want : int;
  mutable fstamp : int;
  mutable asked : int;
}

type t = {
  factoring : factoring;
  budget : Subsets.t;
  fbuckets : factoring_bucket list Int_table.t;
  offering : factoring_bucket Int_table.t;
      (** the buckets with a subset, by [buid] *)
  mutable next_bucket : int;
  mutable takers : factoring_bucket list;
  mutable fdirty : factoring_bucket list;
  known : subset Ids_table.t;
  shared : shared;
  watched : subset Int_table.t;
  live_groups : group Int_table.t;
  mutable next_subset : int;
  recent : recent;
}

let compare_member g (a, _) (b, _) = Rule_graph.compare_position (graph g) a b

let rec insert_member g member = function
  | [] -> [ member ]
  | other :: rest as members ->
      if compare_member g member other > 0 then member :: members
      else other :: insert_member g member rest

(* [members] are the last printed first. *)
let lead_of members =
  let rec last size = function
    | [] -> Option.None
    | [ (first, position) ] -> Option.Some { first; position; size }
    | _ :: rest -> last size rest
  in
  last (List.length members) members

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

let no_lead =
  { first = Rule_graph.Node_id.of_int_exn 0; position = 0; size = 0 }

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
                buid = family.next_bucket;
                fmembers = [];
                lead = no_lead;
                subsets = Subsets.bucket family.budget [];
                taken = [];
                taken_count = 0;
                want = 0;
                fstamp = -1;
                asked = -1;
              }
            in
            family.next_bucket <- family.next_bucket + 1;
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

let refresh_bucket family bucket =
  bucket.subsets <-
    Subsets.bucket family.budget (List.rev_map fst bucket.fmembers);
  bucket.lead <- Option.value (lead_of bucket.fmembers) ~default:no_lead;
  if bucket.fmembers = [] then drop_bucket family bucket;
  if indexed family then
    if Subsets.count bucket.subsets = 0 then
      Int_table.remove family.offering bucket.buid
    else Int_table.replace family.offering bucket.buid bucket

let subset_of core family ids =
  match Ids_table.find_opt family.known ids with
  | Option.Some subset -> subset
  | Option.None ->
      let g = view core in
      let finalize = finalize core and shared = family.shared in
      let recent = family.recent in
      let answer =
        match family.factoring with
        | Exact ->
            lazy (Exact_answer (exact_group ~shared ~finalize ~recent g ids))
        | Default -> lazy (Default_answer (default_members g ~recent ids))
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
            ~recent:family.recent (view core) known
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

(* Point [bucket] at its first [n] subsets. A subset some bucket takes reaches
   its group; one no bucket takes reaches none. *)
let retake core family ~activated ~released bucket n =
  let subsets =
    List.map (subset_of core family) (Subsets.take bucket.subsets n)
  in
  List.iter
    (fun subset ->
      subset.taken_by <- subset.taken_by + 1;
      if subset.taken_by = 1 then activated := subset :: !activated)
    subsets;
  released := List.rev_append bucket.taken !released;
  bucket.taken <- subsets;
  bucket.taken_count <- n

(* An indexed budget is spent on the buckets in the order their first members
   print, so every bucket that took subsets before, or might take some now, is
   asked again; an exhaustive one leaves every bucket all of its subsets. *)
let wanted core family dirty =
  match Subsets.shared family.budget with
  | Option.None ->
      List.iter
        (fun bucket -> bucket.want <- Subsets.count bucket.subsets)
        dirty;
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
              let n = Int.min remaining (Subsets.count bucket.subsets) in
              bucket.want <- n;
              spend (remaining - n) (bucket :: takers) rest
      in
      let compare = Rule_graph.compare_position (graph (view core)) in
      let ordered =
        Int_table.fold (fun _ bucket acc -> bucket :: acc) family.offering []
        |> List.sort (fun a b -> compare_lead compare a.lead b.lead)
      in
      let takers = spend max_candidates [] (List.to_seq ordered) in
      family.takers <- takers;
      List.rev_append takers (List.rev_append previous dirty)

let settle core family { consumed; produced; moved } =
  let dirty = List.rev family.fdirty in
  family.fdirty <- [];
  List.iter (refresh_bucket family) dirty;
  let activated = ref [] and released = ref [] in
  List.iter
    (fun bucket ->
      if bucket.asked <> epoch core then begin
        bucket.asked <- epoch core;
        if bucket.fstamp = epoch core || bucket.want <> bucket.taken_count then
          retake core family ~activated ~released bucket bucket.want
      end)
    (wanted core family dirty);
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
    offering = Int_table.create size;
    next_bucket = 0;
    takers = [];
    fdirty = [];
    known = Ids_table.create size;
    shared = shared size;
    watched = Int_table.create 8;
    live_groups = Int_table.create size;
    next_subset = 0;
    recent = recent ();
  }

(* The nodes a rewrite moved keep their rules and keys. A bucket holds its
   members in printed order, so a bucket holding one is sorted again, and asked
   again when its order changed. Where its first member prints among the other
   buckets' is read at every settle. *)
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
      if reordered then touch_bucket core family bucket)
    !buckets

let offers family acc =
  Int_table.fold
    (fun _ group acc ->
      match group.group_offer with
      | Option.None -> acc
      | Option.Some offer -> offer :: acc)
    family.live_groups acc
