open Stylesheet
open Stdlib
open Rule_rewrite
open Candidate_kind

let declarations_equal (a : Declaration.declaration list)
    (b : Declaration.declaration list) =
  Merge.declarations_equal ~same:same_decl a b

let hash_strings strings =
  strings
  |> List.fold_left
       (fun hash s -> Common.mix_int hash (Common.hash_string s))
       0x123456

(* Identical-body grouping merges same-body rules under a combined selector
   list without touching the body or its order, so it is safe for
   custom-property rules too - unlike the factoring passes, which reorder
   declarations and would disturb a later [var()] resolution.
   [try_rewrite]'s acyclicity check still rejects a group whose merge would
   cross a conflicting (re)definition. *)
(* CSS Nesting 1 sec. 3.4 puts a declaration written after a nested rule
   behind it, so a rule's body is not the run of declarations [declarations]
   holds: merging a later same-selector rule into it moves that rule's
   declarations ahead of everything nested. [nested_merge_is_safe] below reads
   the whole body and decides that per property, so a rule carrying nested
   content stands on the same terms as any other. *)
let same_selector_eligible (r : rule) =
  r.merge_key = Option.None && Merge.listable r.selector
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let identical_body_eligible ~ctx (r : rule) =
  r.nested = [] && r.merge_key = Option.None && Merge.listable r.selector
  && (Ctx.extend_lists ctx || not (Selector.is_compound_list r.selector))
  && not (List.exists Shorthand.is_all_declaration r.declarations)

let selector_key g id = Rule_graph.node_branches (graph g) id

let selector_keys_equal left right =
  let rec loop left right =
    match (left, right) with
    | [], [] -> true
    | l :: left, r :: right -> String.equal l r && loop left right
    | _ -> false
  in
  loop left right

let body_equal_key g id = Rule_graph.declaration_body_key (graph g) id
let body_bucket_key g id = body_equal_key g id |> hash_ints

(* A bucket's members split by exact body, and by which targets drop each
   selector ({!Merge.compatible}), so that every group can share one list. *)
let same_body_groups g ids =
  let rec insert id (rule : rule) compat = function
    | [] -> [ (rule, compat, [ id ]) ]
    | ((head_rule : rule), head_compat, ids) :: rest ->
        if
          Merge.equal_compatibility head_compat compat
          && declarations_equal head_rule.declarations rule.declarations
        then (head_rule, head_compat, id :: ids) :: rest
        else (head_rule, head_compat, ids) :: insert id rule compat rest
  in
  ids
  |> List.fold_left
       (fun groups id ->
         let rule = node_rule g id in
         insert id rule (Merge.compatibility rule.selector) groups)
       []
  |> List.map (fun (_, _, ids) -> List.rev ids)

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

type grouping = Bodies | Selectors

type group_bucket = {
  group_key : int;
  mutable members : Rule_graph.node_id list;  (** newest first *)
  mutable bodies : Rule_graph.node_id list list;
      (** the members split by exact body, as {!same_body_groups} splits them *)
  mutable runs : Rule_graph.node_id list list;
      (** the runs of {!field-bodies} the offers were made for *)
  mutable offers : offer list;
  mutable group_stamp : int;
}

type t = {
  grouping : grouping;
  group_buckets : group_bucket Int_table.t;
  mutable group_dirty : group_bucket list;
  mutable spanned : bool;  (** whether the last settle split runs by span *)
}

let grouping_key core grouping id =
  let rule = node_rule (view core) id in
  match grouping with
  | Bodies ->
      if identical_body_eligible ~ctx:(ctx core) rule && rule.declarations <> []
      then Option.Some (body_bucket_key (view core) id)
      else Option.None
  | Selectors ->
      if same_selector_eligible rule then
        Option.Some (hash_strings (selector_key (view core) id))
      else Option.None

let touch_group core groups bucket =
  if bucket.group_stamp <> epoch core then begin
    bucket.group_stamp <- epoch core;
    groups.group_dirty <- bucket :: groups.group_dirty
  end

let remove core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      let bucket = Int_table.find groups.group_buckets key in
      bucket.members <- without id bucket.members;
      touch_group core groups bucket

let touch core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      touch_group core groups (Int_table.find groups.group_buckets key)

let add core groups id =
  match grouping_key core groups.grouping id with
  | Option.None -> ()
  | Option.Some key ->
      let bucket =
        match Int_table.find_opt groups.group_buckets key with
        | Option.Some bucket -> bucket
        | Option.None ->
            let bucket =
              {
                group_key = key;
                members = [];
                bodies = [];
                runs = [];
                offers = [];
                group_stamp = -1;
              }
            in
            Int_table.replace groups.group_buckets key bucket;
            bucket
      in
      bucket.members <- id :: bucket.members;
      touch_group core groups bucket

(* The transfer objective merges identical bodies only within
   {!Subsets.max_span} printed rules: a compressor encodes a repeated body in a
   few bytes wherever it sits, but a selector moved away from its neighbours
   stops compressing against them. A run of members starts at the first member
   printed and takes every member printed within that many rules of it. *)
let spans core = Ctx.objective (ctx core) = `Transfer

(* The rank of every live rule in the order the graph prints, or [None] while
   the graph prints too few rules for a run to need splitting. *)
let printed_ranks core =
  let g = view core in
  if not (spans core && live g > Subsets.max_span + 1) then Option.None
  else begin
    let ranks = Array.make (Rule_graph.node_count (graph g)) 0 in
    Array.iteri
      (fun rank id -> ranks.(Rule_graph.Node_id.to_int id) <- rank)
      (Rule_graph.canonical_order (graph g));
    Option.Some ranks
  end

let split_runs ~ranks = function
  | [] -> []
  | first :: rest ->
      let rank id = ranks.(Rule_graph.Node_id.to_int id) in
      let rec go first run runs = function
        | [] -> List.rev (List.rev run :: runs)
        | id :: rest ->
            if rank id - rank first <= Subsets.max_span then
              go first (id :: run) runs rest
            else go id [ id ] (List.rev run :: runs) rest
      in
      go first [ first ] [] rest

(* Under the transfer objective a body's members in the order they print, split
   into runs when [ranks] says the graph needs it; otherwise the members as
   filed. *)
let body_runs core ~ranks ids =
  if not (spans core) then [ ids ]
  else
    let sorted =
      List.sort (Rule_graph.compare_position (graph (view core))) ids
    in
    match ranks with
    | Option.None -> [ sorted ]
    | Option.Some ranks -> split_runs ~ranks sorted

let group_candidates core grouping runs =
  let g = view core in
  let finalize = finalize core in
  let candidates = ref [] in
  (match grouping with
  | Bodies ->
      List.iter
        (add_identical_body_group ~ctx:(ctx core) ~finalize g ~candidates)
        runs
  | Selectors ->
      List.iter
        (add_same_selector_group
           ~stats:(Ctx.stats (ctx core))
           ~finalize g ~candidates)
        runs);
  List.rev !candidates

let reoffer core groups bucket runs =
  List.iter withdraw bucket.offers;
  bucket.runs <- runs;
  bucket.offers <-
    group_candidates core groups.grouping runs |> List.map (offer core)

let same_runs a b = List.equal (List.equal same_id) a b

(* A commit anywhere moves the ranks, so while runs are split by span every
   bucket of two or more members is split again, and offered again when its runs
   changed. *)
let settle core groups (_ : changes) =
  let ranks =
    match groups.grouping with
    | Bodies -> printed_ranks core
    | Selectors -> Option.None
  in
  List.iter
    (fun bucket ->
      (match bucket.members with
      | [] | [ _ ] ->
          bucket.bodies <- [];
          reoffer core groups bucket []
      | members -> (
          let members = List.rev members in
          match groups.grouping with
          | Bodies ->
              bucket.bodies <- same_body_groups (view core) members;
              reoffer core groups bucket
                (List.concat_map (body_runs core ~ranks) bucket.bodies)
          | Selectors ->
              reoffer core groups bucket
                (same_selector_groups (view core) members)));
      if bucket.members = [] then
        Int_table.remove groups.group_buckets bucket.group_key)
    (List.rev groups.group_dirty);
  groups.group_dirty <- [];
  let spanned = Option.is_some ranks in
  if spanned || groups.spanned then
    Int_table.iter
      (fun _ bucket ->
        match bucket.members with
        | [] | [ _ ] -> ()
        | _ ->
            if bucket.group_stamp <> epoch core then begin
              let runs =
                List.concat_map (body_runs core ~ranks) bucket.bodies
              in
              if not (same_runs runs bucket.runs) then
                reoffer core groups bucket runs
            end)
      groups.group_buckets;
  groups.spanned <- spanned

let v size grouping =
  {
    grouping;
    group_buckets = Int_table.create size;
    group_dirty = [];
    spanned = false;
  }

(* A bucket holds its members in creation order, and its candidates read the
   printed order afresh: a same-selector merge always, an identical-body merge
   under the transfer objective. *)
let reorder core groups moved =
  match groups.grouping with
  | Selectors -> List.iter (touch core groups) moved
  | Bodies -> if spans core then List.iter (touch core groups) moved

let offers groups acc =
  Int_table.fold
    (fun _ bucket acc -> List.rev_append bucket.offers acc)
    groups.group_buckets acc
