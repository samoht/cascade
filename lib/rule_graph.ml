(** Cascade-dependency graph over a run of flat style rules. *)

open Stylesheet
open Stdlib

(* The declaration-keyed bucket index is probed once per overlap key per node
   while a graph is built, and a generic [Hashtbl] compares whole declaration
   subtrees on every probe. The cached [Declaration.hash] buckets in constant
   time and the structural check runs only when two hashes collide. *)
module Decl_tbl = Hashtbl.Make (struct
  type t = Declaration.declaration

  let equal = Shorthand.same_minified_declaration
  let hash = Declaration.hash
end)

module Key_tbl = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

module String_table = Common.Table.Make (String)

module Node_id = struct
  type t = int

  let compare = Int.compare
  let to_int t = t

  let of_int_exn i =
    if i < 0 then invalid_arg "Rule_graph.Node_id.of_int_exn";
    i
end

type node_id = Node_id.t
type edge_reason = Cascade_conflict | Shared_branch_pin

type rewrite_error =
  | Empty_consume
  | Empty_produce
  | Invalid_node of node_id
  | Duplicate_node of node_id
  | Stale_node of node_id
  | Ambiguous_external_order of { produced : node_id; external_ : node_id }
  | New_external_conflict of { produced : node_id; external_ : node_id }
  | Ambiguous_produced_order of { left : node_id; right : node_id }
  | New_produced_conflict of { left : node_id; right : node_id }
  | Cycle

type decl_overlap = {
  decl : Declaration.declaration;
  important : bool;
  footprint : Shorthand.overlap_key list;
}

(* Which nodes each node can reach, as a bitset per node. Orienting a factoring
   asks the question once per ordered pair of source references, so a run of [n]
   rules is asked about [n^2] pairs, while the edges are settled the moment a
   graph is published. Holding the answers turns the pairs into bit tests over
   one pass of the edges. [Unbuilt] until the first question; [Walked] for a run
   long enough that holding them costs more than re-asking. *)
type reachability = Unbuilt | Held of Bytes.t array | Walked

type t = {
  generation : int;
  closed_world : bool;
      (** when set, two distinct selectors are assumed not to match a common
          element (a caller-asserted closed DOM), so they never cascade-conflict
          on selector grounds *)
  parent : Selector.t option;
      (** the enclosing nesting context; every node's overlap is computed on its
          selector expanded against this, and rewrites reuse it so produced
          nodes are indexed the same way as existing ones. *)
  count : int;
      (** live extent of the node arrays. They are allocated with slack and
          appended to in place, so their length is a capacity, not a node count.
      *)
  rules : rule array;  (** all nodes ever created, dead ones included *)
  summaries : Summary.t array;
      (** declaration-side facts and byte sizes, precomputed once per node *)
  origin : int array;
  live : bool array;
  selectors : Selector.t list array;
      (** effective selector branches for each node, after applying [parent] *)
  selector_summaries : Selector_summary.t list array;
  specificities : Selector.specificity list array;
      (** each node's per-branch specificity, computed once. The order test
          compares specificity on every candidate pair, and recomputing it
          allocates a record per branch per comparison, which made it one of the
          largest allocation sites in the optimizer. *)
  branches : string list array;
  decl_overlaps : decl_overlap list array;
  overlap_keys : Shorthand.overlap_key list array;
  succ : (node_id * edge_reason) list array;
      (** directed partial-order edges: [i] precedes every node reachable from
          [succ.(i)]. Simple dense runs store a transitive chain rather than one
          edge per conflicting pair. Edges follow creation order (source order
          for [of_rules], inherited order for rewrites). *)
  key_index : node_id list Decl_tbl.t Key_tbl.t;
      (** overlap key -> (declaration writing it -> node ids), so a rewrite
          finds an external node's potential conflicts without scanning every
          node, and {e without even iterating} the nodes that write the same
          declaration (which never conflict). Mutable and shared across a
          graph's rewrites (a graph is consumed before the next is produced);
          never pruned, dead nodes are skipped at query time. *)
  branch_index : node_id list String_table.t;
      (** selector branch -> node ids that carry it; same role as [key_index]
          for the [share_branch] dimension. *)
  mutable reach : reachability;
      (** reachable set per node, over the [succ] above. Filled on demand: a
          graph whose order is never questioned holds nothing. *)
  mutable expansions : int;
      (** nodes expanded to answer reachability questions, for
          {!reachability_expansions}. *)
}

(* The two dimensions of the CSS-graph dependency. Two rules conflict - their
   relative order is cascade-significant - only when some element can match both
   selectors at equal specificity AND they write different declarations for a
   shared property at equal importance (shorthand/longhand-aware, on the
   library's [declaration_covers] bar). Disjoint, strictly-ordered specificity,
   exact same-value writes, or differing importance means a reorder is
   cascade-neutral inside a fixed origin/layer/scope context. *)
let declarations_conflict left right =
  left.important = right.important
  && (not (Shorthand.same_minified_declaration left.decl right.decl))
  && Shorthand.declarations_overlap_with_keys left.decl left.footprint
       right.decl right.footprint

(* Written as plain recursion rather than nested [List.exists]: the inner
   closure captured the outer element, so it was rebuilt once per element of the
   left list, and these run on every candidate pair. *)
let rec declaration_conflicts_with a = function
  | [] -> false
  | b :: rest -> declarations_conflict a b || declaration_conflicts_with a rest

let rec decls_order_conflict d1 d2 =
  match d1 with
  | [] -> false
  | a :: rest -> declaration_conflicts_with a d2 || decls_order_conflict rest d2

(* [keys] comes from [rule_overlap_keys], which sorts, so the scan stops at the
   first key past [key] rather than walking the whole list. *)
let rec overlap_key_in key = function
  | [] -> false
  | k :: rest ->
      let c = Shorthand.overlap_key_compare key k in
      c = 0 || (c > 0 && overlap_key_in key rest)

(* Both arguments come from [rule_overlap_keys], which sorts, so this walks the
   two in step rather than scanning one per element of the other: linear instead
   of quadratic in the rules' distinct key counts. *)
let rec overlap_keys_meet a b =
  match (a, b) with
  | [], _ | _, [] -> false
  | x :: xs, y :: ys ->
      let c = Shorthand.overlap_key_compare x y in
      if c = 0 then true
      else if c < 0 then overlap_keys_meet xs b
      else overlap_keys_meet a ys

let overlap_key_lists_intersect a b =
  let broad = Shorthand.broad_overlap_key in
  overlap_key_in broad a || overlap_key_in broad b || overlap_keys_meet a b

let specificity_equal (a : Selector.specificity) (b : Selector.specificity) =
  Int.equal a.ids b.ids
  && Int.equal a.classes b.classes
  && Int.equal a.elements b.elements

(* [List.exists2] over a zipped pair needs the zip built first, and the inner
   one was built inside the outer closure: B's whole tuple list was rebuilt once
   per element of A. Walking the three parallel lists in step decides the same
   question and allocates nothing. Lengths match by construction (one entry per
   selector branch); [invalid_arg] mirrors [List.exists2] if they ever do
   not. *)
let rec exists3 f a b c =
  match (a, b, c) with
  | [], [], [] -> false
  | x :: xs, y :: ys, z :: zs -> f x y z || exists3 f xs ys zs
  | _ -> invalid_arg "Rule_graph.exists3"

let selectors_order_conflict ?(closed_world = false) selectors_a summaries_a
    specificities_a selectors_b summaries_b specificities_b =
  exists3
    (fun selector_a summary_a specificity_a ->
      exists3
        (fun selector_b summary_b specificity_b ->
          specificity_equal specificity_a specificity_b
          &&
          if closed_world then
            (* the caller asserts no element matches two distinct selectors, so
               only an identical selector still ties on the same element *)
            selector_a = selector_b
          else Selector_summary.may_overlap summary_a summary_b)
        selectors_b summaries_b specificities_b)
    selectors_a summaries_a specificities_a

(* Canonical, list-order-independent branch strings of a selector: canonicalize
   each comma branch, then sort. [.foo,.bar] and [.bar,.foo] yield the same
   list. *)
(* Sorted, which is what lets [share_branch] merge-walk two of these. *)
let selector_branch_keys selectors =
  selectors
  |> List.map (fun s ->
      Pp.to_string ~minify:true Selector.pp (Selector.canonicalize s))
  |> List.sort String.compare

(* Two rules share a selector branch when a factoring produced one as a residual
   of the other (e.g. [.a] and [.a,.b]) or they are the same selector. Such
   rules are pinned so a factored group stays contiguous and its declaration
   order is left to the declaration canonicalization.

   Both arguments come from [selector_branch_keys], which sorts, so this walks
   the two in step rather than scanning one per element of the other: linear
   instead of quadratic, and it allocates nothing, where the nested
   [List.exists] built a closure per element of the left list and a partial
   application per pair. *)
let rec share_branch a b =
  match (a, b) with
  | [], _ | _, [] -> false
  | x :: xs, y :: ys ->
      let c = String.compare x y in
      if c = 0 then true
      else if c < 0 then share_branch xs b
      else share_branch a ys

let declaration_size decl = Pp.size ~minify:true Declaration.pp decl

let rule_summary rule =
  Summary.v ~rule_size:Size.rule ~decl_size:declaration_size
    ~selector_size:(Pp.size ~minify:true Selector.pp)
    rule

let selector_list = function
  | [] -> invalid_arg "Rule_graph.selector_list"
  | [ sel ] -> sel
  | sels -> Selector.List sels

(* The effective selector a rule's overlap is computed against: under a nesting
   [parent], a relative nested selector ([&:hover], [> .icon]) is expanded to
   its parent-qualified form so the conflict model uses the real subject. *)
let effective_selector (parent : Selector.t option) (r : rule) =
  match parent with
  | Option.None -> r.selector
  | Option.Some parent ->
      let parents = Edge.selectors parent in
      let children = Edge.selectors r.selector in
      List.concat_map
        (fun parent -> List.map (Nest.combine parent) children)
        parents
      |> selector_list

(* Conflict between two nodes of a graph by their stored summaries/branches. *)
let nodes_conflict_reason t i j =
  if share_branch t.branches.(i) t.branches.(j) then
    Option.Some Shared_branch_pin
  else if
    overlap_key_lists_intersect t.overlap_keys.(i) t.overlap_keys.(j)
    && decls_order_conflict t.decl_overlaps.(i) t.decl_overlaps.(j)
    && selectors_order_conflict ~closed_world:t.closed_world t.selectors.(i)
         t.selector_summaries.(i) t.specificities.(i) t.selectors.(j)
         t.selector_summaries.(j) t.specificities.(j)
  then Option.Some Cascade_conflict
  else Option.None

let nodes_conflict t i j = nodes_conflict_reason t i j <> Option.None

module Overlap_key_table = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

(* Packed specificities, so the bucket index is keyed by an immediate rather
   than by a record a generic table would hash and compare field by field. *)
module Spec_table = Common.Table.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

let declaration_overlap decl =
  {
    decl;
    important = Declaration.is_important decl;
    footprint = Shorthand.declaration_overlap_keys decl;
  }

let rule_decl_overlaps (rule : rule) =
  List.map declaration_overlap rule.declarations

(* Written as plain recursion rather than folds over closures, like the conflict
   tests above: this runs once per rule of every stylesheet, and each closure is
   allocated per call. *)
let rec footprint_total n = function
  | [] -> n
  | (decl : decl_overlap) :: rest ->
      footprint_total (n + List.length decl.footprint) rest

let rec fill_keys keys next = function
  | [] -> next
  | key :: rest ->
      keys.(next) <- key;
      fill_keys keys (next + 1) rest

let rec fill_footprints keys next = function
  | [] -> ()
  | (decl : decl_overlap) :: rest ->
      fill_footprints keys (fill_keys keys next decl.footprint) rest

(* The distinct keys of a sorted array, ascending. *)
let rec distinct_keys keys i acc =
  if i < 0 then acc
  else if i > 0 && Shorthand.overlap_key_equal keys.(i) keys.(i - 1) then
    distinct_keys keys (i - 1) acc
  else distinct_keys keys (i - 1) (keys.(i) :: acc)

(* A rule's distinct overlap keys, sorted: [overlap_key_in] and
   [overlap_keys_meet] read this list once per candidate pair, and sorting is
   what lets them stop early and merge-walk instead of scanning one list per
   element of the other. Sorted in one array, since deduplicating against the
   accumulator scanned it once per key, and [Array.sort] is in place. *)
let rule_overlap_keys decls =
  let total = footprint_total 0 decls in
  if total = 0 then []
  else begin
    let keys = Array.make total Shorthand.broad_overlap_key in
    fill_footprints keys 0 decls;
    Array.sort Shorthand.overlap_key_compare keys;
    distinct_keys keys (total - 1) []
  end

(* A cascade conflict needs two selector branches of equal specificity, so a
   candidate bucket is keyed by specificity as well as by overlap key: rules
   that write the same property at specificities that can never tie are not
   enumerated at all, which is four candidate pairs in five of a real
   stylesheet. The three components count simple selectors and sit far below the
   20-bit field; one past it saturates, which can only merge two specificities
   into one bucket, never split one. *)
let spec_field = 0xFFFFF
let spec_component x = if x > spec_field then spec_field else x

let spec_key (s : Selector.specificity) =
  (spec_component s.ids lsl 40)
  lor (spec_component s.classes lsl 20)
  lor spec_component s.elements

let rec spec_key_mem (key : int) = function
  | [] -> false
  | k :: rest -> key = k || spec_key_mem key rest

(* The distinct specificities of a node's branches. A rule has a handful of
   branches, so the linear membership test beats sorting them. *)
let rec distinct_spec_keys acc = function
  | [] -> acc
  | spec :: rest ->
      let key = spec_key spec in
      distinct_spec_keys (if spec_key_mem key acc then acc else key :: acc) rest

let spec_bucket bucket key =
  match Overlap_key_table.find_opt bucket key with
  | Option.Some inner -> inner
  | Option.None ->
      let inner = Spec_table.create 4 in
      Overlap_key_table.replace bucket key inner;
      inner

let rec add_spec_buckets bucket value = function
  | [] -> ()
  | spec :: rest ->
      Spec_table.push bucket spec value;
      add_spec_buckets bucket value rest

let add_bucket bucket key specs value =
  add_spec_buckets (spec_bucket bucket key) value specs

(* [seen] is a per-target stamp array indexed by node id: [seen.(i) = stamp]
   means node [i] is already a candidate for the current target. The target's
   own index is a fresh monotonically increasing stamp, so no clearing is
   needed, and dedup is O(1) with no hashing or per-target allocation. *)
let add_candidate stamp seen acc i =
  let idx = Node_id.to_int i in
  if seen.(idx) = stamp then acc
  else begin
    seen.(idx) <- stamp;
    i :: acc
  end

let rec add_candidates stamp seen acc = function
  | [] -> acc
  | i :: rest -> add_candidates stamp seen (add_candidate stamp seen acc i) rest

let rec collect_specs bucket specs stamp seen acc =
  match specs with
  | [] -> acc
  | spec :: rest ->
      let acc =
        match Spec_table.find_opt bucket spec with
        | Option.None -> acc
        | Option.Some ids -> add_candidates stamp seen acc ids
      in
      collect_specs bucket rest stamp seen acc

let collect_bucket bucket key specs stamp seen acc =
  match Overlap_key_table.find_opt bucket key with
  | Option.None -> acc
  | Option.Some inner -> collect_specs inner specs stamp seen acc

let collect_string_bucket bucket key stamp seen acc =
  String_table.find_opt bucket key
  |> Option.value ~default:[]
  |> add_candidates stamp seen acc

(* The declaration that writes a key sits below the specificity and the
   selector's exclusive subject facts. Two rules writing the same declaration
   never constrain each other's order there, while two branches with distinct
   mandatory IDs or element names cannot target the same element. Keeping both
   dimensions in the index skips either whole group without enumerating its
   nodes.

   For a simple one-declaration branch, every distinct declaration at the
   frontier gets an edge to the new node. Those old frontier nodes are then
   transitively dominated, so only the new node and any same-declaration peers
   remain. [all] and [opaque] preserve full history for shapes where that
   compaction invariant does not apply. *)
type decl_groups = {
  all : node_id list Decl_tbl.t;
  opaque : node_id list Decl_tbl.t;
  frontier : node_id list Decl_tbl.t;
}

type importance_decl_groups = { normal : decl_groups; important : decl_groups }

type id_decl_groups = {
  no_id : importance_decl_groups;
  by_id : importance_decl_groups String_table.t;
}

type selector_decl_groups = {
  no_element : id_decl_groups;
  by_element : id_decl_groups String_table.t;
}

let empty_decl_groups () =
  {
    all = Decl_tbl.create 4;
    opaque = Decl_tbl.create 4;
    frontier = Decl_tbl.create 4;
  }

let empty_importance_decl_groups () =
  { normal = empty_decl_groups (); important = empty_decl_groups () }

let empty_id_decl_groups () =
  { no_id = empty_importance_decl_groups (); by_id = String_table.create 4 }

let empty_selector_decl_groups () =
  { no_element = empty_id_decl_groups (); by_element = String_table.create 4 }

let decl_spec_bucket bucket key =
  match Overlap_key_table.find_opt bucket key with
  | Option.Some inner -> inner
  | Option.None ->
      let inner = Spec_table.create 4 in
      Overlap_key_table.replace bucket key inner;
      inner

let selector_decl_groups inner spec : selector_decl_groups =
  match Spec_table.find_opt inner spec with
  | Option.Some index -> index
  | Option.None ->
      let index = empty_selector_decl_groups () in
      Spec_table.replace inner spec index;
      index

let element_decl_groups index = function
  | Option.None -> index.no_element
  | Option.Some element -> (
      match String_table.find_opt index.by_element element with
      | Option.Some groups -> groups
      | Option.None ->
          let groups = empty_id_decl_groups () in
          String_table.replace index.by_element element groups;
          groups)

let named_decl_groups groups name =
  match String_table.find_opt groups.by_id name with
  | Option.Some decls -> decls
  | Option.None ->
      let decls = empty_importance_decl_groups () in
      String_table.replace groups.by_id name decls;
      decls

let importance_decl_groups groups decl =
  if Declaration.is_important decl then groups.important else groups.normal

let push_decl groups decl value =
  Decl_tbl.replace groups decl
    (value :: Option.value ~default:[] (Decl_tbl.find_opt groups decl))

let add_decl ~compact groups decl value =
  let groups = importance_decl_groups groups decl in
  push_decl groups.all decl value;
  if compact then begin
    let same =
      Decl_tbl.find_opt groups.frontier decl |> Option.value ~default:[]
    in
    Decl_tbl.clear groups.frontier;
    Decl_tbl.replace groups.frontier decl (value :: same)
  end
  else push_decl groups.opaque decl value

let add_selector_decl ~compact index summary decl value =
  let groups = element_decl_groups index (Selector_summary.element summary) in
  match Selector_summary.ids summary with
  | [] -> add_decl ~compact groups.no_id decl value
  | ids ->
      List.iter
        (fun id -> add_decl ~compact (named_decl_groups groups id) decl value)
        ids

let add_decl_bucket ~compact bucket key spec summary decl value =
  add_selector_decl ~compact
    (selector_decl_groups (decl_spec_bucket bucket key) spec)
    summary decl value

let collect_decl_table decl groups stamp seen acc =
  Decl_tbl.fold
    (fun decl' ids acc ->
      if Shorthand.same_minified_declaration decl decl' then acc
      else add_candidates stamp seen acc ids)
    groups acc

let collect_distinct_decls ~compact decl groups stamp seen acc =
  let groups = importance_decl_groups groups decl in
  if compact then
    collect_decl_table decl groups.frontier stamp seen acc |> fun acc ->
    collect_decl_table decl groups.opaque stamp seen acc
  else collect_decl_table decl groups.all stamp seen acc

let collect_all_decls groups stamp seen acc =
  let collect groups acc =
    Decl_tbl.fold
      (fun _ ids acc -> add_candidates stamp seen acc ids)
      groups.all acc
  in
  collect groups.normal acc |> fun acc -> collect groups.important acc

let collect_id_groups collect groups ids stamp seen acc =
  let acc = collect groups.no_id stamp seen acc in
  match ids with
  | [] ->
      String_table.fold
        (fun _ decls acc -> collect decls stamp seen acc)
        groups.by_id acc
  | ids ->
      List.fold_left
        (fun acc id ->
          match String_table.find_opt groups.by_id id with
          | Option.None -> acc
          | Option.Some decls -> collect decls stamp seen acc)
        acc ids

let collect_selector_groups collect index summary stamp seen acc =
  let ids = Selector_summary.ids summary in
  let acc = collect_id_groups collect index.no_element ids stamp seen acc in
  match Selector_summary.element summary with
  | Option.None ->
      String_table.fold
        (fun _ groups acc ->
          collect_id_groups collect groups ids stamp seen acc)
        index.by_element acc
  | Option.Some element -> (
      match String_table.find_opt index.by_element element with
      | Option.None -> acc
      | Option.Some groups ->
          collect_id_groups collect groups ids stamp seen acc)

let collect_decl_bucket ~compact bucket key spec summary decl stamp seen acc =
  match Overlap_key_table.find_opt bucket key with
  | Option.None -> acc
  | Option.Some inner -> (
      match Spec_table.find_opt inner spec with
      | Option.None -> acc
      | Option.Some index ->
          collect_selector_groups
            (collect_distinct_decls ~compact decl)
            index summary stamp seen acc)

(* A prior node whose own declaration is broad meets every key, so no single
   declaration of the querying node can rule its group out. *)
let collect_broad_bucket bucket spec summary stamp seen acc =
  match Overlap_key_table.find_opt bucket Shorthand.broad_overlap_key with
  | Option.None -> acc
  | Option.Some inner -> (
      match Spec_table.find_opt inner spec with
      | Option.None -> acc
      | Option.Some index ->
          collect_selector_groups collect_all_decls index summary stamp seen acc
      )

let rec collect_decl_keys ~compact bucket spec summary decl stamp seen acc =
  function
  | [] -> acc
  | key :: rest ->
      collect_decl_keys ~compact bucket spec summary decl stamp seen
        (collect_decl_bucket ~compact bucket key spec summary decl stamp seen
           acc)
        rest

let rec collect_decl_overlaps ~compact bucket spec summary stamp seen acc =
  function
  | [] -> acc
  | (ov : decl_overlap) :: rest ->
      collect_decl_overlaps ~compact bucket spec summary stamp seen
        (collect_decl_keys ~compact bucket spec summary ov.decl stamp seen acc
           ov.footprint)
        rest

let rec simple_subject_selector (selector : Selector.t) =
  match selector with
  | Class _ | Id _ | Element _ | Universal _ -> true
  | Compound selectors -> List.for_all simple_subject_selector selectors
  | _ -> false

let compact_rule = function [ { footprint = [ _ ]; _ } ] -> true | _ -> false

let rec collect_rule_overlaps bucket decls selectors summaries specificities
    stamp seen acc =
  match (selectors, summaries, specificities) with
  | [], [], [] -> acc
  | selector :: selectors, summary :: summaries, specificity :: specificities ->
      let spec = spec_key specificity in
      let compact = compact_rule decls && simple_subject_selector selector in
      let acc =
        collect_broad_bucket bucket spec summary stamp seen acc |> fun acc ->
        collect_decl_overlaps ~compact bucket spec summary stamp seen acc decls
      in
      collect_rule_overlaps bucket decls selectors summaries specificities stamp
        seen acc
  | _ -> invalid_arg "Rule_graph.collect_rule_overlaps"

let rec add_decl_keys ~compact bucket spec summary decl value = function
  | [] -> ()
  | key :: rest ->
      add_decl_bucket ~compact bucket key spec summary decl value;
      add_decl_keys ~compact bucket spec summary decl value rest

let rec add_decl_overlaps ~compact bucket spec summary value = function
  | [] -> ()
  | (ov : decl_overlap) :: rest ->
      add_decl_keys ~compact bucket spec summary ov.decl value ov.footprint;
      add_decl_overlaps ~compact bucket spec summary value rest

let rec add_rule_overlaps bucket decls selectors summaries specificities value =
  match (selectors, summaries, specificities) with
  | [], [], [] -> ()
  | selector :: selectors, summary :: summaries, specificity :: specificities ->
      let compact = compact_rule decls && simple_subject_selector selector in
      add_decl_overlaps ~compact bucket (spec_key specificity) summary value
        decls;
      add_rule_overlaps bucket decls selectors summaries specificities value
  | _ -> invalid_arg "Rule_graph.add_rule_overlaps"

let source_order_edges t =
  let n = t.count in
  let succ = Array.make n [] in
  let by_decl_key = Overlap_key_table.create 256 in
  let by_branch = String_table.create 256 in
  (* every prior node, bucketed by specificity: what a node carrying the broad
     key has to be checked against, since the broad key intersects every
     other *)
  let by_spec = Spec_table.create 64 in
  let seen = Array.make (max n 1) (-1) in
  for j = 0 to n - 1 do
    let keys = t.overlap_keys.(j) in
    let specs = distinct_spec_keys [] t.specificities.(j) in
    let candidates =
      (* A node carrying the broad key meets every prior declaration, so its own
         keys index nothing: only specificity narrows it. *)
      if overlap_key_in Shorthand.broad_overlap_key keys then
        collect_specs by_spec specs j seen []
      else
        (* probed per declaration rather than per distinct key: the key alone
           does not say which declaration of [j] faces the bucket, and that is
           what decides which group can never conflict *)
        collect_rule_overlaps by_decl_key t.decl_overlaps.(j) t.selectors.(j)
          t.selector_summaries.(j) t.specificities.(j) j seen []
    in
    let candidates =
      List.fold_left
        (fun acc branch -> collect_string_bucket by_branch branch j seen acc)
        candidates t.branches.(j)
    in
    List.iter
      (fun i ->
        match nodes_conflict_reason t i j with
        | Option.None -> ()
        | Option.Some reason -> succ.(i) <- (j, reason) :: succ.(i))
      candidates;
    add_rule_overlaps by_decl_key t.decl_overlaps.(j) t.selectors.(j)
      t.selector_summaries.(j) t.specificities.(j) j;
    List.iter
      (fun branch -> String_table.push by_branch branch j)
      t.branches.(j);
    add_spec_buckets by_spec j specs
  done;
  Array.map List.rev succ

let index_add tbl key node =
  String_table.replace tbl key
    (node :: Option.value ~default:[] (String_table.find_opt tbl key))

let decl_index_add tbl key node =
  Decl_tbl.replace tbl key
    (node :: Option.value ~default:[] (Decl_tbl.find_opt tbl key))

let key_inner t key =
  match Key_tbl.find_opt t.key_index key with
  | Some inner -> inner
  | None ->
      let inner = Decl_tbl.create 4 in
      Key_tbl.replace t.key_index key inner;
      inner

(* Add [node]'s overlap keys (partitioned by the declaration that writes each)
   and its branches to the graph's external index. *)
let index_node t (node : node_id) =
  let i = Node_id.to_int node in
  List.iter
    (fun (ov : decl_overlap) ->
      List.iter
        (fun key -> decl_index_add (key_inner t key) ov.decl node)
        ov.footprint)
    t.decl_overlaps.(i);
  List.iter (fun branch -> index_add t.branch_index branch node) t.branches.(i)

let of_rules ?parent ?(closed_world = false) (rules : rule list) : t =
  let rules = Array.of_list rules in
  let n = Array.length rules in
  let summaries = Array.map rule_summary rules in
  let effective = Array.map (effective_selector parent) rules in
  let selectors = Array.map Edge.selectors effective in
  let selector_summaries =
    Array.map (List.map Selector_summary.of_selector) selectors
  in
  let specificities = Array.map (List.map Selector.specificity) selectors in
  let branches = Array.map selector_branch_keys selectors in
  let decl_overlaps = Array.map rule_decl_overlaps rules in
  let overlap_keys = Array.map rule_overlap_keys decl_overlaps in
  let live = Array.make n true in
  let t =
    {
      generation = 0;
      closed_world;
      parent;
      count = n;
      rules;
      summaries;
      origin = Array.init n Fun.id;
      live;
      selectors;
      selector_summaries;
      specificities;
      branches;
      decl_overlaps;
      overlap_keys;
      succ = [||];
      key_index = Key_tbl.create (max 16 (n * 2));
      branch_index = String_table.create (max 16 (n * 2));
      reach = Unbuilt;
      expansions = 0;
    }
  in
  for i = 0 to n - 1 do
    index_node t (Node_id.of_int_exn i)
  done;
  { t with succ = source_order_edges t }

let node_count t = t.count
let node_rule t i = t.rules.(i)
let node_size t i = Summary.size t.summaries.(i)
let node_origin t i = t.origin.(i)
let is_live t i = t.live.(i)
let conflict t i j = nodes_conflict t i j
let order_constrained = conflict

(* Compact source-order chains preserve a constraint by reachability rather than
   by retaining every transitive edge. Dead rewrite inputs remain in the graph
   as relay vertices, so paths through them continue to constrain the surviving
   nodes. *)
let walk_to t source target =
  let seen = Bytes.make (max t.count 1) '\000' in
  let rec visit = function
    | [] -> false
    | i :: rest ->
        if i = target then true
        else if Bytes.get seen i <> '\000' then visit rest
        else begin
          Bytes.set seen i '\001';
          t.expansions <- t.expansions + 1;
          let next =
            List.fold_left (fun acc (j, _) -> j :: acc) rest t.succ.(i)
          in
          visit next
        end
  in
  visit [ source ]

(* A node's reachable set, one bit per node. The union below runs once per edge
   of the cone being closed, so it goes a word at a time. *)
module Reach = struct
  let v count = Bytes.make ((count + 7) / 8) '\000'
  let is_set set = Bytes.length set > 0

  let mem set i =
    Char.code (Bytes.get set (i lsr 3)) land (1 lsl (i land 7)) <> 0

  let add set i =
    let byte = i lsr 3 in
    Bytes.set set byte
      (Char.chr (Char.code (Bytes.get set byte) lor (1 lsl (i land 7))))

  let union ~into set =
    let len = Bytes.length into in
    let words = len / 8 in
    for w = 0 to words - 1 do
      let at = w * 8 in
      Bytes.set_int64_ne into at
        (Int64.logor (Bytes.get_int64_ne into at) (Bytes.get_int64_ne set at))
    done;
    for at = words * 8 to len - 1 do
      Bytes.set into at
        (Char.chr
           (Char.code (Bytes.get into at) lor Char.code (Bytes.get set at)))
    done
end

(* A run of [n] nodes closes into [n * n / 8] bytes. Past this length each
   question walks the edges again and the graph holds nothing. *)
let closure_node_limit = 16384

(* One step of the closure walk: [Open] descends into a node, [Close] unions the
   sets of its successors - by then settled - into its own. *)
type reach_step = Open of int | Close of int

let rec union_successors sets set = function
  | [] -> true
  | (j, _) :: rest ->
      let j = Node_id.to_int j in
      Reach.add set j;
      (* An unsettled successor at this point is a back edge, which the graph's
         acyclicity rules out: [of_rules] only ever points a node at a later
         one, and [rewrite] rejects a rewrite that would close a loop. Answering
         from an incomplete set would reorder declarations, so say so instead
         and let the caller fall back to walking. *)
      Reach.is_set sets.(j)
      &&
      (Reach.union ~into:set sets.(j);
       union_successors sets set rest)

let close_node t sets i =
  let set = Reach.v t.count in
  union_successors sets set t.succ.(i)
  && begin
    sets.(i) <- set;
    t.expansions <- t.expansions + 1;
    true
  end

(* Settle every node in [source]'s cone, deepest first, skipping what an earlier
   question already settled. The stack is explicit because a source-order chain
   is as deep as the run is long, and [opened] keeps a node reached twice within
   one cone from being descended twice. *)
let close_cone t sets source =
  let opened = Bytes.make (max t.count 1) '\000' in
  let rec loop = function
    | [] -> true
    | Close i :: rest -> close_node t sets i && loop rest
    | Open i :: rest ->
        if Reach.is_set sets.(i) || Bytes.get opened i <> '\000' then loop rest
        else begin
          Bytes.set opened i '\001';
          loop
            (List.fold_left
               (fun acc (j, _) -> Open (Node_id.to_int j) :: acc)
               (Close i :: rest) t.succ.(i))
        end
  in
  loop [ Open source ]

let reach_sets t =
  match t.reach with
  | Held sets -> Option.Some sets
  | Walked -> Option.None
  | Unbuilt ->
      if t.count > closure_node_limit then begin
        t.reach <- Walked;
        Option.None
      end
      else begin
        let sets = Array.make (max t.count 1) Bytes.empty in
        t.reach <- Held sets;
        Option.Some sets
      end

let reachable_set t source =
  match reach_sets t with
  | Option.None -> Option.None
  | Option.Some sets ->
      if Reach.is_set sets.(source) then Option.Some sets.(source)
      else if close_cone t sets source then Option.Some sets.(source)
      else begin
        t.reach <- Walked;
        Option.None
      end

let path_exists t source target =
  source <> target
  &&
  match reachable_set t source with
  | Option.Some set -> Reach.mem set target
  | Option.None -> walk_to t source target

let precedes t i j = t.live.(i) && t.live.(j) && path_exists t i j
let successors t i = List.map fst t.succ.(i)
let reachability_expansions t = t.expansions
let generation t = t.generation

let live_nodes t =
  let acc = ref [] in
  for i = node_count t - 1 downto 0 do
    if t.live.(i) then acc := i :: !acc
  done;
  !acc

(* Recursive over the index rather than a local closure over [t] and [f], so a
   scan that answers no everywhere allocates nothing at all. *)
let rec exists_live_from t f i =
  i < t.count
  && ((t.live.(i) && f (Node_id.of_int_exn i)) || exists_live_from t f (i + 1))

let exists_live_node t f = exists_live_from t f 0

(* The interned declaration-body hash list of a node, for bucketing nodes by
   identical declaration body. *)
let declaration_body_key t i =
  List.map Declaration.hash t.rules.(i).declarations

type topo_priority = { key : int; node : node_id }

module Topo_queue =
  Psq.Make
    (Node_id)
    (struct
      type t = topo_priority

      let compare left right =
        match Int.compare left.key right.key with
        | 0 -> Node_id.compare left.node right.node
        | order -> order
    end)

let topo_priority rank node = { key = rank node; node }

(* Kahn topological order over the whole graph, emitting only live nodes. Dead
   nodes stay as zero-output relays because a compact source-order chain can
   carry a live-to-live constraint through a node consumed by a rewrite. Among
   ready nodes, take the smallest [rank] key (ties broken by node id). *)
let topo_order_by t (rank : node_id -> int) : node_id array option =
  let live = live_nodes t in
  let n = List.length live in
  (* Node ids are dense, so the in-degree and emitted marks are arrays over the
     whole node space rather than generic tables keyed by id: both are read once
     per edge, and a generic [Hashtbl] hashes and structurally compares the key
     on every read. *)
  let slots = max (node_count t) 1 in
  let pred = Array.make slots 0 in
  for i = 0 to t.count - 1 do
    List.iter (fun (j, _) -> pred.(j) <- pred.(j) + 1) t.succ.(i)
  done;
  let out = Array.make n 0 in
  let emitted = Array.make slots false in
  let queue = ref Topo_queue.empty in
  for i = 0 to t.count - 1 do
    if pred.(i) = 0 then queue := Topo_queue.add i (topo_priority rank i) !queue
  done;
  let rec loop queue processed o =
    if processed = t.count then Option.Some out
    else
      match Topo_queue.pop queue with
      | Option.None -> Option.None
      | Option.Some ((e, _), queue) ->
          emitted.(e) <- true;
          let o =
            if t.live.(e) then begin
              out.(o) <- e;
              o + 1
            end
            else o
          in
          let queue =
            List.fold_left
              (fun queue (j, _) ->
                if not emitted.(j) then begin
                  let count = pred.(j) - 1 in
                  pred.(j) <- count;
                  if count = 0 then
                    Topo_queue.add j (topo_priority rank j) queue
                  else queue
                end
                else queue)
              queue t.succ.(e)
          in
          loop queue (processed + 1) o
  in
  loop !queue 0 0

let canonical_order_by t (rank : node_id -> int) : node_id array =
  match topo_order_by t rank with
  | Option.Some order -> order
  | Option.None -> invalid_arg "Rule_graph.canonical_order: cyclic live graph"

let canonical_order t : node_id array =
  canonical_order_by t (fun node -> t.origin.(node))

let canonical_linearization = canonical_order

(* Replace live nodes [consume] with [produce] as one transaction. Produced
   nodes inherit each consumed node's still-conflicting external edge (partial
   order preserved) and are mutually ordered by their position in [produce].
   Rejected ([Option.None]) if a consumed node is dead/stale or the result has a
   cycle (the factoring would break the cascade). [generation] bumps so stale
   captured candidates are detectable. *)
let duplicate_node xs =
  let sorted = List.sort Int.compare xs in
  let rec loop = function
    | [] | [ _ ] -> Option.None
    | a :: (b :: _ as rest) -> if a = b then Option.Some a else loop rest
  in
  loop sorted

let validate_consume t consume =
  let total = node_count t in
  match consume with
  | [] -> Error Empty_consume
  | _ -> (
      match duplicate_node consume with
      | Option.Some i -> Error (Duplicate_node i)
      | Option.None ->
          let rec loop = function
            | [] -> Ok ()
            | i :: rest ->
                if i < 0 || i >= total then Error (Invalid_node i)
                else if not t.live.(i) then Error (Stale_node i)
                else loop rest
          in
          loop consume)

let consumed_set total consume =
  let consumed = Array.make total false in
  List.iter (fun i -> consumed.(i) <- true) consume;
  consumed

let produced_origin t consume =
  match consume with
  | [] -> 0
  | first :: rest ->
      List.fold_left
        (fun min_origin id -> min min_origin t.origin.(id))
        t.origin.(first) rest

let produced_origins t consume produced_branches =
  let fallback = produced_origin t consume in
  Array.map
    (fun branches ->
      List.fold_left
        (fun origin consumed ->
          if share_branch branches t.branches.(consumed) then
            min origin t.origin.(consumed)
          else origin)
        max_int consume
      |> fun origin -> if origin = max_int then fallback else origin)
    produced_branches

(* [rewrite] has a single caller, which threads one graph and discards the
   parent as soon as a rewrite is accepted, so a produced graph can extend these
   append-only arrays in place and share them: the parent's [count] never covers
   the new slots, and a rejected rewrite simply leaves them to be overwritten by
   the next attempt. Growing geometrically turns a full copy of every array per
   rewrite into an amortised append. [live] and [succ] stay copies, since a
   rewrite mutates both and the parent must not see that. *)
let append_slack arr ~count (items : 'a array) =
  let added = Array.length items in
  let needed = count + added in
  let arr =
    if Array.length arr >= needed then arr
    else begin
      let grown =
        Array.make (max needed (2 * max 1 (Array.length arr))) items.(0)
      in
      Array.blit arr 0 grown 0 count;
      grown
    end
  in
  Array.blit items 0 arr count added;
  arr

let rewrite_live t ~total ~new_n ~consume =
  let live = Array.make new_n false in
  Array.blit t.live 0 live 0 total;
  List.iter (fun i -> live.(i) <- false) consume;
  for i = total to new_n - 1 do
    live.(i) <- true
  done;
  live

let produced_metadata t produced =
  let summaries = Array.map rule_summary produced in
  let effective = Array.map (effective_selector t.parent) produced in
  let selectors = Array.map Edge.selectors effective in
  let selector_summaries =
    Array.map (List.map Selector_summary.of_selector) selectors
  in
  let specificities = Array.map (List.map Selector.specificity) selectors in
  let branches = Array.map selector_branch_keys selectors in
  let decl_overlaps = Array.map rule_decl_overlaps produced in
  let overlap_keys = Array.map rule_overlap_keys decl_overlaps in
  ( summaries,
    selectors,
    selector_summaries,
    specificities,
    branches,
    decl_overlaps,
    overlap_keys )

let produced_graph t ~consume ~total ~new_n produced =
  let ( p_summaries,
        p_selectors,
        p_selector_summaries,
        p_specificities,
        p_branches,
        p_decl_overlaps,
        p_overlap_keys ) =
    produced_metadata t produced
  in
  {
    generation = t.generation + 1;
    closed_world = t.closed_world;
    parent = t.parent;
    count = new_n;
    rules = append_slack t.rules ~count:total produced;
    summaries = append_slack t.summaries ~count:total p_summaries;
    origin =
      append_slack t.origin ~count:total (produced_origins t consume p_branches);
    live = rewrite_live t ~total ~new_n ~consume;
    selectors = append_slack t.selectors ~count:total p_selectors;
    selector_summaries =
      append_slack t.selector_summaries ~count:total p_selector_summaries;
    specificities = append_slack t.specificities ~count:total p_specificities;
    branches = append_slack t.branches ~count:total p_branches;
    decl_overlaps = append_slack t.decl_overlaps ~count:total p_decl_overlaps;
    overlap_keys = append_slack t.overlap_keys ~count:total p_overlap_keys;
    succ =
      Array.append (Array.sub t.succ 0 total)
        (Array.make (Array.length produced) []);
    (* shared with [t]: holds the existing nodes; produced nodes are added by
       {!rewrite} only once the rewrite is accepted, so [add_external_edges]
       below sees the pre-rewrite index. *)
    key_index = t.key_index;
    branch_index = t.branch_index;
    (* the produced graph's edges are still being written; it holds no answers
       until it is published and questioned *)
    reach = Unbuilt;
    expansions = 0;
  }

let rewrite_base t ~consume ~produce :
    (int * bool array * t, rewrite_error) result =
  match validate_consume t consume with
  | Error _ as e -> e
  | Ok () ->
      if produce = [] then Error Empty_produce
      else
        let total = node_count t in
        let produced = Array.of_list produce in
        let new_n = total + Array.length produced in
        Ok
          ( total,
            consumed_set total consume,
            produced_graph t ~consume ~total ~new_n produced )

let add_edge succ i j reason = succ.(i) <- (j, reason) :: succ.(i)
let old_edge = path_exists

type orientation = Before | After | Ambiguous | New_conflict
type source_ref = { node : node_id; decl : int option }

let indexed_declarations (rule : rule) =
  List.mapi (fun i decl -> (i, decl)) rule.declarations

let produced_branch_sources graph consume produced =
  let sources =
    List.filter
      (fun consumed ->
        share_branch graph.branches.(produced) graph.branches.(consumed))
      consume
  in
  match sources with [] -> consume | _ -> sources

let declaration_source_refs graph nodes produced_decl ~matches =
  List.concat_map
    (fun node ->
      indexed_declarations graph.rules.(node)
      |> List.filter_map (fun (index, source_decl) ->
          if matches produced_decl source_decl then
            Option.Some { node; decl = Option.Some index }
          else Option.None))
    nodes

let fallback_source_refs nodes =
  List.map (fun node -> { node; decl = Option.None }) nodes

let refs_cover_nodes nodes refs =
  List.for_all
    (fun node -> List.exists (fun ref_ -> ref_.node = node) refs)
    nodes

let produced_declaration_sources graph consume produced produced_decl =
  let nodes = produced_branch_sources graph consume produced in
  match
    declaration_source_refs graph nodes produced_decl
      ~matches:Shorthand.same_minified_declaration
  with
  | _ :: _ as exact when refs_cover_nodes nodes exact -> exact
  | _ :: _ -> fallback_source_refs nodes
  | [] -> (
      match
        declaration_source_refs graph nodes produced_decl
          ~matches:(fun produced source ->
            Declaration.is_important produced = Declaration.is_important source
            && Shorthand.declaration_covers produced source)
      with
      | _ :: _ as covered when refs_cover_nodes nodes covered -> covered
      | _ :: _ -> fallback_source_refs nodes
      | [] -> fallback_source_refs nodes)

let source_ref_orientation t left right =
  if left.node = right.node then
    match (left.decl, right.decl) with
    | Option.Some l, Option.Some r when l < r -> Before
    | Option.Some l, Option.Some r when l > r -> After
    | _ -> New_conflict
  else
    match
      (old_edge t left.node right.node, old_edge t right.node left.node)
    with
    | true, false -> Before
    | false, true -> After
    | true, true -> Ambiguous
    | false, false -> New_conflict

let merge_orientation current next =
  match (current, next) with
  | New_conflict, other | other, New_conflict -> other
  | Before, Before | After, After -> current
  | Ambiguous, _ | _, Ambiguous -> Ambiguous
  | Before, After | After, Before -> Ambiguous

let source_sets_orientation t left right =
  List.fold_left
    (fun orientation left ->
      List.fold_left
        (fun orientation right ->
          merge_orientation orientation (source_ref_orientation t left right))
        orientation right)
    New_conflict left

let produced_external_orientation t graph consume produced external_ =
  List.fold_left
    (fun orientation produced_decl ->
      if
        List.exists
          (fun external_decl ->
            declarations_conflict produced_decl external_decl)
          graph.decl_overlaps.(external_)
      then
        let sources =
          produced_declaration_sources graph consume produced produced_decl.decl
        in
        let external_source = { node = external_; decl = Option.None } in
        merge_orientation orientation
          (source_sets_orientation t sources [ external_source ])
      else orientation)
    New_conflict
    graph.decl_overlaps.(produced)

let produced_pair_orientation t graph consume left right =
  List.fold_left
    (fun orientation left_decl ->
      List.fold_left
        (fun orientation right_decl ->
          if declarations_conflict left_decl right_decl then
            let left_sources =
              produced_declaration_sources graph consume left left_decl.decl
            in
            let right_sources =
              produced_declaration_sources graph consume right right_decl.decl
            in
            merge_orientation orientation
              (source_sets_orientation t left_sources right_sources)
          else orientation)
        orientation
        graph.decl_overlaps.(right))
    New_conflict graph.decl_overlaps.(left)

let inherited_orientation t consume k =
  match
    ( List.exists (fun c -> old_edge t c k) consume,
      List.exists (fun c -> old_edge t k c) consume )
  with
  | true, false -> Before
  | false, true -> After
  | true, true -> Ambiguous
  | false, false -> New_conflict

let add_inherited_edge t graph consume succ p k reason =
  let orientation =
    match reason with
    | Cascade_conflict -> produced_external_orientation t graph consume p k
    | Shared_branch_pin -> inherited_orientation t consume k
  in
  match orientation with
  | Before ->
      add_edge succ p k reason;
      Ok ()
  | After ->
      add_edge succ k p reason;
      Ok ()
  | Ambiguous ->
      Error (Ambiguous_external_order { produced = p; external_ = k })
  | New_conflict ->
      Error (New_external_conflict { produced = p; external_ = k })

(* Existing nodes a produced node [p] could conflict with: those sharing an
   overlap key (or the broad key, which intersects everything) or a selector
   branch with it, read from the graph's index instead of scanning all [total]
   nodes. [seen] is stamped with [p] (each produced node has a distinct index)
   to dedupe without re-clearing. A produced node carrying the broad key
   conflicts with everything, so it falls back to a full scan (rare: [all]). *)
let external_candidates graph ~total ~consumed ~seen p =
  let acc = ref [] in
  let push k =
    if k < total && graph.live.(k) && (not consumed.(k)) && seen.(k) <> p then begin
      seen.(k) <- p;
      acc := k :: !acc
    end
  in
  let keys = graph.overlap_keys.(p) in
  if overlap_key_in Shorthand.broad_overlap_key keys then
    for k = 0 to total - 1 do
      push k
    done
  else begin
    let push_ids ids = List.iter (fun id -> push (Node_id.to_int id)) ids in
    (* For each declaration [p] writes, collect, per overlap key it covers, the
       nodes whose declaration for that key differs - same-declaration nodes
       never conflict, so their (often large) bucket is skipped, not walked. *)
    List.iter
      (fun (ov : decl_overlap) ->
        List.iter
          (fun key ->
            match Key_tbl.find_opt graph.key_index key with
            | Some inner ->
                Decl_tbl.iter
                  (fun decl' ids ->
                    if not (Shorthand.same_minified_declaration ov.decl decl')
                    then push_ids ids)
                  inner
            | None -> ())
          ov.footprint)
      graph.decl_overlaps.(p);
    (* The broad key ([all]) conflicts with everything, so collect every node
       under it regardless of declaration. *)
    (match Key_tbl.find_opt graph.key_index Shorthand.broad_overlap_key with
    | Some inner -> Decl_tbl.iter (fun _ ids -> push_ids ids) inner
    | None -> ());
    List.iter
      (fun b ->
        match String_table.find_opt graph.branch_index b with
        | Some ids -> push_ids ids
        | None -> ())
      graph.branches.(p)
  end;
  (* the previous full scan visited externals in ascending index order; preserve
     that so the first orientation error (and thus scheduling) is identical *)
  List.sort Int.compare !acc

let add_external_edges t ~consume ~consumed ~total ~produced_count graph succ =
  let seen = Array.make (max total 1) (-1) in
  let rec loop_produced pi =
    if pi = produced_count then Ok ()
    else
      let p = total + pi in
      let rec loop_candidates = function
        | [] -> Ok ()
        | k :: rest -> (
            match nodes_conflict_reason graph p k with
            | Option.None -> loop_candidates rest
            | Option.Some reason -> (
                match add_inherited_edge t graph consume succ p k reason with
                | Ok () -> loop_candidates rest
                | Error _ as e -> e))
      in
      match
        loop_candidates (external_candidates graph ~total ~consumed ~seen p)
      with
      | Ok () -> loop_produced (pi + 1)
      | Error _ as e -> e
  in
  loop_produced 0

let add_produced_edge t graph consume succ left right reason =
  let orientation =
    match reason with
    | Cascade_conflict -> produced_pair_orientation t graph consume left right
    | Shared_branch_pin -> Before
  in
  match orientation with
  | Before ->
      add_edge succ left right reason;
      Ok ()
  | After ->
      add_edge succ right left reason;
      Ok ()
  | Ambiguous -> Error (Ambiguous_produced_order { left; right })
  | New_conflict -> Error (New_produced_conflict { left; right })

(* Prior produced nodes that share a declaration key or branch with [right] at a
   specificity that can tie, and so might conflict with it. A broad key forces a
   scan of every prior node at those specificities; otherwise the decl-key and
   branch buckets narrow the set further. *)
let collect_produced_candidates by_decl_key by_spec by_branch graph right seen
    keys specs =
  let candidates =
    if overlap_key_in Shorthand.broad_overlap_key keys then
      collect_specs by_spec specs right seen []
    else
      collect_bucket by_decl_key Shorthand.broad_overlap_key specs right seen []
  in
  let candidates =
    List.fold_left
      (fun acc key -> collect_bucket by_decl_key key specs right seen acc)
      candidates keys
  in
  List.fold_left
    (fun acc branch -> collect_string_bucket by_branch branch right seen acc)
    candidates graph.branches.(right)

let add_produced_edges t ~consume ~total ~produced_count graph succ =
  let by_decl_key = Overlap_key_table.create 256 in
  let by_branch = String_table.create 256 in
  let by_spec = Spec_table.create 16 in
  let seen = Array.make (max (total + produced_count) 1) (-1) in
  let rec loop pi =
    if pi = produced_count then Ok ()
    else
      let right = total + pi in
      let keys = graph.overlap_keys.(right) in
      let specs = distinct_spec_keys [] graph.specificities.(right) in
      let candidates =
        collect_produced_candidates by_decl_key by_spec by_branch graph right
          seen keys specs
      in
      let rec add_edges = function
        | [] ->
            List.iter (fun key -> add_bucket by_decl_key key specs right) keys;
            List.iter
              (fun branch -> String_table.push by_branch branch right)
              graph.branches.(right);
            add_spec_buckets by_spec right specs;
            loop (pi + 1)
        | left :: rest -> (
            match nodes_conflict_reason graph left right with
            | Option.None -> add_edges rest
            | Option.Some reason -> (
                match
                  add_produced_edge t graph consume succ left right reason
                with
                | Ok () -> add_edges rest
                | Error _ as e -> e))
      in
      add_edges candidates
  in
  loop 0

let rewrite_successors t ~consume ~consumed ~total graph :
    ((node_id * edge_reason) list array, rewrite_error) result =
  let produced_count = node_count graph - total in
  let succ = graph.succ in
  match
    add_external_edges t ~consume ~consumed ~total ~produced_count graph succ
  with
  | Error _ as e -> e
  | Ok () ->
      add_produced_edges t ~consume ~total ~produced_count graph succ
      |> Result.map (fun () -> succ)

(* Visit marks for the cycle DFS below. *)
let unvisited = '\000'
let on_stack = '\001'
let acyclic = '\002'

(* The graph is acyclic before a rewrite and every new edge touches a produced
   node ([total..node_count)), so any new cycle passes through one. A forward
   DFS from the produced nodes detects it visiting only their reachable cone
   (O(reachable) per commit, not the O(n) [topo_order]); shared state across the
   produced roots walks each node/edge at most once. The marks are a byte per
   node, read once per edge of that cone: node ids are dense, and a generic
   [Hashtbl] hashes and structurally compares the key on every probe. *)
let rewrite_introduces_cycle graph ~total =
  let n = node_count graph in
  let state = Bytes.make (max n 1) unvisited in
  let rec dfs i =
    let mark = Bytes.get state i in
    if mark = on_stack then true (* back edge to a node still on the stack *)
    else if mark = acyclic then false (* already proven acyclic *)
    else begin
      Bytes.set state i on_stack;
      let cyclic =
        List.exists (fun (j, _) -> dfs (Node_id.to_int j)) graph.succ.(i)
      in
      Bytes.set state i acyclic;
      cyclic
    end
  in
  let rec from_root p = p < n && (dfs p || from_root (p + 1)) in
  from_root total

let rewrite t ~consume ~produce : (t, rewrite_error) result =
  match rewrite_base t ~consume ~produce with
  | Error _ as e -> e
  | Ok (total, consumed, graph) -> (
      match rewrite_successors t ~consume ~consumed ~total graph with
      | Error _ as e -> e
      | Ok succ ->
          let graph = { graph with succ } in
          if rewrite_introduces_cycle graph ~total then Error Cycle
          else begin
            (* the rewrite is accepted: register its produced nodes in the
               shared index so later rewrites find them as external
               candidates *)
            for p = total to node_count graph - 1 do
              index_node graph (Node_id.of_int_exn p)
            done;
            Ok graph
          end)

let try_rewrite t ~consume ~produce : t option =
  match rewrite t ~consume ~produce with
  | Ok graph -> Option.Some graph
  | Error _ -> Option.None

let to_rules t : rule list =
  let order = canonical_order t in
  Array.to_list (Array.map (fun i -> t.rules.(i)) order)

let canonicalize t =
  of_rules ?parent:t.parent ~closed_world:t.closed_world (to_rules t)

let to_canonical_rules = to_rules
