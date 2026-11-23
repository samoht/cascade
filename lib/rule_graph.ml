(** Cascade-dependency graph over a run of flat style rules. *)

open Stylesheet
open Stdlib
open Conflict_index
module String_table = Common.Table.Make (String)
module Int_table = Common.Table.Make (Int)

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
  | Relay_cycle

(* Which nodes each node can reach, as a bitset per node. Orienting a factoring
   asks the question once per ordered pair of source references, so a run of [n]
   rules is asked about [n^2] pairs, while the edges are settled the moment a
   graph is published. Holding the answers turns the pairs into bit tests over
   one pass of the edges. [Unbuilt] until the first question; [Walked] for a run
   long enough that holding them costs more than re-asking. *)
type reachability = Unbuilt | Held of Bytes.t array | Walked

(* What the graphs one [of_rules] call starts share as they are rewritten from
   one another: [marks], a mark per node a walk can stamp instead of allocating
   a visited set over every node; [row], a reachable set to fill and read at
   once; [indexed], whether [key_index] and [branch_index] hold the live nodes,
   filled on the first rewrite since only a rewrite reads them; and [head], the
   generation the newest committed rewrite reached. Only a graph at [head] may
   be rewritten, because a rewrite writes its nodes into the slots past its
   parent's extent. *)
type lineage = {
  mutable indexed : bool;
  mutable head : int;
  mutable marks : int array;
  mutable stamp : int;
  mutable row : Bytes.t;
  order : Order_maintenance.t;
      (** every node the newest graph holds, consumed ones included, in the
          order it prints them: a topological order of its edges. *)
}

(* What a rule writes under one selector: that selector's branches as the order
   relation reads them, and the declarations written under it. A rule with
   nested content writes under several, each nested rule under its composed
   selector (CSS Nesting 1 sec. 3); the node's selector and declaration arrays
   hold their union, which is what the indexes find candidates by, and its parts
   what a conflict test reads. *)
type part = {
  branches : Selector.t list;
  summaries : Selector_summary.t list;
  weights : Selector.specificity list;
  writes : decl_overlap list;
  slots : Shorthand.overlap_key list;
}

type t = {
  generation : int;
  closed_world : bool;
      (** when set, two distinct selectors are assumed not to match a common
          element (a caller-asserted closed DOM), so they never cascade-conflict
          on selector grounds *)
  pin_shared_branches : bool;
      (** whether equal selector branches add conservative structural edges;
          optimizer rewrites need them, while a canonical projection over
          already-expanded rules does not *)
  agreeing_ties : bool;
      (** whether two rules whose declarations touching the other's are the same
          sequence record no conflict: an element both match takes that sequence
          twice whichever comes first, which a canonical projection reads as no
          order; optimizer rewrites keep the edge *)
  exclusive : (int -> int -> bool) option;
      (** a caller's assertion over the nodes {!of_rules} built, by index: the
          two rules never apply to one element together, so they never
          cascade-conflict. A node a rewrite produces has an index past those,
          and the caller's predicate says nothing about it. *)
  exclusive_count : int;  (** how many nodes [exclusive] speaks for *)
  parent : Selector.t option;
      (** the enclosing nesting context; every node's overlap is computed on its
          selector expanded against this, and rewrites reuse it so produced
          nodes are indexed the same way as existing ones. *)
  count : int;
      (** live extent of the node arrays. They are allocated with slack and
          appended to in place, so their length is a capacity, not a node count.
      *)
  mutable rules : rule array;  (** all nodes ever created, dead ones included *)
  size_print : Size.print;
      (** the flags the run prints with, which [sizes] are measured under *)
  mutable sizes : int array;
      (** each node's minified byte size, worked out once *)
  mutable selector_sizes : int array;
      (** the minified byte size of each node's selector, worked out once *)
  mutable origin : int array;
      (** each node's first source appearance, which [lineage.order] breaks ties
          on: an authored node's position, and for a produced node the earliest
          origin of the consumed nodes it shares a selector branch with, or of
          all of them *)
  mutable slot : int array;
      (** the source position where each node prints: the largest origin up to
          it in [lineage.order] *)
  mutable moved : node_id list;
      (** the live nodes the rewrite that made this graph moved in
          [lineage.order], besides the ones it produced *)
  mutable death : int array;
      (** the generation of the first graph a node is dead in, [max_int] while
          it lives. Shared across the lineage like [rules], so a graph reads
          liveness against its own [generation]. *)
  mutable selectors : Selector.t list array;
      (** effective selector branches for each node, after applying [parent] *)
  mutable selector_summaries : Selector_summary.t list array;
  mutable specificities : Selector.specificity list array;
      (** each node's per-branch specificity, computed once: the order test
          compares specificity on every candidate pair *)
  mutable branches : string list array;
  mutable decl_overlaps : decl_overlap list array;
      (** every declaration each node writes, at any depth *)
  mutable overlap_keys : Shorthand.overlap_key list array;
  mutable parts : part list array;
      (** what each node with nested content writes under each selector, its own
          run first; [[]] for a node whose body is its declarations, which the
          arrays above describe alone *)
  mutable succ : (node_id * edge_reason) list array;
      (** directed partial-order edges: [i] precedes every node reachable from
          [succ.(i)]. Simple dense runs store a transitive chain rather than one
          edge per conflicting pair. Edges follow creation order (source order
          for [of_rules], inherited order for rewrites). Shared across the
          lineage: a committed rewrite conses its edges onto the existing nodes'
          lists, and each of those points at a node it produced, so an edge to a
          node past [count] belongs to a later graph and a graph skips it. *)
  key_index : node_id list Decl_tbl.t Slot_table.t;
      (** overlap key and branch specificity -> (declaration writing it -> node
          ids), so a rewrite finds an external node's potential conflicts
          without scanning every node, without the nodes no branch of which ties
          with it, and {e without even iterating} the nodes that write the same
          declaration (which never conflict). Shared along the lineage and
          filled as rewrites are accepted; never pruned, dead nodes are skipped
          at query time. *)
  branch_index : node_id list String_table.t;
      (** selector branch -> node ids that carry it; same role as [key_index]
          for the [share_branch] dimension. *)
  lineage : lineage;
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
  Shorthand.declarations_conflict_with_keys left.decl left.footprint right.decl
    right.footprint

(* Plain recursion, so the pair tests below allocate no closure per element:
   they run on every candidate pair. *)
let rec declaration_conflicts_with a = function
  | [] -> false
  | b :: rest -> declarations_conflict a b || declaration_conflicts_with a rest

let rec decls_order_conflict d1 d2 =
  match d1 with
  | [] -> false
  | a :: rest -> declaration_conflicts_with a d2 || decls_order_conflict rest d2

(* [List.exists] over three parallel lists walked in step, one entry per
   selector branch, without zipping them. *)
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
          Selector.equal_specificity specificity_a specificity_b
          &&
          if closed_world then
            (* the caller asserts no element matches two distinct selectors, so
               only an identical selector still ties on the same element *)
            selector_a = selector_b
          else Selector_summary.may_overlap summary_a summary_b)
        selectors_b summaries_b specificities_b)
    selectors_a summaries_a specificities_a

let selector_branch_key selector =
  Pp.to_string ~minify:true Selector.pp (Selector.canonicalize selector)

(* The keys of a rule's comma branches, sorted, so [.foo,.bar] and [.bar,.foo]
   have one list and [share_branch] can merge-walk two of them. *)
let selector_branch_keys selectors =
  selectors |> List.map selector_branch_key |> List.sort String.compare

(* Two rules share a selector branch when a factoring produced one as a residual
   of the other (e.g. [.a] and [.a,.b]) or they are the same selector. Such
   rules are pinned so a factored group stays contiguous and its declaration
   order is left to the declaration canonicalization.

   Both arguments come from [selector_branch_keys], which sorts, so this walks
   the two in step. *)
let rec share_branch a b =
  match (a, b) with
  | [], _ | _, [] -> false
  | x :: xs, y :: ys ->
      let c = String.compare x y in
      if c = 0 then true
      else if c < 0 then share_branch xs b
      else share_branch a ys

let selector_list = function
  | [] -> invalid_arg "Rule_graph.selector_list"
  | [ sel ] -> sel
  | sels -> Selector.List sels

(* The effective selector a rule's overlap is computed against: under a nesting
   [parent], a relative nested selector ([&:hover], [> .icon]) is expanded to
   its parent-qualified form so the conflict model uses the real subject. *)
let selector_under (parent : Selector.t option) selector =
  match parent with
  | None -> selector
  | Some parent ->
      let parents = Edge.selectors parent in
      let children = Edge.selectors selector in
      List.concat_map
        (fun parent -> List.map (Nest.combine parent) children)
        parents
      |> selector_list

let effective_selector parent (r : rule) = selector_under parent r.selector

(* The cascade half of the order relation, over each rule's overlap keys,
   declaration footprints and selector branches: the one decision every caller
   asking whether two rules conflict reaches, through the graph or through
   [rules_conflict]. The key test is the cheap first cut: two rules whose
   footprints meet nowhere write no common slot. *)
let declarations_order_conflict keys_a decls_a keys_b decls_b =
  Shorthand.overlap_keys_intersect keys_a keys_b
  && decls_order_conflict decls_a decls_b

let cascade_conflict ~closed_world keys_a decls_a selectors_a summaries_a
    specificities_a keys_b decls_b selectors_b summaries_b specificities_b =
  declarations_order_conflict keys_a decls_a keys_b decls_b
  && selectors_order_conflict ~closed_world selectors_a summaries_a
       specificities_a selectors_b summaries_b specificities_b

(* Conflict between two nodes of a graph by their stored summaries/branches. *)
let asserted_exclusive t i j =
  match t.exclusive with
  | None -> false
  | Some exclusive ->
      i < t.exclusive_count && j < t.exclusive_count && exclusive i j

(* [x] and [y] write one value to the same slots at one importance, as
   [-moz-transform] and [-webkit-transform] can: either order ends at it. *)
let same_write (x : decl_overlap) (y : decl_overlap) =
  Declaration.same_minified x.decl y.decl
  || (not (Declaration.same_property x.decl y.decl))
     && Bool.equal
          (Declaration.is_important x.decl)
          (Declaration.is_important y.decl)
     && List.equal Shorthand.overlap_key_equal x.footprint y.footprint
     && String.equal
          (Declaration.string_of_value ~minify:true x.decl)
          (Declaration.string_of_value ~minify:true y.decl)

(* Every declaration of [a] meeting one of [b] writes what it writes, so a slot
   both write ends at one value whichever rule comes first. *)
let writes_one_value a b =
  List.for_all
    (fun (x : decl_overlap) ->
      List.for_all
        (fun (y : decl_overlap) ->
          (not
             (Shorthand.declarations_overlap_with_keys x.decl x.footprint y.decl
                y.footprint))
          || same_write x y)
        b)
    a

(* Two rules whose declarations touching the other's are the same sequence: on
   an element both match, every slot both write takes that sequence twice,
   whichever comes first, and each one's other writes meet nothing of the
   other's. The two sequences are walked side by side, so a pair that differs
   stops at the first declaration where they part. *)
let writes_agree a b =
  let touches others (x : decl_overlap) =
    List.exists
      (fun (y : decl_overlap) ->
        Shorthand.declarations_overlap_with_keys x.decl x.footprint y.decl
          y.footprint)
      others
  in
  let rec next others = function
    | [] -> Option.None
    | x :: rest ->
        if touches others x then Option.Some (x, rest) else next others rest
  in
  let rec walk xs ys =
    match (next b xs, next a ys) with
    | Option.None, Option.None -> true
    | Option.Some (x, xs), Option.Some (y, ys) ->
        Declaration.same_minified x.decl y.decl && walk xs ys
    | Option.Some _, Option.None | Option.None, Option.Some _ -> false
  in
  walk a b || writes_one_value a b

let part_conflict ~closed_world (a : part) (b : part) =
  cascade_conflict ~closed_world a.slots a.writes a.branches a.summaries
    a.weights b.slots b.writes b.branches b.summaries b.weights

let node_part t i =
  {
    branches = t.selectors.(i);
    summaries = t.selector_summaries.(i);
    weights = t.specificities.(i);
    writes = t.decl_overlaps.(i);
    slots = t.overlap_keys.(i);
  }

let node_parts t i = match t.parts.(i) with [] -> [ node_part t i ] | ps -> ps
let has_no_parts t i = match t.parts.(i) with [] -> true | _ :: _ -> false

(* Two nodes conflict when some selector of one and some selector of the other
   tie on an element and what each writes under it meets. A node with nested
   content is read part by part: the union of its selectors against the union of
   its declarations would pair a selector with declarations written under
   another one. *)
let nodes_cascade_conflict t i j =
  match (t.parts.(i), t.parts.(j)) with
  | [], [] ->
      cascade_conflict ~closed_world:t.closed_world t.overlap_keys.(i)
        t.decl_overlaps.(i) t.selectors.(i) t.selector_summaries.(i)
        t.specificities.(i) t.overlap_keys.(j) t.decl_overlaps.(j)
        t.selectors.(j) t.selector_summaries.(j) t.specificities.(j)
      && not
           (t.agreeing_ties
           && writes_agree t.decl_overlaps.(i) t.decl_overlaps.(j))
  | _ ->
      Shorthand.overlap_keys_intersect t.overlap_keys.(i) t.overlap_keys.(j)
      &&
      let parts_j = node_parts t j in
      List.exists
        (fun a ->
          List.exists (part_conflict ~closed_world:t.closed_world a) parts_j)
        (node_parts t i)

let nodes_conflict_reason t i j =
  if t.pin_shared_branches && share_branch t.branches.(i) t.branches.(j) then
    Some Shared_branch_pin
  else if asserted_exclusive t i j then None
  else if nodes_cascade_conflict t i j then Some Cascade_conflict
  else None

let nodes_conflict t i j = nodes_conflict_reason t i j <> None

let declaration_overlap decl =
  { decl; footprint = Shorthand.declaration_overlap_keys decl }

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

(* A rule's distinct overlap keys, sorted: {!Shorthand.overlap_keys_intersect}
   reads this list once per candidate pair and merge-walks it. Sorted in one
   array, since deduplicating against the accumulator scanned it once per key,
   and [Array.sort] is in place. *)
let rule_overlap_keys decls =
  let total = footprint_total 0 decls in
  if total = 0 then []
  else begin
    let keys = Array.make total Shorthand.broad_overlap_key in
    fill_footprints keys 0 decls;
    Array.sort Shorthand.overlap_key_compare keys;
    distinct_keys keys (total - 1) []
  end

(* What the order relation reads off a selector: each branch with its summary
   and its specificity. *)
let branch_facts selector =
  let branches = Edge.selectors selector in
  ( branches,
    List.map Selector_summary.of_selector branches,
    List.map Selector.specificity branches )

type branches =
  Selector.t list * Selector_summary.t list * Selector.specificity list

let branches = branch_facts

(* The branches node [i] writes under, composed with the graph's parent: its
   selector's and, for a rule with nested content, every nested rule's. *)
let node_branch_facts t i =
  (t.selectors.(i), t.selector_summaries.(i), t.specificities.(i))

let node_selector_conflict t (branches_a, summaries_a, specificities_a) i =
  let branches_b, summaries_b, specificities_b = node_branch_facts t i in
  selectors_order_conflict ~closed_world:false branches_a summaries_a
    specificities_a branches_b summaries_b specificities_b

let node_selectors_conflict t i j =
  node_selector_conflict t (node_branch_facts t i) j

let part_declarations (part : part) =
  List.map (fun (d : decl_overlap) -> d.decl) part.writes

let node_writes_cross t branches crosses i =
  match t.parts.(i) with
  | [] ->
      node_selector_conflict t branches i && crosses t.rules.(i).declarations
  | parts ->
      let selectors_a, summaries_a, specificities_a = branches in
      List.exists
        (fun (part : part) ->
          selectors_order_conflict ~closed_world:false selectors_a summaries_a
            specificities_a part.branches part.summaries part.weights
          && crosses (part_declarations part))
        parts

let branches_conflict (branches_a, summaries_a, specificities_a)
    (branches_b, summaries_b, specificities_b) =
  match (summaries_a, specificities_a, summaries_b, specificities_b) with
  | [ summary_a ], [ specificity_a ], [ summary_b ], [ specificity_b ] ->
      (* one branch each, the common case, asked without a closure *)
      Selector.equal_specificity specificity_a specificity_b
      && Selector_summary.may_overlap summary_a summary_b
  | _ ->
      selectors_order_conflict ~closed_world:false branches_a summaries_a
        specificities_a branches_b summaries_b specificities_b

let branches_under ?parent selector =
  branch_facts (selector_under parent selector)

let selector_branches t selector = branches_under ?parent:t.parent selector
let branch_specificities (_, _, specificities) = specificities

(* What the order relation reads off one rule, read once so a caller asking
   about many pairs pays for each rule once rather than once per pair. The
   footprints are read only for a pair whose selectors may tie: without a graph
   to index them, most pairs a caller asks about are settled by the selectors
   alone, and a footprint costs more to read than a selector. *)
type facts = {
  selector_branches : Selector.t list;
  branch_weights : Selector.specificity list;
  branch_summaries : Selector_summary.t list Lazy.t;
  footprints : (Shorthand.overlap_key list * decl_overlap list) Lazy.t;
  nested : facts list;
      (** each rule the body holds at any depth, under its composed selector *)
}

let selector_facts selector (declarations : Declaration.declaration list) =
  let selector_branches = Edge.selectors selector in
  {
    selector_branches;
    branch_weights = List.map Selector.specificity selector_branches;
    branch_summaries =
      lazy (List.map Selector_summary.of_selector selector_branches);
    footprints =
      lazy
        (let decls = List.map declaration_overlap declarations in
         (rule_overlap_keys decls, decls));
    nested = [];
  }

(* The rules a body holds, at any depth, under the selectors the cascade weighs
   them by. *)
let nested_leaves selector nested =
  match Nest.leaves ~parent:selector nested with
  | Some leaves -> leaves
  | None ->
      invalid_arg "Rule_graph: a nested rule with no selector to weigh it by"

let weighable ?parent (rule : rule) =
  match rule.nested with
  | [] -> true
  | nested ->
      Option.is_some
        (Nest.leaves ~parent:(effective_selector parent rule) nested)

let facts ?parent (rule : rule) =
  let selector = effective_selector parent rule in
  let own = selector_facts selector rule.declarations in
  match rule.nested with
  | [] -> own
  | nested ->
      {
        own with
        nested =
          List.map
            (fun (leaf : rule) ->
              selector_facts leaf.selector leaf.declarations)
            (nested_leaves selector nested);
      }

let rec weights_tie a = function
  | [] -> false
  | w :: rest ->
      List.exists (Selector.equal_specificity w) a || weights_tie a rest

(* [cascade_conflict]'s two halves, the selector one asked first, and within it
   the weights, which settle most pairs before a summary is read. *)
let selector_facts_conflict a b =
  weights_tie a.branch_weights b.branch_weights
  && selectors_order_conflict ~closed_world:false a.selector_branches
       (Lazy.force a.branch_summaries)
       a.branch_weights b.selector_branches
       (Lazy.force b.branch_summaries)
       b.branch_weights
  &&
  let keys_a, decls_a = Lazy.force a.footprints in
  let keys_b, decls_b = Lazy.force b.footprints in
  declarations_order_conflict keys_a decls_a keys_b decls_b

(* A rule with nested content is read selector by selector, as {!part_conflict}
   reads a node. *)
let facts_conflict a b =
  match (a.nested, b.nested) with
  | [], [] -> selector_facts_conflict a b
  | _ ->
      List.exists
        (fun x -> List.exists (selector_facts_conflict x) (b :: b.nested))
        (a :: a.nested)

(* The specificities and overlap keys of everything [rule] writes, which an
   index files it under. *)
let facts_specs rule =
  List.fold_left
    (fun acc f -> distinct_spec_keys acc f.branch_weights)
    (distinct_spec_keys [] rule.branch_weights)
    rule.nested

let facts_keys rule =
  match rule.nested with
  | [] -> fst (Lazy.force rule.footprints)
  | nested ->
      rule_overlap_keys
        (List.concat_map
           (fun f -> snd (Lazy.force f.footprints))
           (rule :: nested))

let rules_conflict ?parent a b =
  facts_conflict (facts ?parent a) (facts ?parent b)

type facts_index = facts positioned

let facts_index = positioned

let index_facts index ~at rule =
  add_at index ~at ~specs:(facts_specs rule) ~keys:(facts_keys rule) rule

let conflicts_after index ~after rule =
  exists_after index ~after ~specs:(facts_specs rule) ~keys:(facts_keys rule)
    (facts_conflict rule)

let collect_string_bucket bucket key stamp seen acc =
  String_table.find_opt bucket key
  |> Option.value ~default:[]
  |> add_candidates stamp seen acc

(* A run of [n] nodes closes into [n * n / 8] bytes. Past this length each
   question walks the edges again and the graph holds nothing. *)
let closure_node_limit = 16384

(* Every conflicting pair as its own edge stores the same order twice over: on a
   real sheet the relation is a union of dense cliques, and a clique over nodes
   that already sit in source order has the same reachability as the chain
   through it. So walk the candidates nearest first and keep [covered], the
   ancestors of everything already linked: a candidate in there already reaches
   [j] through an edge we hold, so its own edge adds no constraint and - the
   point of doing this here - it never reaches [nodes_conflict_reason]. The
   reduction is exact rather than a narrowing: reachability is identical and
   only the redundant edges are gone. [covered] is [anc.(j)], so it ends the
   walk holding [j]'s ancestors for a later target to read.

   A node [relays] refuses may leave the graph later, and an edge the reduction
   dropped because a path through it carried the order would leave with it. So
   its ancestors never join a set through it: [anc.(i)] holds the nodes that
   reach [i] through relays only, and every edge it lets go stays implied once
   the nodes that are not relays are gone. *)
let link_candidates t succ anc ~reduce ~relays j candidates =
  let link i reason = succ.(i) <- (j, reason) :: succ.(i) in
  if not reduce then
    List.iter
      (fun i ->
        match nodes_conflict_reason t i j with
        | None -> ()
        | Some reason -> link i reason)
      candidates
  else begin
    let covered = anc.(j) in
    List.iter
      (fun i ->
        if not (Reach.mem covered i) then
          match nodes_conflict_reason t i j with
          | None -> ()
          | Some reason ->
              link i reason;
              if relays i then Reach.union ~into:covered anc.(i);
              Reach.add covered i)
      (List.sort (fun a b -> Int.compare b a) candidates)
  end

let index_candidates t index ~relay j =
  Conflict_index.candidates index ~relay ~keys:t.overlap_keys.(j)
    t.decl_overlaps.(j) t.selectors.(j) t.selector_summaries.(j)
    t.specificities.(j) j

let index_insert t index ~relay j =
  Conflict_index.insert index ~relay t.decl_overlaps.(j) t.selectors.(j)
    t.selector_summaries.(j) t.specificities.(j) j

(* The edges of [t]'s nodes in source order, and the index that found them,
   holding every node. *)
let source_order_edges ?(relays = fun _ -> true) t =
  let n = t.count in
  let succ = Array.make n [] in
  let reduce = n <= closure_node_limit in
  let anc = if reduce then Array.init n (fun _ -> Reach.v n) else [||] in
  let index = Conflict_index.v n in
  let by_branch = String_table.create 256 in
  for j = 0 to n - 1 do
    (* The compact runs a relay joins assume two of their nodes conflict exactly
       when their one declaration differs, which a node read part by part does
       not keep. *)
    let relay = relays j && has_no_parts t j in
    let candidates = index_candidates t index ~relay j in
    let candidates =
      if t.pin_shared_branches then
        List.fold_left
          (fun acc branch ->
            collect_string_bucket by_branch branch j
              (Conflict_index.seen index)
              acc)
          candidates t.branches.(j)
      else candidates
    in
    link_candidates t succ anc ~reduce ~relays j candidates;
    index_insert t index ~relay j;
    if t.pin_shared_branches then
      List.iter
        (fun branch -> String_table.push by_branch branch j)
        t.branches.(j)
  done;
  (Array.map List.rev succ, index)

let index_add tbl key node =
  String_table.replace tbl key
    (node :: Option.value ~default:[] (String_table.find_opt tbl key))

let decl_index_add tbl key node =
  Decl_tbl.replace tbl key
    (node :: Option.value ~default:[] (Decl_tbl.find_opt tbl key))

let key_inner t key spec =
  let slot : slot = { key; spec } in
  match Slot_table.find_opt t.key_index slot with
  | Some inner -> inner
  | None ->
      let inner = Decl_tbl.create 4 in
      Slot_table.replace t.key_index slot inner;
      inner

(* Add [node]'s overlap keys, partitioned by the specificity of each of its
   branches and by the declaration that writes each, and its branches to the
   graph's external index. *)
let index_node t (node : node_id) =
  let i = Node_id.to_int node in
  let specs = distinct_spec_keys [] t.specificities.(i) in
  List.iter
    (fun (ov : decl_overlap) ->
      List.iter
        (fun key ->
          List.iter
            (fun spec -> decl_index_add (key_inner t key spec) ov.decl node)
            specs)
        ov.footprint)
    t.decl_overlaps.(i);
  List.iter (fun branch -> index_add t.branch_index branch node) t.branches.(i)

let part selector (declarations : Declaration.declaration list) =
  let branches, summaries, weights = branch_facts selector in
  let writes = List.map declaration_overlap declarations in
  { branches; summaries; weights; writes; slots = rule_overlap_keys writes }

(* What the order relation reads off one rule: the branches of its selector,
   which its selector branch keys are read off, then the union of what it writes
   under every selector and the parts that union is made of, [[]] for a rule
   with no nested content. *)
type rule_facts = {
  own : branches;
  all : branches;
  decls : decl_overlap list;
  keys : Shorthand.overlap_key list;
  rule_parts : part list;
}

let rule_facts parent (rule : rule) =
  let selector = effective_selector parent rule in
  match rule.nested with
  | [] ->
      let own = branch_facts selector in
      let decls = rule_decl_overlaps rule in
      { own; all = own; decls; keys = rule_overlap_keys decls; rule_parts = [] }
  | nested ->
      let own = part selector rule.declarations in
      let leaves =
        List.map
          (fun (leaf : rule) -> part leaf.selector leaf.declarations)
          (nested_leaves selector nested)
      in
      let parts = own :: leaves in
      let decls = List.concat_map (fun (p : part) -> p.writes) parts in
      {
        own = (own.branches, own.summaries, own.weights);
        all =
          ( List.concat_map (fun (p : part) -> p.branches) parts,
            List.concat_map (fun (p : part) -> p.summaries) parts,
            List.concat_map (fun (p : part) -> p.weights) parts );
        decls;
        keys = rule_overlap_keys decls;
        rule_parts = parts;
      }

let own_branch_keys (facts : rule_facts) =
  let branches, _, _ = facts.own in
  selector_branch_keys branches

(* What the order relation reads off each of [rules]. *)
let rules_facts parent rules =
  let facts = Array.map (rule_facts parent) rules in
  let pick f = Array.map f facts in
  ( pick (fun { all = branches, _, _; _ } -> branches),
    pick (fun { all = _, summaries, _; _ } -> summaries),
    pick (fun { all = _, _, specificities; _ } -> specificities),
    pick (fun f -> f.decls),
    pick (fun f -> f.keys),
    pick own_branch_keys,
    pick (fun f -> f.rule_parts) )

(* The nodes of [rules] in source order with their edges, their [sizes], and an
   empty external index sized for [index_capacity] entries, which the first
   rewrite fills. *)
let ordered_nodes ~size_print ~selector_sizes ~sizes ~index_capacity ?parent
    ?(closed_world = false) ?(pin_shared_branches = true) ?exclusive
    (rules : rule array) : t =
  let n = Array.length rules in
  let ( selectors,
        selector_summaries,
        specificities,
        decl_overlaps,
        overlap_keys,
        branches,
        parts ) =
    rules_facts parent rules
  in
  let t =
    {
      generation = 0;
      size_print;
      closed_world;
      pin_shared_branches;
      agreeing_ties = false;
      exclusive;
      exclusive_count = n;
      parent;
      count = n;
      rules;
      sizes;
      selector_sizes;
      origin = Array.init n Fun.id;
      slot = Array.init n Fun.id;
      moved = [];
      death = Array.make n max_int;
      selectors;
      selector_summaries;
      specificities;
      branches;
      decl_overlaps;
      overlap_keys;
      parts;
      succ = [||];
      key_index = Slot_table.create index_capacity;
      branch_index = String_table.create index_capacity;
      lineage =
        {
          indexed = false;
          head = 0;
          marks = [||];
          stamp = 0;
          row = Bytes.empty;
          order = Order_maintenance.v n;
        };
      reach = Unbuilt;
      expansions = 0;
    }
  in
  { t with succ = fst (source_order_edges t) }

let of_rules ?(size_print = Size.default) ?parent ?closed_world
    ?pin_shared_branches ?exclusive rules =
  let rules = Array.of_list rules in
  let selector_sizes =
    Array.map (fun (rule : rule) -> Size.selector rule.selector) rules
  in
  ordered_nodes ~size_print ~selector_sizes
    ~sizes:
      (Array.mapi
         (fun i (rule : rule) ->
           selector_sizes.(i) + Size.block size_print rule)
         rules)
    ~index_capacity:(max 16 (Array.length rules * 2))
    ?parent ?closed_world ?pin_shared_branches ?exclusive rules

let size_print t = t.size_print
let node_count t = t.count
let node_rule t i = t.rules.(i)
let node_size t i = t.sizes.(i)
let node_selector_size t i = t.selector_sizes.(i)
let node_branches t i = t.branches.(i)
let node_slot t i = t.slot.(i)
let node_origin t i = t.origin.(i)
let compare_position t i j = Order_maintenance.compare t.lineage.order i j
let moved t = t.moved

(* [i] is a node of [t] that no rewrite up to [t] consumed. *)
let alive t i = t.death.(i) > t.generation
let is_live t i = i < t.count && alive t i
let conflict t i j = nodes_conflict t i j

(* A fresh stamp for [t]'s nodes in the lineage's marks: a walk marks a node
   visited by writing the stamp and asks by comparing with it, so it never
   clears what an earlier walk left. *)
let fresh_stamp t =
  let lineage = t.lineage in
  let room = Array.length lineage.marks in
  if room < t.count then begin
    let marks = Array.make (2 * t.count) (-1) in
    Array.blit lineage.marks 0 marks 0 room;
    lineage.marks <- marks
  end;
  lineage.stamp <- lineage.stamp + 1;
  lineage.stamp

(* Whether [i] was not yet marked with [stamp], marking it. *)
let first_visit t stamp i =
  let marks = t.lineage.marks in
  marks.(i) <> stamp
  && begin
    marks.(i) <- stamp;
    true
  end

(* Compact source-order chains preserve a constraint by reachability rather than
   by retaining every transitive edge. Dead rewrite inputs remain in the graph
   as relay vertices, so paths through them continue to constrain the surviving
   nodes. *)
let rec walk t stamp ~stop = function
  | [] -> false
  | i :: rest ->
      if stop i then true
      else if not (first_visit t stamp i) then walk t stamp ~stop rest
      else begin
        t.expansions <- t.expansions + 1;
        walk t stamp ~stop
          (List.fold_left
             (fun acc (j, _) -> if j < t.count then j :: acc else acc)
             rest t.succ.(i))
      end

let walk_to t source target =
  walk t (fresh_stamp t) ~stop:(Int.equal target) [ source ]

(* One step of the closure walk: [Open] descends into a node, [Close] unions the
   sets of its successors - by then settled - into its own. *)
type reach_step = Open of int | Close of int

let rec union_successors t sets set = function
  | [] -> true
  | (j, _) :: rest when j >= t.count -> union_successors t sets set rest
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
       union_successors t sets set rest)

let close_node t sets i =
  let set = Reach.row sets in
  union_successors t sets set t.succ.(i)
  && begin
    sets.(i) <- set;
    t.expansions <- t.expansions + 1;
    true
  end

(* Settle every node in [source]'s cone, deepest first, skipping what an earlier
   question already settled. The stack is explicit because a source-order chain
   is as deep as the run is long, and the stamp keeps a node reached twice
   within one cone from being descended twice. *)
let close_cone t sets source =
  let stamp = fresh_stamp t in
  let rec loop = function
    | [] -> true
    | Close i :: rest -> close_node t sets i && loop rest
    | Open i :: rest ->
        if Reach.is_set sets.(i) || not (first_visit t stamp i) then loop rest
        else
          loop
            (List.fold_left
               (fun acc (j, _) ->
                 if j < t.count then Open (Node_id.to_int j) :: acc else acc)
               (Close i :: rest) t.succ.(i))
  in
  loop [ Open source ]

let reach_sets t =
  match t.reach with
  | Held sets -> Some sets
  | Walked -> None
  | Unbuilt ->
      if t.count > closure_node_limit then begin
        t.reach <- Walked;
        None
      end
      else begin
        let sets = Reach.table t.count in
        t.reach <- Held sets;
        Some sets
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
  | Some set -> Reach.mem set target
  | None -> walk_to t source target

let precedes t i j = is_live t i && is_live t j && path_exists t i j

(* Whether a node is one of [sources] or reached from one, read before the next
   question to [t]: the union of the sources' sets when [t] holds them, filled
   into the lineage's row, else one walk from all of them. *)
let reached_from t sources =
  match reach_sets t with
  | Some sets
    when List.for_all (fun b -> Option.is_some (reachable_set t b)) sources ->
      let lineage = t.lineage in
      let width = Array.length sets / 8 in
      if Bytes.length lineage.row <> width then
        lineage.row <- Bytes.make width '\000'
      else Bytes.fill lineage.row 0 width '\000';
      let row = lineage.row in
      List.iter
        (fun b ->
          Reach.add row b;
          Reach.union ~into:row sets.(b))
        sources;
      Reach.mem row
  | _ ->
      let stamp = fresh_stamp t in
      ignore (walk t stamp ~stop:(fun _ -> false) sources);
      let marks = t.lineage.marks in
      fun a -> marks.(a) = stamp

let successors t i =
  List.filter_map
    (fun (j, _) -> if j < t.count then Some j else None)
    t.succ.(i)

let reachability_expansions t = t.expansions
let generation t = t.generation

let live_nodes t =
  let acc = ref [] in
  for i = node_count t - 1 downto 0 do
    if alive t i then acc := i :: !acc
  done;
  !acc

(* Recursive over the index rather than a local closure over [t] and [f], so a
   scan that answers no everywhere allocates nothing at all. *)
let rec exists_live_from t f i =
  i < t.count
  && ((alive t i && f (Node_id.of_int_exn i)) || exists_live_from t f (i + 1))

let exists_live_node t f = exists_live_from t f 0

let exists_live_between t lo hi f =
  Order_maintenance.exists_between t.lineage.order lo hi (fun i ->
      i < t.count && alive t i && f i)

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
     node space, read once per edge. *)
  let slots = max (node_count t) 1 in
  let pred = Array.make slots 0 in
  for i = 0 to t.count - 1 do
    List.iter
      (fun (j, _) -> if j < t.count then pred.(j) <- pred.(j) + 1)
      t.succ.(i)
  done;
  let out = Array.make n 0 in
  let emitted = Array.make slots false in
  let queue = ref Topo_queue.empty in
  for i = 0 to t.count - 1 do
    if pred.(i) = 0 then queue := Topo_queue.add i (topo_priority rank i) !queue
  done;
  let rec loop queue processed o =
    if processed = t.count then Some out
    else
      match Topo_queue.pop queue with
      | None -> None
      | Some ((e, _), queue) ->
          emitted.(e) <- true;
          let o =
            if alive t e then begin
              out.(o) <- e;
              o + 1
            end
            else o
          in
          let queue =
            List.fold_left
              (fun queue (j, _) ->
                if j < t.count && not emitted.(j) then begin
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
  | Some order -> order
  | None -> invalid_arg "Rule_graph.canonical_order: cyclic live graph"

(* The lineage's order, keeping [t]'s live nodes. *)
let canonical_order t : node_id array =
  let order = t.lineage.order in
  let out = ref [] in
  let rec loop = function
    | Option.None -> ()
    | Option.Some i ->
        if i < t.count && alive t i then out := i :: !out;
        loop (Order_maintenance.next order i)
  in
  loop (Order_maintenance.first order);
  Array.of_list (List.rev !out)

(* A graph and the graphs rewritten from it share their per-node arrays: an
   attempt writes its produced nodes into the slots past its parent's [count],
   which the parent never reads, and keeps the edges it points at existing nodes
   beside it until it is accepted. A rejected attempt therefore leaves nothing
   to undo, and neither outcome copies anything sized by the graph. *)
let rec first_duplicate (ids : node_id array) at =
  if at >= Array.length ids then Option.None
  else if Int.equal ids.(at - 1) ids.(at) then Option.Some ids.(at)
  else first_duplicate ids (at + 1)

let rec first_unusable t = function
  | [] -> Option.None
  | i :: rest ->
      if i < 0 || i >= t.count then Option.Some (Invalid_node i)
      else if not (alive t i) then Option.Some (Stale_node i)
      else first_unusable t rest

(* [consume] sorted, or why it cannot be consumed. *)
let validate_consume t consume =
  if t.generation <> t.lineage.head then
    invalid_arg "Rule_graph.rewrite: a later rewrite superseded this graph";
  match consume with
  | [] -> Error Empty_consume
  | _ -> (
      let consumed = Array.of_list consume in
      Array.sort Int.compare consumed;
      match first_duplicate consumed 1 with
      | Some i -> Error (Duplicate_node i)
      | None -> (
          match first_unusable t consume with
          | Some e -> Error e
          | None -> Ok consumed))

let rec mem_sorted (ids : node_id array) (x : node_id) lo hi =
  lo < hi
  &&
  let mid = (lo + hi) / 2 in
  let v = ids.(mid) in
  Int.equal v x
  || if v < x then mem_sorted ids x (mid + 1) hi else mem_sorted ids x lo mid

let is_consumed consumed k = mem_sorted consumed k 0 (Array.length consumed)

let produced_origin t consume =
  match consume with
  | [] -> 0
  | first :: rest ->
      List.fold_left
        (fun min_origin id -> min min_origin t.origin.(id))
        t.origin.(first) rest

(* A produced node takes the earliest origin of the consumed nodes it shares a
   branch with, or of all of them. *)
let produced_branch_origin t consume branches =
  List.fold_left
    (fun origin consumed ->
      if share_branch branches t.branches.(consumed) then
        min origin t.origin.(consumed)
      else origin)
    max_int consume
  |> fun origin ->
  if origin = max_int then produced_origin t consume else origin

(* Room in [t]'s node arrays for [added] nodes past its extent. The arrays are
   shared along the lineage, so [t] keeps what it grows even when the attempt
   that needed the room is rejected, and growing geometrically leaves room for
   the attempts after it. *)
let reserve t added =
  let needed = t.count + added in
  let grow : 'a. 'a array -> 'a array =
   fun arr ->
    let room = Array.length arr in
    if room >= needed then arr
    else begin
      let grown = Array.make (max needed (2 * room)) arr.(0) in
      Array.blit arr 0 grown 0 t.count;
      grown
    end
  in
  t.rules <- grow t.rules;
  t.sizes <- grow t.sizes;
  t.selector_sizes <- grow t.selector_sizes;
  t.origin <- grow t.origin;
  t.slot <- grow t.slot;
  t.death <- grow t.death;
  t.selectors <- grow t.selectors;
  t.selector_summaries <- grow t.selector_summaries;
  t.specificities <- grow t.specificities;
  t.branches <- grow t.branches;
  t.decl_overlaps <- grow t.decl_overlaps;
  t.overlap_keys <- grow t.overlap_keys;
  t.parts <- grow t.parts;
  t.succ <- grow t.succ

let place arr ~at items = Array.blit items 0 arr at (Array.length items)

(* The graph [produced] would make, its nodes holding their rules alone. What
   the order relation reads off a produced node is placed by {!place_produced}
   when the rewrite first asks about that node, and its size once the rewrite is
   accepted: most attempts are refused at the first produced node, and read
   nothing of the others. *)
let produced_graph t produced =
  let added = Array.length produced in
  let total = t.count in
  reserve t added;
  place t.rules ~at:total produced;
  Array.fill t.death total added max_int;
  Array.fill t.succ total added [];
  {
    t with
    generation = t.generation + 1;
    count = total + added;
    moved = [];
    (* the produced graph's edges are still being written; {!rewrite} hands it
       [t]'s answers once it is accepted *)
    reach = Unbuilt;
  }

let place_produced t graph ~consume p =
  let facts = rule_facts t.parent graph.rules.(p) in
  let selectors, summaries, specificities = facts.all in
  let branches = own_branch_keys facts in
  graph.origin.(p) <- produced_branch_origin t consume branches;
  graph.selectors.(p) <- selectors;
  graph.selector_summaries.(p) <- summaries;
  graph.specificities.(p) <- specificities;
  graph.branches.(p) <- branches;
  graph.decl_overlaps.(p) <- facts.decls;
  graph.overlap_keys.(p) <- facts.keys;
  graph.parts.(p) <- facts.rule_parts

let add_edge succ i j reason = succ.(i) <- (j, reason) :: succ.(i)

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
            Some { node; decl = Some index }
          else None))
    nodes

let fallback_source_refs nodes =
  List.map (fun node -> { node; decl = None }) nodes

let refs_cover_nodes nodes refs =
  List.for_all
    (fun node -> List.exists (fun ref_ -> ref_.node = node) refs)
    nodes

let produced_declaration_sources graph consume produced produced_decl =
  let nodes = produced_branch_sources graph consume produced in
  match
    declaration_source_refs graph nodes produced_decl
      ~matches:Declaration.same_minified
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

(* The live rules a path from [a] to [b] passes through on a graph built afresh
   from the live rules in the order they print, when there is one. Such a graph
   joins two rules by an edge exactly when they conflict, the first printed
   first, so a path runs through rules printed between [a] and [b], each
   conflicting with the one before. Every such path is one in [t] too, which
   orders every pair that conflicts; [t] holds more, through the rules its
   rewrites consumed. So the rules worth asking about are those [t] puts on a
   path from [a] to [b]. The path stands on later graphs while the rules on it
   stay live: two rules that conflict keep their order. *)
let fresh_path t a b =
  if a = b || not (path_exists t a b) then Option.None
  else if nodes_conflict t a b then Option.Some []
  else
    let reached = ref [ (a, []) ] and found = ref Option.None in
    ignore
      (exists_live_between t a b (fun x ->
           path_exists t a x && path_exists t x b
           &&
           match
             List.find_opt (fun (y, _) -> nodes_conflict t y x) !reached
           with
           | Option.None -> false
           | Option.Some (_, through) ->
               let through = x :: through in
               reached := (x, through) :: !reached;
               nodes_conflict t x b
               && begin
                 found := Option.Some through;
                 true
               end));
    !found

let source_ref_orientation ~path left right =
  if left.node = right.node then
    match (left.decl, right.decl) with
    | Some l, Some r when l < r -> Before
    | Some l, Some r when l > r -> After
    | _ -> New_conflict
  else
    match (path left.node right.node, path right.node left.node) with
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

let source_sets_orientation ~path left right =
  List.fold_left
    (fun orientation left ->
      List.fold_left
        (fun orientation right ->
          merge_orientation orientation
            (source_ref_orientation ~path left right))
        orientation right)
    New_conflict left

let produced_external_orientation ~path graph consume produced external_ =
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
        let external_source = { node = external_; decl = None } in
        merge_orientation orientation
          (source_sets_orientation ~path sources [ external_source ])
      else orientation)
    New_conflict
    graph.decl_overlaps.(produced)

let produced_pair_orientation ~path graph consume left right =
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
              (source_sets_orientation ~path left_sources right_sources)
          else orientation)
        orientation
        graph.decl_overlaps.(right))
    New_conflict graph.decl_overlaps.(left)

let inherited_orientation ~path consume k =
  match
    ( List.exists (fun c -> path c k) consume,
      List.exists (fun c -> path k c) consume )
  with
  | true, false -> Before
  | false, true -> After
  | true, true -> Ambiguous
  | false, false -> New_conflict

(* The edges a rewrite points from existing nodes at the nodes it produces, by
   produced index. They stay here until the rewrite is accepted, since the
   existing nodes' edge lists are shared with the parent. *)
let add_entering entering ~total k p reason =
  let at = p - total in
  entering.(at) <- (k, reason) :: entering.(at)

let add_inherited_edge ~path t graph consume entering p k reason =
  let orientation =
    match reason with
    | Cascade_conflict -> produced_external_orientation ~path graph consume p k
    | Shared_branch_pin -> inherited_orientation ~path consume k
  in
  match orientation with
  | Before ->
      add_edge graph.succ p k reason;
      Ok ()
  | After ->
      add_entering entering ~total:t.count k p reason;
      Ok ()
  | Ambiguous ->
      Error (Ambiguous_external_order { produced = p; external_ = k })
  | New_conflict ->
      Error (New_external_conflict { produced = p; external_ = k })

(* Existing nodes a produced node [p] could conflict with: those sharing an
   overlap key (or the broad key, which intersects everything) or a selector
   branch with it, read from the graph's index instead of scanning all of
   [t]'s nodes. A fresh stamp dedupes them. A produced node carrying the broad
   key conflicts with everything, so it falls back to a full scan (rare:
   [all]). *)
(* A merge moves its rules past everything that sits between them, so a node in
   that span is the one that usually refuses the move. The order the candidates
   are visited in does not change whether the rewrite is accepted, only how soon
   a refusal is found, so the span goes first. *)
let span_first ~lo ~hi candidates =
  let within, outside = List.partition (fun k -> k > lo && k < hi) candidates in
  List.rev_append (List.rev within) outside

(* Hand [push] the indexed nodes [p] could conflict with. *)
let indexed_candidates graph p push =
  let push_ids ids = List.iter (fun id -> push (Node_id.to_int id)) ids in
  (* Only a node with a branch of [p]'s specificity can tie with it. *)
  let specs = distinct_spec_keys [] graph.specificities.(p) in
  let at_specs key f =
    List.iter
      (fun spec ->
        match Slot_table.find_opt graph.key_index ({ key; spec } : slot) with
        | Some inner -> Decl_tbl.iter f inner
        | None -> ())
      specs
  in
  (* For each declaration [p] writes, collect, per overlap key it covers, the
     nodes whose declaration for that key differs - same-declaration nodes never
     conflict, so their (often large) bucket is skipped, not walked. *)
  List.iter
    (fun (ov : decl_overlap) ->
      List.iter
        (fun key ->
          at_specs key (fun decl' ids ->
              if not (Declaration.same_minified ov.decl decl') then push_ids ids))
        ov.footprint)
    graph.decl_overlaps.(p);
  (* The broad key ([all]) conflicts with everything, so collect every node
     under it regardless of declaration. *)
  at_specs Shorthand.broad_overlap_key (fun _ ids -> push_ids ids);
  List.iter
    (fun b ->
      match String_table.find_opt graph.branch_index b with
      | Some ids -> push_ids ids
      | None -> ())
    graph.branches.(p)

let external_candidates t graph ~consumed p =
  let stamp = fresh_stamp graph in
  let acc = ref [] in
  let push k =
    if alive t k && (not (is_consumed consumed k)) && first_visit graph stamp k
    then acc := k :: !acc
  in
  if Shorthand.overlap_keys_broad graph.overlap_keys.(p) then
    for k = 0 to t.count - 1 do
      push k
    done
  else indexed_candidates graph p push;
  (* ascending, so which orientation error comes first, which the scheduler
     reads, does not follow the index *)
  List.sort Int.compare !acc

let add_external_edges ~path t ~consume ~consumed graph entering =
  let total = t.count in
  let lo = consumed.(0) in
  let hi = consumed.(Array.length consumed - 1) in
  let rec loop_produced p =
    if p = graph.count then Ok ()
    else
      let () = place_produced t graph ~consume p in
      let rec loop_candidates = function
        | [] -> Ok ()
        | k :: rest -> (
            match nodes_conflict_reason graph p k with
            | None -> loop_candidates rest
            | Some reason -> (
                match
                  add_inherited_edge ~path t graph consume entering p k reason
                with
                | Ok () -> loop_candidates rest
                | Error _ as e -> e))
      in
      match
        loop_candidates
          (span_first ~lo ~hi (external_candidates t graph ~consumed p))
      with
      | Ok () -> loop_produced (p + 1)
      | Error _ as e -> e
  in
  loop_produced total

let add_produced_edge ~path graph consume left right reason =
  let orientation =
    match reason with
    | Cascade_conflict ->
        produced_pair_orientation ~path graph consume left right
    | Shared_branch_pin -> Before
  in
  match orientation with
  | Before ->
      add_edge graph.succ left right reason;
      Ok ()
  | After ->
      add_edge graph.succ right left reason;
      Ok ()
  | Ambiguous -> Error (Ambiguous_produced_order { left; right })
  | New_conflict -> Error (New_produced_conflict { left; right })

(* Prior produced nodes that share a declaration key or branch with [right] at a
   specificity that can tie, and so might conflict with it. A broad key forces a
   scan of every prior node at those specificities; otherwise the decl-key and
   branch buckets narrow the set further. *)
let collect_produced_candidates by_decl_key by_spec by_branch graph right keys
    specs =
  let stamp = fresh_stamp graph in
  let seen = graph.lineage.marks in
  let candidates =
    if Shorthand.overlap_keys_broad keys then
      collect_specs by_spec specs stamp seen []
    else
      collect_bucket by_decl_key Shorthand.broad_overlap_key specs stamp seen []
  in
  let candidates =
    List.fold_left
      (fun acc key -> collect_bucket by_decl_key key specs stamp seen acc)
      candidates keys
  in
  List.fold_left
    (fun acc branch -> collect_string_bucket by_branch branch stamp seen acc)
    candidates graph.branches.(right)

let add_produced_edges ~path t ~consume graph =
  let by_decl_key = Slot_table.create 16 in
  let by_branch = String_table.create 16 in
  let by_spec = Spec_table.create 16 in
  let rec loop right =
    if right = graph.count then Ok ()
    else
      let keys = graph.overlap_keys.(right) in
      let specs = distinct_spec_keys [] graph.specificities.(right) in
      let candidates =
        collect_produced_candidates by_decl_key by_spec by_branch graph right
          keys specs
      in
      let rec add_edges = function
        | [] ->
            List.iter (fun key -> add_bucket by_decl_key key specs right) keys;
            List.iter
              (fun branch -> String_table.push by_branch branch right)
              graph.branches.(right);
            add_spec_buckets by_spec right specs;
            loop (right + 1)
        | left :: rest -> (
            match nodes_conflict_reason graph left right with
            | None -> add_edges rest
            | Some reason -> (
                match
                  add_produced_edge ~path graph consume left right reason
                with
                | Ok () -> add_edges rest
                | Error _ as e -> e))
      in
      add_edges candidates
  in
  loop t.count

(* Visit marks for the cycle check below. *)
let unvisited = '\000'
let on_stack = '\001'
let acyclic = '\002'

(* [t] is acyclic and every edge a rewrite adds touches a produced node, so a
   new cycle passes through one. Between two produced nodes in turn it is either
   a direct edge or a path [p -> b ->* a -> q] whose middle runs over existing
   nodes, and so over [t]'s edges alone. Which produced node leads to which is
   therefore a question to [t]'s reachability, which the orientation has mostly
   settled already, and the check walks the produced nodes alone, however far
   their cone reaches.

   [reached exits] says which existing nodes the existing nodes [exits] reach.
   [None] is a cycle. Otherwise the produced nodes come back in the order the
   walk left them, so each one follows every produced node it reaches. *)
let produced_order ~reached t graph entering =
  let total = t.count in
  let added = graph.count - total in
  let leads = Array.make_matrix added added false in
  let entered =
    Array.exists (function [] -> false | _ :: _ -> true) entering
  in
  for from = 0 to added - 1 do
    let exits =
      List.fold_left
        (fun exits (j, _) ->
          if j >= total then begin
            leads.(from).(j - total) <- true;
            exits
          end
          else j :: exits)
        []
        graph.succ.(total + from)
    in
    match exits with
    | _ :: _ when entered ->
        let reached = reached exits in
        for at = 0 to added - 1 do
          if List.exists (fun (a, _) -> reached a) entering.(at) then
            leads.(from).(at) <- true
        done
    | _ -> ()
  done;
  let state = Bytes.make added unvisited in
  let finished = ref [] in
  let rec visit from =
    Bytes.set state from on_stack;
    let rec through at =
      at < added
      && (leads.(from).(at)
          && (Bytes.get state at = on_stack
             || (Bytes.get state at = unvisited && visit at))
         || through (at + 1))
    in
    let cyclic = through 0 in
    Bytes.set state from acyclic;
    finished := (total + from) :: !finished;
    cyclic
  in
  let rec roots at =
    at < added
    && ((Bytes.get state at = unvisited && visit at) || roots (at + 1))
  in
  if roots 0 then Option.None else Option.Some (List.rev !finished)

(* The nodes of [entering] a settled node can reach [p] through without passing
   another of them: one that reaches a second is found through the second. One
   with no settled set is reached by no settled node, as a settled node's whole
   cone is settled, so it is left for whoever closes it to read [p] from its
   edges. On a source-order chain this leaves a node or two of the dozens
   pointing at a merge. *)
let entering_frontier sets entering =
  let pointing = Reach.row sets in
  List.iter (fun (k, _) -> Reach.add pointing k) entering;
  List.filter_map
    (fun (k, _) ->
      if Reach.is_set sets.(k) && not (Reach.meets sets.(k) pointing) then
        Some k
      else None)
    entering

(* What a node reaching frontier node [k] gains: what [p] reaches beyond what
   [k] already did, which the node holds as well. A rewrite that replaces one
   rule leaves this empty, and a merge leaves what the other merged rules
   reached. *)
let rec gain set a = function
  | [] -> Option.None
  | (k, gained) :: rest ->
      if k = a || Reach.mem set k then Option.Some gained else gain set a rest

let add_gain set p reached gained =
  Reach.add set p;
  match gained with
  | Some nodes -> List.iter (Reach.add set) nodes
  | None -> Reach.union ~into:set reached

(* Settle produced node [p] and add it, with all it reaches, to every settled
   node that reaches one of [entering], the nodes pointing at it. A node that
   reaches no produced node keeps the set it had: its cone is untouched. *)
let settle_produced graph sets ~total entering p =
  close_cone graph sets p
  && begin
    (match entering_frontier sets entering with
    | [] -> ()
    | frontier ->
        let reached = sets.(p) in
        let frontier =
          List.map (fun k -> (k, Reach.missing reached sets.(k))) frontier
        in
        for a = 0 to total - 1 do
          let set = sets.(a) in
          if Reach.is_set set then
            match gain set a frontier with
            | None -> ()
            | Some gained ->
                add_gain set p reached gained;
                graph.expansions <- graph.expansions + 1
        done);
    true
  end

(* Hand the accepted [graph] the reachable sets its parent [t] settled. The
   rewrite added edges only at the produced nodes, so the sets that change are
   theirs and those of the nodes that reach them. Produced nodes are settled in
   [finished] order: by the time one is closed, every set in its cone already
   holds the produced nodes below it. The parent gives its sets up, and asks
   again from its own edges if it is ever questioned. *)
let carry_reach t graph entering finished =
  match t.reach with
  | Unbuilt | Walked -> ()
  | Held sets ->
      t.reach <- Unbuilt;
      if graph.count <= closure_node_limit then begin
        let sets = Reach.grow sets graph.count in
        if
          List.for_all
            (fun p ->
              settle_produced graph sets ~total:t.count entering.(p - t.count) p)
            finished
        then graph.reach <- Held sets
      end

(* Add produced node [p] to [graph]'s printed order. [placed q] says whether
   node [q] is in it: every existing node, and the produced nodes added before
   [p]. *)
let place_in_order graph ~placed ~moved entering p =
  let total = graph.count - Array.length entering in
  let preds =
    let rec siblings q acc =
      if q >= graph.count then acc
      else
        siblings (q + 1)
          (if
             q <> p && placed q
             && List.exists (fun (j, _) -> j = p) graph.succ.(q)
           then q :: acc
           else acc)
    in
    siblings total (List.map fst entering.(p - total))
  in
  Printed_order.add
    {
      order = graph.lineage.order;
      origin = graph.origin;
      slot = graph.slot;
      succ = graph.succ;
      death = graph.death;
      generation = graph.generation;
    }
    ~preds ~moved p

(* Make the accepted [graph] the lineage's newest: its consumed nodes die, the
   edges it pointed at its produced nodes join the existing nodes' lists, and
   its produced nodes enter the index so later rewrites find them as external
   candidates. *)
let commit t graph ~consumed entering finished =
  let total = t.count in
  for p = total to graph.count - 1 do
    let rule = graph.rules.(p) in
    let selector_size = Size.selector rule.selector in
    graph.selector_sizes.(p) <- selector_size;
    graph.sizes.(p) <- selector_size + Size.block graph.size_print rule
  done;
  Array.iter (fun c -> graph.death.(c) <- graph.generation) consumed;
  Array.iteri
    (fun at edges ->
      List.iter
        (fun (k, reason) -> add_edge graph.succ k (total + at) reason)
        edges)
    entering;
  for p = total to graph.count - 1 do
    index_node graph (Node_id.of_int_exn p)
  done;
  t.lineage.head <- graph.generation;
  let placed = Array.make (graph.count - total) false in
  let moved = Int_table.create 16 in
  for at = 0 to graph.count - total - 1 do
    place_in_order graph
      ~placed:(fun q -> placed.(q - total))
      ~moved:(fun k -> if k < total then Int_table.replace moved k ())
      entering (total + at);
    placed.(at) <- true
  done;
  graph.moved <-
    Int_table.fold (fun k () acc -> k :: acc) moved [] |> List.sort Int.compare;
  carry_reach t graph entering finished

let index_live t =
  if not t.lineage.indexed then begin
    t.lineage.indexed <- true;
    for i = 0 to t.count - 1 do
      if alive t i then index_node t (Node_id.of_int_exn i)
    done
  end

(* Two rules are ordered for a rewrite when they conflict, the first printed
   first, which is what a graph built afresh from the live rules in the order
   they print reads off them. The sources of a produced rule are live, and so is
   every rule it is ordered against, so a rule a rewrite consumed decides no
   order: what orders two rules only through it leaves them free. *)
let live_order t a b = compare_position t a b < 0 && nodes_conflict t a b

(* The graph [~consume ~produce] would make, with every edge at its produced
   nodes oriented, or why one cannot be. *)
let orient t ~consume ~produce =
  match validate_consume t consume with
  | Error _ as e -> e
  | Ok consumed -> (
      index_live t;
      match produce with
      | [] -> Error Empty_produce
      | _ -> (
          let graph = produced_graph t (Array.of_list produce) in
          let entering = Array.make (graph.count - t.count) [] in
          let path = live_order t in
          match
            add_external_edges ~path t ~consume ~consumed graph entering
          with
          | Error _ as e -> e
          | Ok () -> (
              match add_produced_edges ~path t ~consume graph with
              | Error _ as e -> e
              | Ok () -> Ok (consumed, graph, entering))))

(* What a graph built afresh from the live rules in the order they print reaches
   from [exits]: a rule printed after one of them through live rules each
   conflicting with the one before. *)
let live_reached t exits a =
  List.exists (fun e -> e = a || Option.is_some (fresh_path t e a)) exits

(* Whether the rewrite [orient] made closes a cycle, read first on [t]'s
   reachability, which runs through the rules earlier rewrites consumed and so
   holds every cycle the live rules close, and then, for a cycle found there, on
   the live rules alone. *)
type closing = Acyclic of node_id list | Live_cycle | Through_relays

let closing t graph entering =
  match produced_order ~reached:(reached_from t) t graph entering with
  | Some finished -> Acyclic finished
  | None -> (
      match produced_order ~reached:(live_reached t) t graph entering with
      | None -> Live_cycle
      | Some _ -> Through_relays)

let rewrite t ~consume ~produce : (t, rewrite_error) result =
  match orient t ~consume ~produce with
  | Error _ as e -> e
  | Ok (consumed, graph, entering) -> (
      match closing t graph entering with
      | Live_cycle -> Error Cycle
      | Through_relays -> Error Relay_cycle
      | Acyclic finished ->
          commit t graph ~consumed entering finished;
          Ok graph)

let rebuild t =
  let order = canonical_order t in
  let pick values = Array.map (fun i -> values.(i)) order in
  let exclusive =
    Option.map
      (fun exclusive i j ->
        let i = order.(i) and j = order.(j) in
        i < t.exclusive_count && j < t.exclusive_count && exclusive i j)
      t.exclusive
  in
  let g =
    ordered_nodes ~size_print:t.size_print
      ~selector_sizes:(pick t.selector_sizes) ~sizes:(pick t.sizes)
      ~index_capacity:(max 16 (Array.length order * 2))
      ?parent:t.parent ~closed_world:t.closed_world
      ~pin_shared_branches:t.pin_shared_branches ?exclusive (pick t.rules)
  in
  let generation = t.generation + 1 in
  g.lineage.head <- generation;
  { g with generation }

let try_rewrite t ~consume ~produce : t option =
  match rewrite t ~consume ~produce with
  | Ok graph -> Some graph
  | Error _ -> None
