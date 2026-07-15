(** Cascade-dependency graph over a run of flat style rules. *)

open Stylesheet
open Stdlib

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
  rules : rule array;  (** all nodes ever created, dead ones included *)
  summaries : Summary.t array;
      (** declaration-side facts and byte sizes, precomputed once per node *)
  origin : int array;
  live : bool array;
  selectors : Selector.t list array;
      (** effective selector branches for each node, after applying [parent] *)
  selector_summaries : Selector_summary.t list array;
  branches : string list array;
  decl_overlaps : decl_overlap list array;
  overlap_keys : Shorthand.overlap_key list array;
  succ : (node_id * edge_reason) list array;
      (** directed partial-order edges: [i] precedes every node in [succ.(i)].
          An edge exists exactly between two conflicting nodes, oriented by the
          order they were created in (source order for [of_rules], inherited
          order for rewrites). A valid linear extension is any topological order
          of the live sub-graph. *)
  key_index :
    ( Shorthand.overlap_key,
      (Declaration.declaration, node_id list) Hashtbl.t )
    Hashtbl.t;
      (** overlap key -> (declaration writing it -> node ids), so a rewrite
          finds an external node's potential conflicts without scanning every
          node, and {e without even iterating} the nodes that write the same
          declaration (which never conflict). Mutable and shared across a
          graph's rewrites (a graph is consumed before the next is produced);
          never pruned, dead nodes are skipped at query time. *)
  branch_index : (string, node_id list) Hashtbl.t;
      (** selector branch -> node ids that carry it; same role as [key_index]
          for the [share_branch] dimension. *)
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

let decls_order_conflict d1 d2 =
  List.exists (fun a -> List.exists (fun b -> declarations_conflict a b) d2) d1

let overlap_key_lists_intersect a b =
  let broad = Shorthand.broad_overlap_key in
  List.exists (Shorthand.overlap_key_equal broad) a
  || List.exists (Shorthand.overlap_key_equal broad) b
  || List.exists (fun key -> List.exists (Shorthand.overlap_key_equal key) b) a

let specificity_equal a b =
  let a = Selector.specificity a in
  let b = Selector.specificity b in
  Int.equal a.ids b.ids
  && Int.equal a.classes b.classes
  && Int.equal a.elements b.elements

let selectors_order_conflict ?(closed_world = false) selectors_a summaries_a
    selectors_b summaries_b =
  List.exists2
    (fun selector_a summary_a ->
      List.exists2
        (fun selector_b summary_b ->
          specificity_equal selector_a selector_b
          &&
          if closed_world then
            (* the caller asserts no element matches two distinct selectors, so
               only an identical selector still ties on the same element *)
            selector_a = selector_b
          else Selector_summary.may_overlap summary_a summary_b)
        selectors_b summaries_b)
    selectors_a summaries_a

(* Canonical, list-order-independent branch strings of a selector: canonicalize
   each comma branch, then sort. [.foo,.bar] and [.bar,.foo] yield the same
   list. *)
let selector_branch_keys selectors =
  selectors
  |> List.map (fun s ->
      Pp.to_string ~minify:true Selector.pp (Selector.canonicalize s))
  |> List.sort String.compare

(* Two rules share a selector branch when a factoring produced one as a residual
   of the other (e.g. [.a] and [.a,.b]) or they are the same selector. Such
   rules are pinned so a factored group stays contiguous and its declaration
   order is left to the declaration canonicalization. *)
let share_branch a b =
  List.exists (fun branch -> List.exists (String.equal branch) b) a

let declaration_size decl = Pp.size ~minify:true Declaration.pp_declaration decl

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
         t.selector_summaries.(i) t.selectors.(j) t.selector_summaries.(j)
  then Option.Some Cascade_conflict
  else Option.None

let nodes_conflict t i j = nodes_conflict_reason t i j <> Option.None

module Overlap_key_table = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

module String_table = Hashtbl.Make (String)

let add_unique key keys =
  if List.exists (Shorthand.overlap_key_equal key) keys then keys
  else key :: keys

let declaration_overlap decl =
  {
    decl;
    important = Declaration.is_important decl;
    footprint = Shorthand.declaration_overlap_keys decl;
  }

let rule_decl_overlaps (rule : rule) =
  List.map declaration_overlap rule.declarations

let rule_overlap_keys decls =
  List.fold_left
    (fun keys decl ->
      List.fold_left (fun keys key -> add_unique key keys) keys decl.footprint)
    [] decls

let add_bucket bucket key value =
  let prev =
    Overlap_key_table.find_opt bucket key |> Option.value ~default:[]
  in
  Overlap_key_table.replace bucket key (value :: prev)

let add_string_bucket bucket key value =
  let prev = String_table.find_opt bucket key |> Option.value ~default:[] in
  String_table.replace bucket key (value :: prev)

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

let collect_bucket bucket key stamp seen acc =
  Overlap_key_table.find_opt bucket key
  |> Option.value ~default:[]
  |> List.fold_left (add_candidate stamp seen) acc

let collect_string_bucket bucket key stamp seen acc =
  String_table.find_opt bucket key
  |> Option.value ~default:[]
  |> List.fold_left (add_candidate stamp seen) acc

let source_order_edges t =
  let n = Array.length t.rules in
  let succ = Array.make n [] in
  let by_decl_key = Overlap_key_table.create 256 in
  let by_branch = String_table.create 256 in
  let seen = Array.make (max n 1) (-1) in
  let prior_nodes = ref [] in
  for j = 0 to n - 1 do
    let keys = t.overlap_keys.(j) in
    let candidates =
      if
        List.exists
          (Shorthand.overlap_key_equal Shorthand.broad_overlap_key)
          keys
      then List.fold_left (add_candidate j seen) [] !prior_nodes
      else collect_bucket by_decl_key Shorthand.broad_overlap_key j seen []
    in
    let candidates =
      List.fold_left
        (fun acc key -> collect_bucket by_decl_key key j seen acc)
        candidates keys
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
    List.iter (fun key -> add_bucket by_decl_key key j) keys;
    List.iter
      (fun branch -> add_string_bucket by_branch branch j)
      t.branches.(j);
    prior_nodes := j :: !prior_nodes
  done;
  Array.map List.rev succ

let index_add tbl key node =
  Hashtbl.replace tbl key
    (node :: Option.value ~default:[] (Hashtbl.find_opt tbl key))

let key_inner t key =
  match Hashtbl.find_opt t.key_index key with
  | Some inner -> inner
  | None ->
      let inner = Hashtbl.create 4 in
      Hashtbl.replace t.key_index key inner;
      inner

(* Add [node]'s overlap keys (partitioned by the declaration that writes each)
   and its branches to the graph's external index. *)
let index_node t (node : node_id) =
  let i = Node_id.to_int node in
  List.iter
    (fun (ov : decl_overlap) ->
      List.iter
        (fun key -> index_add (key_inner t key) ov.decl node)
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
  let branches = Array.map selector_branch_keys selectors in
  let decl_overlaps = Array.map rule_decl_overlaps rules in
  let overlap_keys = Array.map rule_overlap_keys decl_overlaps in
  let live = Array.make n true in
  let t =
    {
      generation = 0;
      closed_world;
      parent;
      rules;
      summaries;
      origin = Array.init n Fun.id;
      live;
      selectors;
      selector_summaries;
      branches;
      decl_overlaps;
      overlap_keys;
      succ = [||];
      key_index = Hashtbl.create (max 16 (n * 2));
      branch_index = Hashtbl.create (max 16 (n * 2));
    }
  in
  for i = 0 to n - 1 do
    index_node t (Node_id.of_int_exn i)
  done;
  { t with succ = source_order_edges t }

let node_count t = Array.length t.rules
let node_rule t i = t.rules.(i)
let node_size t i = Summary.size t.summaries.(i)
let node_origin t i = t.origin.(i)
let is_live t i = t.live.(i)
let conflict t i j = nodes_conflict t i j
let order_constrained = conflict

let precedes t i j =
  t.live.(i) && t.live.(j) && List.exists (fun (k, _) -> k = j) t.succ.(i)

let generation t = t.generation

let live_nodes t =
  let acc = ref [] in
  for i = node_count t - 1 downto 0 do
    if t.live.(i) then acc := i :: !acc
  done;
  !acc

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

(* Kahn topological order over the live sub-graph: among the live nodes whose
   live predecessors are all emitted, always take the smallest [rank] key (ties
   broken by node id). [rank] resolves the choice among ready nodes - source
   position gives a minimal-disruption order, a content-derived rank gives a
   deterministic order two input orderings converge to. Returns [Option.None] if
   the live sub-graph has a cycle (no valid linear extension). *)
let topo_order_by t (rank : node_id -> int) : node_id array option =
  let live = live_nodes t in
  let n = List.length live in
  let pred = Hashtbl.create (2 * n) in
  List.iter (fun i -> Hashtbl.replace pred i 0) live;
  List.iter
    (fun i ->
      List.iter
        (fun (j, _) ->
          if t.live.(j) then Hashtbl.replace pred j (Hashtbl.find pred j + 1))
        t.succ.(i))
    live;
  let out = Array.make n 0 in
  let emitted = Hashtbl.create (2 * n) in
  let queue =
    List.fold_left
      (fun queue i ->
        if Hashtbl.find pred i = 0 then
          Topo_queue.add i (topo_priority rank i) queue
        else queue)
      Topo_queue.empty live
  in
  let rec loop queue o =
    if o = n then Option.Some out
    else
      match Topo_queue.pop queue with
      | Option.None -> Option.None
      | Option.Some ((e, _), queue) ->
          Hashtbl.replace emitted e ();
          out.(o) <- e;
          let queue =
            List.fold_left
              (fun queue (j, _) ->
                if t.live.(j) && not (Hashtbl.mem emitted j) then begin
                  let count = Hashtbl.find pred j - 1 in
                  Hashtbl.replace pred j count;
                  if count = 0 then
                    Topo_queue.add j (topo_priority rank j) queue
                  else queue
                end
                else queue)
              queue t.succ.(e)
          in
          loop queue (o + 1)
  in
  loop queue 0

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
  let branches = Array.map selector_branch_keys selectors in
  let decl_overlaps = Array.map rule_decl_overlaps produced in
  let overlap_keys = Array.map rule_overlap_keys decl_overlaps in
  ( summaries,
    selectors,
    selector_summaries,
    branches,
    decl_overlaps,
    overlap_keys )

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
        let ( p_summaries,
              p_selectors,
              p_selector_summaries,
              p_branches,
              p_decl_overlaps,
              p_overlap_keys ) =
          produced_metadata t produced
        in
        let graph =
          {
            generation = t.generation + 1;
            closed_world = t.closed_world;
            parent = t.parent;
            rules = Array.append t.rules produced;
            summaries = Array.append t.summaries p_summaries;
            origin =
              Array.append t.origin (produced_origins t consume p_branches);
            live = rewrite_live t ~total ~new_n ~consume;
            selectors = Array.append t.selectors p_selectors;
            selector_summaries =
              Array.append t.selector_summaries p_selector_summaries;
            branches = Array.append t.branches p_branches;
            decl_overlaps = Array.append t.decl_overlaps p_decl_overlaps;
            overlap_keys = Array.append t.overlap_keys p_overlap_keys;
            succ = Array.append t.succ (Array.make (Array.length produced) []);
            (* shared with [t]: holds the existing nodes; produced nodes are
               added by {!rewrite} only once the rewrite is accepted, so
               [add_external_edges] below sees the pre-rewrite index. *)
            key_index = t.key_index;
            branch_index = t.branch_index;
          }
        in
        Ok (total, consumed_set total consume, graph)

let add_edge succ i j reason = succ.(i) <- (j, reason) :: succ.(i)
let old_edge t a b = List.exists (fun (j, _) -> j = b) t.succ.(a)

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
  if List.exists (Shorthand.overlap_key_equal Shorthand.broad_overlap_key) keys
  then
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
            match Hashtbl.find_opt graph.key_index key with
            | Some inner ->
                Hashtbl.iter
                  (fun decl' ids ->
                    if not (Shorthand.same_minified_declaration ov.decl decl')
                    then push_ids ids)
                  inner
            | None -> ())
          ov.footprint)
      graph.decl_overlaps.(p);
    (* The broad key ([all]) conflicts with everything, so collect every node
       under it regardless of declaration. *)
    (match Hashtbl.find_opt graph.key_index Shorthand.broad_overlap_key with
    | Some inner -> Hashtbl.iter (fun _ ids -> push_ids ids) inner
    | None -> ());
    List.iter
      (fun b ->
        match Hashtbl.find_opt graph.branch_index b with
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

(* Prior produced nodes that share a declaration key or branch with [right], and
   so might conflict with it. A broad key forces a scan of every prior node;
   otherwise the decl-key and branch buckets narrow the set. *)
let collect_produced_candidates by_decl_key by_branch graph right seen keys
    prior_nodes =
  let candidates =
    if
      List.exists (Shorthand.overlap_key_equal Shorthand.broad_overlap_key) keys
    then List.fold_left (add_candidate right seen) [] prior_nodes
    else collect_bucket by_decl_key Shorthand.broad_overlap_key right seen []
  in
  let candidates =
    List.fold_left
      (fun acc key -> collect_bucket by_decl_key key right seen acc)
      candidates keys
  in
  List.fold_left
    (fun acc branch -> collect_string_bucket by_branch branch right seen acc)
    candidates graph.branches.(right)

let add_produced_edges t ~consume ~total ~produced_count graph succ =
  let by_decl_key = Overlap_key_table.create 256 in
  let by_branch = String_table.create 256 in
  let seen = Array.make (max (total + produced_count) 1) (-1) in
  let prior_nodes = ref [] in
  let rec loop pi =
    if pi = produced_count then Ok ()
    else
      let right = total + pi in
      let keys = graph.overlap_keys.(right) in
      let candidates =
        collect_produced_candidates by_decl_key by_branch graph right seen keys
          !prior_nodes
      in
      let rec add_edges = function
        | [] ->
            List.iter (fun key -> add_bucket by_decl_key key right) keys;
            List.iter
              (fun branch -> add_string_bucket by_branch branch right)
              graph.branches.(right);
            prior_nodes := right :: !prior_nodes;
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

(* The graph is acyclic before a rewrite and every new edge touches a produced
   node ([total..node_count)), so any new cycle passes through one. A forward
   DFS from the produced nodes detects it visiting only their reachable cone
   (O(reachable) per commit, not the O(n) [topo_order]); shared state across the
   produced roots walks each node/edge at most once. *)
let rewrite_introduces_cycle graph ~total =
  let n = node_count graph in
  let state = Hashtbl.create 64 in
  let rec dfs i =
    match Hashtbl.find_opt state i with
    | Some true -> true (* back edge to a node still on the DFS stack *)
    | Some false -> false (* already proven acyclic *)
    | None ->
        Hashtbl.replace state i true;
        let cyclic =
          List.exists
            (fun (j, _) ->
              let j = Node_id.to_int j in
              graph.live.(j) && dfs j)
            graph.succ.(i)
        in
        Hashtbl.replace state i false;
        cyclic
  in
  let rec from_root p =
    p < n && ((graph.live.(p) && dfs p) || from_root (p + 1))
  in
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
