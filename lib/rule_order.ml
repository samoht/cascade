(** Canonical cascade-safe rule ordering: project each run of reorderable
    statements through {!Rule_graph} to its canonical linear extension. *)

open Stylesheet

(* A run element is a plain style rule with no nested block, or a conditional
   at-rule block ([@media] / [@supports] / [@container]) whose transitive
   content is itself only such elements. The block moves atomically; its
   contained rules supply the conflict footprint. Custom-property declarations
   are reorderable too: the dependency graph keys each custom property by name,
   so two writers of the same property on overlapping selectors keep their
   order, while a [var()] reader - which writes its own property, not the one it
   reads - moves freely past the definition. A named [@layer] block or [@layer]
   declaration pins the layer order at its first occurrence, so it stays a
   barrier, as does everything else. *)
let rec element_rules (stmt : statement) : rule list option =
  match stmt with
  | Rule r -> if r.nested = [] then Some [ r ] else None
  | Media (_, b) | Supports (_, b) | Container (_, _, b) -> block_rules b
  | _ -> None

and block_rules (stmts : statement list) : rule list option =
  List.fold_left
    (fun acc stmt ->
      match (acc, element_rules stmt) with
      | Some acc, Some rules -> Some (List.rev_append rules acc)
      | _ -> None)
    (Some []) stmts

(* The graph node standing for one element. A block collapses to a single
   pseudo-rule holding every contained selector branch and declaration: its edge
   set is the cross product, a superset of the true per-rule edges, so conflict
   detection stays conservative while the block reorders as one unit. The
   applicability condition itself is irrelevant to safety: whether or not the
   condition holds, a swap with a non-conflicting neighbour cannot change any
   computed value. *)
let element_node (stmt : statement) (rules : rule list) : rule =
  match (stmt, rules) with
  | Rule r, _ -> r
  | _, [] ->
      {
        selector = Selector.universal;
        declarations = [];
        nested = [];
        merge_key = None;
      }
  | _, rules ->
      let branches =
        List.concat_map (fun (r : rule) -> Edge.selectors r.selector) rules
      in
      let selector =
        match branches with [ sel ] -> sel | sels -> Selector.List sels
      in
      {
        selector;
        declarations = List.concat_map (fun (r : rule) -> r.declarations) rules;
        nested = [];
        merge_key = None;
      }

let is_identity order =
  let id = ref true in
  Array.iteri
    (fun i v -> if i <> Rule_graph.Node_id.to_int v then id := false)
    order;
  !id

(* Source-order-preserving edge count: [indeg.(j)] is the number of
   source-earlier declarations that overlap [j], which must all be emitted
   before [j]. *)
let overlap_indegrees (arr : Declaration.declaration array) : int array =
  let n = Array.length arr in
  let indeg = Array.make n 0 in
  for j = 0 to n - 1 do
    for i = 0 to j - 1 do
      if Shorthand.declarations_overlap arr.(i) arr.(j) then
        indeg.(j) <- indeg.(j) + 1
    done
  done;
  indeg

(* Among not-yet-emitted declarations whose overlapping predecessors are all
   emitted ([indeg = 0]), the one with the smallest content key (then index). *)
let ready_min ~emitted ~indeg ~keys =
  let best = ref (-1) in
  for i = 0 to Array.length keys - 1 do
    if (not emitted.(i)) && indeg.(i) = 0 then
      if
        !best < 0
        ||
        let c = String.compare keys.(i) keys.(!best) in
        c < 0 || (c = 0 && i < !best)
      then best := i
  done;
  !best

(* Canonical order for a rule's declarations. Two declarations whose footprints
   overlap - the same property, or a shorthand and a longhand writing a common
   slot - keep their source order because it is cascade-significant; two with
   disjoint footprints can never change each other's computed value, so they
   sort into a deterministic content order. The declaration-level analogue of
   the rule-level reordering, so two rules holding the same declarations in a
   different commuting order project to one canonical form. Preserves physical
   identity when the source order is already canonical. *)
let canonical_declarations (decls : Declaration.declaration list) :
    Declaration.declaration list =
  match decls with
  | [] | [ _ ] -> decls
  | _ ->
      let arr = Array.of_list decls in
      let n = Array.length arr in
      let keys =
        Array.map
          (fun d -> Pp.to_string ~minify:true Declaration.pp_declaration d)
          arr
      in
      let indeg = overlap_indegrees arr in
      let emitted = Array.make n false in
      let order = Array.make n 0 in
      let changed = ref false in
      for pos = 0 to n - 1 do
        let b = ready_min ~emitted ~indeg ~keys in
        if b <> pos then changed := true;
        emitted.(b) <- true;
        order.(pos) <- b;
        for j = b + 1 to n - 1 do
          if (not emitted.(j)) && Shorthand.declarations_overlap arr.(b) arr.(j)
          then indeg.(j) <- indeg.(j) - 1
        done
      done;
      if not !changed then decls
      else Array.to_list (Array.map (fun i -> arr.(i)) order)

(* Reorder one maximal run of elements into a canonical linear extension of its
   dependency graph, breaking ties among independent elements by serialized
   statement content so two source orderings converge to one form, while
   preserving physical identity when that order already matches the input.
   [parent], when set, is the enclosing nesting context: the graph expands each
   element's relative selectors against it so overlap is computed on the
   effective selector. *)
let sort_run ?parent changed (run : (statement * rule list) list) :
    statement list =
  let arr = Array.of_list run in
  let n = Array.length arr in
  if n < 2 then List.map fst run
  else
    let g =
      Rule_graph.of_rules ?parent
        (List.map (fun (stmt, rules) -> element_node stmt rules) run)
    in
    let keys =
      Array.map
        (fun (stmt, _) -> Pp.to_string ~minify:true pp_stylesheet [ stmt ])
        arr
    in
    let ranked = Array.init n (fun i -> i) in
    Array.sort
      (fun a b ->
        match String.compare keys.(a) keys.(b) with
        | 0 -> Int.compare a b
        | c -> c)
      ranked;
    let rank = Array.make n 0 in
    Array.iteri (fun pos i -> rank.(i) <- pos) ranked;
    let order =
      Rule_graph.canonical_order_by g (fun node ->
          rank.(Rule_graph.Node_id.to_int node))
    in
    if is_identity order then List.map fst run
    else begin
      changed := true;
      Array.to_list
        (Array.map (fun i -> fst arr.(Rule_graph.Node_id.to_int i)) order)
    end

let branch_key sel = Pp.to_string ~minify:true Selector.pp sel

(* Selector branches that occur in more than one rule of a block: the only
   branches a later coalesce can fold. Expanding a list rule is only worthwhile
   when one of its branches is such a shared branch. *)
let shared_branches (stmts : statement list) : (string, unit) Hashtbl.t =
  let counts = Hashtbl.create 64 in
  List.iter
    (function
      | Rule r when r.nested = [] && r.merge_key = None ->
          List.iter
            (fun sel ->
              let k = branch_key sel in
              Hashtbl.replace counts k
                (1 + Option.value ~default:0 (Hashtbl.find_opt counts k)))
            (Edge.selectors r.selector)
      | _ -> ())
    stmts;
  let shared = Hashtbl.create 16 in
  Hashtbl.iter (fun k n -> if n > 1 then Hashtbl.replace shared k ()) counts;
  shared

(* A grouped rule is semantically the sequence of its per-branch rules, and a
   hoisted shared declaration is the same declaration written inline, so two
   sheets that factor the same content differently ([.absolute,.sr-only
   {position:absolute}] on one side, the declaration kept inline in [.sr-only]
   on the other) only project to one canonical form if grouping is undone first.
   Expand a selector-list rule into singleton-branch rules only when a branch is
   shared with another rule, so a coalesce can actually fold it; a list whose
   branches all occur once ([:host,:root], [*,::before,::after]) has nothing to
   coalesce with, and splitting it only bloats the projection and desynchronises
   the structural comparison of two otherwise-equivalent sheets. *)
let expand_lists shared (stmt : statement) : statement list =
  match stmt with
  | Rule r when r.nested = [] && r.merge_key = None -> (
      match Edge.selectors r.selector with
      | [] | [ _ ] -> [ stmt ]
      | branches
        when List.exists
               (fun sel -> Hashtbl.mem shared (branch_key sel))
               branches ->
          List.map (fun selector -> Rule { r with selector }) branches
      | _ -> [ stmt ])
  | _ -> [ stmt ]

(* Coalescing two occurrences of a selector concatenates their declarations, so
   an earlier write that a later one overrides (as when one occurrence carried a
   shared default the other specialises) becomes dead. Reduce with the same
   cascade dedup the optimizer uses - keep the last write of each property,
   preserving genuine fallback pairs - so a coalesced rule holds the same
   declaration set a sheet that never split it would. *)
let coalesced_declarations decls = Shorthand.deduplicate_declarations decls

(* Mutable state of one coalescing scan: the run's elements, the graph nodes
   accumulated into each surviving element, and which elements remain. *)
type scan = {
  graph : Rule_graph.t;
  arr : (statement * rule list) array;
  members : int list array;
  alive : bool array;
  mutable merged_any : bool;
}

(* Nothing strictly between [lo] and [hi] conflicts with any accumulated
   occurrence in [mems]: moving those writes across the interval is
   unobservable. *)
let interval_clear scan ~lo ~hi mems =
  let node i = Rule_graph.Node_id.of_int_exn i in
  let ok = ref true in
  for m = lo + 1 to hi - 1 do
    if
      List.exists
        (fun a -> Rule_graph.conflict scan.graph (node m) (node a))
        mems
    then ok := false
  done;
  !ok

let rec merge ?parent scan changed ~from ~into =
  (match (scan.arr.(from), scan.arr.(into)) with
  | (Rule rf, _), (Rule ri, _) ->
      let earlier, later = if from < into then (rf, ri) else (ri, rf) in
      let merged =
        {
          earlier with
          declarations =
            canonical_declarations
              (coalesced_declarations
                 (earlier.declarations @ later.declarations));
        }
      in
      scan.arr.(into) <- (Rule merged, [ merged ])
  | (be, _), (bl, _) ->
      (* Two conditional blocks sharing a condition: concatenate their bodies in
         source order and re-canonicalise the merged interior, so a sheet that
         emits one block and a sheet that splits it across several converge. *)
      let earlier, later = if from < into then (be, bl) else (bl, be) in
      let merged = merge_condition_blocks ?parent changed ~earlier ~later in
      scan.arr.(into) <-
        (merged, Option.value ~default:[] (element_rules merged)));
  scan.members.(into) <- scan.members.(from) @ scan.members.(into);
  scan.alive.(from) <- false;
  scan.merged_any <- true;
  changed := true

and merge_condition_blocks ?parent changed ~earlier ~later =
  let recanon body = canonicalize_block ~parent changed body in
  match (earlier, later) with
  | Media (m, b1), Media (_, b2) -> Media (m, recanon (b1 @ b2))
  | Supports (s, b1), Supports (_, b2) -> Supports (s, recanon (b1 @ b2))
  | Container (n, c, b1), Container (_, _, b2) ->
      Container (n, c, recanon (b1 @ b2))
  | _ -> assert false

(* Fold the earlier occurrence [i] down into [j] when nothing in between
   observes its writes moving; otherwise fold [j]'s writes up into [i] when
   nothing in between observes those. *)
and try_merge ?parent scan changed ~last ~key i j =
  if interval_clear scan ~lo:i ~hi:j scan.members.(i) then begin
    merge ?parent scan changed ~from:i ~into:j;
    Hashtbl.replace last key j
  end
  else if interval_clear scan ~lo:i ~hi:j scan.members.(j) then
    merge ?parent scan changed ~from:j ~into:i
  else Hashtbl.replace last key j

(* Coalesce same-selector singleton rules within one run. Folding an occurrence
   into another moves its declarations past every element in between, which is
   observable only if one of those elements conflicts with the moved rule; the
   conflict test is the graph's, against every original occurrence already
   accumulated into the surviving element. *)
and coalesce ?parent changed (run : (statement * rule list) list) :
    (statement * rule list) list =
  match run with
  | [] | [ _ ] -> run
  | _ ->
      let arr = Array.of_list run in
      let n = Array.length arr in
      let graph =
        Rule_graph.of_rules ?parent
          (List.map (fun (stmt, rules) -> element_node stmt rules) run)
      in
      let scan =
        {
          graph;
          arr;
          members = Array.init n (fun i -> [ i ]);
          alive = Array.make n true;
          merged_any = false;
        }
      in
      (* A style rule coalesces by selector; a conditional block coalesces by
         its condition (an empty-bodied render keys the header only), so two
         same-condition blocks fold together. The [@media]/[@supports]/
         [@container] prefix keeps a block key from colliding with a
         selector. *)
      let coalesce_key i =
        match arr.(i) with
        | Rule r, _ when r.merge_key = None ->
            Some (Pp.to_string ~minify:true Selector.pp r.selector)
        | Media (m, _), _ ->
            Some (Pp.to_string ~minify:true pp_stylesheet [ Media (m, []) ])
        | Supports (s, _), _ ->
            Some (Pp.to_string ~minify:true pp_stylesheet [ Supports (s, []) ])
        | Container (n, c, _), _ ->
            Some
              (Pp.to_string ~minify:true pp_stylesheet [ Container (n, c, []) ])
        | _ -> None
      in
      let last = Hashtbl.create 16 in
      for j = 0 to n - 1 do
        match coalesce_key j with
        | None -> ()
        | Some key -> (
            match Hashtbl.find_opt last key with
            | Some i when scan.alive.(i) ->
                try_merge ?parent scan changed ~last ~key i j
            | _ -> Hashtbl.replace last key j)
      done;
      if not scan.merged_any then run
      else Array.to_list arr |> List.filteri (fun i _ -> scan.alive.(i))

(* Sort and coalesce to a fixed point: sorting can empty the interval between
   two same-selector occurrences that a conflicting element previously kept
   apart, enabling a merge the source order hid, so equivalent sheets converge
   regardless of which arrangement they started from. Each merging round removes
   at least one element, so this terminates. *)
and settle ?parent changed (run : (statement * rule list) list) : statement list
    =
  let stmts = sort_run ?parent changed run in
  let sorted =
    List.filter_map
      (fun s ->
        match element_rules s with
        | Some rules -> Some (s, rules)
        | None -> None)
      stmts
  in
  let coalesced = coalesce ?parent changed sorted in
  if coalesced == sorted then stmts else settle ?parent changed coalesced

(* Canonicalise every cascade context. At-rule block bodies inherit the current
   nesting [parent]; a style rule's nested body is canonicalised under the
   rule's own (expanded) selector as the new parent, so nested relative
   selectors are compared on their effective form. *)
and recurse ~(parent : Selector.t option) changed (stmt : statement) : statement
    =
  let here b = canonicalize_block ~parent changed b in
  match stmt with
  | Rule r ->
      let child_parent =
        match parent with
        | None -> r.selector
        | Some p -> Nest.combine p r.selector
      in
      let nested =
        canonicalize_block ~parent:(Some child_parent) changed r.nested
      in
      let declarations = canonical_declarations r.declarations in
      if declarations != r.declarations then changed := true;
      if nested == r.nested && declarations == r.declarations then stmt
      else Rule { r with declarations; nested }
  | Layer (n, b) -> Layer (n, here b)
  | Media (m, b) -> Media (m, here b)
  | Container (n, c, b) -> Container (n, c, here b)
  | Supports (s, b) -> Supports (s, here b)
  | Moz_document (c, b) -> Moz_document (c, here b)
  | When (c, b) -> When (c, here b)
  | Else (c, b) -> Else (c, here b)
  | Starting_style b -> Starting_style (here b)
  | Origin (o, b) -> Origin (o, here b)
  | Scope (s, e, b) -> Scope (s, e, here b)
  | other -> other

and canonicalize_block ~parent changed (stmts : statement list) : statement list
    =
  (* Canonicalise interiors first so run elements are ranked on their canonical
     serialized form, then undo grouping so equivalent factorings converge. *)
  let stmts = List.map (recurse ~parent changed) stmts in
  let shared = shared_branches stmts in
  let expanded = List.concat_map (expand_lists shared) stmts in
  if List.compare_lengths expanded stmts <> 0 then changed := true;
  let rec go = function
    | [] -> []
    | stmt :: rest -> (
        match element_rules stmt with
        | Some rules ->
            let rec take acc = function
              | s :: r as l -> (
                  match element_rules s with
                  | Some rs -> take ((s, rs) :: acc) r
                  | None -> (List.rev acc, l))
              | [] -> (List.rev acc, [])
            in
            let run, rest = take [ (stmt, rules) ] rest in
            settle ?parent changed run @ go rest
        | None -> stmt :: go rest)
  in
  go expanded

let canonicalize (stmts : statement list) : statement list =
  let changed = ref false in
  let result =
    canonicalize_block ~parent:(None : Selector.t option) changed stmts
  in
  if !changed then result else stmts

(* Order-constraint predicate over a flat statement list. [order_constrained
   stmts i j] is true when reordering the statements at positions [i] and [j]
   could change a computed value: run elements (style rules and
   [@media]/[@supports]/[@container] blocks over plain rules) use the dependency
   graph's conflict test, and any other statement is a barrier, order-
   constrained with everything. The diff uses this to separate a cascade-neutral
   position drift (both endpoints unconstrained) from a real reorder. *)
let order_constrained (stmts : statement list) : int -> int -> bool =
  let arr = Array.of_list stmts in
  let n = Array.length arr in
  let is_element = Array.map (fun s -> Option.is_some (element_rules s)) arr in
  (* Compare per contained rule, not on a block's union footprint: an [@media]
     taken over the union of its rules' selectors and declarations conflicts
     with far more than any single rule does. Flatten every element to its rules
     and constrain two elements exactly when some rule of one conflicts with
     some rule of the other. *)
  let rules_of s = match element_rules s with Some rs -> rs | None -> [] in
  let ranges = Array.make n (0, 0) in
  let flat = ref [] in
  let acc = ref 0 in
  Array.iteri
    (fun i s ->
      let rs = rules_of s in
      ranges.(i) <- (!acc, List.length rs);
      List.iter
        (fun r ->
          flat := r :: !flat;
          incr acc)
        rs)
    arr;
  let g = Rule_graph.of_rules (List.rev !flat) in
  let node = Rule_graph.Node_id.of_int_exn in
  fun i j ->
    if i = j || i < 0 || j < 0 || i >= n || j >= n then false
    else if (not is_element.(i)) || not is_element.(j) then true
    else
      let si, ci = ranges.(i) and sj, cj = ranges.(j) in
      let found = ref false in
      for a = si to si + ci - 1 do
        for b = sj to sj + cj - 1 do
          if (not !found) && Rule_graph.conflict g (node a) (node b) then
            found := true
        done
      done;
      !found
