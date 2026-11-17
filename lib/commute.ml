(* Whether declaration blocks commute: replayed in either order, they leave
   every element computing the same thing. *)

open Stylesheet

(* How much of a declaration's overlap relation its precomputed footprint
   settles. [Slots] is decided by the keys alone; [Custom] by the property name,
   which is the whole of a custom property's footprint; [Broad] by neither,
   since [all] and an unknown name reach declarations no key of theirs names. *)
type shape = Slots | Custom of string | Broad

type fact = {
  decl : Declaration.declaration;
  important : bool;
  keys : Shorthand.overlap_key list;
  shape : shape;
}

let shape decl =
  if Shorthand.declaration_is_broad decl then Broad
  else
    match Shorthand.custom_property_name decl with
    | Option.Some name -> Custom name
    | Option.None -> Slots

let fact decl =
  {
    decl;
    important = Declaration.is_important decl;
    keys = Shorthand.declaration_overlap_keys decl;
    shape = shape decl;
  }

let facts decls = List.map fact decls

let conflict a b =
  Shorthand.declarations_conflict_with_keys a.decl a.keys b.decl b.keys

let decl fact = fact.decl
let same_decl = Declaration.same_minified

module Overlap_table = Common.Table.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

(* One side of a commute test, read once into the slots its declarations write,
   so the other side is walked once rather than once per declaration facing it.

   A slot holds the declarations that write one key at one weight, custom
   properties under their own name: an overlap key is a property-name hash, and
   only the name itself separates two custom properties, which write nothing but
   the slot they name. [conflict] clears a pair of declarations that minify the
   same, so a slot every declaration filed under it agrees on answers for all of
   them by comparing that one witness; a slot two of them disagree on conflicts
   with whatever reaches it, since at most one of the two can be the declaration
   asking. *)
type slot = { witness : fact; mutable uniform : bool }

(* [broad] holds the declarations no slot decides. What [all] and an unknown
   name overlap is read off the name rather than the footprint, so they keep the
   pairwise test; they are rare enough that keeping it costs nothing. [indexed]
   is the side itself, for a [Broad] declaration facing it. *)
type facts_index = {
  mutable indexed : fact list;
  written : slot list Overlap_table.t;
  mutable broad : fact list;
}

(* Whether two declarations share a slot: one weight, and, for a custom
   property, one name. *)
let same_commute_slot (a : fact) (b : fact) =
  Bool.equal a.important b.important
  &&
  match (a.shape, b.shape) with
  | Custom left, Custom right -> String.equal left right
  | Custom _, _ | _, Custom _ -> false
  | _ -> true

(* Spelled as explicit recursion throughout, and reading the table with
   [mem]/[find] rather than [find_opt]: a closure or an option allocated per
   declaration would put back the per-pair cost the index exists to remove. *)
let rec absorb (fact : fact) = function
  | [] -> false
  | slot :: rest ->
      if same_commute_slot slot.witness fact then begin
        if slot.uniform && not (same_decl slot.witness.decl fact.decl) then
          slot.uniform <- false;
        true
      end
      else absorb fact rest

let rec file_keys table fact = function
  | [] -> ()
  | key :: rest ->
      let slots =
        if Overlap_table.mem table key then Overlap_table.find table key else []
      in
      if not (absorb fact slots) then
        Overlap_table.replace table key
          ({ witness = fact; uniform = true } :: slots);
      file_keys table fact rest

let index_facts facts =
  let written = Overlap_table.create 16 in
  let broad =
    List.fold_left
      (fun broad (fact : fact) ->
        match fact.shape with
        | Broad -> fact :: broad
        | Slots | Custom _ ->
            file_keys written fact fact.keys;
            broad)
      [] facts
  in
  { indexed = facts; written; broad }

let empty_index () =
  { indexed = []; written = Overlap_table.create 16; broad = [] }

(* Filing further facts into a built index. A slot answers for the whole
   multiset filed into it - it conflicts with whatever reaches it once two of
   them disagree, and otherwise compares the one witness they all match - so an
   index over a union of footprints answers exactly what asking each of them
   separately would. *)
let rec add_facts index = function
  | [] -> ()
  | (fact : fact) :: rest ->
      index.indexed <- fact :: index.indexed;
      (match fact.shape with
      | Broad -> index.broad <- fact :: index.broad
      | Slots | Custom _ -> file_keys index.written fact fact.keys);
      add_facts index rest

let rec slots_conflict (fact : fact) = function
  | [] -> false
  | slot :: rest ->
      same_commute_slot slot.witness fact
      && ((not slot.uniform) || not (same_decl slot.witness.decl fact.decl))
      || slots_conflict fact rest

let rec any_conflicts (fact : fact) = function
  | [] -> false
  | other :: rest -> conflict fact other || any_conflicts fact rest

let rec keys_conflict written (fact : fact) = function
  | [] -> false
  | key :: rest ->
      Overlap_table.mem written key
      && slots_conflict fact (Overlap_table.find written key)
      || keys_conflict written fact rest

let fact_conflicts index (fact : fact) =
  match fact.shape with
  | Broad -> any_conflicts fact index.indexed
  | Slots | Custom _ ->
      any_conflicts fact index.broad
      || keys_conflict index.written fact fact.keys

let rec facts_conflict index = function
  | [] -> false
  | fact :: rest -> fact_conflicts index fact || facts_conflict index rest

(* Two footprints commute when no pair of them writes a common cascade slot: the
   order they are replayed in cannot then be observed. *)
let facts_commute_with index left = not (facts_conflict index left)

(* Merging a run of same-selector rules into the first one moves each later
   rule's declarations ahead of any nested block an earlier one carries. That
   only matters where the two write a common cascade slot, so a rule with nested
   children can still take part as long as those children and the declarations
   that would move past them commute.

   A slot, not a property name: a nested [margin] and a later [margin-top] name
   two properties and write one slot, so the nested children are carried as
   footprints and put through the overlap relation two flat declarations use.

   Read through the exhaustive statement walks: every nested statement sets
   properties, a nested declarations run as much as a nested style rule, and one
   this walk cannot see is one the merge would happily hoist past. *)
let rec nested_facts acc (stmts : statement list) =
  List.fold_left
    (fun acc stmt ->
      let acc =
        List.fold_left
          (fun acc d -> fact d :: acc)
          acc
          (statement_declarations stmt)
      in
      nested_facts acc (statement_children stmt))
    acc stmts

(* [rules] in source order: whether a declaration ends up ahead of a nested
   block it followed is a question about where the two were written, and the
   merge order below answers a different one.

   [conflict] is symmetric - every arm of the overlap relation under it is
   spelled both ways round - so either side of a commute test can be the indexed
   one. The nested bodies are the side worth indexing, and one index over all of
   them read so far is what a block moving past them faces: "every nested body
   before it commutes with this block" is one question about their union, not
   one per body. So the run is walked once, each block probed once and each
   nested body read once, rather than every (body, later block) pair being put
   to the index separately.

   The scan first is what keeps a run carrying no nested block, the common one,
   from reading a body at all, and the empty index short-circuits the blocks
   ahead of the first nested body. *)
let nested_merge_is_safe ~stats (rules : rule list) =
  let rec nested_ahead = function
    | [] | [ _ ] -> false
    | (r : rule) :: rest -> r.nested <> [] || nested_ahead rest
  in
  let rec go ahead = function
    | [] -> true
    | (r : rule) :: rest -> (
        let block_commutes =
          match ahead.indexed with
          | [] -> true
          | _ ->
              Stats.probe_nested_merge stats;
              facts_commute_with ahead (facts r.declarations)
        in
        block_commutes
        &&
        match rest with
        | [] -> true
        | _ ->
            add_facts ahead (nested_facts [] r.nested);
            go ahead rest)
  in
  (not (nested_ahead rules)) || go (empty_index ()) rules

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

(* Read off a rule once. The insertion sort below asks for the same order of the
   same rule against every rule already queued, and rendering a body into keys
   per comparison is what made that sort cost a body read per pair. *)
let rule_order_key (id, (r : rule)) =
  (List.map declaration_order_key r.declarations, Rule_graph.Node_id.to_int id)

let compare_rule_order_key (left_keys, left_id) (right_keys, right_id) =
  match compare_declaration_order_keys left_keys right_keys with
  | 0 -> Int.compare left_id right_id
  | order -> order

(* The precedence DAG holds every pair the merged rule replays in emission
   order: its declarations, and its nested blocks, which follow all of them. A
   pair that competes keeps the order it was written in; the rest is free for
   [compare_rule_order_key] to canonicalise. *)
let merge_order (rules_with_ids : (Rule_graph.node_id * rule) list) =
  let rows = Array.of_list rules_with_ids in
  let n = Array.length rows in
  let facts = Array.map (fun (_, (r : rule)) -> facts r.declarations) rows in
  let nested_facts =
    Array.map (fun (_, (r : rule)) -> nested_facts [] r.nested) rows
  in
  (* Indexed once per rule rather than once per pair: every rule below faces
     every later one, and reading a body is what the pair test costs. *)
  let body_index = Array.map index_facts facts in
  let nested_body_index = Array.map index_facts nested_facts in
  let order_key = Array.map rule_order_key rows in
  let succ = Array.make n [] in
  let pred = Array.make n 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if
        (not (facts_commute_with body_index.(j) facts.(i)))
        || not (facts_commute_with nested_body_index.(j) nested_facts.(i))
      then begin
        succ.(i) <- j :: succ.(i);
        pred.(j) <- pred.(j) + 1
      end
    done
  done;
  let rec insert index = function
    | [] -> [ index ]
    | head :: rest as queue ->
        if compare_rule_order_key order_key.(index) order_key.(head) < 0 then
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
