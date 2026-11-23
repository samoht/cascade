(** The index that finds the rules that may conflict with one another. *)

open Stdlib

(* The declaration-keyed bucket index is probed once per overlap key per node
   while a graph is built, and a generic [Hashtbl] compares whole declaration
   subtrees on every probe. The cached [Declaration.hash] buckets in constant
   time and the structural check runs only when two hashes collide. *)
module Decl_tbl = Hashtbl.Make (struct
  type t = Declaration.declaration

  let equal = Declaration.same_minified
  let hash = Declaration.hash
end)

module String_table = Common.Table.Make (String)

type decl_overlap = {
  decl : Declaration.declaration;
  footprint : Shorthand.overlap_key list;
}

(* Packed specificities, so the bucket index is keyed by an immediate rather
   than by a record a generic table would hash and compare field by field. *)
module Spec_table = Common.Table.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Int.hash
end)

(* An overlap key at a packed specificity: what two rules have to share for a
   bucket to hold both. One table keyed by the pair serves every bucket, where a
   table per key would allocate one more small table for each key met. *)
type slot = { key : Shorthand.overlap_key; spec : int }

module Slot_table = Common.Table.Make (struct
  type t = slot

  let equal a b =
    Shorthand.overlap_key_equal a.key b.key && Int.equal a.spec b.spec

  let hash { key; spec } =
    Common.mix_int (Shorthand.overlap_key_hash key) spec land max_int
end)

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

let rec add_spec_buckets bucket value = function
  | [] -> ()
  | spec :: rest ->
      Spec_table.push bucket spec value;
      add_spec_buckets bucket value rest

let rec add_bucket bucket key specs value =
  match specs with
  | [] -> ()
  | spec :: rest ->
      Slot_table.push bucket { key; spec } value;
      add_bucket bucket key rest value

(* Values, each at a position, bucketed by what two rules need to share to
   conflict: a branch specificity, and an overlap key or a broad footprint on
   either side. A query reads only the buckets the asking rule names, so it
   tests the values that could conflict with it rather than the whole run.
   [tested] holds the stamp of the last query that tested a value, since one
   value sits in a bucket per key and per specificity. *)
type 'a entry = { at : int; value : 'a; mutable tested : int }

type 'a positioned = {
  by_slot : 'a entry list Slot_table.t;
  by_spec : 'a entry list Spec_table.t;
  mutable stamp : int;
}

let positioned () =
  { by_slot = Slot_table.create 64; by_spec = Spec_table.create 16; stamp = 0 }

let add_at index ~at ~specs ~keys value =
  let entry = { at; value; tested = 0 } in
  add_spec_buckets index.by_spec entry specs;
  List.iter (fun key -> add_bucket index.by_slot key specs entry) keys

(* A bucket lists its values newest first, so the walk stops at the first value
   at or before [after]. *)
let rec found_in ~after test stamp = function
  | e :: rest when e.at > after ->
      (not (Int.equal e.tested stamp))
      &&
      (e.tested <- stamp;
       test e.value)
      || found_in ~after test stamp rest
  | _ -> false

let found_in_specs ~after test stamp table specs =
  List.exists
    (fun spec ->
      match Spec_table.find_opt table spec with
      | Option.None -> false
      | Option.Some entries -> found_in ~after test stamp entries)
    specs

let found_in_slots ~after test stamp table key specs =
  List.exists
    (fun spec ->
      match Slot_table.find_opt table { key; spec } with
      | Option.None -> false
      | Option.Some entries -> found_in ~after test stamp entries)
    specs

(* A rule whose footprint is broad overlaps every key, so it reads every value
   of a shared specificity; any other reads its own keys and the broad one. *)
let exists_after index ~after ~specs ~keys test =
  index.stamp <- index.stamp + 1;
  let stamp = index.stamp in
  if Shorthand.overlap_keys_broad keys then
    found_in_specs ~after test stamp index.by_spec specs
  else
    List.exists
      (fun key -> found_in_slots ~after test stamp index.by_slot key specs)
      (Shorthand.broad_overlap_key :: keys)

(* [seen] is a per-target stamp array indexed by node id: [seen.(i) = stamp]
   means node [i] is already a candidate for the current target. The target's
   own index is a fresh monotonically increasing stamp, so no clearing is
   needed, and dedup is O(1) with no hashing or per-target allocation. *)
let add_candidate (stamp : int) (seen : int array) acc i =
  if Int.equal seen.(i) stamp then acc
  else begin
    seen.(i) <- stamp;
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

let rec collect_bucket bucket key specs stamp seen acc =
  match specs with
  | [] -> acc
  | spec :: rest ->
      let acc =
        match Slot_table.find_opt bucket { key; spec } with
        | Option.None -> acc
        | Option.Some ids -> add_candidates stamp seen acc ids
      in
      collect_bucket bucket key rest stamp seen acc

(* The declaration that writes a key sits below the specificity and the
   selector's exclusive subject facts. Two rules writing the same declaration
   never constrain each other's order there, while two branches with distinct
   mandatory IDs or element names cannot target the same element. Keeping both
   dimensions in the index skips either whole group without enumerating its
   nodes.

   Simple one-declaration branches in one group conflict exactly when their
   declarations differ, so in source order they form runs of writers of one
   declaration, each run ordered after the whole run before it. A new node links
   to the newest run, or to the run before it when the newest writes its own
   declaration; every older node reaches both through the runs in between. So
   the group keeps only those two runs. Keeping only the newest would lose a
   node two runs back that writes neither the newest declaration nor the new
   node's. [all_nodes] and [opaque] preserve full history for shapes where that
   compaction invariant does not apply. *)
type writers = { decl : Declaration.declaration; mutable members : int list }

(* The tables below a group are made when the first node needs one: most groups
   hold only compact runs, under no element and no ID. *)
type decl_groups = {
  mutable opaque : int list Decl_tbl.t option;
  mutable newest : writers option;
      (** the newest run of simple branches writing one declaration *)
  mutable previous : writers option;  (** the run before [newest] *)
  mutable all_nodes : int list;
      (** every node of the group, newest first, for the walk a query takes over
          the whole group *)
  mutable all_scanned : int;
      (** the query stamp that last walked [all_nodes]: a block node asks about
          one group once per branch and declaration that lands in it, and the
          walk is the same every time *)
}

type importance_decl_groups = { normal : decl_groups; important : decl_groups }

type id_decl_groups = {
  no_id : importance_decl_groups;
  mutable by_id : importance_decl_groups String_table.t option;
}

type selector_decl_groups = {
  no_element : id_decl_groups;
  mutable by_element : id_decl_groups String_table.t option;
}

let empty_decl_groups () =
  {
    opaque = Option.None;
    newest = Option.None;
    previous = Option.None;
    all_nodes = [];
    all_scanned = -1;
  }

let empty_importance_decl_groups () =
  { normal = empty_decl_groups (); important = empty_decl_groups () }

let empty_id_decl_groups () =
  { no_id = empty_importance_decl_groups (); by_id = Option.None }

let empty_selector_decl_groups () =
  { no_element = empty_id_decl_groups (); by_element = Option.None }

let slot_decl_groups bucket key spec : selector_decl_groups =
  let slot = { key; spec } in
  match Slot_table.find_opt bucket slot with
  | Option.Some index -> index
  | Option.None ->
      let index = empty_selector_decl_groups () in
      Slot_table.replace bucket slot index;
      index

(* The entry of [name] in a lazily made table of [table], made with [empty]. *)
let named_entry table set empty name =
  let table =
    match table with
    | Option.Some table -> table
    | Option.None ->
        let table = String_table.create 4 in
        set table;
        table
  in
  match String_table.find_opt table name with
  | Option.Some entry -> entry
  | Option.None ->
      let entry = empty () in
      String_table.replace table name entry;
      entry

let element_decl_groups index = function
  | Option.None -> index.no_element
  | Option.Some element ->
      named_entry index.by_element
        (fun table -> index.by_element <- Option.Some table)
        empty_id_decl_groups element

let named_decl_groups groups name =
  named_entry groups.by_id
    (fun table -> groups.by_id <- Option.Some table)
    empty_importance_decl_groups name

let importance_decl_groups groups decl =
  if Declaration.is_important decl then groups.important else groups.normal

let push_decl groups decl value =
  let table =
    match groups.opaque with
    | Option.Some table -> table
    | Option.None ->
        let table = Decl_tbl.create 4 in
        groups.opaque <- Option.Some table;
        table
  in
  Decl_tbl.replace table decl
    (value :: Option.value ~default:[] (Decl_tbl.find_opt table decl))

let add_decl ~compact groups decl value =
  let groups = importance_decl_groups groups decl in
  groups.all_nodes <- value :: groups.all_nodes;
  if compact then (
    match groups.newest with
    | Option.Some run when Declaration.same_minified run.decl decl ->
        run.members <- value :: run.members
    | newest ->
        groups.previous <- newest;
        groups.newest <- Option.Some { decl; members = [ value ] })
  else push_decl groups decl value

(* What a branch requires of its subject, read once per node: the element name
   and the IDs. *)
type subject = { element : string option; ids : string list }

let add_selector_decl ~compact index subject decl value =
  let groups = element_decl_groups index subject.element in
  match subject.ids with
  | [] -> add_decl ~compact groups.no_id decl value
  | ids ->
      List.iter
        (fun id -> add_decl ~compact (named_decl_groups groups id) decl value)
        ids

let add_decl_bucket ~compact bucket key spec subject decl value =
  add_selector_decl ~compact
    (slot_decl_groups bucket key spec)
    subject decl value

let collect_decl_table decl groups stamp seen acc =
  match groups.opaque with
  | Option.None -> acc
  | Option.Some table ->
      Decl_tbl.fold
        (fun decl' ids acc ->
          if Declaration.same_minified decl decl' then acc
          else add_candidates stamp seen acc ids)
        table acc

let collect_writers decl run stamp seen acc =
  match run with
  | Option.Some run when not (Declaration.same_minified decl run.decl) ->
      add_candidates stamp seen acc run.members
  | Option.Some _ | Option.None -> acc

(* The whole group as candidates, once per query. A node writing the same
   declaration is among them: the conflict test refuses it, and a candidate the
   test refuses adds no edge, so the answer is the one a walk per distinct
   declaration gives, without walking the group once per declaration of the
   querying node. *)
let collect_group_nodes groups stamp seen acc =
  if groups.all_scanned = stamp then acc
  else begin
    groups.all_scanned <- stamp;
    add_candidates stamp seen acc groups.all_nodes
  end

let collect_distinct_decls ~compact decl groups stamp seen acc =
  let groups = importance_decl_groups groups decl in
  if compact then
    collect_writers decl groups.newest stamp seen acc |> fun acc ->
    collect_writers decl groups.previous stamp seen acc |> fun acc ->
    collect_decl_table decl groups stamp seen acc
  else collect_group_nodes groups stamp seen acc

let collect_all_decls groups stamp seen acc =
  collect_group_nodes groups.normal stamp seen acc |> fun acc ->
  collect_group_nodes groups.important stamp seen acc

let fold_named table f acc =
  match table with
  | Option.None -> acc
  | Option.Some table ->
      String_table.fold (fun _ entry acc -> f entry acc) table acc

let named table name =
  match table with
  | Option.None -> Option.None
  | Option.Some table -> String_table.find_opt table name

let collect_id_groups collect groups ids stamp seen acc =
  let acc = collect groups.no_id stamp seen acc in
  match ids with
  | [] ->
      fold_named groups.by_id
        (fun decls acc -> collect decls stamp seen acc)
        acc
  | ids ->
      List.fold_left
        (fun acc id ->
          match named groups.by_id id with
          | Option.None -> acc
          | Option.Some decls -> collect decls stamp seen acc)
        acc ids

let collect_selector_groups collect index subject stamp seen acc =
  let ids = subject.ids in
  let acc = collect_id_groups collect index.no_element ids stamp seen acc in
  match subject.element with
  | Option.None ->
      fold_named index.by_element
        (fun groups acc -> collect_id_groups collect groups ids stamp seen acc)
        acc
  | Option.Some element -> (
      match named index.by_element element with
      | Option.None -> acc
      | Option.Some groups ->
          collect_id_groups collect groups ids stamp seen acc)

let collect_decl_bucket ~compact bucket key spec subject decl stamp seen acc =
  match Slot_table.find_opt bucket { key; spec } with
  | Option.None -> acc
  | Option.Some index ->
      collect_selector_groups
        (collect_distinct_decls ~compact decl)
        index subject stamp seen acc

(* A prior node whose own declaration is broad meets every key, so no single
   declaration of the querying node can rule its group out. *)
let collect_broad_bucket bucket spec subject stamp seen acc =
  match
    Slot_table.find_opt bucket { key = Shorthand.broad_overlap_key; spec }
  with
  | Option.None -> acc
  | Option.Some index ->
      collect_selector_groups collect_all_decls index subject stamp seen acc

let rec collect_decl_keys ~compact bucket spec subject decl stamp seen acc =
  function
  | [] -> acc
  | key :: rest ->
      collect_decl_keys ~compact bucket spec subject decl stamp seen
        (collect_decl_bucket ~compact bucket key spec subject decl stamp seen
           acc)
        rest

let rec collect_decl_overlaps ~compact bucket spec subject stamp seen acc =
  function
  | [] -> acc
  | (ov : decl_overlap) :: rest ->
      collect_decl_overlaps ~compact bucket spec subject stamp seen
        (collect_decl_keys ~compact bucket spec subject ov.decl stamp seen acc
           ov.footprint)
        rest

let rec simple_subject_selector (selector : Selector.t) =
  match selector with
  | Class _ | Id _ | Element _ | Universal _ -> true
  | Compound selectors -> List.for_all simple_subject_selector selectors
  | _ -> false

let compact_rule = function [ { footprint = [ _ ]; _ } ] -> true | _ -> false

(* The groups a node's branches reach. A branch reaches the groups of its
   specificity and of the element and IDs its subject requires, so two branches
   that agree on those, and on whether the node sits in the compact runs through
   them, reach the same groups: a block node holding dozens of class branches
   reaches them once. *)
type reach = { spec : int; subject : subject; compact : bool }

let same_reach a b =
  Int.equal a.spec b.spec
  && Bool.equal a.compact b.compact
  && Option.equal String.equal a.subject.element b.subject.element
  && List.equal String.equal a.subject.ids b.subject.ids

let rec reaches ~relay decls acc selectors summaries specificities =
  match (selectors, summaries, specificities) with
  | [], [], [] -> List.rev acc
  | selector :: selectors, summary :: summaries, specificity :: specificities ->
      let reach =
        {
          spec = spec_key specificity;
          subject =
            {
              element = Selector_summary.element summary;
              ids = Selector_summary.ids summary;
            };
          compact =
            relay && compact_rule decls && simple_subject_selector selector;
        }
      in
      let acc =
        if List.exists (same_reach reach) acc then acc else reach :: acc
      in
      reaches ~relay decls acc selectors summaries specificities
  | _ -> invalid_arg "Conflict_index.reaches"

let collect_rule_overlaps ~relay bucket decls selectors summaries specificities
    stamp seen acc =
  List.fold_left
    (fun acc { spec; subject; compact } ->
      collect_broad_bucket bucket spec subject stamp seen acc |> fun acc ->
      collect_decl_overlaps ~compact bucket spec subject stamp seen acc decls)
    acc
    (reaches ~relay decls [] selectors summaries specificities)

let rec add_decl_keys ~compact bucket spec subject decl value = function
  | [] -> ()
  | key :: rest ->
      add_decl_bucket ~compact bucket key spec subject decl value;
      add_decl_keys ~compact bucket spec subject decl value rest

let rec add_decl_overlaps ~compact bucket spec subject value = function
  | [] -> ()
  | (ov : decl_overlap) :: rest ->
      add_decl_keys ~compact bucket spec subject ov.decl value ov.footprint;
      add_decl_overlaps ~compact bucket spec subject value rest

let add_rule_overlaps ~relay bucket decls selectors summaries specificities
    value =
  List.iter
    (fun { spec; subject; compact } ->
      add_decl_overlaps ~compact bucket spec subject value decls)
    (reaches ~relay decls [] selectors summaries specificities)

(* Every node indexed so far: each bucketed by the keys its declarations write,
   then by specificity and subject, and by specificity alone, which is what a
   node carrying the broad key is checked against, since the broad key
   intersects every other. [seen] dedups the candidates of one query, stamped
   with the node asking. *)
type t = {
  by_decl_key : selector_decl_groups Slot_table.t;
  nodes_by_spec : int list Spec_table.t;
  mutable seen : int array;
}

let v n =
  {
    by_decl_key = Slot_table.create 256;
    nodes_by_spec = Spec_table.create 64;
    seen = Array.make (max n 1) (-1);
  }

let seen t = t.seen

let grow t n =
  if Array.length t.seen < n then begin
    let seen = Array.make (max n (2 * Array.length t.seen)) (-1) in
    Array.blit t.seen 0 seen 0 (Array.length t.seen);
    t.seen <- seen
  end

let candidates t ~relay ~keys decls selectors summaries specificities node =
  if Shorthand.overlap_keys_broad keys then
    (* its own keys index nothing: only specificity narrows it *)
    collect_specs t.nodes_by_spec
      (distinct_spec_keys [] specificities)
      node t.seen []
  else
    (* probed per declaration rather than per distinct key: the key alone does
       not say which declaration of [node] faces the bucket, and that is what
       decides which group can never conflict *)
    collect_rule_overlaps ~relay t.by_decl_key decls selectors summaries
      specificities node t.seen []

let insert t ~relay decls selectors summaries specificities node =
  add_rule_overlaps ~relay t.by_decl_key decls selectors summaries specificities
    node;
  add_spec_buckets t.nodes_by_spec node (distinct_spec_keys [] specificities)
