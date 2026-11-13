(* Rule alignment: pairing the rules of two statement lists into added, removed,
   changed, moved and regrouped entries. *)

open Cascade
open Tree_diff_intf
open Tree_diff_text

(* A declaration reorder changes the cascade only when two overlapping
   declarations swap relative order; disjoint declarations commute, so their
   reorder is no difference (README contract). Duplicated property names are a
   same-property override, reported conservatively. *)
let reorder_is_significant decls1 decls2 =
  let name = Css.declaration_name in
  let names1 = List.map name decls1 in
  let has_dup =
    let s = List.sort String.compare names1 in
    let rec go = function a :: (b :: _ as t) -> a = b || go t | _ -> false in
    go s
  in
  has_dup
  ||
  let pos2 = Hashtbl.create 16 in
  List.iteri (fun i d -> Hashtbl.replace pos2 (name d) i) decls2;
  let arr = Array.of_list decls1 in
  let n = Array.length arr in
  (* Both hoisted out of the pair loop: naming a declaration and reading its
     overlap keys each serialize the property through a fresh buffer. *)
  let pos =
    Array.of_list
      (List.map
         (fun prop -> Option.value ~default:(-1) (Hashtbl.find_opt pos2 prop))
         names1)
  in
  let footprints = Array.map Shorthand.declaration_overlap_keys arr in
  let flipped = ref false in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if
        Shorthand.declarations_overlap_with_keys arr.(i) footprints.(i) arr.(j)
          footprints.(j)
        && pos.(i) >= pos.(j)
      then flipped := true
    done
  done;
  !flipped

(* The text a statement prints to, cut at its block, for the statements no
   selector names: [@charset "UTF-8";], [@layer a, b;], [@namespace ...]. An
   entry with no name cannot be classified, so it corrupts every count the
   summary derives from the same list, and it renders as a bare tree
   connector. *)
let statement_head stmt =
  let text =
    Css.Stylesheet.to_string ~minify:true (Css.v [ stmt ]) |> String.trim
  in
  let head =
    match String.index_opt text '{' with
    | Some i -> String.trim (String.sub text 0 i)
    | None -> text
  in
  if head = "" then
    Option.value ~default:"(other statement)" (describe_statement stmt)
  else head

(* Helper to extract rule information from statements *)
let strings_of_rule (stmt : Css.statement) =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  (* CSS Nesting 1 sec. 3.4: a run written after a nested statement is a nested
     declarations rule, which acts as [&]. It has no head to cut at, so the text
     up to the first brace is its first declaration, which reads in the selector
     column as a selector it is not. *)
  | None -> (
      match stmt with
      | Declarations decls -> ("&", decls)
      | _ -> (statement_head stmt, Css.Stylesheet.statement_declarations stmt))

let decl_to_prop_value decl =
  ( Css.declaration_name decl,
    decl_with_important decl (Css.declaration_value_for_equivalence decl) )

(* What a changed declaration puts in the report: the key answers whether the
   two sides differ, and the author's own spelling is what the reader is shown.
   Printing the key instead quotes a value neither file holds, since the key
   folds the spellings the two sides chose onto one. *)
let decl_to_reported_value decl =
  let name, key = decl_to_prop_value decl in
  (name, (key, decl_shown_value decl))

let compare_prop_value (name1, value1) (name2, value2) =
  let by_name = String.compare name1 name2 in
  if by_name <> 0 then by_name else String.compare value1 value2

let equal_prop_value (name1, value1) (name2, value2) =
  String.equal name1 name2 && String.equal value1 value2

let decls_signature (decls : Css.declaration list) =
  List.map decl_to_prop_value decls |> List.sort compare_prop_value

let equal_decls_signature = List.equal equal_prop_value

(* Normalize a selector string by sorting comma-separated selector items. This
   ensures we consider ".a,.b" equivalent to ".b,.a" when matching.

   Policy: Selector lists with the same items in different orders are considered
   equivalent for matching purposes. This means: - ".a, .b" and ".b, .a" will
   match as the same selector - Reordering within a list is not considered a
   structural change - This prevents false positives when CSS tools reorder
   selector lists *)
let rule_selector stmt =
  match Css.statement_selector stmt with
  | Some s -> s
  | None -> Css.Selector.universal

(* [selector_key_of_*] is called O(N M) times during structural rule diffs. Use
   the typed selector AST as the key: normalise a [List] of selectors by sorting
   the alternatives so [h1, h2] and [h2, h1] map to the same key, then rely on
   structural equality + [Hashtbl.hash]. Avoids serialising through
   [Pp.to_string] for every comparison. *)
let selector_key_of_selector (sel : Css.Selector.t) : Css.Selector.t =
  match sel with
  | List subs -> List (List.sort Selector.compare subs)
  | _ -> sel

let selector_key_of_stmt stmt = selector_key_of_selector (rule_selector stmt)
let rule_declarations = Css.Stylesheet.statement_declarations

let rule_nested stmt =
  match Css.as_rule stmt with Some (_, _, nested) -> nested | None -> []

(* A container takes part in the ordering comparison too: swapping a rule with
   an [@media] that writes the same property flips the cascade winner. *)
type order_key = Rule_order of Css.Selector.t | Block_order of string

let order_key_of_stmt stmt =
  match Css.as_rule stmt with
  | Some (sel, _, _) -> Some (Rule_order (selector_key_of_selector sel))
  | None -> Option.map (fun desc -> Block_order desc) (describe_statement stmt)

(* Rules take part once per selector: a selector split over several rules moves
   as one cascade participant. Containers do not have that identity. Separate
   blocks under the same condition can sit on opposite sides of another rule, so
   number their occurrences instead of collapsing them onto the first. *)
let order_keys_in_order stmts =
  let seen_rules = Hashtbl.create (List.length stmts) in
  let block_occurrences = Hashtbl.create 8 in
  List.filter_map
    (fun stmt ->
      match order_key_of_stmt stmt with
      | Some (Rule_order _ as key) when not (Hashtbl.mem seen_rules key) ->
          Hashtbl.add seen_rules key ();
          Some (key, 0)
      | Some (Block_order _ as key) ->
          let occurrence =
            Option.value ~default:0 (Hashtbl.find_opt block_occurrences key)
          in
          Hashtbl.replace block_occurrences key (occurrence + 1);
          Some (key, occurrence)
      | Some (Rule_order _) | None -> None)
    stmts

(* Positions of one longest increasing subsequence of [ranks], by patience
   sorting: [tails.(l)] is the position ending the smallest subsequence of
   length [l + 1] seen so far, and [prev] chains each position to its
   predecessor. *)
let increasing_subsequence ranks =
  let n = Array.length ranks in
  let tails = Array.make n 0 in
  let prev = Array.make n (-1) in
  let len = ref 0 in
  for i = 0 to n - 1 do
    let lo = ref 0 and hi = ref !len in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      if ranks.(tails.(mid)) < ranks.(i) then lo := mid + 1 else hi := mid
    done;
    let pos = !lo in
    prev.(i) <- (if pos > 0 then tails.(pos - 1) else -1);
    tails.(pos) <- i;
    if pos = !len then incr len
  done;
  let members = Array.make n false in
  (if !len > 0 then
     let i = ref tails.(!len - 1) in
     while !i >= 0 do
       members.(!i) <- true;
       i := prev.(!i)
     done);
  members

(* Statements whose order against the rest actually inverted. Judged on the
   statements both sides share, since one the other side never had shifts every
   absolute position after it without transposing anything: comparing positions
   on each side reports the whole tail of the stylesheet as reordered whenever a
   rule is added or dropped. Anchoring on a longest order-preserving matching of
   that common sequence also keeps one move to one entry, where comparing rank
   pairwise would name every statement the move passed. *)
let moved_order_keys stmts1 stmts2 =
  let keys2 = order_keys_in_order stmts2 in
  let rank2 = Hashtbl.create (List.length keys2) in
  List.iteri (fun i key -> Hashtbl.replace rank2 key i) keys2;
  let common = List.filter (Hashtbl.mem rank2) (order_keys_in_order stmts1) in
  let ranks = Array.of_list (List.map (Hashtbl.find rank2) common) in
  let anchored = increasing_subsequence ranks in
  let moved = Hashtbl.create (Array.length ranks) in
  List.iteri
    (fun i (key, _) -> if not anchored.(i) then Hashtbl.replace moved key ())
    common;
  moved

let selector_moved moved sel = Hashtbl.mem moved (Rule_order sel)

(* Generic helper for finding added/removed/modified items between two lists.
   [key_of] returns a canonical structural key. *)
let diffs ~(key_of : 'item -> 'key) ~(is_empty_diff : 'item -> 'item -> bool)
    items1 items2 =
  let items1_keyed = List.map (fun i -> (i, key_of i)) items1 in
  let items2_keyed = Array.of_list (List.map (fun i -> (i, key_of i)) items2) in
  (* A FIFO per key gives duplicate keys the same source-order pairing as the
     old left-to-right scan, without restarting that scan for every item. *)
  let available = Hashtbl.create (Array.length items2_keyed) in
  Array.iteri
    (fun i (item, key) ->
      let bucket =
        match Hashtbl.find_opt available key with
        | Some bucket -> bucket
        | None ->
            let bucket = Queue.create () in
            Hashtbl.add available key bucket;
            bucket
      in
      Queue.add (i, item) bucket)
    items2_keyed;
  let claimed = Array.make (Array.length items2_keyed) false in
  let claim key =
    match Hashtbl.find_opt available key with
    | None -> None
    | Some bucket -> (
        match Queue.take_opt bucket with
        | None -> None
        | Some (i, item) ->
            claimed.(i) <- true;
            Some item)
  in
  let pairs, removed =
    List.fold_left
      (fun (pairs, removed) (item1, key1) ->
        match claim key1 with
        | Some item2 -> ((item1, item2) :: pairs, removed)
        | None -> (pairs, item1 :: removed))
      ([], []) items1_keyed
  in
  let pairs = List.rev pairs and removed = List.rev removed in
  let added =
    Array.to_list items2_keyed
    |> List.filteri (fun i _ -> not claimed.(i))
    |> List.map fst
  in
  let modified =
    List.filter (fun (item1, item2) -> not (is_empty_diff item1 item2)) pairs
  in
  (added, removed, modified)

let rules_added_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let is_empty_diff _ _ = true in
  let added, _removed, _modified = diffs ~key_of ~is_empty_diff rules1 rules2 in
  added

let rules_removed_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let is_empty_diff _ _ = true in
  let _added, removed, _modified = diffs ~key_of ~is_empty_diff rules1 rules2 in
  removed

let selectors_share_parent sel1_str sel2_str =
  (* Check if two selectors share a common parent context *)
  let parts1 = String.split_on_char ' ' sel1_str |> List.rev in
  let parts2 = String.split_on_char ' ' sel2_str |> List.rev in
  match (parts1, parts2) with
  | _ :: p1_rest, _ :: p2_rest ->
      List.rev p1_rest = List.rev p2_rest && p1_rest <> []
  | _ -> false

(* The rules of [rules2] by selector key and by declaration signature. A rule is
   named by its position in [rules2]: two identical rules are still two rules,
   and claiming one must leave the other free. *)
let build_rule_lookup_tables rules2 =
  let rules2_by_key = Hashtbl.create (List.length rules2) in
  let rules2_by_props = Hashtbl.create (List.length rules2) in
  List.iteri
    (fun i r ->
      let key = selector_key_of_stmt r in
      let decls = rule_declarations r in
      let props = decls_signature decls in
      let existing_key =
        try Hashtbl.find rules2_by_key key with Not_found -> []
      in
      Hashtbl.replace rules2_by_key key ((i, r) :: existing_key);
      let existing_props =
        try Hashtbl.find rules2_by_props props with Not_found -> []
      in
      Hashtbl.replace rules2_by_props props ((i, r) :: existing_props))
    rules2;
  (* Buckets were built by prepending. Put them back in source order so every
     equal-quality choice below has a stable positional tie-break. *)
  Hashtbl.iter
    (fun key rules -> Hashtbl.replace rules2_by_key key (List.rev rules))
    rules2_by_key;
  Hashtbl.iter
    (fun props rules -> Hashtbl.replace rules2_by_props props (List.rev rules))
    rules2_by_props;
  (rules2_by_key, rules2_by_props)

(* A rule paired with its partner on the other side: the two selectors and the
   two declaration lists, in the order the rule matcher hands them on. *)
type rule_modification =
  Css.Selector.t * Css.Selector.t * Css.declaration list * Css.declaration list

(* What an exact match reports. It usually says nothing: the same selector
   carrying the same declarations is the same rule. One exception: the rule sits
   somewhere else against the rest of the sheet, which is a cascade change
   however identical the block is, so the pair has to reach
   [convert_modified_rule] for the reorder entry to be made. *)
type exact_match = Same | Moved of rule_modification

(* Try to find an exact match by selector key and declarations. [None] when
   nothing on the other side matches exactly. *)
let try_exact_match ~moved rules2_by_key used r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  match
    List.find_opt
      (fun (i, r) ->
        (not used.(i))
        && List.equal Declaration.equal_declaration (rule_declarations r) d1
        && Stylesheet.equal (rule_nested r) (rule_nested r1))
      candidates
  with
  | Some (i, exact) ->
      used.(i) <- true;
      let sel1 = rule_selector r1 in
      let sel2 = rule_selector exact in
      if selector_moved moved key1 then Some (Moved (sel1, sel2, d1, d1))
      else Some Same
  | None -> None

(* The multiset of properties a rule writes. Values are deliberately absent: two
   repeated occurrences that both write [color] stay tied and source order
   exposes a changed last winner instead of pairing equal values across the
   sheet. Repeated properties remain repeated in the footprint. *)
let declaration_footprint declarations =
  List.map Css.declaration_name declarations |> List.sort String.compare

(* The size of the symmetric multiset difference between two sorted declaration
   footprints. *)
let rec footprint_distance one other =
  match (one, other) with
  | [], rest | rest, [] -> List.length rest
  | name1 :: rest1, name2 :: rest2 ->
      let order = String.compare name1 name2 in
      if order = 0 then footprint_distance rest1 rest2
      else if order < 0 then 1 + footprint_distance rest1 other
      else 1 + footprint_distance one rest2

(* Find the unused same-key rule with the closest declaration footprint. Buckets
   and this fold are in source order, so the earlier occurrence wins a tie. *)
let try_same_key_match rules2_by_key used r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  let footprint1 = declaration_footprint d1 in
  let closest =
    List.fold_left
      (fun best ((i, r) as candidate) ->
        if used.(i) then best
        else
          let distance =
            footprint_distance footprint1
              (declaration_footprint (rule_declarations r))
          in
          match best with
          | None -> Some (distance, candidate)
          | Some (best_distance, _) when distance < best_distance ->
              Some (distance, candidate)
          | Some _ -> best)
      None candidates
    |> Option.map snd
  in
  match closest with
  | Some (i, r2) ->
      used.(i) <- true;
      let d2 = rule_declarations r2 in
      Some (rule_selector r1, rule_selector r2, d1, d2)
  | None -> None

(* Try to find equivalent rule by properties with shared parent *)
let try_equivalent_props_match rules2_by_props used r1 d1 props1 =
  let candidates =
    try Hashtbl.find rules2_by_props props1 with Not_found -> []
  in
  let sel1_str = Css.Selector.to_string (rule_selector r1) in
  match
    List.find_opt
      (fun (i, r) ->
        (not used.(i))
        &&
        let sel2_str = Css.Selector.to_string (rule_selector r) in
        selectors_share_parent sel1_str sel2_str)
      candidates
  with
  | Some (i, r2) ->
      used.(i) <- true;
      let d2 = rule_declarations r2 in
      Some (rule_selector r1, rule_selector r2, d1, d2)
  | None -> None

let pick_non_exact_rule rules2_by_key rules2_by_props used r1 key1 d1 props1 =
  match try_same_key_match rules2_by_key used r1 key1 d1 with
  | Some result -> Some result
  | None -> try_equivalent_props_match rules2_by_props used r1 d1 props1

(* The pairs the rules of [rules1] make with [rules2], as [(moved, changed)]:
   the rules that only changed places, and the rules whose content or selector
   differs. They are kept apart because only the second answers whether the two
   sheets differ structurally, which is what decides how the ordering is read
   back. *)
let rules_modified_diff ~moved rules1 rules2 =
  let rules2_by_key, rules2_by_props = build_rule_lookup_tables rules2 in
  let used = Array.make (List.length rules2) false in
  (* Claim every exact match first, wherever it sits. Matching in one greedy
     pass lets an early rule take, through the property fallback, the partner a
     later rule matches exactly, so a page of [.to-*] gradient rules, all with
     the same property signature, pairs off by position and every one reports as
     modified. *)
  let exact, pending =
    List.partition_map
      (fun r1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = rule_declarations r1 in
        match try_exact_match ~moved rules2_by_key used r1 key1 d1 with
        | Some pick -> Left pick
        | None -> Right r1)
      rules1
  in
  let moved_exact =
    List.filter_map (function Moved pair -> Some pair | _ -> None) exact
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | r1 :: t1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = rule_declarations r1 in
        let props1 = decls_signature d1 in
        let pick =
          pick_non_exact_rule rules2_by_key rules2_by_props used r1 key1 d1
            props1
        in
        let acc = match pick with None -> acc | Some x -> x :: acc in
        aux acc t1
  in
  (moved_exact, aux [] pending)

let has_same_selectors rules1 rules2 =
  if List.length rules1 <> List.length rules2 then false
  else
    (* Use hash table for O(n) comparison instead of O(n log n) sorting *)
    let keys1_counts = Hashtbl.create (List.length rules1) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys1_counts key with Not_found -> 0 in
        Hashtbl.replace keys1_counts key (count + 1))
      rules1;

    let keys2_counts = Hashtbl.create (List.length rules2) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys2_counts key with Not_found -> 0 in
        Hashtbl.replace keys2_counts key (count + 1))
      rules2;

    (* Check if hash tables are equivalent *)
    try
      Hashtbl.iter
        (fun key count1 ->
          let count2 =
            try Hashtbl.find keys2_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys1_counts;

      Hashtbl.iter
        (fun key count2 ->
          let count1 =
            try Hashtbl.find keys1_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys2_counts;

      true
    with Exit -> false

let build_selector_map rules =
  (* Create map from selector to declarations *)
  List.fold_left
    (fun acc rule ->
      let sel = rule_selector rule in
      let decls = rule_declarations rule in
      (sel, decls) :: acc)
    [] rules
  |> List.rev

(* The conditions of the containers that changed places against the rest of the
   enclosing statement list, judged on the same key space as the rule ordering.
   The absolute index a container sits at is not that coordinate: one statement
   inserted ahead of a block renumbers it and everything after it without
   transposing anything, which is why the index comparisons this replaces
   carried a slack distance and still answered the wrong question on both sides
   of it. [moved_order_keys] anchors on a longest order-preserving matching of
   the statements both sides share, so an insertion moves nothing at any
   distance and a block that swapped with a rule or another block moves even by
   one position. *)
let moved_conditions ~condition_of stmts1 stmts2 =
  let moved = moved_order_keys stmts1 stmts2 in
  let conds = Hashtbl.create 8 in
  List.iter
    (fun stmt ->
      match (condition_of stmt, order_key_of_stmt stmt) with
      | Some cond, Some key when Hashtbl.mem moved key ->
          Hashtbl.replace conds cond ()
      | _ -> ())
    stmts1;
  conds

(* Locate matching declarations in map2 for a given selector key *)
let matching_decls_in_map2 sel1_key decls1 map2 decls2 =
  (* Prefer an exact declaration match for the same selector key if available *)
  match
    List.find_opt
      (fun (s, d) ->
        Selector.equal (selector_key_of_selector s) sel1_key
        && List.equal Declaration.equal_declaration d decls1)
      map2
  with
  | Some (s, d) -> (d, Some s)
  | None -> (
      match
        List.find_opt
          (fun (s, _) -> Selector.equal (selector_key_of_selector s) sel1_key)
          map2
      with
      | Some (s, d) -> (d, Some s)
      | None -> (decls2, None))

let add_ordering_issue ~moved map2 acc sel1 decls1 sel2 decls2 =
  let sel1_key = selector_key_of_selector sel1 in
  let sel2_key = selector_key_of_selector sel2 in
  if Selector.equal sel1_key sel2_key then
    (* Same selector at this position: a difference only when its declarations
       differ, i.e. same-selector rules were reordered so the cascade winner
       flips. *)
    if equal_decls_signature (decls_signature decls1) (decls_signature decls2)
    then acc
    else (sel1, sel2, decls1, decls2) :: acc
  else if selector_moved moved sel1_key then
    (* Report the selector that moved, not the ones it displaced: a rule pulled
       to the front sits opposite a different selector at every position it
       passed, and pairing on that named each of them instead. *)
    let decls1_from_map2, sel2_opt =
      matching_decls_in_map2 sel1_key decls1 map2 decls2
    in
    let sel2 = match sel2_opt with Some s -> s | None -> sel1 in
    (sel1, sel2, decls1, decls1_from_map2) :: acc
  else acc

(* no-op: pure rule ordering is handled in handle_structural_diff via
   has_ordering_changes/ordering_diff *)

let ordering_diff ~moved rules1 rules2 =
  let map1 = build_selector_map rules1 in
  let map2 = build_selector_map rules2 in

  let rec find_ordering_issues acc remaining1 remaining2 =
    match (remaining1, remaining2) with
    | [], [] -> List.rev acc
    | (sel1, decls1) :: rest1, (sel2, decls2) :: rest2 ->
        let acc = add_ordering_issue ~moved map2 acc sel1 decls1 sel2 decls2 in
        find_ordering_issues acc rest1 rest2
    | _, _ -> List.rev acc
  in

  find_ordering_issues [] map1 map2

let extract_base_parent_selector sel =
  let sel_str = Css.Selector.to_string sel in
  match String.index_opt sel_str ' ' with
  | None -> None
  | Some sp ->
      let parent = String.sub sel_str 0 sp in
      let stripped =
        match String.index_opt parent ':' with
        | Some idx -> String.sub parent 0 idx
        | None -> parent
      in
      Some stripped

let selectors_share_parent_ast sel1 sel2 =
  match
    (extract_base_parent_selector sel1, extract_base_parent_selector sel2)
  with
  | Some p1, Some p2 -> p1 = p2
  | _ -> false

let selector_changes all_added_candidates all_removed_candidates =
  (* Index added rules by their declaration signature so the inner loop is a
     hashtable lookup, not a linear scan over [all_added_candidates]. With N
     removed and M added rules, the previous shape was O(N M) [decls_signature]
     computations; now it's O(N + M) plus the per-bucket scan for the
     share-parent check (buckets are typically small). *)
  let added_by_props : (string list, Css.statement list) Hashtbl.t =
    Hashtbl.create (List.length all_added_candidates)
  in
  List.iter
    (fun added ->
      let props = decls_signature (rule_declarations added) |> List.map snd in
      let prev =
        Hashtbl.find_opt added_by_props props |> Option.value ~default:[]
      in
      Hashtbl.replace added_by_props props (added :: prev))
    all_added_candidates;
  let added_with_props_sig sig_strings =
    Hashtbl.find_opt added_by_props sig_strings |> Option.value ~default:[]
  in
  let matched_added = ref [] in
  let matched_removed = ref [] in
  let changes = ref [] in
  List.iter
    (fun removed_rule ->
      let removed_sel = rule_selector removed_rule in
      let removed_decls = rule_declarations removed_rule in
      let removed_props = decls_signature removed_decls |> List.map snd in
      let matching_added =
        List.find_opt
          (fun added_rule ->
            let added_sel = rule_selector added_rule in
            (not (Selector.equal removed_sel added_sel))
            && selectors_share_parent_ast removed_sel added_sel)
          (added_with_props_sig removed_props)
      in
      match matching_added with
      | Some added_rule ->
          let added_sel = rule_selector added_rule in
          changes :=
            (removed_sel, added_sel, removed_decls, removed_decls) :: !changes;
          matched_removed := removed_rule :: !matched_removed;
          matched_added := added_rule :: !matched_added
      | None -> ())
    all_removed_candidates;
  (!changes, !matched_added, !matched_removed)

(* Filter other_modified to exclude changes already captured as selector
   changes *)
let exclude_modified_selector_changes sel_changes other_modified =
  let sel_change_selectors =
    List.map
      (fun (sel1, sel2, _, _) ->
        (Css.Selector.to_string sel1, Css.Selector.to_string sel2))
      sel_changes
  in
  List.filter
    (fun (sel1, sel2, _, _) ->
      let sel1_str = Css.Selector.to_string sel1 in
      let sel2_str = Css.Selector.to_string sel2 in
      not (List.mem (sel1_str, sel2_str) sel_change_selectors))
    other_modified

(* The single selectors and declaration signature of a flat rule (no nested
   body); [None] for any other statement. *)
let flat_rule_parts stmt =
  match Css.as_rule stmt with
  | Some (sel, decls, []) ->
      let subs = match sel with List subs -> subs | s -> [ s ] in
      Some (decls, subs, decls_signature decls)
  | _ -> None

let grouping_pair_count rules =
  let h = Hashtbl.create 16 in
  List.iter
    (fun stmt ->
      match flat_rule_parts stmt with
      | Some (_, subs, sign) ->
          List.iter
            (fun sub ->
              let p = (selector_key_of_selector sub, sign) in
              Hashtbl.replace h p
                (1 + Option.value ~default:0 (Hashtbl.find_opt h p)))
            subs
      | None -> ())
    rules;
  h

(* Drop each selector whose pair the [common] budget still covers; keep the rule
   unchanged when none drop, trim it to the survivors otherwise, remove it when
   all drop. *)
let trim_reconciled_grouping common rules =
  let budget = Hashtbl.copy common in
  List.filter_map
    (fun stmt ->
      match flat_rule_parts stmt with
      | Some (decls, subs, sign) ->
          let kept =
            List.filter
              (fun sub ->
                let p = (selector_key_of_selector sub, sign) in
                match Hashtbl.find_opt budget p with
                | Some n when n > 0 ->
                    Hashtbl.replace budget p (n - 1);
                    false
                | _ -> true)
              subs
          in
          if kept = [] then None
          else if List.compare_lengths kept subs = 0 then Some stmt
          else
            let selector =
              match kept with [ s ] -> s | many -> Css.Selector.list many
            in
            Some (Css.rule ~selector decls)
      | None -> Some stmt)
    rules

(* A comma-grouped rule split or merged across rules with identical declarations
   ([.a, .b { x }] vs [.a { x } .b { x }]) is not a semantic change: the same
   [(single selector, declarations)] pairs survive, only regrouped. Reconcile
   the leftover add/remove candidates at the pair level so the regrouping does
   not read as add/remove noise - a pair on both sides is unchanged and drops
   from each, trimming the rule's selector list, or dropping the rule when no
   selector survives. Restricted to flat rules: a nested rule's [(selector,
   declarations)] pair does not capture its nested body. *)
let partial_trim added removed =
  let added_count = grouping_pair_count added in
  let removed_count = grouping_pair_count removed in
  let common = Hashtbl.create 16 in
  Hashtbl.iter
    (fun p ac ->
      match Hashtbl.find_opt removed_count p with
      | Some rc -> Hashtbl.replace common p (min ac rc)
      | None -> ())
    added_count;
  if Hashtbl.length common = 0 then (added, removed)
  else
    ( trim_reconciled_grouping common added,
      trim_reconciled_grouping common removed )

(* Declaration signatures in the order [decls_signature] sorts declarations. *)
module Signature_map = Map.Make (struct
  type t = (string * string) list

  let compare = List.compare compare_prop_value
end)

(* The flat rules of [rules] by declaration signature, each with its single
   selectors, in source order. *)
let flat_rules_by_signature rules =
  List.fold_left
    (fun groups stmt ->
      match flat_rule_parts stmt with
      | Some (_, subs, sign) ->
          Signature_map.update sign
            (fun group -> Some ((stmt, subs) :: Option.value ~default:[] group))
            groups
      | None -> groups)
    Signature_map.empty rules
  |> Signature_map.map List.rev

let rule_selector_str stmt =
  Option.map (fun (s, _, _) -> Css.Selector.to_string s) (Css.as_rule stmt)

(* A declaration signature is a pure regroup when its removed and added flat
   rules carry the same multiset of single selectors (only the grouping moved).
   Emit a [Regrouped] note for it; the rules are dropped from add/remove. *)
let detect_pure_regroups ~added_by_sig ~removed_by_sig =
  let single_keys group =
    List.concat_map
      (fun (_, subs) -> List.map selector_key_of_selector subs)
      group
    |> List.sort Selector.compare
  in
  let selectors group =
    List.filter_map (fun (stmt, _) -> rule_selector_str stmt) group
  in
  Signature_map.merge
    (fun _ radd rrem ->
      match (radd, rrem) with
      | Some radd, Some rrem
        when List.equal Selector.equal (single_keys radd) (single_keys rrem) ->
          Some
            (Regrouped
               {
                 from_selectors = selectors rrem;
                 to_selectors = selectors radd;
               }
              : rule_diff)
      | _ -> None)
    added_by_sig removed_by_sig

let reconcile_selector_grouping added removed =
  let pure =
    detect_pure_regroups
      ~added_by_sig:(flat_rules_by_signature added)
      ~removed_by_sig:(flat_rules_by_signature removed)
  in
  let not_pure stmt =
    match flat_rule_parts stmt with
    | Some (_, _, s) -> not (Signature_map.mem s pure)
    | None -> true
  in
  let added, removed =
    partial_trim (List.filter not_pure added) (List.filter not_pure removed)
  in
  (added, removed, List.map snd (Signature_map.bindings pure))

(* Key reorder detection uses both selector and declarations: two same-selector
   rules with conflicting declarations cascade last-wins. *)
let order_signature stmts =
  List.map
    (fun stmt ->
      (selector_key_of_stmt stmt, decls_signature (rule_declarations stmt)))
    stmts

let equal_order_signature =
  List.equal (fun (selector1, declarations1) (selector2, declarations2) ->
      Selector.equal selector1 selector2
      && equal_decls_signature declarations1 declarations2)

let handle_structural_diff rules1 rules2 =
  let all_added_candidates = rules_added_diff rules1 rules2 in
  let all_removed_candidates = rules_removed_diff rules1 rules2 in

  let sel_changes, matched_added, matched_removed =
    selector_changes all_added_candidates all_removed_candidates
  in

  let added =
    List.filter (fun r -> not (List.memq r matched_added)) all_added_candidates
  in
  let removed =
    List.filter
      (fun r -> not (List.memq r matched_removed))
      all_removed_candidates
  in
  let added, removed, regrouped = reconcile_selector_grouping added removed in

  let moved = moved_order_keys rules1 rules2 in
  let moved_exact, other_modified = rules_modified_diff ~moved rules1 rules2 in
  let filtered_other_modified =
    exclude_modified_selector_changes sel_changes other_modified
  in

  let modified = sel_changes @ filtered_other_modified in

  let has_structural_changes =
    added <> [] || removed <> [] || modified <> [] || regrouped <> []
  in
  let has_ordering_changes =
    (not has_structural_changes)
    && has_same_selectors rules1 rules2
    && not
         (equal_order_signature (order_signature rules1)
            (order_signature rules2))
  in

  (* With nothing else to report, the two sides line up position by position and
     [ordering_diff] can also name a swap of two rules that share a selector,
     which no order key distinguishes. Once something else did change that walk
     no longer lines up, and the rules that only changed places are the ones the
     order keys name. *)
  let modified_with_order =
    if has_ordering_changes then ordering_diff ~moved rules1 rules2 @ modified
    else moved_exact @ modified
  in

  (added, removed, modified_with_order, regrouped)

let rule_diffs rules1 rules2 = handle_structural_diff rules1 rules2

(* Each key and shown value a rule writes for [name], in the order it writes
   them. *)
let occurrences_of name props =
  List.filter_map (fun (p, v) -> if p = name then Some v else None) props

(* The property names of [props], each once, in first-appearance order. *)
let names_of props =
  List.fold_left
    (fun acc (p, _) -> if List.mem p acc then acc else p :: acc)
    [] props
  |> List.rev

(* Zip one name's occurrence lists. A rule may write a property several times -
   a fallback chain is the usual reason - so occurrence n on one side answers
   occurrence n on the other, and whichever side has more occurrences carries
   the surplus. Matching by name alone binds every occurrence to the first entry
   opposite and reports values neither side holds. *)
let rec zip_occurrences name (modified, added, removed) values1 values2 =
  match (values1, values2) with
  | [], [] -> (modified, added, removed)
  | (key1, shown1) :: rest1, (key2, shown2) :: rest2 ->
      let modified =
        if String.equal key1 key2 then modified
        else
          {
            property_name = name;
            expected_value = shown1;
            actual_value = shown2;
          }
          :: modified
      in
      zip_occurrences name (modified, added, removed) rest1 rest2
  | (_, shown) :: rest1, [] ->
      zip_occurrences name (modified, added, (name, shown) :: removed) rest1 []
  | [], (_, shown) :: rest2 ->
      zip_occurrences name (modified, (name, shown) :: added, removed) [] rest2

(* Helper function to compute property diffs between two declaration lists,
   including added and removed properties *)
let properties_diff decls1 decls2 :
    declaration list * (string * string) list * (string * string) list =
  let props1 = List.map decl_to_reported_value decls1 in
  let props2 = List.map decl_to_reported_value decls2 in
  (* Names the expected side writes first, then the ones only the actual side
     writes, so the report reads in source order. *)
  let names =
    let names1 = names_of props1 in
    names1 @ List.filter (fun p -> not (List.mem p names1)) (names_of props2)
  in
  let modified, added, removed =
    List.fold_left
      (fun acc name ->
        zip_occurrences name acc
          (occurrences_of name props1)
          (occurrences_of name props2))
      ([], [], []) names
  in
  (List.rev modified, List.rev added, List.rev removed)

(* Helper functions for converting rule changes - moved here for mutual
   recursion *)
(* A container still takes part in the ordering comparison, since swapping a
   rule with an [@media] is cascade-significant, but [container_changes] is what
   reports it. Converting it here as well gives a second entry for the same
   block. *)
let is_container_statement stmt =
  Css.as_media stmt <> None
  || Css.as_supports stmt <> None
  || Css.as_layer stmt <> None
  || Css.as_container stmt <> None

let convert_added_rule stmt =
  if is_container_statement stmt then None
  else
    let sel, decls = strings_of_rule stmt in
    Some (Added { selector = sel; declarations = decls } : rule_diff)

let convert_removed_rule stmt =
  if is_container_statement stmt then None
  else
    let sel, decls = strings_of_rule stmt in
    Some (Removed { selector = sel; declarations = decls } : rule_diff)

module Selector_tbl = Hashtbl.Make (struct
  type t = Css.Selector.t

  let equal = Selector.equal
  let hash = Selector.hash
end)

(* Where a [Reordered] entry places a selector: the index of the first rule each
   side writes it in, and the statements of the actual side by index. Built once
   per pair of statement lists, since a report can hold a move for every
   rule. *)
type positions = {
  first1 : int Selector_tbl.t;
  first2 : int Selector_tbl.t;
  stmts2 : Css.statement array;
}

let first_positions rules =
  let first = Selector_tbl.create (List.length rules) in
  List.iteri
    (fun i stmt ->
      match Css.as_rule stmt with
      | Some (s, _, _) ->
          let key = selector_key_of_selector s in
          if not (Selector_tbl.mem first key) then Selector_tbl.add first key i
      | None -> ())
    rules;
  first

let positions rules1 rules2 =
  {
    first1 = first_positions rules1;
    first2 = first_positions rules2;
    stmts2 = Array.of_list rules2;
  }

let selector_position sel first =
  Option.value ~default:(-1)
    (Selector_tbl.find_opt first (selector_key_of_selector sel))

let selector_at_position pos stmts =
  if pos < 0 || pos >= Array.length stmts then None
  else describe_statement stmts.(pos)

let content_changed selector old_decls new_decls =
  let property_changes, added_props, removed_props =
    properties_diff old_decls new_decls
  in
  Content_changed
    {
      selector;
      old_declarations = old_decls;
      new_declarations = new_decls;
      property_changes;
      added_properties = added_props;
      removed_properties = removed_props;
    }

let reordered ~positions sel1 sel2 selector : rule_diff =
  let expected_pos = selector_position sel1 positions.first1 in
  let actual_pos = selector_position sel2 positions.first2 in
  let swapped_with = selector_at_position expected_pos positions.stmts2 in
  (Reordered
     {
       selector;
       expected_pos;
       actual_pos;
       swapped_with;
       old_declarations = None;
       new_declarations = None;
     }
    : rule_diff)

(* The change is reported under [sel1], so [sel1] is what has to have moved. *)
let position_changed ~moved sel1 =
  selector_moved moved (selector_key_of_selector sel1)

let is_pure_decl_reordering decls1 decls2 =
  let property_changes, added_props, removed_props =
    properties_diff decls1 decls2
  in
  let pure =
    property_changes = [] && added_props = [] && removed_props = []
    && equal_decls_signature (decls_signature decls1) (decls_signature decls2)
  in
  (pure, property_changes, added_props, removed_props)

let decl_level_reorder selector decls1 decls2 : rule_diff =
  (Reordered
     {
       selector;
       expected_pos = -1;
       actual_pos = -1;
       swapped_with = None;
       old_declarations = Some decls1;
       new_declarations = Some decls2;
     }
    : rule_diff)

let decls_str_equal d1 d2 =
  List.length d1 = List.length d2
  && List.for_all2
       (fun x y -> decl_to_prop_value x = decl_to_prop_value y)
       d1 d2

let convert_modified_rule ~moved ~positions (sel1, sel2, decls1, decls2) =
  let sel1_str = Css.Selector.to_string sel1 in
  let sel2_str = Css.Selector.to_string sel2 in
  let position_changed () = position_changed ~moved sel1 in
  let reordered selector = reordered ~positions sel1 sel2 selector in
  let reorder_or_content selector d1 d2 =
    if position_changed () then Some (reordered selector)
    else Some (content_changed selector d1 d2)
  in

  (* Handle each modification case *)
  match (decls1, decls2) with
  | [], [] -> reorder_or_content sel1_str decls1 decls2
  | [], _ | _, [] -> Some (content_changed sel1_str decls1 decls2)
  (* The selector key ignores the order of a comma group, so the same
     alternatives written the other way round select the same elements with the
     same specificities and the rule is left alone. *)
  | _, _
    when not
           (Selector.equal
              (selector_key_of_selector sel1)
              (selector_key_of_selector sel2)) ->
      Some
        (Selector_changed
           {
             old_selector = sel1_str;
             new_selector = sel2_str;
             declarations = decls2;
           })
  | _, _ when List.equal Declaration.equal_declaration decls1 decls2 ->
      reorder_or_content sel1_str decls1 decls2
  | _, _ ->
      let pure, property_changes, added_props, removed_props =
        is_pure_decl_reordering decls1 decls2
      in
      if pure then
        if position_changed () then Some (reordered sel1_str)
        else if decls_str_equal decls1 decls2 then
          (* OCaml ASTs differ but string output is identical (e.g., Nested vs
             bare expression after calc() normalization) -- no real
             difference *)
          None
        else if reorder_is_significant decls1 decls2 then
          Some (decl_level_reorder sel1_str decls1 decls2)
        else (* cascade-neutral reorder of disjoint declarations *) None
      else if property_changes <> [] || added_props <> [] || removed_props <> []
      then Some (content_changed sel1_str decls1 decls2)
      else reorder_or_content sel1_str decls1 decls2

(* Assemble rule changes (added/removed/modified) between two rule lists *)

(* The selector a change is about, when it names one. *)
let changed_selector : rule_diff -> string option = function
  | Added { selector; _ }
  | Removed { selector; _ }
  | Content_changed { selector; _ } ->
      Some selector
  | Rearranged { selector; _ } -> Some selector
  | Reordered _ | Selector_changed _ | Regrouped _ -> None

let change_sides : rule_diff -> Css.declaration list * Css.declaration list =
  function
  | Added { declarations; _ } -> ([], declarations)
  | Removed { declarations; _ } -> (declarations, [])
  | Content_changed { old_declarations; new_declarations; _ } ->
      (old_declarations, new_declarations)
  | _ -> ([], [])

(* A property arriving and a property leaving, judged on what the entry names
   rather than on which constructor it is: a [Content_changed] that only
   restates a value, or that names nothing at all, moves no declaration. *)
let change_gains : rule_diff -> bool = function
  | Added _ -> true
  | Content_changed { added_properties; _ } -> added_properties <> []
  | _ -> false

let change_loses : rule_diff -> bool = function
  | Removed _ -> true
  | Content_changed { removed_properties; _ } -> removed_properties <> []
  | _ -> false

(* Every declaration [sel] writes on one side, across all of its rules. *)
let declarations_of_selector sel stmts =
  List.concat_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, decls, _) when Css.Selector.to_string selector = sel ->
          decls
      | _ -> [])
    stmts

(* Judge on every rule of the selector, not only the differing ones: a
   declaration a matching rule already carries distinguishes a move from a
   loss. *)
let merge_selector_group ~rules1 ~rules2 sel peers =
  let old_all = declarations_of_selector sel rules1
  and new_all = declarations_of_selector sel rules2 in
  if
    old_all <> []
    && equal_decls_signature (decls_signature old_all) (decls_signature new_all)
  then Rearranged { selector = sel; declarations = new_all }
  else
    content_changed sel
      (List.concat_map (fun d -> fst (change_sides d)) peers)
      (List.concat_map (fun d -> snd (change_sides d)) peers)

(* One selector, one node. Two rules writing the same selector in a container
   produced two sibling entries under the same label, one reporting a
   declaration added and the other a different one removed, which reads as a
   contradiction rather than as a declaration moving between them. The group
   collapses to a single before-and-after for that selector.

   Only a group that both gains and loses collapses: several rules added under
   one selector really are several additions, and merging those would hide the
   count. A single entry carries both sides of that on its own, and does when a
   container sits between two rules of one selector: the two sheets then no
   longer line up rule for rule, and the positional walk pairs one rule of the
   selector against another, reading as a swap of declarations that never left
   the selector. What the selector writes across all of its rules is what
   settles it. *)
let merge_same_selector_changes ~rules1 ~rules2 (changes : rule_diff list) :
    rule_diff list =
  (* Index the peers once. Looking for them with [List.filter] in the walk below
     scanned every reported change once per reported change, even though the
     common case is one entry per selector. The flags also replace two scans of
     a group; the reversed peer list is restored only for a group that actually
     collapses. *)
  let groups = Hashtbl.create (List.length changes) in
  List.iter
    (fun diff ->
      match changed_selector diff with
      | None -> ()
      | Some sel ->
          let peers, gains, loses =
            Option.value ~default:([], false, false)
              (Hashtbl.find_opt groups sel)
          in
          Hashtbl.replace groups sel
            ( diff :: peers,
              gains || change_gains diff,
              loses || change_loses diff ))
    changes;
  let done_ = Hashtbl.create 8 in
  List.filter_map
    (fun diff ->
      match changed_selector diff with
      | None -> Some diff
      | Some sel when Hashtbl.mem done_ sel -> None
      | Some sel -> (
          match Hashtbl.find_opt groups sel with
          | Some (reversed_peers, true, true) ->
              Hashtbl.replace done_ sel ();
              Some
                (merge_selector_group ~rules1 ~rules2 sel
                   (List.rev reversed_peers))
          | Some _ | None -> Some diff))
    changes

(* Everything a [Reordered] entry puts in the report, as one string; [None] for
   any other entry. Two entries sharing a key print the same line. *)
let reorder_key : rule_diff -> string option = function
  | Reordered r ->
      let buf = Buffer.create 64 in
      let add_decls = function
        | None -> Buffer.add_char buf '-'
        | Some decls ->
            List.iter
              (fun decl ->
                let name, value = decl_to_prop_value decl in
                add_strings buf [ name; ":"; value; ";" ])
              decls
      in
      add_strings buf
        [
          r.selector;
          "|";
          string_of_int r.expected_pos;
          "|";
          string_of_int r.actual_pos;
          "|";
          Option.value r.swapped_with ~default:"-";
          "|";
        ];
      add_decls r.old_declarations;
      Buffer.add_char buf '|';
      add_decls r.new_declarations;
      Some (Buffer.contents buf)
  | Added _ | Removed _ | Content_changed _ | Selector_changed _ | Rearranged _
  | Regrouped _ ->
      None

(* One move, one entry. A reorder names the selector and the position that
   selector holds on each side; which of its rules carried the declarations
   across is no part of it. A selector written by several rules that travel
   together therefore builds the same entry once per rule, and the node then
   states the move as many times as it has rules and counts it as many times
   over. Keyed on what the entry prints rather than on the selector alone, so a
   declaration-level reorder inside another rule of that selector is a different
   entry and stays. *)
let dedup_reorders (changes : rule_diff list) : rule_diff list =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun diff ->
      match reorder_key diff with
      | None -> true
      | Some key ->
          if Hashtbl.mem seen key then false
          else (
            Hashtbl.replace seen key ();
            true))
    changes

(* At-rules that carry neither a selector nor a condition the other processors
   key on: [@page], [@font-face], [@counter-style], [@scope], [@starting-style]
   and friends. [rule_diffs] gives every one of them the universal selector, so
   it pairs them without ever reading their bodies. What they hold below the
   brace decides how a pair is compared. *)
type at_rule_body =
  | Block of Css.statement list  (** statements, walked like a container *)
  | Declarations of Css.declaration list  (** a rule body without a rule *)
  | Opaque  (** descriptors, compared as the text they print to *)

(* Which of the three a statement is, is the only thing said here: the body
   itself comes from the shared readers. Listed one by one rather than closed
   with a wildcard, so an at-rule added to the AST does not compile until this
   processor has claimed it or handed it to another. *)
let at_rule_body (stmt : Css.statement) : at_rule_body option =
  match stmt with
  | Starting_style _ | Scope _ | Moz_document _ | When _ | Else _ ->
      Some (Block (Css.Stylesheet.statement_children stmt))
  (* With margin rules the declarations are only part of the body, so the whole
     block is compared as text instead. *)
  | Page _
  | Page_with_margins (_, _, [])
  | Position_try _ | Supports_condition _ ->
      Some (Declarations (Css.Stylesheet.statement_declarations stmt))
  | Font_face _ | Counter_style _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Viewport _ | Webkit_keyframes _
  | Moz_keyframes _ | Unknown_at_rule _ | Namespace _ ->
      Some Opaque
  (* Owned by another processor: a rule and a bare nesting block by the rule
     matcher, a container by [moved_order_keys], [@keyframes] and [@property] by
     their own, and the rest carry no body to compare. *)
  | Rule _ | Declarations _ | Layer _ | Media _ | Container _ | Supports _
  | Origin _ | Keyframes _ | Property _ | Layer_decl _ | Import _ | Charset _
  | Bang_comment _ ->
      None

(* [process_at_rules] owns these statements, so leaving them in the rule diff as
   well would report one change twice, once against the universal selector. *)
let is_selectorless_at_rule stmt = at_rule_body stmt <> None

(* Every statement a processor of its own reads and names.
   [selector_key_of_stmt] gives all of them the universal selector, so the rule
   matcher pairs a [@property] with a [@keyframes] with a [@media] and hands
   whichever it has one too many of to the report, where nothing can name it.
   Containers are the exception and stay: [moved_order_keys] reads their
   position, and [convert_added_rule]/[convert_removed_rule] keep them out of
   the entries. *)
let is_reported_by_own_processor stmt =
  is_selectorless_at_rule stmt
  || Css.as_property stmt <> None
  || Css.as_keyframes stmt <> None

(* A stylesheet may repeat one at-rule (several [@font-face] blocks, several
   [@page] rules), so the head alone does not name a block. Number the blocks
   that share a head and pair them in order. *)
let at_rule_items stmts =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun stmt ->
      match at_rule_body stmt with
      | None -> None
      | Some body ->
          let head, text = statement_text_split stmt in
          let n = Option.value ~default:0 (Hashtbl.find_opt seen head) in
          Hashtbl.replace seen head (n + 1);
          Some ((head, n), (head, body, text)))
    stmts

(* The container line already names the at-rule, so these changes carry no
   selector of their own; a second label would read as a second subject. *)

let at_rule_declarations_change decls1 decls2 =
  let property_changes, added_properties, removed_properties =
    properties_diff decls1 decls2
  in
  if
    property_changes = [] && added_properties = [] && removed_properties = []
    && not (reorder_is_significant decls1 decls2)
  then None
  else
    Some
      (Content_changed
         {
           selector = "";
           old_declarations = decls1;
           new_declarations = decls2;
           property_changes;
           added_properties;
           removed_properties;
         })

(* Descriptors hold neither statements nor declarations, so a pair of them is
   compared on the text it prints to. *)
let at_rule_text_change text1 text2 =
  Content_changed
    {
      selector = "";
      old_declarations = [];
      new_declarations = [];
      property_changes =
        [
          {
            property_name = "descriptors";
            expected_value = text1;
            actual_value = text2;
          };
        ];
      added_properties = [];
      removed_properties = [];
    }

(* A [Content_changed] naming a rule whose own declarations are untouched states
   a difference nothing shows: the rules nested inside it are reported as the
   nesting containers they are. Dropping it keeps the summary's count and the
   report's entries in step. *)
let content_change_says_something (diff : rule_diff) =
  match diff with
  | Content_changed
      {
        old_declarations;
        new_declarations;
        property_changes;
        added_properties;
        removed_properties;
        _;
      } ->
      property_changes <> [] || added_properties <> []
      || removed_properties <> []
      || not
           (List.equal Declaration.equal_declaration old_declarations
              new_declarations)
  | _ -> true

let to_rule_changes rules1 rules2 : rule_diff list =
  let rules1 =
    List.filter (fun s -> not (is_reported_by_own_processor s)) rules1
  in
  let rules2 =
    List.filter (fun s -> not (is_reported_by_own_processor s)) rules2
  in
  let r_added, r_removed, r_modified, r_regrouped = rule_diffs rules1 rules2 in
  let moved = moved_order_keys rules1 rules2 in
  let positions = positions rules1 rules2 in
  List.filter_map convert_added_rule r_added
  @ List.filter_map convert_removed_rule r_removed
  @ List.filter_map (convert_modified_rule ~moved ~positions) r_modified
  @ r_regrouped
  |> merge_same_selector_changes ~rules1 ~rules2
  |> dedup_reorders
  |> List.filter content_change_says_something
