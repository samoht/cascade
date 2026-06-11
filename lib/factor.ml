(** Cascade-aware rule factoring. *)

open Declaration
open Stylesheet
open Common

let src = Logs.Src.create "cascade.factor" ~doc:"Cascade CSS factor optimizer"

module Log = (val Logs.src_log src : Logs.LOG)

let preserve_list = List.preserve
let list_map_preserve = List.map_preserve
let same_minified_declaration = Shorthand.same_minified_declaration
let declaration_covers = Shorthand.declaration_covers
let is_all_declaration = Shorthand.is_all_declaration
let contains_vendor_pseudo_element = Merge.vendor
let extract_pseudo_element = Merge.pseudo
let canonical_selector_key = Merge.key
let merge_selector_list = Merge.selector_list
let merge_two_adjacent_rules = Merge.pair
let merge_rules = Merge.adjacent
let combine_identical_rules = Merge.identical ~same:same_minified_declaration

let combine_identical_rules_global ?extend_lists =
  Merge.identical_global ?extend_lists ~same:same_minified_declaration

let declarations_css_equal =
  Merge.declarations_equal ~same:same_minified_declaration

let newer_pseudo_class_compatible = Merge.compatible
let selectors_of_rule_selector = Edge.selectors
let counters = Stats.counters
let pass_stat = Stats.pass
let record_factor_saving = Stats.add_saving

(* CSS Cascade L5: when a run of adjacent rules shares two or more identical
   declarations, hoist them into a single grouped rule whose selector is the
   union of the originals, and keep the remaining (rule-specific) declarations
   in per-selector follow-up rules. The transformation preserves cascade order
   for every element, regardless of whether it matches one or several of the
   original selectors:

   S1 { X; A } S1, S2 { X } S2 { X; B } becomes S1 { A } S2 { B }

   Only adjacent rules are eligible; an intervening rule's matched elements
   could otherwise pick up declarations they didn't see in the source. Nested
   rules and unparsed merge keys disable factoring as a precaution.

   Byte budget: hoisting saves [(N - 1) * common_size] but costs
   [sum(selector_size_i) + N] extra characters for the new rule headers and
   commas. Only commit when the savings are positive. *)

let decls_pp_size = Size.decls

let rule_factor_boundary (r : Stylesheet.rule) =
  r.nested <> [] || r.merge_key <> None
  || contains_vendor_pseudo_element r.selector

let rule_factor_eligible (r : Stylesheet.rule) =
  (not (rule_factor_boundary r))
  && not (List.exists is_all_declaration r.declarations)

let decl_property = Summary.prop

(* Byte size of the printed rule [<selector>{<d1>;<d2>;...;<dn>}].
   [decls_pp_size] measures declarations without their separators - the [;]
   between them is added by the rule printer at emission time, so we add [n - 1]
   back here. *)
let rule_pp_size = Size.rule
let rules_pp_size = Size.rules
let decl_list_size = Size.decl_list
let rule_size_from_parts = Size.rule_from_parts

module Prop_set = Summary.Props
module Prop_map = Summary.Map

type summary = Summary.t

let prop_ids_empty = Summary.ids_empty
let prop_ids_mem = Summary.ids_mem
let prop_ids_disjoint = Summary.ids_disjoint
let prop_ids_inter = Summary.ids_inter

(* Memoise summaries by physical identity. Rules are persistent records, so the
   same rule heap object is inspected by many overlapping factoring windows in a
   single [to_fixpoint] iteration; [rule_pp_size] does two [Pp.size] traversals
   each call, which made the inner factoring loops quadratic in the window size.
   We key on the boxed [Obj.repr] of the rule: [Hashtbl] uses our [equal =
   (==)], so misses on identity, never false hits. The hash function is
   fixed-depth (stdlib default depth, capped) so it is bounded regardless of the
   rule's structure. *)
module Rule_id_tbl = Hashtbl.Make (struct
  type t = Stylesheet.rule

  let equal = ( == )

  (* O(1) bucket hash combining the cached [Declaration.hash] of the first two
     declarations -- both are field loads on the structural fingerprint stored
     at declaration construction. With [equal = (==)], a bucket collision falls
     through to a physical pointer scan, so we never walk the rule structure on
     lookup. *)
  let hash (r : t) =
    match r.Stylesheet_intf.declarations with
    | [] -> 0
    | [ d ] -> Declaration.hash d
    | d1 :: d2 :: _ -> Declaration.hash d1 lxor (Declaration.hash d2 lsl 1)
end)

let summary_memo : summary Rule_id_tbl.t = Rule_id_tbl.create 4096
let clear () = Rule_id_tbl.reset summary_memo

let summarize_factor_rule factor_rule =
  match Rule_id_tbl.find_opt summary_memo factor_rule with
  | Some s ->
      counters.summary_hits <- counters.summary_hits + 1;
      s
  | None ->
      counters.summary_misses <- counters.summary_misses + 1;
      let s =
        Summary.v ~rule_size:rule_pp_size
          ~decl_size:(Pp.size ~minify:true Declaration.pp_declaration)
          ~selector_size:(Pp.size ~minify:true Selector.pp)
          factor_rule
      in
      Rule_id_tbl.add summary_memo factor_rule s;
      s

let declares_all summary props = Summary.declares_all summary props
let declares_ids summary props = Summary.declares_ids summary props
let summary_decl_for_prop summary prop = Summary.decl_for_prop summary prop

let summary_decl_size_for_prop summary prop =
  Summary.decl_size_for_prop summary prop

let summary_contains_declaration summary decl =
  Summary.contains ~same:same_minified_declaration summary decl

(* Hoisting [common] into a shared rule pays off for a member only when its
   selector entry ([|selector| + 1]) is cheaper than the bytes it would
   otherwise duplicate ([decls_pp_size common] plus one separator per
   declaration). A member fully consumed by [common] (empty leftover) always
   joins, since dropping it would spawn a whole separate rule. [leftovers] is
   aligned with [rules_arr] ([None] = empty leftover). Returns [None] when fewer
   than two members survive; otherwise the grouped rule (members only) and a
   leftover array where each pruned member keeps its full declarations inline.
   Pruning is cascade-safe: a pruned member's declarations stay in place, so the
   group is a subset of the run already proven safe to factor. *)
let cost_aware_factor_group (first : Stylesheet.rule)
    (rules_arr : Stylesheet.rule array) (factor_summaries : summary array)
    (common : Declaration.declaration list)
    (leftovers : Stylesheet.rule option array) :
    (Stylesheet.rule * Stylesheet.rule option array) option =
  let common_inline_cost = decls_pp_size common + List.length common in
  let member =
    Array.mapi
      (fun i summary ->
        match leftovers.(i) with
        | None -> true
        | Some _ ->
            (* Pruning only saves bytes when the member carries [common]
               verbatim: dropping it then removes exactly [common] from its
               rule. A member that overrides a [common] property (default-value
               factoring) keeps a differing value inline whether grouped or not,
               so the cost model does not apply - leave it in the group as the
               scan selected it. *)
            let exact =
              List.for_all
                (fun cd -> summary_contains_declaration summary cd)
                common
            in
            (not exact)
            || Summary.selector_size summary + 1 <= common_inline_cost)
      factor_summaries
  in
  let member_count =
    Array.fold_left
      (fun count keep -> if keep then count + 1 else count)
      0 member
  in
  if member_count < 2 then Option.None
  else
    let sels =
      Array.to_list rules_arr
      |> List.mapi (fun i r -> (i, r))
      |> List.filter (fun (i, _) -> member.(i))
      |> List.map (fun (_, (r : Stylesheet.rule)) -> r.Stylesheet_intf.selector)
    in
    let grouped =
      { first with selector = merge_selector_list sels; declarations = common }
    in
    let leftovers =
      Array.mapi
        (fun i lo -> if member.(i) then lo else Some rules_arr.(i))
        leftovers
    in
    Some (grouped, leftovers)

let first_decl_map decls =
  List.fold_left
    (fun map decl ->
      let prop = decl_property decl in
      if Prop_map.mem prop map then map else Prop_map.add prop decl map)
    Prop_map.empty decls

let common_prop_set_of_summaries = function
  | [] -> Prop_set.empty
  | first :: rest ->
      List.fold_left
        (fun props summary -> Prop_set.inter props (Summary.prop_set summary))
        (Summary.prop_set first) rest

let common_props_of_array summaries =
  let len = Array.length summaries in
  if len = 0 then Prop_set.empty
  else
    let props = ref (Summary.prop_set summaries.(0)) in
    for i = 1 to len - 1 do
      props := Prop_set.inter !props (Summary.prop_set summaries.(i))
    done;
    !props

let common_prop_ids_of_array summaries =
  let len = Array.length summaries in
  if len = 0 then [||]
  else
    let props = ref (Summary.prop_ids summaries.(0)) in
    for i = 1 to len - 1 do
      props := prop_ids_inter !props (Summary.prop_ids summaries.(i))
    done;
    !props

let common_decls_from_props common_props first_summary =
  List.filter_map
    (fun d ->
      let prop = decl_property d in
      if Prop_set.mem prop common_props then
        summary_decl_for_prop first_summary prop
      else None)
    (Summary.rule first_summary).Stylesheet_intf.declarations

let common_decls_from_ids common_ids first_summary =
  let rec loop i acc = function
    | [] -> List.rev acc
    | d :: rest ->
        let prop = decl_property d in
        let acc =
          if prop_ids_mem (Summary.decl_prop_ids first_summary).(i) common_ids
          then
            match summary_decl_for_prop first_summary prop with
            | Some decl -> decl :: acc
            | None -> acc
          else acc
        in
        loop (i + 1) acc rest
  in
  loop 0 [] (Summary.rule first_summary).Stylesheet_intf.declarations

let common_decls_from_summaries summaries first =
  let first_summary =
    match summaries with
    | first_summary :: _ -> first_summary
    | [] -> summarize_factor_rule first
  in
  common_decls_from_props (common_prop_set_of_summaries summaries) first_summary

let common_decl_entries_by_ids common_ids first_summary =
  let rec loop i acc = function
    | [] -> List.rev acc
    | d :: rest ->
        let prop = decl_property d in
        let acc =
          if prop_ids_mem (Summary.decl_prop_ids first_summary).(i) common_ids
          then
            match
              ( summary_decl_for_prop first_summary prop,
                summary_decl_size_for_prop first_summary prop )
            with
            | Some decl, Some size -> (decl, size) :: acc
            | _ -> acc
          else acc
        in
        loop (i + 1) acc rest
  in
  loop 0 [] (Summary.rule first_summary).Stylesheet_intf.declarations

let common_decls_size_by_ids common_ids first_summary =
  let entries = common_decl_entries_by_ids common_ids first_summary in
  let decls, size =
    List.fold_right
      (fun (decl, decl_size) (decls, size) -> (decl :: decls, size + decl_size))
      entries ([], 0)
  in
  (decls, size, List.length entries)

let minimum_leftover_size_by_ids common_ids summary =
  let rec loop i kept_count kept_size = function
    | [] -> (kept_count, kept_size)
    | size :: sizes ->
        if prop_ids_mem (Summary.decl_prop_ids summary).(i) common_ids then
          loop (i + 1) kept_count kept_size sizes
        else loop (i + 1) (kept_count + 1) (kept_size + size) sizes
  in
  let kept_count, kept_size = loop 0 0 0 (Summary.decl_sizes summary) in
  if kept_count = 0 then 0
  else rule_size_from_parts (Summary.selector_size summary) kept_size kept_count

let common_factorable_decls rules first =
  common_decls_from_summaries (List.map summarize_factor_rule rules) first

(* For a rule [R_i] whose value for [prop] equals the default, we must still
   emit it in the leftover when an EARLIER rule [R_j] with overlapping selector
   declares a different value - otherwise [R_j]'s leftover would override the
   shared default for elements matching both. *)
let earlier_overrides_overlap ?(start = 0) ~selector_summaries ~factor_summaries
    ~default i =
  let r_i_summary = Array.get selector_summaries i in
  let default_prop = decl_property default in
  let rec loop j =
    if j >= i then false
    else
      match
        summary_decl_for_prop (Array.get factor_summaries j) default_prop
      with
      | Some d
        when (not (same_minified_declaration d default))
             && Selector_summary.may_overlap
                  (Array.get selector_summaries j)
                  r_i_summary ->
          true
      | _ -> loop (j + 1)
  in
  loop start

let keep_factor_leftover ?(start = 0) ~selector_summaries ~factor_summaries
    ~default_decl ~i decl =
  (not (same_minified_declaration decl default_decl))
  || earlier_overrides_overlap ~start ~selector_summaries ~factor_summaries
       ~default:default_decl i

let leftover_for_factor_rule ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i (r : Stylesheet.rule) =
  List.filter
    (fun d ->
      let prop = decl_property d in
      match Prop_map.find_opt prop common_by_prop with
      | None -> true
      | Some default ->
          keep_factor_leftover ~start ~selector_summaries ~factor_summaries
            ~default_decl:default ~i d)
    r.declarations

let leftover_option ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i (r : Stylesheet.rule) : Stylesheet.rule option =
  let l =
    leftover_for_factor_rule ~start ~common_by_prop ~selector_summaries
      ~factor_summaries i r
  in
  if l = [] then Option.None else Some { r with declarations = l }

let leftover_options ~common ~selector_summaries ~factor_summaries rules_arr =
  let common_by_prop = first_decl_map common in
  Array.mapi
    (fun i r ->
      leftover_option ~common_by_prop ~selector_summaries ~factor_summaries i r)
    rules_arr
  |> Array.to_list

let leftover_size ?(start = 0) ~common_by_prop ~selector_summaries
    ~factor_summaries i summary : int option =
  let rec loop kept_count kept_decl_size decls sizes =
    match (decls, sizes) with
    | [], [] -> (kept_count, kept_decl_size)
    | decl :: decls, size :: sizes ->
        let keep =
          let prop = decl_property decl in
          match Prop_map.find_opt prop common_by_prop with
          | None -> true
          | Some default ->
              keep_factor_leftover ~start ~selector_summaries ~factor_summaries
                ~default_decl:default ~i decl
        in
        if keep then loop (kept_count + 1) (kept_decl_size + size) decls sizes
        else loop kept_count kept_decl_size decls sizes
    | _ -> assert false
  in
  let kept_count, kept_decl_size =
    loop 0 0 (Summary.rule summary).declarations (Summary.decl_sizes summary)
  in
  if kept_count = 0 then Option.None
  else
    Some
      (rule_size_from_parts
         (Summary.selector_size summary)
         kept_decl_size kept_count)

let safety =
  Factor_safe.v ~same_minified_declaration ~declaration_covers
    ~contains_vendor_pseudo_element ~rule_factor_boundary ~decl_property

let safe_summary summary =
  Factor_safe.summary (Summary.rule summary)
    ~selectors:(Summary.selector_summary summary)

module Factor_interval = struct
  type score = { common : declaration list; member : bool array; saving : int }

  type payload = {
    decls : declaration list;
    decl_size : int;
    decl_count : int;
    prop_ids : int array;
    by_prop : declaration Prop_map.t;
    inline_cost : int;
  }

  let common factor_summaries start common_props : payload option =
    if prop_ids_empty common_props then Option.None
    else
      let common_decls, common_decl_size, common_decl_count =
        common_decls_size_by_ids common_props factor_summaries.(start)
      in
      if common_decls = [] then Option.None
      else
        Some
          {
            decls = common_decls;
            decl_size = common_decl_size;
            decl_count = common_decl_count;
            prop_ids = common_props;
            by_prop = first_decl_map common_decls;
            inline_cost = common_decl_size + common_decl_count;
          }

  let keep_member payload summary (leftover_size : int option) =
    match leftover_size with
    | None -> true
    | Some _ ->
        let exact =
          List.for_all
            (fun cd -> summary_contains_declaration summary cd)
            payload.decls
        in
        (not exact) || Summary.selector_size summary + 1 <= payload.inline_cost

  let seed_contains seed offset =
    match seed with Option.None -> true | Option.Some seed -> seed.(offset)

  let score_members ?seed payload factor_summaries selector_summaries start len
      =
    let member = Array.make len false in
    let leftover_sizes : int option array = Array.make len Option.None in
    let member_count = ref 0 in
    for offset = 0 to len - 1 do
      if seed_contains seed offset then begin
        let i = start + offset in
        let summary = factor_summaries.(i) in
        let leftover_size =
          leftover_size ~start ~common_by_prop:payload.by_prop
            ~selector_summaries ~factor_summaries i summary
        in
        leftover_sizes.(offset) <- leftover_size;
        if keep_member payload summary leftover_size then (
          member.(offset) <- true;
          incr member_count)
      end
    done;
    (member, leftover_sizes, !member_count)

  let summary_rule_size summary =
    rule_size_from_parts
      (Summary.selector_size summary)
      (Summary.decl_pp_size summary)
      (Summary.decl_count summary)

  let grouped_size payload factor_summaries start member member_count =
    let selector_size = ref 0 in
    for offset = 0 to Array.length member - 1 do
      if member.(offset) then
        selector_size :=
          !selector_size
          + Summary.selector_size factor_summaries.(start + offset)
    done;
    rule_size_from_parts
      (!selector_size + max 0 (member_count - 1))
      payload.decl_size payload.decl_count

  let size payload factor_summaries start member
      (leftover_sizes : int option array) member_count =
    let before_size = ref 0 in
    let after_size =
      ref (grouped_size payload factor_summaries start member member_count)
    in
    for offset = 0 to Array.length member - 1 do
      let summary = factor_summaries.(start + offset) in
      before_size := !before_size + summary_rule_size summary;
      after_size :=
        !after_size
        +
        if member.(offset) then
          match leftover_sizes.(offset) with None -> 0 | Some size -> size
        else summary_rule_size summary
    done;
    !before_size - !after_size

  let optimistic_saving_upper_bound payload factor_summaries start len =
    let fixed = decl_list_size payload.decl_size payload.decl_count + 1 in
    let top1 = ref min_int and top2 = ref min_int in
    let positive_sum = ref 0 and positive_count = ref 0 in
    for offset = 0 to len - 1 do
      let summary = factor_summaries.(start + offset) in
      let min_leftover =
        minimum_leftover_size_by_ids payload.prop_ids summary
      in
      let adjusted =
        Summary.size summary - min_leftover - Summary.selector_size summary - 1
      in
      if adjusted > !top1 then (
        top2 := !top1;
        top1 := adjusted)
      else if adjusted > !top2 then top2 := adjusted;
      if adjusted > 0 then (
        positive_sum := !positive_sum + adjusted;
        incr positive_count)
    done;
    let best = if !positive_count >= 2 then !positive_sum else !top1 + !top2 in
    best - fixed

  let exact_score ?(allow_zero = false) ?seed payload factor_summaries
      selector_summaries start len : score option =
    counters.interval_scored <- counters.interval_scored + 1;
    let member, leftover_sizes, member_count =
      score_members ?seed payload factor_summaries selector_summaries start len
    in
    if member_count < 2 then None
    else
      let saving =
        size payload factor_summaries start member leftover_sizes member_count
      in
      if saving > 0 || (allow_zero && saving = 0) then
        Some { common = payload.decls; member; saving }
      else None

  let score ?(allow_zero = false) factor_summaries selector_summaries start stop
      common_props : score option =
    let len = stop - start + 1 in
    match common factor_summaries start common_props with
    | None -> None
    | Some _ when len < 2 -> None
    | Some payload ->
        let upper_bound =
          optimistic_saving_upper_bound payload factor_summaries start len
        in
        if upper_bound < 0 || ((not allow_zero) && upper_bound = 0) then (
          counters.interval_pruned <- counters.interval_pruned + 1;
          None)
        else
          exact_score ~allow_zero payload factor_summaries selector_summaries
            start len

  let build_scored (rules_arr : Stylesheet.rule array) factor_summaries
      selector_summaries start score =
    let first = rules_arr.(start) in
    let sels =
      let acc : Selector.t list ref = ref [] in
      for offset = Array.length score.member - 1 downto 0 do
        if score.member.(offset) then
          acc := rules_arr.(start + offset).Stylesheet_intf.selector :: !acc
      done;
      !acc
    in
    let grouped =
      {
        first with
        selector = merge_selector_list sels;
        declarations = score.common;
      }
    in
    let common_by_prop = first_decl_map score.common in
    let leftover_rev = ref [] in
    for offset = Array.length score.member - 1 downto 0 do
      let i = start + offset in
      let leftover =
        if score.member.(offset) then
          leftover_option ~start ~common_by_prop ~selector_summaries
            ~factor_summaries i rules_arr.(i)
        else Some rules_arr.(i)
      in
      match leftover with
      | None -> ()
      | Some r -> leftover_rev := r :: !leftover_rev
    done;
    (grouped :: !leftover_rev, score.saving)

  let build ?(allow_zero = false) rules_arr factor_summaries selector_summaries
      start stop common_props : (Stylesheet.rule list * int) option =
    score ~allow_zero factor_summaries selector_summaries start stop
      common_props
    |> Option.map
         (build_scored rules_arr factor_summaries selector_summaries start)

  let candidate ?(allow_zero = false) rules_arr factor_summaries common_props :
      (Stylesheet.rule list * int) option =
    let len = Array.length rules_arr in
    if len < 2 || prop_ids_empty common_props then Option.None
    else
      let selector_summaries =
        Array.map
          (fun s -> Lazy.force (Summary.selector_summary s))
          factor_summaries
      in
      build ~allow_zero rules_arr factor_summaries selector_summaries 0
        (len - 1) common_props

  let greedy rules_arr factor_summaries =
    match
      candidate ~allow_zero:true rules_arr factor_summaries
        (common_prop_ids_of_array factor_summaries)
    with
    | Some (replacement, saving) -> (replacement, saving)
    | None -> (Array.to_list rules_arr, 0)

  type candidate = { score : score }

  let factor_common_interval_lookahead = 24
  let factor_common_indexed_occurrence_window = 24
  let factor_common_indexed_max_span = 128
  let factor_common_indexed_max_candidates = 1024

  let add_candidate schedule factor_summaries selector_summaries start stop
      common_props =
    if not (prop_ids_empty common_props) then (
      counters.interval_candidates <- counters.interval_candidates + 1;
      match
        score factor_summaries selector_summaries start stop common_props
      with
      | None -> ()
      | Some score ->
          Weighted_interval.add schedule ~start ~stop ~weight:score.saving
            { score })

  let seeded_cascade_safe factor_summaries start seed common =
    let len = Array.length seed in
    let skipped_rev = ref [] in
    let safe = ref true in
    let offset = ref 0 in
    while !safe && !offset < len do
      let summary = factor_summaries.(start + !offset) in
      if seed.(!offset) then
        if
          List.exists
            (fun skipped ->
              Factor_safe.blocks_factor safety common (safe_summary summary)
                (safe_summary skipped))
            !skipped_rev
        then safe := false
        else ()
      else if
        not (Factor_safe.can_cross safety (Some common) (Summary.rule summary))
      then safe := false
      else skipped_rev := summary :: !skipped_rev;
      incr offset
    done;
    !safe

  let seeded_score factor_summaries selector_summaries start stop seed
      common_props =
    let len = stop - start + 1 in
    if
      len < 2
      || Array.length seed <> len
      || (not seed.(0))
      || not seed.(len - 1)
    then Option.None
    else
      match common factor_summaries start common_props with
      | None -> None
      | Some payload ->
          if seeded_cascade_safe factor_summaries start seed payload.decls then
            exact_score ~seed payload factor_summaries selector_summaries start
              len
          else None

  let add_seeded_candidate schedule factor_summaries selector_summaries ~start
      ~stop ~seed common_props =
    if not (prop_ids_empty common_props) then (
      counters.interval_candidates <- counters.interval_candidates + 1;
      match
        seeded_score factor_summaries selector_summaries start stop seed
          common_props
      with
      | None -> ()
      | Some score ->
          Weighted_interval.add schedule ~start ~stop ~weight:score.saving
            { score })

  let add_indexed_occurrence_slice schedule factor_summaries selector_summaries
      rows first last =
    let start = rows.(first) in
    let stop = rows.(last) in
    let len = stop - start + 1 in
    let count = last - first + 1 in
    if len > count then begin
      let seed = Array.make len false in
      let common_props = ref (Summary.prop_ids factor_summaries.(start)) in
      for i = first to last do
        let row = rows.(i) in
        seed.(row - start) <- true;
        if i <> first then
          common_props :=
            prop_ids_inter !common_props
              (Summary.prop_ids factor_summaries.(row))
      done;
      add_seeded_candidate schedule factor_summaries selector_summaries ~start
        ~stop ~seed !common_props
    end

  let indexed_candidate_count rows =
    let count = ref 0 in
    let row_count = Array.length rows in
    if row_count >= 2 then
      for first = 0 to row_count - 2 do
        let last_limit =
          min (row_count - 1)
            (first + factor_common_indexed_occurrence_window - 1)
        in
        let last = ref (first + 1) in
        while
          !last <= last_limit
          && rows.(!last) - rows.(first) <= factor_common_indexed_max_span
        do
          if rows.(!last) - rows.(first) > !last - first then incr count;
          incr last
        done
      done;
    !count

  let indexed_candidate_surface index =
    let count = ref 0 in
    Index.iter index (fun _ rows ->
        if !count <= factor_common_indexed_max_candidates then
          count := !count + indexed_candidate_count rows);
    !count

  let add_indexed_candidates schedule factor_summaries selector_summaries =
    let rules = Array.map (fun s -> Summary.rule s) factor_summaries in
    let index =
      Index.v ~same:same_minified_declaration ~keep:rule_factor_eligible rules
    in
    if indexed_candidate_surface index <= factor_common_indexed_max_candidates
    then
      Index.iter index (fun _ rows ->
          let row_count = Array.length rows in
          if row_count >= 2 then
            for first = 0 to row_count - 2 do
              let last_limit =
                min (row_count - 1)
                  (first + factor_common_indexed_occurrence_window - 1)
              in
              let last = ref (first + 1) in
              while
                !last <= last_limit
                && rows.(!last) - rows.(first) <= factor_common_indexed_max_span
              do
                add_indexed_occurrence_slice schedule factor_summaries
                  selector_summaries rows first !last;
                incr last
              done
            done)

  let index factor_summaries selector_summaries =
    let len = Array.length factor_summaries in
    let schedule = Weighted_interval.v ~length:len in
    for start = 0 to len - 2 do
      let common_props = ref (Summary.prop_ids factor_summaries.(start)) in
      let stop = ref (start + 1) in
      let last = min (len - 1) (start + factor_common_interval_lookahead - 1) in
      while !stop <= last && not (prop_ids_empty !common_props) do
        common_props :=
          prop_ids_inter !common_props
            (Summary.prop_ids factor_summaries.(!stop));
        add_candidate schedule factor_summaries selector_summaries start !stop
          !common_props;
        incr stop
      done
    done;
    add_indexed_candidates schedule factor_summaries selector_summaries;
    schedule

  let rewrite rules_arr factor_summaries : (Stylesheet.rule list * int) option =
    let len = Array.length rules_arr in
    if len < 3 then Option.None
    else
      let selector_summaries =
        Array.map
          (fun s -> Lazy.force (Summary.selector_summary s))
          factor_summaries
      in
      let schedule = index factor_summaries selector_summaries in
      let saving, selected = Weighted_interval.select schedule in
      counters.interval_selected <-
        counters.interval_selected + List.length selected;
      if saving <= 0 || selected = [] then Option.None
      else
        let rec emit i selected acc : (Stylesheet.rule list * int) option =
          if i >= len then Some (List.rev acc, saving)
          else
            match selected with
            | interval :: rest when interval.Weighted_interval.start = i ->
                let replacement, _ =
                  build_scored rules_arr factor_summaries selector_summaries
                    interval.start interval.value.score
                in
                emit (interval.stop + 1) rest (List.rev_append replacement acc)
            | _ -> emit (i + 1) selected (rules_arr.(i) :: acc)
        in
        emit 0 selected []
end

let factorise_group (rules : Stylesheet.rule list) : Stylesheet.rule list =
  match rules with
  | [] | [ _ ] -> rules
  | _ -> (
      let rules_arr = Array.of_list rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let greedy_rules, greedy_saving =
        Factor_interval.greedy rules_arr factor_summaries
      in
      match Factor_interval.rewrite rules_arr factor_summaries with
      | Some (dp_rules, dp_saving) when dp_saving > greedy_saving ->
          record_factor_saving dp_saving;
          dp_rules
      | _ ->
          record_factor_saving greedy_saving;
          greedy_rules)

let declarations_overlap common decls = Factor_safe.overlap safety common decls
let rule_selector_may_overlap_summary = Factor_safe.selector_overlap
let rule_specificity_ties_on_overlap = Factor_safe.specificity_ties

let skipped_rule_blocks_factor =
 fun common target skipped ->
  Factor_safe.blocks_factor safety common (safe_summary target)
    (safe_summary skipped)

let skipped_blocks_factor_tie common target skipped =
  Factor_safe.blocks_tie safety common (safe_summary target)
    (safe_summary skipped)

let boundary_stops_scan common_opt candidate =
  not (Factor_safe.can_cross safety common_opt candidate)

let rule_gap_merge_eligible (rule : Stylesheet.rule) =
  rule.nested = [] && rule.merge_key = None
  && not (contains_vendor_pseudo_element rule.selector)

(* [merged] is the combined rule a same-selector merge would produce. An
   intervening rule blocks it when it overlaps the selector, writes one of the
   merged rule's properties, and ties on specificity - then source order decides
   that property and the merge would change it. A strictly higher- or lower-
   specificity competitor is decided by specificity (not order), and one writing
   only other properties does not conflict; both are safe to cross. *)
let skipped_blocks_same_selector_merge (merged : Stylesheet.rule)
    (skipped : Stylesheet.rule) =
  rule_selector_may_overlap_summary skipped
    (Selector_summary.of_selector merged.Stylesheet_intf.selector)
  && declarations_overlap merged.declarations skipped.declarations
  && rule_specificity_ties_on_overlap merged skipped

let merge_same_selector_gaps (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items =
    List.map
      (fun (r : Stylesheet.rule) -> (r, canonical_selector_key r.selector))
      rules
  in
  let rec scan_for_match anchor anchor_key skipped_rev fuel
      (items : (Stylesheet.rule * Selector.t list) list) :
      (Stylesheet.rule list * (Stylesheet.rule * Selector.t list) list) option =
    match items with
    | [] -> Option.None
    | _ when fuel <= 0 -> Option.None
    | (candidate, candidate_key) :: tail ->
        if not (rule_gap_merge_eligible candidate) then Option.None
        else if anchor_key = candidate_key then
          let skipped = List.rev skipped_rev in
          let merged = merge_two_adjacent_rules anchor candidate in
          (* Check the merged rule (anchor + candidate declarations) against the
             intervening rules: a tie on any property the merged rule writes
             makes the merge observable. *)
          if List.exists (skipped_blocks_same_selector_merge merged) skipped
          then Option.None
          else
            let before = anchor :: (skipped @ [ candidate ]) in
            let after = merged :: skipped in
            if rules_pp_size after < rules_pp_size before then
              Some (merged :: skipped, tail)
            else Option.None
        else
          scan_for_match anchor anchor_key (candidate :: skipped_rev) (fuel - 1)
            tail
  in
  let rec walk acc = function
    | [] -> List.rev acc
    | (rule, key) :: rest -> (
        if not (rule_gap_merge_eligible rule) then walk (rule :: acc) rest
        else
          match scan_for_match rule key [] 128 rest with
          | None -> walk (rule :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

let filter_some xs = List.filter_map (fun x -> x) xs

let split_at n xs =
  let rec loop n acc xs =
    if n <= 0 then (List.rev acc, xs)
    else
      match xs with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (n - 1) (x :: acc) xs
  in
  loop n [] xs

let rules_with_skips factor_rules skipped : Stylesheet.rule list option =
  match factor_rules with
  | [] | [ _ ] -> Option.None
  | first :: _ -> (
      let rules_arr = Array.of_list factor_rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let selector_summaries =
        Array.map
          (fun s -> Lazy.force (Summary.selector_summary s))
          factor_summaries
      in
      let common =
        common_decls_from_props
          (common_props_of_array factor_summaries)
          factor_summaries.(0)
      in
      if common = [] then Option.None
      else
        let leftovers =
          leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let leftover_options = Array.to_list leftovers in
            let current_count = List.length factor_rules - 1 in
            let current_leftovers, target_leftovers =
              split_at current_count leftover_options
            in
            let after =
              (grouped :: filter_some current_leftovers)
              @ skipped
              @ filter_some target_leftovers
            in
            let before = factor_rules @ skipped in
            let before_size = rules_pp_size before in
            let after_size = rules_pp_size after in
            if after_size < before_size then begin
              record_factor_saving (before_size - after_size);
              Some after
            end
            else Option.None)

type gap_entry = Factor of summary | Skip of summary

let size_of_gap_entry = function Factor s | Skip s -> Summary.size s

(* Size of [first :: entry rules] from the cached per-summary sizes, so a scan
   that re-evaluates a growing prefix does not re-render each rule every
   step. *)
let gap_before_size first entries =
  List.fold_left
    (fun acc e -> acc + size_of_gap_entry e)
    (rule_pp_size first) entries

let rules_of_gap first entries =
  first
  :: List.filter_map
       (function Factor s -> Some (Summary.rule s) | Skip _ -> None)
       entries

let summaries_of_gap first_summary entries =
  first_summary
  :: List.filter_map (function Factor s -> Some s | Skip _ -> None) entries

let gap_rewrite first entries : (Stylesheet.rule list * int) option =
  let factor_rules = rules_of_gap first entries in
  match factor_rules with
  | [] | [ _ ] -> Option.None
  | _ -> (
      let rules_arr = Array.of_list factor_rules in
      let factor_summaries = Array.map summarize_factor_rule rules_arr in
      let selector_summaries =
        Array.map
          (fun s -> Lazy.force (Summary.selector_summary s))
          factor_summaries
      in
      let common =
        common_decls_from_props
          (common_props_of_array factor_summaries)
          factor_summaries.(0)
      in
      if common = [] then Option.None
      else
        let leftovers =
          leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let next_factor = ref 1 in
            let entry_after = function
              | Skip s -> Some (Summary.rule s)
              | Factor _ ->
                  let leftover = leftovers.(!next_factor) in
                  incr next_factor;
                  leftover
            in
            let after =
              grouped
              :: (filter_some [ leftovers.(0) ]
                 @ List.filter_map entry_after entries)
            in
            let before_size = gap_before_size first entries in
            let after_size = rules_pp_size after in
            if after_size < before_size then
              Some (after, before_size - after_size)
            else Option.None)

let common_equal_decls rules (first : Stylesheet.rule) =
  let summaries = List.map summarize_factor_rule rules in
  List.filter
    (fun decl ->
      List.for_all (fun s -> summary_contains_declaration s decl) summaries)
    first.Stylesheet_intf.declarations

(* [common] is the declaration list the scan has already proven present in each
   factored rule and safe across skipped rules. Reusing it avoids recomputing
   the same intersection for every improving equal-anchor prefix. *)
let gap_equal_rewrite ~common first_summary entries :
    (Stylesheet.rule list * int) option =
  let first = Summary.rule first_summary in
  let factor_summaries = summaries_of_gap first_summary entries in
  match factor_summaries with
  | [] | [ _ ] -> Option.None
  | _ -> (
      let factor_summaries = Array.of_list factor_summaries in
      let rules_arr = Array.map (fun s -> Summary.rule s) factor_summaries in
      let selector_summaries =
        Array.map
          (fun s -> Lazy.force (Summary.selector_summary s))
          factor_summaries
      in
      if common = [] then Option.None
      else
        let leftovers =
          leftover_options ~common ~selector_summaries ~factor_summaries
            rules_arr
          |> Array.of_list
        in
        match
          cost_aware_factor_group first rules_arr factor_summaries common
            leftovers
        with
        | None -> Option.None
        | Some (grouped, leftovers) ->
            let next_factor = ref 1 in
            let entry_after = function
              | Skip s -> Some (Summary.rule s)
              | Factor _ ->
                  let leftover = leftovers.(!next_factor) in
                  incr next_factor;
                  leftover
            in
            let after =
              grouped
              :: (filter_some [ leftovers.(0) ]
                 @ List.filter_map entry_after entries)
            in
            let before_size = gap_before_size first entries in
            let after_size = rules_pp_size after in
            if after_size < before_size then
              Some (after, before_size - after_size)
            else Option.None)

let anchor_common first candidate (common : Declaration.declaration list option)
    =
  match common with
  | None -> common_factorable_decls [ first; Summary.rule candidate ] first
  | Some common ->
      if declares_all candidate (List.map decl_property common) then common
      else []

let better_factor_gap
    (best :
      (Stylesheet.rule list * (Stylesheet.rule * summary) list * int) option)
    replacement tail savings =
  match best with
  | None -> Some (replacement, tail, savings)
  | Some (_, _, best_savings) when savings > best_savings ->
      Some (replacement, tail, savings)
  | Some _ -> best

let equal_factor_lookahead = 32

let single_anchor_blocked common candidate_summary entries_rev =
  common = []
  || List.exists
       (fun entry ->
         match entry with
         | Factor _ -> false
         | Skip skipped ->
             skipped_rule_blocks_factor common candidate_summary skipped)
       entries_rev

let update_single_anchor_best first entries_rev tail best candidate_summary =
  let entries = List.rev (Factor candidate_summary :: entries_rev) in
  match gap_rewrite first entries with
  | Some (replacement, savings) ->
      better_factor_gap best replacement tail savings
  | None -> best

let try_single_anchor_indexed prefix first first_summary rest_summaries =
  (* [rest_summaries] is threaded as the tail in [better_factor_gap] so callers
     with a precomputed summary index can continue without rebuilding it. *)
  let first_bloom = Summary.bloom first_summary in
  let rec scan entries_rev (common : Declaration.declaration list option)
      (best :
        (Stylesheet.rule list * (Stylesheet.rule * summary) list * int) option)
      fuel = function
    | [] -> best
    | _ when fuel <= 0 -> best
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then best
        else if not (rule_factor_eligible candidate) then
          scan
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else if not (Summary.may_share_bloom candidate_summary first_bloom) then
          (* Bloom prefilter: candidate's declarations share no hash with the
             anchor's, so [anchor_common] would yield []. Skip the check
             entirely. *)
          scan
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else
          let candidate_common = anchor_common first candidate_summary common in
          if
            single_anchor_blocked candidate_common candidate_summary entries_rev
          then
            scan
              (Skip candidate_summary :: entries_rev)
              common best (fuel - 1) tail
          else
            let best =
              update_single_anchor_best first entries_rev tail best
                candidate_summary
            in
            scan
              (Factor candidate_summary :: entries_rev)
              (Some candidate_common) best (fuel - 1) tail
  in
  match
    scan [] Option.None Option.None equal_factor_lookahead rest_summaries
  with
  | None -> Option.None
  | Some (replacement, tail, _) -> Some (prefix @ replacement, tail)

let equal_anchor_common first candidate
    (common : Declaration.declaration list option) =
  match common with
  | None -> common_equal_decls [ first; Summary.rule candidate ] first
  | Some common ->
      (* Cached Bloom filter on the candidate's summary lets us drop any [decl]
         from [common] whose hash isn't a possible member in O(1) -- a single
         bit-AND -- instead of walking the candidate's declarations;
         declarations whose hash hits the bloom still need the structural
         [same_minified_declaration] check to disambiguate filter collisions,
         which are correctness-preserving. *)
      List.filter (summary_contains_declaration candidate) common

(* Returns [Some (replacement, tail, savings)] - the [savings] is the strict
   output-size reduction [gap_equal_rewrite] already computed, surfaced so the
   best-first scheduler can prioritise without re-rendering. *)
let rec scan_equal_anchor ~first_bloom first_summary entries_rev common best
    fuel = function
  | [] -> best
  | _ when fuel <= 0 -> best
  | (candidate, candidate_summary) :: tail ->
      let first = Summary.rule first_summary in
      if boundary_stops_scan common candidate then best
      else if not (rule_factor_eligible candidate) then
        scan_equal_anchor ~first_bloom first_summary
          (Skip candidate_summary :: entries_rev)
          common best (fuel - 1) tail
      else if not (Summary.may_share_bloom candidate_summary first_bloom) then
        (* Bloom prefilter: no declaration of the candidate shares a
           [Declaration.hash] with any declaration of the anchor, so the common
           subset is necessarily empty. Skip the [equal_anchor_common] /
           [blocks] checks and treat the candidate as a skipped rule. *)
        scan_equal_anchor ~first_bloom first_summary
          (Skip candidate_summary :: entries_rev)
          common best (fuel - 1) tail
      else
        let candidate_common =
          equal_anchor_common first candidate_summary common
        in
        let blocks =
          candidate_common = []
          || List.exists
               (fun entry ->
                 match entry with
                 | Factor _ -> false
                 | Skip skipped ->
                     skipped_blocks_factor_tie candidate_common
                       candidate_summary skipped)
               entries_rev
        in
        if blocks then
          scan_equal_anchor ~first_bloom first_summary
            (Skip candidate_summary :: entries_rev)
            common best (fuel - 1) tail
        else
          let entries = List.rev (Factor candidate_summary :: entries_rev) in
          let best =
            match
              gap_equal_rewrite ~common:candidate_common first_summary entries
            with
            | Some (replacement, savings) ->
                better_factor_gap best replacement tail savings
            | None -> best
          in
          scan_equal_anchor ~first_bloom first_summary
            (Factor candidate_summary :: entries_rev)
            (Some candidate_common) best (fuel - 1) tail

let seed_bloom : Declaration.declaration list option -> Summary.bloom option =
  function
  | None -> Option.None
  | Some decls -> Some (Summary.bloom_of_decls decls)

let try_factor_equal_anchor ~shared_decl (first : Stylesheet.rule) first_summary
    rest_summaries =
  (* The auto-narrowing chain ([None]) commits to the first sharer's common and
     handles multi-property blocks, but it loses a single-property group when an
     earlier rule shares a different anchor declaration (the running common
     narrows away from the property the later cluster shares). Seeding the scan
     with each shared individual anchor declaration recovers those groups; the
     best factoring across all seeds wins. Non-shared declarations cannot be
     factored, and a seed-specific Bloom avoids scanning candidates that cannot
     contain the seeded declaration. *)
  let seeds : Declaration.declaration list option list =
    let shared_decls = List.filter shared_decl first.declarations in
    Option.None :: List.map (fun d -> Some [ d ]) shared_decls
  in
  let first_bloom = Summary.bloom first_summary in
  List.fold_left
    (fun best seed ->
      let first_bloom = Option.value (seed_bloom seed) ~default:first_bloom in
      match
        scan_equal_anchor ~first_bloom first_summary [] seed Option.None
          equal_factor_lookahead rest_summaries
      with
      | None -> best
      | Some (replacement, tail, savings) ->
          better_factor_gap best replacement tail savings)
    Option.None seeds

let suffix_prop_ids summaries =
  let rec loop acc = function
    | [] -> acc
    | summary :: rest ->
        let props =
          match acc with
          | [] -> Summary.prop_ids summary
          | next_props :: _ ->
              prop_ids_inter (Summary.prop_ids summary) next_props
        in
        loop (props :: acc) rest
  in
  loop [] (List.rev summaries)

let try_group_suffix_against_rest ~prefix_rev ~current ~common_props
    ~current_common rest :
    (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
  let rec scan skipped_rev fuel :
      (Stylesheet.rule * summary) list ->
      (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
    function
    | _ when fuel <= 0 -> None
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then None
        else if not (rule_factor_eligible candidate) then
          scan (candidate_summary :: skipped_rev) (fuel - 1) tail
        else if
          declares_ids candidate_summary common_props
          && not
               (List.exists
                  (skipped_rule_blocks_factor current_common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> Summary.rule s) skipped_rev in
          let factor_rules = List.map fst current @ [ candidate ] in
          match rules_with_skips factor_rules skipped with
          | Some replacement -> Some (List.rev prefix_rev @ replacement, tail)
          | None -> None
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
    | [] -> None
  in
  if current_common = [] then None else scan [] 128 rest

let try_group_indexed_lookahead current rest :
    (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
  let current = List.rev current in
  let current_summaries = List.map snd current in
  let current_suffix_props = suffix_prop_ids current_summaries in
  let rec try_suffix prefix_rev current summaries suffix_props :
      (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
    match current with
    | [] -> Option.None
    | [ (first, first_summary) ] ->
        try_single_anchor_indexed (List.rev prefix_rev) first first_summary rest
    | (first, _) :: tail_current -> (
        match (summaries, suffix_props) with
        | first_summary :: tail_summaries, common_props :: tail_suffix_props
          -> (
            let current_common =
              common_decls_from_ids common_props first_summary
            in
            match
              try_group_suffix_against_rest ~prefix_rev ~current ~common_props
                ~current_common rest
            with
            | Some _ as result -> result
            | None ->
                try_suffix (first :: prefix_rev) tail_current tail_summaries
                  tail_suffix_props)
        | _ -> Option.None)
  in
  try_suffix [] current current_summaries current_suffix_props

let try_extend_factored_rule anchor rest :
    (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
  let common = (Summary.rule anchor).declarations in
  let common_props = List.map decl_property common in
  let rec scan skipped_rev fuel :
      (Stylesheet.rule * summary) list ->
      (Stylesheet.rule list * (Stylesheet.rule * summary) list) option =
    function
    | [] -> Option.None
    | _ when fuel <= 0 -> Option.None
    | (candidate, candidate_summary) :: tail ->
        if rule_factor_boundary candidate then Option.None
        else if not (rule_factor_eligible candidate) then
          scan (candidate_summary :: skipped_rev) (fuel - 1) tail
        else if
          declares_all candidate_summary common_props
          && not
               (List.exists
                  (skipped_rule_blocks_factor common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> Summary.rule s) skipped_rev in
          let factor_rules =
            [ Summary.rule anchor; Summary.rule candidate_summary ]
          in
          match rules_with_skips factor_rules skipped with
          | None -> Option.None
          | Some replacement -> Some (replacement, tail)
        else if
          skipped_rule_blocks_factor common candidate_summary candidate_summary
        then Option.None
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
  in
  if common = [] then Option.None else scan [] 128 rest

let extend_factored_declarations (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let rec walk acc = function
    | [] -> List.rev acc
    | (r, summary) :: rest -> (
        if not (rule_factor_eligible r) then walk (r :: acc) rest
        else
          match try_extend_factored_rule summary rest with
          | None -> walk (r :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

let rule_identical_extend_eligible (r : Stylesheet.rule) =
  rule_factor_eligible r

let can_extend_identical_rule ~anchor_summary ~candidate_summary anchor
    candidate =
  let anchor : Stylesheet.rule = anchor in
  let candidate : Stylesheet.rule = candidate in
  (* Bloom prefilter: two rules with different declaration-hash sets cannot have
     structurally equal declaration lists. Avoids the full
     [declarations_css_equal] walk on the common rejected case. *)
  Summary.decl_count anchor_summary = Summary.decl_count candidate_summary
  && Summary.same_bloom anchor_summary candidate_summary
  && declarations_css_equal anchor.declarations candidate.declarations
  && extract_pseudo_element anchor.selector
     = extract_pseudo_element candidate.selector
  && newer_pseudo_class_compatible anchor.selector candidate.selector

let try_extend_identical_rule ~ctx anchor_summary rest =
  let anchor = Summary.rule anchor_summary in
  let common = (Summary.rule anchor_summary).declarations in
  let skipped_rule_blocks =
    match Ctx.scope ctx with
    | `Stylesheet -> skipped_blocks_factor_tie
    | `Fragment -> skipped_rule_blocks_factor
  in
  let rec scan skipped_rev fuel = function
    | [] -> (None : _ option)
    | _ when fuel <= 0 -> (None : _ option)
    | (candidate, candidate_summary) :: tail ->
        if not (rule_identical_extend_eligible candidate) then (None : _ option)
        else if
          can_extend_identical_rule ~anchor_summary ~candidate_summary anchor
            candidate
          && not
               (List.exists
                  (skipped_rule_blocks common candidate_summary)
                  skipped_rev)
        then
          let skipped = List.rev_map (fun s -> Summary.rule s) skipped_rev in
          let grouped =
            {
              anchor with
              selector =
                merge_selector_list
                  [ anchor.selector; (Summary.rule candidate_summary).selector ];
            }
          in
          let before =
            (anchor :: skipped) @ [ Summary.rule candidate_summary ]
          in
          let after = grouped :: skipped in
          let before_size = rules_pp_size before in
          let after_size = rules_pp_size after in
          if after_size < before_size then begin
            record_factor_saving (before_size - after_size);
            Some (after, tail)
          end
          else (None : _ option)
        else scan (candidate_summary :: skipped_rev) (fuel - 1) tail
  in
  if common = [] then
    (None : (Stylesheet.rule list * (Stylesheet.rule * summary) list) option)
  else scan [] 128 rest

let extend_identical_declaration_rules ~ctx (rules : Stylesheet.rule list) :
    Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let rec walk acc = function
    | [] -> List.rev acc
    | (r, summary) :: rest -> (
        if not (rule_identical_extend_eligible r) then walk (r :: acc) rest
        else
          match try_extend_identical_rule ~ctx summary rest with
          | None -> walk (r :: acc) rest
          | Some (replacement, tail) ->
              walk (List.rev_append replacement acc) tail)
  in
  preserve_list rules (walk [] items)

(* [r] shares a property with the whole current group iff some property it
   declares is declared by every group member - i.e. [r]'s property set meets
   the running intersection of the group's property sets. Tracking that
   intersection avoids the per-pair [decl_property] rescans of the group. *)
let common (rules : Stylesheet.rule list) : Stylesheet.rule list =
  let items = List.map (fun r -> (r, summarize_factor_rule r)) rules in
  let factorise_items items = factorise_group (List.rev_map fst items) in
  let rec group acc current current_props = function
    | [] -> List.rev_append (factorise_items current) acc
    | (r, summary) :: rest -> (
        if not (rule_factor_eligible r) then
          let acc = List.rev_append (factorise_items current) acc in
          group (r :: acc) [] [||] rest
        else
          let r_props = Summary.prop_ids summary in
          let shares =
            match current with
            | [] -> not (prop_ids_empty r_props)
            | _ -> not (prop_ids_disjoint r_props current_props)
          in
          if shares then
            let current_props =
              match current with
              | [] -> r_props
              | _ -> prop_ids_inter current_props r_props
            in
            group acc ((r, summary) :: current) current_props rest
          else
            match
              try_group_indexed_lookahead current ((r, summary) :: rest)
            with
            | Some (replacement, tail) ->
                group (List.rev_append replacement acc) [] [||] tail
            | None ->
                let acc = List.rev_append (factorise_items current) acc in
                group acc [ (r, summary) ] r_props rest)
  in
  preserve_list rules (List.rev (group [] [] [||] items))

(* One pass of gap-factoring: hoist a declaration shared across rules with
   non-conflicting selectors, even across intervening rules, when cascade-safe
   and smaller. Scheduling is best-first (the greedy weight order of SatCSS,
   Hague, Lin & Hong, TOPLAS 2019) over globally indexed physical intervals:
   each queued anchor stores the exact live-node interval it scored over. On
   pop, a still-live interval can be applied directly; stale intervals fall back
   to exact re-scoring. That keeps correctness tied to physical node identity
   while avoiding the old score-on-enqueue, score-again-on-pop hot path. *)
(* The next [k] live nodes after [node], in pool order. *)
let rec window node k acc =
  if k <= 0 then List.rev acc
  else
    match Pool.next node with
    | None -> List.rev acc
    | Some m -> window m (k - 1) (m :: acc)

let rec take n = function x :: xs when n > 0 -> x :: take (n - 1) xs | _ -> []

(* Pool-wide declaration index keyed on [Declaration.hash] (precomputed at
   construction, so the lookup is one int compare instead of walking the AST).
   For each declaration hash, record how many distinct rules in the pool carry a
   matching declaration -- an anchor whose every declaration appears in exactly
   one rule cannot be factored at all and we skip the full [window] / [scan]
   cost. False positives from hash collisions are harmless: a
   colliding-but-different declaration may let an unfactorable anchor through,
   and the scan will reject it at the usual miss cost. *)
let build_shared_decl_predicate pool =
  let counts : (int, int) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun n ->
      let r = Pool.rule n in
      let seen = Hashtbl.create 8 in
      List.iter
        (fun d -> Hashtbl.replace seen (Declaration.hash d) ())
        r.Stylesheet_intf.declarations;
      Hashtbl.iter
        (fun h () ->
          let c = try Hashtbl.find counts h with Not_found -> 0 in
          Hashtbl.replace counts h (c + 1))
        seen)
    (Pool.nodes pool);
  fun d ->
    match Hashtbl.find_opt counts (Declaration.hash d) with
    | Some c -> c > 1
    | None -> false

(* Score an anchor node: [None] when it cannot factor, else the replacement
   rules, the nodes the factoring consumes, and the bytes it saves. *)
let anchor_score ~shared_decl n =
  counters.anchors_scored <- counters.anchors_scored + 1;
  let r = Pool.rule n in
  if not (rule_factor_eligible r) then (None : _ option)
  else if
    (* Cheap pre-filter: factor_anchor needs at least one of this anchor's
       declarations to appear verbatim in some other live rule. *)
    not (List.exists shared_decl r.declarations)
  then (
    counters.anchors_prefiltered <- counters.anchors_prefiltered + 1;
    (None : _ option))
  else
    let win_nodes = window n equal_factor_lookahead [] in
    let win_summaries =
      List.map
        (fun n ->
          let rule = Pool.rule n in
          (rule, summarize_factor_rule rule))
        win_nodes
    in
    let summary = summarize_factor_rule r in
    match try_factor_equal_anchor ~shared_decl r summary win_summaries with
    | None -> (None : _ option)
    | Some (replacement, tail, savings) ->
        let consumed =
          take (List.length win_summaries - List.length tail) win_nodes
        in
        Some (Loop.action ~replacement ~consumed ~saving:savings)

let anchor (rules : Stylesheet.rule list) : Stylesheet.rule list =
  let pool = Pool.of_rules rules in
  let shared_decl = build_shared_decl_predicate pool in
  let applied =
    Loop.run
      ~on_apply:(fun saving ->
        counters.factorings_applied <- counters.factorings_applied + 1;
        record_factor_saving saving)
      (Loop.v pool (anchor_score ~shared_decl))
  in
  if applied > 0 then
    Log.debug (fun m -> m "factor_anchor_gaps: applied %d factorings" applied);
  preserve_list rules (Pool.to_rules pool)

let extract_group_branch_into_adjacent rules =
  let plain (r : Stylesheet.rule) =
    r.nested = [] && r.merge_key = None
    && not (contains_vendor_pseudo_element r.selector)
  in
  let beneficial branch_sel decls =
    let sel_size = Pp.size ~minify:true Selector.pp branch_sel in
    sel_size + 1 > decls_pp_size decls + List.length decls
  in
  (* Split from group [b] the single branch equal to [neighbour_sel]. *)
  let extract neighbour_sel (b : Stylesheet.rule) =
    match selectors_of_rule_selector b.selector with
    | _ :: _ :: _ as branches -> (
        let key = canonical_selector_key in
        let matched, others =
          List.partition (fun s -> key s = key neighbour_sel) branches
        in
        match matched with
        | [ si ] when others <> [] && beneficial si b.declarations ->
            Some
              ( { b with selector = si },
                { b with selector = merge_selector_list others } )
        | _ -> None)
    | _ -> None
  in
  let changed = ref false in
  let rec go acc = function
    | a :: b :: rest when plain a && plain b -> (
        match extract a.Stylesheet_intf.selector b with
        | Some (branch, remainder) ->
            changed := true;
            go (branch :: a :: acc) (remainder :: rest)
        | None -> (
            match extract b.Stylesheet_intf.selector a with
            | Some (branch, remainder) ->
                changed := true;
                go (branch :: remainder :: acc) (b :: rest)
            | None -> go (a :: acc) (b :: rest)))
    | x :: rest -> go (x :: acc) rest
    | [] -> List.rev acc
  in
  let result = go [] rules in
  if !changed then result else rules

let pass_stable_threshold = 2
let min_adaptive_iterations = 2
let stalled_iteration_threshold = 2
let min_marginal_saving = 128
let min_marginal_ratio_ppm = 1_500

let units rules =
  List.fold_left
    (fun acc (rule : Stylesheet.rule) ->
      acc + 8 + (16 * List.length rule.Stylesheet_intf.declarations))
    0 rules

let run_factor_pass quiet any_active_pass_changed active_passes changed_passes
    fuel name f r =
  let q = try Hashtbl.find quiet name with Not_found -> 0 in
  if q >= pass_stable_threshold then r
  else
    let s = pass_stat name in
    let t0 = Unix.gettimeofday () in
    incr active_passes;
    let r' = f r in
    let t1 = Unix.gettimeofday () in
    s.time <- s.time +. (t1 -. t0);
    s.calls <- s.calls + 1;
    s.rules_in <- s.rules_in + List.length r;
    s.rules_out <- s.rules_out + List.length r';
    if r' == r then Hashtbl.replace quiet name (q + 1)
    else begin
      s.changes <- s.changes + 1;
      incr changed_passes;
      Hashtbl.replace quiet name 0;
      any_active_pass_changed := true;
      Log.debug (fun m -> m "factor iter %d: %s changed" fuel name)
    end;
    r'

let passes ~ctx ~finalize pass rules =
  rules
  (* Gap merging/factoring is pure cascade-safety reasoning (specificity is
     world-independent), so it runs in every scope - neither pass takes a [ctx].
     It is part of the pipeline (not a one-shot before the loop) so a merge it
     can only make after another pass reorders rules is reached within a single
     fixpoint, not on a second [optimize] call. *)
  |> pass "extract_branch" extract_group_branch_into_adjacent
  |> pass "merge_same_selector" merge_same_selector_gaps
  |> pass "combine_identical" combine_identical_rules
  |> pass "combine_identical_global"
       (combine_identical_rules_global ~extend_lists:(Ctx.extend_lists ctx))
  |> pass "extend_identical" (extend_identical_declaration_rules ~ctx)
  |> pass "factor_common" common
  |> pass "factor_anchor" anchor
  |> pass "extend_factored" extend_factored_declarations
  |> pass "merge_rules" merge_rules
  |> pass "finalize" (list_map_preserve finalize)

let low_marginal_gain before_units bytes_saved =
  bytes_saved < min_marginal_saving
  || bytes_saved * 1_000_000 < before_units * min_marginal_ratio_ppm

let record_factor_iteration ~fixpoint ~local_iteration ~before_rules
    ~before_bytes ~rules' ~after_bytes ~bytes_saved ~active_passes
    ~changed_passes ~elapsed =
  Stats.record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules:(List.length rules') ~after_bytes ~bytes_saved
    ~active_passes:!active_passes ~changed_passes:!changed_passes ~elapsed

let update_factor_stall ~local_iteration ~stalled ~before_units ~bytes_saved =
  if local_iteration < min_adaptive_iterations then 0
  else if low_marginal_gain before_units bytes_saved then stalled + 1
  else 0

let stop stalled bytes_saved =
  counters.marginal_stops <- counters.marginal_stops + 1;
  Log.debug (fun m ->
      m
        "factor fixpoint: stopped after %d low-gain iterations (last saved %d \
         bytes)"
        stalled bytes_saved)

type iteration_result = {
  result_rules : Stylesheet.rule list;
  changed : bool;
  before_units : int;
  bytes_saved : int;
}

let run_factor_iteration ~ctx ~finalize ~quiet ~fixpoint ~local_iteration ~fuel
    rules =
  counters.iterations <- counters.iterations + 1;
  let any_active_pass_changed = ref false in
  let active_passes = ref 0 in
  let changed_passes = ref 0 in
  let before_rules = List.length rules in
  let before_units = units rules in
  let before_bytes = if Stats.profile () then rules_pp_size rules else 0 in
  Stats.reset_saving ();
  let started_at = Unix.gettimeofday () in
  let pass name f r =
    run_factor_pass quiet any_active_pass_changed active_passes changed_passes
      fuel name f r
  in
  let rules' = passes ~ctx ~finalize pass rules in
  let after_bytes = if Stats.profile () then rules_pp_size rules' else 0 in
  let elapsed = Unix.gettimeofday () -. started_at in
  let bytes_saved = Stats.saving () in
  record_factor_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~rules' ~after_bytes ~bytes_saved ~active_passes ~changed_passes ~elapsed;
  {
    result_rules = rules';
    changed = !any_active_pass_changed;
    before_units;
    bytes_saved;
  }

let to_fixpoint ?(adaptive = true) ~ctx ~finalize fuel rules =
  (* Per-pass quiet-streak counter. A pass that has returned its input unchanged
     [pass_stable_threshold] times in a row gets skipped on subsequent
     iterations. The fixpoint usually needs 8 iterations because
     [extend_identical] keeps changing the list in ways that don't enable new
     factorings for the heavy passes; this counter lets the heavy
     [factor_anchor] / [factor_common] sit out once they've confirmed
     convergence twice running, while the cheap passes keep iterating. *)
  let quiet : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let initial_fuel = fuel in
  counters.factor_fixpoints_run <- counters.factor_fixpoints_run + 1;
  let fixpoint = counters.factor_fixpoints_run in
  let rec go stalled fuel rules =
    if fuel <= 0 then (
      Log.debug (fun m ->
          m "factor fixpoint: fuel exhausted, not yet converged");
      rules)
    else begin
      let local_iteration = initial_fuel - fuel + 1 in
      let result =
        run_factor_iteration ~ctx ~finalize ~quiet ~fixpoint ~local_iteration
          ~fuel rules
      in
      if not result.changed then (
        Log.debug (fun m ->
            m "factor fixpoint: converged after %d iterations" (16 - fuel));
        result.result_rules)
      else
        let stalled =
          if adaptive then
            update_factor_stall ~local_iteration ~stalled
              ~before_units:result.before_units ~bytes_saved:result.bytes_saved
          else 0
        in
        if stalled >= stalled_iteration_threshold then begin
          stop stalled result.bytes_saved;
          result.result_rules
        end
        else go stalled (fuel - 1) result.result_rules
    end
  in
  go 0 fuel rules

let run ~ctx ~finalize (rules : Stylesheet.rule list) =
  let summary = Preflight.summarize rules in
  if Preflight.declaration_count summary > Preflight.small_declaration_threshold
  then
    counters.factor_preflight_gain <-
      counters.factor_preflight_gain + Preflight.estimated_gain summary;
  (* [--aggressive] forces the fixpoint regardless of the preflight estimate. *)
  if Preflight.useful summary || Ctx.aggressive ctx then
    let adaptive =
      Preflight.declaration_count summary
      > Preflight.small_declaration_threshold
    in
    to_fixpoint ~adaptive ~ctx ~finalize 16 rules
  else begin
    counters.factor_fixpoints_skipped <- counters.factor_fixpoints_skipped + 1;
    rules
  end
