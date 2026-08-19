(** Cascade-aware rule factoring. *)

open Stylesheet

let src = Logs.Src.create "cascade.factor" ~doc:"Cascade rule factoring"

module Log = (val Logs.src_log src : Logs.LOG)

let rules_pp_size = Size.rules
let list_map_preserve = Common.List.map_preserve

(* The memo key carries a whole run of rules. The default structural hash reads
   only the first handful of nodes in it, so runs sharing a prefix land in one
   bucket and each probe then compares rule lists in full; folding the cached
   per-declaration hashes separates the buckets in one cheap integer pass, and
   the structural check is left to confirm the one entry that matches. *)
let mix acc x = (acc * 31) + x

let rule_hash (r : rule) =
  List.fold_left
    (fun acc d -> mix acc (Declaration.hash d))
    (Selector.hash r.Stylesheet_intf.selector)
    r.Stylesheet_intf.declarations

module Cache_tbl = Hashtbl.Make (struct
  type t = string * rule list

  let equal = ( = )

  let hash (knobs, rules) =
    List.fold_left
      (fun acc r -> mix acc (rule_hash r))
      (Hashtbl.hash knobs) rules
end)

type cache = {
  memo : rule list Cache_tbl.t;
  mutable reverted : (int * int) list;
      (* [(declaration_count, source_units)] of segments whose factoring the
         transfer gate threw away this run. The pipeline re-presents one segment
         several times with a rule or two moved, so the exact-match memo misses
         while the gate's verdict repeats; a segment that matches a reverted one
         this closely reverts too, and factoring it only to discard the result
         is the single largest block of wasted work on a large sheet. *)
}

let cache () = { memo = Cache_tbl.create 16; reverted = [] }

(* Within a fortieth on both axes: far tighter than the drift the pipeline
   introduces between iterations (a handful of rules in ~2450), and far looser
   than exact match, which never fires. *)
let segment_worth_remembering summary =
  Preflight.declaration_count summary > Preflight.small_declaration_threshold

let near_reverted cache summary =
  segment_worth_remembering summary
  &&
  let close a b = abs (a - b) * 40 <= max a b in
  let decls = Preflight.declaration_count summary
  and units = Preflight.source_units summary in
  List.exists (fun (d, u) -> close d decls && close u units) cache.reverted

(* Cache key over the value-typed knobs that change factoring, built explicitly
   so a {!Ctx.pp} debug-printer change cannot break cache correctness;
   [registered] is a constant closure, omitted. *)
let ctx_key ctx =
  let b x = if x then "1" else "0" in
  String.concat ""
    [
      (match Ctx.scope ctx with `Fragment -> "f" | `Stylesheet -> "s");
      b (Ctx.lossless ctx);
      b (Ctx.aggressive ctx);
      b (Ctx.extend_lists ctx);
      b (Ctx.closed_world ctx);
      (match Ctx.objective ctx with `Raw -> "r" | `Transfer -> "t");
    ]

(* The rule list keys the cache directly: poly hash/equal is sound over the
   immutable AST, so a lookup need not render every rule to CSS. *)
let cache_key ~ctx rules = (ctx_key ctx, rules)

let order_is_original rules graph order =
  Rule_graph.generation graph = 0
  && Array.length order = List.length rules
  &&
  let ok = ref true in
  for i = 0 to Array.length order - 1 do
    if Rule_graph.Node_id.to_int order.(i) <> i then ok := false
  done;
  !ok

let ordered_rules rules graph =
  let order = Rule_graph.canonical_order graph in
  if order_is_original rules graph order then rules
  else Array.to_list (Array.map (Rule_graph.node_rule graph) order)

let should_run_preflight ~ctx summary =
  if Preflight.declaration_count summary > Preflight.small_declaration_threshold
  then
    Stats.add_preflight_gain (Ctx.stats ctx) (Preflight.estimated_gain summary);
  Preflight.useful summary || Ctx.aggressive ctx

let record_iteration stats ~fixpoint ~local_iteration ~before_rules
    ~before_bytes ~after_rules ~after_bytes ~bytes_saved ~changed ~elapsed =
  Stats.record_iteration stats ~fixpoint ~local_iteration ~before_rules
    ~before_bytes ~after_rules ~after_bytes ~bytes_saved ~active_passes:1
    ~changed_passes:(if changed then 1 else 0)
    ~elapsed

let optimize_graph ~ctx ~finalize ~fixpoint ~local_iteration rules graph =
  let stats = Ctx.stats ctx in
  Stats.reset_saving stats;
  let before_rules = List.length rules in
  let profile = Stats.profile stats in
  let before_bytes = if profile then rules_pp_size rules else 0 in
  let started_at = Unix.gettimeofday () in
  let graph = Rule_scheduler.run ~ctx ~finalize graph in
  let ordered = ordered_rules rules graph in
  let rules' = list_map_preserve finalize ordered in
  let after_bytes = if profile then rules_pp_size rules' else 0 in
  let elapsed = Unix.gettimeofday () -. started_at in
  let bytes_saved = Stats.saving stats in
  (* Both return the input unchanged by physical identity on a no-op, so a
     pointer compare detects change without rendering to CSS. *)
  let changed = rules' != rules in
  let after_rules = List.length rules' in
  record_iteration stats ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~changed ~elapsed;
  (* Per fixpoint iteration, not per rule, so the closure cost is noise. The
     byte columns are only computed under [--profile]; rules and savings are
     always available. *)
  Log.debug (fun m ->
      m "fixpoint %d.%d: %d -> %d rules, %d bytes saved, %.3fs%s" fixpoint
        local_iteration before_rules after_rules bytes_saved elapsed
        (if changed then "" else " (no change)"));
  if changed then rules' else rules

(* Stylesheets ship DEFLATE-compressed, so a raw-byte factoring win can grow the
   compressed output (LZ77-cheap repeated text traded for unique selector
   structure). Under [`Transfer] keep a segment's factoring only if estimated
   transfer size does not grow; below this floor the estimate is DEFLATE
   block-overhead noise, so raw wins stand. *)
let transfer_gate_min_bytes = 4096

(* The greedy-LZ77 estimate prices a group by the whole segment's
   compressibility, so unrelated groupings jitter it a byte or two. Revert only
   past this margin, so noise cannot flip a distant raw-smaller factoring; real
   regressions (youtube-class, ~2% of the segment) stay well clear. *)
let transfer_gate_margin before = max 16 (before / 100)

let render_rules rules =
  Pp.to_string ~minify:true (fun ctx -> List.iter (pp_rule ctx)) rules

let factored_grows_transfer ~ctx ~unfactored ~factored =
  Ctx.objective ctx = `Transfer
  &&
  let before = render_rules unfactored in
  String.length before >= transfer_gate_min_bytes
  &&
  let before_gz = Gzip_size.estimate before in
  Gzip_size.estimate (render_rules factored)
  > before_gz + transfer_gate_margin before_gz

(* The two decisions worth watching from outside: which segments never get
   factored, and which get factored and then thrown away. The second is the
   event the preflight cannot yet predict - it scored the segment worth
   factoring on raw bytes, and the gate then found the compressed size grew. *)
let log_skip ~known_revert summary =
  Log.debug (fun m ->
      m "segment of %d declarations skipped: %s"
        (Preflight.declaration_count summary)
        (if known_revert then "matches a segment the gate reverted"
         else "preflight gain too small"))

let log_transfer_revert ~fixpoint summary =
  Log.debug (fun m ->
      m
        "fixpoint %d reverted: factoring grew the estimated transfer size (%d \
         declarations)"
        fixpoint
        (Preflight.declaration_count summary))

let run_segment ?cache ~ctx ~finalize (rules : rule list) =
  let key = Option.map (fun _ -> cache_key ~ctx rules) cache in
  match
    match (cache, key) with
    | Some cache, Some key -> Cache_tbl.find_opt cache.memo key
    | _ -> None
  with
  | Some rules -> rules
  | None ->
      let stats = Ctx.stats ctx in
      let summary = Preflight.summarize rules in
      let graph =
        Rule_graph.of_rules ~closed_world:(Ctx.closed_world ctx) rules
      in
      let known_revert =
        match cache with
        | Some cache -> near_reverted cache summary
        | None -> false
      in
      let result =
        if known_revert || not (should_run_preflight ~ctx summary) then begin
          Stats.skip_fixpoint stats;
          log_skip ~known_revert summary;
          ordered_rules rules graph
        end
        else begin
          let fixpoint = Stats.start_fixpoint stats in
          let unfactored = ordered_rules rules graph in
          let factored =
            optimize_graph ~ctx ~finalize ~fixpoint ~local_iteration:1 rules
              graph
          in
          if
            factored != rules
            && factored_grows_transfer ~ctx ~unfactored ~factored
          then begin
            Stats.revert_fixpoint stats;
            log_transfer_revert ~fixpoint summary;
            (* Only a large segment is worth remembering: factoring a small one
               costs little, so suppressing it saves nothing and risks giving up
               a grouping the gate would have kept. *)
            (match cache with
            | Some cache when segment_worth_remembering summary ->
                cache.reverted <-
                  ( Preflight.declaration_count summary,
                    Preflight.source_units summary )
                  :: cache.reverted
            | Some _ | None -> ());
            unfactored
          end
          else factored
        end
      in
      (match (cache, key) with
      | Some cache, Some key -> Cache_tbl.replace cache.memo key result
      | _ -> ());
      result

(* Custom-property rules are not cascade barriers: the DAG keys each custom
   property by name (a [var()] consumer writes its own property, not the one it
   reads), so disjoint writes reorder freely and the whole list is one
   segment. *)
let run ?cache ~ctx ~finalize (rules : rule list) =
  run_segment ?cache ~ctx ~finalize rules
