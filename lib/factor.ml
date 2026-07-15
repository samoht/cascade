(** Cascade-aware rule factoring. *)

open Stylesheet

let counters = Stats.counters
let rules_pp_size = Size.rules
let list_map_preserve = Common.List.map_preserve

type cache = (string * rule list, rule list) Hashtbl.t

let cache () = Hashtbl.create 16

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
    counters.factor_preflight_gain <-
      counters.factor_preflight_gain + Preflight.estimated_gain summary;
  Preflight.useful summary || Ctx.aggressive ctx

let record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~changed ~elapsed =
  Stats.record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~active_passes:1
    ~changed_passes:(if changed then 1 else 0)
    ~elapsed

let optimize_graph ~ctx ~finalize ~fixpoint ~local_iteration rules graph =
  counters.iterations <- counters.iterations + 1;
  Stats.reset_saving ();
  let before_rules = List.length rules in
  let before_bytes = if Stats.profile () then rules_pp_size rules else 0 in
  let started_at = Unix.gettimeofday () in
  let graph = Rule_scheduler.run ~ctx ~finalize graph in
  let ordered = ordered_rules rules graph in
  let rules' = list_map_preserve finalize ordered in
  let after_bytes = if Stats.profile () then rules_pp_size rules' else 0 in
  let elapsed = Unix.gettimeofday () -. started_at in
  let bytes_saved = Stats.saving () in
  (* Both return the input unchanged by physical identity on a no-op, so a
     pointer compare detects change without rendering to CSS. *)
  let changed = rules' != rules in
  record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules:(List.length rules') ~after_bytes ~bytes_saved ~changed
    ~elapsed;
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

let run_segment ?cache ~ctx ~finalize (rules : rule list) =
  let key = Option.map (fun _ -> cache_key ~ctx rules) cache in
  match
    match (cache, key) with
    | Some cache, Some key -> Hashtbl.find_opt cache key
    | _ -> None
  with
  | Some rules -> rules
  | None ->
      let summary = Preflight.summarize rules in
      let graph =
        Rule_graph.of_rules ~closed_world:(Ctx.closed_world ctx) rules
      in
      let result =
        if not (should_run_preflight ~ctx summary) then begin
          counters.factor_fixpoints_skipped <-
            counters.factor_fixpoints_skipped + 1;
          ordered_rules rules graph
        end
        else begin
          counters.factor_fixpoints_run <- counters.factor_fixpoints_run + 1;
          let fixpoint = counters.factor_fixpoints_run in
          let unfactored = ordered_rules rules graph in
          let factored =
            optimize_graph ~ctx ~finalize ~fixpoint ~local_iteration:1 rules
              graph
          in
          if
            factored != rules
            && factored_grows_transfer ~ctx ~unfactored ~factored
          then begin
            counters.factor_transfer_reverts <-
              counters.factor_transfer_reverts + 1;
            unfactored
          end
          else factored
        end
      in
      (match (cache, key) with
      | Some cache, Some key -> Hashtbl.replace cache key result
      | _ -> ());
      result

(* Custom-property rules are not cascade barriers: the DAG keys each custom
   property by name (a [var()] consumer writes its own property, not the one it
   reads), so disjoint writes reorder freely and the whole list is one
   segment. *)
let run ?cache ~ctx ~finalize (rules : rule list) =
  run_segment ?cache ~ctx ~finalize rules
