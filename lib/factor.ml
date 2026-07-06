(** Cascade-aware rule factoring. *)

open Stylesheet

let counters = Stats.counters
let rules_pp_size = Size.rules
let list_map_preserve = Common.List.map_preserve

type cache = (string * rule list, rule list) Hashtbl.t

let cache () = Hashtbl.create 16

(* Cache identity for a context: every value-typed knob that changes factoring,
   built explicitly rather than from {!Ctx.pp} so a debug-printer change cannot
   silently break cache correctness. [registered] is a closure that cannot be
   keyed; it is constant for a cache's lifetime (one context per top-level
   optimize), so it does not need to appear here. *)
let ctx_key ctx =
  let b x = if x then "1" else "0" in
  String.concat ""
    [
      (match Ctx.scope ctx with `Fragment -> "f" | `Stylesheet -> "s");
      b (Ctx.lossless ctx);
      b (Ctx.aggressive ctx);
      b (Ctx.extend_lists ctx);
      b (Ctx.closed_world ctx);
    ]

(* Structural key: the rule list keys the cache directly (sound poly hash/equal
   over the immutable AST), so a lookup hashes a sample instead of rendering
   every rule to CSS. *)
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
  (* [ordered_rules] and [list_map_preserve] return the input list unchanged by
     physical identity on a no-op, so a pointer compare detects whether
     factoring changed anything - no need to render both sides to CSS and
     compare. *)
  let changed = rules' != rules in
  record_iteration ~fixpoint ~local_iteration ~before_rules ~before_bytes
    ~after_rules:(List.length rules') ~after_bytes ~bytes_saved ~changed
    ~elapsed;
  if changed then rules' else rules

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
          optimize_graph ~ctx ~finalize ~fixpoint ~local_iteration:1 rules graph
        end
      in
      (match (cache, key) with
      | Some cache, Some key -> Hashtbl.replace cache key result
      | _ -> ());
      result

(* Custom-property rules are no longer cascade barriers: the DAG's conflict
   model keys each custom property by name (a [var()] consumer writes its own
   property, not the one it reads), so same-custom-property writes stay ordered
   while disjoint ones merge and reorder freely. The whole rule list is one DAG
   segment. *)
let run ?cache ~ctx ~finalize (rules : rule list) =
  run_segment ?cache ~ctx ~finalize rules
