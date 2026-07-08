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
      (match Ctx.objective ctx with `Raw -> "r" | `Transfer -> "t");
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

(* Factoring rewrites are chosen by raw-byte gain, but stylesheets ship
   DEFLATE-compressed: replacing repeated declaration text (nearly free under
   LZ77) with unique selector-list structure can grow the compressed output even
   as raw bytes shrink. Under the [`Transfer] objective, keep a segment's
   factoring only when the estimated transfer size does not grow. Below
   [transfer_gate_min_bytes] the estimate is noise against DEFLATE's block
   overhead, so raw-byte wins stand unchallenged. *)
let transfer_gate_min_bytes = 4096

(* The estimate is a greedy-LZ77 approximation, and it costs a factored group by
   the whole segment's compressibility, so an unrelated grouping elsewhere in
   the segment shifts it by a byte or two. Revert only when factoring grows the
   estimate beyond that noise floor, so a sub-percent difference does not flip a
   distant, otherwise raw-smaller factoring; real regressions (the youtube-class
   case, ~2% of the segment) stay well clear of it. Reverting on the noise also
   hurt real gzip - the estimate's error swamps the byte it was chasing. *)
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

(* Custom-property rules are no longer cascade barriers: the DAG's conflict
   model keys each custom property by name (a [var()] consumer writes its own
   property, not the one it reads), so same-custom-property writes stay ordered
   while disjoint ones merge and reorder freely. The whole rule list is one DAG
   segment. *)
let run ?cache ~ctx ~finalize (rules : rule list) =
  run_segment ?cache ~ctx ~finalize rules
