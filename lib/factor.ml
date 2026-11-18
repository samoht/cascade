(** Cascade-aware rule factoring. *)

open Stylesheet

let src = Logs.Src.create "cascade.factor" ~doc:"Cascade rule factoring"

module Log = (val Logs.src_log src : Logs.LOG)

let rules_pp_size = Size.rules

(* The memo key carries a whole run of rules. The default structural hash reads
   only the first handful of nodes in it, so runs sharing a prefix land in one
   bucket and each probe then compares rule lists in full; folding the cached
   per-rule hashes separates the buckets in one cheap integer pass, and the
   structural check is left to confirm the one entry that matches. *)
module Cache_tbl = Hashtbl.Make (struct
  type t = string * rule list

  let equal = ( = )

  let hash (knobs, rules) =
    List.fold_left
      (fun acc r -> (acc * 31) + Stylesheet.rule_hash r)
      (Hashtbl.hash knobs) rules
end)

type cache = { memo : rule list Cache_tbl.t }

let cache () = { memo = Cache_tbl.create 16 }

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

let optimize_graph ~ctx ~finalize ~fixpoint rules graph =
  let stats = Ctx.stats ctx in
  Stats.reset_saving stats;
  let before_rules = List.length rules in
  let profile = Stats.profile stats in
  let before_bytes = if profile then rules_pp_size rules else 0 in
  let timer = if profile then Some (Mtime_clock.counter ()) else None in
  let { Rule_scheduler.graph; _ } = Rule_scheduler.run ~ctx ~finalize graph in
  (* A produced rule left the scheduler finalized, and an input rule arrives
     finalized, so the order is the result. *)
  let rules' = ordered_rules rules graph in
  let after_bytes = if profile then rules_pp_size rules' else 0 in
  let elapsed =
    match timer with
    | Some timer -> Mtime.Span.to_float_ns (Mtime_clock.count timer) /. 1e9
    | None -> 0.
  in
  let bytes_saved = Stats.saving stats in
  (* Both return the input unchanged by physical identity on a no-op, so a
     pointer compare detects change without rendering to CSS. *)
  let changed = rules' != rules in
  let after_rules = List.length rules' in
  Stats.record_iteration stats ~fixpoint ~before_rules ~before_bytes
    ~after_rules ~after_bytes ~bytes_saved ~changed ~elapsed;
  (* Per fixpoint, not per rule, so the closure cost is noise. The byte columns
     and the time are only measured under [--profile]; rules and savings are
     always available. *)
  Log.debug (fun m ->
      m "fixpoint %d: %d -> %d rules, %d bytes saved%s" fixpoint before_rules
        after_rules bytes_saved
        (if changed then "" else " (no change)"));
  if changed then rules' else rules

let log_skip summary =
  Log.debug (fun m ->
      m "segment of %d declarations skipped: preflight gain too small"
        (Preflight.declaration_count summary))

let run_segment ?cache ?(settle = Fun.id) ~ctx ~finalize (rules : rule list) =
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
      let result =
        if not (should_run_preflight ~ctx summary) then begin
          Stats.skip_fixpoint stats;
          log_skip summary;
          (* A graph fresh from [rules] orders them as given, so this needs
             none. *)
          settle rules
        end
        else begin
          let fixpoint = Stats.start_fixpoint stats in
          let graph =
            Rule_graph.of_rules ~closed_world:(Ctx.closed_world ctx) rules
          in
          settle (optimize_graph ~ctx ~finalize ~fixpoint rules graph)
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
let run ?cache ?settle ~ctx ~finalize (rules : rule list) =
  run_segment ?cache ?settle ~ctx ~finalize rules
