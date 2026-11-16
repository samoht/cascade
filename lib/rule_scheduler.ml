(** Incremental greedy scheduler for graph rewrites. *)

let max_iterations graph = max 256 (16 * Rule_graph.node_count graph)

module Candidate_key = struct
  type t = int

  let compare = Int.compare
end

module Priority = struct
  type t = {
    saving : int;
    kind : int;
    consume : int array;
    produce : string list Lazy.t;
    ordinal : int;
    candidate : Rule_rewrite.candidate;  (** carried, never compared *)
  }

  let rec compare_int_arrays a b i =
    if a == b then 0
    else if i = Array.length a then
      Int.compare (Array.length a) (Array.length b)
    else if i = Array.length b then 1
    else
      match Int.compare a.(i) b.(i) with
      | 0 -> compare_int_arrays a b (i + 1)
      | order -> order

  let compare_next order next = if order = 0 then next () else order

  (* Shortest first, then by content. *)
  let shortlex a b =
    compare_next
      (Int.compare (String.length a) (String.length b))
      (fun () -> String.compare a b)

  let rec compare_texts a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: a, y :: b ->
        compare_next (shortlex x y) (fun () -> compare_texts a b)

  (* Consolidation rewrites (identical-body and same-selector merges) run before
     the declaration-factoring rewrites, so a selector's own split shorthand is
     merged and composed before a partial group could absorb its base. This
     keeps factoring order-independent: cascade's already-consolidated output
     and an equivalent split-shorthand input reach the same result. Within each
     phase the largest byte saving still wins, so factoring quality is
     unchanged. [kind] is the rank from [kind_rank]: 0 = identical-body, 1 =
     same-selector (consolidation); 2+ = the factoring rewrites. *)
  let phase kind = if kind <= 1 then 0 else 1

  let compare a b =
    if a == b then 0
    else
      compare_next
        (Int.compare (phase a.kind) (phase b.kind))
        (fun () ->
          compare_next (Int.compare a.saving b.saving) (fun () ->
              compare_next (Int.compare a.kind b.kind) (fun () ->
                  compare_next
                    (compare_texts (Lazy.force a.produce) (Lazy.force b.produce))
                    (fun () ->
                      compare_next (compare_int_arrays a.consume b.consume 0)
                        (fun () -> Int.compare a.ordinal b.ordinal)))))
end

module Queue = Psq.Make (Candidate_key) (Priority)

let kind_rank = function
  | Rule_rewrite.Identical_body -> 0
  | Same_selector -> 1
  | Exact_shared_declarations -> 2
  | Selector_branch_inline -> 3
  | Default_factoring -> 4

(* Ties between equal savings break on content and source order, never on a hash
   or a table's iteration order, which a change to how the syntax tree is built
   would move: first the produced rules, one by one, shortest first and then by
   their text, so the candidate that moves the least into a shared rule goes
   first; then the source positions of the consumed rules, earliest first. Two
   candidates tied on both are the same rewrite, and [ordinal] only keeps the
   order total. *)
let consume_key graph (candidate : Rule_rewrite.candidate) =
  candidate.consume
  |> List.map (Rule_graph.node_origin graph)
  |> List.sort Int.compare |> Array.of_list

let produce_text (candidate : Rule_rewrite.candidate) =
  lazy
    (List.map (Pp.to_string ~minify:true Stylesheet.pp_rule) candidate.produce)

let priority graph ordinal (candidate : Rule_rewrite.candidate) =
  Priority.
    {
      saving = -candidate.saving;
      kind = kind_rank candidate.kind;
      consume = consume_key graph candidate;
      produce = produce_text candidate;
      ordinal;
      candidate;
    }

let offer graph queue offer =
  let key = Rule_candidate.key offer in
  Queue.add key (priority graph key (Rule_candidate.candidate offer)) queue

(* Below 128 nodes the candidates a commit brings about are queued as soon as it
   lands; above, they wait for the queue to drain, when every candidate of the
   graph is queued again. A candidate stays queued until it is tried:
   [Rule_graph.rewrite] refuses one whose rules a later commit consumed.

   A drain with no commit since the last one leaves every candidate of the graph
   tried on it and refused: the run is settled. A rewrite only adds orders, so a
   refusal for sources ordered both ways stands on every later graph and the
   candidate is not tried again, until a commit consumes the outside rule they
   are ordered around. A candidate refused for a conflict no order decides yet,
   or for a cycle, is tried again: a later order can decide the conflict, and a
   cycle can run through an outside rule a later commit consumes. One stopped by
   [max_iterations] is not settled. *)
type outcome = { graph : Rule_graph.t; settled : bool }

(* How long a refusal stands: not past this graph, until the given outside rule
   is consumed, or for good. *)
type refusal = Retry | Until_consumed of Rule_graph.node_id | Final

let refusal : Rule_graph.rewrite_error -> refusal = function
  | New_external_conflict _ | New_produced_conflict _ | Cycle -> Retry
  | Ambiguous_external_order { external_; _ } -> Until_consumed external_
  | Empty_consume | Empty_produce | Invalid_node _ | Duplicate_node _
  | Stale_node _ | Ambiguous_produced_order _ ->
      Final

let still_refused pool refused o =
  match Hashtbl.find_opt refused (Rule_candidate.key o) with
  | Option.None | Option.Some Retry -> false
  | Option.Some Final -> true
  | Option.Some (Until_consumed external_) ->
      Rule_candidate.is_live pool external_

let run ~ctx ~finalize graph =
  let max_iterations = max_iterations graph in
  let refresh_after_commit = Rule_graph.node_count graph <= 128 in
  let pool = Rule_candidate.start ~ctx ~finalize graph in
  let refused = Hashtbl.create 64 in
  let rec loop iteration ~committed graph queue =
    if iteration >= max_iterations then { graph; settled = false }
    else
      match Queue.pop queue with
      | None ->
          if not committed then { graph; settled = true }
          else begin
            ignore (Rule_candidate.refresh pool);
            loop iteration ~committed:false graph
              (List.fold_left
                 (fun queue o ->
                   if still_refused pool refused o then queue
                   else offer graph queue o)
                 Queue.empty
                 (Rule_candidate.offers pool))
          end
      | Some ((key, { candidate; _ }), queue) -> (
          match
            Rule_graph.rewrite graph ~consume:candidate.consume
              ~produce:candidate.produce
          with
          | Error e ->
              Hashtbl.replace refused key (refusal e);
              loop iteration ~committed graph queue
          | Ok graph' ->
              Stats.add_saving (Ctx.stats ctx) candidate.saving;
              Rule_candidate.rewrite pool graph' ~consume:candidate.consume;
              let queue =
                if refresh_after_commit then
                  List.fold_left (offer graph') queue
                    (Rule_candidate.refresh pool)
                else queue
              in
              loop (iteration + 1) ~committed:true graph' queue)
  in
  loop 0 ~committed:false graph
    (List.fold_left (offer graph) Queue.empty (Rule_candidate.refresh pool))
