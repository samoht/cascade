open Stylesheet
open Stdlib
open Rule_rewrite
module Node_set = Set.Make (Rule_graph.Node_id)
module Int_set = Set.Make (Int)

(* The graph the search reads, which a rewrite replaces, the nodes it has
   consumed and how many it prints. *)
type view = {
  mutable graph : Rule_graph.t;
  mutable dead : Int_set.t;
  mutable live : int;
}

let node_rule g id = Rule_graph.node_rule g.graph id
let is_live g id = not (Int_set.mem (Rule_graph.Node_id.to_int id) g.dead)

let view_of graph =
  let live =
    Int_set.of_list
      (List.map Rule_graph.Node_id.to_int (Rule_graph.live_nodes graph))
  in
  let dead = ref Int_set.empty in
  for i = 0 to Rule_graph.node_count graph - 1 do
    if not (Int_set.mem i live) then dead := Int_set.add i !dead
  done;
  { graph; dead = !dead; live = Int_set.cardinal live }

let rewrite_view g graph ~consume =
  let produced = Rule_graph.node_count graph - Rule_graph.node_count g.graph in
  g.live <- g.live - List.length consume + produced;
  g.graph <- graph;
  g.dead <-
    List.fold_left
      (fun dead id -> Int_set.add (Rule_graph.Node_id.to_int id) dead)
      g.dead consume

let same_id a b = Rule_graph.Node_id.compare a b = 0
let same_decl = Declaration.same_minified
let merge_selector_list = Merge.selector_list
let listable = Merge.listable
let mix_int = Common.mix_int
let hash_ints xs = List.fold_left mix_int 0x345678 xs

module Int_table = Common.Table.Make (struct
  type t = int

  let equal = Int.equal
  let hash x = x land max_int
end)

let rule_eligible (r : rule) =
  r.nested = [] && r.merge_key = Option.None && listable r.selector
  && not (List.exists Shorthand.is_all_declaration r.declarations)

(* Any identical-body rules can group into a selector list: a list of disjoint
   selectors, including distinct pseudo-elements ([::before] and [::after] never
   match a common box), is exactly equivalent to the separate rules, and the DAG
   enforces cascade-order safety. *)
let can_group_selectors = Merge.all_compatible
let rules_with_ids g ids = List.map (fun id -> (id, node_rule g id)) ids

(* The prefix synthesis reads one rule at a time and asks only whether THAT rule
   already carries the prefixed spelling. So a rewrite that leaves the two
   halves of a pair in two rules has the missing half written back beside the
   one it kept, even though the group rule above already serves it: the element
   ends up carrying the prefixed declaration twice and the emission is longer
   than the rules the rewrite replaced. Keep the pair in one rule or leave it
   alone. *)
let splits_a_fallback_pair g ~consume ~produce =
  let together (a, b) =
    let holds decl (r : rule) = List.exists (same_decl decl) r.declarations in
    (* A rewrite that keeps neither half has not separated them: the shorthand
       contraction writes both as their shorthands, which is one pair again
       under two other names. Only a half that outlives its partner is a
       split. *)
    (not (List.exists (holds a) produce || List.exists (holds b) produce))
    || List.exists (fun r -> holds a r && holds b r) produce
  in
  List.exists
    (fun id ->
      List.exists
        (fun pair -> not (together pair))
        (Webkit_fallback.pairs (node_rule g id).declarations))
    consume

let rewrite_candidate ~kind ~finalize g ~consume ~produce =
  if splits_a_fallback_pair g ~consume ~produce then Option.None
  else Rule_rewrite.v ~kind ~finalize g.graph ~consume ~produce

let selector_size g id = Rule_graph.node_selector_size g.graph id

let decls_inline_cost g decls =
  Size.decls (Rule_graph.size_print g.graph) decls + List.length decls

let grouped_rule (rules : rule list) : rule option =
  match rules with
  | [] -> Option.None
  | first :: _ ->
      let selectors = List.map (fun (r : rule) -> r.selector) rules in
      if not (can_group_selectors selectors) then Option.None
      else
        Option.Some
          {
            first with
            selector = merge_selector_list selectors;
            nested = [];
            merge_key = Option.None;
          }

(* [ids] once each, in the order the graph prints them: a bucket's windows are
   in that order already, and are handed back as they are. *)
let ordered_ids g ids =
  let rec printed = function
    | a :: (b :: _ as rest) ->
        Rule_graph.compare_position g.graph a b < 0 && printed rest
    | [ _ ] | [] -> true
  in
  if printed ids then ids
  else
    ids |> Node_set.of_list |> Node_set.elements
    |> List.sort (Rule_graph.compare_position g.graph)

type offer = { key : int; candidate : candidate; mutable withdrawn : bool }

(* A candidate worked out on an earlier graph of the lineage, as [g] offers
   it. *)
let offered_on g (candidate : candidate) =
  { candidate with generation = Rule_graph.generation g.graph }

type core = {
  ctx : Ctx.t;
  finalize : rule -> rule;
  view : view;
  mutable epoch : int;
  mutable next_key : int;
  mutable offered : offer list;
  mutable consumed : Rule_graph.node_id list;
  mutable produced : Rule_graph.node_id list;
  mutable moved : Rule_graph.node_id list;
}

let key (offer : offer) = offer.key
let candidate (offer : offer) = offer.candidate

let offer core candidate =
  let key = core.next_key in
  core.next_key <- key + 1;
  let offer =
    { key; candidate = offered_on core.view candidate; withdrawn = false }
  in
  core.offered <- offer :: core.offered;
  offer

let withdraw (offer : offer) = offer.withdrawn <- true
let withdraw_option = Option.iter withdraw
let without id ids = List.filter (fun other -> not (same_id id other)) ids

(* The rewrites since the last refresh: the rules they consumed, and the live
   rules they produced and moved. *)
type changes = {
  consumed : Rule_graph.node_id list;
  produced : Rule_graph.node_id list;
  moved : Rule_graph.node_id list;
}

(* One kind of candidate. A generator files every live rule under the keys that
   decide which of its candidates the rule takes part in, and when the run
   refreshes it offers the candidates of the keys the rewrites touched. Every
   generator is asked the same questions in the same order, and the order its
   offers are made in breaks ties between equal savings. *)
module type S = sig
  type t

  val add : core -> t -> Rule_graph.node_id -> unit
  (** [add core t id] files the live rule [id] under its keys. *)

  val remove : core -> t -> Rule_graph.node_id -> unit
  (** [remove core t id] takes the consumed rule [id] out of its keys. *)

  val reorder : core -> t -> Rule_graph.node_id list -> unit
  (** [reorder core t moved] marks what reads where the rules [moved] print. *)

  val settle : core -> t -> changes -> unit
  (** [settle core t changes] offers the candidates of the keys touched since
      the last settle, and asks again what [changes] can have answered
      differently. *)

  val offers : t -> offer list -> offer list
  (** [offers t acc] is the offers [t] holds, prepended to [acc]. *)
end

let graph g = g.graph
let live g = g.live
let ctx core = core.ctx
let finalize core = core.finalize
let view core = core.view
let epoch core = core.epoch

let v ~ctx ~finalize graph =
  {
    ctx;
    finalize;
    view = view_of graph;
    epoch = 0;
    next_key = 0;
    offered = [];
    consumed = [];
    produced = [];
    moved = [];
  }

let rewrite core graph ~consume =
  let first = Rule_graph.node_count core.view.graph in
  rewrite_view core.view graph ~consume;
  let produced =
    List.init
      (Rule_graph.node_count graph - first)
      (fun offset -> Rule_graph.Node_id.of_int_exn (first + offset))
  in
  let moved = Rule_graph.moved graph in
  core.consumed <- List.rev_append consume core.consumed;
  core.produced <- List.rev_append produced core.produced;
  core.moved <- List.rev_append moved core.moved;
  { consumed = consume; produced; moved }

let pending core =
  if core.epoch > 0 && core.consumed = [] then Option.None
  else
    Option.Some
      {
        consumed = core.consumed;
        produced = List.filter (is_live core.view) core.produced;
        moved = List.filter (is_live core.view) core.moved;
      }

let close core =
  core.epoch <- core.epoch + 1;
  core.consumed <- [];
  core.produced <- [];
  core.moved <- [];
  let offered =
    List.rev
      (List.filter (fun (offer : offer) -> not offer.withdrawn) core.offered)
  in
  core.offered <- [];
  offered
