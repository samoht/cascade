(* Every kind of candidate behind {!Candidate_kind.S}, asked in one order: the
   order offers are made in breaks ties between equal savings. *)

open Stylesheet
open Stdlib

type offer = Candidate_kind.offer

let key = Candidate_kind.key
let candidate = Candidate_kind.candidate

type t = {
  core : Candidate_kind.core;
  bodies : Candidate_groups.t;
  selectors : Candidate_groups.t;
  inlining : Candidate_inlining.t;
  exact : Candidate_factoring.t;
  default : Candidate_factoring.t;
}

let remove_node t id =
  Candidate_groups.remove t.core t.bodies id;
  Candidate_groups.remove t.core t.selectors id;
  Candidate_inlining.remove t.core t.inlining id;
  Candidate_factoring.remove t.core t.exact id;
  Candidate_factoring.remove t.core t.default id

let add_node t id =
  Candidate_groups.add t.core t.bodies id;
  Candidate_groups.add t.core t.selectors id;
  Candidate_inlining.add t.core t.inlining id;
  Candidate_factoring.add t.core t.exact id;
  Candidate_factoring.add t.core t.default id

let reorder_moved t moved =
  Candidate_groups.reorder t.core t.bodies moved;
  Candidate_groups.reorder t.core t.selectors moved;
  Candidate_inlining.reorder t.core t.inlining moved;
  Candidate_factoring.reorder t.core t.exact moved;
  Candidate_factoring.reorder t.core t.default moved

let settle t changes =
  Candidate_groups.settle t.core t.bodies changes;
  Candidate_groups.settle t.core t.selectors changes;
  Candidate_inlining.settle t.core t.inlining changes;
  Candidate_factoring.settle t.core t.exact changes;
  Candidate_factoring.settle t.core t.default changes

module Rule_table = Hashtbl.Make (struct
  type t = rule

  let equal a b = Stylesheet.equal_statement (Rule a) (Rule b)
  let hash = Stylesheet.rule_hash
end)

(* [finalize] is a pure normalisation, and the candidates of one run hand it the
   same leftover rules over and over: every subset that factors the same
   declarations out of a member leaves it the same rule. *)
let remembered size finalize =
  let known = Rule_table.create size in
  fun rule ->
    match Rule_table.find_opt known rule with
    | Option.Some finalized -> finalized
    | Option.None ->
        let finalized = finalize rule in
        Rule_table.replace known rule finalized;
        finalized

let v ?nodes ~ctx ~finalize g =
  (* Tables start at the run's size: most runs hold a handful of rules. *)
  let size = Rule_graph.node_count g in
  let finalize = remembered size finalize in
  let nodes = Option.value nodes ~default:size in
  let aggressive = Ctx.aggressive ctx in
  let t =
    {
      core = Candidate_kind.v ~ctx ~finalize g;
      bodies = Candidate_groups.v size Bodies;
      selectors = Candidate_groups.v size Selectors;
      inlining = Candidate_inlining.v size;
      exact = Candidate_factoring.v size Exact (Subsets.exact ~aggressive nodes);
      default =
        Candidate_factoring.v size Default (Subsets.default ~aggressive nodes);
    }
  in
  List.iter (add_node t) (Rule_graph.live_nodes g);
  t

let refresh t =
  match Candidate_kind.pending t.core with
  | Option.None -> []
  | Option.Some changes ->
      settle t changes;
      Candidate_kind.close t.core

let start ?nodes ~ctx ~finalize g = v ?nodes ~ctx ~finalize g

let rewrite t graph ~consume =
  let changes = Candidate_kind.rewrite t.core graph ~consume in
  List.iter (remove_node t) changes.consumed;
  List.iter (add_node t) changes.produced;
  if changes.moved <> [] then reorder_moved t changes.moved

let offers t =
  []
  |> Candidate_groups.offers t.bodies
  |> Candidate_groups.offers t.selectors
  |> Candidate_inlining.offers t.inlining
  |> Candidate_factoring.offers t.exact
  |> Candidate_factoring.offers t.default
  |> List.sort (fun a b -> Int.compare (key a) (key b))

let afresh t =
  let g = Candidate_kind.graph (Candidate_kind.view t.core) in
  let ranks =
    lazy
      (let ranks = Array.make (Rule_graph.node_count g) 0 in
       Array.iteri
         (fun rank id -> ranks.(Rule_graph.Node_id.to_int id) <- rank)
         (Rule_graph.canonical_order g);
       ranks)
  in
  Candidate_factoring.searched t.core t.exact ~ranks
  && Candidate_factoring.searched t.core t.default ~ranks

let is_live t id = Candidate_kind.is_live (Candidate_kind.view t.core) id
let candidates t = List.map candidate (offers t)

let enumerate ?nodes ~ctx ~finalize g =
  let t = v ?nodes ~ctx ~finalize g in
  ignore (refresh t);
  candidates t
